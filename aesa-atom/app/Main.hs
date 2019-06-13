{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import System.Process

import Control.Exception
import System.CPUTime
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List

import ForSyDe.Atom.MoC.Stream (Stream(..), tailS)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY  (Signal(..), SY(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF (Signal(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V (Vector(..), vector, fromVector, farm11)

import ForSyDe.AESA.HighLevelAtom
import ForSyDe.AESA.Params
-- import ForSyDe.AESA.Coefs

import Utils

main = do
  args <- getArgs >>= parse
  print args
  -- Begin execution
  when (genInps args) $ generateInputs (genPath args) (inFilePath args)
  putStrLn $ "Reading the indata at: " ++ inFilePath args ++ "..."
  aesaIn <- readInData (inFilePath args)
  let nAntennas = length aesaIn
      nSamples  = length $ head aesaIn
  putStrLn $ "Read " ++ show nAntennas ++ " * " ++ show nSamples
    ++ " complex samples which means " ++ show ((nAntennas * nSamples) `div` (nA * nb * nFFT))
    ++ " indata cube(s)..."
  let iSigs           = map SY.signal aesaIn
      beams           = dbf $ vector iSigs
      oPC             = pc beams
      (oCTR,oCTL)     = ct oPC
      (oDFBR,oDFBL)   = (dfb oCTR,dfb oCTL)
      (oCFARR,oCFARL) = (cfar oDFBR,cfar oDFBL)
      oAESA           = let aesa = int oCFARR $ V.farm11 tailS oCFARL
                        in if (parExec args)
                           then vector (fromVector aesa `using` parList rdeepseq)
                           else aesa
      ------------------------------------------------------
      -- declarations for unwrapped (dumpable) data
      inData       = toListVecSig SY.fromSignal $ vector iSigs
      outDbfData   = toListVecSig SY.fromSignal beams
      outPcData    = toListVecSig SDF.fromSignal oPC
      outCtrData   = toListVecSig SDF.fromSignal oCTR
      outCtlData   = toListVecSig SDF.fromSignal oCTL
      outDfbrData  = toListVecSig SDF.fromSignal oDFBR  
      outDfblData  = toListVecSig SDF.fromSignal oDFBL
      outCfarrData = toListVecMat SDF.fromSignal oCFARR
      outCfarlData = toListVecMat SDF.fromSignal oCFARL
      outAesaData  = toListVecMat SY.fromSignal oAESA
  -- Carry on with the program execution
  if timeM args
    then do
      putStrLn "Only measuring execution time. No data will be dumped, printed or plotted..."
      runtime <- time oAESA
      putStrLn $ "Finished execution in " ++ show runtime ++ " sec"
      exitSuccess
    else if null (inter args)
         then do
           putStrLn "Executing and dumping only the AESA output..."
           dumpData3 (outFilePath args) showFloat outAesaData
           printDimen3 ("Dumped at "++ outFilePath args ++" the last cube of AESA output: ") outAesaData
           exitSuccess
         else do
           putStrLn "Executing and dumping intermediate AESA outputs..."
           mapM_ (dumpInter args inData outDbfData outPcData outCtrData outCtlData
                  outDfbrData outDfblData outCfarrData outCfarlData outAesaData) (inter args)
           exitSuccess
  where
    ---------------------------------------------------------
    inFilePath  args = inPath args  ++ "/AESA_INDATA.csv"
    indDbgFile  args = outPath args ++ "/AESA_INDATA_DEBUG.csv"
    outFilePath args = outPath args ++ "/AESA_OUTPUT.csv"
    dbfOutFile  args = outPath args ++ "/DBF.csv"
    pcOutFile   args = outPath args ++ "/PC.csv"
    ctOutFile   args = (outPath args ++ "/CT_R.csv",outPath args ++ "/CT_L.csv")
    dfbOutFile  args = (outPath args ++ "/DFB_R.csv",outPath args ++ "/DFB_L.csv")
    cfarOutFile args = (outPath args ++ "/CFAR_R.csv",outPath args ++ "/CFAR_L.csv")
    ---------------------------------------------------------
    -- take only the last cube of data!
    toListVecMat unwrap = map (  (map V.fromVector . V.fromVector) . last . unwrap) . V.fromVector
    toListVecSig unwrap = transpose . map unwrap . V.fromVector
    ---------------------------------------------------------
    dumpInter args inData outDbfData outPcData outCtrData outCtlData
      outDfbrData outDfblData outCfarrData outCfarlData outAesaData flag  = case flag of
      'i' -> dumpData2 (indDbgFile args) showComplex inData     >> printDimen2 "AESAi  :" inData
      'd' -> dumpData2 (dbfOutFile args) showComplex outDbfData >> printDimen2 "DBFo   :" outDbfData
      'p' -> dumpData2 (pcOutFile args) showComplex outPcData   >> printDimen2 "PCo    :" outPcData
      'c' -> dumpData2 (fst $ ctOutFile args) showComplex outCtrData   >> printDimen2 "CTro   :" outCtrData >>
             dumpData2 (snd $ ctOutFile args) showComplex outCtlData   >> printDimen2 "CTlo   :" outCtlData
      'f' -> dumpData2 (fst $ dfbOutFile args) showFloat outDfbrData   >> printDimen2 "DBFro  :" outDfbrData >>
             dumpData2 (snd $ dfbOutFile args) showFloat outDfblData   >> printDimen2 "DBFlo  :" outDfblData 
      'a' -> dumpData3 (fst $ cfarOutFile args) showFloat outCfarrData >> printDimen3 "CFARro :" outCfarrData >>
             dumpData3 (snd $ cfarOutFile args) showFloat outCfarlData >> printDimen3 "CFARlo :" outCfarlData
      'o' -> dumpData3 (outFilePath args) showFloat outAesaData        >> printDimen3 "AESAo  :" outAesaData
      _ -> return ()
    ---------------------------------------------------------
    generateInputs scrPath filePath = do
      putStrLn "Generating AESA radar input, please wait..."
      (exit,out,errs) <- readProcessWithExitCode  "python3" [scrPath] filePath
      case exit of
        ExitSuccess -> do
          putStrLn out
          putStrLn $ "Generated AESA radar input files in: " ++ filePath
        ExitFailure n -> do 
          hPutStrLn stderr errs
          exitWith (ExitFailure n)

data Args = Args
  { genInps :: Bool
  , pltOuts :: Bool
  , timeM   :: Bool
  , parExec :: Bool
  , genPath :: String
  , pltPath :: String
  , inPath  :: String
  , outPath :: String
  , inter   :: String
  } deriving (Eq,Ord,Show)
             
flags =
  [Option ['g'] ["gen-data"] (NoArg GenInput)
    "Invokes the antenna input data generator script."
  ,Option ['l'] ["plot-graph"] (NoArg PlotOutput)
    "Invokes the output plotter script."
  ,Option ['p'] ["parallel"] (NoArg Parallel)
    "Distributes the simulation on multiple cores if possible."
  ,Option ['t'] ["time"]     (NoArg MeasureTime)
    "Measures and prints out execution time."
  ,Option [] ["gen-path"]    (ReqArg GenScript "PATH")
    "Path to input generator script. Default: scripts/generate-input.py"
  ,Option [] ["plot-path"]   (ReqArg PlotPath "PATH")
    "Path to output plotter script. Default: scripts/plot-output.py"
  ,Option ['i'] ["input"]    (ReqArg InPath "PATH")
    "Path to input data files. Default: gen"
  ,Option ['o'] ["output"]   (ReqArg DumpPath "PATH")
    "Path to dump generated files. Default: gen"
  ,Option [] ["inter"]       (ReqArg Intermediate "STAGE")
    ("Dumps and plots data at intermediate stages. Options:\n" ++
       "d: after DBF stage\n" ++
       "p: after PC stage\n" ++
       "c: after CT stage\n" ++
       "f: after DFB stage\n" ++
       "a: after CFAR stage\n"++
       "o: after complete AESA")
  ,Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  ]

data Flag
  = GenInput            -- -g
  | PlotOutput          -- -l
  | MeasureTime         -- -t
  | Parallel            -- -p
  | GenScript String    -- --gen-path
  | PlotPath String     -- --plot-path
  | InPath String       -- -i
  | DumpPath String     -- -o
  | Intermediate String -- --inter
  | Help                -- --help
  deriving (Ord,Show)

instance Eq Flag where
  GenInput         == GenInput         = True
  PlotOutput       == PlotOutput       = True
  MeasureTime      == MeasureTime      = True
  Parallel         == Parallel         = True
  (GenScript _)    == (GenScript _)    = True
  (PlotPath _)     == (PlotPath _)     = True
  (InPath _)       == (InPath _)       = True
  (DumpPath _)     == (DumpPath _)     = True
  (Intermediate _) == (Intermediate _) = True
  Help             == Help = True
  _ == _ = False

getFlagArg (GenScript s) = s
getFlagArg (PlotPath s) = s
getFlagArg (InPath s) = s
getFlagArg (DumpPath s) = s
getFlagArg (Intermediate s) = s

  
parse argv = case getOpt Permute flags argv of
               (args,_,[]) -> do
                 let select x = filter (==x) args
                     selArg x = let arg =  select (x "") in if null arg then "" else getFlagArg (head arg)
                     genInps  = if null (select GenInput) then False else True
                     pltOuts  = if null (select PlotOutput) then False else True
                     timeM    = if null (select MeasureTime) then False else True
                     par      = if null (select Parallel) then False else True
                     inter    = selArg Intermediate
                 genPath <- checkIfFile "scripts/generate-input.py" (selArg GenScript)
                 pltPath <- checkIfFile "scripts/plot-output.py" (selArg PlotPath)
                 inPath  <- checkIfDir "gen" (selArg InPath)
                 outPath <- checkIfDir "gen" (selArg DumpPath)
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitWith ExitSuccess
                   else return $ Args genInps pltOuts timeM par genPath pltPath inPath outPath inter
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-hl [-gh|--out=PATH|--inter=[dpcfa]|...]"
 

lim :: Int
lim = 10^5

time :: (NFData t) => (Vector (SY.Signal (Vector (Vector t))))  -> IO (Double)
time y = do
    start <- getCPUTime
    -- replicateM_ lim $ do
    x <- evaluate $ y
    rnf x `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    -- printf "Computation time: %0.9f sec\n" (diff :: Double)
    -- printf "Individual time: %0.9f sec\n" ( :: Double)
    return diff


instance NFData a => NFData (SY.SY a) where
  rnf = rnf . SY.val

instance NFData a => NFData (SY.Signal a) where
  rnf = rnf . SY.fromSignal

instance NFData a => NFData (V.Vector a) where
  rnf = rnf . V.fromVector 
