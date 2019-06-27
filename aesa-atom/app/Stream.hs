{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Stream where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

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

import AESA.StreamsAtom
import AESA.Params

import Utils

main = do
  iargs <- getArgs >>= parse
  inPath <- checkIfFile (inFilePath iargs)
  outDir <- checkIfDir (outFile iargs)
  -- Begin execution
  putStrLn $ "Reading the indata at: " ++ inPath ++ "..."
  aesaIn <- readInData inPath
  let args      = iargs { outPath=outDir }
      nAntennas = length aesaIn
      nSamples  = length $ head aesaIn
  print args
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
      outCfarrData = toListVecMat ((:[]) . head . SDF.fromSignal) oCFARR
      outCfarlData = toListVecMat ((:[]) . head . SDF.fromSignal) oCFARL
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
    inFilePath       = inPath 
    outFilePath      = outFile
    indDbgFile  args = outPath args ++ "/AESA_INDATA_DEBUG.csv"
    dbfOutFile  args = outPath args ++ "/DBF_S.csv"
    pcOutFile   args = outPath args ++ "/PC_S.csv"
    ctOutFile   args = (outPath args ++ "/CT_R_S.csv",outPath args ++ "/CT_L_S.csv")
    dfbOutFile  args = (outPath args ++ "/DFB_R_S.csv",outPath args ++ "/DFB_L_S.csv")
    cfarOutFile args = (outPath args ++ "/CFAR_R_S.csv",outPath args ++ "/CFAR_L_S.csv")
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
      ls_ -> return ()
    ---------------------------------------------------------

data Args = Args
  { timeM   :: Bool
  , parExec :: Bool
  , inPath  :: String
  , outFile :: String
  , outPath :: String
  , inter   :: String
  } deriving (Eq,Ord,Show)
             
flags =
  [Option ['p'] ["parallel"] (NoArg Parallel)
    "Distributes the simulation on multiple cores if possible."
  ,Option ['t'] ["time"]     (NoArg MeasureTime)
    "Measures and prints out execution time."
  ,Option ['i'] ["input"]    (ReqArg InPath "PATH")
    "Path to input data file. Default: gen/AESA_INPUT.csv"
  ,Option ['o'] ["output"]   (ReqArg DumpPath "PATH")
    "Path to main generated file. Default: gen/AESA_OUT_S.csv"
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
  = MeasureTime         -- -t
  | Parallel            -- -p
  | InPath String       -- -i
  | DumpPath String     -- -o
  | Intermediate String -- --inter
  | Help                -- --help
  deriving (Ord,Show)

instance Eq Flag where
  MeasureTime      == MeasureTime      = True
  Parallel         == Parallel         = True
  (InPath _)       == (InPath _)       = True
  (DumpPath _)     == (DumpPath _)     = True
  (Intermediate _) == (Intermediate _) = True
  Help             == Help = True
  _ == _ = False

getFlagArg (InPath s) = s
getFlagArg (DumpPath s) = s
getFlagArg (Intermediate s) = s

  
parse argv = case getOpt Permute flags argv of
               (args,_,[]) -> do
                 let select x = filter (==x) args
                     selArg x = let arg =  select (x "") in if null arg then "" else getFlagArg (head arg)
                     timeM    = not (null (select MeasureTime)) 
                     par      = not (null (select Parallel))
                     inter    = selArg Intermediate
                     inPath   = let p = (selArg InPath) in if null p then "gen/AESA_INPUT.csv" else p 
                     outFile  = let p = (selArg DumpPath) in if null p then "gen/AESA_OUT_S.csv" else p 
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitSuccess
                   else return $ Args timeM par inPath outFile outFile inter
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-hl [-tpio|--inter=[dpcfa]|...]"
 

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
