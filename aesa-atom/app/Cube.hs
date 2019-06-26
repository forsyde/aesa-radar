{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Cube where

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

import ForSyDe.AESA.CubesAtom
import ForSyDe.AESA.Params
-- import ForSyDe.AESA.Coefs

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
      nCubes    = (nAntennas * nSamples) `div` (nA * nb * nFFT)
  print args
  putStrLn $ "Read " ++ show nAntennas ++ " * " ++ show nSamples
    ++ " complex samples which means " ++ show nCubes ++ " indata cube(s)..."

  let iSigs           = SY.signal $ map toCubeLists $ transpose $ map (sublist (nb*nFFT)) aesaIn
      beams           = dbf iSigs
      oPC             = pc beams
      (oDFBR,oDFBL)   = (dfb oPC, dfb $ tailS $ overlap oPC)
      (oCFARR,oCFARL) = (cfar oDFBR, cfar oDFBL)
      oAESA           = int oCFARR oCFARL
      ------------------------------------------------------
      -- declarations for unwrapped (dumpable) data
      inData       = toListCube iSigs
      outDbfData   = toListCube beams
      outPcData    = toListCube oPC
      outDfbrData  = toListCube oDFBR  
      outDfblData  = toListCube oDFBL
      outCfarrData = toListCube oCFARR
      outCfarlData = toListCube oCFARL
      outAesaData  = toListCube oAESA

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
           mapM_ (dumpInter args inData outDbfData outPcData outDfbrData outDfblData
                  outCfarrData outCfarlData outAesaData) (inter args)
           exitSuccess
  where
    ---------------------------------------------------------
    inFilePath       = inPath 
    outFilePath      = outFile
    indDbgFile  args = outPath args ++ "/AESA_INDATA_DEBUG.csv"
    dbfOutFile  args = outPath args ++ "/DBF_C.csv"
    pcOutFile   args = outPath args ++ "/PC_C.csv"
    dfbOutFile  args = (outPath args ++ "/DFB_R_C.csv",outPath args ++ "/DFB_L_C.csv")
    cfarOutFile args = (outPath args ++ "/CFAR_R_C.csv",outPath args ++ "/CFAR_L_C.csv")
    ---------------------------------------------------------
    -- take only the last cube of data!
    toCubeLists :: [[a]] -> Vector (Vector (Vector (a)))
    toCubeLists = vector . map (vector . map vector . sublist nb) 
    toListCube  = map (map fromVector . fromVector) . fromVector . last . SY.fromSignal
    ---------------------------------------------------------
    dumpInter args inData outDbfData outPcData outDfbrData outDfblData
      outCfarrData outCfarlData outAesaData flag  = case flag of
      'i' -> dumpData3 (indDbgFile args) showComplex inData     >> printDimen3 "AESAi  :" inData
      'd' -> dumpData3 (dbfOutFile args) showComplex outDbfData >> printDimen3 "DBFo   :" outDbfData
      'p' -> dumpData3 (pcOutFile args) showComplex outPcData   >> printDimen3 "PCo    :" outPcData
      'f' -> dumpData3 (fst $ dfbOutFile args) showFloat outDfbrData   >> printDimen3 "DBFro  :" outDfbrData >>
             dumpData3 (snd $ dfbOutFile args) showFloat outDfblData   >> printDimen3 "DBFlo  :" outDfblData 
      'a' -> dumpData3 (fst $ cfarOutFile args) showFloat outCfarrData >> printDimen3 "CFARro :" outCfarrData >>
             dumpData3 (snd $ cfarOutFile args) showFloat outCfarlData >> printDimen3 "CFARlo :" outCfarlData
      'o' -> dumpData3 (outFilePath args) showFloat outAesaData        >> printDimen3 "AESAo  :" outAesaData
      ls_ -> return ()
    ---------------------------------------------------------

data Args = Args
  { timeM   :: Bool
  , inPath  :: String
  , outFile :: String
  , outPath :: String
  , inter   :: String
  } deriving (Eq,Ord,Show)
             
flags =
  [Option ['t'] ["time"]     (NoArg MeasureTime)
    "Measures and prints out execution time."
  ,Option ['i'] ["input"]    (ReqArg InPath "PATH")
    "Path to input data file. Default: gen/AESA_INPUT.csv"
  ,Option ['o'] ["output"]   (ReqArg DumpPath "PATH")
    "Path to main generated file. Default: gen/AESA_OUT_C.csv"
  ,Option [] ["inter"]       (ReqArg Intermediate "STAGE")
    ("Dumps and plots data at intermediate stages. Options:\n" ++
       "d: after DBF stage\n" ++
       "p: after PC stage\n" ++
       "f: after DFB stage\n" ++
       "a: after CFAR stage\n"++
       "o: after complete AESA")
  ,Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  ]

data Flag
  = MeasureTime         -- -t
  | InPath String       -- -i
  | DumpPath String     -- -o
  | Intermediate String -- --inter
  | Help                -- --help
  deriving (Ord,Show)

instance Eq Flag where
  MeasureTime      == MeasureTime      = True
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
                     inter    = selArg Intermediate
                     inPath   = let p = (selArg InPath) in if null p then "gen/AESA_INPUT.csv" else p 
                     outFile  = let p = (selArg DumpPath) in if null p then "gen/AESA_OUT_C.csv" else p 
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitSuccess
                   else return $ Args timeM inPath outFile outFile inter
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-hl [-tpio|--inter=[dpcfa]|...]"
 

lim :: Int
lim = 10^5

time :: (NFData t) => (SY.Signal (Vector (Vector (Vector t))))  -> IO (Double)
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
