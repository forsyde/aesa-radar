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
-- import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List

import ForSyDe.Atom.MoC.Stream (tailS)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY  (Signal(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF (Signal(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V (Vector(..), vector, fromVector, farm11)

import ForSyDe.AESA.HighLevelAtom
import ForSyDe.AESA.Params
-- import ForSyDe.AESA.Coefs

import Utils

main = do
  args <- getArgs >>= parse
  -- Begin execution
  when (genInps args) $ generateInputs (genPath args) (inFilePath args)
  putStrLn $ "Reading the indata at: " ++ inFilePath args ++ "..."
  aesaIn <- readInData (inFilePath args)
  let nAntennas = length aesaIn
      nSamples  = length $ head aesaIn
  putStrLn $ "Read " ++ show nAntennas ++ " * " ++ show nSamples
    ++ " complex samples which means " ++ show ((nAntennas * nSamples) `div` (nA * nb * nFFT))
    ++ " indata cube(s)..."
  -- Re-build & declare an (unsafe) observable AESA system, only for the purpose of testing
  let iSigs           = V.vector $ map SY.signal $ aesaIn
      oDBF            = dbf iSigs
      oPC             = pc oDBF
      (oCTR,oCTL)     = ct oPC
      (oDFBR,oDFBL)   = (dfb oCTR,dfb oCTL)
      (oCFARR,oCFARL) = (cfar oDFBR,cfar oDFBL)
      oAESA           = int oCFARR $ V.farm11 tailS oCFARL
      ------------------------------------------------------
      -- declarations for unwrapped (dumpable) data
      inData       = toListVecSig SY.fromSignal iSigs
      outDbfData   = toListVecSig SY.fromSignal oDBF
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
      exitWith ExitSuccess
    else do
      putStrLn "Executing the AESA and dumping eventual intermediate files..."
      dumpData3 (outFilePath args) showFloat outAesaData
      printDimen3 ("Dumped at "++ outFilePath args ++" the last cube of AESA output: ") outAesaData  
      exitWith ExitSuccess
  where
    ---------------------------------------------------------
    inFilePath  args = inPath args  ++ "/AESA_INDATA.csv"
    outFilePath args = outPath args ++ "/AESA_OUTPUT.csv"
    dbfOutFile  args = outPath args ++ "/DBF.csv"
    pcOutFile   args = outPath args ++ "/PC.csv"
    ctOutFile   args = (outPath args ++ "/CT_R.csv",outPath args ++ "/CT_L.csv")
    dfbOutFile  args = (outPath args ++ "/DFB_R.csv",outPath args ++ "/DFB_L.csv")
    cfarOutFile args = (outPath args ++ "/CFAR_R.csv",outPath args ++ "/CFAR_L.csv")
    ---------------------------------------------------------
    -- take only the last cube of data!
    toListVecMat unwrap = map (  (map V.fromVector . V.fromVector) . head . reverse . unwrap) . V.fromVector
    toListVecSig unwrap = transpose . map unwrap . V.fromVector
    ---------------------------------------------------------
    generateInputs scrPath filePath = do
      putStrLn "Generating AESA radar input, please wait..."
      (exit,out,errs) <- readProcessWithExitCode  "python3" [scrPath] filePath
      case exit of
        ExitSuccess -> do
          hPutStrLn stdout out
          putStrLn $ "Generated AESA radar input files in: " ++ filePath
        ExitFailure n -> do 
          hPutStrLn stderr errs
          exitWith (ExitFailure n)

  -- mapM_ (executeModel n or jfkS laxS ordS) imp
  -- where
  --   executeModel n or jfkS laxS ordS imp = do
  --     let (jfkL, laxL, ordL)  = implem jfkS laxS ordS
  --         implem = case imp of DESeq -> S.threeAirports
  --                              DEPar -> P.threeAirports
  --                              SADFSeq -> SadfS.threeAirports
  --                              SADFPar -> SadfP.threeAirports
  --                              SADFScen -> SadfPP.threeAirports
  --                              _ -> error "Not implemented"
  --     if or then do
  --       runtime <- time (takeS n jfkL, takeS n laxL, takeS n ordL)
  --       putStrLn $ show runtime
  --       else do
  --         putStr "\nJFK: "
  --         pprint n jfkL
  --         putStr "\nLAX: "
  --         pprint n laxL
  --         putStr "\nORD: "
  --         pprint n ordL
            

data Args = Args
  { genInps :: Bool
  , pltOuts :: Bool
  , timeM   :: Bool
  , genPath :: String
  , pltPath :: String
  , inPath  :: String
  , outPath :: String
  , inter   :: String
  } deriving (Eq,Ord,Show)
             
flags =
  [Option ['g'] ["gen-data"] (NoArg GenInput)
    "Invokes the antenna input data generator script."
  ,Option ['p'] ["plot"]     (NoArg PlotOutput)
    "Invokes the output plotter script."
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
       "a: after CFAR stage")
  ,Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  ]

data Flag
  = GenInput            -- -g
  | PlotOutput          -- -p
  | MeasureTime         -- -t
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
                     inter    = selArg Intermediate
                 genPath <- checkIfFile "scripts/generate-input.py" (selArg GenScript)
                 pltPath <- checkIfFile "scripts/plot-output.py" (selArg PlotPath)
                 inPath  <- checkIfDir "gen" (selArg InPath)
                 outPath <- checkIfDir "gen" (selArg DumpPath)
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitWith ExitSuccess
                   else return $ Args genInps pltOuts timeM genPath pltPath inPath outPath inter
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-hl [-gh|--out=PATH|--inter=[dpcfa]|...]"
 


-- import qualified                           Data.ByteString.Char8 as S
-- import qualified                           Data.ByteString.Unsafe as S
-- import                                     Data.List
-- import                                     System.Environment

-- import qualified "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
-- import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
-- import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
-- import                                     AESA
-- import                                     Types
-- import                                     Coefs
-- import                                     Utils

-- main = do
--   inpath  <- getArgs
--   inBytes <- S.readFile (head inpath)
--   let iSigs           = V.vector $ map SY.signal $ transpose
--                         $ map (getSamples) $ S.lines inBytes
--       oDBF            = dbf iSigs
--       oPC             = pc' oDBF
--       (oCTR,oCTL)     = ct oPC
--       (oDFBR,oDFBL)   = (dfb oCTR,dfb oCTL)
--       (iCFARR,iCFARL) = (convertToCube oDFBR,convertToCube oDFBL)
--       (oCFARR,oCFARL) = (cfar iCFARR,cfar iCFARL)
--       oAESA           = int mkFirCoefs oCFARR oCFARL
--       ------------------------------------------------------
--       inData       = toListVecSig SY.fromSignal iSigs
--       outDbfData   = toListVecSig SY.fromSignal oDBF
--       outPcData    = toListVecSig SY.fromSignal oPC
--       outCtrData   = toListVecSig SDF.fromSignal oCTR
--       outCtlData   = toListVecSig SDF.fromSignal oCTL
--       outDfbrData  = toListVecSig SDF.fromSignal oDFBR  
--       outDfblData  = toListVecSig SDF.fromSignal oDFBL
--       inCfarrData  = toListSigCub SY.fromSignal iCFARR
--       inCfarlData  = toListSigCub SY.fromSignal iCFARL
--       outCfarrData = toListSigCub SY.fromSignal oCFARR
--       outCfarlData = toListSigCub SY.fromSignal oCFARL
--       outAesaData  = toListSigCub SY.fromSignal oAESA
--       ------------------------------------------------------
--   S.writeFile "dump/AESAin.csv" (toString2 showComplex inData)        >> printDimen2 "AESAi  :" inData
--   S.writeFile "dump/DBFout.csv" (toString2 showComplex outDbfData)    >> printDimen2 "DBFo   :" outDbfData
--   S.writeFile "dump/PCout.csv" (toString2 showComplex outPcData)      >> printDimen2 "PCo    :" outPcData
--   S.writeFile "dump/CT-Rout.csv" (toString2 showComplex outCtrData)   >> printDimen2 "CTro   :" outCtrData
--   S.writeFile "dump/CT-Lout.csv" (toString2 showComplex outCtlData)   >> printDimen2 "CTlo   :" outCtlData
--   S.writeFile "dump/DFB-Rout.csv" (toString2 showFloat outDfbrData)   >> printDimen2 "DBFro  :" outDfbrData
--   S.writeFile "dump/DFB-Lout.csv" (toString2 showFloat outDfblData)   >> printDimen2 "DBFlo  :" outDfblData 
--   S.writeFile "dump/CFAR-Rin.csv" (toString4 showFloat inCfarrData)   >> printDimen4 "CFARri :" inCfarrData 
--   S.writeFile "dump/CFAR-Lin.csv" (toString4 showFloat inCfarlData)   >> printDimen4 "CFARli :" inCfarlData 
--   S.writeFile "dump/CFAR-Rout.csv" (toString4 showFloat outCfarrData) >> printDimen4 "CFARro :" outCfarrData 
--   S.writeFile "dump/CFAR-Lout.csv" (toString4 showFloat outCfarlData) >> printDimen4 "CFARlo :" outCfarlData 
--   S.writeFile "dump/AESAout.csv" (toString4 showFloat outAesaData)    >> printDimen4 "AESAo  :" outAesaData
--   where
--     toListVecSig unwrap = transpose . map unwrap . V.fromVector
--     toListSigCub unwrap = map (map (map V.fromVector . V.fromVector) . V.fromVector) . unwrap
--     toString2 showNum = S.unlines . map (S.concat . map showNum)
--     toString4 showNum = S.unlines . map (S.unlines . map (S.unlines . map (S.concat . map showNum)))
--     printDimen2 str what = putStrLn $ str ++ (show $ length what) ++ " * " ++ (show $ length $ head what)
--     printDimen4 str what = let fb = head what 
--                                fr = head fb
--                                fw = head fr
--                            in putStrLn $ str ++ (show $ length what) ++ " * " ++ (show $ length fb)
--                               ++ " * " ++ (show $ length fr)  ++ " * " ++ (show $ length fw) 
--     -- printDimenVSDF str what = do
--     --   putStr $ str ++ (show $ V.length what) ++ " x "
--     --   print  $ length $ SDF.fromSignal $ V.first what
--     -- printDimenSYC str what = do
--     --   let sg = SY.fromSignal what
--     --       fb = head sg 
--     --       fr = V.first fb
--     --       fw = V.first fr
--     --   putStrLn $ str ++ show (length sg) ++ " x " ++ show (V.length fb)
--     --     ++ " x " ++ show (V.length fr) ++ " x " ++ show (V.length fw) 


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


instance NFData a => NFData (SY.Signal a) where
  rnf = rnf . SY.fromSignal

instance NFData a => NFData (V.Vector a) where
  rnf = rnf . V.fromVector 
