{-# LANGUAGE PackageImports #-}
module Main where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List

import ForSyDe.Atom.MoC.Stream (Stream(..), tailS)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY  (Signal(..), SY(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF (Signal(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V (Vector(..), vector, fromVector, farm11)
-- import ForSyDe.Deep.Complex (toDeepCpx, fromDeepCpx)

import AESA.StreamsAtom as M2
import AESA.PC.R1 as R1
import AESA.PC.R2 as R2
import AESA.PC.R3 as R3
import AESA.Params

import Utils

main = do
  (inFilePath, outFile, runArg) <- getArgs >>= parse
  inPath <- checkIfFile inFilePath
  _ <- checkIfDir  outFile
  -- Begin execution
  putStrLn $ "Reading the indata at: " ++ inPath ++ "..."
  aesaIn <- readInData inPath
  let nAntennas = length aesaIn
      nSamples  = length $ head aesaIn
  putStrLn $ "Read " ++ show nAntennas ++ " * " ++ show nSamples
    ++ " complex samples which means "
    ++ show ((nAntennas * nSamples) `div` (nA * nb * nFFT))
    ++ " indata cube(s)..."
  let iSigs = map SY.signal aesaIn
      refPC = selectPC runArg
      oSigs = let (oCTR,oCTL) = ct $ refPC $ dbf $ vector iSigs
                  oCFARR = cfar $ dfb $ oCTR
                  oCFARL = cfar $ dfb $ oCTL
              in int oCFARR oCFARL
      oData = map ((map V.fromVector . V.fromVector) . last . SY.fromSignal)
              $ V.fromVector oSigs
  putStrLn $ "Executing and dumping the AESA output with PC in Refinement Phase "
    ++ runArg ++ "..."
  dumpData3 outFile showFloat oData
  printDimen3 ("Dumped at "++ outFile ++" the last cube of AESA output: ") oData
  where
    selectPC arg
      | arg == "1" = R1.pc'
      | arg == "2" = R2.wrappedPC''
      -- for simulation efficiency we do not simulate 'wrappedPC3' because the zipx . unzipx pattern would
      -- cause a memory overflow (32GB RAM are not enough!) we keep it within the list domain instead.
      | arg == "3" = wrapR2 (V.vector . map SDF.signal . simList R3.procPCSys . map SY.fromSignal . V.fromVector)
      | otherwise  = error $ "No refinement stage " ++ arg ++ ". Re-run the program with --help to see the accepted input commands."
    
             
flags =
  [Option ['i'] ["input"]    (ReqArg InPath "PATH")
    "Path to input data file. Default: gen/AESA_INPUT.csv"
  ,Option ['o'] ["output"]   (ReqArg DumpPath "PATH")
    "Path to main generated file. Default: gen/AESA_OUT_S.csv"
  ,Option ['r'] ["run"]      (ReqArg Run "PHASE")
    ("Runs the AESA in one of the refinement phases [1-4]. Default: 1")
  ,Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  ]

data Flag
  = Run String       -- -r
  | InPath String    -- -i
  | DumpPath String  -- -o
  | Help             -- --help
  deriving (Ord,Show)

instance Eq Flag where
  (Run _)          == (Run _)          = True
  (InPath _)       == (InPath _)       = True
  (DumpPath _)     == (DumpPath _)     = True
  Help             == Help = True
  _ == _ = False


getFlagArg (Run s) = s
getFlagArg (InPath s) = s
getFlagArg (DumpPath s) = s
  
parse argv = case getOpt Permute flags argv of
               (args,_,[]) -> do
                 let select x = filter (==x) args
                     selArg x = let arg =  select (x "")
                                in if null arg then "" else getFlagArg (head arg)
                     runArg   = let p = (selArg Run)
                                in if null p then "1" else p 
                     inPath   = let p = (selArg InPath)
                                in if null p then "gen/AESA_INPUT.csv" else p 
                     outFile  = let p = (selArg DumpPath)
                                in if null p
                                   then "gen/AESA_REFINE_R" ++ runArg ++".csv"
                                   else p 
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitSuccess
                   else return (inPath, outFile, runArg)
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-deep [-ior]"
