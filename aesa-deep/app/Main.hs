{-# LANGUAGE PackageImports #-}
module Main where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Control.Monad

import ForSyDe.Atom.MoC.Stream (Stream(..), tailS)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY  (Signal(..), SY(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF (Signal(..), signal, fromSignal)
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V (Vector(..), vector, fromVector, farm11)

import AESA.StreamsAtom as M2
import AESA.PC.R1 as R1
import AESA.PC.R2 as R2
import AESA.PC.R3 as R3
import AESA.PC.R4 as R4
import AESA.Params

import Utils

main = do
  (inFilePath, outFile, runArg, gmlArg, vhdArg, quaArg) <- getArgs >>= parse
  
  when (not $ null runArg) $ executeSim inFilePath outFile runArg 
    
  when (not $ null gmlArg) $ do {
    putStrLn $ "Dumping GraphML for Refinement Phase " ++ gmlArg ++ "...";
    selectGml gmlArg
    }
    
  when (not $ null vhdArg) $ do {
    putStrLn $ "Generating VHDL files for Refinement Phase " ++ gmlArg ++ "...";
    selectVhd vhdArg
    }   

  when (not $ null quaArg) $ do {
    putStrLn $ "Generating Quartus project files for Refinement Phase " ++ gmlArg ++ "...";
    selectQua quaArg
    }   
    
  where
    executeSim inFilePath outFile runArg = do
      inPath <- checkIfFile inFilePath
      _ <- checkIfDir  outFile;
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
      putStrLn $ "Executing and dumping the AESA output with PC in Refinement Phase " ++ runArg ++ "...";
      dumpData3 outFile showFloat oData;
      printDimen3 ("Dumped at "++ outFile ++" the last cube of AESA output: ") oData
    
    selectPC arg
      | arg == "1" = R1.pc'
      | arg == "2" = R2.wrappedPC''
      -- for simulation efficiency we do not simulate 'wrappedPC3' because the zipx . unzipx pattern would
      -- cause a memory overflow (32GB RAM are not enough!) we keep it within the list domain instead.
      | arg == "3" = wrapR2 (V.vector . map SDF.signal . simList R3.procPCSys . map SY.fromSignal . V.fromVector)
      | arg == "4" = wrapR2 (V.vector . map SDF.signal . simList R4.procPCSys' . map SY.fromSignal . V.fromVector)
      | otherwise  = error $ noArgErr arg
    selectGml arg
      | arg == "3" = R3.graphmlPC3
      | arg == "4" = R4.graphmlPC4
      | otherwise  = error $ noArgErr arg
    selectVhd arg
      | arg == "3" = R3.vhdlPC3
      | arg == "4" = R4.vhdlPC4
      | otherwise  = error $ noArgErr arg
    selectQua arg
      | arg == "3" = R3.quartusPC3
      | arg == "4" = R4.quartusPC4
      | otherwise  = error $ noArgErr arg
                 
    noArgErr arg = "No refinement stage " ++ arg ++ ". Re-run the program with --help to see the accepted input commands."
             
flags =
  [Option ['i'] ["input"]    (ReqArg InPath "PATH")
    "Path to input data file. Default: gen/AESA_INPUT.csv"
  ,Option ['o'] ["output"]   (ReqArg DumpPath "PATH")
    "Path to main generated file. Default: gen/AESA_OUT_S.csv"
  ,Option ['r'] ["run"]      (ReqArg Run "PHASE")
    ("Runs the AESA in one of the refinement phases [1-4]. Default 1.")
  ,Option ['g'] ["graphml"]  (ReqArg GraphML "PHASE")
    ("Dumps the internal structure of a ForSyDe-Deep model for refinement phases [3-4] as graphML files.")
  ,Option ['v'] ["vhdl"]     (ReqArg VHDL "PHASE")
    ("Generates the VHDL files for refinement phases [3-4].")
  ,Option ['q'] ["quartus"]  (ReqArg Quartus "PHASE")
    ("Generates the Quartus II project and verifies if the VHDL is synthesizable for refinement phases [3-4].")
  ,Option ['h'] ["help"]     (NoArg Help)
    "Print this help message"
  ]

data Flag
  = Run String       -- -r
  | GraphML String   -- -g
  | VHDL String      -- -v
  | Quartus String   -- -q
  | InPath String    -- -i
  | DumpPath String  -- -o
  | Help             -- --help
  deriving (Ord,Show)

instance Eq Flag where
  (Run _)          == (Run _)          = True
  (VHDL _)         == (VHDL _)         = True
  (GraphML _)      == (GraphML _)      = True
  (Quartus _)      == (Quartus _)      = True
  (InPath _)       == (InPath _)       = True
  (DumpPath _)     == (DumpPath _)     = True
  Help             == Help = True
  _ == _ = False


getFlagArg (Run s) = s
getFlagArg (VHDL s) = s
getFlagArg (GraphML s) = s
getFlagArg (Quartus s) = s
getFlagArg (InPath s) = s
getFlagArg (DumpPath s) = s
  
parse argv = case getOpt Permute flags argv of
               (args,_,[]) -> do
                 let select x = filter (==x) args
                     selArg x = let arg =  select (x "")
                                in if null arg then "" else getFlagArg (head arg)
                     runArg   = let p = (selArg Run)
                                in if null p && null gmlArg && null vhdArg && null quaArg then "1" else p
                     gmlArg   = (selArg GraphML)
                     vhdArg   = (selArg VHDL)
                     quaArg   = (selArg Quartus)
                     inPath   = let p = (selArg InPath)
                                in if null p then "gen/AESA_INPUT.csv" else p 
                     outFile  = let p = (selArg DumpPath)
                                in if null p
                                   then "gen/AESA_REFINE_R" ++ runArg ++".csv"
                                   else p 
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitSuccess
                   else return (inPath, outFile, runArg, gmlArg, vhdArg, quaArg)
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-deep [-iorgvq]"
