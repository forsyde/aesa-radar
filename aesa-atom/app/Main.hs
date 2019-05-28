module Main where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

import Control.Exception
import System.CPUTime
-- import Control.Monad
-- import Control.Parallel.Strategies
-- import Control.DeepSeq

import ForSyDe.AESA.HighLevelAtom
-- import ForSyDe.AESA.Params
-- import ForSyDe.AESA.Coefs

import Utils

main = do
  (n,or,imp) <- getArgs >>= parse
  putStrLn "hallo"
  -- jfkGen <- generate arbitrary :: IO (Signal Flight)
  -- laxGen <- generate arbitrary :: IO (Signal Flight)
  -- ordGen <- generate arbitrary :: IO (Signal Flight)
  -- let jfkS = clean JFK jfkGen
  --     laxS = clean LAX laxGen
  --     ordS = clean ORD ordGen
  -- mapM_ (executeModel n or jfkS laxS ordS) imp
  -- where
  --   clean :: Airport -> Signal Flight -> Signal Flight
  --   clean name NullS = NullS
  --   clean name (s@(E _ x):-ss)
  --     | dest x == name = clean name ss
  --     | otherwise      = s :- clean name ss
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
            

data Flag
  = GenInput            -- -g
  | GenScript String    -- --gen-path
  | PlotPath String     -- --plot-path
  | DumpPath String     -- -o
  | Intermediate String -- --inter
  | Help                -- --help
  deriving (Eq,Ord,Show)
 
flags =
  [Option ['g'] ["gen-data"] (NoArg GenInput)
    "Invokes the antenna input data generator script."
  ,Option [] ["gen-path"]    (ReqArg GenScript "PATH")
    "Path to input generator script. Default: scripts/generate-input.py"
  ,Option [] ["plot-path"]   (ReqArg PlotPath "PATH")
    "Path to output plotter script. Default: scripts/plot-output.py"
  ,Option ['o'] ["out"]      (ReqArg DumpPath "PATH")
    "Runs the concurrent implementation. Default: out"
  ,Option [] ["inter"]       (ReqArg DumpPath "STAGE")
    ("Dumps and plots data at intermediate stages. Options:\n" ++
       "d: after DBF stage\n" ++
       "p: after PC stage\n" ++
       "c: after CT stage\n" ++
       "f: after DFB stage\n" ++
       "a: after CFAR stage")
  ,Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  ]
  
parse argv = case getOpt Permute flags argv of
               (args,_,[]) -> do
                 if Help `elem` args
                   then do hPutStrLn stderr (usageInfo header flags)
                           exitWith ExitSuccess
                   else return (1, 2, 3)
  
               (_,_,errs)   -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
 
  where header = "Usage: aesa-hl [-gh|--out=PATH|--inter=[dpcfa]|...]"
 

-- lim :: Int
-- lim = 10^5

-- time :: (NFData t) => (Signal t,Signal t,Signal t)  -> IO (Double)
-- time y = do
--     start <- getCPUTime
--     -- replicateM_ lim $ do
--     x <- evaluate $ y
--     rnf x `seq` return ()
--     end   <- getCPUTime
--     let diff = (fromIntegral (end - start)) / (10^12)
--     -- printf "Computation time: %0.9f sec\n" (diff :: Double)
--     -- printf "Individual time: %0.9f sec\n" ( :: Double)
--     return (diff / fromIntegral lim)


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
