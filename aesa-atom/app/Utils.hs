module Utils where

import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Lex.Fractional
import qualified Data.ByteString.Unsafe as S
import           Data.Complex
import           Data.Double.Conversion.ByteString
import           Data.List
import           System.Directory
import           System.FilePath

checkIfFile path = do
  existsPath    <- doesFileExist path
  if existsPath
    then return path
    else error $ "Did not find file '" ++ path ++ "'! Aborting..."

checkIfDir path = do
  let out = takeDirectory path
  createDirectoryIfMissing True out
  return out

readInData filePath = do
  inData <- S.readFile filePath
  return $ transpose $ map (getSamples) $ S.lines inData


sublist :: Int -> [a] -> [[a]]
sublist n = takeWhile ((==n) . length) . map (take n) . iterate (drop n)

stenList :: Int -> [a] -> [[a]]
stenList n v = map (take n) $ dropFromEnd n $ tails v
  where dropFromEnd n = take (length v - n + 1)
    
getSamples :: S.ByteString -> [Complex Float]
getSamples = getReal []
  where
    readSDecimal = readSigned readDecimal
    getReal n s = case readSDecimal s of
                    Nothing       -> n
                    Just (k,rst)  -> if S.null rst
                                     then error "You should not be here!"
                                     else getImag k n (S.tail rst)
    getImag r n s = case readSDecimal s of
                      Nothing     -> error "AAAA!"
                      Just (k,rst)-> if S.null rst
                                     then n ++ [r :+ k]
                                     else getReal (n ++ [r :+ k]) (S.tail rst)
                                          
showComplex :: Complex Float -> S.ByteString
-- showComplex x = ' ' `S.cons` toFixed 6 (realToFrac $ realPart x) `S.append` (' ' `S.cons` toFixed 6 (realToFrac $ imagPart x))
showComplex x = ' ' `S.cons` toShortest (realToFrac $ realPart x) `S.append` (' ' `S.cons` toShortest (realToFrac $ imagPart x))

showFloat :: Float -> S.ByteString
-- showFloat x = ' ' `S.cons` toFixed 8 (realToFrac x)
showFloat x = ' ' `S.cons` toShortest (realToFrac x)

printDimen2 str what = putStrLn $ str ++ (show $ length what) ++ " * " ++ (show $ length $ head what)
printDimen3 str what = let fb = head what 
                           fr = head fb
                       in putStrLn $ str ++ (show $ length what) ++ " * " ++ (show $ length fb)
                          ++ " * " ++ (show $ length fr) 
printDimen4 str what = let fb = head what 
                           fr = head fb
                           fw = head fr
                       in putStrLn $ str ++ (show $ length what) ++ " * " ++ (show $ length fb)
                          ++ " * " ++ (show $ length fr)  ++ " * " ++ (show $ length fw) 



dumpData2 file showNum list = S.writeFile file (toString2 list)
  where
    toString2 = S.unlines . map (S.concat . map showNum)
    
dumpData3 file showNum list = S.writeFile file (toString3 list)
  where
    toString3 = S.unlines . map (S.unlines . map (S.concat . map showNum))
