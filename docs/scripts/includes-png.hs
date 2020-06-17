#!/usr/bin/env stack exec runhaskell
import Text.Pandoc.JSON
import System.FilePath

main :: IO ()
main = toJSONFilter toSVG
  where
    toSVG (Image attr inline (trg,x))
      = let (name,ext) = splitExtension trg
        in if (ext == ".pdf")
           then Image attr inline (name ++ ".png", x)
           else Image attr inline (trg, x)
    toSVG x = x
