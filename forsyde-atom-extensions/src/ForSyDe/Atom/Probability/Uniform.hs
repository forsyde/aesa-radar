{-# LANGUAGE GADTs #-}
module ForSyDe.Atom.Probability.Uniform where

import System.Random
import ForSyDe.Atom.Probability

uniform :: Random a => Dist a
uniform = Dist (fst . random)

uniformR :: Random a => a -> a -> Dist a
uniformR rl rh = Dist $ fst . randomR (rl,rh)

uniformD :: (Random a, Num a) => a -> a -> Dist a
uniformD dev a = Dist $ fst . randomR (a - dev , a + dev)
