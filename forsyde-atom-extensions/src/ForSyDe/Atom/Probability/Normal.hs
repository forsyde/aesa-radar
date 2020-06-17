module ForSyDe.Atom.Probability.Normal where

import System.Random
import qualified Data.Random.Normal as N
import ForSyDe.Atom.Probability

normal' :: (Random a, Floating a) => Dist a
normal' = Dist (fst . N.normal)

normal :: (Random a, Floating a) => a -> a -> Dist a
normal dev m = Dist $ fst . N.normal' (m,dev)
