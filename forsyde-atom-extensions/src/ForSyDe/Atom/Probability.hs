{-# LANGUAGE FlexibleContexts, PostfixOperators #-}
{-# LANGUAGE GADTs #-}
module ForSyDe.Atom.Probability where

import ForSyDe.Atom.Utility
import System.Random

data Dist a where
  Dist ::  {recipe :: StdGen -> a} -> Dist a

instance Functor Dist where
  fmap f (Dist g) = Dist (f . g)

-------------- ATOMS -------------

(%.) :: Random b => (a -> b) -> Dist a -> Dist b
f %. (Dist g)          = Dist (f . g)

(%*) :: Random b => Dist (a -> b) -> Dist a -> Dist b
(Dist f) %* (Dist g) = Dist (\a -> (f a) (g a))

sample :: StdGen -> Dist a -> a
sample g (Dist r) = r g

------------ PATTERNS ------------

trans11 p v1                      = (p %. v1)
trans21 p v1 v2                   = (p %. v1 %* v2)
trans31 p v1 v2 v3                = (p %. v1 %* v2 %* v3)
trans41 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4)
trans51 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5)
trans61 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6)
trans71 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7)
trans81 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 %* v8)
trans12 p v1                      = (p %. v1 |<)
trans22 p v1 v2                   = (p %. v1 %* v2 |<)
trans32 p v1 v2 v3                = (p %. v1 %* v2 %* v3 |<)
trans42 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4 |<)
trans52 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5 |<)
trans62 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 |<)
trans72 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 |<)
trans82 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v5 %* v8 |<)
trans13 p v1                      = (p %. v1 |<<)
trans23 p v1 v2                   = (p %. v1 %* v2 |<<)
trans33 p v1 v2 v3                = (p %. v1 %* v2 %* v3 |<<)
trans43 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4 |<<)
trans53 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5 |<<)
trans63 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 |<<)
trans73 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 |<<)
trans83 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v5 %* v8 |<<)
trans14 p v1                      = (p %. v1 |<<<)
trans24 p v1 v2                   = (p %. v1 %* v2 |<<<)
trans34 p v1 v2 v3                = (p %. v1 %* v2 %* v3 |<<<)
trans44 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4 |<<<)
trans54 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5 |<<<)
trans64 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 |<<<)
trans74 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 |<<<)
trans84 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 %* v8 |<<<)

------------ UTILITIES ------------

mkStdGens :: StdGen -> [StdGen]
mkStdGens = map mkStdGen . randoms

-- buildGens :: (StdGen -> (a,StdGen)) -> StdGen -> [StdGen]
-- buildGens rand = go
--   where
--     go g = g' `seq` (g' : go g') where (_,g') = rand g
