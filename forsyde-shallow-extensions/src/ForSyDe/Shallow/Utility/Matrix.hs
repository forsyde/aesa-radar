{-# LANGUAGE PackageImports #-}

module ForSyDe.Shallow.Utility.Matrix(
  module M,
  atMat, infiniteMat
) where

import ForSyDe.Shallow.Core.Vector (atV, vector)
import "forsyde-shallow" ForSyDe.Shallow.Utility.Matrix as M hiding (atMat)

-- | Returns the element of a matrix at a certain position.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> atMat 2 1 m
-- 13
atMat :: Matrix a
      -> (Int, Int)  -- ^ @(X,Y)@ position
      -> a
mat `atMat` (x,y) = (mat `atV` y) `atV` x

infiniteMat  = vector . repeat . vector . repeat
