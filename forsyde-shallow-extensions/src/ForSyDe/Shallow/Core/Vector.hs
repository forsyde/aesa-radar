{-# LANGUAGE PackageImports #-}

module ForSyDe.Shallow.Core.Vector(
  module Vector, recurV, recuriV, zipWith4V, infiniteV
) where

import "forsyde-shallow" ForSyDe.Shallow.Core.Vector as Vector

recurV  ps s  = mapV (`pipeV` s) (tailsV ps)
recuriV  ps s = mapV (`pipeV` s) (tailsV $ ps <: id)

-- | The higher-order function 'zipWith4V' applies a function 4-tuple-wise on four vectors.
zipWith4V :: (a -> b -> c -> d -> e)
          -> Vector a  -- ^ /length/ = @la@
          -> Vector b  -- ^ /length/ = @lb@
          -> Vector c  -- ^ /length/ = @lc@
          -> Vector d  -- ^ /length/ = @ld@
          -> Vector e  -- ^ /length/ = @minimum [la,lb,lc,ld]@
zipWith4V f (x:>xs) (y:>ys) (z:>zs)  (q:>qs) = f x y z q :> (zipWith4V f xs ys zs qs)
zipWith4V _ _ _ _ _ = NullV

infiniteV = vector . repeat
