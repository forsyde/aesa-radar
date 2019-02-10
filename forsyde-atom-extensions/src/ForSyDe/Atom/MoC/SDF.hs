{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.SDF (
  module SDF,
  distribute, gatherAndStream
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.SDF as SDF
import ForSyDe.Atom.Skeleton.Vector as V

distribute :: Vector Cons -> SDF.Signal a -> Vector (SDF.Signal a)
distribute vc = SDF.unzipx vc . SDF.comb11 (l,1, (:[]) . vector)
  where l = V.reduce (+) vc

gatherAndStream :: Vector Prod -> Vector (SDF.Signal a) -> SDF.Signal a
gatherAndStream vp = SDF.comb11 (1,l,fromVector . head) . SDF.zipx vp
  where l = V.reduce (+) vp
