{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.SY (
  module SY,
  fromSignal, interleave
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.SDF as SDF
import "forsyde-atom" ForSyDe.Atom.MoC.SY as SY
import "forsyde-atom" ForSyDe.Atom.MoC.Stream
import                ForSyDe.Atom.Skeleton.Vector as V

fromSignal = map SY.val . fromStream

interleave :: SY.Signal a -> SY.Signal a -> SY.Signal a
interleave a b = SDF.toSY1 $ SDF.comb21 ((1,1),2,(++)) (SY.toSDF a) (SY.toSDF b)
