{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.SY (
  module SY,
  fromSignal, interleave, unzipx, zipx
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.SDF as SDF hiding (unzipx,unzipx',zipx)
import "forsyde-atom" ForSyDe.Atom.MoC.SY as SY hiding (unzipx,unzipx',zipx)
import "forsyde-atom" ForSyDe.Atom.MoC.Stream
import                ForSyDe.Atom.Skeleton.Vector as V
import Data.List

fromSignal = map SY.val . fromStream

interleave :: SY.Signal a -> SY.Signal a -> SY.Signal a
interleave a b = SDF.toSY1 $ SDF.comb21 ((1,1),2,(++)) (SY.toSDF a) (SY.toSDF b)

unzipx :: SY.Signal (Vector a) -> Vector (SY.Signal a)
unzipx = vector . map SY.signal . transpose . map fromVector . fromSignal

zipx :: Vector (SY.Signal a) -> SY.Signal (Vector a)
zipx = SY.signal . map vector . transpose . map fromSignal . fromVector
