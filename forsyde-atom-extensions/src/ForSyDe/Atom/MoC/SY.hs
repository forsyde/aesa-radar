{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.SY (
  module SY,
  fromSignal, interleave, unzipx, zipx,
  toDE1, toDE2, toDE3, toDE4
  ) where

import                Data.List
import "forsyde-atom" ForSyDe.Atom.MoC.SDF as SDF hiding (unzipx,unzipx',zipx)
import "forsyde-atom" ForSyDe.Atom.MoC.SY as SY hiding
       (unzipx,unzipx',zipx, toDE, toDE2, toDE3, toDE4)
import "this"         ForSyDe.Atom.MoC.SY.Interface
import "forsyde-atom" ForSyDe.Atom.MoC.Stream
import "this"         ForSyDe.Atom.MoC.TimeStamp
import                ForSyDe.Atom.Skeleton.Vector as V

fromSignal = map SY.val . fromStream

interleave :: SY.Signal a -> SY.Signal a -> SY.Signal a
interleave a b = SDF.toSY1 $ SDF.comb21 ((1,1),2,(++)) (SY.toSDF a) (SY.toSDF b)

unzipx :: SY.Signal (Vector a) -> Vector (SY.Signal a)
unzipx = vector . map SY.signal . transpose . map fromVector . fromSignal

zipx :: Vector (SY.Signal a) -> SY.Signal (Vector a)
zipx = SY.signal . map vector . transpose . map fromSignal . fromVector


