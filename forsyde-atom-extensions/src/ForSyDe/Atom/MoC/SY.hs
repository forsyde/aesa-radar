{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.SY (
  module SY,
  fromSignal
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.SY as SY
import "forsyde-atom" ForSyDe.Atom.MoC.Stream

fromSignal = map SY.val . fromStream

