{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.TimeStamp (
  module Ts,
  timeStamp
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.TimeStamp as Ts

timeStamp :: Real a => a -> TimeStamp
timeStamp = realToFrac
