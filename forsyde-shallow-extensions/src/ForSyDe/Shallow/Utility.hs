{-# LANGUAGE PackageImports #-}

module ForSyDe.Shallow.Utility(
  module U,
  module ForSyDe.Shallow.Utility.Matrix,
  module ForSyDe.Shallow.Utility.Cube,
  module ForSyDe.Shallow.Utility.FIR,
  module ForSyDe.Shallow.Utility.DSP,
  interleaveSY
) where

import ForSyDe.Shallow
import ForSyDe.Shallow.Core.Vector (atV, vector)
import "forsyde-shallow" ForSyDe.Shallow.Utility as U hiding (atMat)
import ForSyDe.Shallow.Utility.Matrix
import ForSyDe.Shallow.Utility.Cube
import ForSyDe.Shallow.Utility.DSP
import ForSyDe.Shallow.Utility.FIR

interleaveSY :: Signal a -> Signal a -> Signal a
interleaveSY = actor21SDF (1,1) 2 (++)
