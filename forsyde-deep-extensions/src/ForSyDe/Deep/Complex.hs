{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable #-}
module ForSyDe.Deep.Complex where

import qualified Data.Complex as C
import Language.Haskell.TH.Lift
import Data.Data

data Complex a = Cpx (a, a) deriving (Show, Eq, Ord, Data)


instance Functor Complex where
  fmap f (Cpx (a,b)) = Cpx (f a, f b)

instance Num a => Num (Complex a) where
  Cpx (x,y) + Cpx (x',y') = Cpx (x+x', y+y')
  Cpx (x,y) - Cpx (x',y') = Cpx (x-x', y-y')
  Cpx (x,y) * Cpx (x',y') = Cpx (x*x'-y*y', x*y'+y*x')
  negate (Cpx (x,y))      = Cpx (negate x, negate y)
  abs                     = undefined
  signum                  = undefined
  fromInteger n           = Cpx (fromInteger n, 0)

cpx x y = Cpx (x,y)
fromDeepCpx (Cpx (x,y)) = x C.:+ y
toDeepCpx (x C.:+ y) = Cpx (x,y)

$(deriveLift1 ''Complex)

-- deriving instance (Data a) => Data (Complex a)
