{-# LANGUAGE PackageImports #-}
module Util where

import Test.QuickCheck

import qualified "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP
import ForSyDe.Atom.MoC.Stream

-- import Data.List.Split
import Data.Complex


instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = do
    x <- arbitrary `suchThat` (not . null)
    return (V.vector x)

instance Arbitrary a => Arbitrary (Stream a) where
  arbitrary = do
    x <- arbitrary
    return (stream x)

instance Arbitrary a => Arbitrary (SY.SY a) where
  arbitrary = do
    x <- arbitrary
    return (SY.SY x)
    
instance Arbitrary a => Arbitrary (SDF.SDF a) where
  arbitrary = do
    x <- arbitrary
    return (SDF.SDF x)
