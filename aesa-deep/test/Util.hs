{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Util where

import Test.QuickCheck as QC

import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF

import Data.List as L
import Data.Complex
import Data.Ratio

import AESA.Params


largeSySigs :: Gen a -> Gen (SY.Signal a)
largeSySigs g = do
  n <- choose (nb, nb*3+1)
  sigData <- vectorOf n g
  return (SY.signal sigData)

largeSdfSigs :: Gen a -> Gen (SDF.Signal a)
largeSdfSigs g = do
  n <- choose (nb, nb*3+1)
  sigData <- vectorOf n g
  return (SDF.signal sigData)

-- complexRationals :: Gen (Complex Rational)
-- complexRationals = arbitrary
