{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Generators where

import Test.QuickCheck as QC

import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF

import Data.List as L
import Data.Complex
import Data.Ratio
import ForSyDe.Deep.Fixed
import ForSyDe.Deep.Int
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

rationals :: Gen Rational
rationals = arbitrary

complexRationals :: Gen (Complex Rational)
complexRationals = do
  x <- arbitrary
  y <- arbitrary
  return (x :+ y)

sySignals :: Gen a -> Gen (SY.Signal a)
sySignals a = do
  sigData <- listOf a
  return (SY.signal sigData)


decimalCpxNum :: Gen (Complex Float)
decimalCpxNum = do
  realPart <- choose (-1,0.99999999999)
  imagPart <- choose (-1,0.99999999999)
  return (realPart :+ imagPart)

decimalCpxRat :: Gen (Complex Rational)
decimalCpxRat = do
  realPartNum <- choose (-2^31,2^31)
  imagPartNum <- choose (-2^31,2^31)
  return ((realPartNum % 2^32) :+ (imagPartNum % 2^32))


cpxFixed20 :: Gen (Complex Fixed20)
cpxFixed20 = do
  x <- choose (-1,2^20-1)
  y <- choose (-1,2^20-1)
  return (F20 (I20 x) :+ F20 (I20 y))  

nonNullVector :: Gen a -> Gen (Vector a)
nonNullVector a = do
  ld <- listOf a `suchThat` (not . L.null)
  return $ V.vector ld 

fracToFixed20 :: Rational -> Fixed20 
fracToFixed20 x = if a == 0
                  then F20 y 
                  else if x == -1.0
                       then F20 (2^(20-1))
                       else error "No Fixed20 Value"
  where    (a, b) = (numerator x, denominator x)        
           y      = fromIntegral ((2^(20-1)) * b)

fixed20ToFrac :: Fractional a => Fixed20 -> a
fixed20ToFrac = (/(2^(20-1))) . realToFrac . fixed20ToInt

