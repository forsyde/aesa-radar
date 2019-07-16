{-# LANGUAGE PackageImports #-}
module Main where

import Test.QuickCheck as QC
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF

import AESA.StreamsAtom as M1
import AESA.PC.FIR as R1
import AESA.Params
import AESA.Coefs

import Data.List as L
import Data.Complex
import Data.Ratio


-- | Generates non-null vectors, i.e. which satisfy 'forall v . length v 0'.
nonNullVector :: Gen a -> Gen (Vector a)
nonNullVector a = do
  ld <- listOf a `suchThat` (not . L.null)
  return $ V.vector ld 

largeSigs :: Gen (SDF.Signal Rational)
largeSigs = do
  n <- choose (nb, nb * 2)
  sigData <- vectorOf n arbitrary
  return (SDF.signal sigData)


prop_refine1_equiv = forAll largeSigs $ \s -> equiv s (SDF.toSY1 s)
  where
    equiv sdf sy = all id $ zipWith (==)
                   (SDF.fromSignal $ R1.procPC' nb (mkPcCoefs 5) sdf)
                   (SY.fromSignal $ R1.pcFirNet nb (mkPcCoefs 5) sy)



tests :: [Test]
tests = [
  testGroup "PC refinements tests "
    [ testProperty "PC' is sequence equivalent with PC "
      (withMaxSuccess 100 prop_refine1_equiv)
    ]
  ]

main :: IO()
main = defaultMain tests
