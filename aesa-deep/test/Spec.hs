module Main where

import Test.QuickCheck as QC
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import TestR1

-- -- | Generates non-null vectors, i.e. which satisfy 'forall v . length v 0'.
-- nonNullVector :: Gen a -> Gen (Vector a)
-- nonNullVector a = do
--   ld <- listOf a `suchThat` (not . L.null)
--   return $ V.vector ld 

-- largeSigs :: Gen (SDF.Signal Rational)
-- largeSigs = do
--   n <- choose (nb, nb * 2)
--   sigData <- vectorOf n arbitrary
--   return (SDF.signal sigData)

-- sigs :: Gen a -> Gen (SY.Signal a)
-- sigs a = do
--   sigData <- listOf a
--   return (SY.signal sigData)


-- decimalCpxNum :: Gen (Complex Float)
-- decimalCpxNum = do
--   realPart <- choose (-1,0.99999999999)
--   imagPart <- choose (-1,0.99999999999)
--   return (realPart :+ imagPart)

-- withinRangeComplex :: Ord a => a -> a -> Complex a -> Bool
-- withinRangeComplex a b c
--   | realPart c <  a = False
--   | imagPart c <  a = False
--   | realPart c >= b = False
--   | imagPart c >= b = False
--   | otherwise = True


-- prop_refine1_equiv = forAll largeSigs $ \s -> equiv s (SDF.toSY1 s)
--   where
--     equiv sdf sy = all id $ zipWith (==)
--                    (SDF.fromSignal $ M1.procPC sdf)
--                    (SY.fromSignal $ R1.pcFIR sy)


-- prop_refine2_values = forAll (sigs decimalCpxNum)
--                       $ \s -> all (withinRangeComplex (-1) 1) $ SY.fromSignal (wrPcFirNet nb (mkPcCoefs 5) s)

-- prop_refine2_error = forAll largeSigs
--                      $ \s -> all (\a -> let q = (1/2^18) in realPart a <= q && imagPart a <= q) $ zipWith (\a b-> abs (a - b)) (SY.fromSignal (pcFirNet nb (mkPcCoefs 5) s)) (SY.fromSignal (wrPcFirNet nb (mkPcCoefs 5) s))


tests :: [Test]
tests = [
  testGroup "PC refinements tests "
    [ testProperty "PC' is sequence equivalent with PC "
      (withMaxSuccess 100 prop_refine1_equiv)
    -- , testProperty "PC'' values are within range "
    --   (withMaxSuccess 100 prop_refine2_values)
    -- , testProperty "PC'' values are within range "
    --   (withMaxSuccess 100 prop_refine2_error)
    ]
  ]

main :: IO()
main = defaultMain tests
