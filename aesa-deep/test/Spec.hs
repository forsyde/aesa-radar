module Main where

import Test.QuickCheck as QC
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import TestR1
import TestR2
import TestR3
import TestR4

tests :: [Test]
tests = [
  testGroup "PC refinements tests "
    [ testProperty "PC' is sequence equivalent with PC "
      (withMaxSuccess 100 prop_refine1_equiv)
    , testProperty "PC'' values are within legal range "
      (withMaxSuccess 100 prop_refine2_values)
    , testProperty "PC'' acceptable cumulative errors  "
      (withMaxSuccess 1000 prop_refine2_error)
    , testProperty "PC3 is equivalent with PC''        "
      (withMaxSuccess 100 prop_refine3_equiv)
    , testProperty "PC4 is equivalent with PC3         "
      (withMaxSuccess 100 prop_refine4_equiv)
    ]
  ]

main :: IO()
main = defaultMain tests
