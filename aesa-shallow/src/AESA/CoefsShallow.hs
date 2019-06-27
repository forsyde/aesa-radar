{-# LANGUAGE PackageImports #-}
module AESA.CoefsShallow (mkBeamConsts, mkPcCoefs, mkWeightCoefs, mkIntCoefs, maxFloat) where

import AESA.Coefs hiding (mkBeamConsts, mkPcCoefs, mkWeightCoefs, mkIntCoefs)
import "forsyde-shallow-extensions" ForSyDe.Shallow.Core.Vector as V
import "forsyde-shallow-extensions" ForSyDe.Shallow.Utility
import Data.Complex

mkBeamConsts :: RealFloat a
             => a                  -- ^ distance between radar elements
             -> a                  -- ^ radar signal wavelength
             -> Int                -- ^ Number of antenna elements
             -> Int                -- ^ Number of resulting beams
             -> Matrix (Complex a)
mkBeamConsts d l nA nB = vector $ map vector $ mkBeamConsts' d l nA nB

mkPcCoefs :: Fractional a => Int -> Vector a
mkPcCoefs = vector . mkPcCoefs'

mkWeightCoefs :: Fractional a => Int -> Vector a
mkWeightCoefs = vector . mkWeightCoefs'

mkIntCoefs :: Fractional a => Vector a
mkIntCoefs = vector mkIntCoefs'

