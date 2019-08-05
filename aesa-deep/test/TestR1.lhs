 ### Properties

> {-# LANGUAGE PackageImports #-}
> module TestR1 where

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF

> import Test.QuickCheck as QC
> import AESA.StreamsAtom as M1
> import AESA.PC.R1 as R1

> import Data.List as L
> import Data.Complex
> import Data.Ratio
> import Util

> prop_refine1_equiv = forAll (largeSdfSigs arbitrary) $ \s -> equiv s (SDF.toSY1 s)
>   where
>     equiv :: (SDF.Signal (Rational) -> SY.Signal (Rational) -> Bool)
>     equiv sdf sy = all id $ zipWith (==)
>                    (SDF.fromSignal $ M1.procPC sdf)
>                    ( SY.fromSignal $ R1.pcFIR coefsR1 sy)
