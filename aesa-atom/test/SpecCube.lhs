 ## Properties

In this subsetion we formulate a handful of properties whose purpose is:

1. to test that the model implementation from @sec:cube-atom-operation does not
   violate in any circumstance these "contracts"; and

1. ensure that any future (iterative) model refinement does not alter or violate tese
   "contracts".

Below you find the code written in a runnable module found at `aesa-atom/test`, which
constitute the `:tests-cube` suite:

> {-# LANGUAGE PackageImports #-}
> module SpecCube where

 ### Imports

A couple of modules need to be imported before we can proceed. The
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) and
[Framework](http://hackage.haskell.org/package/HTF) modules a provide the test DSL as
well as a couple of handy utilities for writing and handling test suites.

> import Test.QuickCheck
> import Test.Framework
> import Test.Framework.Providers.QuickCheck2 (testProperty)

We import some relevant ForSyDe-Atom modules, mainly to get access to the internal
structure of ForSyDe types such as `Vector` or `Signal`.

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY

Obviously, we need to import the AESA designs modules as well.

> import AESA.CubesAtom
> import AESA.Coefs
> import AESA.Params

Finally, we import some in-house utilities which define `Arbitrary` instances for
ForSyDe types, as well as getters and setters to ease the definition of properties.

> import Util

 ### Property formulations



> prop_dbf_num_outputs :: (Antenna CpxData) -> Bool
> prop_dbf_num_outputs vs = V.length (fDBF vs) == nB

 ### Runner

> tests :: [Test]
> tests = [
>   testGroup "SY Tests"
>     [ testProperty "DBF  right number of outputs                            "
>       (withMaxSuccess 100 prop_dbf_num_outputs)       
>     ]
>   ]

> main :: IO()
> main = defaultMain tests

