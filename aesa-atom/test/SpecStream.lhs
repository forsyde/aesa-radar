 ## Checking System Properties {#sec:stream-prop-defs}

In @sec:props we preached the need for ways of verifying that an (executable) model
satisfies a set of specification properties, and that those properties are not being
violated during the refinement process. We presented `QuickCheck` as a practical and
easy means for formulating properties and validating them against randomly-generated
test cases. Following the same mindset, in this subsection we will try to validate
through simulation[^fn:check] that the semantics of the main blocks in the AESA signal
processing system defined in @sec:atom-network preserve the semantics of their initial
high-level description from @sec:cube-atom-operation, with respect to the properties
formulated in @sec:prop-defs.

[^fn:check]: actually for most of the properties in this section there are theories which can prove the preservation of semantics during certain transformations (e.g. the Bird-Merteens formalism) without the need to resort to simulation, but they are out of the scope of this report. This report rather focuses on common practices for disciplined design instead. 

Below you find the code written in a runnable module found at `aesa-atom/test`, which
constitute the `:tests-cube` suite:

> {-# LANGUAGE PackageImports #-}
> module SpecStream where

 ### Imports

We import the necessary libraries, such as
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) the two AESA midel
implementations `AESA.CubesAtom` and `AESA.StreamsAtom`, some ForSyDe-Atom libraries
and a couple of other utility libraries. We use the data generators defined in the
`Generators` module defined in @sec:prop-gens.

> import Test.QuickCheck as QC
> import Test.QuickCheck.Function
> import Test.Framework
> import Test.Framework.Providers.QuickCheck2 (testProperty)
> 
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M -- (Matrix, size)
> import ForSyDe.Atom.Skeleton.Vector.Cube as C -- (Cube)
> 
> import AESA.CubesAtom   as AESAC
> import AESA.StreamsAtom as AESAS
> import AESA.Coefs
> import AESA.Params
> 
> import Generators
> import Data.List as L
> import Data.Complex

 ### Properties

Perhaps the most noticeable transformation has been performed on the DBF module, since
all the primitive operations performed by the vector-matrix dot product have become
synchronous processes themselves. In order to show that the two models are in fact
sematically equivalent _with respect to the values and their structured order_, but
not necessarily to the causal order or time implications, we formulate a property
which degrades the signals and vectors to lists and compares them. Lists do not
capture any temporal or spatial semantics, but they do have a clearly-defined
_ordering_ relation, which is equally valuable in our case.

> prop_dbf_transf_equiv = forAll (sigOfCubes arbitrary) $ \sc -> same (cbRes sc) (stRes sc)
>   where
>     same c s = (all . all) id $ (zipWith . zipWith) (==) c s
>     cbRes sc = toLists $ V.farm11 unroll $ SY.unzipx $ SY.comb11 C.transpose' $ AESAC.dbf sc
>     stRes sc = toLists $ AESAS.dbf $ V.farm11 unroll $ SY.unzipx sc
>     toLists  = map SY.fromSignal . V.fromVector
>     unroll   = SY.signal . concatMap (concatMap fromVector . fromVector) .  SY.fromSignal


 ### Main function

Finally we gather all the properties defined in this section in a bundle of tests
called "Cube HL Model Tests", using the utilities provided by the `Test.Framework`
library. `withMaxSuccess` determines how many random tests will be generated per
property durin one run.

>     
> tests :: [Test]
> tests = [
>   testGroup " Stream HL Model Tests "
>     [ testProperty "DBF transformation preserves the order of elements         "
>       (withMaxSuccess 50 prop_dbf_transf_equiv)
>     ]
>   ]

> main :: IO()
> main = defaultMain tests

