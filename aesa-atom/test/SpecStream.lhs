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

[^fn:check]: actually for most of the properties in this section there are theories which can prove the preservation of semantics during certain transformations (e.g. the Bird-Merteens formalism) without the need to resort to simulation, but they are out of the scope of this report. This report rather focuses on common practices and tools for disciplined design instead. 

Below you find the code written in a runnable module found at `aesa-atom/test`, which
constitute the `:tests-cube` suite:

> {-# LANGUAGE PackageImports #-}
> module SpecStream where

 ### Imports

We import the necessary libraries, such as
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) the two AESA model
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
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M (transpose)
> import ForSyDe.Atom.Skeleton.Vector.Cube   as C (Cube, transpose, transpose')
> 
> import AESA.CubesAtom   as AESAC
> import AESA.StreamsAtom as AESAS
> import AESA.Params
> 
> import Generators
> import Data.List as L
> import Data.Complex

 ### Properties {#sec:atom-stream-props}

Perhaps the most noticeable transformation has been performed on the DBF module, since
all the primitive operations performed by the vector-matrix dot product have become
synchronous processes instead. In order to show that the two models are in fact
sematically equivalent _with respect to the values and their structured order_, but
not necessarily to the causal order or time implications, we formulate a property
which degrades the signals and vectors to lists and compares them. Lists do not
capture any temporal or spatial semantics, but they do have a clearly-defined
_ordering_ relation, which is equally valuable in our case. The formulations below
follow the notation from @sec:prop-notation, and we denote the vector dimensions `Rn`
for _range_, `Wn` for _window_, `An` for _antenna_, `Bm` for _beam_ and `Ps` for
_pulse_.
$$
\forall c \in \langle\langle\langle\mathbb{C}\rangle_{\mathtt{Rn}}\rangle_{\mathtt{Wn}}\rangle_{\mathtt{An}} \Rightarrow \Sigma_{\mathtt{An}\rightarrow\mathtt{Wn}\rightarrow\mathtt{Rn}}(\mathtt{dbf}_{\text{cube}}(\overline{c})) =
\Sigma_{\mathtt{An}\rightarrow\mathtt{Wn}\rightarrow\mathtt{Rn}}(\mathtt{dbf}_{\text{stream}}(\overline{c}))
$$ {#eq:prop_dbf_transf_equiv}


> prop_dbf_transf_equiv = forAll (sigOfSmallCubes arbitrary) $ \sc -> same (cbRes sc) (stRes sc)
>   where
>     -- Compare all the values in the resulting nested lists
>     same c s = (all . all) id $ (zipWith . zipWith) (==) c s
>     -- Results of the cube version of DBF as nested lists. Outputs are re-arranged
>     cbRes sc = toLists $ V.farm11 unroll $ SY.unzipx $ SY.comb11 C.transpose' $ AESAC.dbf sc
>     -- Results of the stream version of DBF as nested lists. Inputs are re-arranged 
>     stRes sc = toLists $ AESAS.dbf $ V.farm11 unroll $ SY.unzipx sc
>     -- Transforms a vector of signals into a list of lists
>     toLists  = map SY.fromSignal . V.fromVector
>     -- Flattens a signal of matrices into a signal of ordered samples
>     unroll   = SY.signal . concatMap (concatMap fromVector . fromVector) .  SY.fromSignal

Let us now test the transformed PC block for the same property. Although the innermost
operation is still `fir`, we would like to make sure that changing the MoC domain of
the process from SY (on vectors) to SDF (on tokens) did affect in any way the ordering
(and values) of the data.
$$
\forall c \in \langle\langle\langle\mathbb{C}\rangle_{\mathtt{Rn}}\rangle_{\mathtt{Wn}}\rangle_{\mathtt{Bm}},
r\in c_{\mathtt{Rn}} : |r| = N_b \Rightarrow
\Sigma_{\mathtt{Bm}\rightarrow\mathtt{Wn}\rightarrow\mathtt{Rn}}(\mathtt{pc}_{\text{cube}}(\overline{c})) =
\Sigma_{\mathtt{Bm}\rightarrow\mathtt{Wn}\rightarrow\mathtt{Rn}}(\mathtt{pc}_{\text{stream}}(\overline{c}))
$$ {#eq:prop_pc_transf_equiv}

> -- We need an custom generator for cubes with 'nb'-sized range vectors
> sigOfRnCubes :: Gen (SY.Signal (C.Cube (Complex Float)))
> sigOfRnCubes = do
>   rangeL  <- elements [nb]  -- range vector needs to have exactly 'nb' samples  
>   windowL <- choose (2, 10)  
>   beamL   <- choose (2, 10)  
>   sigData <- listOf1 $ sizedCube beamL windowL rangeL arbitrary
>   return (SY.signal sigData)

> prop_pc_transf_equiv = forAll sigOfRnCubes $ \sc -> same (cbRes sc) (stRes sc)
>   where
>     -- Compare all the values in the resulting nested lists
>     same c s = (all . all) id $ (zipWith . zipWith) (==) c s
>     -- Results of the cube version of DBF as nested lists. Outputs are re-arranged
>     cbRes sc = syToLst $ V.farm11 unroll $ SY.unzipx
>                -- inverse the transposes during DBF and PC, align cubes with streams
>                $ SY.comb11 M.transpose $ AESAC.pc $ SY.comb11 C.transpose sc
>     -- Results of the stream version of DBF as nested lists. Inputs are re-arranged 
>     stRes sc = sdfToLst $ AESAS.pc $ V.farm11 unroll $ SY.unzipx sc
>     -- Transforms a vector of signals into a list of lists
>     syToLst  = map SY.fromSignal  . V.fromVector
>     sdfToLst = map SDF.fromSignal . V.fromVector
>     -- Flattens a signal of matrices into a signal of ordered samples
>     unroll   = SY.signal . concatMap (concatMap fromVector . fromVector) .  SY.fromSignal

_N.B.:_ after a few iterations we realized that the property `prop_pc_transf_equiv`
would not hold $\forall c \in \langle\langle\langle\mathbb{C}\rangle\rangle\rangle$
because of the consumption rules of `SDF.comb11`. The streaming version of PC would
not produce the same result as the cube version had the `Rn` dimension not been an
integer multiple of the consumption rate $N_b$, simply because if there are not enough
tokens at the input, a SDF actor does not execute, whereas a scalable vector operation
is evaluated regardless of how many elements it operates on. We thus had to adjust the
property in @eq:prop_pc_transf_equiv accordingly having this insight, which means that
we _restrict_ the legal inputs pool to fit more to the specifications in
@sec:aesa-parameters, otherwise we cannot guarantee the property above.

When evaluating the corner turning (CT) it is now easier to verify the 50% overlap
because we have access to the streams directly. We thus formulate the property
$$
\forall \overline{a} \text{ large enough}, v\in \langle\alpha\rangle_{\mathtt{Wn}} :
|v|=N_{FFT} \wedge \mathtt{ct}(\overline{a}) = (\overline{v}_{\text{right}},\overline{v}_{\text{left}}) \Rightarrow
v_{\text{right}}[i] = v_{\text{left}}[i+\frac{N_{FFT}}{2}]
$$ {#eq:prop_ct_50_overlap}

> -- We need a generator for signals of size larger than 'nFFT/2'
> largeSigs :: Gen (SDF.Signal Int)
> largeSigs = do
>   n <- choose (nb * nFFT `div` 2, nb * nFFT)
>   sigData <- vectorOf n arbitrary
>   return (SDF.signal sigData)

> prop_ct_50_overlap = forAll largeSigs $ \s -> over (AESAS.procCT s)
>   where
>     over (rc,lc) = all id $ SDF.fromSignal
>                    $ SDF.comb21 ((nFFT,nFFT), nFFT `div` 2, overF) rc lc
>     overF rl ll  = zipWith (==) rl (L.drop (nFFT `div` 2) ll)

For the rest of the blocks, DFB, CFAR and INT we can follow the model set by
`prop_pc_transf_equiv` to formulate properties testing for functional equivalence. We
only mention these properties as formulas, and we leave the writing of the QuickCheck
code as an exercise to the reader.
$$
\forall c \in \langle\langle\langle\mathbb{C}\rangle_{\mathtt{Wn}}\rangle_{\mathtt{Rn}}\rangle_{\mathtt{Bm}},
w\in c_{\mathtt{Wn}} : |w| = N_{FFT} \Rightarrow
\Sigma_{\mathtt{Bm}\rightarrow\mathtt{Rn}\rightarrow\mathtt{Ps}}(\mathtt{dfb}_{\text{cube}}(\overline{c})) =
\Sigma_{\mathtt{Bm}\rightarrow\mathtt{Rn}\rightarrow\mathtt{Ps}}(\mathtt{dfb}_{\text{stream}}(\overline{c}))
$$ {#eq:prop_dfb_transf_equiv}
$$
\forall c \in \langle\langle\langle\mathbb{C}\rangle_{\mathtt{Ps}}\rangle_{\mathtt{Rn}}\rangle_{\mathtt{Bm}},
w\in c_{\mathtt{Wn}}, r\in c_{\mathtt{Rn}} : |w| = N_{FFT} \wedge  |r| = N_{b} \Rightarrow
\Sigma_{\mathtt{Bm}\rightarrow\mathtt{Rn}\rightarrow\mathtt{Ps}}(\mathtt{cfar}_{\text{cube}}(\overline{c})) =
\Sigma_{\mathtt{Bm}\rightarrow\mathtt{Rn}\rightarrow\mathtt{Ps}}(\mathtt{cfar}_{\text{stream}}(\overline{c}))
$$ {#eq:prop_cfar_transf_equiv}
$$
\forall c_1,c_2 \in \langle\langle\langle\mathbb{C}\rangle\rangle\rangle \Rightarrow
\Sigma(\mathtt{int}_{\text{cube}}(\overline{c_1},\overline{c_2})) =
\Sigma(\mathtt{int}_{\text{stream}}(\overline{c_1},\overline{c_2}))
$$ {#eq:prop_int_transf_equiv}

 ### Main function. Test Suite Results

We gather the QuickCheck properties defined above into one runnable suite:

> tests :: [Test]
> tests = [
>   testGroup " Stream HL Model Tests "
>     [ testProperty "DBF transformation preserves the order of elements "
>       (withMaxSuccess 50 prop_dbf_transf_equiv)
>     , testProperty "PC transformation preserves the order of elements  "
>       (withMaxSuccess 20 prop_pc_transf_equiv)
>     , testProperty "CT 50% overlap                                     "
>       (withMaxSuccess 20 prop_ct_50_overlap)
>     ]
>   ]

> main :: IO()
> main = defaultMain tests

which we execute, as per the instructions in the project's `README` file. Since we are
generating random tests on cubes of noticeable dimensions, the execution time is also
quite long, hence the smaller number of test cases generated per suite. The expected
printout is:

	aesa-atom-0.1.0.0: test (suite: tests-stream)
	
	 Stream HL Model Tests :
	  DBF transformation preserves the order of elements : [OK, passed 50 tests]
	  PC transformation preserves the order of elements  : [OK, passed 20 tests]
	  CT 50% overlap                                     : [OK, passed 20 tests]
	
             Properties  Total      
	 Passed  3           3          
	 Failed  0           0          
	 Total   3           3          
	
	aesa-atom-0.1.0.0: Test suite tests-stream passed

This means that, at least when concerning the functionality of the AESA signal video
processing chain, we have not introduced any unwanted flaws with our model refinement
process. We also have a better understanding of the conditions and restrictions within
which the new refined model operates as expected.
