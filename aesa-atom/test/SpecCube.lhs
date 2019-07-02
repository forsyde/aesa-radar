 ## Properties {#sec:prop-defs}

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

> import Test.QuickCheck as QC
> import Test.QuickCheck.Function
> import Test.Framework
> import Test.Framework.Providers.QuickCheck2 (testProperty)

We import some relevant ForSyDe-Atom modules, mainly to get access to the internal
structure of ForSyDe types such as `Vector` or `Signal`.

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import qualified ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import qualified ForSyDe.Atom.Skeleton.Vector.Cube as C
> import ForSyDe.Atom.Skeleton.Vector.DSP (fir)

Obviously, we need to import the AESA designs modules as well.

> import AESA.CubesAtom
> import AESA.Coefs
> import AESA.Params

Finally, we import some in-house utilities which define `Arbitrary` instances for
ForSyDe types, as well as getters and setters to ease the definition of properties, as
well as some Haskell data type libraries.

> import Util
> import Data.List as L
> import Data.Complex

 ### Formulations

The first property we want to check is that the main function in DBF (see
@sec:cube-dbf-atom), $f_{DBF}$ will always yield $n_B$ beam samples, no matter what or
how many inputs it has. Using the notation from @sec:prop-notation we can formalize
this property as follows:
$$
\forall v \in \langle\mathbb{C}\rangle : |v| > 0 \Rightarrow |f_{DBF}(v)|=n_B
$$ {#eq:prop_dbf_num_outputs}

First of all, we need to instantiate a generator that greates non-null vectors from a
base type, i.e. the generated vectors will always satisfy $\forall
v\in\langle\alpha\rangle:|v|>0$, which we will use all across this module:

> nonNullVector :: Gen a -> Gen (Vector a)
> nonNullVector a = do
>   ld <- listOf a `suchThat` (not . L.null)
>   return $ V.vector ld 

Now to translate @eq:prop_dbf_num_outputs to QuickCheck code:

> prop_dbf_num_outputs = forAll (nonNullVector arbitrary)
>                        $ \v -> V.length (fDBF v) == nB

This property ensures that the `Beam` dimension is respected, but what about the
`Range` and `Pulse` dimensions of the indata video cube? These dimensions must not be
altered. This is easy to prove, since the vector on those dimensions undergo a set of
nested `farm` transformations, which _esure by definition_ that they do not alter the
structure of the input data. However, let us be skeptical and assume that the library
might be faulty, since `farm` is such an essential skeleton in the AESA system.
A property which verifies that `farm` does not alter the structure of its
input type, and thus any $n$-dimensional vector (e.g. `Matrix`, `Cube`) undergoing a
`farm11` keeps its original dimensions, can be formulated as:
$$
\forall v \in \langle\mathbb{C}\rangle \Rightarrow |\texttt{farm11}(f,v)| = |v|
$$ {#eq:prop_generic_farm_structure}

> prop_generic_farm_structure :: Fun Int Int -> Vector Int -> Bool
> prop_generic_farm_structure f v = V.length v == V.length (farm11 (apply f) v)

Notice that if there are no special pre-conditions for the input data, e.g. $\forall v
\in \langle\mathbb{C}\rangle$, we don't need to invoke our own generator with the
`forAll` keyword, but rather just specify de input type and QuickCheck will
automatically call the default `arbitrary` generator for that particular type. Also,
we enable QuickCheck to generate _arbitrary unary functions_ along with arbitrary
data, by using its `Fun a b = Fun {apply :: a -> b}` function wrapper.

Another property we want to make sure is not violated during any stage in the
refinement flow is that the DBF block does not produce overflown numbers, i.e. the
beam coefficients are well-scaled. At this abstraction level, we do not really
consider the number representation, and for what it's worth we can assume that our
type `CpxData` is in fact $\mathbb{C}=a+bi$ where $a,b\in\mathbb{R}$. However, we
_know_ from the specifications that the input values $\forall a\in\mathbb{C}:a>=-1-i
\wedge a<1+i$ and that we eventually need to find efficient implementation for these
number representations. The engineering intuition/experience tells that a more
efficient representation would deal only with decimal numbers and would not need to be
concerned with the integer part (e.g. Qn fixed point representation). Thus, at the
functional level we need to ensure that the outputs themselves remain within the
$[-1-i,1+i)$ value pool as well, in order to avoid overflow in an arbitrary number
representation.

First we need to instantiate a number generator for complex numbers within
$[-1-i,1+i)$:

> decimalCpxNum :: Gen CpxData
> decimalCpxNum = do
>   realPart <- choose (-1,0.99999999999)
>   imagPart <- choose (-1,0.99999999999)
>   return (realPart :+ imagPart)

For the function $f_{DBF}$, the property ensuring legal value bounds would be
formulated as:
$$
\forall v \in \langle\mathbb{C}\rangle, a \in v, b \in f_{DBF}(v) : |v| > 0 \wedge a \in [-1-i,1+i) \Rightarrow b \in [-1-i,1+i)
$$ {#eq:prop_dbf_value_range}

which translates into QuickCheck code[^fn:range] to:

> prop_dbf_value_range = forAll (nonNullVector decimalCpxNum)
>                        $ \v -> all (withinRangeComplex (-1) 1) $ V.fromVector (fDBF v)

[^fn:range]: using our in-house `withinRangeComplex` defined in the `Util` module

Recall that in @sec:cube-dbf-atom we have said that $f_{DBF}$ is the equivalent of a
simple vector-matrix dot operation, and provided a simplified definition $f_{DBF}'$
using the library-provided `dotvm` function. But _are_ $f_{DBF}$ and $f_{DBF}'$ really
equivalent? We test this out by formulating a property:
$$
\forall v \in \langle\mathbb{C}\rangle \Rightarrow f_{DBF}(v) = f_{DBF}'(v)
$$ {#eq:prop_dbf_value_range}

> prop_dbf_func_equiv :: V.Vector CpxData -> Bool
> prop_dbf_func_equiv v = fDBF v == fDBF' v

Next we target the PC component (see @sec:cube-pc-atom). The problem of dimension
preserving has been solved by the previous `prop_generic_farm_structure` however,
keeping our skeptical attitude, we want to check that the library-provided `fir`
function performing a moving average, does not alter the dimensions of the input data

$$
\forall v \in \langle\mathbb{C}\rangle \Rightarrow |f_{PC}(v)| = |v|
$$ {#eq:prop_generic_farm_structure}

> prop_pc_num_outputs :: Vector CpxData -> Bool
> prop_pc_num_outputs v = V.length v == V.length (fPC v)

or that it is indeed having the right response to an impulse sequence:
$$\forall v \in \langle\mathbb{C}\rangle, i=\langle 1, 0,...\rangle \Rightarrow \texttt{fir} (v,i) = v
$$ {#eq:prop_generic_farm_structure}

> prop_pc_fir_response :: Vector CpxData -> Bool
> prop_pc_fir_response v = and $ zipWith (==) coefs response
>   where
>     coefs    = fromVector v
>     response = L.reverse $ fromVector $ fir v impulse
>     impulse  = V.vector $ L.reverse $ 1 : replicate 100 0

Furthernire we need to check that the FIR coefficients are scaled correctly and the
outputs are within the legal range to avoid future possible overflows.
$$
\forall v \in \langle\mathbb{C}\rangle, a \in v, b \in f_{PC}(v) : |v| > 0 \wedge a \in [-1-i,1+i) \Rightarrow b \in [-1-i,1+i)
$$ {#eq:prop_pc_value_range}

> prop_pc_value_range = forAll (nonNullVector decimalCpxNum)
>                       $ \v -> all (withinRangeComplex (-1) 1) $ V.fromVector (fPC v)

Checking the `overlap` process in @sec:cube-pc-atom is a quite tricky to check due to
the structure of the data: we would need to access and compare data wrapped in nested
vectors with large dimensions, meaning that it will be a very slow process. The 50%
overlap will be able to be tested more easily using a random test generator further in
@sec:refine. For now we test that the cube dimensions are preserved by the `overlap`
function:
$$
\forall c \in \langle\langle\langle\mathbb{C}\rangle\rangle\rangle, o \in \texttt{overlap}(\overline{c}) : |c| = (n_b,n_B,n_{FFT}) \Rightarrow |o| = (n_b,n_B,n_{FFT})
$$ {#eq:prop_dbf_value_range}

We instantiate a vector generator having a fixed length first:

> sizedVector :: Int -> Gen a -> Gen (Vector a)
> sizedVector n a = do
>   v <- QC.vectorOf n a
>   return $ V.vector v

which we use to generate random dimensioned cubes:

> prop_ct_dimensions = forAll (sizedVector nFFT $ sizedVector nB $ sizedVector nb arbitrary)
>                      $ \c -> dimensionsMatch $ overlap $ SY.signal [c]
>   where
>     dimensionsMatch s = let c = L.head $ SY.fromSignal $ s
>                             z = V.length c
>                             y = V.length $ V.first c
>                             x = V.length $ V.first $ V.first c
>                         in z == nFFT && y == nB && x == nb

From DFB onwards we deal with Doppler values which have a higher range, so we are no
longer concerned with testing for owerflowing output data, until we take a decision in
the refinement process to fix a particular number representation. However, we would
still like to test that the format and dimensions of the intermediate cubes is the
same, thus we formulate:

> prop_dfb_num_outputs = forAll (sizedVector nFFT arbitrary)
>                        $ \v -> V.length v == V.length (fDFB v)

> prop_cfar_num_outputs :: M.Matrix RealData -> Bool
> prop_cfar_num_outputs m = M.size m == M.size (fCFAR m)

The DFB contains a FFT function which is hardcoded to work for $n_{FFT}$ samples thus
we need to fix the input size accordingly. For the CFAR output, we use the `Matrix`
`size` utility.

We have tested that the `fir` implementation gives the correct impulse response, but
what about our `fir'` network instantiation `firNet` defined in @sec:cube-int-atom?
Does it act like a proper FIR filter? We test it by giving it an "impulse cube"
signal, and test that the response is the expected one:
$$\forall v \in \langle\mathbb{R}\rangle, c_1, c_0 \in
\langle\langle\langle\mathbb{R}\rangle\rangle\rangle, e_1 \in c_1, e_0 \in c_0 : e_1 =
1 \wedge e_0=0 \Rightarrow \texttt{firNet} (v,\overline{\{c_1,c_0,c_0,...\}}) = \overline{s_v}
$${#eq:prop_generic_farm_structure}

where $\overline{s_v}$ is the response signal whose events are events are cubes
containing the coefficients in $v$. Again we define a generator for the impulse signal of cubes:

> impulseSigOfCubes :: Int -> Gen (SY.Signal (C.Cube Int))
> impulseSigOfCubes n = do
>   windowLen <- choose (2, 20)  -- do not choose too large dimensions otherwise
>   rangeLen  <- choose (2, 20)  -- the tests will take too long... small tests are
>   beamLen   <- choose (2, 20)  -- good enough
>   impulse   <- sizedCube beamLen rangeLen windowLen $ elements [1] 
>   trail     <- sizedCube beamLen rangeLen windowLen $ elements [0]
>   return (SY.signal (impulse : replicate n trail))
>   where
>     sizedCube z y x  = sizedVector z . sizedVector y . sizedVector x

which we use in the property:

> prop_int_fir_response :: V.Vector Int -> Property
> prop_int_fir_response cf = forAll (impulseSigOfCubes $ V.length cf)
>                            $ \i -> correctResponse (firNet cf i)
>  where
>     correctResponse r = and $ zipWith checkEq coefsL (toLists r)
>     checkEq i = all (all (all (==i)))
>     coefsL    = L.reverse $ V.fromVector cf
>     toLists   = map (map (map V.fromVector . V.fromVector) . V.fromVector) . fromSignal

 ### Main function

Finally we gather all the properties defined in this section in a bundle of tests
called "Cube HL Model Tests", using the utilities provided by the `Test.Framework`
library. `withMaxSuccess` determines how many random tests will be generated per
propert durin one run.

> tests :: [Test]
> tests = [
>   testGroup " Cube HL Model Tests "
>     [ testProperty "GENERIC farm does not alter the input structure         "
>       (withMaxSuccess 100 prop_generic_farm_structure)
>     , testProperty "DBF  right number of outputs                            "
>       (withMaxSuccess 100 prop_dbf_num_outputs)
>     , testProperty "DBF  legal value range                                  "
>       (withMaxSuccess 200 prop_dbf_value_range)
>     , testProperty "DBF  equivalence with simple dot product operation      "
>       (withMaxSuccess 200 prop_dbf_func_equiv)
>     , testProperty "PC   right number of outputs                            "
>       (withMaxSuccess 100 prop_pc_num_outputs)
>     , testProperty "PC   right unit impulse response                        "
>       (withMaxSuccess 100 prop_pc_fir_response)
>     , testProperty "PC   legal value range                                  "
>       (withMaxSuccess 200 prop_pc_value_range)
>     , testProperty "CT   both channels have cubes of the same dimensions    "
>       (withMaxSuccess 100 prop_ct_dimensions)
>     , testProperty "DFB  right number of outputs                            "
>       (withMaxSuccess 100 prop_dfb_num_outputs)
>     , testProperty "CFAR right number of outputs                            "
>       (withMaxSuccess 100 prop_cfar_num_outputs)
>     , testProperty "INT  right unit impulse response                        "
>       (withMaxSuccess  70 prop_int_fir_response)
>     ]
>   ]

> main :: IO()
> main = defaultMain tests

