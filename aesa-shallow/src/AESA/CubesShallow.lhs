 ## The High-Level Model {#sec:cube-shallow-operation}

The behavioral model of this section is exactly the same as the one presented in in
@sec:cube-atom-operation, and thus we will not go through all the details of each
functional block, but rather list the code and point out the syntax differences.

The code for this section is written in the following module, see [@sec:usage] on how
to use it:

> {-# LANGUAGE PackageImports #-}   -- you can ignore this line for now

The code for this section is written in the following module, see [@sec:usage] on how
to use it:

> module AESA.CubesShallow where

 ### Imported Libraries

The first main difference between ForSyDe-Shallow and ForSyDe-Atom becomes apparent
when importing the libraries: ForSyDe-Shallow does not require to import as many
sub-modules as its younger counterpart. This is because the main library
[`ForSyDe.Shallow`](http://hackage.haskell.org/package/forsyde-shallow) exports all
the main language constructs, except for specialized utility blocks, so the user does
not need to know where each function is placed.

> import ForSyDe.Shallow

For the AESA model we make use of such utilities, such as $n$-dimensional vectors
(i.e. matrices, cubes) and DSP blocks, thus we import our local extended
[`ForSyDe.Shallow.Utilities`](http://hackage.haskell.org/package/forsyde-shallow-3.4.0.0/docs/ForSyDe-Shallow-Utility.html)
library.

> -- | explicit import from extensions package. Will be imported
> -- normally once the extensions are merged into forsyde-shallow
> import "forsyde-shallow-extensions" ForSyDe.Shallow.Core.Vector
> import "forsyde-shallow-extensions" ForSyDe.Shallow.Utility

Finally, we import Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html) type, to
represent complex numbers.

> import Data.Complex

To keep the model consistent with the design in @sec:cube-atom-operation we import the
*same* parameters and coefficient generator functions from the `aesa-atom` package, as
presented in [@sec:aesa-parameters;@sec:coefs-atom].

> import AESA.Params
> import AESA.CoefsShallow  -- wraps the list functions exported by 'AESA.Coefs' into
>                           -- 'ForSyDe.Shallow' data types.

 ### Type Synonyms{#sec:aliases-shallow}

We use the same (local) aliases for types representing the different
data structure and dimensions.

> type Antenna     = Vector       -- length: nA
> type Beam        = Vector       -- length: nB
> type Range       = Vector       -- length: nb
> type Window      = Vector       -- length: nFFT
> type CpxData     = Complex Float
> type RealData    = Float

 ### Video Processing Pipeline Stages

This section follows the same model as @sec:cube-atom-pipe-stages using the
ForSyDe-Shallow modeling libraries. The digital beamforming (DBF) block presented in
@sec:cube-dbf-atom becomes:

> dbf :: Signal (Antenna (Window (Range CpxData)))
>     -> Signal (Window  (Range  (Beam  CpxData)))
> dbf = combSY (mapMat fDBF . transposeCube)
> 
> fDBF :: Antenna CpxData -- ^ input antenna elements
>      -> Beam    CpxData -- ^ output beams
> fDBF antennas  = beams
>   where
>     beams      = reduceV (zipWithV (+)) beamMatrix
>     beamMatrix = zipWithMat (*) elMatrix beamConsts
>     elMatrix   = mapV (copyV nB) antennas
>     beamConsts = mkBeamConsts dElements waveLength nA nB

The second main difference between the syntax of ForSyDe-Shallow and ForSyDe-Atom can
be noticed when using the library functions, such as process constructors: function
names are not invoked with their module name (alias), but rather with a suffix
denoting the type they operate on, e.g. `transposeCube` is the equivalent of
`C.transpose`. Another difference is that constructors with different numbers of
inputs and outputs are not differentiated by a two-number suffix, but rather by
canonical name, e.g. `combSY` is the equivalent of `SY.comb11`. Lastly, you might
notice that some function names are completely different. That is because
ForSyDe-Shallow uses names inspired from functional programming, mainly associated
with operations on lists, whereas ForSyDe-Atom tries to adopt more suggestive names
with respect to the layer of each design component and its associated jargon,
e.g. `mapMat` is the equivalent of `M.farm11`, or `zipWithV` is equivalent with
`V.farm21`.

We go further with the description of the pulse compression (PC) block, as described
previously in @sec:cube-dbf-atom becomes. The same variation in naming convention can
be noticed, but the syntax is still the same.

> pc :: Signal (Window (Range (Beam  CpxData))) 
>    -> Signal (Window (Beam  (Range CpxData)))
> pc = combSY (mapV (mapV fPC . transposeMat))
> 
> fPC :: Range CpxData -- ^ input range bin     
>     -> Range CpxData -- ^ output pulse-compressed bin
> fPC = mav (mkPcCoefs 5)

The same goes for `overlap` state machine use in the corner turn (CT) stage, as well
as the Doppler filter bank (DFB) and the constant false alarm ratio (CFAR) stages from
[@sec:cube-ct-atom;@sec:cube-dfb-atom;@sec:cube-cfar-atom] respectively.

> overlap :: Signal (Window (Beam (Range CpxData)))
>         -> Signal (Window (Beam (Range CpxData)))
> overlap = mealySY nextState outDecode initState
>   where
>     nextState _ cube = dropV (nFFT `div` 2) cube
>     outDecode s cube = s <+> takeV (nFFT `div` 2) cube
>     initState        =  copyCube nb nB (nFFT `div` 2) 0 -- (copyV (nFFT `div` 2) . copyV nB . copyV ) 0

> dfb :: Signal (Window (Beam  (Range  CpxData )))
>     -> Signal (Beam   (Range (Window RealData)))
> dfb = combSY (mapMat fDFB . transposeCube)
> 
> fDFB :: Window CpxData -> Window RealData
> fDFB = mapV envelope . fromDouble (fft nFFT) . weight
>   where
>     weight     = zipWithV (*) (mkWeightCoefs nFFT)
>     envelope a = let (i, q) = (realPart a, imagPart a)
>                  in realToFrac $ sqrt (i * i + q * q)
>     -- since ForSyDe-Shallow's 'fft' is monomorphic defined on Double, 
>     -- we need to wrap it in order to work with another type (in our case Float)
>     fromDouble f = mapV (fmap realToFrac) . f . mapV (fmap realToFrac)

> cfar :: Signal (Beam (Range (Window RealData)))
>      -> Signal (Beam (Range (Window RealData)))
> cfar = combSY (mapV fCFAR)
> 
> fCFAR :: Range (Window RealData) -> Range (Window RealData)
> fCFAR rbins = zipWith4V (\m -> zipWith3V (normCfa m)) md rbins lmv emv
>   where
>     md  = mapV (logBase 2 . reduceV min) rbins
>     emv = (copyV (nFFT + 1) dummy) <+> (mapV aritMean neighbors)
>     lmv = (dropV 2 $ mapV aritMean neighbors) <+> (copyV (nFFT+2) dummy) 
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     aritMean  = mapV (/n) . reduceV addV . mapV geomMean . groupV 4
>     geomMean  = mapV (logBase 2 . (/4)) . reduceV addV
>     -----------------------------------------------
>     dummy     = copyV nFFT $ (-maxFloat)/n
>     neighbors = stencilV nFFT rbins
>     -----------------------------------------------
>     addV      = zipWithV (+)
>     n         = fromIntegral nFFT

For the integration stage (INT) presented in @sec:cube-ct-atom we extended the
`ForSyDe.Shallow.Utility` library with a `fir'` skeleton and an `interleaveSY`
process, similar to the ones developed for ForSyDe-Atom[^fn:utils]. Thus we can
implement this stage exactly as before:

> int :: Signal (Beam (Range (Window RealData)))
>     -> Signal (Beam (Range (Window RealData)))
>     -> Signal (Beam (Range (Window RealData)))
> int r l = firNet $ interleaveSY r l
>   where
>     firNet = zipCubeSY . mapCube (firSY mkIntCoefs) . unzipCubeSY

[^fn:utils]: at this moment it is enough to know that both are implemented in terms of existing skeletons or process constructors. More complex behaviors such as these two will be made explicit later in @sec:refine. Interested readers can consult the implementations in the `forsyde-shallow-extensions` package.

| Function                                  | Original module                     | Package          |
|------------------------------------------|---------------------------------------|------------------|
| `mapV`, `zipWith`, `zipWith`[`4`/`3`],    | [`ForSyDe.Shallow.Core.Vector`]     | forsyde-shallow            |
| `reduceV`, `lengthV`, `dropV`, `takeV`,   |                                     |                            |
| `copyV`, `(<+>)`, `stencilV`              |                                     |                            |
| `mapMat`, `zipWithMat`, `transposeMat`    | [`ForSyDe.Shallow.Utility.Matrix`]  | forsyde-shallow            |
| `transposeCube`                           | `ForSyDe.Shallow.Utility.Cube`      | forsyde-shallow-extensions |
| `fir`                                     | [`ForSyDe.Shallow.Utility.FIR`]     | forsyde-shallow            |
| `fft`                                     | [`ForSyDe.Shallow.Utility.DFT`]     | forsyde-shallow            |
| `mav`                                     | `ForSyDe.Shallow.Utility.DSP`       | forsyde-shallow-extensions |
| `combSY`, `comb2SY`, `mealySY`, `delaySY` | [`ForSyDe.Shallow.MoC.Synchronous`] | forsyde-shallow            |
| `interleaveSY`                            | `ForSyDe.Shallow.Utility`           | forsyde-shallow-extensions |
| `mkBeamConsts`,`mkPcCoefs`,               | `AESA.Coefs`                        | aesa-atom                  |
| `mkWeightCoefs`, `mkFirCoefs`, `maxFloat` |                                     |                            |
| `dElements`, `waveLenth`, `nA`, `nB`      | `AESA.Params`                       | aesa-atom                  |
| `nFFT`, `nb`                              |                                     |                            |

 ### The AESA Process Network

The process network is exactly the same as the one in @sec:cube-atom-operation, but
instantiating the locally defined components.

> aesa :: Signal (Antenna (Window (Range  CpxData )))
>      -> Signal (Beam    (Range  (Window RealData)))
> aesa video = int rCfar lCfar
>   where
>     rCfar = cfar $ dfb oPc
>     lCfar = cfar $ dfb $ overlap oPc
>     oPc   = pc $ dbf video

