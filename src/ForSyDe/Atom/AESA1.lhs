 ### An Operation-Oriented Approach to modeling the AESA

> {-# LANGUAGE PackageImports #-}
> module ForSyDe.Atom.AESA1 where

> import Data.Complex
> import ForSyDe.Atom.AESA.Types
> import ForSyDe.Atom.AESA.Coefs

For describing temporal (i.e. streaming) behavior of the application
our design will use a heterogeneous approach, using a combination of
_synchronous reactive (SY)_ processes [@halbwachs91], where the main
assumption is that all events in the system are synchronized, and
_synchronous data flow (SDF)_ processes [@lee95], where the temporal
behavior is formulated in terms of partial order constraints between
events. We import the
[`SY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html)
and
[`SDF`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html)
libraries described in the
_[MoC](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html)
layer_[^layers] using an appropriate alias for each.

[^layers]: see [@ungureanu17] for more on layers.

> import ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.MoC.SY  as SY

For describing parallel operations on data we use _algorithmic
skeletons_ [@skillicorn05], which are homomorphisms on regular
algebraic structures (e,g, vectors, matrices, cubes) describing common
patterns of communication and computation and exposing an inherent
potential for parallel exploitation. For the scope of this project we
use the
[`Vector`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html)
data type, described in the
[`Skeleton`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton.html)
layer, which is a shallow (lazy-evaluated) implementation of unbounded
arrays, ideal for early design validation. Although type-checked,
bounded, and even boxed (i.e. memory-mapped) alternatives exist, such
as
[`FSVec`](http://hackage.haskell.org/package/parameterized-data/docs/Data-Param-FSVec.html)
or REPA [`Array`](http://hackage.haskell.org/package/repa)s, for the
scope of this project the functional validation and (by-hand)
requirement analysis on the properties of skeletons will suffice. We
also import the `Matrix` and `Cube` utility libraries which contain
type synonyms for nested `Vector`s along with their derived skeletons,
as well a `Designs` which contain commonly used DSP blocks defined in
terms of vector skeletons.

**NOTE:** at the time being the modules below are imported from our
  custom package `forsyde-atom-extensions`. The next release of
  `forsyde-atom` will most likely contain these extensions, and they
  will be imported normally. Until then, the API documentation needs
  to be generated locally using the `haddock` commands, as described
  in the `README.md` file.

> -- import our own custom Vector module, with bug-fixes. It will be
> -- imported normally once 'forsyde-atom-extensions' is pushed to upstream.
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import ForSyDe.Atom.Skeleton.Vector.Designs

[@sec:aliases-shallow]

In this model pulses are dimensioned in time, where the time aspect is
captured by the properly "one pulse arrives after the
other". Furthermore, we can safely assume that, at least for the first
part of the signal processing pipeline, all pulses are syncronized
with the A/D converter rate, thus an _infinite stream of pulses_ can
be modeled as a ForSyDe SY signal. After Corner Turn (CT), as long as
we operate on chunks/windows of data the most natural MoC to describe
processing is SDF, which drops the assumption of _total synchrony_
throughout the system in favor of a _partial synchrony_
(i.e. consistent cut) with respect to the firing of the actor. In
ForSyDe-Atom SY signals are distinguished from SDF signals in the
sense that they carry different tag systems [@lee98] and infer
different time semantics. Thus we use two aliases for each respective
type of signal: `Pulses` and `WPulses` (windowed pulses).

> type Pulses a  = SY.Signal a
> type WPulses a = SDF.Signal a

 #### Video Processing Pipeline Stages

As presented [earlier]() the AESA application consists in a signal
processing chain on input video streams coming from the array of
antennas. For each antenna the data arrives _pulse by pulse_, and for
each pulse arrives _range bin by range bin_. This happens _for all
antennas in parallel_, and all complex samples are synchronized with
the same sampling rate, e.g. of the A/D converter. Each processing
stage is transforming this stream of numbers as follows.

 ##### Digital Beamforming (DBF)

The DBF receives complex indata, from $N_A$ antenna elements and forms
 $N_B$ simultaneous receiver beams, or "listening directions", by
 summing individually phase-shifted indata signals from all
 elements. Basically, considering the input video "cube" described
 [previously](), the the transformation applied by DBF, could be
 depicted as in [@fig:dbf-cube], where the _pulse_ dimension
 goes to infinity (i.e. data is received pulse by pulse).

![Digital Beam Forming on video structure](figs/dbf-cube.pdf)

However, using knowledge of _how_ data arrives, the _range bin_
dimension can also be unrolled in time, and thus the DBF algorithm can
be applied as soon as $N_A$ complex samples arrive.

![Digital Beam Forming on streams of complex samples](figs/dbf-samp.pdf){#fig:dbf-samp}

To translate [@fig:dbf-samp] into MoC behavior, instead of modeling a
vector of $N_A$ synchronous signals, i.e. a signal for each antenna
element, we choose to represent it as one signal of $N_A$ samples, as
for the SY MoC the two representations are semantically
equivalent. However, with this approach we focus on the parallel
procesing of data aspect of the computation rather than the concurrent
distribution of signals, which is more suitable for this application.

As such, the DBF signal processing block boils down to a single
synchronous
[`comb11`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html#v:comb22)
process applying the DBF algorithm on sampled vectors of complex data.


> dbf :: SY.Signal (Range (Antenna CpxData))
>     -> SY.Signal (Range (Beam CpxData))
> dbf = SY.comb11 (V.farm11 fDBF)



> fDBF :: Antenna CpxData
>      -> Beam CpxData
> fDBF antennaEl = beam
>   where
>     beam       = V.reduce (V.farm21 (+)) beamMatrix
>     beamMatrix = M.farm21 (*) elMatrix beamConsts
>     elMatrix   = V.farm11 V.fanout antennaEl
>     beamConsts = mkBeamConsts (V.length antennaEl) nB

 ##### Pulse Compression (PC)

> pc :: Pulses (Range (Beam CpxData))
>    -> Pulses (Beam (Range CpxData))
> pc = SY.comb11 (V.farm11 fPC . M.transpose)

> fPC :: Range CpxData -- ^ input range bin     
>     -> Range CpxData -- ^ output pulse-compressed bin
> fPC = fir mkPcCoefs

 ##### Corner Turn (CT)

> ct :: Pulses (Beam (Range CpxData))
>   -> (WPulses (Beam (Range CpxData)),
>       WPulses (Beam (Range CpxData)))
> ct pcSig = (rightWSig, leftWSig)
>   where
>     wsig      = SY.toSDF pcSig
>     rightWSig = wsig
>     leftWSig  = SDF.delay initBatch wsig
>     initBatch = replicate (nFFT `div` 2) (M.fanout (cis 0))


 ##### Doppler Filter Bank (DFB)

> doppler :: WPulses (Beam (Range CpxData))
>         -> WPulses (Beam (Range (Window RealData)))
> doppler = SDF.comb11 (nFFT, 1, (:[]) . transformCube . C.transpose1 . V.vector)
>   where
>     transformCube = M.farm11 (fDoppler . fWeight)

> fDoppler :: Window CpxData -> Window RealData
> fDoppler = V.farm11 envelope . fft nFFT
>   where
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

> fWeight :: Window CpxData -> Window CpxData
> fWeight = V.farm21 (*) mkWeightCoefs



 ##### Constant False Alarm Ratio (CFAR)

> cfar :: WPulses (Beam ( Range (Window RealData)))
>      -> WPulses (Beam (CRange (Window RealData)))
> cfar = SDF.comb11 (1, 1, (:[]) . V.farm11 fCFAR . head)

> fCFAR :: Range (Window RealData) -> CRange (Window RealData)
> fCFAR r_of_d = V.farm41 (\m -> V.farm31 (normCfa m)) md bin lmv emv
>   where
>     bin = V.drop (nFFT + 1) r_of_d
>     md  = V.farm11 (logBase 2 . V.reduce min) bin
>     emv = V.farm11 (meanFun . V.take nFFT) stens
>     lmv = V.farm11 (meanFun . V.drop (nFFT + 3)) stens
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     meanFun :: Vector (Vector RealData) -> Vector RealData
>     meanFun = V.reduce addV . V.farm11 (V.farm11 (logBase 2 . (/4)) . V.reduce addV) . V.group 4
>     stens   = V.stencil (2 * nFFT + 3) r_of_d
>     addV    = V.farm21 (+)
>   

 ##### Integrator (INT)

> int :: WPulses (Beam (CRange (Window RealData)))
>     -> WPulses (Beam (CRange (Window RealData)))
>     -> Pulses  (Beam (CRange (Window RealData)))
> int cr cl = applyFIR $ addSCube (SDF.toSY cr) (SDF.toSY cl)
>   where
>     applyFIR   = fir' addSCube mulSCube delaySCube mkFirCoefs
>     addSCube   = SY.comb21 (C.farm21 (+))
>     mulSCube c = SY.comb11 (C.farm11 (*c))
>     delaySCube = SY.delay (C.fanout 0)

 #### System Process Network

> aesa :: Pulses (Range (Antenna CpxData))
>      -> Pulses (Beam (CRange (Window RealData)))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ doppler lCt
>     rDfb      = cfar $ doppler rCt
>     (lCt,rCt) = ct $ pc $ dbf video




