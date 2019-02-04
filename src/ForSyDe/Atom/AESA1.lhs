 ## An Operation-Oriented Approach to Modeling{#sec:atom-operation}

The first implementation in ForSyDe-Atom is a straightforward
_translation_ from the ForSyDe-Shallow implementation presented in
[@sec:shallow]. The purpose of this section is mainly to sediment the
knowledge acquired in the previous section and introduce the reader to
the ForSyDe-Atom API. This is why we recommend reading through the
previous chapter before carrying on with this one.

> {-# LANGUAGE PackageImports #-} -- you can ignore this line for now
> module ForSyDe.Atom.AESA1 where

We import Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html)
type definition to be able to use complex numbers. The `Types` local
submodule gathers the ForSyDe-Atom _exact_ equivalents of the aliases
used in the previous section and presented in
[@sec:aliases-shallow]. Similarly, the `Coefs` submodule exports the
same constants and vectors of coefficients as the ones described in
[@sec:coefs-shallow].

> import Data.Complex
> import ForSyDe.Atom.AESA.Types
> import ForSyDe.Atom.AESA.Coefs

Recall that for describing temporal (i.e. streaming) behavior of the
application our design will use a heterogeneous approach, using a
combination of _synchronous reactive (SY)_ processes [@halbwachs91],
where the main assumption is that all events in the system are
synchronized, and _synchronous data flow (SDF)_ processes [@lee95],
where the temporal behavior is formulated in terms of partial order
constraints between events. We import the
[`SY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html)
and
[`SDF`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html)
libraries described in the
_[MoC](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html)
layer_[^layers] using an appropriate alias for each.

[^layers]: see [@ungureanu17] for more on layers.

> import ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.MoC.SY  as SY

Also recall that for describing parallel operations on data we use
_algorithmic skeletons_ [@skillicorn05], which are homomorphisms on
regular algebraic structures (e,g, vectors, matrices, cubes)
describing common patterns of communication and computation and
exposing an inherent potential for parallel exploitation. For the
scope of this project we use the
[`Vector`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html)
data type, described in the
[`Skeleton`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton.html)
layer, which is a shallow (lazy-evaluated) implementation of unbounded
arrays, ideal for early design validation. The `Cube`, `Matrix` and
`DSP` modules export vector utilities used for convenience, and are
all described in term of primitives defined in the
[`Vector`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html)
module.

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
> import ForSyDe.Atom.Skeleton.Vector.DSP

 ### Video Processing Pipeline Stages

We recapitulate the main assumptions of the AESA signal processing
application presented in [@sec:video-chain-spec]: For each antenna the
data arrives _pulse by pulse_, and for each pulse arrives _range bin
by range bin_. This happens _for all antennas in parallel_, and all
complex samples are synchronized with the same sampling rate, e.g. of
the A/D converter. Each processing stage is transforming this stream
of numbers as follows.

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
different time semantics, thus we will refer to them with their
appropriate type constructor.

As presented in [@sec:dbf-shallow], the digital beamforming stage is
represented by a SY
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html#v:comb22)
process applying a
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
of $f_{DBF}$ functions over each row of the the `transpose`d[^transpV]
input $\mathtt{Antenna}\times\mathtt{Range}$ matrix carried by each
$\mathtt{Pulse}$. The function $f_{DBF}$ is defined similarly to its
ForSyDe-Shallow counterpart, in terms of the skeletons
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
[`reduce`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:reduce2)
[`fanout`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:fanout)
and
[`length`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:length)
applied over eithe
[`Vector`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html)s
or `Matrix`es.

![DBF stage process](figs/dbf-proc-atom.pdf){#fig:dbf-proc-atom}

[^transpV]: see `ForSyDe.Atom.Skeleton.Vector.Matrix.transpose` from
            the `forsyde-atom-extensions` documentation.

> dbf :: SY.Signal (Antenna (Range CpxData))
>     -> SY.Signal (Beam    (Range CpxData))
> dbf = SY.comb11 (M.transpose . V.farm11 fDBF . M.transpose)
> 
> fDBF :: Antenna CpxData
>      -> Beam    CpxData
> fDBF antennaEl = beam
>   where
>     beam       = V.reduce (V.farm21 (+)) beamMatrix
>     beamMatrix = M.farm21 (*) elMatrix beamConsts
>     elMatrix   = V.farm11 V.fanout antennaEl
>     beamConsts = mkBeamConsts (V.length antennaEl) nB

The pulse compression stage is described like in [@sec:pc-shallow] as
a SY
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html#v:comb22)
process applying a
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
of $f_{DBF}$ functions over each row of the
$\mathtt{Beam}\times\mathtt{Range}$ matrix carried by each
$\mathtt{Pulse}$. The function $f_{PC}$ is applying the FIR sliding
window using the `fir` utility[^firA] which, as we saw in
[@sec:int-shallow], is actually a convenience utility for the more
general form `fir'` which restricts this parallel pattern to be
applied on numbers only.

![PC stage process](figs/pc-proc-atom.pdf){#fig:pc-proc-atom}

[^firA]: see `ForSyDe.Atom.Skeleton.Vector.DSP.fir` from
  the `forsyde-atom-extensions` documentation.

> pc :: SY.Signal (Beam (Range CpxData))
>    -> SY.Signal (Beam (Range CpxData))
> pc = SY.comb11 (V.farm11 fPC)
> 
> fPC :: Range CpxData -- ^ input range bins     
>     -> Range CpxData -- ^ output pulse-compressed bins
> fPC = fir mkPcCoefs

During corner turning with memory overlapping we see a major
difference as compared to the definition in [@sec:ct-shallow], namely
that the transition from the SY signal semantics to the SDF signal
semantics needs to be explicitly marked by the MoC interface
[`toSDF`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html#v:toSDF2). This
is a deliberate feature of ForSyDe-Atom, since signals themselves
enablers of process to behave under certain MoC semantics. This is
shown also in the type signature of signals. The SDF
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html#v:comb22)
applies the cube `transpose`[^transpAC] in order to arrange the video
data in rows of samples in the direction of pulse windows.

![CT process](figs/ct-proc-atom.pdf){#fig:ct-proc-atom}

[^transpAC]: see `ForSyDe.Atom.Skeleton.Vector.Cube.transpose` from
  the `forsyde-atom-extensions` documentation.

> ct :: SY.Signal (Beam (Range CpxData))
>    -> (SDF.Signal (Beam (Range (Window CpxData))),
>        SDF.Signal (Beam (Range (Window CpxData))))
> ct sig = (pCT rightSig, pCT leftSig)
>   where
>     pCT       = SDF.comb11 (nFFT,1, (:[]) . C.transpose . vector)
>     wsig      = SY.toSDF sig
>     rightSig  = wsig
>     leftSig   = SDF.delay initBatch wsig
>     initBatch = replicate (nFFT `div` 2) (M.fanout (cis 0))

The Doppler filter bank behave similarly to [@sec:dfb-shallow] with
the sole exception that it needs to explicitly convert the SDF signal
back to SY using the MoC interface
[`toSY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html#v:toSY)
in order to operate in the synchronous domain again[^fn:1].

![DFB process](figs/dfb-proc-atom.pdf){#fig:dfb-proc-atom}

[^fn:1]: note that we have carried the SY domain accross stages mainly
  for didactic purposes to expose this domain transition through the
  type signature. the `SDF.toSY` conversion chould have very well been
  performed within the CT stage.

> dfb :: SDF.Signal (Beam (Range (Window CpxData)))
>         -> SY.Signal (Beam (Range (Window RealData)))
> dfb = SY.comb11 (M.farm11 fDFB) . SDF.toSY
> 
> fDFB :: Window CpxData -> Window RealData
> fDFB = V.farm11 envelope . fft nFFT . weight
>   where
>     weight     = V.farm21 (*) mkWeightCoefs
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

The constant false alarm ratio stage is implemented exactly the same as in [@sec:cfar-shallow], but now we use the ForSyDe atom skeletons
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22),
[`reduce`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:reduce2),
[`fanout`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:fanout),
[`group`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:group),
[`take`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:take)
[`drop`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:drop)
and `stencil`[^stenA].

![CFAR process](figs/cfar-proc-atom.pdf){#fig:cfar-proc-atom}

[^stenA]: see `ForSyDe.Atom.Skeleton.Vector.stencil` from the
  `forsyde-atom-extensions` documentation.

> cfar :: SY.Signal (Beam ( Range (Window RealData)))
>      -> SY.Signal (Beam (CRange (Window RealData)))
> cfar = SY.comb11 (V.farm11 fCFAR)
> 
> fCFAR :: Range (Window RealData) -> CRange (Window RealData)
> fCFAR r_of_d = V.farm41 (\m -> V.farm31 (normCfa m)) md bin lmv emv
>   where
>     bin = V.drop (nFFT + 1) r_of_d
>     md  = V.farm11 (logBase 2 . V.reduce min) bin
>     emv = V.farm11 (aritMean . V.take nFFT) neighbors
>     lmv = V.farm11 (aritMean . V.drop (nFFT + 3)) neighbors
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     aritMean :: Vector (Vector RealData) -> Vector RealData
>     aritMean  = V.farm11 (/n) . V.reduce addV . V.farm11 geomMean . V.group 4
>     geomMean  = V.farm11 (logBase 2 . (/4)) . V.reduce addV
>     neighbors = V.stencil (2 * nFFT + 3) r_of_d
>     addV      = V.farm21 (+)
>     n         = fromIntegral nFFT

Finally, the integration is performed like in [@sec:int-shallow], by
using the `fir'` pattern to create a FIR process network.

> int :: SY.Signal (Beam (CRange (Window RealData)))
>     -> SY.Signal (Beam (CRange (Window RealData)))
>     -> SY.Signal (Beam (CRange (Window RealData)))
> int cr cl = firNet $ addSC cr cl
>   where
>     firNet  = fir' addSC mulSC dlySC mkFirCoefs
>     addSC   = SY.comb21 (C.farm21 (+))
>     mulSC c = SY.comb11 (C.farm11 (*c))
>     dlySC   = SY.delay (C.fanout 0)

 ### System Process Network

The AESA process network is formed by "plugging in" together all
components instantiated in the previous sections, and thus obtaining
the system description in [@fig:aesa-proc-atom].

![The AESA process network instance](figs/aesa-proc-atom.pdf){#fig:aesa-proc-atom}

> aesa :: SY.Signal (Range (Antenna CpxData))
>      -> SY.Signal (Beam (CRange (Window RealData)))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ dfb lCt
>     rDfb      = cfar $ dfb rCt
>     (lCt,rCt) = ct $ pc $ dbf video




