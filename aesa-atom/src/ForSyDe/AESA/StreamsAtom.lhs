 ## The High-Level Model{#sec:atom-network}

This section presents a high-level behavioral model of the AESA signal processing
chain presented in [@sec:video-chain-spec], which is, in any circumstance, _not_ the
only modeling alternative, but rather shows an intuitive and didactic way to tackle
the challenge of translating the _textual_ specifications into an _executable_ ForSyDe
specification. While most design choices are driven by the ambition to introduce the
new modeling concepts presented in [@sec:crash-atom], they can be justified by the
need to capture the essential behavioral properties in a formal way, in order to be
exploitable in future stages of a design flow towards efficient implementations.  The
main design approach is to exploit the relative independence between each data path
associated with each antenna element or beam, and to model these paths as skeletons
(e.g. farms) of (chains of) processes. Each design decision will infer a
re-partitioning of the indata cubes between the time/causality and space dimensions
following the design patters depicted in [@fig:atom-layers], namely:

* _skeletons of processes_ which: 1) express parallelism at the process level; 2)
  depicts processes as operating on elementary streams of data, e.g. originating from
  each antenna element in particular, and skeletons as the structured interactions
  between these streams; 3) expose a fine-grained modular view allowing to quantify
  the potential for _load distribution_, since each "operation" (i.e. process) clearly
  captures the aspect of precedence constraints.

* _process of skeletons_ which : 1) express parallelism at the datum level; 2) depicts
  processes as operating on structures of data (e.g. vectors, matrices or cubes); 3)
  expose a monolithic view of processes where precedence constraints are expressed
  "outside" of the algorithm, and where the algorithm itself expresses potential for
  _data parallelism_.

The code for this section is written in the following module, see [@sec:usage] on how
to use it:

> {-# LANGUAGE PackageImports #-} -- allows explicit import of modules from custom
>                                 -- libraries instead of standard ones. Will be taken
>                                 -- out once the extensions are merged upstream.
> module ForSyDe.AESA.StreamsAtom where

 ### Imported Libraries

As the AESA application uses complex numbers, we use Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html) type.

> import Data.Complex

For describing streaming behavior of the application our design will use a
heterogeneous approach, using a combination of _synchronous reactive (SY)_ processes
(@leeseshia-15,@Benveniste03), where the main assumption is that all events in the
system are synchronized; and _synchronous data flow (SDF)_ processes
(@leeseshia-15,@lee95), where the temporal behavior is formulated in terms of partial
order constraints between events. We import the
[`SY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html) and
[`SDF`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html)
libraries described in the
_[MoC](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html) layer_, see
[@ungureanu17], using an appropriate alias for each.

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF

For describing parallel operations on data we use algorithmic skeletons
(@Fischer-2003,@skillicorn05), formulated on ForSyDe-Atom's in-house
[`Vector`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html)
data type, which is a shallow, lazy-evaluated implementation of unbounded arrays,
ideal for early design validation. Although dependent, bounded, and even boxed
(i.e. memory-mapped) alternatives exist, such as
[`FSVec`](http://hackage.haskell.org/package/parameterized-data/docs/Data-Param-FSVec.html)
or REPA [`Array`](http://hackage.haskell.org/package/repa)s, for the scope of this
project the functional validation and (by-hand) requirement analysis on the properties
of skeletons will suffice. We also import the `Matrix` and `Cube` utility libraries
which contain type synonyms for nested `Vector`s along with their derived skeletons,
as well a `DSP` which contain commonly used DSP blocks defined in terms of vector
skeletons.

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector        as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP

Finally, we import the local project module defining different coefficients for the
AESA algorithms, presented in detail in @sec:coefs-atom.

> import ForSyDe.AESA.Coefs

 ### Type Aliases and Constants{#sec:aliases-shallow label="Type Aliases and Constants"}

The system parameters are integer constants defining the size of the application. For
a simple test scenario provided by Saab AB, we have bundled these parameters in the
following module, and we shall use their variable names throughout the whole report:

> import ForSyDe.AESA.Params

For ease of documentation we will be using type synonyms (aliases) for all types and
structures throughout this design:

* `Antenna` denotes a vector container for the antenna elements. Its length is equal
  to the number of antennas in the radar $N_A$.

* After Digital Beamforming (DBF), the antenna elements are transformed into $N_B$
  beams, thus we associate the `Beam` alias for the vector container wrapping those
  beams.

* `Range` is a vector container for range bins. All antennas have the same number of
  range bins $N_b$, rendering each $\text{Antenna} \times \text{Range}$ a perfect
  matrix of samples for every pulse.

* `Window` stands for a Doppler window of $N_{FFT}$ pulses.

> type Antenna     = Vector -- length: nA
> type Beam        = Vector -- length: nB
> type Range       = Vector -- length: nb
> type Window      = Vector -- length: nFFT

Finally we provide two aliases for the basic Haskell data types used in the system, to
stay consistent with the application specification.

> type CpxData  = Complex Float
> type RealData = Float

 ### Video Processing Stages

In this section we follow each stage described in [@sec:video-chain-spec], and exploit
the initial assumption on the order of events stating: _"For each antenna the data
arrives _pulse by pulse_, and each pulse arrives _range bin by range bin_. This
happens _for all antennas in parallel_, and all complex samples are synchronized with
the same sampling rate, e.g. of the A/D converter."_

This allows us to "unroll" the indata video cubes into $N_A$ parallel synchronous
streams, each stream being able to be processed as soon as it contains enough
data. This unrolling is depicted in [@fig:cube-unrolling] as streaming the pulses as
soon as they arrive: range bin by range bin. We say that we partition the data _in
time_ rather than _in space_, which is a more appropriate partition judging by the
assumption above.

![Video cube unrolling](figs/cube-unrolling.pdf){#fig:cube-unrolling}

 #### Digital Beamforming (DBF){#sec:dbf-atom}

The DBF receives complex in data, from $N_A$ antenna elements and forms $N_B$
simultaneous receiver beams, or "listening directions", by summing individually
phase-shifted in data signals from all elements. Depicted from a streaming point of
view, DBF would like in @fig:dbf-samp.

\suppressfloats
![Digital Beam Forming on streams of complex
samples](figs/dbf-samp.pdf){#fig:dbf-samp}

As can be seen in @fig:dbf-samp, a beam can be formed _as soon as_ all antennas have
produced a complex sample. The parallel streams of data coming from each antenna
element are represented as a _vector of synchronous (SY) signals_, i.e. vector of
signals where each event is synchronous with each other. This allows us to depict the
dataflow interaction between the streams during digital beamforming as the process
network in @fig:dbf-net-atom, where an  $\oplus$ represents a combinational process
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html#v:comb22).

\suppressfloats
![DBF network](figs/dbf-net-atom.pdf){#fig:dbf-net-atom}

> dbf :: Antenna (SY.Signal CpxData)
>     -> Beam    (SY.Signal CpxData)
> dbf antennaSigs = beamSigs
>   where
>     beamSigs   = V.reduce (V.farm21 (SY.comb21 (+))) beamMatrix
>     beamMatrix = M.farm21 (\c -> SY.comb11 (*c)) beamConsts sigMatrix
>     sigMatrix  = V.farm11 V.fanout antennaSigs
>     beamConsts = mkBeamConsts dElements waveLength nA nB :: Matrix CpxData

| Function                     | Original module                       | Package                 |
|------------------------------|---------------------------------------|-------------------------|
| `farm11`, `reduce`, `length` | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| `farm21`                     | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `comb11`, `comb21`           | [`ForSyDe.Atom.MoC.SY`]               | forsyde-atom            |
| `mkBeamConsts`               | `ForSyDe.AESA.Coefs`                  | aesa-atom               |
| `dElements`, `waveLenth`, `nA`, `nB` | `ForSyDe.AESA.Params`         | aesa-atom               |
\suppressfloats

The previous code listing, depicted in @fig:dbf-net-atom, is actually showing the
"internals" of a matrix-vector dot product[^dotMatMat]. However, the elementary
operations, instead of regular arithmetic operations $\times$ and $+$, are _processes_
applying these operations on SY streams. As such, the `fanout` skeleton distributes
one signal to a whole row (i.e. vector) of processes, the matrix `farm` applies
pair-wise a matrix of partially applied processes on this matrix of signals, and
`reduce` creates a reduction network of binary processes pair-wise applying the
function $+$ on all events in signals. Practically the DBF network transforms $N_A$
synchronous signals originating from each antenna element into $N_B$ synchronous
signals for each beam. The internal structure of this transformation exposes multiple
degrees of potential distribution on parallel synchronous resources.


The table above gives some pointers where to look for additional documentation of each
imported function. The formula to generate the beam constants `mkBeamConsts` is
presented later in [@sec:atom-coefs].

[^dotMatMat]: see `dotMatMat` in the `ForSyDe.Atom.Skeleton.Vector.DSP` module from package `forsyde-atom-extensions`.

 #### Pulse Compression (PC){#sec:pc-atom}

In this stage the received echo of the modulated pulse, i.e. the information contained
by the range bins, is passed through a matched filter for decoding their modulation.
This essentially applies a sliding window, or a moving average (MAV) on the range bin
samples.

![Pulse Compression on streams of complex samples](figs/pc-samp.pdf){#fig:pc-samp}

In @fig:pc-samp we can see that, in order to apply the MAV algorithm on all the range
bins of every pulse, we need to accumulate $N_b$ samples and process them in
batches. Intuitively this can be done by processing each beam with a _synchronous
dataflow_ (SDF) actor which, with each firing, consumes $N_b$ samples and produces
$N_b$ samples. Note that at this stage of modeling we value intuition and the
capturing of the right application properties rather than efficiency. We will tackle
this problem later in the design stages (see [@sec:refinement]) where we will try to
transform the model toward more "efficient" implementation models (with respect to
some constraint, e.g. throughput) which preserve these behavioral properties.

![PC stage process](figs/pc-proc-atom.pdf){#fig:pc-proc-atom}

> pc :: Beam ( SY.Signal CpxData)
>    -> Beam (SDF.Signal CpxData)
> pc = V.farm11 (procPC . SY.toSDF)

| Function    | Original module                    | Package                 |
|-------------|------------------------------------|-------------------------|
| `farm11`    | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom            |
| `toSDF`     | [`ForSyDe.Atom.MoC.SY`]            | forsyde-atom            |
| `comb11`    | [`ForSyDe.Atom.MoC.SDF`]           | forsyde-atom            |
| `fir`       | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkPcCoefs` | `ForSyDe.AESA.Coefs`               | aesa-atom               |
| `nb`        | `ForSyDe.AESA.Params`              | aesa-atom               |

Following the reasoning above, we instantiate the PC video processing stage as a
`farm` of SDF processes `procPC` as depicted in [@fig:pc-proc-atom]. Notice that
before being able to apply the SDF actors we need to translate the SY signals yielded
by the DBF stage into SDF signals. This is done by the `toSDF` interface which is an
injective mapping from the (timed) domain of a SY MoC tag system, to the (untimed)
codomain of a SDF MoC tag system. For more on tag systems please consult [@lee98].

> procPC :: Fractional a => SDF.Signal a -> SDF.Signal a 
> procPC = SDF.comb11 (nb, nb, V.fromVector . fir (mkPcCoefs 5) . V.vector)

The `procPC` actor consumes and produces `nb` tokens each firing, forms a `Vector`
from these tokens, and applies the `fir` skeleton on these vectors (which computes the
MAV if considering vectors). The `fir` skeleton is a utility formulated in terms of
primitive skeletons (i.e. `map` and `reduce`) on numbers, i.e. lifting arithmetic
functions. We will study this skeleton later in this report and for now we take it
"for granted", as conveniently provided by the `DSP` utility library. Also notice that
the type signature for `procPC` is left polymorphic as to be more convenient later
when we formulate properties over it.


 #### Corner Turn (CT){#sec:ct-atom}

In order to be able to calculate the Doppler channels further in the processing
pipeline, during a CT, a rearrangement of data must be performed between functions
that process data in “different” directions, e.g. range and pulse. This rearrangement
is called corner turn.  We make use of the knowledge that for each beam samples arrive
in order, one range bin at a time, in the direction of consumption suggested in
@fig:ct-samp, and "fill back in" the video cube in the direction of production. In
order to maximize the efficiency of the AESA processing the datapath is split into two
concurrent processing channels with 50% overlapped data, as shown in [@fig:ct-cube].

![Building matrices of complex samples during CT](figs/ct-samp.pdf){#fig:ct-samp}

![Concurrent processing on 50% overlapped data](figs/ct-cube.pdf){#fig:ct-cube} 

Our CT network thus maps on each beam signal a corner turn process which, under the
SDF execution semantics, consumes $N_{FFT}\times N_b$ ordered samples, interprets them
as a matrix, transposes this matrix, and produces $N_b\times N_{FFT}$ samples ordered
in the direction suggested in @fig:ct-samp. In order to achieve 50% overlapping
between the two output channels, the left one needs to be "delayed" with a prefix
signal equivalent to half a video cube. In this case that prefix is formed of
$\frac{N_b \times N_{FFT}}{2}$ complex zeroes on each beam path. This way, whatever
input arrives from the PC stage, will be observed at the left channel only after
$N_{FFT}/2$ samples.

> ct :: Beam (SDF.Signal CpxData)
>    -> (Beam (SDF.Signal CpxData),
>        Beam (SDF.Signal CpxData))
> ct = V.farm12 procCT
> 
> procCT :: Num a => SDF.Signal a -> (SDF.Signal a, SDF.Signal a)
> procCT sig = (cornerTurn rightChannel, cornerTurn leftChannel)
>   where
>     rightChannel = sig
>     leftChannel  = SDF.delay initBatch sig
>     initBatch    = replicate (nb * nFFT `div` 2) 0
>     cornerTurn   = SDF.comb11 (nFFT * nb, nb * nFFT,
>                                fromMatrix . M.transpose . matrix nb nFFT)

![CT network](figs/ct-net-atom.pdf){#fig:ct-net-atom}

*Modeling tips:* the application specification mentions that the first $N_{FFT}$ batch
 of pulses is ignored, yet we do the other way around: we "fill in" with dummy
 data. Although in ForSyDe-Atom it is possible to "clean up" signals using any class
 of dynamic dataflow (i.e. adaptive, scenario-aware) processes, doing so without a
 proper system-wide causality or schedulability analysis is considered bad practice,
 especially since at this stage in development we have no knowledge whether the AESA
 processing chain is going to be part of a closed-loop system or not. Ignoring the
 beginning of a signal implies that some parts of the system start "in the future",
 which can cause serious problems due to non-deterministic behavior especially if
 feedback is involved. Starting a design process with such an assumption is dangerous,
 this is why we "stay safe" and consider that the _entire_ system has _completely
 determined_ behavior starting from time 0, even if this means filling up 50% of the
 first batch with junk/dummy data. We pass the responsibility of ignoring the effects
 of this junk data to an observer (i.e. testbench, sink), which has full knowledge of
 this effect can provide a safe open-loop environment. We shall see this soon in
 section @sec:atom-sim where we test the system and gather only the relevant
 output. Still, using dynamic processes is a versatile modeling technique but their
 analysis is far from trivial and we leave their study for a future report.
 
 <!-- In fact ForSyDe-Atom does not allow "cleaning up" (i.e. dropping, ignoring) -->
 <!-- events from signals, due to non-determinism introduced in case of possible feedback -->
 <!-- loops. Only an observer (i.e. testbench, sink) is allowed to do that, outside the -->
 <!-- process network, as we shall see soon, when testing the system. -->

| Function                 | Original module                       | Package                 |
|--------------------------|---------------------------------------|-------------------------|
| `farm12`                 | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| (`from`-)`matrix`, `transpose` | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `comb11`, `delay`        | [`ForSyDe.Atom.MoC.SDF`]              | forsyde-atom            |
| `nb`, `nFFT`             | `ForSyDe.AESA.Params`                 | aesa-atom               |

 #### Doppler Filter Bank (DFB){#sec:dfb-atom}

During the Doppler filter bank, every window of samples, associated with each range
bin is transformed into a Doppler channel and the complex samples are converted to
real numbers by calculating their envelope. Since the samples have been arranged in
pulse window-order during the previous stage, the DFB transformation is applied over a
window of $N_{FFT}$ samples arriving in-order, like in @fig:dfb-samp.

![Doppler Filter Bank on streams of complex samples](figs/dfb-samp.pdf){#fig:dfb-samp}

The `dfb` process applies the the following chain of functions on each window of
complex samples, in three consecutive steps:

 * scale the window samples with a set of coefficients to decrease the Doppler side
   lobes from each FFT output and thereby to increase the clutter rejection.

 * apply an $N_{FFT}$-point 2-radix decimation in frequency Fast Fourier Transform
   (FFT) algorithm.

 * compute the envelope of each complex sample when phase information is no longer of
   interest. The envelope is obtained by calculating the absolute value of the complex
   number, converting it into a real number.


> dfb :: Beam (SDF.Signal CpxData)
>     -> Beam (SDF.Signal RealData)
> dfb = V.farm11 procDFB
> 
> procDFB :: SDF.Signal CpxData -> SDF.Signal RealData
> procDFB = SDF.comb11 (nFFT, nFFT, fromVector . fDFB . vector)
>   where
>     fDFB       = V.farm11 envelope . fft nS . V.farm21 (*) (mkWeightCoefs nFFT)
>     envelope a = let (i, q) = (realPart a, imagPart a)
>                  in sqrt (i * i + q * q)

![DFB network](figs/dfb-net-atom.pdf){#fig:dfb-net-atom}

| Function           | Original module                    | Package                 |
|--------------------|------------------------------------|-------------------------|
| `farm11`,` farm21` | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom            |
| `fft`              | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkWeightCoefs`    | `ForSyDe.AESA.Coefs`               | aesa-atom               |
| `nS`, `nFFT`       | `ForSyDe.AESA.Params`              | aesa-atom               |

*Modeling tips:* each function composing $f_{DFB}$ is itself inherently parallel, as
it is described in terms of parallel skeletons. We could have "lifted" these skeletons
as far as associating a process for each elementary arithmetic operation, following
the example set in @sec:dbf-atom. Although the two representation (if carefully
modeled) are semantically equivalent, at this stage the modeling choice should be
driven by the designer's intuition of the application's behavior. Only further in the
design process, thanks to the formal description, can choose, or transform (ideally
being aided by a computer/tool) between different equivalent representations, one
which is more appropriate to the target platform model. The skeleton lifting to the
process network level is left as an exercise for the reader. The interested reader is
also recommended to read the skeletons chapter in the technical manual [@atom-manual]
to see how the `fft` skeleton is defined, and how it behaves during different
instantiations.

 #### Constant False Alarm Ratio (CFAR)

The CFAR normalizes the data within the video cubes in order to maintain a constant
false alarm rate with respect to a detection threshold. This is done in order to keep
the number of false targets at an acceptable level by adapting the normalization to
the clutter situation in the area (around a cell under test) of interest. The
described process can be depicted as in @fig:cfar-cube which suggests the
[stencil](https://en.wikipedia.org/wiki/Stencil_code) data accessing pattern within
the video cubes.

![Constant False Alarm Ratio on cubes of complex samples](figs/cfar-cube.pdf){#fig:cfar-cube}

> cfar :: Beam (SDF.Signal RealData)
>      -> Beam (SDF.Signal (Range (Window RealData)))
> cfar = V.farm11 procCFAR
>
> procCFAR = SDF.comb11 (nb * nFFT, 1, (:[]) . fCFAR . M.matrix nFFT nb)

![CFAR network](figs/cfar-proc-atom.pdf){#fig:cfar-net-atom}


Similar to the `cornerTurn` process in @sec:ct-atom, the `procCFAR` process builds
up matrices of $N_b\times N_{FFT}$ samples and applies $f_{CFAR}$ function on these
matrices. The $f_{CFAR}$ function normalizes each Doppler window, after which the
sensitivity will be adapted to the clutter situation in current area, as seen in
@fig:cfar-signal. The blue line indicates the mean value of maximum of the left and
right reference bins, which means that for each Doppler sample, a swipe of
neighbouring bins is necessary, as suggested by [@fig:cfar-cube]. This is a typical
pattern in signal processing called
[stencil](https://en.wikipedia.org/wiki/Stencil_code), which will constitute the main
parallel skeleton within the $f_{CFAR}$ function.

![The signal level within one pulse window: a) before CFAR; b) after CFAR](figs/cfar-signal.pdf){#fig:cfar-signal width=400px}

> fCFAR :: Range (Window RealData) -> Range (Window RealData)
> fCFAR rbins = V.farm41 (\m -> V.farm31 (normCfa m)) md rbins lmv emv
>   where
>     md  = V.farm11 (logBase 2 . V.reduce min) rbins
>     emv = (V.fanoutn (nFFT + 1) dummy) <++> (V.farm11 aritMean neighbors)
>     lmv = (V.drop 2 $ V.farm11 aritMean neighbors) <++> (V.fanout dummy) 
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     aritMean :: Vector (Vector RealData) -> Vector RealData
>     aritMean  = V.farm11 (/n) . V.reduce addV . V.farm11 geomMean . V.group 4
>     geomMean  = V.farm11 (logBase 2 . (/4)) . V.reduce addV
>     -----------------------------------------------
>     dummy     = V.fanoutn nFFT $ (-maxFloat)/n
>     neighbors = V.stencil nFFT rbins
>     -----------------------------------------------
>     addV      = V.farm21 (+)
>     n         = fromIntegral nFFT


| Function                                  | Original module                  | Package      |
|-------------------------------------------|----------------------------------|--------------|
| `farm`[`4`/`3`/`1`]`1`, `reduce`, `<++>`, | [`ForSyDe.Atom.Skeleton.Vector`] | forsyde-atom |
| `drop`, `fanout`, `fanoutn`, `stencil `   |                                  |              |
| `comb11`                                  | [`ForSyDe.Atom.MoC.SDF`]         | forsyde-atom |
| `maxFloat`                                | ForSyDe.AESA.Coefs               | aesa-atom    |
| `nb`, `nFFT`                              | `ForSyDe.AESA.Params`            | aesa-atom    |

The $f_{CFAR}$ function itself can be described with the system of [@eq:cfar], where

 * $MD$ is the minimum value over all Doppler channels in a batch for a specific data
   channel and range bin.

 * $EMV$ and $LMV$ calculate the early and respectively late mean values from the
   neighboring range bins as a combination of geometric and arithmetic mean values.

 * $eb$ and $lb$ are the earliest bin, respectively latest bin for which the CFAR can
   be calculated as $EMV$ and $LMV$ require at least $N_{FFT}$ bins + 1 guard bin
   before and respectively after the current bin. This phenomenon is also called the
   "stencil halo", which means that CFAR, as defined in [@eq:cfar] is applied only on
   $N_b'=N_b-2N_{FFT}-2$ bins.
   
 * bins earlier than $eb$, respectively later than $lb$, are ingnored by the CFAR
   formula and therefore their respective EMV and LMV are replaced with the lowest
   representable value.
   
 * 5 is added to the exponent of the CFAR equation to set the gain to 32 (i.e. with
   only noi se in the incoming video the output values will be 32).

$$\begin{aligned}&\left\{\begin{aligned}
  &CFAR(a_{ij})= 2^{(5 + \log_2 a_{ij}) - \max (EMV(a_{ij}),LMV(a_{ij}),MD(a_{ij}))}\\
  &EMV(a_{ij}) = \frac{1}{N}\sum_{k=0}^{N-1}\left(\log_2\left(\frac{1}{4}\sum_{l=0}^{3}a_{(i-2-4k-l)j}\right)\right)\\
  &LMV(a_{ij}) = \frac{1}{N}\sum_{k=0}^{N-1}\left(\log_2\left(\frac{1}{4}\sum_{l=0}^{3}a_{(i+2+4k+l)j}\right)\right)\\
  &MD(a_{ij})  = \log_{2}\left(\min_{k=1}^N(a_{ik})\right)
  \end{aligned}\right.\\
  &\qquad \forall i\in[eb,lb], j\in[1,N] \text{ where }\left\{
  \begin{aligned}
  &N = N_{FFT}\\
  &eb = N_{FFT} + 1\\
  &lb = N_b - N_{FFT} - 1\\
  \end{aligned}\right.
  \end{aligned}
$${#eq:cfar}

The first thing we calculate is the $MD$ for each Doppler window (row). For each row
of `rbins` (i.e. range bins of Doppler windows) we look for the minimum value
(`reduceV min`) and apply the binary logarithm on it.

Another action performed over the matrix `rbins` is to form two stencil "cubes" for
EMV and LMV respectively, by gathering batches of $N_{FFT}$ Doppler windows like in
[@eq:cfar-stencil], computing them like in [@eq:cfar-emv].

$$
  \stackrel{\mbox{rbins}}{
  \begin{bmatrix}
  a_{11} & a_{12} & \cdots & a_{1N_{FFT}} \\
  a_{21} & a_{22} & \cdots & a_{2N_{FFT}} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{N_b1} & a_{N_b2} & \cdots & a_{N_bN_{FFT}}
  \end{bmatrix}}
  \stackrel{\mathtt{stencil}}{\rightarrow}
  \stackrel{\mbox{neighbors}}{
  \begin{bmatrix}
  \begin{bmatrix}
  a_{11} & a_{12} & \cdots & a_{1N_{FFT}} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{N_{FFT}1} & a_{N_{FFT}2} & \cdots & a_{N_{FFT}N_{FFT}} \\
  \end{bmatrix}\\
  \begin{bmatrix}
  a_{21} & a_{22} & \cdots & a_{2N_{FFT}} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{(N_{FFT}+1)1} & a_{(N_{FFT}+1)2} & \cdots & a_{(N_{FFT}+1)N_{FFT}} \\
  \end{bmatrix}\\
  \vdots \\
  \begin{bmatrix}
  a_{(N_b-N_{FFT})1} & a_{(N_b-N_{FFT})2} & \cdots & a_{(N_b-N_{FFT})N_{FFT}}\\
  \vdots & \vdots & \ddots & \vdots \\
  a_{N_b1} & a_{N_b2} & \cdots & a_{N_bN_{FFT}}
  \end{bmatrix}
  \end{bmatrix}}
$${#eq:cfar-stencil}

Each one of these neighbors matrices will constitute the input data for calculating
the $EMV$ and $LMV$ for each Doppler window. $EMV$ and $LMV$ are calculated by
applying the mean function `arithMean` over them, as shown (only for the window
associated with the $eb$ bin) in [@eq:cfar-emv]. The resulting `emv` and `lmv`
matrices are padded with rows of the minimum representable value `-maxFloat`, so that
they align properly with `rbins` in order to combine into the 2D farm/stencil defined
at @eq:cfar. Finally, `fCFAR` yields a matrix of normalized Doppler windows. The resulting matrices are not transformed back into sample streams by the parent process, but rather they are passed as single tokens downstream to the INT stage, where they will be processed as such.

$$\begin{aligned}
  &\begin{bmatrix}
  a_{11} & \cdots & a_{1N_{FFT}} \\
  \vdots  & \ddots & \vdots \\
  a_{N_{FFT}1}  & \cdots & a_{N_{FFT}N_{FFT}}
  \end{bmatrix}
  \stackrel{\mathtt{group}}{\rightarrow}
  \begin{bmatrix}
  \begin{bmatrix}
  a_{11} & \cdots & a_{1N_{FFT}} \\
  \vdots & \ddots & \vdots \\
  a_{41} & \cdots & a_{4N_{FFT}}
  \end{bmatrix}\\
  \vdots \\
  \begin{bmatrix}
  a_{(N_{FFT}-4)1}  & \cdots & a_{(N_{FFT}-4)N_{FFT}}\\
  \vdots & \ddots & \vdots \\
  a_{N_{FFT}1}  & \cdots & a_{N_{FFT}N_{FFT}}
  \end{bmatrix}
  \end{bmatrix}\\
  &\stackrel{\mathtt{farm(geomMean)}}{\rightarrow}
  \begin{bmatrix}
  \log_2\frac{1}{4}\sum_{i=1}^{4}a_{i1} & \cdots & \log_2\frac{1}{4}\sum_{i=1}^{4}a_{iN_{FFT}} \\
  \vdots & \ddots & \vdots \\
  \log_2\frac{1}{4}\sum_{i=N_{FFT}-4}^{N_{FFT}}a_{i1} & \cdots & \log_2\frac{1}{4}\sum_{i=N_{FFT}-4}^{N_{FFT}}a_{iN_{FFT}}
  \end{bmatrix}\\
  &\stackrel{\mathtt{(/N_{FFT})\circ reduce(+)}}{\rightarrow}
  \begin{bmatrix}
  EMV(a_{eb,1}) & \cdots & EMV(a_{eb,N_{FFT}}) 
  \end{bmatrix}
  \end{aligned}
$${#eq:cfar-emv}


 #### Integrator (INT){#sec:int-atom}

During the last stage of the video processing chain each data sample of the video cube
is integrated against its 8 previous values using an 8-tap FIR filter, as suggested by
the drawing in @fig:int-cube-atom.

![Integration on cubes of complex samples](figs/int-cube.pdf){#fig:int-cube-atom}

The integration depicted in @fig:int-cube-atom, like each stage until now, can be
modeled in dozens of different ways based on how the designer envisions the
partitioning of the data "in time" or "in space". This partitioning could be as
coarse-grained as streams of cubes of samples, or as fine-grained as networks of
streams of indvidual samples. For convenience and for simulation efficiency[^eff] we
choose a middle approach: video cubes are represented as farms (i.e. vectors) of
streams of matrices, as conveniently bundled by the previous DFB stages. We pass the
responsibility of re-partitioning and interpreting the data accordingly to the
downstream process, e.g. a control/monitoring system, or a testbench sink.

> int :: Beam (SDF.Signal (Range (Window RealData)))
>     -> Beam (SDF.Signal (Range (Window RealData)))
>     -> Beam (SY.Signal  (Range (Window RealData)))
> int = V.farm21 procINT

Before integrating though, the data from both the left and the right channel need to
be merged. This is done by the process `merge` below, which consumes one (matrix)
token from each channel and interleaves them at its output. When considering only
abstract tokens, the `merge` process can be regarded as an up-sampler with the rate
2/1. When taking into consideration the size of the entire data set (i.e. token rates
$\times$ structure sizes $\times$ data size), we can easily see that the overall
required system bandwidth (ratio) remains the same between the PC and INT stages,
i.e. $\frac{2\times N_B \times N_{b} \times N_{FFT}\times
\mathit{size}(\mathtt{RealData})}{N_B \times N_{b} \times N_{FFT}\times
\mathit{size}(\mathtt{CpxData})}=1/1$. For the integration stage `firNet` it is more
appropriate to translate back to SY MoC semantics, hence the `toSY` domain interface.

![INT network](figs/int-net.pdf){#fig:int-net-atom}

> procINT :: Fractional a => SDF.Signal (Matrix a) -> SDF.Signal (Matrix a) -> SY.Signal (Matrix a)
> procINT cr = firNet mkIntCoefs . SDF.toSY . merge cr
>   where
>     merge   = SDF.comb21 ((1,1), 2, \[r] [l] -> [r, l])
 
The 8-tap FIR filter used for integration is also a moving average, but as compared to
the `mav` function used in @sec:pc-atom, the window slides in time domain,
i.e. over streaming samples rather than over vector elements. To instantiate a FIR
system we use the `firSk` skeleton provided by the ForSyDe-Atom utility libraries,
which constructs the the well-recognizable FIR pattern in @fig:int-net-atom, i.e. a
recur-farm-reduce composition. In order to do so, `firSk` needs to know _what_ to fill
this template with, thus we need to provide as arguments its "basic" operations, which
in our case are processes operating on signals of matrices.  In fact, `mav` itself is
a _specialization_ of the `firSk` skeleton, which defines its basic operations as
corresponding functions on vectors. This feature derives from a powerful algebra of
skeletons which grants them both modularity, and the possibility to transform them
into semantically-equivalent forms, as we shall soon explore in @sec:refinement.

> firNet :: Num a => Vector a -> SY.Signal (Matrix a) -> SY.Signal (Matrix a)
> firNet coefs = fir' addSM mulSM dlySM coefs
>   where
>     addSM   = SY.comb21 (M.farm21 (+))
>     mulSM c = SY.comb11 (M.farm11 (*c))
>     dlySM   = SY.delay  (M.fanout 0)
   
| Function          | Original module                     | Package                 |
|-------------------|-------------------------------------|-------------------------|
| `farm21`          | [`ForSyDe.Atom.Skeleton.Vector`]    | forsyde-atom            |
| `farm21`,`farm11`,`fanout` | ForSyDe.Atom.Skeleton.Vector.Matrix | forsyde-atom-extensions |
| `firSk`           | ForSyDe.Atom.Skeleton.Vector.DSP    | forsyde-atom-extensions |
| `comb21`,`comb11` | [`ForSyDe.Atom.MoC.SDF`]            | forsyde-atom            |
| `mkFirCoefs`      | ForSyDe.AESA.Coefs                  | aesa-atom               |

[^eff]: we try to avoid unnecessary transposes (i.e. type traversals) which are time-consuming.

 ### System Process Network

Finally, when putting all the blocks together in an equation, we obtain the system
`aesa'` in @fig:aesa-net-atom.

![AESA network as black-box components](figs/aesa-net-atom.pdf){#fig:aesa-net-atom}

> aesa' :: Antenna (SY.Signal CpxData) -> Beam (SY.Signal (Range (Window RealData)))
> aesa' video = int rCfar lCfar
>   where
>     lCfar     = cfar $ dfb lCt
>     rCfar     = cfar $ dfb rCt
>     (rCt,lCt) = ct $ pc $ dbf video

Although completely functional and modular, the system depicted in @fig:aesa-net-atom
is not the most "pleasant" to look at. For the sake of code elegance and to increase
the potential of simulation distribution (see @sec:parallel-sim), we refine the system
`aesa'` to avoid unnecessary merging-splitting of vectors between stages. On the other
hand, from the point of view of sequential performance it does not really matter,
because these patterns are "ignored" (reduced) by Haskell's lazy evaluation system
which can easily identify them as compositions of inverse functions. Once we
understand that between PC and INT the processing path is replicated for each beam
generated by the DBF. Therefore it is possible to fuse the related `farm` skeletons
into the much simpler and more elegant model from @fig:aesa-net-atom2. As both models
are semantically-equivalent an automated or tool-assisted transformation process
should be trivial.


> aesa :: Antenna (SY.Signal CpxData) -> Beam (SY.Signal (Range (Window RealData)))
> aesa = V.farm11 pcToInt . dbf

> pcToInt beam = let (rb,lb) = procCT $ procPC $ SY.toSDF beam
>                    lCFAR   = procCFAR $ procDFB lb
>                    rCFAR   = procCFAR $ procDFB rb
>                in  procINT rCFAR lCFAR

![AESA network when fusing the related `farm`s](figs/aesa-net-atom2.pdf){#fig:aesa-net-atom2}

 ## Coefficients, constants and parameters {#sec:consts-coefs-atom}

This section briefly presents the constants and coefficients which have been used
until now, and which shall be further used throughout this report.
