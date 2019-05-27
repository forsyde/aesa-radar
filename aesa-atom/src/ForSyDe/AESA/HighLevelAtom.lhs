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
> module ForSyDe.AESA.HighLevelAtom where

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
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP

 ### Type Aliases and Constants{#sec:aliases-shallow label="Type Aliases and Constants"}

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

Here we define the size constants, for a simple test scenario provided by Saab AB. The
size `nA` can be inferred from the size of input data and the vector operations.

> nA   =   16 :: Int -- does not really affect the application
> nB   =    8 :: Int
> nb   = 1024 :: Int
> nFFT =  256 :: Int

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

 #### Digital Beamforming (DBF){#sec:dbf-atom-net}

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
>     beamSigs   = V.farm11 (V.reduce (SY.comb21 (+))) beamMatrix
>     beamMatrix = M.farm21 (\c -> SY.comb11 (*c)) beamConsts sigMatrix
>     sigMatrix  = V.farm11 V.fanout antennaSigs
>     beamConsts = mkBeamConsts (V.length antennaSigs) nB


| Function                     | Original module                       | Package                 |
|------------------------------|---------------------------------------|-------------------------|
| `farm11`, `reduce`, `length` | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| `farm21`                     | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `comb11`, `comb21`           | [`ForSyDe.Atom.MoC.SY`]               | forsyde-atom            |
| `mkBeamConsts`               | `ForSyDe.AESA.Coefs`                  | aesa-atom               |
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

 #### Pulse Compression (PC){#sec:pc-atom-net}

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
| `mav`       | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkPcCoefs` | `ForSyDe.AESA.Coefs`               | aesa-atom               |
\suppressfloats

Following the reasoning above, we instantiate the PC video processing stage as a
`farm` of SDF processes `procPC` as depicted in [@fig:pc-proc-atom]. Notice that
before being able to apply the SDF actors we need to translate the SY signals yielded
by the DBF stage into SDF signals. This is done by the `toSDF` interface which is an
injective mapping from the (timed) domain of a SY MoC tag system, to the (untimed)
codomain of a SDF MoC tag system. For more on tag systems please consult [@lee98].

> procPC :: Fractional a => SDF.Signal a -> SDF.Signal a 
> procPC = SDF.comb11 (nb, nb, V.fromVector . mav mkPcCoefs . V.vector)

The `procPC` actor consumes and produces `nb` tokens each firing, forms a `Vector`
from these tokens, and applies the `mav` skeleton on these vectors. The `mav` skeleton
is a utility formulated in terms of primitive skeletons (i.e. `map` and `reduce`) on
numbers, i.e. lifting arithmetic functions. We will study this skeleton later in this
report and for now we take it "for granted", as conveniently provided by the `DSP`
utility library. Also notice that the type signature for `procPC` is left polymorphic
as to be more convenient later when we formulate properties over it.


 #### Corner Turn (CT)

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

![CT network](figs/ct-net-atom.pdf){#fig:ct-net-atom}

> ct :: Beam (SDF.Signal CpxData)
>    -> (Beam (SDF.Signal CpxData),
>        Beam (SDF.Signal CpxData))
> ct = V.farm12 procCT
> 
> procCT :: Num a => SDF.Signal a -> (SDF.Signal a, SDF.Signal a)
> procCT sig = (rightChannel, leftChannel)
>   where
>     rightChannel = cornerTurn sig
>     leftChannel  = SDF.delay initBatch rightChannel
>     initBatch    = replicate (nb * nFFT `div` 2) 0
>     cornerTurn   = SDF.comb11 (nFFT * nb, nb * nFFT,
>                                fromMatrix . M.transpose . matrix nb nFFT)

| Function                       | Original module                       | Package                 |
|--------------------------------|---------------------------------------|-------------------------|
| `farm12`                       | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| (`from`-)`matrix`, `transpose` | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `comb11`, `delay`              | [`ForSyDe.Atom.MoC.SDF`]              | forsyde-atom            |

*Modeling tips:* the application specification mentions that the first $N_{FFT}$ batch
 of pulses is ignored, yet we do the other way around: we "fill in" with dummy
 data. In fact ForSyDe-Atom does not allow "cleaning up" (i.e. dropping, ignoring)
 events from signals, due to non-determinism introduced in case of possible feedback
 loops. Only an observer (i.e. testbench, sink) is allowed to do that, outside the
 process network, as we shall see soon, when testing the system.

 #### Doppler Filter Bank (DFB){#sec:dfb-atom-net}


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

![DFB network](figs/dfb-net-atom.pdf){#fig:dfb-net-atom}

> dfb :: Beam (SDF.Signal CpxData)
>     -> Beam (SDF.Signal RealData)
> dfb = V.farm11 procDFB
> 
> procDFB = SDF.Signal CpxData -> SDF.Signal RealData
> procDFB = SDF.comb11 (nFFT, nFFT, fromVector . fDFB . vector)
>   where
>     fDFB       = V.farm11 envelope . fft 8 . V.farm21 (*) mkWeightCoefs 
>     envelope a = let (i, q) = (realPart a, imagPart a)
>                  in sqrt (i * i + q * q)

| Function           | Original module                    | Package                 |
|--------------------|------------------------------------|-------------------------|
| `farm11`,` farm21` | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom            |
| `fft`              | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkWeightCoefs`    | `ForSyDe.AESA.Coefs`               | aesa-atom               |

Each function composing $f_{DFB}$ is itself inherently parallel, as it
is described in terms of parallel skeletons. We could have "lifted"
these skeletons as deep as associating a process for each elementary
arithmetic operation, following the example set in
@sec:dbf-atom-net. This, however, is left as an exercise for the
reader. The interested reader is also recommended to read the
associated chapter in the technical manual (@atom-manual) to see how
the `fft'`[^fftA] skeleton behaves when instantiated as a function
versus as a butterfly network of processes acting under different MoC
semantics.

[^fftA]: see `ForSyDe.Atom.Skeleton.Vector.DSP.fft'` from
            the `forsyde-atom-extensions` documentation.

 #### Constant False Alarm Ratio (CFAR)

The CFAR, as presented earlier in @sec:cfar-shallow is by far the most
intensive stage in terms of data interaction patterns, dominated by
numerous reduction-type operations between neighboring elements which
follow stencil accessing patterns as suggested in @fig:cfar-cube-atom.

![Constant False Alarm Ratio on cubes of complex samples](figs/cfar-cube.pdf){#fig:cfar-cube-atom}

Strictly speaking from a functional point of view, we could implement
the CFAR in many different manners for example:

1. following the style of the previous DFB stage in @sec:dfb-atom-net,
i.e. for each beam: consume $N_{b}\times N_{FFT}$ sample tokens
$\rightarrow$ apply $f_{CFAR}$ $\rightarrow$ produce $N_{b'}\times
N_{FFT}$ sample tokens, where $f_{CFAR}$ is _exactly_ the same
matrix-wise operation as in [@sec:cfar-shallow;@sec:cfar-atom].

2. following the style presented for the DBF stage in
@sec:dbf-atom-net, i.e. for each beam lift _all_ the elementary
operations into the signal domain, associating a process (i.e. a time
behavior) for each data interaction (e.g. arithmetic function). This
would expose the inherent potential of these interactions to be
executed in parallel or concurrently.

3. a combination of the two styles exposing parts of the algorithm as
a concurrent process network and "concealing" other parts within
process functions.

From a design automation point of view choosing one style over the
other should not really matter since, if designed correctly, any of
the three approaches should be perfectly isomorphic and semantically
equivalent. In other words they inherently express the same potential
for concurrency, the only thing being shaped is the designer's
intuition on "how the system behaves".

For didactic purpose, we describe the behavior of the CFAR stage by
employing a third approach: we lift some operations (to some extent)
into the signal domain. Translating the matrix operation $f_{CFAR}$
from @sec:cfar-shallow into a network of processes, we obtain the
design in @fig:cfar-net-atom.

![CFAR network](figs/cfar-net-atom.pdf){#fig:cfar-net-atom}

> cfar :: Beam (SDF.Signal RealData)
>      -> Beam (SDF.Signal RealData)
> cfar = V.farm11 (pCFAR . assign)

We describe the CFAR network as a
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
distributed over all beams, where each worker is logically separated
into two stages:

 * `assign`, which partitions the data into the appropriate work sets
   based on the algorithm accessing patterns of @eq:cfar.

 * `pCFAR` maps @eq:cfar on the partitioned data and obtains the
   output matrix of normalized Doppler bins.
        
> assign :: SDF.Signal RealData
>        -> Vector (SDF.Signal (Matrix RealData)
>                  ,SDF.Signal (Vector RealData)
>                  ,SDF.Signal (Matrix RealData))
> assign = V.farm11 get . distrib . neighbors
>   where
>     neighbors = SDF.comb11 (nb * nFFT, nb',
>                             fromVector . V.stencil (2*nFFT+3) . matrix nFFT nb)
>     distrib   = SDF.distribute (V.fanoutn nb' 1)
>     get smx   = (early smx, pres smx, late smx)
>       where
>         early = SDF.comb11 (1,1,\[a] -> [V.take nFFT a])
>         pres  = SDF.comb11 (1,1,\[a] -> [V.first $ V.drop (nFFT + 1) a])
>         late  = SDF.comb11 (1,1,\[a] -> [V.drop (nFFT + 3) a])

The `assign` process first consumes $N_{b}\times N_{FFT}$ tokens,
forms a $\mathtt{Bin}\times\mathtt{Window}$ matrix and applies the
`stencil`[^stenA] pattern, creating $N_{b'}$ matrices of $2N_{FFT}+3$
neighboring Doppler windows. These matrix tokens are then
`distribute`[^distributeA]d to a
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
where respectively $N_{b'}$ workers are partitioning each matrix of
neighbors into Doppler windows corresponding to _early_, _present_ and
_late_ range bins, as described by @eq:cfar.

**NOTE:** It is important for the reader to understand that at this
  level of abstraction, passing "data" around is the only analyzable
  way to describe behavior, however it is irrelevant what those means
  of the data transmission will eventually become in an actual
  implementation. For example consuming $x$ tokens does not
  necessarily imply that those $x$ tokens "arrive" in sequence from
  somewhere, but rather the consumption rate gives a quantum and a
  measure of the data necessary to operate on. Depending on the
  available resources and platforms, the data may be passed through
  e.g. virtual channels, locations in shared memory, even locations in
  the local memory of a multi-processor unit. It is up to the analysis
  and the design space exploration stages in a design flow to decide
  upon the relevant means to represent ForSyDe signals. Similarly,
  passing "one token" which consists of a matrix does not necessarily
  mean that we transmit the whole matrix data to another physical
  process; in the case of the `assign` process passing "tokens of
  matrices" around subtly represents the designer's intuition of
  communicating position information in existing data sets rather than
  the sets themselves.

[^distributeA]: see `ForSyDe.Atom.MoC.SDF.distribute` from the
            `forsyde-atom-extensions` documentation.

> pCFAR :: Vector (SDF.Signal (Matrix RealData)
>                 ,SDF.Signal (Vector RealData)
>                 ,SDF.Signal (Matrix RealData))
>       -> SDF.Signal RealData
> pCFAR = SDF.merge (V.fanoutn nb' nFFT) . V.farm11 calc
>   where
>     calc (e,m,l) = normP (minVP m) (meanP e) (meanP l) m
>     ------------------------------------------------------
>     normP = SDF.comb41
>             ((1,1,1,1), nFFT,
>              \[m] [e] [l] [a] -> fromVector $ V.farm31 (norm m) e l a)
>     minVP = SDF.comb11 (1,1,\[a] -> [minVal a])
>     meanP = SDF.comb11 (1,1,\[a] -> [arithMean a])
>     ------------------------------------------------------
>     norm m e l a = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     minVal    = logBase 2 . V.reduce min
>     arithMean = V.farm11 (/n) . V.reduce addV . V.farm11 geomMean . V.group 4
>     geomMean  = V.farm11 (logBase 2 . (/4)) . V.reduce addV
>     addV      = V.farm21 (+)
>     n         = fromIntegral nFFT

The second part of of this stage, `pCFAR` is actually applying the
CFAR system of @eq:cfar on each of the $N_{b'}$ triples formed of
_early_, _present_ and _late_ Doppler windows. Each worker is
processing these matrices accordingly, we obtain one window of
$N_{FFT}$ normalized Doppler bins. Finally, the resulting $N_{b'}$
windows are `merge`[^mergeA]d into a consistent SDF signal of Doppler
bins, and thus producing $N_{b'}\times N_{FFT}$ tokens which are
passed further in the downstream processing.

[^mergeA]: see `ForSyDe.Atom.MoC.SDF.merge` from the
            `forsyde-atom-extensions` documentation.

 #### Integrator (INT)

During the last stage of the video processing chain each data sample
of the video cube is integrated against its 8 previous values using an
8-tap FIR filter, as presented earlier in @sec:int-shallow and
suggested by the drawing in @fig:int-cube-atom.

![Integration on cubes of complex samples](figs/int-cube.pdf){#fig:int-cube-atom}

For modeling the INT stage we choose the fully-exposed network
approach from @fig:int-net-atom by distributing each $N_{b'}\times
N_{FFT}$ tokens received from the previous stage into an _own signal_,
i.e. obtaining a matrix of signals. This is done for each path
associated with a beam, thus in the end we obtain a _cube of
signals_. Each signals is synchronous with each other and the event
rate is in fact the rate of processing a corner-turned cube. We
express this by interfacing all signals back to the SY domain. From
there on we can pass each one through an own FIR filter and obtain, on
each channel, the integrated radar output.

![INT network](figs/int-net-atom.pdf){#fig:int-net-atom}

> int :: Beam (SDF.Signal RealData)
>     -> Beam (SDF.Signal RealData)
>     -> Beam (CRange (Window (SY.Signal RealData)))
> int cr cl = C.farm11 firChain $ C.farm21 addSY (unroll cr) (unroll cl)
>   where
>     unroll   = C.farm11 SDF.toSY .
>                M.farm11 (SDF.distribute (V.fanoutn nFFT 1)) .
>                V.farm11 (SDF.distribute (V.fanoutn nb' nFFT))
>     firChain = fir' addSY mulSY (SY.delay 0) mkFirCoefs
>     addSY    = SY.comb21 (+)
>     mulSY c  = SY.comb11 (*c)

**NOTE:** continuing the side idea started in the previous section, it
  is easy to observe that the last `merge` process from CFAR and the
  first `distribute` process from INT are in fact inverse
  transformations and cancel each other. These "artifices", along with
  other similar examples throughout the design, are perfectly
  acceptable in a design, once we understand that they are merely
  semantic interfaces resulted from the chosen modeling paradigm(s)
  and should not affect at all the final implementation of the
  system. Even more, these modeling artifacts aids the designer to
  partition the design into logical components with clearly defined
  interfaces, as seen below.

 ### System Process Network

Finally, when putting all the instantiated blocks together in an
equational style (see code), we obtain the system in
@fig:aesa-net-atom.

![AESA network as black-box components](figs/aesa-net-atom.pdf){#fig:aesa-net-atom}

> aesa :: Antenna (SY.Signal CpxData)
>      -> Beam (CRange (Window (SY.Signal RealData)))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ dfb lCt
>     rDfb      = cfar $ dfb rCt
>     (lCt,rCt) = ct $ pc $ dbf video

The system model in @fig:aesa-net-atom can further be refined once we
understand that from PC to INT the processing path is replicated for
each beam generated by the DBF. Therefore it is possible to fuse the
related `farm` skeletons into the much simpler and more elegant model
from @fig:aesa-net-atom2. As both models are semantically-equivalent
an automated or tool-assisted transformation process should be
trivial.

![AESA network when fusing the related `farm`s](figs/aesa-net-atom2.pdf){#fig:aesa-net-atom2}
