 ## A High-Level Model{#sec:atom-operation}

This section presents a high-level behavioral model of the AESA signal processing
chain presented in [@sec:video-chain-spec]. This model follows intuitive and didactic
way to tackle the challenge of translating the _textual_ specifications into an
_executable_ ForSyDe specification and is not, in any circumstance, the only way to
model the application. As of [@sec:crash-atom] we represent each stage in the AESA
chain as a _process_ acting upon (complete) _cubes_ of data, i.e. as processes of
skeleton functions. At this phase in the system design we do not care _how_ the cubes
are being formed or how the data is being carried around, bur rather on _what_
transformations are applied on the data cubes during each subsequent stage of the AESA
pipeline. The purpose of the model is to provide a executable _reference_ for the AESA
system functionality, that can later be derived to more efficient descriptions.

The code for this section is written in the following module, see [@sec:usage] on how
to use it:

> {-# LANGUAGE PackageImports #-} -- allows explicit import of modules from custom
>                                 -- libraries instead of standard ones. Will be taken
>                                 -- out once the extensions are merged upstream.
> module ForSyDe.AESA.CubesAtom where

As the AESA application uses complex numbers, we use Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html) type.

> import Data.Complex

The only timed behavior exerted by the model in this section is the causal,
i.e. ordered, passing of cubes from one stage to another. In order to enable a simple,
abstract, and thus analyzable "pipeline" behavior this passing can be described
according to the _perfect synchrony hypothesis_, which assumes the processing of each
event (cube) takes an infinetly small amount of time and it is ready before the next
sinchronization point. This in turn implies that all events in a system are
synchronized, enabling the description of fully deterministic behaviors over infinite
streams of events. These precise execution semantics are captured by the _synchronous
reactive (SY) model of computation (MoC)_ (@leeseshia-15,@Benveniste03), hence we
import the [`SY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html)
library from the
_[MoC](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html) layer_ of
ForSyDe-Atom, see [@ungureanu17], using an appropriate alias.

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY

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
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Cube   as C
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

 ### Video Processing Pipeline Stages

In this section we follow each stage described in [@sec:video-chain-spec], and model
them as a processes operating on cubes (three-dimensional vectors) of antenna samples.

 #### Digital Beamforming (DFB){#sec:dbf-atom}

The DBF receives complex in data, from $N_A$ antenna elements and forms $N_B$
simultaneous receiver beams, or "listening directions", by summing individually
phase-shifted in data signals from all elements. Considering the indata video cube,
the transformation applied by DBF, could be depicted as in [@fig:dbf-cube].

![DBF on video structure](figs/dbf-cube.pdf){#fig:dbf-cube}

Considering the application specification in @sec:video-chain-spec on the input data,
namely _"for each antenna the data arrives _pulse by pulse_, and each pulse arrives
_range bin by range bin_"_, we can assume that the video is received as `Antenna
(Window (Range a))` cubes, meaning that the inner vectors are the range bins. However,
@fig:dbf-cube shows that the beamforming function is applied in the antenna direction,
so we need to transpose the cube in such a way that `Antenna` becomes the inner
vector, i.e. `Window (Range (Antenna a))`. We thus describe the DBF stage as a
combinational SY process `comb` acting upon signals of `Cube`s, namely mapping the
beamforming function $f_{DBF}$ on each column of each _pulse matrix_ (see
[@fig:dbf-cube]).

![DBF stage process](figs/dbf-proc-atom.pdf){#fig:dbf-proc-shallow}

> dbf :: Signal (Antenna (Window (Range CpxData)))
>     -> Signal (Window  (Range  (Beam  CpxData)))
> dbf = SY.comb11 (M.farm11 fDBF . C.transpose)

The beamforming function is specified like in [@eq:dbf], where $e_k ,\ \forall k \in
[1,N_A]$ denotes the samples from antenna elements, and respectively $b_i,\ \forall i
\in [1,N_B]$ are samples for each beam. This is in fact a of matrix-vector
multiplication, thus we implement [@eq:dbf] at the highest level of abstraction simply
as matrix/vector operations like in [@eq:dbf-mat].

$$ b_i(n) = \sum_{k=1}^{N_A} e_k(n) \cdot \alpha_{ki}\qquad\forall i \in [1,N_B] $$ {#eq:dbf}

> fDBF :: Antenna CpxData -- ^ input antenna elements
>      -> Beam    CpxData -- ^ output beams
> fDBF antennaEl = beam
>   where
>     beam       = V.reduce (V.farm21 (+)) beamMatrix
>     beamMatrix = M.farm21 (\c -> (*c)) beamConsts elMatrix
>     elMatrix   = V.farm11 V.fanout antennaEl
>     beamConsts = mkBeamConsts dElements waveLength nA nB :: Matrix CpxData

| Function                     | Original module                       | Package                 |
|------------------------------|---------------------------------------|-------------------------|
| `farm11`, `reduce`, `length` | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| `farm11`, `farm21`           | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `transpose`                  | `ForSyDe.Atom.Skeleton.Vector.Cube`   | forsyde-atom-extensions |
| `comb11`                     | [`ForSyDe.Atom.MoC.SY`]               | forsyde-atom            |
| `mkBeamConsts`               | `ForSyDe.AESA.Coefs`                  | aesa-atom               |
| `dElements`, `waveLenth`, `nA`, `nB` | `ForSyDe.AESA.Params`         | aesa-atom               |

$$ \begin{gathered}
   \stackrel{\mbox{elMatrix}}{
   \begin{bmatrix}
    e_{1} & e_{1} & \dots  \\
    e_{2} & e_{2} & \dots  \\
    \vdots& \vdots& \ddots \\
    e_{N_A} & e_{N_A} & \dots
   \end{bmatrix}}
   \times
   \stackrel{\mbox{beamConsts}}{
   \begin{bmatrix}
    \alpha_{11} & \alpha_{12} & \dots  & \alpha_{1N_B} \\
    \alpha_{21} & \alpha_{22} & \dots  & \alpha_{2N_B} \\
    \vdots      & \vdots      & \ddots & \vdots\\
    \alpha_{N_A1} & \alpha_{N_A2} & \dots  & \alpha_{N_AN_B} 
   \end{bmatrix}}
   \Rightarrow\\\Rightarrow
   \stackrel{\mbox{beamMatrix}}{
   \left.\begin{bmatrix}
    e_1\alpha_{11} & e_1\alpha_{12} & \dots  & e_1\alpha_{1N_B} \\
    e_2\alpha_{21} & e_2\alpha_{22} & \dots  & e_2\alpha_{2N_B} \\
    \vdots & \vdots & \ddots & \vdots\\
    e_{N_A}\alpha_{N_A1} & e_{N_A}\alpha_{N_A2} & \dots  & e_{N_A}\alpha_{N_AN_B} 
   \end{bmatrix}\right\}}
   \sum_\mathit{rows}=
   \stackrel{\mbox{beam}}{
   \begin{bmatrix}
    b_1 & b_2 & \dots  & b_n 
   \end{bmatrix}}
   \end{gathered}
$$ {#eq:dbf-mat}

 #### Pulse Compression (PC){#sec:pc-atom}

In this stage the received echo of the modulated pulse, i.e. the information contained
by the range bins, is passed through a matched filter for decoding their
modulation. This essentially applies a sliding window, or a moving average on the
range bin samples. Considering the video cube, the PC transformation is applied in the
direction shown in @fig:pc-cube, i.e. on vectors formed from the range of every pulse.

![PC: direction of application on video structure (left); process (right)](figs/pc-cube.pdf){#fig:pc-cube}

The PC process is mapping the $f_{PC}$ on each row of the pulse matrices in a cube,
however the previous stage has arranged the cube to be aligned beam-wise. This is why
we need to re-arrange the data so that the innermost vectors are `Range`s instead, and
we do this by simply `transpose`-ing the inner `Range` $\times$ `Beam` matrices into
`Beam` $\times$ `Range` ones.



> pc :: Signal (Window (Range (Beam  CpxData))) 
>    -> Signal (Window (Beam  (Range CpxData)))
> pc = SY.comb11 (V.farm11 (V.farm11 fPC . M.transpose))
> --             ^ == (M.farm11 fPC . V.farm11 M.transpose)

Here the function $f_{PC}$ applies the `fir` skeleton on these vectors (which computes
a moving average if considering vectors). The `fir` skeleton is a utility formulated
in terms of primitive skeletons (i.e. `farm` and `reduce`) on numbers, i.e. lifting
arithmetic functions. We will study this skeleton later in this report and for now we
take it "for granted", as conveniently provided by the `DSP` utility library. For this
application we also use a relatively small average window (5 taps).

> fPC :: Range CpxData -- ^ input range bin     
>     -> Range CpxData -- ^ output pulse-compressed bin
> fPC = fir (mkPcCoefs 5)

| Function              | Original module                       | Package                 |
|-----------------------|---------------------------------------|-------------------------|
| `comb11`              | [`ForSyDe.Atom.MoC.SY`]               | forsyde-atom            |
| `farm11`              | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| `farm11`, `transpose` | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `fir`                 | `ForSyDe.Atom.Skeleton.Vector.DSP`    | forsyde-atom-extensions |
| `mkPcCoefs`           | `ForSyDe.AESA.Coefs`                  | aesa-atom               |

 #### Corner Turn (CT) with 50% overlapping data {#sec:ct-atom}

During the CT a rearrangement of data must be performed between functions that process
data in “different” directions, e.g. range and pulse, in order to be able to calculate
the Doppler channels further in the processing pipeline. The process of corner turning
becomes highly relevant when detailed time behavior of the application is being
derived or inferred, since it demands well-planned design decisions to make full use
of the underlying architecture. At the level of abstraction on which we work right now
though, it is merely a matrix `transpose` operation, and it can very well be posponed
until the beginning of the next stage. However, a much more interesting operation is
depicted in [@fig:ct-cube]: in order to maximize the efficiency of the AESA processing
the datapath is split into two concurrent processing channels with 50% overlapped
data.

![Concurrent processing on 50% overlapped data](figs/ct-cube.pdf){#fig:ct-cube} 

Implmeneting such a behavior requires a bit of "ForSyDe thinking". At a first glance,
the problem seems easily solved considering only the cube structures: just "ignore"
half of the first cube of the right channel, while the left channel replicates the
input. However, there are some timing issues with this setup: from the left channel's
perspective, the right channel is in fact "peaking into the future", which is an
abnormal behavior. Without going too much into details, you need to understand that
_any type of signal "cleaning", like dropping or filtering out events, can cause
serious causality issues in a generic process network, and thus it is **illegal** in
ForSyDe system modeling_. On the other hand we could _delay_ the left channel in a
determinstic manner by assuming a well-defined *initial state* (e.g. all zeroes) while
it waits for the right channel to consume and process its first half of data. This
defines the history of a system where all components start from *time zero* and
eliminates any source of "clairvoyant"/ambiguous behavior.

To keep things simple, we stay within the same time domain, keeping the perfect
synchrony assumption, and instantiating the left channel building mechanism as a
simple Mealy finite state machine. This machine splits an input cube into two halves,
stores one half and merges the other half with the previously stored state to create
the left channel stream of cubes.

![Left channel data builder process](figs/ct-proc-atom.pdf){#fig:ct-proc-atom}

> overlap :: Signal (Window (Beam (Range CpxData)))
>         -> Signal (Window (Beam (Range CpxData)))
> overlap = SY.mealy11 nextState outDecode initState
>   where
>     nextState _ cube = V.drop (nFFT `div` 2) cube
>     outDecode s cube = s <++> V.take (nFFT `div` 2) cube
>     initState        = (V.fanoutn (nFFT `div` 2) . V.fanoutn nB . V.fanoutn nb) 0

| Function                            | Original module                  | Package      |
|-------------------------------------|----------------------------------|--------------|
| `mealy11`                           | [`ForSyDe.Atom.MoC.SY`]          | forsyde-atom |
| `drop`, `take`, `fanoutn`, `(<++>)` | [`ForSyDe.Atom.Skeleton.Vector`] | forsyde-atom |
| `nFFT`, `nA`, `nB`                  | `ForSyDe.AESA.Params`            | aesa-atom    |

**OBS!** Perhaps considering all zeroes for the initial state might not be the best
design decision, since that is in fact "junk data" which is propagated throughout the
system and which alters the expected behavior. A much safer (and semantically correct)
approach would be to model the initial state using *absent events* instead of
arbitrary data. However this demands the introduction of a new layer and some quite
advanced modeling concepts which are out of the scope of this report[^fn:abst]. For
the sake of simplicity we now consider that the initial state is half a cube of zeroes
and that there are no absent events in the system. As earlier mentioned, it is
*illegal* to assume any type of signal cleaning during system modeling, however this
law does not apply to the *observer* (i.e. the testbench), who is free to take into
consideration whichever parts of the signals it deems necessary. We will abuse this
knowledge in order to show a realistic output behavior of the AESA signal processing
system: as "observers", we will ignore the effects of the initial state propagation
from the output signal and instead plot only the useful data.

[^fn:abst]: For more information on absent semantics, check out Chapter 3 of [@leeseshia-15]

 #### Doppler Filter Bank (DFB){#sec:dfb-atom}

During the Doppler filter bank, every window of samples, associated with each range
bin is transformed into a Doppler channel and the complex samples are converted to
real numbers by calculating their envelope. The DFB transformation is applied over a
window of $N_{FFT}$ samples, thus we need to re-arrange the data cubes again as
suggested in @fig:dfb-samp.

The `dfb` process applies the the following chain of functions on each window of
complex samples, in three consecutive steps:

 * scale the window samples with a set of coefficients to decrease the Doppler side
   lobes from each FFT output and thereby to increase the clutter rejection.

 * apply an $N_{FFT}$-point 2-radix decimation in frequency Fast Fourier Transform
   (FFT) algorithm.

 * compute the envelope of each complex sample when phase information is no longer of
   interest. The envelope is obtained by calculating the absolute value of the complex
   number, converting it into a real number.

| Function           | Original module                    | Package                 |
|--------------------|------------------------------------|-------------------------|
| `farm11`,` farm21` | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom            |
| `fft`              | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkWeightCoefs`    | `ForSyDe.AESA.Coefs`               | aesa-atom               |
| `nS`, `nFFT`       | `ForSyDe.AESA.Params`              | aesa-atom               |
 

![Doppler Filter Bank on video structure](figs/dfb-cube.pdf){#fig:dfb-cube}

![DFB process](figs/dfb-proc-atom.pdf){#fig:dfb-proc-atom}
 
> dfb :: Signal (Window (Beam  (Range  CpxData )))
>     -> Signal (Beam   (Range (Window RealData)))
> dfb = SY.comb11 (M.farm11 fDFB . C.transpose)
> 
> fDFB :: Window CpxData -> Window RealData
> fDFB = V.farm11 envelope . fft nFFT . weight
>   where
>     weight     = V.farm21 (*) mkWeightCoefs
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

 #### Constant False Alarm Ratio (CFAR){#sec:cfar-atom}

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


 #### Integration (INT){#sec:int-atom label="INT in ForSyDe-Atom"}

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




