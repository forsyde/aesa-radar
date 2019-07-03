 ## The High-Level Model{#sec:atom-network}


The code for this section is written in the following module, see [@sec:usage] on how
to use it:

> {-# LANGUAGE PackageImports #-} --can be ignored
> module AESA.StreamsAtom where

 ### Libraries and Aliases

We import exactly the same libraries as in @sec:cube-atom-operation, so we don't need
to explain what each one does. However, in this model we introduce a new MoC.  For
describing streaming behavior of the application our design will use a heterogeneous
approach, using a combination of _synchronous reactive (SY)_ processes
[@leeseshia-15;@Benveniste03], where the main assumption is that all events in the
system are synchronized; and _synchronous data flow (SDF)_ processes
[@leeseshia-15;@lee95], where the temporal behavior is formulated in terms of partial
order constraints between events. We also use the Boolean dataflow model, which is an
extension to SDF to allow for dynamic behaviors [@buck93]. We import the
[`SY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html),
[`SDF`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html)
and `BDF` libraries described in the
_[MoC](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html) layer_, see
[@ungureanu17], using an appropriate alias for each.

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.BDF as BDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector        as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP
> import AESA.Coefs
> import AESA.Params
> import Data.Complex

> import qualified AESA.CubesAtom as CAESA

We use the same internal aliases to name the different types employed in this model:

> type Antenna  = Vector -- length: nA
> type Beam     = Vector -- length: nB
> type Range    = Vector -- length: nb
> type Window   = Vector -- length: nFFT
> type CpxData  = Complex Float
> type RealData = Float

 ### Video Processing Stages

In this section we follow each stage earlier described in described in
@sec:cube-atom-operation, and exploit the initial assumption on the order of events
stating: _"For each antenna the data arrives _pulse by pulse_, and each pulse arrives
_range bin by range bin_. This happens _for all antennas in parallel_, and all complex
samples are synchronized with the same sampling rate, e.g. of the A/D converter."_

This allows us to "unroll" the indata video cubes into $N_A$ parallel synchronous
streams, each stream being able to be processed as soon as it contains enough
data. This unrolling is depicted in [@fig:cube-unrolling] as streaming the pulses as
soon as they arrive: range bin by range bin. We say that we partition the data _in
time_ rather than _in space_, which is a more appropriate partition judging by the
assumption above.

![Video cube unrolling](figs/cube-unrolling.pdf){#fig:cube-unrolling}

 #### Digital Beamforming (DBF){#sec:dbf-atom}

The role of the DBF stage is explained in @sec:cube-dbf-atom. Depicted from a
streaming point of view, DBF would like in @fig:dbf-samp.

![Digital Beam Forming on streams of complex
samples](figs/dbf-samp.pdf){#fig:dbf-samp}

As can be seen in @fig:dbf-samp, a beam can be formed _as soon as_ all antennas have
produced a complex sample. The parallel streams of data coming from each antenna
element are represented as a _vector of synchronous (SY) signals_, i.e. vector of
signals where each event is synchronous with each other. This allows us to depict the
dataflow interaction between the streams during digital beamforming as the process
network in @fig:dbf-net-atom, where an  $\oplus$ represents a combinational process
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html#v:comb22).

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
| `mkBeamConsts`               | `AESA.Coefs`                  | aesa-atom               |
| `dElements`, `waveLenth`, `nA`, `nB` | `AESA.Params`         | aesa-atom               |

The previous code listing, depicted in @fig:dbf-net-atom, is actually showing the
_exact same_ "internals" as the vector-matrix dot product presented in
@sec:cube-dbf-atom. However, the elementary operations, instead of regular arithmetic
operations $\times$ and $+$, are _processes_ applying these operations on SY
streams. As such, the `fanout` skeleton distributes one signal to a whole row
(i.e. vector) of processes, the matrix `farm` applies pair-wise a matrix of partially
applied processes on this matrix of signals, and `reduce` creates a reduction network
of binary processes pair-wise applying the function $+$ on all events in
signals. Practically the DBF network transforms $N_A$ synchronous signals originating
from each antenna element into $N_B$ synchronous signals for each beam. The internal
structure of this transformation exposes multiple degrees of potential distribution on
parallel synchronous resources.

 #### Pulse Compression (PC){#sec:pc-atom}

The role of the DBF stage is explained in @sec:cube-pc-atom. The sliding window, or
moving average (MAV), is now applied on the range bin samples in the order of their
arrival.

![Pulse Compression on streams of complex samples](figs/pc-samp.pdf){#fig:pc-samp}

In @fig:pc-samp we can see that, in order to apply the MAV algorithm on all the range
bins of every pulse, we need to accumulate $N_b$ samples and process them in
batches. Intuitively this can be done by processing each beam with a _synchronous
dataflow_ (SDF) actor which, with each firing, consumes $N_b$ samples and produces
$N_b$ samples. Note that at this stage of modeling we value intuition and the
capturing of the right application properties rather than target implementation
efficiency. We will tackle this problem later in the design stages (see @sec:synth)
where we will try to transform the model toward more efficient implementation models
(with respect to some constraint, e.g. throughput) which preserve these behavioral
properties.

> pc :: Beam ( SY.Signal CpxData)
>    -> Beam (SDF.Signal CpxData)
> pc = V.farm11 (procPC . SY.toSDF)

| Function    | Original module                    | Package                 |
|-------------|------------------------------------|-------------------------|
| `farm11`    | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom            |
| `toSDF`     | [`ForSyDe.Atom.MoC.SY`]            | forsyde-atom            |
| `comb11`    | [`ForSyDe.Atom.MoC.SDF`]           | forsyde-atom            |
| `fir`       | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkPcCoefs` | `AESA.Coefs`               | aesa-atom               |
| `nb`        | `AESA.Params`              | aesa-atom               |

![PC stage process](figs/pc-proc-atom.pdf){#fig:pc-proc-atom}

Following the reasoning above, we instantiate the PC video processing stage as a
`farm` of SDF processes `procPC` as depicted in [@fig:pc-proc-atom]. Notice that
before being able to apply the SDF actors we need to translate the SY signals yielded
by the DBF stage into SDF signals. This is done by the `toSDF` interface which is an
injective mapping from the (timed) domain of a SY MoC tag system, to the (untimed)
codomain of a SDF MoC tag system. For more on tag systems please consult [@lee98].

> procPC :: Fractional a => SDF.Signal a -> SDF.Signal a 
> procPC = SDF.comb11 (nb, nb, V.fromVector . fir (mkPcCoefs 5) . V.vector)

The `procPC` actor consumes and produces `nb` tokens each firing, forms a `Vector`
from these tokens, and applies the same `fir` function used in @sec:cube-pc-atom on
these vectors. Also notice that the type signature for `procPC` is left polymorphic as
to be more convenient later when we formulate properties over it.

 #### Corner Turn (CT) with 50% overlap {#sec:ct-atom}

The role of the CT stage is explained in @sec:cube-ct-atom. In this model we make use
of the knowledge that for each beam sample arrives in order, one range bin at a time,
in the direction of consumption suggested in @fig:ct-samp, and "fill back in" the
video cube in the direction of production. In order to maximize the efficiency of the
AESA processing the datapath is split into two concurrent processing channels with 50%
overlapped data, as shown in [@fig:cube-ct-cube] from @sec:cube-ct-atom.

![Building matrices of complex samples during CT](figs/ct-samp.pdf){#fig:ct-samp}

<!-- ![Concurrent processing on 50% overlapped data](figs/ct-cube.pdf){#fig:ct-cube}  -->

> ct :: Beam (SDF.Signal CpxData)
>    -> (Beam (SDF.Signal CpxData),
>        Beam (SDF.Signal CpxData))
> ct = V.farm12 procCT

Our CT network thus maps on each beam signal a corner turn process which, under the
SDF execution semantics, consumes $N_{FFT}\times N_b$ ordered samples, interprets them
as a matrix, transposes this matrix, and produces $N_b\times N_{FFT}$ samples ordered
in the direction suggested in @fig:ct-samp.

> procCT :: Num a => SDF.Signal a -> (SDF.Signal a, SDF.Signal a)
> procCT sig = (corner rightCh, corner leftCh)
>   where
>     leftCh      = sig
>     (_,rightCh) = BDF.switch selectSig sig
>     selectSig   = SDF.delay (replicate (nb * nFFT `div` 2) True)
>                   $ SDF.constant1 [False]
>     corner      = SDF.comb11 (nFFT * nb, nb * nFFT,
>                               fromMatrix . M.transpose . matrix nb nFFT)


| Function                 | Original module                       | Package                 |
|--------------------------|---------------------------------------|-------------------------|
| `farm12`                 | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| (`from`-)`matrix`, `transpose` | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `comb11`, `delay`        | [`ForSyDe.Atom.MoC.SDF`]              | forsyde-atom            |
| `nb`, `nFFT`             | `AESA.Params`                 | aesa-atom               |


Recall that, in order to achieve 50% overlapping between the two output channels, in
the initial high-level model in @sec:cube-ct-atom we "delayed" the left channel with
half a cube of "useless data" which we later ignored in the testbench. While it is
possible to do the same trick here, i.e. delay the left channel with $N_b \times
N_{FFT}/2$ samples per beam, which is the equivalent of half a cube, we agreed upon it
being a simulation artifice to avoid undefined behavior, but not really reflecting the
target implementation. The end artifact would start streaming the left channels only
after the first half cube. We can think of three ways how to model such a behavior
_without stepping outside the data flow paradigm_[^fn:outdataflow]:

1. "starting" the _entire_ system (i.e. considering time 0) once we _know_ the values
  for half a cube. This means that the behavior of the AESA system does not include
  the acquisition of the first half video cube, which would be described in its
  _history_ (i.e. initial state, e.g. passed as top-level argument).

1. _using absent semantics_, which would be the _correct_ way to define such an
  abstract behavior, but would render the description of the whole system more
  complicated, since every block would then need to be aware of the "absence" aspect
  of computation.

1. _using a dynamic dataflow MoC_, which is a bit more risky, because in general most
  of these MoCs are undecideable and might lead to deadlocks if not used properly, but
  are less abstract and much closer to what you would expect from a dynamic
  "start/stop" mechanism.

[^fn:outdataflow]: this type of behavior is quite naturally expressed in other
paradigms, such as communicating sequential processes (CSP), redezvous or Petri
Nets. Currently ForSyDe does not support such MoCs and they are out of the scope of
this report.

![CT network](figs/ct-net-atom.pdf){#fig:ct-net-atom}

Since we are already considering a refined behavior model of the AESA system, we have
chosen the third approach, as hinted in @fig:ct-net-atom. We use the Boolean dataflow
(BDF) MoC introduced by @buck93 to express the dynamic switching behavior. BDF
_extends_ SDF with two actors `switch` and `select` which are able to redirect the
flow of data to/from separate channels based on an input selection signal carrying
Boolean tokens. We use the BDF `switch` process which uses the _hard-coded_
`selectSig` signal to redirect the first $N_b \times N_{FFT}/2$ tokens for each beam
(the equivlent of half a cube of indata) to a "null" channel, and only after that to
start streaming into the right channel.

**OBS:** in general it is advised to _avoid_ BDF, which does not benefit from static
analysis methods for schedulability and deadlock detection, in favor of a more
analizable one, such as scenario aware dataflow (SADF) [@stuijk-2011]. However, in our
case we base our modeling choice based on the knowledge that, lacking any type of
feedback composition, the AESA signal processing system cannot cause any deadlock in
any of its possible states. Even so, hard-coding the selection signa can also be
considered a safe practice, because it is possible to derive a fixed set of SDF
scenarios based on a known reconfiguration stream.

 #### Doppler Filter Bank (DFB) and Constant False Alarm Ratio (CFAR) {#sec:dfb-cfar-atom}

For these two processing stages presented in [@sec:cube-dfb-atom;@sec:cube-cfar-atom],
we do not change the functional description at all, but rather the granularity of
their timed description. We continue to work over the premise that signals are
streaming samples of data, which have recently been arranged to arrive in _pulse
order_, as suggested by @fig:dfb-samp. As such, both stages are modeled as farms of
SDF processes operating over beam streams, and for each stream they consume the
necessary data, apply their (vector) function, and produce their respective data.

![Doppler Filter Bank on streams of complex samples](figs/dfb-samp.pdf){#fig:dfb-samp}

![DFB + CFAR networks](figs/dfb-cfar-net-atom.pdf){#fig:dfb-net-atom}


> dfb :: Beam (SDF.Signal CpxData)
>     -> Beam (SDF.Signal RealData)
> dfb = V.farm11 procDFB

> cfar :: Beam (SDF.Signal RealData)
>      -> Beam (SDF.Signal (Range (Window RealData)))
> cfar = V.farm11 procCFAR

> procDFB  = SDF.comb11 (nFFT, nFFT, fromVector . CAESA.fDFB . vector)
> procCFAR = SDF.comb11 (nb * nFFT, 1, (:[]) . CAESA.fCFAR . M.matrix nFFT nb)

| Function                   | Original module                       | Package                 |
|----------------------------|---------------------------------------|-------------------------|
| `farm11`,(`from`-)`vector` | [`ForSyDe.Atom.Skeleton.Vector`]      | forsyde-atom            |
| `matrix`                   | `ForSyDe.Atom.Skeleton.Vector.Matrix` | forsyde-atom-extensions |
| `nb`, `nFFT`               | `AESA.Params`                         | aesa-atom               |

Notice how we reuse the _exact same_ functions $f_{DFB}$ and $f_{CFAR}$ imported from
the `AESA.CubesAtom` previously defined in @sec:cube-atom-operation. This way we are
sure that we have not introduced coding errors. Also, notice that after the CFAR
process we do not unwrap the matrices any longer, but produce them as _individual
tokens_. This is because the subsequent INT block, as seen in the next paragraph,
operates much more efficiently on matrices, and thus we spare some intermediate
wrapping/unwrapping.

**OBS:** each function composing $f_{DFB}$ and $f_{CFAR}$ is itself inherently
parallel, as it is described in terms of parallel skeletons. We could have "lifted"
these skeletons as far as associating a process for each elementary arithmetic
operation, following the example set by the DBF network in @sec:dbf-atom. The two
representations (if carefully modeled) are semantically equivalent and, thanks to the
chosen formal description, can be transformed (ideally being aided by a computer/tool
in the future) from one to another. In fact there are multiple degrees of freedom to
partition the application on the time/space domains, thus a particular representation
should be chosen in order to be more appropriate to the target platform model. However
in this report we are not planning to refine these blocks even further, so for the
purpose of simulation and visualisation, this abstraction level is good enough for
now. The skeleton lifting to the process network level is left as an exercise for the
reader. The interested reader is also recommended to read the skeletons chapter in the
technical manual [@atom-manual] to see how the `fft` skeleton used in $f_{DFB}$ is
defined, and how it behaves during different instantiations, as network of processes
or as function.

 #### Integrator (INT){#sec:int-atom}

During the last stage of the video processing chain each data sample of the video cube
is integrated against its 8 previous values using an 8-tap FIR filter.

![Integration on cubes of complex samples](figs/int-cube.pdf){#fig:int-cube-atom}

The integration, re-drawn in @fig:int-cube-atom, like each stage until now, can be
modeled in different ways based on how the designer envisions the partitioning of the
data "in time" or "in space". This partitioning could be as coarse-grained as streams
of cubes of samples, or as fine-grained as networks of streams of indvidual
samples. For convenience and for simulation efficiency[^eff] we choose a middle
approach: video cubes are represented as farms (i.e. vectors) of streams of matrices,
as conveniently bundled by the previous DFB stages. We pass the responsibility of
re-partitioning and interpreting the data accordingly to the downstream process,
e.g. a control/monitoring system, or a testbench sink.

> int :: Beam (SDF.Signal (Range (Window RealData)))
>     -> Beam (SDF.Signal (Range (Window RealData)))
>     -> Beam (SY.Signal  (Range (Window RealData)))
> int = V.farm21 procINT

Please review @sec:cube-int-atom concerning the up-sampler `interleave` used as a SY
utility process. Here, since you have been already introduced to the SDF MoC, we can
"unmask" how an interleaving process actually operates underneath. It is in fact a SDF
actor which consumes one token from each input and interleaves them at the output.

![INT network](figs/int-net.pdf){#fig:int-net-atom}

> procINT :: Fractional a => SDF.Signal (Matrix a) -> SDF.Signal (Matrix a) -> SY.Signal (Matrix a)
> procINT cr = firNet mkIntCoefs . SDF.toSY1 . merge cr
>   where
>     merge   = SDF.comb21 ((1,1), 2, \[r] [l] -> [r, l])
 
As for the FIR network, we prefer working in the SY MoC domain, which describes more
naturally a streaming $n$-tap filter, hennce we use translate back using the `toSY`
MoC interface.

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
| `mkFirCoefs`      | AESA.Coefs                  | aesa-atom               |

[^eff]: we try to avoid unnecessary transposes (i.e. type traversals) which, at the moment, are not very efficiently implemented.

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

At least for the sake of code elegance if not for more, we refine the system `aesa'`
in @fig:aesa-net-atom to avoid unnecessary merging-splitting of vectors between
stages, by _fusing_ the `farm` skeletons between the PC and INT stages, like in
@fig:aesa-net-atom2. While for simulation this transformation does not make much of a
difference (thanks to Haskell's lazy evaluation mechanisms), for future synthesis
stages it might be a valuable insight, e.g. it increases the potential of parallel
distribution. As both models are semantically equivalent an automated or tool-assisted
transformation process should be trivial.

> aesa :: Antenna (SY.Signal CpxData) -> Beam (SY.Signal (Range (Window RealData)))
> aesa = V.farm11 pcToInt . dbf

> pcToInt beam = let (rb,lb) = procCT $ procPC $ SY.toSDF beam
>                    lCFAR   = procCFAR $ procDFB lb
>                    rCFAR   = procCFAR $ procDFB rb
>                in  procINT rCFAR lCFAR

![AESA network when fusing the related `farm`s](figs/aesa-net-atom2.pdf){#fig:aesa-net-atom2}
