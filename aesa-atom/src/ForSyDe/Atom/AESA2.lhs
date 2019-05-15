 ## A Process Network-Oriented Approach to Modeling{#sec:atom-network}

The second approach to modeling the radar signal processing
application carries on the idea incepted earlier in
[@sec:int-shallow], namely to use skeletons to express parallelism at
the process level rather than at datum level. This way processes are
expressed as operating on elementary streams of data, i.e. originating
from each antenna element in particular, and skeletons rather describe
patterns of interaction and synchronization between these
processes. We thus trade the monolithic view of processes computing
"cubes" of data for a much finer-grained view which allows to quantify
the potential for parallelism by exploiting 1) the arithmetic/data
dependencies; 2) the precedence relations.

The ForSyDe-Atom definition file for the second approach to modeling
the Saab-AESA application is found at
`<root>/src/ForSyDe/Atom/AESA2.lhs` and it can be imported as a
generic library (e.g. in the interpreter session). Below you find the
module definition and exported functions.

> {-# LANGUAGE PackageImports #-}  -- you can ignore this line
> module ForSyDe.Atom.AESA2 where

By now the imported libraries are not a mystery any longer. Check
[@sec:atom-operation] for more details on what each is imported for.

> import Data.Complex
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import ForSyDe.Atom.MoC.SY  as SY

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP

As in the previous section, we import these local submodule containing
the type synonym declarations presented in [@sec:aliases-shallow] and
the coefficient windows like in [@sec:coefs-shallow].

> import ForSyDe.Atom.AESA.Types
> import ForSyDe.Atom.AESA.Coefs

 ### Video Processing Pipeline Stages

By now the functional specification of each stage in the video
processing pipe should be quite clear clear. The modeling approaches
used until now tried to _translate_ this functional specification into
an _executable_ ForSyDe specification. We now stretch the intuition
gained with these models and exploit the initial assumption stated as
early as [@sec:video-chain-spec]: _"For each antenna the data arrives
_pulse by pulse_, and each pulse arrives _range bin by range
bin_. This happens _for all antennas in parallel_, and all complex
samples are synchronized with the same sampling rate, e.g. of the A/D
converter."_

This allows us to "unroll" the video cubes into parallel (synchronous)
streams, each stream being able to be processed as soon as it contains
enough data. This unrolling can be simply depicted in
[@fig:cube-unrolling] as chaining the pulses as they arrive: range bin
by range bin. We say that we partition the data _in time_ rather than
_in space_, which is a more appropriate partition judging by the
specification's assumptions.

![Video cube unrolling](figs/cube-unrolling.pdf){#fig:cube-unrolling}

 #### Digital Beamforming (DBF){#sec:dbf-atom-net}

Recall that the DBF $N_B$ simultaneous receiver beams from the complex
data received from $N_A$ antenna elements, as explained in
[@sec:dbf-shallow]. Depicted from a streaming point of view, DBF would
look more like in @fig:dbf-samp.

![Digital Beam Forming on streams of complex samples](figs/dbf-samp.pdf){#fig:dbf-samp}

As can be seen in @fig:dbf-samp, a beam can be formed _as soon as_ all
antennas have produced a complex sample each. We thus represent the
parallel streams of data coming from each antenna element as a _vector
of synchronous signals_, i.e. vector of signals where each event is
synchronous with each other. This allows us to depict the dataflow
interaction between the streams during digital beamforming as the
process network in @fig:dbf-net-atom, where (for the sake of space) we
represent a combinational process
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html#v:comb22)
as a node with the $\oplus$ symbol.

![DBF network](figs/dbf-net-atom.pdf){#fig:dbf-net-atom}

> dbf :: Antenna (SY.Signal CpxData)
>     -> Beam    (SY.Signal CpxData)
> dbf antennaSigs = beamSigs
>   where
>     beamSigs   = V.farm11 (V.reduce (SY.comb21 (+))) beamMatrix
>     beamMatrix = M.farm21 (\c -> SY.comb11 (*c)) beamConsts sigMatrix
>     sigMatrix  = V.farm11 V.fanout antennaSigs
>     beamConsts = mkBeamConsts (V.length antennaSigs) nB

In fact the code listing above, depicted in @fig:dbf-net-atom, is an
equivalent semantic "lifting" of the vector skeletons in
[@sec:dbf-shallow;@sec:dbf-atom] into the _signal_ domain. In other
words instead of representing signals as carrying (multi-dimensional)
vectors of values in their events, signals themselves are organized
into (multi-dimensional) vectors and are carrying values in their
events. Consequently, the "elementary" operations passed to skeletons
are thus _processes_ rather than simple functions on values. The
visual depiction in @fig:dbf-net-atom suggests the interaction of
processes and signals, for example through the
[`fanout`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:fanout)
skeleton which distributes one signal to a whole row (i.e. vector) of
processes, the matrix `farm`[^farmM] applies a matrix of partially
applied processes on a matrix of signals, and
[`reduce`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:reduce),
which creates a reduction network of binary processes pair-wise
applying the function $+$ on all events in signals. Practically the
DBF network transforms $N_A$ synchronous signals originating from each
antenna element into $N_B$ synchronous signals for each beam. The
internal structure of the transformation though exposes multiple
degrees of potential for parallel exploitation.

[^farmM]: see `ForSyDe.Atom.Skeleton.Vector.Matrix.farm22` from
            the `forsyde-atom-extensions` documentation.

 #### Pulse Compression (PC){#sec:pc-atom-net}

Recall from @sec:pc-shallow that the pulse compression stage decodes
the modulation of the pulse by applying a matched filer on the range
bins. This essentially applies a sliding window (i.e. a FIR filter) on
the bin samples and we have already seen in @sec:int-shallow that this
operation expands nicely into a pipelined network structure provided
the samples are streamed through a signal.
 
![Pulse Compression on streams of complex samples](figs/pc-samp.pdf){#fig:pc-samp}

In fact, as shown in @fig:pc-samp samples _are_ actually arriving in
synchronous streams from the DBF stage, as an effect of the initial
assumption. This in turn creates a perfect match to exploit the
application's inherent potential for parallelism by feeding each
arriving beam into a pipelined $N$-tap FIR filter. The resulting
process network is depicted in @fig:pc-net-atom where the
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
and
[`reduce`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:reduce)
skeletons are captured by appropriate graphical primitives, suggesting
the structured replication of a certain composite process.

![PC network](figs/pc-net-atom.pdf){#fig:pc-net-atom}

> pc :: Beam (SY.Signal CpxData)
>    -> Beam (SY.Signal CpxData)
> pc = V.farm11 (fir' addProc mulProc delayCount mkPcCoefs)
>   where
>     addProc    = SY.comb21 (+)
>     mulProc c  = SY.comb11 (*c)
>     delayCount = fst . SY.stated12 countBin (0, nb)
>     countBin _ 0 _ = (0, nb)
>     countBin _ c s = (s, c-1)

In @fig:pc-net-atom we instantiate an $N$-tap FIR filter for each beam
signal by using the `fir'`[^firA] skeleton which takes as arguments a)
a process used as kernel operation for reduction; b) a process for
scaling each tap with the according coefficient; c) a process for
delaying each tap and d) a vector with $N$ coefficients; and
replicates and connects each process accordingly to form the
well-known FIR-filter pattern.

The FIR skeleton, as other skeletons, is not only parametric, but also
orthogonal from "what" it is performing. Its role is simply to
describe a "pattern" of structured composition, abstracting away the
operations which are actually performed. The operations passed as
arguments fix the semantics of the process network: if they are
functions on values, then the network becomes a parallel algorithm; if
they are processes on signals, then it becomes a network of concurrent
processes. Another advantage with this orthogonal scheme is that we
can elegantly customize our design, as seen in @fig:pc-net-atom, where
instead of a simple
[`delay`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html#v:delay)
process for delaying the FIR taps, we pass a
[`stated`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html#v:stated22)
process structured like in @fig:pc-counter-atom which both delays the
taps, but also flushes the FIR pipeline after $N_b$ samples. This
needs to be done in order to preserve the behavior of the application
as specified in @sec:video-chain-spec: each pulse of range bins needs
to be computed independently from each other.

![`countDelay` process structure](figs/pc-counter-atom.pdf){#fig:pc-counter-atom}

 #### Corner Turn (CT)

During the corner turning the sampled data is re-aligned to form rows
of $N_{FFT}$ samples for each range bin. _How_ to do this was already
presented in [@sec:ct-shallow;@sec:ct-atom], namely to consume as many
pulses (matrices) as necessary to form full video cubes, transpose
these cubes and then pass them as single tokens for the upcoming
processing stages. However, in this approach we use the awareness that
for each beam samples arrive in order, one range bin at a time, and
fill the video cube in the direction suggested in the left side of
@fig:ct-samp.

![Building matrices of complex samples during CT](figs/ct-samp.pdf){#fig:ct-samp}

Our CT network thus maps on each beam signal a corner turn process
which, under the SDF execution semantics, consumes $N_{FFT}\times N_b$
(ordered) samples, interprets it as a matrix, transposes it, and
$N_b\times N_{FFT}$ samples ordered in the direction suggested in the
right side of @fig:ct-samp. Like before, in order to achieve 50%
overlapping between the two output channels, the channel one needs to
be "delayed" with a prefix signal equivalent to half a video cube. In
this case that prefix is formed of $\frac{N_b \times N_{FFT}}{2}$
zeroes on each beam path.

![CT network](figs/ct-net-atom.pdf){#fig:ct-net-atom}

> ct :: Beam (SY.Signal CpxData)
>    -> (Beam (SDF.Signal CpxData),
>        Beam (SDF.Signal CpxData))
> ct sigs = (V.farm11 rightCorner sigs, V.farm11 leftCorner sigs)
>   where
>     rightCorner = cornerTurn . SY.toSDF
>     leftCorner  = SDF.delay initBatch . rightCorner
>     initBatch   = replicate (nb * nFFT `div` 2) (cis 0)
>     cornerTurn  = SDF.comb11 (nFFT * nb, nb * nFFT,
>                               fromMatrix . M.transpose . matrix nb nFFT)

 #### Doppler Filter Bank (DFB){#sec:dfb-atom-net}

During the Doppler filter bank, every window of samples, associated
with each range bin goes through a transformation consisting in
weighting, FFT, and envelope calculation, as already presented in
@sec:dfb-shallow. Since the samples have been arranged in pulse
window-order during the previous stage, the DFB transformation can
occur as soon as a window of $N_{FFT}$ samples arrive, like in
@fig:dfb-samp.

![Doppler Filter Bank on streams of complex samples](figs/dfb-samp.pdf){#fig:dfb-samp}

In contrast with the pulse compression (PC) stage in @sec:pc-atom-net,
the DFB is not carried out in a "sliding window" fashion, i.e. it
cannot be processed "one sample at a time", but rather it needs the
whole $N_{FFT}$ batch in order to apply the FFT algorithm. As such,
the best-fitted MoC execution semantics to describe the process(es)
associated with DFB is still SDF. We can simply describe the DFB stage
as a
[`farm`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html#v:farm22)
of
[`comb`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SDF.html#v:comb22)
SDF actors which apply the $f_{DFB}$ function on $N_{FFT}$ tokens
every firing cycle, as seen in @fig:dfb-net-atom.

![DFB network](figs/dfb-net-atom.pdf){#fig:dfb-net-atom}

> dfb :: Beam (SDF.Signal CpxData)
>     -> Beam (SDF.Signal RealData)
> dfb = V.farm11
>       (SDF.comb11 (nFFT, nFFT,
>                    fromVector . fDFB . vector))
>   where
>     fDFB       = V.farm11 envelope . fft nFFT . weight 
>     weight     = V.farm21 (*) mkWeightCoefs
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

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
