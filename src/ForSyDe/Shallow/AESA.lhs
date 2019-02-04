> {-# LANGUAGE PackageImports #-}   -- you can ignore this line for now

> module ForSyDe.Shallow.AESA where 

 ## Imported Libraries

As we the AESA application uses complex numbers, we use Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html)
type.

> import Data.Complex

Next we import the
[`ForSyDe.Shallow`](http://hackage.haskell.org/package/forsyde-shallow)
modeling library.

> import ForSyDe.Shallow hiding (fft) -- we use our extension library
>                                     -- for 'fft' at the moment.

For describing parallel operations on data we use _algorithmic
skeletons_ [@skillicorn05], which are homomorphisms on regular
algebraic structures (e,g, vectors, matrices, cubes) describing common
patterns of communication and computation and exposing an inherent
potential for parallel exploitation. Skeletons (as homomorphisms) have
been formulated within the
[`ForSyDe-Atom`](https://forsyde.github.io/forsyde-atom/) framework,
but have been also included within ForSyDe-Shallow recently. For the
scope of this project we use the
[`Vector`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html)
data type, which is a shallow (lazy-evaluated) implementation of
unbounded arrays, ideal for early design validation. Although
type-checked, bounded, and even boxed (i.e. memory-mapped)
alternatives exist, such as
[`FSVec`](http://hackage.haskell.org/package/parameterized-data/docs/Data-Param-FSVec.html)
or REPA [`Array`](http://hackage.haskell.org/package/repa)s, for the
scope of this project the functional validation and (by-hand)
requirement analysis on the properties of skeletons will suffice. We
also import the `Matrix` and `Cube` utility libraries which contain
type synonyms for nested `Vector`s along with their derived skeletons,
as well a `DSP` which contain commonly used DSP blocks defined in
terms of vector skeletons.

**NOTE:** at the time being the modules below are imported from our
  custom package `forsyde-shallow-extensions`. The next release of
  `forsyde-shallow` will most likely contain these extensions, and
  they will be imported normally. Until then, the API documentation
  needs to be generated locally using the `haddock` commands, as
  described in the `README.md` file.

> -- | explicit inport from extensions package. Will be imported
> -- normally once the extensions are merged into forsyde-shallow
> import "forsyde-shallow-extensions" ForSyDe.Shallow.Core.Vector
> import "forsyde-shallow-extensions" ForSyDe.Shallow.Utility.Matrix
> import ForSyDe.Shallow.Utility.Cube
> import ForSyDe.Shallow.Utility.DSP

 ## Type Aliases and Constants{#sec:aliases-shallow label="Type Aliases and Constants"}

For ease of documentation we will be using type synonyms (aliases) for
all types and structures throughout this design:

* `Antenna` denotes a vector container for the antenna elements. Its
  length is equal to the number of antennas in the radar $N_A$.

* After Digital Beamforming (DBF), the antenna elements are
  transformed into $N_B$ beams, thus we associate the `Beam` alias for
  the vector container wrapping those beams.

* `Range` is a vector container for range bins. All antennas have the
  same number of range bins $N_b$, rendering each $\text{Antenna}
  \times \text{Range}$ a pefect matrix of samples for every pulse.

* For ease of problem dimensioning, we use another vector alias
  `CRange` for the _center range bins_ calculated after the Constant
  False Alarm Ratio (CFAR) has been applied. Its length is $N_b' =
  N_b-2N_{FFT}-2$.

* `Window` stands for a doppler window of $N_{FFT}$ pulses.

> type Antenna     = Vector -- length: nA
> type Beam        = Vector -- length: nB
> type Range       = Vector -- length: nb
> type CRange      = Vector -- length: nb'
> type Window      = Vector -- length: nFFT

Here we define the size constants. The only parameters needed by this
system and used for the correct partitioning of data are `nB` and
`nFFT`. The rest can be inferred from the size of input data and the
vector operations.

> nB   = 8   :: Int
> nFFT = 256 :: Int
> -- nA = 16 :: Int
> -- nb = 1024 :: Int
> -- nb' = nb - 2 * nFFT - 2

Finally we provide two aliases for the basic Haskell data types used
in the system, to stay consistent with the application specification.

> type CpxData     = Complex Float
> type RealData    = Float

 ## Video Processing Pipeline Stages

In this section we follow each stage described in
[@sec:video-chain-spec], and model it as a process operating on a
matrix or on a cube of antenna samples.

 ### Digital Beamforming (DBF){#sec:dbf-shallow label="DBF in ForSyDe-Shallow"}

 The DBF receives complex indata, from $N_A$ antenna elements and
 forms $N_B$ simultaneous receiver beams, or "listening directions",
 by summing individually phase-shifted indata signals from all
 elements. Basically, considering the input video "cube", the the
 transformation applied by DBF, could be depicted as in
 [@fig:dbf-cube], where the _pulse_ dimension goes to infinity
 (i.e. data is received pulse by pulse). 

![DBF on video structure](figs/dbf-cube.pdf){#fig:dbf-cube}

As can be seen in the picture, each _pulse_ carries a matrix of
samples, which can be processed as soon it is available. From
[@sec:video-chain-spec] we also _know_ that all complex samples
arriving from the antennas are synchronized with the A/D converter
rate, thus we can consider the pulse matrices to be synchronized as
well. As such, we can say that the system's (timed) behavior follows
the perfect synchrony assumption, and therefore can be modeled under
the the synchronous reactive model of computation (MoC)
(@halbwachs91,@lee98). The DBF stage can be described as a
combinational SY process
[`combSY`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-MoC-Synchronous.html#v:combSY)
acting upon signals of
[`Matrix`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Utility-Matrix.html)es.

![DBF stage process](figs/dbf-proc-shallow.pdf){#fig:dbf-proc-shallow}

The job of the `dbf` process is to
[map](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:mapV)
the beamforming function $f_{DBF}$ on each column of each _pulse
matrix_ (see [@fig:dbf-cube]). In order for this to work, we need to
transpose[^transp] the input pulse matrix so that samples belonging to
the same range bin are arranged on rows. After applying $f_{DBF}$ on
each row we transpose the matrix back for the following stages.

[^transp]: see `transposeMat` from the `forsyde-shallow-extensions` documentation

> dbf :: Signal (Antenna (Range CpxData))
>     -> Signal (Beam    (Range CpxData))
> dbf = combSY (transposeMat . mapV fDBF . transposeMat)

The beamforming function is specified like in [@eq:dbf], where $e_k ,\
\forall k \in [1,N_A]$ denotes the samples from antenna elements, and
respectively $b_i,\ \forall i \in [1,N_B]$ are samples for each
beam. It can be seen immediately that it is a form of matrix-vector
multiplication and reduction, thus we implement [@eq:dbf] at the
highest level of abstraction simply as matrix/vector operations using the
[`mapV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:mapV),
[`zipWithV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:zipWithV),
[`reduceV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:reduceV),
[`mapMat`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Utility-Matrix.html#v:mapMat),
[`zipWithMat`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Utility-Matrix.html#v:zipWithMat)
accordingly, like in [@eq:dbf-mat].

$$ b_i(n) = \sum_{k=1}^{N_A} e_k(n) \cdot \alpha_{ki}\qquad\forall i \in [1,N_B] $$ {#eq:dbf}

> fDBF :: Antenna CpxData -- ^ input antenna elements
>      -> Beam    CpxData -- ^ output beams
> fDBF antennaEl = beam
>   where
>     beam       = reduceV (zipWithV (+)) beamMatrix
>     beamMatrix = zipWithMat (*) elMatrix beamConsts
>     elMatrix   = mapV infiniteV antennaEl
>     beamConsts = mkBeamConsts (lengthV antennaEl) nB

The matrix `beamConsts` is generated using the generator `mkBeamConsts` defined in [@sec:coefs-shallow].

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

 ### Pulse Compression (PC){#sec:pc-shallow label="PC in ForSyDe-Shallow"}

In this stage the received echo of the modulated pulse, i.e. the the
information contained by the range bins, is passed through a matched
filter for decoding their modulation. Similarly to the previous stage,
considering the input video, the the transformation applied by PC,
could be depicted as in [@fig:pc-cube], also considering _pulse_ to be
an infinite (streaming) dimension.

![PC on video structure](figs/pc-cube.pdf){#fig:pc-cube}

The process applying PC is similar to the previous one in the sense
that it operates on synchronous pulses of matrices, and is shown in
[@fig:pc-proc-shallow]. The operation performed upon pulse matrices is mapping
the $f_{PC}$ on each row, as suggested by [@fig:pc-cube].

![PC stage process](figs/pc-proc-shallow.pdf){#fig:pc-proc-shallow}

> pc :: Signal (Beam (Range CpxData)) 
>    -> Signal (Beam (Range CpxData))
> pc = combSY (mapV fPC)

Here the function $f_{PC}$ simply applies the `fir`[^fir] skeleton
exported by the `DSP` library of `ForSyDe.Shallow`. `fir` is a
parallel skeleton itself built as a parallel prefix/recursion pattern
over vectors, and its structure will be further shown in
[@sec:int-shallow]. For now we take it "for granted" as a function
over vectors. Due to internal mechanisms, `fir` is applied from the
vector's tail to its head; however since the _sliding window_ function
is symmetric and associative this particular aspect does not matter
for this application.

In this case, for simplicity we generate statically (i.e. hard-code)
the FIR coefficients with the function `mkPcCoefs` defined in
[@sec:coefs-shallow]. Making them dependent on the pulse code length
would require modeling an additional external depenency which we left
out of the scope of this use case.

[^fir]: see `fir` from the `forsyde-shallow-extensions` documentation.

> fPC :: Range CpxData -- ^ input range bin     
>     -> Range CpxData -- ^ output pulse-compressed bin
> fPC = fir mkPcCoefs

 ### Corner Turn (CT){#sec:ct-shallow label="CT in ForSyDe-Shallow"}

The role of the corner turn in the video processing chain is simply to
split the input signal as specified in [@sec:video-chain-spec] into
the right and the left channel, and to "delay" the left channel with
$N_{FFT}/2$ samples in order to achieve 50% overlap in the downstream
processes.

![Corner turn splitter](figs/ct-proc-shallow.pdf){#fig:ct-proc-shallow}

The process is depicted [@fig:ct-proc-shallow]. The stream of pulses
is now seen from an _untimed_ SDF perspective -- we will see shortly
why. The _left_ signal of pulses is filled with $N_{FFT}$ initial
tokens, made up of matrices filled up with complex $0$s, using the
[`delaySDF`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-MoC-SDF.html#v:delaySDF)
process constructor. This way, whatever input arrives from the PC
stage, will be observed at the left channel only after $N_{FFT}/2$
samples.

> ct :: Signal (Beam (Range CpxData))
>   -> (Signal (Beam (Range CpxData)),
>       Signal (Beam (Range CpxData)))
> ct pcSig = (rightWSig, leftWSig)
>   where
>     rightWSig = pcSig
>     leftWSig  = delaySDF initBatch pcSig
>     initBatch = replicate (nFFT `div` 2) (infiniteMat (cis 0))

*Interesting fact:* although the application specification mentions
 that the first $N_{FFT}$ batch of pulses is ignored, we cannot do
 that at this stage. In fact ForSyDe does not allow "cleaning up"
 (i.e. dropping, ignoring) events from signals, due to non-determinism
 introduced in case of possible feedback loops. Only an observer
 (i.e. testbench, sink) is allowed to do that, outside the process
 network. Furthermore, due to Haskell's lazy evaluation, ignoring some
 output values means that those values are not even computed to start
 with.

 ### Doppler Filter Bank (DFB){#sec:dfb-shallow label="DFB in ForSyDe-Shallow"}

During the DFB stage The pulse bins in the data set are transformed
into Doppler channels and the complex samples ar converted to real
numbers by calculating their envelope. Considering the data samples
being structured like in the previous stages, the transformation
applied by the DBF can be depicted as in [@fig:dfb-cube]. 

![DFB on video structure](figs/dfb-cube.pdf){#fig:dfb-cube}

This figure shows an important particularity of this stage as compared
to others: the _pulse_ axis is partitioned in finite _windows_ of
$N_{FFT}$ pulse matrices. This implies that the DBF function $f_{DBF}$
is applied to a window of $N_{FFT}$ _consecutive_ pulse samples _at
once_. As such we alter the notion of _total_ (consecutive) order
inferred by the synchronous MoC into a notion of _partial_ order among
samples, with respect to (i.e. relative to) the application of the FFT
algorithm on a window of pulses. The synchronous dataflow (SDF) MoC
(@lee95,@lee98) is a more appropriate MoC to describe such a
behavior. As such we define the DFB signal processing stage as an SDF
actor
[`actor11SDF`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-MoC-SDF.html#v:actor11SDF)
operating on $N_{FFT}$ pulse matrices, and generating one video cube
every time the process fires. In other words we say that the DBF actor
has a consumption rate of $N_{FFT}$ (matrix) tokens and a production
rate of 1 (cube) token, like in [@fig:dfb-proc-shallow].

![DFB stage process](figs/dfb-proc-shallow.pdf){#fig:dfb-proc-shallow}

The way in which the `dfb` process operates on the window of $N_{FFT}$
pulses is to transform the list of input tokens into a `Vector` using the
[`vector`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html)
utility, thus forming a cube of `Window (Beam (Range CpxData))`. We
need to transpose this cube so that we can operate on individual
windows of samples, thus the `transposeCube`[^transpC] skeleton
re-shuffles the elements into the structure `Beam (Range (Window
CpxData))`. After that we apply the function $f_{DFB}$ on each
$\mathtt{Window}$ contained by the newly-formed
$\mathtt{Beam}\times\mathtt{Range}$ matrix. Finally we wrap the
resulting video cube into a singleton token list (i.e. one output
token) by using the `:[]` list constructors.

[^transpC]: see `transposeCube` from the `forsyde-shallow-extensions` documentation.

> dfb :: Signal (Beam (Range CpxData))
>     -> Signal (Beam (Range (Window RealData)))
> dfb = actor11SDF nFFT 1 ((:[]) . mapMat fDFB . transposeCube . vector)

The function $f_{DFB}$ is applied on each _window_ of complex data
samples and consists in three consecutive steps:

 * scale the window samples with a set of coefficients to decrease the
   Doppler side lobes from each FFT output and thereby to increase the
   clutter rejection.

 * apply an $N_{FFT}$-point 2-radix decimation in frequency Fast
   Fourier Transfor (FFT) algorithm.

 * compute the envelope of each complex sample when phase information
   is no longer of interest. The envelope is obtained by calculating
   the absolute value of the complex number, converting it into a real
   number.

> fDFB :: Window CpxData -> Window RealData
> fDFB = mapV envelope . fft nFFT . weight
>   where
>     weight     = zipWithV (*) mkWeightCoefs
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)
>     

 ### Constant False Alarm Ratio (CFAR){#sec:cfar-shallow label="CFAR in ForSyDe-Shallow"}

The CFAR normalises the data within the video cubes in order to
maintain a constant false alarm rate with respect to a detection
threshold. This is done in order to keep the number of false targets
at an acceptable level by adapting the normalization to the clutter
situation in the area (around a cell under test) of interest. The
described process can be depicted as in [@fig:cfar-cube] which
suggests the data accessing pattern within the video _cubes_.

![CFAR on video structure](figs/cfar-cube.pdf){#fig:cfar-cube}

To model the streaming behavior of the CFAR stage we stay in the SDF
domain. The `cfar` process operates on one _cube_ at a time, exactly
as they are handed from the previous DFB stage, and for each input
cube yields another processed cube, as seen in
[@fig:cfar-proc-shallow].

![CFAR stage process](figs/cfar-proc-shallow.pdf){#fig:cfar-proc-shallow}

The process applies $f_{CFAR}$ on each
$\mathtt{Range}\times\mathtt{Doppler\ Window}$ matrix of real samples
originating from a beam.

> cfar :: Signal (Beam ( Range (Window RealData)))
>      -> Signal (Beam (CRange (Window RealData)))
> cfar = actor11SDF 1 1 ((:[]) . mapV fCFAR . head)

The $f_{CFAR}$ function normalizes each doppler window, after which
the sensitivity will be adapted to the clutter situation in current
area, as seen in [fig:cfar-signal]. The blue line indicates the mean
value of maximum of the left and right reference bins, which means
that for each doppler sample, a swipe of neighbouring bins is
necessary, as suggested by [@fig:cfar-cube]. This is a typical pattern
in signal processing called
[stencil](https://en.wikipedia.org/wiki/Stencil_code), which will
constitute the main parallel skeleton within the $f_{CFAR}$ function.

![The signal level within one pulse window: a) before CFAR; b) after CFAR](figs/cfar-signal.pdf){#fig:cfar-signal}

The $f_{CFAR}$ function itself can be described with the system of [@eq:cfar], where

 * $MD$ is the the minimum value over all Doppler channels in a batch
   for a specific data channel and range bin.

 * $EMV$ and $LMV$ calculate the early and respectively late mean
   values from the neighboring range bins as a combination of
   geometric and arithmetic mean values.

* $eb$ and $lb$ are the ealiest bin, respectively latest bin for which
  the CFAR can be calculated as $EMV$ and $LMV$ require at least
  $N_{FFT}$ bins + 1 guard bin before and respectively after the
  current bin. This phenomenon is also called the "stencil halo",
  which means that after the CFAR is applied, only
  $N_b'=N_b-2N_{FFT}-2$ bins will be yielded.

* 5 is added to the exponent of the CFAR equation to set the gain to
  32 (i.e. with only noise in the incoming video the output values
  will be 32).

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

In order to maximize the (potential) parallelism of our described
model, we systematically apply the skeletons
[`stencilV`](http://hackage.haskell.org/package/forsyde-shallow-3.4.0.0/docs/ForSyDe-Shallow-Core-Vector.html#v:stencilV)
[`mapV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:mapV),
[`zipWithV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:zipWithV),
[`zipWith3V`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:zipWith3V),
[`reduceV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:reduceV),
[`takeV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:takeV),
[`dropV`](http://hackage.haskell.org/package/forsyde-shallow/docs/ForSyDe-Shallow-Core-Vector.html#v:dropV)
as follows.

> fCFAR :: Range (Window RealData) -> CRange (Window RealData)
> fCFAR r_of_d = zipWith4V (\m -> zipWith3V (normCfa m)) md bin lmv emv
>   where
>     bin = dropV (nFFT + 1) r_of_d
>     md  = mapV (logBase 2 . reduceV min) bin
>     emv = mapV (arithMean . takeV nFFT) neighbors
>     lmv = mapV (arithMean . dropV (nFFT + 3)) neighbors
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     arithMean :: Vector (Vector RealData) -> Vector RealData
>     arithMean = (/nFFT) . reduceV addV . mapV geomMean . groupV 4
>     geomMean  = mapV (logBase 2 . (/4)) . reduceV addV
>     neighbors = stencilV (2 * nFFT + 3) r_of_d
>     addV      = zipWithV (+)

The first thing we calculate is the $MD$ for each doppler window (row)
belonging to the _center bins_, thus dropping the unnecessary earliest
$N_{FFT}+1$ bin rows from the input matrix `r_of_d` (i.e. _ranges_ of
_doppler windows_). for each one of these rows we look for the minimum
(`reduceV min`) and apply the binary logarithm on it.

Another action performed over the matrix `r_of_d` is to form a stencil
"cube" by gathering for each doppler window (row) $N_s=2N_{FFT}+3$
closest neighbors (including it), as sugessted by
[@eq:cfar-stencil]. 

$$
  \stackrel{\mbox{r\_of\_d}}{
  \begin{bmatrix}
  a_{11} & a_{12} & \cdots & a_{1N_{FFT}} \\
  a_{21} & a_{22} & \cdots & a_{2N_{FFT}} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{N_b1} & a_{N_b2} & \cdots & a_{N_bN_{FFT}}
  \end{bmatrix}}
  \stackrel{\mathtt{stencilV}}{\rightarrow}
  \stackrel{\mbox{neighbors}}{
  \begin{bmatrix}
  \begin{bmatrix}
  a_{11} & a_{12} & \cdots & a_{1N_{FFT}} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{N_s1} & a_{N_s2} & \cdots & a_{N_sN_{FFT}} \\
  \end{bmatrix}\\
  \begin{bmatrix}
  a_{21} & a_{22} & \cdots & a_{2N_{FFT}} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{(N_s+1)1} & a_{(N_s+1)2} & \cdots & a_{(N_s+1)N_{FFT}} \\
  \end{bmatrix}\\
  \vdots \\
  \begin{bmatrix}
  a_{(N_b-N_s)1} & a_{(N_b-N_s)2} & \cdots & a_{(N_b-N_s)N_{FFT}}\\
  \vdots & \vdots & \ddots & \vdots \\
  a_{N_b1} & a_{N_b2} & \cdots & a_{N_bN_{FFT}}
  \end{bmatrix}
  \end{bmatrix}}
$${#eq:cfar-stencil}

Each one of these neighbors matrices will constitute the input data
for calculating the $EMV$ and $LMV$ for each doppler window. $EMV$ is
calculated by taking the first $N_{FFT}$ bin rows from each neighbors
matrix and applying the mean function `arithMean` over
them. Similarly, $LMV$ drops the first $N_{FFT}+3$ bin rows (thus
taking the last $N_{FFT}$ rows) and performs the mean function. The
processing of the mean function for EMV over one neighbors matrix is
depicted in [@eq:cfar-emv].


$$\begin{aligned}
  &\begin{bmatrix}
  a_{11} & \cdots & a_{1N_{FFT}} \\
  \vdots &  \ddots & \vdots \\
  a_{N_s1} & \cdots & a_{N_sN_{FFT}}
  \end{bmatrix}
  \stackrel{\mathtt{takeV}}{\rightarrow}
  \begin{bmatrix}
  a_{11} & \cdots & a_{1N_{FFT}} \\
  \vdots  & \ddots & \vdots \\
  a_{N_{FFT}1}  & \cdots & a_{N_{FFT}N_{FFT}}
  \end{bmatrix}\\
  &\stackrel{\mathtt{groupV}}{\rightarrow}
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
  &\stackrel{\mathtt{mapV(geomMean)}}{\rightarrow}
  \begin{bmatrix}
  \log_2\frac{1}{4}\sum_{i=1}^{4}a_{i1} & \cdots & \log_2\frac{1}{4}\sum_{i=1}^{4}a_{iN_{FFT}} \\
  \vdots & \ddots & \vdots \\
  \log_2\frac{1}{4}\sum_{i=N_{FFT}-4}^{N_{FFT}}a_{i1} & \cdots & \log_2\frac{1}{4}\sum_{i=N_{FFT}-4}^{N_{FFT}}a_{iN_{FFT}}
  \end{bmatrix}\\
  &\stackrel{\mathtt{(/N_{FFT})\circ reduceV(+)}}{\rightarrow}
  \begin{bmatrix}
  EMV(a_{eb,1}) & \cdots & EMV(a_{eb,N_{FFT}}) 
  \end{bmatrix}
  \end{aligned}
$${#eq:cfar-emv}

Finally, it remains to zip the extracted `bin` matrix, the yielded
`emv`, `lmv` matrices and the yielded `md` vector with the CFAR
normalization function defined in [@eq:cfar], thus obtaining the
normalized $\mathtt{Center Range Bin}\times\mathtt{Doppler Window}$
matrix for each beam.

 ### Integration (INT){#sec:int-shallow label="INT in ForSyDe-Shallow"}

![INT on video structure](figs/int-cube.pdf){#fig:int-cube}


> int :: Signal (Beam (CRange (Window RealData)))
>     -> Signal (Beam (CRange (Window RealData)))
>     -> Signal (Beam (CRange (Window RealData)))
> int cr cl = applyFIR $ addSCube cr cl
>   where
>     applyFIR   = fir' addSCube mulSCube delaySCube mkFirCoefs
>     addSCube   = comb2SY (zipWithCube (+))
>     mulSCube c = combSY (mapCube (*c))
>     delaySCube = delaySY (infiniteCube 0)

> ------------------------------------------------------
> -- System Process Network
> ------------------------------------------------------

> aesa :: Signal (Range (Antenna CpxData))
>      -> Signal (Beam (CRange (Window RealData)))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ dfb lCt
>     rDfb      = cfar $ dfb rCt
>     (lCt,rCt) = ct $ pc $ dbf video
>     
> ------------------------------------------------------
> -- Helpers, constants generators
> ------------------------------------------------------

 ## Coefficient Generators{#sec:coefs-shallow label="Coefficients"}


> mkBeamConsts :: Int  -- ^ Number of antenna elements (x in e_x, Figure 3)
>              -> Int  -- ^ Number of beams (y in b_y, Figure 3)
>              -> Vector (Vector CpxData)
> mkBeamConsts n_ae n_b = zipWithMat (*) taperingCoefs phaseShiftCoefs
>   where
>     taperingCoefs     = mapV (copyV n_b) taylorCf
>     phaseShiftCoefs   = mapV (\k -> mapV (mkCoeff k) thetas) (vectorFloat [1..n_ae])
>     mkCoeff k theta_l = exp $ cis $ (k - 9.5) * d * sin theta_l / lambda -- Eqs.(1) and (2)
>     -----------------------------
>     taylorCf = mapV (\x -> mkPolar x 0) (taylor n_ae 4 (-30)) -- random parameters; stand for c_k, Eq.1
>     thetas   = vectorFloat [1..n_b]                               -- random theta_l for l=1 to y
>     d        = 0.1                                                -- the distance between the antenna elements.
>     lambda   = 660                                                -- wavelength of the pulse
>     -----------------------------
>     vectorFloat = vector . map fromIntegral

> mkPcCoefs = hanning 8

> mkWeightCoefs = mapV (\x -> mkPolar x x) (hanning nFFT)

> mkFirCoefs = vector [1,1,1,1,0,0,0,0]

