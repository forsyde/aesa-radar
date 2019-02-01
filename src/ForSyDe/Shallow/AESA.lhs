> {-# LANGUAGE PackageImports #-}   -- you can ignore this line for now
> module ForSyDe.Shallow.AESA where 

 ### Imported Libraries

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

 ### Type Aliases and Constants{#sec:aliases-shallow label="Type Aliases and Constants"}

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

 ### Video Processing Pipeline Stages

In this section we follow each stage described in
[@sec:video-chain-spec], and model it as a process operating on a
matrix or on a cube of antenna samples.

 #### Digital Beamforming (DBF){#sec:dbf-shallow label="DBF in ForSyDe-Shallow"}

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
well. As such, we can model the DBF stage as a combinational SY
process
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

> fDBF :: Antenna CpxData
>      -> Beam    CpxData
> fDBF antennaEl = beam
>   where
>     beam       = reduceV (zipWithV (+)) beamMatrix
>     beamMatrix = zipWithMat (*) elMatrix beamConsts
>     elMatrix   = mapV infiniteV antennaEl
>     beamConsts = mkBeamConsts (lengthV antennaEl) nB

The matrix `beamConsts` is generated using the generator `mkBeamConsts` defined in [@sec:coefs-shallow].

$$ \stackrel{\mbox{elMatrix}}{
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
   \Rightarrow
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
$$ {#eq:dbf-mat}

 #### Pulse Compression (PC){#sec:pc-shallow label="PC in ForSyDe-Shallow"}


![PC on video structure](figs/pc-cube.pdf){#fig:pc-cube}

> pc :: Signal (Beam (Range CpxData))
>    -> Signal (Beam (Range CpxData))
> pc = combSY (mapV fPC)

> fPC :: Range CpxData -- ^ input range bin     
>     -> Range CpxData -- ^ output pulse-compressed bin
> fPC = fir mkPcCoefs

> fWeight :: Window CpxData -> Window CpxData
> fWeight = zipWithV (*) mkWeightCoefs

> fDoppler :: Window CpxData -> Window RealData
> fDoppler = mapV envelope . fft nFFT
>   where
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

> fCFAR :: Range (Window RealData) -> CRange (Window RealData)
> fCFAR r_of_d = zipWith4V (\m -> zipWith3V (normCfa m)) md bin lmv emv
>   where
>     bin = dropV (nFFT + 1) r_of_d
>     md  = mapV (logBase 2 . reduceV min) bin
>     emv = mapV (meanFun . takeV nFFT) stens
>     lmv = mapV (meanFun . dropV (nFFT + 3)) stens
>     -----------------------------------------------
>     normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
>     meanFun :: Vector (Vector RealData) -> Vector RealData
>     meanFun = reduceV addV . mapV (mapV (logBase 2 . (/4)) . reduceV addV) . groupV 4
>     stens   = stencilV (2 * nFFT + 3) r_of_d
>     addV    = zipWithV (+)
>   
> ------------------------------------------------------
> -- Stage processes 
> ------------------------------------------------------



> ct :: Signal (Beam (Range CpxData))
>   -> (Signal (Beam (Range CpxData)),
>       Signal (Beam (Range CpxData)))
> ct pcSig = (rightWSig, leftWSig)
>   where
>     rightWSig = pcSig
>     leftWSig  = delaySDF initBatch pcSig
>     initBatch = replicate (nFFT `div` 2) (infiniteMat (cis 0))

> doppler :: Signal (Beam (Range CpxData))
>         -> Signal (Beam (Range (Window RealData)))
> doppler = actor11SDF nFFT 1 ((:[]) . transformCube . transposeCube . vector)
>   where
>     transformCube = mapMat (fDoppler . fWeight)

> cfar :: Signal (Beam ( Range (Window RealData)))
>      -> Signal (Beam (CRange (Window RealData)))
> cfar = actor11SDF 1 1 ((:[]) . mapV fCFAR . head)

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
>     lDfb      = cfar $ doppler lCt
>     rDfb      = cfar $ doppler rCt
>     (lCt,rCt) = ct $ pc $ dbf video
>     
> ------------------------------------------------------
> -- Helpers, constants generators
> ------------------------------------------------------

 ### Coefficient Generators{#sec:coefs-shallow label="Coefficients"}


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

