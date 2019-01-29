
 ## The ForSyDe-Atom Implementation of the AESA Model

[ForSyDe-Atom](https://forsyde.github.io/forsyde-atom/) is a spin-off
of [ForSyDe-Shallow](https://forsyde.github.io/forsyde-atom/) which
explores new modeling concepts, and has a fundamentally different
approach to how models are described and instantiated. Historically,
it has been the "playground" for developing and applying modeling
concepts such as algorithmic skeletons (parallel patterns) and
applicative-style modeling in ForSyDe. For more information on the
main concepts behind ForSyDe-Atom see [@ungureanu17]. From the point
of view of user experience though, the API is pretty much identical to
ForSyDe-Shallow's, with the following two main differences:

 * the user has more control of which libraries are imported. Functions are not differentiated by their suffix any longer, which means that multiple libraries export functions which deliberately share the same name. As such, the suggested programming style for mixed-library designs is to alias the imported library (e.g. `import ForSyDe.Atom.MoC.SY as SY`) and reference the function using the alias as a prefix (e.g. `SY.mealy` instead of `mealySY` in ForSyDe-Shallow).

 * some of ForSyDe's "canonical" names for process constructors inspired from functional programming have been replaced with more suggestive names inspired from component-based modeling, which are denoting common building blocks, relevant to their domain. For example `mapSY` is now called `SY.comb11`; `zipWithSY` is now called `SY.comb21`; `zipWithV` is now called `V.farm21`, etc.

The ForSyDe-Atom definition of the Saab-AESA application is found in
`<root>/src/AESAAtom.lhs` and can be imported as a generic library
(e.g. in the interpreter session).

> {-# LANGUAGE PackageImports #-}
> module AESAAtom where

 ### Imported libraries

As we the AESA application uses complex numbers, we use Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html)
type.

> import Data.Complex

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
potential for parallel exploitation.

> -- import our own custom Vector module, with bug-fixes. It will be
> -- imported normally once 'forsyde-atom-extensions' is pushed to upstream.
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Atom.Skeleton.Vector.Cube   as C
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import ForSyDe.Atom.Skeleton.Vector.Designs


> type Antenna     = Vector 
> type Range       = Vector       -- ^ all anenna elements have the same number of range bins
> type CRange      = Vector       -- ^ center range bins; L = L_Range - 2 * NFFT - 3
> type PulseSig a  = SY.Signal a  -- ^ individual (syncemake cd) pulses
> type PulseWSig a = SDF.Signal a -- ^ pulses belonging to a doppler window (of size NFFT)
> type Beam        = Vector
> type Window      = Vector
> type CpxData     = Complex Float
> type RealData    = Float

> -------------

> beamSIZE = 8
> nFFT = 8 :: Int

> ------------------------------------------------------
> -- Vector (i.e. kernel) functions used in each stage 
> ------------------------------------------------------

> fDBF :: Antenna CpxData
>      -> Beam CpxData
> fDBF antennaEl = beam
>   where
>     beam       = V.reduce (V.farm21 (+)) beamMatrix
>     beamMatrix = M.farm21 (*) elMatrix beamConsts
>     elMatrix   = V.farm11 V.fanout antennaEl
>     beamConsts = mkBeamConsts (V.length antennaEl) beamSIZE

> fPC :: Range CpxData -- ^ input range bin     
>     -> Range CpxData -- ^ output pulse-compressed bin
> fPC = fir mkPcCoefs

> fWeight :: Window CpxData -> Window CpxData
> fWeight = V.farm21 (*) mkWeightCoefs

> fDoppler :: Window CpxData -> Window RealData
> fDoppler = V.farm11 envelope . fft nFFT
>   where
>     envelope a = let i = realPart a
>                      q = imagPart a
>                  in sqrt (i * i + q * q)

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
> ------------------------------------------------------
> -- Stage processes 
> ------------------------------------------------------

> dbf :: PulseSig (Range (Antenna CpxData))
>     -> PulseSig (Range (Beam    CpxData))
> dbf = SY.comb11 (V.farm11 fDBF)

> pc :: PulseSig (Range (Beam CpxData))
>    -> PulseSig (Beam (Range CpxData))
> pc = SY.comb11 (V.farm11 fPC . M.transpose)

> ct :: PulseSig (Beam (Range CpxData))
>   -> (PulseWSig (Beam (Range CpxData)),
>       PulseWSig (Beam (Range CpxData)))
> ct pcSig = (rightWSig, leftWSig)
>   where
>     wsig      = SY.toSDF pcSig
>     rightWSig = wsig
>     leftWSig  = SDF.delay initBatch wsig
>     initBatch = replicate (nFFT `div` 2) (M.fanout (cis 0))

> doppler :: PulseWSig (Beam (Range CpxData))
>         -> PulseWSig (Beam (Range (Window RealData)))
> doppler = SDF.comb11 (nFFT, 1, (:[]) . transformCube . C.transpose1 . V.vector)
>   where
>     transformCube = M.farm11 (fDoppler . fWeight)

> cfar :: PulseWSig (Beam ( Range (Window RealData)))
>      -> PulseWSig (Beam (CRange (Window RealData)))
> cfar = SDF.comb11 (1, 1, (:[]) . V.farm11 fCFAR . head)

> int :: PulseWSig (Beam (CRange (Window RealData)))
>     -> PulseWSig (Beam (CRange (Window RealData)))
>     -> PulseSig  (Beam (CRange (Window RealData)))
> int cr cl = applyFIR $ addSCube (SDF.toSY cr) (SDF.toSY cl)
>   where
>     applyFIR   = fir' addSCube mulSCube delaySCube mkFirCoefs
>     addSCube   = SY.comb21 (C.farm21 (+))
>     mulSCube c = SY.comb11 (C.farm11 (*c))
>     delaySCube = SY.delay (C.fanout 0)

> ------------------------------------------------------
> -- System Process Network
> ------------------------------------------------------

> aesa :: PulseSig (Range (Antenna CpxData))
>      -> PulseSig (Beam (CRange (Window RealData)))
> aesa video = int lDfb rDfb
>   where
>     lDfb      = cfar $ doppler lCt
>     rDfb      = cfar $ doppler rCt
>     (lCt,rCt) = ct $ pc $ dbf video
>     
> ------------------------------------------------------
> -- Helpers, constants generators
> ------------------------------------------------------

> mkBeamConsts :: Int  -- ^ Number of antenna elements (x in e_x, Figure 3)
>              -> Int  -- ^ Number of beams (y in b_y, Figure 3)
>              -> Vector (Vector CpxData)
> mkBeamConsts n_ae n_b = M.farm21 (*) taperingCoefs phaseShiftCoefs
>   where
>     taperingCoefs     = V.farm11 (V.fanoutn n_b) taylorCf
>     phaseShiftCoefs   = V.farm11 (\k -> V.farm11 (mkCoeff k) thetas) (vectorFloat [1..n_ae])
>     mkCoeff k theta_l = exp $ cis $ (k - 9.5) * d * sin theta_l / lambda -- Eqs.(1) and (2)
>     --------------
>     taylorCf = V.farm11 (\x -> mkPolar x 0) (taylor n_ae 4 (-30)) -- random parameters; stand for c_k, Eq.1
>     thetas   = vectorFloat [1..n_b]                               -- random theta_l for l=1 to y
>     d        = 0.1                                                -- the distance between the antenna elements.
>     lambda   = 660                                                -- wavelength of the pulse
>     --------------
>     vectorFloat = vector . map fromIntegral

> mkPcCoefs = hanning 8

> mkWeightCoefs = V.farm11 (\x -> mkPolar x x) (hanning nFFT)

> mkFirCoefs = vector [1,1,1,1,0,0,0,0]
> ---------------------------

