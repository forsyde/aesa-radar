> {-# LANGUAGE PackageImports #-}

> module ForSyDe.Shallow.AESA where

> import Data.Complex
> import ForSyDe.Shallow hiding (fft)
> import ForSyDe.Shallow.Utility.Cube
> import ForSyDe.Shallow.Utility.DSP

> -- | explicit inport from extensions package. Will be imported
> -- normally once the extensions are merged into forsyde-shallow
> import "forsyde-shallow-extensions" ForSyDe.Shallow.Core.Vector
> import "forsyde-shallow-extensions" ForSyDe.Shallow.Utility.Matrix

> type Antenna     = Vector 
> type Range       = Vector    -- ^ all anenna elements have the same number of range bins
> type CRange      = Vector    -- ^ center range bins; L = L_Range - 2 * NFFT - 3
> type PulseSig a  = Signal a  -- ^ individual (synced) pulses
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
>     beam       = reduceV (zipWithV (+)) beamMatrix
>     beamMatrix = zipWithMat (*) elMatrix beamConsts
>     elMatrix   = mapV infiniteV antennaEl
>     beamConsts = mkBeamConsts (lengthV antennaEl) beamSIZE

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

> dbf :: PulseSig (Range (Antenna CpxData))
>     -> PulseSig (Range (Beam    CpxData))
> dbf = combSY (mapV fDBF)

> pc :: PulseSig (Range (Beam CpxData))
>    -> PulseSig (Beam (Range CpxData))
> pc = combSY (mapV fPC . transposeMat)

> ct :: PulseSig (Beam (Range CpxData))
>   -> (PulseSig (Beam (Range CpxData)),
>       PulseSig (Beam (Range CpxData)))
> ct pcSig = (rightWSig, leftWSig)
>   where
>     rightWSig = pcSig
>     leftWSig  = delaySDF initBatch pcSig
>     initBatch = replicate (nFFT `div` 2) (infiniteMat (cis 0))

> doppler :: PulseSig (Beam (Range CpxData))
>         -> PulseSig (Beam (Range (Window RealData)))
> doppler = actor11SDF nFFT 1 ((:[]) . transformCube . transposeCube . vector)
>   where
>     transformCube = mapMat (fDoppler . fWeight)

> cfar :: PulseSig (Beam ( Range (Window RealData)))
>      -> PulseSig (Beam (CRange (Window RealData)))
> cfar = actor11SDF 1 1 ((:[]) . mapV fCFAR . head)

> int :: PulseSig (Beam (CRange (Window RealData)))
>     -> PulseSig (Beam (CRange (Window RealData)))
>     -> PulseSig (Beam (CRange (Window RealData)))
> int cr cl = applyFIR $ addSCube cr cl
>   where
>     applyFIR   = fir' addSCube mulSCube delaySCube mkFirCoefs
>     addSCube   = comb2SY (zipWithCube (+))
>     mulSCube c = combSY (mapCube (*c))
>     delaySCube = delaySY (infiniteCube 0)

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

