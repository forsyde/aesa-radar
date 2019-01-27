{-# LANGUAGE PackageImports #-}
module AESAAtom where

import Data.Complex
import ForSyDe.Atom.MoC.SDF                as SDF
import ForSyDe.Atom.MoC.SY                 as SY

-- import custom Vector module, with bug-fixes. It will be imported
-- normally once 'forsyde-atom-extensions' is pushed to upstream.
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
import ForSyDe.Atom.Skeleton.Vector.Cube   as C
import ForSyDe.Atom.Skeleton.Vector.Matrix as M
import ForSyDe.Atom.Skeleton.Vector.Designs


type Antenna     = Vector 
type Range       = Vector       -- ^ all anenna elements have the same number of range bins
type CRange      = Vector       -- ^ center range bins; L = L_Range - 2 * NFFT - 3
type PulseSig a  = SY.Signal a  -- ^ individual (synced) pulses
type PulseWSig a = SDF.Signal a -- ^ pulses belonging to a doppler window (of size NFFT)
type Beam        = Vector
type Window      = Vector
type CpxData     = Complex Float
type RealData    = Float

-------------

beamSIZE = 8
nFFT = 8 :: Int

------------------------------------------------------
-- Vector (i.e. kernel) functions used in each stage 
------------------------------------------------------

fDBF :: Antenna CpxData
     -> Beam CpxData
fDBF antennaEl = beam
  where
    beam       = V.reduce (V.farm21 (+)) beamMatrix
    beamMatrix = M.farm21 (*) elMatrix beamConsts
    elMatrix   = V.farm11 V.fanout antennaEl
    beamConsts = mkBeamConsts (V.length antennaEl) beamSIZE

fPC :: Range CpxData -- ^ input range bin     
    -> Range CpxData -- ^ output pulse-compressed bin
fPC = fir mkPcCoefs

fWeight :: Window CpxData -> Window CpxData
fWeight = V.farm21 (*) mkWeightCoefs

fDoppler :: Window CpxData -> Window RealData
fDoppler = V.farm11 envelope . fft nFFT
  where
    envelope a = let i = realPart a
                     q = imagPart a
                 in sqrt (i * i + q * q)

fCFAR :: Range (Window RealData) -> CRange (Window RealData)
fCFAR r_of_d = V.farm41 (\m -> V.farm31 (normCfa m)) md bin lmv emv
  where
    bin = V.drop (nFFT + 1) r_of_d
    md  = V.farm11 (logBase 2 . V.reduce min) bin
    emv = V.farm11 (meanFun . V.take nFFT) stens
    lmv = V.farm11 (meanFun . V.drop (nFFT + 3)) stens
    -----------------------------------------------
    normCfa m a l e = 2 ** (5 + logBase 2 a - maximum [l,e,m])
    meanFun :: Vector (Vector RealData) -> Vector RealData
    meanFun = V.reduce addV . V.farm11 (V.farm11 (logBase 2 . (/4)) . V.reduce addV) . V.group 4
    stens   = V.stencil (2 * nFFT + 3) r_of_d
    addV    = V.farm21 (+)
  
------------------------------------------------------
-- Stage processes 
------------------------------------------------------

dbf :: PulseSig (Range (Antenna CpxData))
    -> PulseSig (Range (Beam    CpxData))
dbf = SY.comb11 (V.farm11 fDBF)

pc :: PulseSig (Range (Beam CpxData))
   -> PulseSig (Beam (Range CpxData))
pc = SY.comb11 (V.farm11 fPC . M.transpose)

ct :: PulseSig (Beam (Range CpxData))
  -> (PulseWSig (Beam (Range CpxData)),
      PulseWSig (Beam (Range CpxData)))
ct pcSig = (rightWSig, leftWSig)
  where
    wsig      = SY.toSDF pcSig
    rightWSig = wsig
    leftWSig  = SDF.delay initBatch wsig
    initBatch = replicate (nFFT `div` 2) (M.fanout (cis 0))

doppler :: PulseWSig (Beam (Range CpxData))
        -> PulseWSig (Beam (Range (Window RealData)))
doppler = SDF.comb11 (nFFT, 1, (:[]) . transformCube . C.transpose1 . V.vector)
  where
    transformCube = M.farm11 (fDoppler . fWeight)

cfar :: PulseWSig (Beam ( Range (Window RealData)))
     -> PulseWSig (Beam (CRange (Window RealData)))
cfar = SDF.comb11 (1, 1, (:[]) . V.farm11 fCFAR . head)

int :: PulseWSig (Beam (CRange (Window RealData)))
    -> PulseWSig (Beam (CRange (Window RealData)))
    -> PulseSig  (Beam (CRange (Window RealData)))
int cr cl = applyFIR $ addSCube (SDF.toSY cr) (SDF.toSY cl)
  where
    applyFIR   = fir' addSCube mulSCube delaySCube mkFirCoefs
    addSCube   = SY.comb21 (C.farm21 (+))
    mulSCube c = SY.comb11 (C.farm11 (*c))
    delaySCube = SY.delay (C.fanout 0)

------------------------------------------------------
-- System Process Network
------------------------------------------------------

aesa :: PulseSig (Range (Antenna CpxData))
     -> PulseSig (Beam (CRange (Window RealData)))
aesa video = int lDfb rDfb
  where
    lDfb      = cfar $ doppler lCt
    rDfb      = cfar $ doppler rCt
    (lCt,rCt) = ct $ pc $ dbf video
    
------------------------------------------------------
-- Helpers, constants generators
------------------------------------------------------

mkBeamConsts :: Int  -- ^ Number of antenna elements (x in e_x, Figure 3)
             -> Int  -- ^ Number of beams (y in b_y, Figure 3)
             -> Vector (Vector CpxData)
mkBeamConsts n_ae n_b = M.farm21 (*) taperingCoefs phaseShiftCoefs
  where
    taperingCoefs     = V.farm11 (V.fanoutn n_b) taylorCf
    phaseShiftCoefs   = V.farm11 (\k -> V.farm11 (mkCoeff k) thetas) (vectorFloat [1..n_ae])
    mkCoeff k theta_l = exp $ cis $ (k - 9.5) * d * sin theta_l / lambda -- Eqs.(1) and (2)
    --------------
    taylorCf = V.farm11 (\x -> mkPolar x 0) (taylor n_ae 4 (-30)) -- random parameters; stand for c_k, Eq.1
    thetas   = vectorFloat [1..n_b]                               -- random theta_l for l=1 to y
    d        = 0.1                                                -- the distance between the antenna elements.
    lambda   = 660                                                -- wavelength of the pulse
    --------------
    vectorFloat = vector . map fromIntegral

mkPcCoefs = hanning 8

mkWeightCoefs = V.farm11 (\x -> mkPolar x x) (hanning nFFT)

mkFirCoefs = vector [1,1,1,1,0,0,0,0]
---------------------------
 
