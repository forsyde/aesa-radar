> {-# LANGUAGE PackageImports #-}
> module ForSyDe.Atom.AESA.Coefs where

> -- import our own custom Vector module, with bug-fixes. It will be
> -- imported normally once 'forsyde-atom-extensions' is pushed to upstream.
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import ForSyDe.Atom.Skeleton.Vector.DSP
> import Data.Complex
> import ForSyDe.Atom.AESA.Types

The only constants which need to be hard-coded (or passed as arguments
for that matter) are $N_B$ and $N_{FFT}$. The rest are inferred from
the size of the input data.

> nB   = 8   :: Int
> nFFT = 512 :: Int

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


> mkPcCoefs = hanning 8  :: Vector CpxData

> mkWeightCoefs = V.farm11 (\x -> mkPolar x x) (hanning nFFT) :: Vector CpxData

> mkFirCoefs = vector [1,1,1,1,0,0,0,0] :: Vector RealData

