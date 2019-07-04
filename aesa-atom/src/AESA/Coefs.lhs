 ### Coefficient Generators{#sec:coefs-atom label="Coefficients"}

Here we define the vectors of coefficients used throughout the AESA design. We keep
this module as independent as possible from the main design and export the
coefficients both as ForSyDe-Atom vectors but also as Haskell native lists, so that
other packages can make use of them, without importing the whole ForSyDe-Atom chain.

> {-# LANGUAGE PackageImports #-}
> module AESA.Coefs where

> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import ForSyDe.Atom.Skeleton.Vector.DSP
> import Data.Complex

The `mkBeamConst` generator creates a matrix of $\alpha_{ij}$ beam constants used in
the digital beamforming stage [@sec:dbf-atom]. These beam constants perform both phase
shift and tapering according to [@eq:beam-coef], where $c_k$ performs tapering and
$\varphi_{kl}$ perform phase shifting. For tapering we use a set of Taylor
coefficients generated with our in-house utility `taylor`. The phase shift
shall be calculated according to [@eq:beam-phase], where $d$ is the distance between
the antenna elements. $\theta_l$ is the angle between the wave front of the current
beam and normal of the antenna elements and $\lambda$ is the wavelength of the pulse.

$$ \alpha_{kl}=c_k e^{j\varphi_{kl}},\ \forall k\in[0,N_A-1], l \in [0,N_B-1]$$ {#eq:beam-coef}

$$ \varphi_{kl}=\frac{(k-9.5)\cdot 2\pi\cdot d \sin\theta}{\lambda}$$ {#eq:beam-phase}

> mkBeamConsts :: RealFloat a
>              => a                  -- ^ distance between radar elements
>              -> a                  -- ^ radar signal wavelength
>              -> Int                -- ^ Number of antenna elements
>              -> Int                -- ^ Number of resulting beams
>              -> Matrix (Complex a)
> mkBeamConsts d lambda nA nB = M.farm21 mulScale taperingCf phaseShiftCf
>   where
>     -- all coefficients are normalized, i.e. scaled with 1/nA' 
>     mulScale x y   = x * y / nA'
>     -- tapering coefficients, c_k in Eq. (4)
>     taperingCf     = V.farm11 (V.fanoutn nB) taylorCf
>     -- phase shift coefficients, e^(j*phi_kl) in Eqs.(4) and (5)
>     phaseShiftCf   = V.farm11 (\k -> V.farm11 (mkCf k) thetas) antennaIxs
>     mkCf k theta_l = cis $ (k - 9.5) * 2 * pi * d * sin theta_l / lambda
>     --------------
>     -- Taylor series: nA real numbers; 4 nearly constant adjacent side lobes;
>     -- peak sidelobe level of -30dB
>     taylorCf = taylor nA 4 (-30)
>     -- theta_l spanning nB angles from 0 to pi
>     thetas   = V.farm11 (\t -> pi/3 + t * (pi - 2*pi/3)/(nB'-1)) beamIxs
>     --------------
>     nA'       = fromIntegral nA
>     nB'       = fromIntegral nB
>     antennaIxs = vector $ map realToFrac [0..nA-1]
>     beamIxs    = vector $ map realToFrac [0..nB-1]

> -- Can be used without importing the ForSyDe.Atom libraries.
> mkBeamConsts' :: RealFloat a => a -> a-> Int -> Int -> [[Complex a]]
> mkBeamConsts' d l nA nB = map fromVector $ fromVector $ mkBeamConsts d l nA nB


| Function           | Original module                    | Package                 |
|--------------------|------------------------------------|-------------------------|
| `farm11`,` farm21`, `fanout`, (`from`-)`vector` | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom  |
| `taylor`           | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |


The `mkPcCoefs` generator for the FIR filter in [@sec:pc-atom] is simply a $n$-tap
Hanning window. It can be changed according to the user requirements.  All
coefficients are scaled with $1/n$ so that the output does not overflow a possible
fixed point representation.

> mkPcCoefs :: Fractional a => Int -> Vector a
> mkPcCoefs n = V.farm11 (\a -> realToFrac a / realToFrac n) $ hanning n

| Function  | Original module                    | Package                 |
|-----------|------------------------------------|-------------------------|
| `hanning` | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |

> 
> -- Can be used without importing the ForSyDe.Atom libraries.
> mkPcCoefs' :: Fractional a => Int -> [a]
> mkPcCoefs' n = fromVector $ mkPcCoefs n

We use also a Hanning window to generate the complex weight coefficients for
decreasing the Doppler side lobes during DFB in [@sec:cube-dfb-atom]. This can be changed
according to the user requirements.

> mkWeightCoefs :: Fractional a => Int -> Vector a
> mkWeightCoefs nFFT = V.farm11 realToFrac $ hanning nFFT

> -- Can be used without importing the ForSyDe.Atom libraries.
> mkWeightCoefs' :: Fractional a => Int -> [a]
> mkWeightCoefs' nFFT = fromVector $ mkWeightCoefs nFFT

For the integrator FIR in [@sec:int-atom] we use a normalized square window.

> mkIntCoefs :: Fractional a => Vector a
> mkIntCoefs = vector mkIntCoefs'

> -- Can be used without importing the ForSyDe.Atom libraries.
> mkIntCoefs' :: Fractional a => [a]
> mkIntCoefs' = [1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8] 

> -- The maximum floating point number representable in Haskell. 
> maxFloat :: Float
> maxFloat = x / 256
>  where n = floatDigits x
>        b = floatRadix x
>        (_, u) = floatRange x
>        x = encodeFloat (b^n - 1) (u - n)

