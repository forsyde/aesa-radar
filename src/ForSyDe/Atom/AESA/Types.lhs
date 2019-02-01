 ### Aliases and constants

> {-# LANGUAGE PackageImports #-}
> module ForSyDe.Atom.AESA.Types where

As we the AESA application uses complex numbers, we use Haskell's
[`Complex`](http://hackage.haskell.org/package/base/docs/Data-Complex.html)
type.

> import Data.Complex

> -- import our own custom Vector module, with bug-fixes. It will be
> -- imported normally once 'forsyde-atom-extensions' is pushed to upstream.
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector

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
  N_b-2N_{FFT}-3$.

* `Window` stands for a doppler window of $N_{FFT}$ pulses.

> type Antenna     = Vector -- length: nA
> type Beam        = Vector -- length: nB
> type Range       = Vector -- length: nb
> type CRange      = Vector -- length: nb'
> type Window      = Vector -- length: nFFT

Finally we provide two aliases for the basic Haskell data types used
in the system, to stay consistent with the application specification.

> type CpxData     = Complex Float
> type RealData    = Float

