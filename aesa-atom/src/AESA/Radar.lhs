
The radar environment model is found in the `AESA.Radar` module in `aesa-atom`
package. This module is by default bypassed by the AESA signal processing executable,
but if invoked manually, it recreates the AESA radar input for the same 13 object
reflections seen in @fig:aesa-indata, and feeds this input to the AESA signal
processing chain. The functions generating this data are presented as follows.

> {-# LANGUAGE PackageImports, FlexibleInstances, TypeSynonymInstances #-}
> module AESA.Radar where

For this model we import the following
[`MoC`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC.html) libraries:
[`CT`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-CT.html) defines
discrete interactions between continuous sub-domains,
[`DE`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-DE.html) defines
the _discrete event_ MoC, which in this case is only used as a conduit MoC, and
[`SY`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-SY.html) which
defines the _synchronous reactive_ MoC, on which the AESA signal processing chain
operates. From the
[`Skeleton`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton.html)
layer we import
[`Vector`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Skeleton-Vector.html). From
the
[`Probability`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Probability.html)
layer we import the
[`Normal`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-Probability-Normal.html)
distribution to represent white noise.

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.Time as T
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.CT as CT
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.DE as DE
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Probability as Prob
> import "forsyde-atom-extensions" ForSyDe.Atom.Probability.Normal as N

We also import other necessary utilities.

> import                           AESA.Params
> import                           Data.Complex
> import                           System.Random

Since we model high frequency signals, we want to avoid unnecessary quantization
errors when representing time instants, therefore we choose a more appropriate
representation for timestamps, as
[`Rational`](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Ratio.html#t:Rational)
numbers. We thus define two aliases `CTSignal` and `DESignal` to represent CT
respectively DE signals with `Rational` timestamps.

> instance TimeStamp Rational
> 
> type CTSignal a = CT.Signal Rational a
> type DESignal a = DE.Signal Rational a

 ## Object Reflection Model {#sec:radar-refl}

The beamforming principle, illustrated in @fig:aesa-beamforming and briefly presented in @sec:cube-dbf-atom allows to extract both distance and speed information by cross-checking the information carried by a reflection signal as seen by multiple antenna elements.

![Antenna Beamforming](figs/beam-acq.png){#fig:aesa-beamforming height=3.5cm}


![The geometry for beamforming calculations](figs/beamform-calculations.png){#fig:aesa-calculations height=3.5cm}

> freqRadar'  = 10e9              :: Rational
> waveLength' = 3e8 / freqRadar'
> dElements'  = waveLength' / 2
> fSampling'  = 3e6               :: Rational
> pulseWidth' = 1e-6              :: Rational
> sampPeriod'  = 1 / fSampling'   :: Rational
> pulsePeriod' = sampPeriod' * realToFrac nb

 ### Approach 1: Translate Discrete Interpretation{#sec:atom-radar-app1}

> objectReflection :: Float -> Float -> Float -> Float -> Integer
>                  -> Vector (CTSignal (Complex Float))
> objectReflection radix distance angle relativeSpeed power
>   = V.farm11 (channelReflection phi_start distance angle relativeSpeed power)
>     (vector [0..nA-1])
>   where phi_start = 2 * pi * radix / 360

> -- radix is a random number in (0,359)
> -- Distance in meters
> -- Relative speed in m/s, positive relative speed means approaching object
> -- Angle to object, given as Theta above
> channelReflection :: Float -> Float -> Float -> Float -> Integer
>                    -> Int -> CTSignal (Complex Float)
> channelReflection phi distance angle relativeSpeed power chanIndex
>   = CT.infinite1 (reflectionFunc phi distance angle relativeSpeed power chanIndex)
> 
> reflectionFunc :: Float -> Float -> Float -> Float -> Integer
>                -> Int -> T.Time -> Complex Float
> reflectionFunc phi distance angle relativeSpeed signalPower chanIx t
>   | range_bin >= trefl_start && range_bin <= trefl_stop && not crossing_reflection = value
>   | not (range_bin >= trefl_start && range_bin <= trefl_stop) && crossing_reflection = value
>   | otherwise = 0
>   where
>     i' = realToFrac chanIx
>     t' = realToFrac t
>     
>     -- wd is 2*pi*doppler frequency
>     wd = 2 * pi * relativeSpeed / waveLength
>     
>     -- A is the power of the reflected signal (-5 => 1/32 of fullscale)
>     bigA = 2 ^^ signalPower
> 
>     -- Large distances will fold to lower ones, assume infinite sequences
>     -- Otherwise the the first X pulses would be absent
>     trefl_start = ceiling ((2 * distance / 3e8) * fSampling) `mod` nb 
>     trefl_stop  = ceiling ((2 * distance / 3e8 + pulseWidth) * fSampling) `mod` nb
>     range_bin   = ceiling (t' * fSampling) `mod` nb
> 
>     -- Handling for distances at the edge of the
>     crossing_reflection = trefl_stop < trefl_start
> 
>     -- channelDelay :: Integer -> Double
>     channelDelay = (-1) * i' * pi * sin angle
>     bigI  =        bigA * cos (wd * t' + phi)
>     bigQ  = (-1) * bigA * sin (wd * t' + phi)
>     value = (bigI :+ bigQ) * (cos channelDelay :+ sin channelDelay)



 ### Approach 2: CT Signal Generators{#sec:atom-radar-app2}

> channelReflection' :: Float -> Float -> Float -> Float -> Integer
>                    -> Int -> CTSignal (Complex Float)
> channelReflection' phi distance angle relativeSpeed power chanIndex
>   -- delay the modulated pulse reflections according to the object distance.
>   -- until the first reflection is observed, the signal is constant 0
>   = CT.comb21 (*) pulseSig (modulationSig chanIndex)
>   where
>     -- convert floating point numbers to timestamp format
>     distance'    = realToFrac distance
> 
>     -- reflection time, given as timestamp
>     reflTime      = 2 * distance' / 3e8
> 
>     -- a discrete (infinite) PWM signal with amplitude 1, converted to CT domain
>     pulseSig      = DE.hold1 $ DE.delay reflTime 0 $ pwm pulseWidth' pulsePeriod'
> 
>     -- an infinite CT signal describing the modulation for each channel
>     modulationSig = CT.infinite1 . reflectionEnvelope phi angle relativeSpeed power
> 
> reflectionEnvelope :: Float -> Float -> Float -> Integer
>                    -> Int -> T.Time -> Complex Float
> reflectionEnvelope phi angle relativeSpeed power chanIdx t
>   = (bigI :+ bigQ) * (cos channelDelay :+ sin channelDelay)
>   where
>    -- convert integer to floating point
>     i'  = realToFrac chanIdx 
>     -- convert "real" numbers to floating point (part of the spec)
>     t'  = realToFrac t
>     
>     -- wd is 2*pi*doppler frequency
>     wd = 2 * pi * relativeSpeed / waveLength
>     
>     -- A is the power of the reflected signal (-5 => 1/32 of fullscale)
>     bigA = 2 ^^ power
> 
>     channelDelay = (-1) * i' * pi * sin angle
>     bigI  =        bigA * cos (wd * t' + phi)
>     bigQ  = (-1) * bigA * sin (wd * t' + phi)
> 
> -------------------------------------------------------------
> 
> 
> objectReflection' :: Float -> Float -> Float -> Float -> Integer
>                   -> Vector (CTSignal (Complex Float))
> objectReflection' radix distance angle relativeSpeed power
>   = V.farm11 (channelReflection' phi_start distance angle relativeSpeed power)
>     (vector [0..nA-1])
>   where phi_start = 2 * pi * radix / 360

 ## Sampling Noisy Data. Using Distributions.

> sampSignal :: DESignal ()
> sampSignal = DE.generate1 id (sampPeriod', ())



> videoInData :: Integer
>             -> DESignal ()                               -- ^ trigger
>             -> Vector (SY.Signal StdGen)              -- ^ random seeds for sampling
>             -> Vector (Vector (CTSignal (Complex Float)))
>             -- ^ reflections for all objects
>             -> Vector (SY.Signal (Complex Float))         -- ^ video Indata
> videoInData noisePow sampler seeds reflections = inData
>   where
>     mixedRefl = (V.reduce . V.farm21 . CT.comb21) (+) reflections
>     sampRefl  = V.farm11 (snd . DE.toSY1 . CT.sampDE1 sampler) mixedRefl
>     noisyData = V.farm11 (SY.comb11 (N.normal (2^^noisePow))) sampRefl
>     inData    = V.farm21 (SY.comb21 Prob.sample) seeds noisyData



> instance Random (Complex Float) where
>   randomR (lo,hi) g = let (i,g')  = randomR (realPart lo, realPart hi) g
>                           (q,g'') = randomR (imagPart lo, imagPart hi) g'
>                       in (i:+q, g'')
>   random g  = let (i,g')  = random g
>                   (q,g'') = random g'
>               in (i:+q, g'')

