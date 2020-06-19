
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

Other necessary utilities are also imported.

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

According to basic radar principles, an object is detected by sending carrier pulse
wave (usually in GHz band, pulse width $w$, period $p$), and decoding the cumulative
information from the returning signal, such as reflection time $r$ or phase modulation
$\phi$. In an AESA radar, the constructive interference created by multiple
simultaneous detections (see @fig:aesa-beamforming) infers information about direction
or speed. Each antenna element is extracting phase information as complex numbers
$I+iQ$ where $I=A\cos(\phi t)$ and $Q=A\sin(\phi t)$, which are then samples them and
stores them into range bins, like in @fig:aesa-iq.

![Extracting the phase information](figs/iq.pdf){#fig:aesa-iq}

We model the radar environment by recreating, in CT domain, the image of signals
reflected by arbitrary objects, as perceived by every antenna element. Since many of
the calculations are time-dependent, we need a high precision for the number
representation. Therefore, as with the time representation, the coefficients
representing the radar physics need to be _defined_ as rationals (and not just
converted), hence we define `'` alternatives to the coefficients in `AESA.Params`.

> freqRadar'  = 10e9              :: Rational
> waveLength' = 3e8 / freqRadar'
> dElements'  = waveLength' / 2
> fSampling'  = 3e6               :: Rational
> pulseWidth' = 1e-6              :: Rational
> sampPeriod'  = 1 / fSampling'   :: Rational
> pulsePeriod' = sampPeriod' * realToFrac nb

We approach modeling of the object reflection signals from two perspectives: the first
one is a simple "translation" of a numerical program, e.g. written in Matlab or
Python, where we only abstract away the time representation as a function argument;
the second one is a more proper description of the signal transformations and
interactions through CT processes.

 ### Approach 1: Translating a Numerical Program{#sec:atom-radar-app1}

In this approach we simply translate the Python script used to generate the AESA radar
indata for the previous sections, included in this project source files. We do this in
order to familiarize with the concept of continuum in ForSyDe as simply functions
over (an abstract representation of) time. This way any numerical program can become a
CT signal by defining it as a function which exposes the time variable $t$ as an
argument and passes it to an infinite signal generator.

The following function describes the value in time of the impulses reflected from a
specific object with certain characteristics (see arguments list). 


> reflectionFunc :: Float   -- ^ initial phase, random between [0,2pi)
>                -> Float   -- ^ object distance from radar, in meters
>                -> Float   -- ^ $\theta$, angle relative to the radar element
>                -> Float   -- ^ relative speed in m/s. Positive speed means approaching object
>                -> Integer -- ^ signal power
>                -> Int     -- ^ index of the antenna element in [0..nA]
>                -> T.Time  -- ^ Abstract time representation. Evaluated only when plotting
>                -> Complex Float -- ^ Value of reflection signal for an antenna element (t)
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
>     -- Models the delay between the first antenna element and the current one    
>     channelDelay = (-1) * i' * pi * sin angle
>     bigI  =        bigA * cos (wd * t' + phi)
>     bigQ  = (-1) * bigA * sin (wd * t' + phi)
>     value = (bigI :+ bigQ) * (cos channelDelay :+ sin channelDelay)

The `reflectionFunc` function is then passed to a `CT.infinite1` process constructor which generates an infinite signal. The object reflection on all antennas in a AESA system is constructed with a `V.farm11` skeleton, which instantiates each signal generator characteristic function according to its antenna index. 

> objectReflection :: Float -> Float -> Float -> Float -> Integer
>                  -> Vector (CTSignal (Complex Float))
> objectReflection radix distance angle relativeSpeed power
>   = V.farm11 channelRefl (vector [0..nA-1])
>   where phi_start     = 2 * pi * radix / 360
>         channelRefl i = CT.infinite1
>           (reflectionFunc phi_start distance angle relativeSpeed power i)

 ### Approach 2: CT Signal Generators{#sec:atom-radar-app2}

The second approach combines two CT signals to create the reflection model: a pulse
width modulation (PWM) signal modeling the radar pulses, and an envelope signal
containing the object's phase information $I+iQ$ as a function of time, like in
@fig:aesa-iq. The envelope function is shown below. Observe that it only describes
angle and relative speed, but not distance, since the distance is a function of the
reflection time.

> reflectionEnvelope :: Float   -- ^ initial phase, random between [0,2pi)
>                    -> Float   -- ^ $\theta$, angle relative to the radar element
>                    -> Float   -- ^ relative speed in m/s. Positive speed means approaching object
>                    -> Integer -- ^ signal power
>                    -> Int     -- ^ index of the antenna element in [0..nA]
>                    -> T.Time  -- ^ Abstract time representation. Evaluated only when plotting
>                    -> Complex Float -- ^ envelope for one antenna element
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

![Modeling an object reflection signal in all channels](figs/radar-env.pdf){#fig:radar-env}

The reflection signal in a channel is modeled like in @fig:radar-env as the product
between a PWM signal and an envelope generator. The `pwm` generator is a specialized
instance of a
[`DE.state11`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-DE.html#v:state22)
process, and the reflection time $r$ is controlled with a [`DE.delay`](https://forsyde.github.io/forsyde-atom/api/ForSyDe-Atom-MoC-DE.html#v:delay) process. `DE.hold1` is a MoC interface which transforms a DE signal into a CT signal of constant sub-signals (see $s_3$ in @fig:radar-env). 

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
>     pulseSig      = DE.hold1 $ DE.delay reflTime 0 $ DE.pwm pulseWidth' pulsePeriod'
> 
>     -- an infinite CT signal describing the modulation for each channel
>     modulationSig = CT.infinite1 . reflectionEnvelope phi angle relativeSpeed power

Finally, we describe an object reflection in all channels similarly to the previous
approach, namely as a farm of `channelReflection processes`.

> objectReflection' :: Float -> Float -> Float -> Float -> Integer
>                   -> Vector (CTSignal (Complex Float))
> objectReflection' radix distance angle relativeSpeed power
>   = V.farm11 channelRefl (vector [0..nA-1])
>   where phi_start   = 2 * pi * radix / 360
>         channelRefl =
>           channelReflection' phi_start distance angle relativeSpeed power

Both approaches 1 and 2 are equivalent. The reader is free to test this statement
using whatever method she chooses, but for the sake of readability we will only use
approach 2 from now on, i.e. `objectReflection'`.

 ## Sampling Noisy Data. Using Distributions.

Multiple reflected objects are described as a vector of vectors of complex CT signals,
each originating from its corresponding `objectReflection'` farm. To obtain the
composite signal from multiple object reflections we sum-reduce all object reflection
models, like in @fig:radar-adc.

![Video indata acquisition model](figs/radar-adc.pdf){#fig:radar-adc}

> reflectionMix :: Vector (Vector (CTSignal (Complex Float)))
>               -> Vector (CTSignal (Complex Float))
> reflectionMix = (V.reduce . V.farm21 . CT.comb21) (+) 

![Ideal ADC model](figs/if-ad.pdf){#fig:radar-if-ad}

The next step is to sample the CT signals using an analog/digital converter (ADC)
model. An ADC is characterized by:

1. a sampling rate, whose principle is illustrated in @fig:radar-if-ad. In our case we
   describe a CT/SY MoC interface as a serialization of CT/DE and DE/SY interfaces. 

1. an additive _white noise_ for each channel. White noise is a side-effect of the
   physical environment, and any sampled value is randomly-dependent on a particular
   experiment/observation. To describe random distributions in a pure setting, we wrap
   the object reflection SY samples into a `N.normal` recipe for generating
   normally-distributed observations with a standard deviation dependent on the power
   of the noise.

The `N.normal` wrapper is a function describing a Gaussian distribution, in our case
the Box-Muller method to transform uniform distributions, and is dependent on a
(pseudo-)random number generator, in our case
[`StdGen`](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#t:StdGen). A
`StdGen` is acquired outside the model as an IO action and is only passed to the model
as an argument. By all means, these recipes (pure functions of "random seeds") can be
propagated and transformed throughout the AESA model, _exactly_ in the same way as CT
signals (pure functions of "time"), using atoms and patterns from to the `Probability`
layer. However, for the scope of this model we sample these distributions immediately,
because the AESA system is expecting _numbers_ as video indata, not
distributions. Hence our ADC model looks as follows:

> adc :: Integer                   -- ^ noise power
>     -> DESignal ()               -- ^ global ADC sampler signal. Defines sampling rate
>     -> SY.Signal StdGen          -- ^ signal of random generators used as "seeds"
>     -> CTSignal (Complex Float)  -- ^ pure CT signal
>     -> SY.Signal (Complex Float) -- ^ noisy, sampled/observed SY signal
> adc noisePow sampler seeds = probeSignal . addNoise . sampleAndHold
>   where
>     sampleAndHold = snd . DE.toSY1 . CT.sampDE1 sampler
>     addNoise      = SY.comb11 (N.normal (2^^noisePow))
>     probeSignal   = SY.comb21 Prob.sample seeds

The full acquisition model for the AESA signal processing system in @fig:radar-adc is
thus the following, where each CT signal vector is generated with a specific instance
of `objectReflection'`.

> videoInData :: Integer                   -- ^ noise power
>             -> Vector (SY.Signal StdGen) -- ^ random seeds for sampling
>             -> Vector (Vector (CTSignal (Complex Float))) -- ^ reflections for all objects
>             -> Vector (SY.Signal (Complex Float))         -- ^ video Indata
> videoInData noisePow seeds reflections = inData
>   where
>     mixedRefl = reflectionMix reflections
>     inData    = V.farm21 (adc noisePow sampler) seeds mixedRefl
>     sampler   = DE.generate1 id (sampPeriod', ()) -- global ADC trigger

The following code can be safely ignored! It enables to generate random `Complex` numbers, because their respective Haskell library does not define this instance.

> instance Random (Complex Float) where
>   randomR (lo,hi) g = let (i,g')  = randomR (realPart lo, realPart hi) g
>                           (q,g'') = randomR (imagPart lo, imagPart hi) g'
>                       in (i:+q, g'')
>   random g  = let (i,g')  = random g
>                   (q,g'') = random g'
>               in (i:+q, g'')

