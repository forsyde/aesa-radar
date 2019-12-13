{-# LANGUAGE PackageImports #-} --can be ignored
module AESA.Radar where

import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
import                           ForSyDe.Atom.MoC.CT as CT
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.DE as DE
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V

import qualified                 ForSyDe.Atom.MoC.Time as T
import "forsyde-atom-extensions" ForSyDe.Atom.MoC.TimeStamp as Ts hiding (pi)

import                           AESA.Params
import                           Control.Applicative
import                           Control.Monad
import                           Data.Complex
import                           System.Random

------------------------------------------------------------

pulsePeriod = sampPeriod * timeStamp nb :: TimeStamp
sampPeriod  = timeStamp (1 / fSampling) :: TimeStamp

------------------------------------------------------------
-- APPROACH 1: translate Python function
------------------------------------------------------------


-- radix is a random number in (0,359)
-- Distance in meters
-- Relative speed in m/s, positive relative speed means approaching object
-- Angle to object, given as Theta above
channelReflection :: Float -> Float -> Float -> Float -> Integer
                   -> Int -> CT.Signal (Complex Float)
channelReflection phi distance angle relativeSpeed power chanIndex
  = CT.infinite1 (reflectionFunc phi distance angle relativeSpeed power chanIndex)

reflectionFunc :: Float -> Float -> Float -> Float -> Integer
               -> Int -> T.Time -> Complex Float
reflectionFunc phi distance angle relativeSpeed power chanIndex t
  | t' >= trefl_start && t' <= trefl_stop = value
  | otherwise = 0
  where
    -- convert integer to floating point
    i'  = realToFrac chanIndex
    -- convert "real" numbers to floating point, which are part of the spec
    t'  = realToFrac t
    
    -- wd is 2*pi*doppler frequency
    wd = 2 * pi * relativeSpeed / waveLength
    
    -- A is the power of the reflected signal (-5 => 1/32 of fullscale)
    bigA = 2 ^ power

    -- Large distances will fold to lower ones, assume infinite sequences
    -- Otherwise the the first X pulses would be absent
    trefl_start = 2 * distance / 3e8 
    trefl_stop  = 2 * distance / 3e8 + pulseWidth
    
    channelDelay = (-1) * i' * pi * sin angle
    bigI  =        bigA * cos (wd * t' + phi)
    bigQ  = (-1) * bigA * sin (wd * t' + phi)
    value = (bigI :+ bigQ) * (cos channelDelay :+ sin channelDelay)

------------------------------------------------------------
-- APPROACH 2: combine CT/DE signals to generate reflection
------------------------------------------------------------

channelReflection' :: Float -> Float -> Float -> Float -> Integer
                   -> Int -> CT.Signal (Complex Float)
channelReflection' phi distance angle relativeSpeed power chanIndex
  -- delay the modulated pulse reflections according to the object distance.
  -- until the first reflection is observed, the signal is constant 0
  = CT.delay reflTime (T.const 0) $ CT.comb21 (*) pulseSig (modulationSig chanIndex)
  where
    -- convert floating point numbers to timestamp format
    distance'   = timeStamp distance
    pulseWidth' = timeStamp pulseWidth

    -- reflection time, given as timestamp
    reflTime      = 2 * distance' / 3e8

    -- a discrete (infinite) PWM signal with amplitude 1, converted to CT domain
    pulseSig      = DE.toCT1 $ DE.comb11 T.const $ pwm pulseWidth' pulsePeriod

    -- an infinite CT signal describing the modulation for each channel
    modulationSig = CT.infinite1 . reflectionModulator phi angle relativeSpeed power

reflectionModulator :: Float -> Float -> Float -> Integer
                    -> Int -> T.Time -> Complex Float
reflectionModulator phi angle relativeSpeed power chanIdx t
  = (bigI :+ bigQ) * (cos channelDelay :+ sin channelDelay)
  where
   -- convert integer to floating point
    i'  = realToFrac chanIdx 
    -- convert "real" numbers to floating point (part of the spec)
    t'  = realToFrac t
    
    -- wd is 2*pi*doppler frequency
    wd = 2 * pi * relativeSpeed / waveLength
    
    -- A is the power of the reflected signal (-5 => 1/32 of fullscale)
    bigA = 2 ^^ power

    channelDelay = (-1) * i' * pi * sin angle
    bigI  =        bigA * cos (wd * t' + phi)
    bigQ  = (-1) * bigA * sin (wd * t' + phi)

-------------------------------------------------------------

objectReflection :: Float -> Float -> Float -> Float -> Integer
                 -> Vector (CT.Signal (Complex Float))
objectReflection radix distance angle relativeSpeed power
  = V.farm11 (channelReflection phi_start distance angle relativeSpeed power)
    (vector [1..nA])
  where phi_start = 2 * pi * radix / 360

objectReflection' :: Float -> Float -> Float -> Float -> Integer
                  -> Vector (CT.Signal (Complex Float))
objectReflection' radix distance angle relativeSpeed power
  = V.farm11 (channelReflection' phi_start distance angle relativeSpeed power)
    (vector [1..nA])
  where phi_start = 2 * pi * radix / 360
    
-------------------------------------------------------------

noiseGenerator :: Integer -> IO (DE.Signal () ->  DE.Signal (Complex Float))
noiseGenerator power = do
  gen1 <- getStdGen
  gen2 <- newStdGen
  let sdev = 2 ^^ power
      isig = DE.normalR (0,sdev) gen1
      qsig = DE.normalR (0,sdev) gen2 
  return $ \trig -> DE.comb21 (:+) (isig trig) (qsig trig)

sampSignal :: DE.Signal ()
sampSignal = DE.generate1 id (sampPeriod, ())

-------------------------------------------------------------

videoInData :: DE.Signal ()                               -- ^ trigger
            -> Vector (DE.Signal (Complex Float))         -- ^ white noise
            -> Vector (Vector (CT.Signal (Complex Float)))-- ^ reflections for all objects
            -> Vector (SY.Signal (Complex Float))         -- ^ video Indata
videoInData sampler noise reflections = inData
  where
    mixedRefl = (V.reduce . V.farm21 . CT.comb21) (+) reflections
    sampRefl  = V.farm11 (CT.sampDE1 sampler) mixedRefl
    noisyData = V.farm21 (DE.comb21 (+)) noise sampRefl
    inData    = V.farm11 (snd . DE.toSY1) noisyData
