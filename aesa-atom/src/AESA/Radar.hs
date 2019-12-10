{-# LANGUAGE PackageImports #-} --can be ignored
module AESA.Radar where

import ForSyDe.Atom.MoC.CT as CT
import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V

import qualified ForSyDe.Atom.MoC.TimeStamp as Ts
import qualified ForSyDe.Atom.MoC.Time as T

import AESA.Params
import Data.Complex-- SignalPower given as relative to the full scale power

objectReflection :: Float -> Float -> Float -> Float -> Integer
                 -> Vector (CT.Signal (Complex Float))
objectReflection  radix d a r s
  = V.farm11 (CT.infinite1 . generateObjectReflection radix d a r s) (vector [1..nA])

-- radix is a random number in (0,359)
-- Distance in meters
-- Relative speed in m/s, positive relative speed means approaching object
-- Angle to object, given as Theta above
generateObjectReflection :: Float -> Float -> Float -> Float -> Integer
                         -> Int -> T.Time -> Complex Float
generateObjectReflection radix distance angle relativeSpeed signalPower chanIx t
  | range_bin >= trefl_start && range_bin <= trefl_stop && not crossing_reflection = value
  | crossing_reflection = value
  | otherwise = 0

  where
    i' = realToFrac chanIx
    t' = realToFrac t
    -- wd is 2*pi*doppler frequency
    wd = 2 * pi * relativeSpeed / waveLength
    
    -- A is the power of the reflected signal (-5 => 1/32 of fullscale)
    bigA = 2 ^ signalPower

    -- Large distances will fold to lower ones, assume infinite sequences
    -- Otherwise the the first X pulses would be absent
    trefl_start = ceiling (2 * distance / 3e8 * fSampling) `mod` nb 
    trefl_stop  = ceiling (2 * distance / 3e8 + pulseWidth * fSampling) `mod` nb
    range_bin   = ceiling (t' * fSampling) `mod` nb

    -- Handling for distances at the edge of the
    crossing_reflection = trefl_stop < trefl_start

    -- The initial phase of a full data cube will be random
    phi_start = 2 * pi * radix / 360

    -- channelDelay :: Integer -> Double
    channelDelay = (-1) * i' * pi * sin angle
    bigI  =        bigA * cos (wd * t' + phi_start)
    bigQ  = (-1) * bigA * sin (wd * t' + phi_start)
    value = (bigI :+ bigQ) * (cos channelDelay :+ sin channelDelay)
