{-# LANGUAGE PackageImports #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.Time
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of utility functions for working with 'Time'. While the
-- 'ForSyDe.Atom.MoC.CT.CT' MoC describes time as being a non-disjoint
-- continuum (represented in ForSyDe-Atom with 'Rational' numbers),
-- most of the functions here are non-ideal approximations or
-- conversions from floating point equivalents. The trigonometric
-- functions are imported from the
-- <https://hackage.haskell.org/package/numbers-3000.2.0.2 numbers>
-- package, with a fixed @eps@ parameter.
--
-- These utilities are meant to get started with using the CT MoC, and
-- should be used with caution if result fidelity is a requirement. In
-- this case the user should find a native 'Rational' implementation
-- for a particular function.
----------------------------------------------------------------------
module ForSyDe.Atom.MoC.Time where

import "this" ForSyDe.Atom.MoC.TimeStamp

import qualified Data.Number.FixedFunctions as RatF

-- | Type alias for the type to represent metric (continuous)
-- time. Underneath we use 'Rational' that is able to represent any
-- \(t\) between \(t_1 < t_2 \in T\).
type Time = Rational

-- | Converts 'TimeStamp' into 'Time' representation.
time :: TimeStamp t => t -> Time
time = toRational

-- | Returns a constant function.
const :: a -> (Time -> a) 
const v = (\_ -> v)

-- | Euler's number in 'Time' format. Converted from the "Prelude"
-- equivalent, which is 'Floating'.
e :: Time
e = toRational (Prelude.exp 1)

-- | "Power of" function taking 'Time's as arguments. Converts back
-- and forth to 'Floating', as it uses the 'Prelude.**' operator, so
-- it is prone to conversion errors.
(*^*) :: Time -> Time -> Time
x *^* y = realToFrac $ realToFrac x ** realToFrac y


-- FROM Data.Number.FixedFunctions


-- -- | 'Time' representation of the number &#960;. Rational
-- -- representation with a precision of @0.000001@.
-- pi :: Time
-- pi = RatF.pi 0.000001

-- -- | Sine of 'Time'. Rational representation with a precision of
-- -- @0.000001@.
-- sin :: Time -> Time
-- sin = RatF.sin 0.000001

-- -- | Cosine of 'Time'. Rational representation with a precision of
-- -- @0.000001@.
-- cos :: Time -> Time
-- cos = RatF.cos 0.000001

-- -- | Tangent of 'Time'. Rational representation with a precision of
-- -- @0.000001@.
-- tan :: Time -> Time
-- tan = RatF.tan 0.000001

-- -- | Arctangent of 'Time'. Rational representation with a precision
-- -- of @0.000001@.
-- atan :: Time -> Time
-- atan = RatF.atan 0.000001

-- -- | Arcsine of 'Time'. Rational representation with a precision of
-- -- @0.000001@.
-- asin :: Time -> Time
-- asin = RatF.asin 0.000001

-- -- | Arccosine of 'Time'. Rational representation with a precision
-- -- of @0.000001@.
-- acos :: Time -> Time
-- acos = RatF.acos 0.000001

-- -- | Square root of 'Time'. Rational representation with a precision
-- -- of @0.000001@.
-- sqrt :: Time -> Time
-- sqrt = RatF.sqrt 0.000001

-- -- | Exponent of 'Time'. Rational representation with a precision of
-- -- @0.000001@.
-- exp :: Time -> Time
-- exp = RatF.exp 0.000001

-- -- | Hyperbolic cosine of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- cosh :: Time -> Time
-- cosh = RatF.cosh 0.000001

-- -- | Hyperbolic sine of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- sinh :: Time -> Time
-- sinh = RatF.sinh 0.000001

-- -- | Hyperbolic tangent of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- tanh :: Time -> Time
-- tanh = RatF.tanh 0.000001

-- -- | Hyperbolic arctangent of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- atanh :: Time -> Time
-- atanh = RatF.atanh 0.000001

-- -- | Hyperbolic arcsine of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- asinh :: Time -> Time
-- asinh = RatF.asinh 0.000001

-- -- | Hyperbolic arccosine of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- acosh :: Time -> Time
-- acosh = RatF.acosh 0.000001

-- -- | Natural logarithm of 'Time'. Rational representation with a
-- -- precision of @0.000001@.
-- log :: Time -> Time
-- log = RatF.log 0.000001
