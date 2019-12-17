{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs, StandaloneDeriving, PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Core where

import        ForSyDe.Atom.MoC
import        ForSyDe.Atom.MoC.Stream
import "this" ForSyDe.Atom.MoC.TimeStamp
import        ForSyDe.Atom.Utility (($$),($$$),($$$$))

import        Prelude hiding (until)

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Signal t a   = Stream (DE t a)

-- | The DE event. It identifies a discrete event signal.
data DE t a where
  DE :: TimeStamp t => { tag :: t,  -- ^ timestamp
                         val :: a   -- ^ the value
                       } -> DE t a
deriving instance (TimeStamp t, Eq a) => Eq (DE t a) 

-- | Implenents the execution semantics for the DE MoC atoms.
instance TimeStamp t => MoC (DE t) where
  type Fun (DE t) a b = a -> b
  type Ret (DE t) b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs)               =      f  <*> x  :- comb f  x  fs xs
    where comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f `eq` tag x      = f %> f  <*> x  :- comb f  x  fs xs
            | tag f `lt` tag x      = f %> f  <*> px :- comb f  px fs s2
            | tag f `gt` tag x      = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f  px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x  NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (-*) = id
  ---------------------
  (DE _ v :- _) -<- xs = pure v :- xs
  ---------------------
  (_ :- DE d _ :- _) -&- xs = (\(DE t v) -> DE (t + d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.DE] signal delayed to infinity"
  ---------------------

-- | Shows the event with tag @t@ and value @v@ as @v\@t@.
instance (Show a, Show t) => Show (DE t a) where
  showsPrec _ (DE t x) = (++) ( show x ++ "@" ++ show t )

-- | Reads the string of type @v\@t@ as an event @DE t v@.
instance (Read a, Read t, TimeStamp t) => Read (DE t a) where
  readsPrec _ x = [ (DE tg val, r2)
                  | (val,r1) <- reads $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]

-- | Allows for mapping of functions on a DE event.
instance TimeStamp t => Functor (DE t) where
  fmap f (DE t a) = DE t (f a)

-- | Allows for lifting functions on a pair of DE events.
instance TimeStamp t => Applicative (DE t) where
  pure = DE zero
  (DE tf f) <*> (DE _ x) = DE tf (f x)

-----------------------------------------------------------------------------

unit  :: TimeStamp t => (t, a) -> Signal t a
unit2 :: TimeStamp t
      => ((t, a1), (t, a2))
      -> (Signal t a1, Signal t a2)
unit3 :: TimeStamp t
      => ((t, a1), (t, a2), (t, a3))
      -> (Signal t a1, Signal t a2, Signal t a3)
unit4 :: TimeStamp t
      => ((t, a1), (t, a2), (t, a3), (t, a4))
      -> (Signal t a1, Signal t a2, Signal t a3, Signal t a4)

unit (t,v) = (DE zero v :- DE t v :- NullS)

-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent
-- unit signal(s). A unit signal is a signal with one event with the
-- period @tag@ carrying @value@.
--
-- Helpers: @unit@ and @unit[2-4]@.
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: TimeStamp t => a -> Signal t a
infinite v = DE zero v :- NullS

-- | Transforms a list of tuples @(tag, value)@ into a DE
-- signal. Checks if it is well-formed.
signal :: TimeStamp t => [(t, a)] -> Signal t a
signal = checkSignal . stream . fmap (\(t, v) -> DE t v)

-- | Takes the first part of the signal util a given timestamp. The
-- last event of the resulting signal is at the given timestamp and
-- carries the previous value. This utility is useful when plotting
-- a signal, to specify the interval of plotting.
until :: TimeStamp t => t -> Signal t a -> Signal t a
until _ NullS = NullS
until u (DE t v:-NullS)
  | t < u     = DE t v :- DE u v :- NullS
  | otherwise = DE u v :- NullS
until u (DE t v:-xs)
  | t < u     = DE t v :- until u xs
  | otherwise = DE u v :- NullS

-- | Reads a signal from a string and checks if it is well-formed.
-- Like with the @read@ function from @Prelude@, you must specify the
-- type of the signal.
--
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- {1@0s,2@2s,3@5s,4@7s,5@10s}
--
-- Incorrect usage (not covered by @doctest@):
--
-- > λ> readSignal "{ 1@0, 2@2, 3@5, 4@10, 5@7 }" :: Signal Int
-- > {1@0s,2@2s,3@5s*** Exception: [MoC.DE] malformed signal
-- > λ> readSignal "{ 1@1, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- > *** Exception: [MoC.DE] signal does not start from global 0
readSignal :: (Read a, Read t, TimeStamp t) => String -> Signal t a
readSignal s = checkSignal $ read s

-- | Checks if a signal is well-formed or not, according to the DE MoC
-- interpretation in ForSyDe-Atom.
checkSignal NullS = NullS
checkSignal s@(x:-_)
  | tag x == 0 = checkOrder s
  | otherwise  = error "[MoC.DE] signal does not start from global 0"
  where
    checkOrder NullS      = NullS
    checkOrder (x:-NullS) = (x:-NullS)
    checkOrder (x:-y:-xs) | tag x < tag y = x :-checkOrder (y:-xs)
                          | otherwise = error "[MoC.DE] malformed signal"

----------------------------------------------------------------------------- 
-- These functions are not exported and are used internally.

infixl 7 %>
(DE t _) %> (DE _ x) = DE t x

-- end of testbench functions
-----------------------------------------------------------------------------
