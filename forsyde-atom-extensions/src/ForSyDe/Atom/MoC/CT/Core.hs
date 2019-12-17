{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs, StandaloneDeriving, PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the CT MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT.Core where

import Data.Ratio
import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility


-- | Type synonym for a CT signal, i.e. "a signal of CT events"
type Signal t a = Stream (CT t a)

-- | The CT type, identifying a continuous time event and implementing an
-- instance of the 'MoC' class.
data CT t a where
  CT :: TimeStamp t => { tag   :: t
                       -- ^ start time of event
                       , phase :: Time
                       -- ^ phase. Models function delays
                       , func  :: Time -> a
                       -- ^ function of time
                       } -> CT t a

-- | Implenents the execution semantics for the CT MoC atoms.
instance TimeStamp t => MoC (CT t) where
  ---------------------
  type Fun (CT t) a b = a -> b
  type Ret (CT t) b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs)           =      f  <*> x  :- comb f  x  fs xs
    where
      comb pf px s1@(f :- fs) s2@(x :- xs)
        | tag f `eq` tag x      = f %> f  <*> x  :- comb f  x  fs xs
        | tag f `lt` tag x      = f %> f  <*> px :- comb f  px fs s2
        | tag f `gt` tag x      = x %> pf <*> x  :- comb pf x  s1 xs
      comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f px fs NullS
      comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x NullS xs
      comb _ _ NullS NullS      = NullS
  ---------------------
  (-*) = id
  ---------------------
  (CT _ p v :- _) -<- xs = (CT zero p v) :- xs
  ---------------------
  (_ :- CT d _ _ :- _) -&- xs
    = (\(CT t p v) -> CT (t + d) (p - toRational d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.CT] signal delayed to infinity"
  ---------------------
    
-- | Allows for mapping of functions on a CT event.
instance TimeStamp t => Functor (CT t) where
  fmap f (CT t p g) = CT t p (f . g)

-- | Allows for lifting functions on a pair of CT events.
instance TimeStamp t => Applicative (CT t) where
  pure x = CT zero 0 (\_ -> x)
  (CT t p1 f) <*> (CT _ p2 g) = CT t 0 (\x -> (f (x+p1)) (g (x+p2)))

-- | Meant for debug purpose only! It evaluates /only/ the signal at
-- the start time.
instance (Show a, Show t, TimeStamp t) => Show (CT t a) where
  showsPrec _ e
    = (++) ( show (evalTs (tag e) e) ++
             "@" ++ show (tag e) )

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(CT t _ _) %> (CT _ p x) = CT t p x

-- end of testbench functions
-----------------------------------------------------------------------------

evalTm t (CT _ p f) = f (t + p)
evalTs t (CT _ p f) = f ((toRational t) + p)
evalEv (CT t p f) = f ((toRational t) + p)
                   
unit  :: TimeStamp t => (t, Time -> a) -> Signal t a 
unit2 :: TimeStamp t
      => ((t, Time -> a1), (t, Time -> a2))
      -> (Signal t a1, Signal t a2)
unit3 :: TimeStamp t
      => ((t, Time -> a1), (t, Time -> a2), (t, Time -> a3))
      -> (Signal t a1, Signal t a2, Signal t a3)
unit4 :: TimeStamp t
      => ((t, Time -> a1), (t, Time -> a2), (t, Time -> a3), (t, Time -> a4))
      -> (Signal t a1, Signal t a2, Signal t a3, Signal t a4)
      
unit (t,f) = (CT zero 0 f :- CT t 0 f :- NullS)

-- | Wraps a (tuple of) pair(s) @(time, function)@ into the equivalent
-- unit signal(s), i.e. signal(s) with one event with the period
-- @time@ carrying @function@.
--
-- Helpers: @unit@ and @unit[2-4]@.
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: TimeStamp t => (Time -> a) -> Signal t a
infinite f = CT zero 0 f :- NullS

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a CT signal
signal :: TimeStamp t => [(t, Time -> a)] -> Signal t a
signal = checkSignal . stream . fmap (\(t, f) -> CT t 0 f)

-- | Checks if a signal is well-formed or not, according to the CT MoC
-- interpretation in @ForSyDe-Atom@.
checkSignal NullS = NullS
checkSignal s@(x:-_)
  | tag x `eq` zero = checkOrder s
  | otherwise  = error "[MoC.CT] signal does not tag from global 0"
  where
    checkOrder NullS      = NullS
    checkOrder (x:-NullS) = (x:-NullS)
    checkOrder (x:-y:-xs)
      | tag x < tag y = x :-checkOrder (y:-xs)
      | otherwise = error "[MoC.CT] malformed signal"

-- | Returns a stream with the results of evaluating a signal with a
-- given sampling period.
plot :: TimeStamp t => t  -- ^ sample period
     -> t  -- ^ end of plotting
     -> Signal t a   -- ^ input CT signal
     -> Stream a   -- ^ stream with evaluation results
plot step until = evalPlot zero
  where
    evalPlot t s@(x:-y:-xs)
      | t >= until = NullS
      | tag y > t  = evalTs t x :- evalPlot (t + step) s
      | otherwise  = evalPlot t (y:-xs)
    evalPlot t s@(x:-NullS)
      | t >= until = NullS
      | otherwise  = evalTs t x :- evalPlot (t + step) s
plot2 s u = ($$)   (plot s u, plot s u)
plot3 s u = ($$$)  (plot s u, plot s u, plot s u)
plot4 s u = ($$$$) (plot s u, plot s u, plot s u, plot s u)

-- | Same as 'plot', but also converts rational outputs to floats
-- (which are easier to be read by drawing engines).
plotFloat :: TimeStamp t => RealFloat b
          => t     -- ^ sample period
          -> t     -- ^ end of plotting
          -> Signal t Time   -- ^ CT signal with a 'Time' codomain
          -> Stream b      -- ^ plot output in floating point format
plotFloat  s u = fmap  fromRational . plot s u
plotFloat2 s u = ($$)   (plotFloat s u, plotFloat s u)
plotFloat3 s u = ($$$)  (plotFloat s u, plotFloat s u, plotFloat s u)
plotFloat4 s u = ($$$$) (plotFloat s u, plotFloat s u, plotFloat s u, plotFloat s u)


