{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE PostfixOperators #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT
-- Copyright   :  (c) George Ungureanu, 2016-2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @CT@ library implements the execution semantics for the atoms
-- operating according to the continuous time model of
-- computation. This module also provides a set of helpers for
-- instantiating the MoC layer patterns described in the
-- "ForSyDe.Atom.MoC" module as meaningful continuous time process
-- constructors.
--
-- For an overview about atoms, layers and patterns, please refer to
-- the "ForSyDe.Atom" module documentation, and for an overview of the
-- MoC layer entities refer to <ForSyDe-Atom.html#g:3 the MoC layer section>.
-- For working with time or timestamps please check the utilities
-- provided by the "ForSyDe.Atom.MoC.Time" and
-- "ForSyDe.Atom.MoC.TimeStamp" modules.
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
----------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT (

  -- * Continuous time (@CT@) event

  -- | According to <ForSyDe-Atom.html#lee98 [Lee98]>, "[regarding
  -- metric time] at a minimum, \(T\) is an Abelian group, in addition
  -- to being totally ordered. A consequence is that \(t_2-t_1\) is
  -- itself a tag \(\forall t_1,t_2 \in T\). In a slightly more
  -- elaborate model of computation, \(T\) has a metric. (...) A
  -- continuous-time system is a metric timed system \(Q\) where \(T\)
  -- is a continuum (a closed connected set)."
  --
  -- The continuous time (@CT@) MoC defines the closest behavior to
  -- what we could call "physical time", where signals cover the full
  -- span of a simulation as "functions of time" rather than
  -- "values". As such, we can state:
  --
  -- [The CT MoC] is abstracting the execution semantics and describes
  --   a system where computation is performed continuously over a
  --   (possibly infinite) span of time.
  --
  -- For a detailed descrption of the ForSyDe-Atom CT execution model,
  -- refer to <ForSyDe-Atom.html#ungureanu18 [Ungureanu18]>. Below is
  -- an illustration of the behavior in time of the input and the
  -- output signals of a CT process:
  --
  -- <<fig/moc-ct-example.png>>
  --
  -- Our 'ForSyDe.MoC.CT.CT' MoC is implemented as a generalized
  -- version of ForSyDe-Atom's 'ForSyDe.MoC.DE.DE' with respect to the
  -- CT MoC definition, or rather the 'ForSyDe.MoC.DE.DE' MoC is a
  -- special case of 'ForSyDe.MoC.CT.CT' in the sense that:
  --
  -- 1. tags \(t\) are represented with 'ForSyDe.Atom.MoC.TimeStamp's
  --    just like in 'ForSyDe.MoC.DE.DE'. As such, we can say that
  --    changes in a CT signal happen, or rather are observed, at
  --    discrete times (see below).
  --
  -- 1. values are represented as /functions/ over a continuous span
  --    of time \(f(t)\) rather than, like most commercial simulators,
  --    series of sampled values. The time domain is abstracted away,
  --    and may be represented with rational numbers which, as
  --    compared to floating point numbers, do not suffer from
  --    inherent quantisation, and are the closest numerical
  --    representation which can model continuity.
  --
  -- 1. for practical reasons, the 'CT' event constructor has also a
  --    /phase/ component \(\phi\), which is taken into consideration
  --    only when evaluating the event function,
  --    i.e. \(f(t+\phi)\). This enables the modeling of "phase
  --    dispacements" of delay lines without altering the function
  --    itself, i.e. without increasing the complexity of the
  --    un-evaluated functions (redexes). The phase is reset during
  --    event synchronization.
  --
  -- Based on the these particularities we can say that the 'CT' MoC
  -- is simply a 'ForSyDe.Atom.DE.DE' machine/observer operating on
  -- continuous subdomains, and we can formulate the following
  -- properties:
  --
  -- <<fig/misc-ct-model.png>>
  --
  -- 1. 'CT' signals, due to their formation as streams of tagged
  --    events, represent /discrete/ changes in a continuous function
  --    over time (e.g. analog signal). While the functions carried by
  --    events are infinite (have always happened and will always
  --    happen), being carried by events in a tag system suggests that
  --    changes occur at discrete times. A CT signal can be
  --    represented by the analog circuit above, where the inputs are
  --    continuous signals, but the switch is discrete. Like in the
  --    'ForSyDe.MoC.DE.DE' MoC, the absolute time \(0\) represent the
  --    time when the system started to be observed.
  --
  -- 1. the previous property is also proven by the fact that the
  --    evaluation engine of ForSyDe-Atom is inherently a discrete
  --    machine, i.e. evaluation is performed whenever a new event
  --    occurs, in a dataflow manner. Allowing infinitely small
  --    distances between tags would not allow the advancement of
  --    simulation time.
  --
  -- 1. events carry /functions/ and not /values/. In a lazy
  --    evaluation system like Haskell's, functions are kept symbolic
  --    until evaluation. This means that in a CT system computations
  --    are propagated as function graphs until a result is needed,
  --    e.g. a signal needs to be plotted for arbitrary positions in
  --    time. This way intermediate quantization errors are
  --    eliminated, and the cost of higher plot resolution is the cost
  --    of evaluating the final results only.
  --
  -- 1. needless to say, for each \(t \in T\), a signal is able to
  --    return (e.g. plot) the exact value \(t\) for that particular
  --    \(t\).
  --
  -- 1. since itself the 'ForSyDe.MoC.CT.CT' MoC is simply a
  --    'ForSyDe.MoC.DE.DE' system operating on continuous subdomains,
  --    all atom evaluation properties are inherited from it: feedback
  --    loops need to advance time, atoms are forbidden to clean
  --    signals, and the conservative approach makes it ideal for
  --    parallel/distributed simulation.
  --
  -- 1. since /T/ is a total order, there is no need for an
  --    <ForSyDe-Atom-MoC.html#context execution context> and we can
  --    ignore the formatting of functions in "ForSyDe.Atom.MoC", thus
  --    we can safely assume:
  --
  -- <<fig/eqs-moc-timed-context.png>>

  TimeStamp, Time, CT(..),

  -- * Aliases & utilities

  -- | A set of type synonyms and utilities are provided for
  -- convenience. The API type signatures will feature these aliases
  -- to hide the cumbersome construction of atoms and patters as seen
  -- in "ForSyDe.Atom.MoC".

  Signal, unit, unit2, unit3, unit4, infinite,
  signal, checkSignal, 

  -- plot, plot2, plot3, plot4,
  -- plotFloat, plotFloat2, plotFloat3, plotFloat4,
  
  -- * @CT@ process constuctors

  -- | The CT process constructors are basically specific
  -- instantiations of patterns defined in "ForSyDe.Atom.MoC". 
  --
  -- In the examples below we have imported and instantiated the
  -- functions such as @e'@ @pi'@, @sin'@ and @cos'@ from the
  -- collection of utilities in "ForSyDe.Atom.MoC.Time" and
  -- "ForSyDe.Atom.MoC.TimeStamp". Also, for the sake of documentation
  -- the interactive examples are only dumping the CT signals in data
  -- files using the 'dumpDat' utility defined in
  -- "ForSyDe.Atom.Utility.Plot", according to the custom @cfg@
  -- structure. These files can be further plotted by any tool of
  -- choice, or using the plotting utilities provided in the
  -- "ForSyDe.Atom.Utility.Plot" module.
  --
  -- > import ForSyDe.Atom.Utility.Plot
  -- > import ForSyDe.Atom.MoC.Time as Time
  -- > import ForSyDe.Atom.MoC.TimeStamp as TimeStamp
  -- > let pi'  = TimeStamp.pi
  -- > let exp' = Time.exp
  -- > let sin' = Time.sin
  -- > let cos' = Time.cos
  -- > let cfg  = defaultCfg {xmax=10, rate=0.1}

  -- ** Simple

  -- | These are mainly direct instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC", using CT-specific utilities.

  delay, delay',
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  reconfig11, reconfig12, reconfig13, reconfig14,
  reconfig21, reconfig22, reconfig23, reconfig24,
  reconfig31, reconfig32, reconfig33, reconfig34,
  reconfig41, reconfig42, reconfig43, reconfig44,

  constant1, constant2, constant3, constant4,
  infinite1, infinite2, infinite3, infinite4,

  generate1, generate2, generate3, generate4,

  stated11, stated12, stated13, stated14,
  stated21, stated22, stated23, stated24,
  stated31, stated32, stated33, stated34,
  stated41, stated42, stated43, stated44,

  state11, state12, state13, state14,
  state21, state22, state23, state24,
  state31, state32, state33, state34,
  state41, state42, state43, state44,

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,

  -- ** Interfaces

  toDE1, toDE2, toDE3, toDE4,
  sampDE1, sampDE2, sampDE3, sampDE4,
  -- zipx, unzipx, unzipx'
  
  ) where

import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.MoC.CT.Core
import ForSyDe.Atom.MoC.CT.Lib
import ForSyDe.Atom.MoC.CT.Interface
