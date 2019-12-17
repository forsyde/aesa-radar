-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.TimeStamp
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements a timestamp data type, based on
-- "Data.Time.Clock". 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.TimeStamp where

class (Num t, Eq t, Ord t, Real t) => TimeStamp t where
  zero :: t
  zero = 0

  eq :: t -> t -> Bool
  eq = (==)
  
  lt :: t -> t -> Bool
  lt = (<)

  gt :: t -> t -> Bool
  gt = (>)
