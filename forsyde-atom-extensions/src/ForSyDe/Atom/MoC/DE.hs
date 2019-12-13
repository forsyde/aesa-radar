{-# LANGUAGE PackageImports #-}
module ForSyDe.Atom.MoC.DE (
  module DE,
  timeStamps, pwm,
  ForSyDe.Atom.MoC.DE.random, ForSyDe.Atom.MoC.DE.randomR,
  ForSyDe.Atom.MoC.DE.normal, ForSyDe.Atom.MoC.DE.normalR
  ) where

import "forsyde-atom" ForSyDe.Atom.MoC.SY as SY
import "forsyde-atom" ForSyDe.Atom.MoC.DE as DE
import "forsyde-atom" ForSyDe.Atom.MoC.TimeStamp as Ts

import System.Random as R
import Data.Random.Normal as N

timeStamps :: DE.Signal a -> DE.Signal Ts.TimeStamp
timeStamps sig    = SY.toDE tsSig tsSig
  where (tsSig,_) = DE.toSY1 sig

pwm :: Num a => Ts.TimeStamp -> Ts.TimeStamp -> DE.Signal a
pwm width period = DE.mealy11 ns od ini $ DE.signal [(0,1),(width,0)]
  where
    ns (True, _) a = (False,a)
    ns (False,s) _ = (False,s)
    od s = snd . ns s
    ini = (period, (True,0))

random :: Random b => StdGen -> DE.Signal a -> DE.Signal b
random gen = DE.embedSY11 (SY.moore11 (\s _ -> tail s) head (randoms gen))

randomR :: Random b => (b,b) -> StdGen -> DE.Signal a -> DE.Signal b
randomR range gen = DE.embedSY11
                    (SY.moore11 (\s _ -> tail s) head (randomRs range gen))

normal :: (Random b, Floating b) => StdGen -> DE.Signal a -> DE.Signal b
normal gen = DE.embedSY11 (SY.moore11 (\s _ -> tail s) head (normals gen))

normalR :: (Random b, Floating b) => (b,b) -> StdGen -> DE.Signal a -> DE.Signal b
normalR param gen = DE.embedSY11
                    (SY.moore11 (\s _ -> tail s) head (normals' param gen))
