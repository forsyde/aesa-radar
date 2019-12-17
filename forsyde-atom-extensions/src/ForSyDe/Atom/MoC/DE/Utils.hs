
module ForSyDe.Atom.MoC.DE.Utils where

import ForSyDe.Atom.MoC.SY.Interface as SY
import ForSyDe.Atom.MoC.SY.Core as SY
import ForSyDe.Atom.MoC.SY.Lib as SY
import ForSyDe.Atom.MoC.DE.Core as DE
import ForSyDe.Atom.MoC.DE.Lib as DE
import ForSyDe.Atom.MoC.DE.Interface
import ForSyDe.Atom.MoC.TimeStamp

import System.Random as R
import Data.Random.Normal as N

timeStamps :: TimeStamp t => DE.Signal t a -> DE.Signal t t
timeStamps sig    = SY.toDE1 tsSig tsSig
  where (tsSig,_) = toSY1 sig

pwm :: (Num a, TimeStamp t) => t -> t -> DE.Signal t a
pwm width period = DE.mealy11 ns od ini $ DE.signal [(0,1),(width,0)]
  where
    ns (True, _) a = (False,a)
    ns (False,s) _ = (False,s)
    od s = snd . ns s
    ini = (period, (True,0))

embedSY11 :: TimeStamp t
          => (SY.Signal a1 -> SY.Signal b1)
          -> DE.Signal t a1 -- ^ first input DE signal
          -> DE.Signal t b1
          
embedSY11 syproc de1 = let (ts, sy1) = toSY1 de1
                       in  SY.toDE1 ts $     syproc sy1 

random :: (Random b, TimeStamp t) => StdGen -> DE.Signal t a -> DE.Signal t b
random gen = embedSY11 (SY.moore11 (\s _ -> tail s) head (randoms gen))

randomR :: (Random b, TimeStamp t) => (b,b) -> StdGen -> DE.Signal t a -> DE.Signal t b
randomR range gen = embedSY11
                    (SY.moore11 (\s _ -> tail s) head (randomRs range gen))

normal :: (Random b, Floating b, TimeStamp t) => StdGen -> DE.Signal t a -> DE.Signal t b
normal gen = embedSY11 (SY.moore11 (\s _ -> tail s) head (normals gen))

normalR :: (Random b, Floating b, TimeStamp t) => (b,b) -> StdGen -> DE.Signal t a -> DE.Signal t b
normalR param gen = embedSY11
                    (SY.moore11 (\s _ -> tail s) head (normals' param gen))
