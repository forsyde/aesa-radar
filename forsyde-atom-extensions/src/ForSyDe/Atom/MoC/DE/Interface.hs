{-# LANGUAGE PostfixOperators, PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.DE.Interface where

import "this"           ForSyDe.Atom.MoC.DE.Lib (sync2, sync3, sync4,comb11)
import                  ForSyDe.Atom.MoC.Stream (Stream(..))
import                  ForSyDe.Atom.MoC.Time as T (Time(..), const)
import "this"           ForSyDe.Atom.MoC.TimeStamp
-- import qualified "this" ForSyDe.Atom.Skeleton.Vector as V (
--   Vector, zipx, unzipx, fanout, unit, length, vector, reverse)
import                  ForSyDe.Atom.Utility

import qualified "this" ForSyDe.Atom.MoC.CT.Core as CT
import qualified "this" ForSyDe.Atom.MoC.DE.Core as DE
import qualified        ForSyDe.Atom.MoC.SY.Core as SY

import                  Prelude hiding ((<>))

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.DE.Lib

------- MoC INTERFACES -------

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.DE.DE' signal(s) an
-- strips off their explicit tags, outputting the equivalent
-- 'ForSyDe.Atom.MoC.SY.SY' signal(s), tupled with an SY signal
-- carrying the timestamps for the synchronization points.
--
-- Constructors: @toSY[1-4]@
--
-- >>> let s1 = DE.infinite 1
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: TimeStamp t =>  DE.Signal t Int
-- >>> toSY2 s1 s2
-- ({0s,2s,6s,8s,9s},{1,1,1,1,1},{1,2,3,4,5})
--
-- <<fig/moc-de-tosy.png>>
toSY2 :: TimeStamp t =>  DE.Signal t a             -- ^ first input DE signal
      -> DE.Signal t b             -- ^ second input DE signal
      -> (SY.Signal t, SY.Signal a, SY.Signal b)
      -- ^ signal carrying timestamps tupled with the two output
      -- 'ForSyDe.Atom.MoC.SY.SY' signals
toSY1  :: TimeStamp t =>  DE.Signal t a
      -> (SY.Signal t, SY.Signal a)
toSY3 :: TimeStamp t =>  DE.Signal t a -> DE.Signal t b -> DE.Signal t c
      -> (SY.Signal t, SY.Signal a, SY.Signal b, SY.Signal c)
toSY4 :: TimeStamp t =>  DE.Signal t a -> DE.Signal t b -> DE.Signal t c -> DE.Signal t d
      -> (SY.Signal t, SY.Signal a, SY.Signal b, SY.Signal c, SY.Signal d)

eventToSY (DE.DE t a) = (SY.SY t, SY.SY a)
toSY1 s1              = (eventToSY <$> s1 |<)
toSY2 s1 s2
  = let (sy1,sy2) = (toSY1,toSY1) $$ sync2 s1 s2
    in  (fst,snd,snd) $$$ (sy1,sy1,sy2) 
toSY3 s1 s2 s3
  = let (sy1,sy2,sy3) = (toSY1,toSY1,toSY1) $$$ sync3 s1 s2 s3
    in  (fst,snd,snd,snd) $$$$ (sy1,sy1,sy2,sy3)  
toSY4 s1 s2 s3 s4  
  = let (sy1,sy2,sy3,sy4) = (toSY1,toSY1,toSY1,toSY1) $$$$ sync4 s1 s2 s3 s4
    in  (fst,snd,snd,snd,snd) $$$$$ (sy1,sy1,sy2,sy3,sy4) 


-- | Semantic preserving transformation between a (set of) DE
-- signal(s) and the equivalent CT signals. The
-- 'ForSyDe.Atom.MoC.DE.DE' events must carry a function of 'Time'
-- which will be lifted by providing it with 'ForSyDe.Atom.MoC.CT.CT'
-- implicit time semantics.
--
-- Constructors: @toCT[1-4]@.
--
-- <<fig/moc-de-toct.png>>
toCT2 :: TimeStamp t
      => DE.Signal t (Time -> a)  -- ^ first input DE signal
      -> DE.Signal t (Time -> b)  -- ^ second input DE signal
      -> (CT.Signal t a, CT.Signal t b)
      -- ^ two output 'ForSyDe.Atom.MoC.CT.CT' signals
eventToCT (DE.DE t a) = CT.CT t 0 a
toCT1 s1          = eventToCT <$> s1
toCT2 s1 s2       = (toCT1, toCT1) $$ (s1,s2)
toCT3 s1 s2 s3    = (toCT1, toCT1, toCT1) $$$ (s1,s2,s3)
toCT4 s1 s2 s3 s4 = (toCT1, toCT1, toCT1, toCT1) $$$$ (s1,s2,s3,s4)

hold1 :: TimeStamp t => DE.Signal t a -> CT.Signal t a
hold1 = toCT1 . comb11 T.const


-- -- Towards skeleton layer

-- -- | Synchronizes all the signals contained by a vector and zips them
-- -- into one signal of vectors. It instantiates the
-- -- 'ForSyDe.Atom.Skeleton.Vector.zipx' skeleton.
-- --
-- -- >>> let s1 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: TimeStamp t =>  DE.Signal t Int
-- -- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@4, 4@8, 5@9}" :: TimeStamp t =>  DE.Signal t Int
-- -- >>> let v1 = V.vector [s1,s1,s2,s2]
-- -- >>> v1
-- -- <{1@0s,2@2s,3@6s,4@8s,5@9s},{1@0s,2@2s,3@6s,4@8s,5@9s},{1@0s,2@2s,3@4s,4@8s,5@9s},{1@0s,2@2s,3@4s,4@8s,5@9s}>
-- -- >>> zipx v1
-- -- {<1,1,1,1>@0s,<2,2,2,2>@2s,<2,2,3,3>@4s,<3,3,3,3>@6s,<4,4,4,4>@8s,<5,5,5,5>@9s}
-- --
-- -- <<fig/moc-de-zipx.png>>
-- zipx :: TimeStamp t => V.Vector (DE.Signal t a) -> DE.Signal t (V.Vector a)
-- zipx = V.zipx (V.fanout (\cat a b -> a `cat` b))

-- -- | Unzips the vectors carried by a signal into a vector of
-- -- signals. It instantiates the 'ForSyDe.Atom.Skeleton.Vector.unzipx'
-- -- skeleton. To avoid infinite recurrence, the user needs to provide
-- -- the length of the output vector.
-- --
-- -- >>> let v1 = V.vector [1,2,3,4]
-- -- >>> let s1 = DE.signal [(0,v1),(2,v1),(6,v1),(8,v1),(9,v1)]
-- -- >>> s1
-- -- {<1,2,3,4>@0s,<1,2,3,4>@2s,<1,2,3,4>@6s,<1,2,3,4>@8s,<1,2,3,4>@9s}
-- -- >>> unzipx 4 s1
-- -- <{1@0s,1@2s,1@6s,1@8s,1@9s},{2@0s,2@2s,2@6s,2@8s,2@9s},{3@0s,3@2s,3@6s,3@8s,3@9s},{4@0s,4@2s,4@6s,4@8s,4@9s}>
-- --
-- -- <<fig/moc-de-unzipx.png>>
-- unzipx :: TimeStamp t =>  Integer -> DE.Signal t (V.Vector a) -> V.Vector (DE.Signal t a)
-- unzipx n = V.reverse . V.unzipx id n

-- -- | Same as 'unzipx', but \"sniffs\" the first event to determine the length of the output vector. Might have unsafe behavior!
-- --
-- -- >>> let v1 = V.vector [1,2,3,4]
-- -- >>> let s1 = DE.signal [(0,v1),(2,v1),(6,v1),(8,v1),(9,v1)]
-- -- >>> s1
-- -- {<1,2,3,4>@0s,<1,2,3,4>@2s,<1,2,3,4>@6s,<1,2,3,4>@8s,<1,2,3,4>@9s}
-- -- >>> unzipx' s1
-- -- <{1@0s,1@2s,1@6s,1@8s,1@9s},{2@0s,2@2s,2@6s,2@8s,2@9s},{3@0s,3@2s,3@6s,3@8s,3@9s},{4@0s,4@2s,4@6s,4@8s,4@9s}>
-- unzipx' :: TimeStamp t =>  DE.Signal t (V.Vector a) -> V.Vector (DE.Signal t a)
-- unzipx' s@(a:-_) = unzipx (V.length $ DE.val a) s

