{-# LANGUAGE PostfixOperators, PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.CT.Interface where

import ForSyDe.Atom.MoC ((-.-), (-*-))
import ForSyDe.Atom.MoC.CT.Core as CT
import ForSyDe.Atom.MoC.DE.Core as DE
import ForSyDe.Atom.MoC.DE.Interface (toCT1)
import ForSyDe.Atom.MoC.Stream (Stream(..))
import ForSyDe.Atom.MoC.Time (Time(..))
import "this" ForSyDe.Atom.MoC.TimeStamp
-- import qualified ForSyDe.Atom.Skeleton.Vector as V (
--   Vector, zipx, unzipx, fanout, unit, length, vector)
import ForSyDe.Atom.Utility (($$),($$$),($$$$))

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import ForSyDe.Atom.MoC.DE.Lib as DE
-- >>> import ForSyDe.Atom.MoC.CT.Lib as CT
-- >>> import qualified Data.Number.FixedFunctions as RatF
-- >>> let pi'  = realToFrac pi
-- >>> let sin' = RatF.sin 0.001
-- >>> let cos' = RatF.cos 0.001


------- MoC INTERFACES -------

-- | Translates a (set of) 'ForSyDe.Atom.MoC.CT.CT' signal(s) into
-- 'ForSyDe.Atom.MoC.DE.DE' semantics without loss of information. In
-- 'ForSyDe.Atom.MoC.DE.DE', the abstract function of time inferred by
-- the 'ForSyDe.Atom.MoC.CT.CT' event loses its abstraction and it is
-- "dropped" to explicit form, under a lower layer. In other words the
-- implicit time semantics are lost, the carried value simply becoming
-- an ordinary function.
--
-- Constructors: @toDE[1-4]@.
--
-- <<fig/moc-ct-tode.png>>
toDE1 :: TimeStamp t => CT.Signal t a -> DE.Signal t (Time -> a)
toDE2 :: TimeStamp t => (CT.Signal t a, CT.Signal t b)
      -> (DE.Signal t (Time -> a), DE.Signal t (Time -> b))
toDE3 :: TimeStamp t => (CT.Signal t a, CT.Signal t b, CT.Signal t c)
      -> (DE.Signal t (Time -> a), DE.Signal t (Time -> b), DE.Signal t (Time -> c))
toDE4 :: TimeStamp t => (CT.Signal t a, CT.Signal t b, CT.Signal t c, CT.Signal t d)
      -> (DE.Signal t (Time -> a), DE.Signal t (Time -> b), DE.Signal t (Time -> c), DE.Signal t (Time -> d))


toDE1 = fmap (\(CT ts p f) -> DE ts (\t -> f (t + p)))
toDE2 = ($$) (toDE1, toDE1)
toDE3 = ($$$) (toDE1, toDE1, toDE1)
toDE4 = ($$$$) (toDE1, toDE1, toDE1, toDE1)

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.CT.CT' signal(s) with a
-- 'ForSyDe.Atom.MoC.DE.DE' carrier which holds the timestamps at
-- which the CT signal must be sampled, and outputs the respective
-- (set of) 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- Constructors: @sampDE[1-4]@.
--
-- >>> let s = CT.infinite (fromRational . sin')
-- >>> let c = DE.generate1 id (pi'/2, 1)
-- >>> takeS 6 $ sampDE1 c s
-- {0.0@0s,1.0@1.570796326794s,1.793238520564752e-12@3.141592653588s,-1.0@4.712388980382s,0.0@6.283185307176s,1.0@7.85398163397s}
--
-- <<fig/moc-ct-sampde.png>>
sampDE2 :: TimeStamp t => DE.Signal t q -- ^ 'ForSyDe.Atom.MoC.DE.DE' timestamp carrier 
        -> CT.Signal t a -- ^ 'ForSyDe.Atom.MoC.CT.CT' input
        -> CT.Signal t b -- ^ 'ForSyDe.Atom.MoC.CT.CT' input
        -> (DE.Signal t a, DE.Signal t b) -- ^ 'ForSyDe.Atom.MoC.DE.DE' outputs
sampDE1 :: TimeStamp t => DE.Signal t q
        -> CT.Signal t a
        -> DE.Signal t a
sampDE3 :: TimeStamp t => DE.Signal t q
        -> CT.Signal t a -> CT.Signal t b -> CT.Signal t c
        -> (DE.Signal t a, DE.Signal t b, DE.Signal t c)
sampDE4 :: TimeStamp t => DE.Signal t q
        -> CT.Signal t a -> CT.Signal t b -> CT.Signal t c -> CT.Signal t d
        -> (DE.Signal t a, DE.Signal t b, DE.Signal t c, DE.Signal t d)

sampDE1 carrier = fmap evalEvent . sync carrier
  where evalEvent e@(CT ts _ _) = DE.DE ts (CT.evalTs ts e)
        sync c s = (\_ x -> x) -.- (toCT1 $ (\_->(\_->())) -.- c) -*- s
sampDE2 c s1 s2       = (sampDE1 c s1, sampDE1 c s2)
sampDE3 c s1 s2 s3    = (sampDE1 c s1, sampDE1 c s2, sampDE1 c s3) 
sampDE4 c s1 s2 s3 s4 = (sampDE1 c s1, sampDE1 c s2, sampDE1 c s3, sampDE1 c s4)


-- -- Towards skeleton layer

-- -- | Synchronizes all the signals contained by a vector and zips them
-- -- into one signal of vectors. It instantiates the
-- -- 'ForSyDe.Atom.Skeleton.Vector.zipx' skeleton.
-- --
-- -- >>> let s1 = CT.signal [(0,const 1), (2,const 2), (6,const 3)]
-- -- >>> let s2 = CT.signal [(0,const 1), (2,const 2), (4,const 3)]
-- -- >>> let v1 = V.vector [s1,s1,s2,s2]
-- -- >>> zipx v1
-- -- {<1,1,1,1>@0s,<2,2,2,2>@2s,<2,2,3,3>@4s,<3,3,3,3>@6s}
-- --
-- -- See 'ForSyDe.Atom.MoC.DE.zipx' from the "ForSyDe.Atom.MoC.DE"
-- -- library for a comprehensive visual example.
-- zipx :: TimeStamp t =>V.Vector (CT.Signal t a) -> CT.Signal t (V.Vector a)
-- zipx = V.zipx (V.fanout (\cat a b -> a `cat` b))

-- -- | Unzips the vectors carried by a signal into a vector of
-- -- signals. It instantiates the 'ForSyCt.Atom.Skeleton.Vector.unzipx'
-- -- skeleton. To avoid infinite recurrence, the user needs to provict
-- -- the length of the output vector.
-- --
-- -- >>> let v1 = V.vector [1,2,3,4]
-- -- >>> let s1 = CT.signal [(0,const v1),(2,const v1),(6,const v1)]
-- -- >>> unzipx 4 s1
-- -- <{4@0s,4@2s,4@6s},{3@0s,3@2s,3@6s},{2@0s,2@2s,2@6s},{1@0s,1@2s,1@6s}>
-- --
-- -- See 'ForSyDe.Atom.MoC.DE.unzipx' from the "ForSyDe.Atom.MoC.DE"
-- -- library for a comprehensive visual example.
-- unzipx :: TimeStamp t => Integer -> CT.Signal t (V.Vector a) -> V.Vector (CT.Signal t a)
-- unzipx = V.unzipx id

-- -- | Same as 'unzipx', but \"sniffs\" the first event to determine the
-- -- length of the output vector. Has unsafe behavior!
-- --
-- -- >>> let v1 = V.vector [1,2,3,4]
-- -- >>> let s1 = CT.signal [(0,const v1),(2,const v1),(6,const v1)]
-- -- >>> unzipx' s1
-- -- <{4@0s,4@2s,4@6s},{3@0s,3@2s,3@6s},{2@0s,2@2s,2@6s},{1@0s,1@2s,1@6s}>
-- unzipx' :: TimeStamp t => CT.Signal t (V.Vector a) -> V.Vector (CT.Signal t a)
-- unzipx' s@(a:-_) = unzipx (V.length $ CT.evalEv a) s
