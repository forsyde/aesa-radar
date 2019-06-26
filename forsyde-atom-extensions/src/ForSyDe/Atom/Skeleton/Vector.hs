{-# LANGUAGE PackageImports #-}

-- module ForSyDe.Atom.Skeleton.Vector(
--   module V,
--   length, rotate, take, drop, get, (<@), (<@!), indexes, group, indexesTo, stencil, zip, unzip
-- ) where

-- import "forsyde-atom" ForSyDe.Atom.Skeleton.Vector as V hiding (
--   length, take, drop, get, (<@), (<@!), indexes, group, duals, unduals
--   )

module ForSyDe.Atom.Skeleton.Vector where

-- import "forsyde-atom" ForSyDe.Atom.Skeleton as S
import ForSyDe.Atom.Utility 
import Data.Maybe
import Control.Applicative
import Data.List.Split
import qualified Data.List as L
import Prelude hiding (take, drop, length, zip, unzip)


newtype Vector a = Vector { fromVector :: [a] } deriving (Eq)

vector = Vector

instance Functor Vector where
  fmap f (Vector a) = Vector (fmap f a)

instance Applicative Vector where
  pure a = Vector [a]
  (Vector fs) <*> (Vector as) = Vector $ getZipList (ZipList fs <*> ZipList as) 

instance (Show a) => Show (Vector a) where
  showsPrec p (Vector []) = showParen (p > 9) (showString "<>")
  showsPrec p (Vector xs) = showParen (p > 9) (showChar '<' . showVector1 xs)
    where
      showVector1 [] = showChar '>'            
      showVector1 (y:[]) = shows y . showChar '>'
      showVector1 (y:ys) = shows y . showChar ',' 
        . showVector1 ys

instance Foldable Vector where
  foldr f x (Vector a) = foldr f x a

farm11 f = fmap f
farm21 f a b = f <$> a <*> b
farm31 f a b c = f <$> a <*> b <*> c
farm41 f a b c d = f <$> a <*> b <*> c <*> d
farm51 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
farm12 f = (|<) . fmap f
farm22 f a = (|<) . farm21 f a

infixr 5 <++>
(Vector a) <++> (Vector b) = Vector (a ++ b)

unsafeApply f (Vector a) = f a
unsafeLift  f (Vector a) = Vector (f a)

reduce f = unsafeApply (L.foldr1 f)

length = unsafeApply (L.length)

drop n = unsafeLift (L.drop n)
take n = unsafeLift (L.take n)
first = unsafeApply L.head

group :: Int -> Vector a -> Vector (Vector a)
group n (Vector a) = vector $ map vector $ chunksOf n a

fanout    = vector . L.repeat
fanoutn n = vector . L.replicate n

stencil n v = farm11 (take n) $ dropFromEnd n $ tails v
  where dropFromEnd n = take (length v - n + 1)

tails = unsafeLift (L.init . map vector . L.tails)

concat = unsafeLift (L.concat . map fromVector)

iterate n f i = vector $ L.take n $ L.iterate f i 

bitrev = unsafeLift bitrevF
  where
    bitrevF [x] = [x]
    bitrevF xs  = bitrevF (evensF xs) ++ bitrevF (oddsF xs)

evensF [] = []
evensF [x] = [x]
evensF (x:_:xs) = x:evensF xs
oddsF  [] = []
oddsF  [_] = []
oddsF  (_:y:xs) = y:oddsF xs

evens = unsafeLift evensF
odds = unsafeLift oddsF

pipe (Vector []) i = i
pipe v i = unsafeApply (L.foldr1 (.)) v $ i

pipe1 f v i = unsafeApply (L.foldr f i) v

reverse = unsafeLift L.reverse

recuri ps s = farm11 (`pipe` s) (unsafeLift (L.map vector . L.tails) ps)

-- length :: Vector a -> Int
-- length Null = 0
-- length (v:>vs) = 1 + length vs



-- reducei1 p i v1 vs       = S.farm21 p v1 vs =<<= i
-- tail'  Null = Null
-- tail'  xs   = V.tail xs
-- first' Null = Null
-- first' xs   = V.first xs

-- -- | The function 'rotate' rotates a vector based on an index offset.
-- --
-- -- * @(> 0)@ : rotates the vector left with the corresponding number
-- -- of positions.
-- --
-- -- * @(= 0)@ : does not modify the vector.
-- --
-- -- * @(< 0)@ : rotates the vector right with the corresponding number
-- -- of positions.
-- rotate :: Int -> Vector a -> Vector a
-- rotate n
--   | n > 0     = V.pipe (V.fanoutn (abs n) V.rotl)
--   | n < 0     = V.pipe (V.fanoutn (abs n) V.rotr)
--   | otherwise = id


-- -- | takes the first /n/ elements of a vector.
-- --
-- -- >>> take 5 $ vector [1,2,3,4,5,6,7,8,9]
-- -- <1,2,3,4,5>
-- --
-- -- <<fig/eqs-skel-vector-take.png>>
-- take _ Null = Null
-- take n v    = reducei1 sel Null indexes . S.farm11 unit $ v
--   where sel i x y = if i < n then x <++> y else x

-- -- | drops the first /n/ elements of a vector.
-- --
-- -- >>> drop 5 $ vector [1,2,3,4,5,6,7,8,9]
-- -- <6,7,8,9>
-- --
-- -- <<fig/eqs-skel-vector-drop.png>>
-- drop _ Null = Null
-- drop n v    = reducei1 sel Null indexes . S.farm11 unit $ v
--   where sel i x y = if i > n then x <++> y else y

-- -- | groups a vector into sub-vectors of /n/ elements.
-- --
-- -- >>> group 3 $ vector [1,2,3,4,5,6,7,8]
-- -- <<1,2,3>,<4,5,6>,<7,8>>
-- --
-- -- <<fig/eqs-skel-vector-group.png>>
-- --
-- -- <<fig/skel-vector-comm-group.png>>
-- -- <<fig/skel-vector-comm-group-net.png>>
-- group :: Int -> Vector a -> Vector (Vector a)
-- group _ Null = unit Null
-- group n v = reducei1 sel Null indexes . S.farm11 (unit . unit) $ v
--   where sel i x y
--           | i `mod` n == 0 = x <++> y
--           | otherwise      = (S.first x <++> first' y) :> tail' y


-- -- | returns the /n/-th element in a vector, or @Nothing@ if /n > l/.
-- --
-- -- >>> get 3 $ vector [1,2,3,4,5]
-- -- Just 3
-- --
-- -- <<fig/eqs-skel-vector-get.png>>
-- get _ Null = Nothing
-- get n v    = reducei1 sel Nothing indexes . S.farm11 Just $ v
--   where sel i x y = if i == n then x else y

-- -- | the same as 'get' but with flipped arguments.
-- v <@  ix = get ix v

-- -- | unsafe version of '<@>'. Throws an exception if /n > l/.
-- v <@! ix | isNothing e = error "get!: index out of bounds"
--          | otherwise   = fromJust e
--   where e = get ix v


-- indexes = V.vector [1..] :: Vector Int

-- indexesTo n = take n $ indexes


-- -- | Returns a stencil of @n@ neighboring elements for each possible
-- -- element in a vector.
-- --
-- -- >>> stencilV 3 $ vector [1..5]
-- -- <<1,2,3>,<2,3,4>,<3,4,5>>
-- stencil :: Int               -- ^ stencil size @= n@
--         -> Vector a          -- ^ /length/ = @la@ 
--         -> Vector (Vector a) -- ^ /length/ = @la - n + 1@ 
-- stencil n v = V.farm11 (take n) $ dropFromEnd n $ V.tails v
--   where dropFromEnd n = take (length v - n + 1)


-- zip = farm21 (,)

-- unzip = farm12 id
