-- TODO :: minimal Bit definitions for all types

module ForSyDe.Deep.Int
  ( Int8
  , Int16
  , Int20
  , Int32
  , Int64
  , int8ToInt  -- :: Int8  -> Int
  , intToInt8  -- :: Int   -> Int8
  , int16ToInt -- :: Int16 -> Int
  , intToInt16 -- :: Int   -> Int16
  , int32ToInt -- :: Int32 -> Int
  , intToInt32 -- :: Int   -> Int32
  , int20ToInt -- :: Int20 -> Int
  , intToInt20 -- :: Int   -> Int20
  -- , toInt
  -- , fromInt
  -- plus Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
  --  Show and Bits instances for each of Int8, Int16 and Int32
  ) where
import Data.Bits
import Data.Ratio

-----------------------------------------------------------------------------
-- The "official" coercion functions
-----------------------------------------------------------------------------

int8ToInt  :: Int8  -> Int
intToInt8  :: Int   -> Int8
int16ToInt :: Int16 -> Int
intToInt16 :: Int   -> Int16
int32ToInt :: Int32 -> Int
intToInt32 :: Int   -> Int32

int20ToInt :: Int20 -> Int
intToInt20 :: Int   -> Int20

-- And some non-exported ones

int8ToInt16  :: Int8  -> Int16
int8ToInt32  :: Int8  -> Int32
int16ToInt8  :: Int16 -> Int8
int16ToInt32 :: Int16 -> Int32
int32ToInt8  :: Int32 -> Int8
int32ToInt16 :: Int32 -> Int16

int8ToInt16  = I16 . int8ToInt
int8ToInt32  = I32 . int8ToInt
int16ToInt8  = I8  . int16ToInt
int16ToInt32 = I32 . int16ToInt
int32ToInt8  = I8  . int32ToInt
int32ToInt16 = I16 . int32ToInt

-----------------------------------------------------------------------------
-- Int8
-----------------------------------------------------------------------------

newtype Int8  = I8 Int

int8ToInt (I8 x) = if x' <= 0x7f then x' else x' - 0x100
  where x' = x .&. 0xff
intToInt8 = I8

instance Eq  Int8     where (==)    = binop (==)
instance Ord Int8     where compare = binop compare

instance Num Int8 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    -- fromInt       = to

instance Bounded Int8 where
    minBound = 0x80
    maxBound = 0x7f 

instance Real Int8 where
    toRational x = toInteger x % 1

instance Integral Int8 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    -- even          = even      . from
    toInteger     = toInteger . from
    -- toInt         = toInt     . from

-- instance Ix Int8 where
--     range (m,n)          = [m..n]
--     index b@(m,n) i
--        | inRange b i = from (i - m)
--        | otherwise   = error "index: Index out of range"
--     inRange (m,n) i      = m <= i && i <= n

instance Enum Int8 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int8)]
      where last = if d < c then minBound else maxBound

instance Read Int8 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int8 where
    showsPrec p = showsPrec p . from

instance Bits Int8 where
  x .&. y       = intToInt8 (binop (.&.) x y)
  x .|. y       = intToInt8 (binop (.|.) x y)
  x `xor` y     = intToInt8 (binop xor x y)
  complement    = intToInt8 . complement . int8ToInt
  x `shift` i   = intToInt8 (int8ToInt x `shift` i)
--  rotate      
  bit           = intToInt8 . bit
  setBit x i    = intToInt8 (setBit (int8ToInt x) i)
  clearBit x i  = intToInt8 (clearBit (int8ToInt x) i)
  complementBit x i = intToInt8 (complementBit (int8ToInt x) i)
  testBit x i   = testBit (int8ToInt x) i
  bitSize  _    = 8
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int16
-----------------------------------------------------------------------------

newtype Int16  = I16 Int

int16ToInt (I16 x) = if x' <= 0x7fff then x' else x' - 0x10000
 where x' = x .&. 0xffff
intToInt16 = I16

instance Eq  Int16     where (==)    = binop (==)
instance Ord Int16     where compare = binop compare

instance Num Int16 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    -- fromInt       = to

instance Bounded Int16 where
    minBound = 0x8000
    maxBound = 0x7fff 

instance Real Int16 where
    toRational x = toInteger x % 1

instance Integral Int16 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    -- even          = even      . from
    toInteger     = toInteger . from
    -- toInt         = toInt     . from

-- instance Ix Int16 where
--     range (m,n)          = [m..n]
--     index b@(m,n) i
--        | inRange b i = from (i - m)
--        | otherwise   = error "index: Index out of range"
--     inRange (m,n) i      = m <= i && i <= n

instance Enum Int16 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int16)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int16)]
     where last = if d < c then minBound else maxBound

instance Read Int16 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int16 where
    showsPrec p = showsPrec p . from

binop16 :: (Int32 -> Int32 -> a) -> (Int16 -> Int16 -> a)
binop16 op x y = int16ToInt32 x `op` int16ToInt32 y

instance Bits Int16 where
  x .&. y       = intToInt16 (binop (.&.) x y)
  x .|. y       = intToInt16 (binop (.|.) x y)
  x `xor` y     = intToInt16 (binop xor x y)
  complement    = intToInt16 . complement . int16ToInt
  x `shift` i   = intToInt16 (int16ToInt x `shift` i)
--  rotate      
  bit           = intToInt16 . bit
  setBit x i    = intToInt16 (setBit (int16ToInt x) i)
  clearBit x i  = intToInt16 (clearBit (int16ToInt x) i)
  complementBit x i = intToInt16 (complementBit (int16ToInt x) i)
  testBit x i   = testBit (int16ToInt x) i
  bitSize  _    = 16
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int32
-----------------------------------------------------------------------------

newtype Int32  = I32 Int

int32ToInt (I32 x) = x
intToInt32 = I32

instance Eq  Int32     where (==)    = binop (==)
instance Ord Int32     where compare = binop compare

instance Num Int32 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    -- fromInt       = to

instance Bounded Int32 where
    minBound = to minBound
    maxBound = to maxBound

instance Real Int32 where
    toRational x = toInteger x % 1

instance Integral Int32 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    -- even          = even      . from
    toInteger     = toInteger . from
    -- toInt         = toInt     . from

-- instance Ix Int32 where
--     range (m,n)          = [m..n]
--     index b@(m,n) i
--        | inRange b i = from (i - m)
--        | otherwise   = error "index: Index out of range"
--     inRange (m,n) i      = m <= i && i <= n

instance Enum Int32 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int32)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int32)]
     where last = if d < c then minBound else maxBound

instance Read Int32 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int32 where
    showsPrec p = showsPrec p . from

instance Bits Int32 where
  x .&. y       = intToInt32 (binop (.&.) x y)
  x .|. y       = intToInt32 (binop (.|.) x y)
  x `xor` y     = intToInt32 (binop xor x y)
  complement    = intToInt32 . complement . int32ToInt
  x `shift` i   = intToInt32 (int32ToInt x `shift` i)
--  rotate      
  bit           = intToInt32 . bit
  setBit x i    = intToInt32 (setBit (int32ToInt x) i)
  clearBit x i  = intToInt32 (clearBit (int32ToInt x) i)
  complementBit x i = intToInt32 (complementBit (int32ToInt x) i)
  testBit x i   = testBit (int32ToInt x) i
  bitSize  _    = 32
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int64
--
-- This is not ideal, but does have the advantage that you can 
-- now typecheck generated code that include Int64 statements.
--
-----------------------------------------------------------------------------

type Int64 = Integer


-----------------------------------------------------------------------------
-- Int20
-----------------------------------------------------------------------------

newtype Int20  = I20 Int

int20ToInt (I20 x) = if x' <= 0x7ffff then x' else x' - 0x100000
 where x' = x .&. 0xfffff
intToInt20 = I20

instance Eq  Int20     where (==)    = binop (==)
instance Ord Int20     where compare = binop compare

instance Num Int20 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    -- fromInt       = to

instance Bounded Int20 where
    minBound = 0x80000
    maxBound = 0x7ffff 

instance Real Int20 where
    toRational x = toInteger x % 1

instance Integral Int20 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    -- even          = even      . from
    toInteger     = toInteger . from
    -- toInt         = toInt     . from

-- instance Ix Int20 where
--     range (m,n)          = [m..n]
--     index b@(m,n) i
--        | inRange b i = from (i - m)
--        | otherwise   = error "index: Index out of range"
--     inRange (m,n) i      = m <= i && i <= n

instance Enum Int20 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int20)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int20)]
     where last = if d < c then minBound else maxBound

instance Read Int20 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int20 where
    showsPrec p = showsPrec p . from

-- binop20 :: (Int32 -> Int32 -> a) -> (Int20 -> Int20 -> a)
-- binop20 op x y = int20ToInt32 x `op` int20ToInt32 y

instance Bits Int20 where
  x .&. y       = intToInt20 (binop (.&.) x y)
  x .|. y       = intToInt20 (binop (.|.) x y)
  x `xor` y     = intToInt20 (binop xor x y)
  complement    = intToInt20 . complement . int20ToInt
  x `shift` i   = intToInt20 (int20ToInt x `shift` i)
--  rotate      
  bit           = intToInt20 . bit
  setBit x i    = intToInt20 (setBit (int20ToInt x) i)
  clearBit x i  = intToInt20 (clearBit (int20ToInt x) i)
  complementBit x i = intToInt20 (complementBit (int20ToInt x) i)
  testBit x i   = testBit (int20ToInt x) i
  bitSize  _    = 20
  isSigned _    = True


-----------------------------------------------------------------------------
-- End of exported definitions
--
-- The remainder of this file consists of definitions which are only
-- used in the implementation.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Coercions - used to make the instance declarations more uniform
-----------------------------------------------------------------------------

class Coerce a where
  to   :: Int -> a
  from :: a -> Int

instance Coerce Int32 where
  from = int32ToInt
  to   = intToInt32

instance Coerce Int8 where
  from = int8ToInt
  to   = intToInt8

instance Coerce Int16 where
  from = int16ToInt
  to   = intToInt16

instance Coerce Int20 where
  from = int20ToInt
  to   = intToInt20


binop :: Coerce int => (Int -> Int -> a) -> (int -> int -> a)
binop op x y = from x `op` from y

to2 :: Coerce int => (Int, Int) -> (int, int)
to2 (x,y) = (to x, to y)

-- -----------------------------------------------------------------------------
-- -- Extra primitives
-- -----------------------------------------------------------------------------

-- primitive primAnd "primAndInt" :: Int -> Int -> Int

-- primitive primAndInt        :: Int32 -> Int32 -> Int32
-- primitive primOrInt         :: Int32 -> Int32 -> Int32
-- primitive primXorInt        :: Int32 -> Int32 -> Int32
-- primitive primComplementInt :: Int32 -> Int32
-- primitive primShiftInt      :: Int32 -> Int -> Int32
-- primitive primBitInt        :: Int -> Int32
-- primitive primTestInt       :: Int32 -> Int -> Bool


-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
             | otherwise = -x
      
signumReal x | x == 0    =  0
             | x > 0     =  1
             | otherwise = -1

-----------------------------------------------------------------------------
-- End
-----------------------------------------------------------------------------
