{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- Signed Integers
-- Suitable for use with Hugs 98 on 32 bit systems.
-----------------------------------------------------------------------------

module ForSyDe.Deep.Fixed where

import ForSyDe.Deep.Int
import Data.Bits
import Language.Haskell.TH.Lift
import Data.Data

class Coerce a where
  to   :: Int -> a
  from :: a -> Int


-- The function 'scale' scales a Fixed Point value with the factor (2^n).

class Fixed a where
  scale :: Int -> a -> a

-- Transformation Functions

fixed8ToFixed16 :: Fixed8 -> Fixed16
fixed16ToFixed8 :: Fixed16 -> Fixed8

fixed8ToFixed16 = F16 . intToInt16 . (*(2^8)) . fixed8ToInt
fixed16ToFixed8 = F8 . intToInt8 . (`div` (2^8)) . fixed16ToInt
fixed16ToFixed32 = F32 . intToInt32 . (*(2^16)) .fixed16ToInt
fixed32ToFixed16 = F16 . intToInt16 . (`div` (2^16)) . fixed32ToInt

fixed32ToFixed20 = F20 . intToInt20 . (`div` (2^20)) . fixed32ToInt
fixed40ToFixed20 = F20 . intToInt20 . fromIntegral . (`div` (2^20))
----------------------------------------------------------------------------

data Fixed8 = F8 Int8

instance Eq Fixed8   where 
  F8 x == F8 y    = x == y

instance Num Fixed8 where
  (F8 x) + (F8 y) = F8 (x + y)
  (F8 x) - (F8 y) = F8 (x - y)
  negate (F8 x)   = F8 (negate x)
  (F8 x) * (F8 y) = fixed16ToFixed8 (intToFixed16 (2 * (signExtend8To16 (int8ToInt x)) * (signExtend8To16 (int8ToInt y))))
  abs (F8 x)      = F8 (abs x)
  signum (F8 x)   = F8 (signum x)
  fromInteger     = to . fromInteger
  -- to     = to

instance Ord Fixed8     where 
  compare (F8 x) (F8 y) = compare x y

instance Show Fixed8 where
  showsPrec p (F8 x) = shows ((realToFrac (int8ToInt x)) / (2^(8-1)))

instance Read Fixed8 where
  readsPrec _ x = readsFixed8 x

readsFixed8 s = [(realToFixed8 x, c) | (x,c) <- reads s]

realToFixed8 x = if a == 0 then
                  F8 y 
                else
                  if x == -1.0 then
                    F8 (2^(8-1))
                  else 
                    error "No Fixed8 Value"
  where    (a, b) = properFraction x        
           y      = truncate ((2^(8-1)) * b)

fixed8ToReal :: RealFrac a => Fixed8 -> a
fixed8ToReal = (/(2^(8-1))) . realToFrac . fixed8ToInt
           
fixed8ToInt (F8 x) = int8ToInt x
intToFixed8 x      = F8 (intToInt8 x)

signExtend8To16 x  = if x < (2^(8-1)) then
                       x
                     else
                       2^16 - x

instance Coerce Fixed8 where
   from = fixed8ToInt
   to   = intToFixed8

instance Fixed Fixed8 where
  scale n (F8 x) = F8 (shift x n)

-----------------------------------------------------------------------------

data Fixed16 = F16 Int16

instance Eq Fixed16 where 
   F16 x == F16 y = x == y

instance Num Fixed16 where
  (F16 x) + (F16 y) = F16 (x + y)
  (F16 x) - (F16 y) = F16 (x - y)
  negate (F16 x)    = F16 (negate x)
  (F16 x) * (F16 y) = fixed32ToFixed16 (intToFixed32 
                                        (2 * (signExtend16To32 (int16ToInt x)) 
                                         * (signExtend16To32 (int16ToInt y))))
  abs (F16 x)       = F16 (abs x)
  signum (F16 x)    = F16 (signum x)
  fromInteger       = to . fromInteger
  -- to           = to

instance Ord Fixed16     where 
  compare (F16 x) (F16 y) = compare x y

instance Show Fixed16 where
  showsPrec p (F16 x) = shows ((realToFrac (int16ToInt x)) / (2^(16-1)))

instance Read Fixed16 where
  readsPrec _ x = readsFixed16 x

readsFixed16 s = [(realToFixed16 x, c) | (x,c) <- reads s]

realToFixed16 x = if a == 0 then
                   F16 y 
                 else
                   if x == -1.0 then
                     F16 (2^(16-1))
                   else 
                     error "No Fixed16 Value"
  where    (a, b) = properFraction x        
           y      = truncate ((2^(16-1)) * b) 

fixed16ToReal :: RealFrac a => Fixed16 -> a
fixed16ToReal = (/(2^16-1)) . realToFrac . fixed16ToInt

fixed16ToInt (F16 x) = int16ToInt x
intToFixed16 x       = F16 (intToInt16 x)

signExtend16To32 x   = if x < (2^(16-1)) then
                         x
                       else
                         2^32 - x
                        
instance Coerce Fixed16 where
   from = fixed16ToInt
   to   = intToFixed16

instance Fixed Fixed16 where
  scale n (F16 x) = F16 (shift x n)

----------------------------------------------------------------------------

data Fixed32 = F32 Int32

instance Eq  Fixed32   where 
   F32 x == F32 y    = x == y

instance Num Fixed32 where
  (F32 x) + (F32 y) = F32 (x + y)
  (F32 x) - (F32 y) = F32 (x - y)
  negate (F32 x)   = F32 (negate x)
--    (F32 x) * (F32 y) = fixed64ToFixed32 (intToFixed64 (2 * (signExtend32To64 (int32ToInt x)) * (signExtend32To64 (int32ToInt y))))
  abs (F32 x)     = F32 (abs x)
  signum (F32 x)   = F32 (signum x)
  fromInteger     = to . fromInteger
  -- to     = to

instance Ord Fixed32     where 
  compare (F32 x) (F32 y) = compare x y

instance Show Fixed32 where
  showsPrec p (F32 x) = shows ((realToFrac (int32ToInt x)) / (2^(32-1)))

instance Read Fixed32 where
  readsPrec _ x = readsFixed32 x

readsFixed32 s = [(realToFixed32 x, c) | (x,c) <- reads s]

realToFixed32 x = if a == 0 then
                   F32 y 
                 else
                   if x == -1.0 then
                     F32 (2^(32-1))
                   else 
                     error "No Fixed32 Value"
  where    (a, b) = properFraction x        
           y      = truncate ((2^(32-1)) * b) 

fixed32ToReal :: RealFrac a => Fixed32 -> a
fixed32ToReal = (/(2^(32-1))) . realToFrac . fixed32ToInt
           
fixed32ToInt (F32 x) = int32ToInt x
intToFixed32 x       = F32 (intToInt32 x)

signExtend32To64 x   = if x < (2^(32-1)) then
                         x
                       else
                         2^32 - x

instance Coerce Fixed32 where
   from = fixed32ToInt
   to   = intToFixed32

instance Fixed Fixed32 where
  scale n (F32 x) = F32 (shift x n)


-----------------------------------------------------------------------------

data Fixed20 = F20 Int20

instance Eq Fixed20 where 
   F20 x == F20 y = x == y

instance Num Fixed20 where
  (F20 x) + (F20 y) = F20 (x + y)
  (F20 x) - (F20 y) = F20 (x - y)
  negate (F20 x)    = F20 (negate x)
  (F20 x) * (F20 y)
    = fixed40ToFixed20 (2 * (signExtend20To40 (toInteger $ int20ToInt x)) 
                        * (signExtend20To40 (toInteger $ int20ToInt y)))
  abs (F20 x)       = F20 (abs x)
  signum (F20 x)    = F20 (signum x)
  fromInteger       = to . fromInteger
  -- to           = to

instance Ord Fixed20     where 
  compare (F20 x) (F20 y) = compare x y

instance Show Fixed20 where
  showsPrec p (F20 x) = shows ((realToFrac (int20ToInt x)) / (2^(20-1)))

instance Read Fixed20 where
  readsPrec _ x = readsFixed20 x

readsFixed20 s = [(realToFixed20 x, c) | (x,c) <- reads s]

realToFixed20 x = if a == 0 then
                   F20 y 
                 else
                   if x == -1.0 then
                     F20 (2^(20-1))
                   else 
                     error "No Fixed20 Value"
  where    (a, b) = properFraction x        
           y      = truncate ((2^(20-1)) * b) 

fixed20ToReal :: RealFrac a => Fixed20 -> a
fixed20ToReal = (/(2^(20-1))) . realToFrac . fixed20ToInt

fixed20ToInt (F20 x) = int20ToInt x
intToFixed20 x       = F20 (intToInt20 x)

-- Hack: keep 40 worth of bits inside a long int
signExtend20To40 :: Integer -> Integer
signExtend20To40 x   = if x < (2^(20-1)) then
                         x
                       else
                         2^40 - x
                        
instance Coerce Fixed20 where
   from = fixed20ToInt
   to   = intToFixed20

instance Fixed Fixed20 where
  scale n (F20 x) = F20 (shift x n)

$(mapM deriveLift [''Fixed8, ''Fixed16, ''Fixed20, ''Fixed32])

fixed20Type :: DataType
fixed20Type = mkIntType "ForSyDe.Deep.Int.Fixed20"

instance Data Fixed20 where
  toConstr = mkIntegralConstr fixed20Type . fixed20ToInt
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Fixed20."
  dataTypeOf _ = fixed20Type
