module ForSyDe.Shallow.Utility.Cube where

import ForSyDe.Shallow.Core.Vector
import ForSyDe.Shallow.Utility.Matrix

-- | 'Cube' is simply a type synonym for vector of vectors. This
-- means that /any/ function on 'Vector' works also on 'Cube'.
type Cube a = Vector (Vector (Vector a))

-- | Prints out to the terminal a cube in a readable format, where
-- all elements are right-aligned and separated by a custom separator.
--
-- >>> let m = cube 2 2 2 [1,2,3,3,100,4,12,32]
-- >>> pretty "|" m
-- --------
-- 1|2
-- 3|3
-- --------
-- 100| 4
--  12|32
-- --------
prettyCube :: Show a
       => String   -- ^ separator string
       -> Cube a -- ^ input cube
       -> IO ()
prettyCube sep (m:>ms) = putStrLn "--------" >> prettyMat sep m
prettyCube _   NullV   = putStrLn "--------"

-- | Checks if a cube is null. @<>@, @<<>>@ and @<<<>>>@ are all null
-- cubes.
isNullCube :: Cube a -> Bool
isNullCube NullV = True
isNullCube (NullV:>NullV) = True
isNullCube (NullV:>NullV:>NullV) = True
isNullCube _ = False

-- | Returns the X and Y dimensions of cube and checks if it is well formed.
size :: Cube a -> (Int,Int,Int)
size m = (x,y,z)
  where
    z = lengthV (wellFormedCube m)
    y = (lengthV . headV) m
    x = (lengthV . headV . headV) m

-- | Checks if a cube is well-formed, meaning that all its rows are
-- of equal length. Returns the same cube in case it is well-formed
-- or throws an exception if it is ill-formed.
wellFormedCube :: Cube a -> Cube a
wellFormedCube NullV = NullV
wellFormedCube (x:>xs) = wellFormedMat x :> wellFormedCube xs

groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n l
  | n < 0        = error $ "cannot group list by negative n: " ++ show n
  | length l < n = error "input list cannot be split into all-equal parts"
  | otherwise    = take n l : groupEvery n (drop n l)

-- | Converts a list into a 'Cube'. See example from 'pretty'.
cube :: Int      -- ^ number of columns (X dimension) @= x@
     -> Int      -- ^ number of rows (Y dimension) @= y@
     -> Int      -- ^ depth (Z dimension) @= z@
     -> [a]      -- ^ list of values; /length/ = @x * y * z@
     -> Cube a -- ^ 'Cube' of values; /size/ = @(x,y,z)@
cube x y z = vector . Prelude.map (matrix x y) . groupEvery (x * y) . check
  where
    check l | length l == x * y * z = l
            | otherwise
      = error $ "cannot form cube (" ++ show x ++ ","
              ++ show y ++ "," ++ show z ++ ") from a list with "
              ++ show (length l) ++ " elements"

-- | Converts a cube back to a list.
fromCube :: Cube a -- ^ /size/ = @(x,y)@
         -> [a]      -- ^ /length/ = @x * y@
fromCube = concatMap fromVector . fromMatrix

-- | Creates a unit (i.e. singleton) cube, which is a cube with
-- only one element.
unit :: a -> Cube a -- ^ /size/ = @(1,1)@
unit a = ((a:>NullV):>NullV):>NullV

-- | Creates an /infinite cube/ which repeats one element
copyCube :: Int -> Int -> Int -> a -> Cube a
copyCube x y z n = copyV z $ copyV y $ copyV x n

-- | Returns an /infinite cube/ with (X,Y) index pairs. You need to
-- zip it against another (finite) cube or to extract a finite
-- subset in order to be useful (see example below).
--
-- >>> pretty " " $ takeMat 3 4 indexMat 
-- (0,0) (1,0) (2,0)
-- (0,1) (1,1) (2,1)
-- (0,2) (1,2) (2,2)
-- (0,3) (1,3) (2,3)
indexCube :: Cube (Int, Int, Int)
indexCube = zipWith3Cube (,,) colix rowix depthix
  where
    colix = vector $ repeat $ vector $ repeat $ vector [0..]
    rowix = mapV transposeMat colix
    depthix =  transposeMat $ mapV transposeMat colix

transposeCube :: Cube a -- ^ dimensions @(Z,Y,X)@
              -> Cube a -- ^ dimensions @(Y,X,Z)@
transposeCube = mapV transposeMat . transposeMat

transposeCube' :: Cube a -- ^ dimensions @(Y,X,Z)@
               -> Cube a -- ^ dimensions @(Z,Y,X)@
transposeCube' = transposeMat . mapV transposeMat

-- | Maps a function on every value of a cube.
--
-- __OBS:__ this function does not check if the output cube is well-formed.
mapCube :: (a -> b)
       -> Cube a -- ^ /size/ = @(xa,ya)@
       -> Cube b -- ^ /size/ = @(xa,ya)@
mapCube = mapV . mapV . mapV

-- | Applies a binary function pair-wise on each element in two matrices.
--
-- __OBS:__ this function does not check if the output cube is well-formed.
zipWithCube :: (a -> b -> c)
            -> Cube a -- ^ /size/ = @(xa,ya)@
            -> Cube b -- ^ /size/ = @(xb,yb)@
            -> Cube c -- ^ /size/ = @(minimum [xa,xb], minimum [ya,yb])@
zipWithCube f = zipWithV (zipWithV (zipWithV f))

-- | Applies a function 3-tuple-wise on each element in three matrices.
--
-- __OBS:__ this function does not check if the output cube is well-formed.
zipWith3Cube :: (a -> b -> c -> d)
             -> Cube a -- ^ /size/ = @(xa,ya)@
             -> Cube b -- ^ /size/ = @(xb,yb)@
             -> Cube c -- ^ /size/ = @(xc,yc)@
             -> Cube d -- ^ /size/ = @(minimum [xa,xb,xc], minimum [ya,yb,yc])@
zipWith3Cube f = zipWith3V (zipWith3V (zipWith3V f))

-- | Reduces all the elements of a cube to one element based on a
-- binary function.
--
-- >>> let m = cube 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> reduce (+) m
-- 108
reduceCube :: (a -> a -> a) -> Cube a -> a
reduceCube f = reduceV f . mapV (reduceMat f)

-- | Returns the element of a matrix at a certain position.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> at 2 1 m
-- 13
atCube :: Cube a
       -> (Int,Int,Int)
       -> a
cube `atCube` (x,y,z) =  (cube `atV` z) `atMat` (x,y)

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ take 2 2 m
--  1  2
-- 11 12
takeCube :: Int       -- ^ X index starting from zero
         -> Int       -- ^ Y index starting from zero
         -> Int       -- ^ > index starting from zero
         -> Cube a
         -> Cube a
takeCube x y z = mapV (takeMat x y) . takeV z

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ drop 2 2 m
-- 23 24
-- 33 34
dropCube :: Int       -- ^ X index starting from zero
         -> Int       -- ^ Y index starting from zero
         -> Int       -- ^ Z index starting from zero
         -> Cube a
         -> Cube a
dropCube x y z = mapV (dropMat x y) . dropV z

infiniteCube  = vector . repeat . vector . repeat . vector . repeat
