module ForSyDe.Atom.Skeleton.Vector.Cube where

import ForSyDe.Atom.Skeleton.Vector (Vector(..), vector, fromVector, (<++>))
import ForSyDe.Atom.Skeleton.Vector.Matrix (Matrix, matrix, fromMatrix)

import qualified ForSyDe.Atom.Skeleton.Vector as V
import qualified ForSyDe.Atom.Skeleton.Vector.Matrix as M

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
pretty :: Show a
       => String   -- ^ separator string
       -> Cube a -- ^ input cube
       -> IO ()
pretty sep mat = mapM_ (\m -> putStrLn "--------" >> M.pretty sep m) mat >> putStrLn "--------"

-- | Checks if a cube is null. @<>@, @<<>>@ and @<<<>>>@ are all null
-- cubes.
isNull :: Cube a -> Bool
isNull Null = True
isNull (Null:>Null) = True
isNull (Null:>Null:>Null) = True
isNull _ = False

-- | Returns the X and Y dimensions of cube and checks if it is well formed.
size :: Cube a -> (Int,Int,Int)
size m = (x,y,z)
  where
    z = V.length (wellFormed m)
    y = (V.length . V.first) m
    x = (V.length . V.first . V.first) m

-- | Checks if a cube is well-formed, meaning that all its rows are
-- of equal length. Returns the same cube in case it is well-formed
-- or throws an exception if it is ill-formed.
wellFormed :: Cube a -> Cube a
wellFormed Null = Null
wellFormed (x:>xs) = M.wellFormed x :> wellFormed xs

-- | Converts a list into a 'Cube'. See example from 'pretty'.
cube :: Int      -- ^ number of columns (X dimension) @= x@
     -> Int      -- ^ number of rows (Y dimension) @= y@
     -> Int      -- ^ depth (Z dimension) @= z@
     -> [a]      -- ^ list of values; /length/ = @x * y * z@
     -> Cube a -- ^ 'Cube' of values; /size/ = @(x,y,z)@
cube x y z = vector . Prelude.map (matrix x y) . M.groupEvery (x * y) . check
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
unit a = ((a:>Null):>Null):>Null

-- | Creates an /infinite cube/ which repeats one element
fanout :: a -> Cube a
fanout n = V.fanout $ V.fanout $ V.fanout n

-- | Returns an /infinite cube/ with (X,Y) index pairs. You need to
-- zip it against another (finite) cube or to extract a finite
-- subset in order to be useful (see example below).
--
-- >>> pretty " " $ takeMat 3 4 indexMat 
-- (0,0) (1,0) (2,0)
-- (0,1) (1,1) (2,1)
-- (0,2) (1,2) (2,2)
-- (0,3) (1,3) (2,3)
indexes :: Cube (Int, Int, Int)
indexes = farm31 (,,) colix rowix depthix
  where
    colix = vector $ repeat $ vector $ repeat $ vector [0..]
    rowix = V.farm11 M.transpose colix
    depthix =  M.transpose $ V.farm11 M.transpose colix

transpose :: Cube a -- ^ dimensions @(Z,Y,X)@
           -> Cube a -- ^ dimensions @(Y,X,Z)@
transpose = V.farm11 M.transpose . M.transpose

transpose' :: Cube a -- ^ dimensions @(Y,X,Z)@
            -> Cube a -- ^ dimensions @(Z,Y,X)@
transpose' = M.transpose . V.farm11 M.transpose

-- | Maps a function on every value of a cube.
--
-- __OBS:__ this function does not check if the output cube is well-formed.
farm11 :: (a -> b)
       -> Cube a -- ^ /size/ = @(xa,ya)@
       -> Cube b -- ^ /size/ = @(xa,ya)@
farm11 = V.farm11 . V.farm11 . V.farm11

-- | Applies a binary function pair-wise on each element in two matrices.
--
-- __OBS:__ this function does not check if the output cube is well-formed.
farm21 :: (a -> b -> c)
           -> Cube a -- ^ /size/ = @(xa,ya)@
           -> Cube b -- ^ /size/ = @(xb,yb)@
           -> Cube c -- ^ /size/ = @(minimum [xa,xb], minimum [ya,yb])@
farm21 f = V.farm21 (V.farm21 (V.farm21 f))

-- | Applies a function 3-tuple-wise on each element in three matrices.
--
-- __OBS:__ this function does not check if the output cube is well-formed.
farm31 :: (a -> b -> c -> d)
            -> Cube a -- ^ /size/ = @(xa,ya)@
            -> Cube b -- ^ /size/ = @(xb,yb)@
            -> Cube c -- ^ /size/ = @(xc,yc)@
            -> Cube d -- ^ /size/ = @(minimum [xa,xb,xc], minimum [ya,yb,yc])@
farm31 f = V.farm31 (V.farm31 (V.farm31 f))

-- | Reduces all the elements of a cube to one element based on a
-- binary function.
--
-- >>> let m = cube 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> reduce (+) m
-- 108
reduce :: (a -> a -> a) -> Cube a -> a
reduce f = V.reduce f . V.farm11 (M.reduce f)

-- | Returns the element of a matrix at a certain position.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> at 2 1 m
-- 13
get :: Int       -- ^ X index starting from zero
    -> Int       -- ^ Y index starting from zero
    -> Int       -- ^ Z index starting from zero
    -> Cube a
    -> Maybe a
get x y z = getMaybe . V.get z
  where getMaybe Nothing = Nothing
        getMaybe (Just a) = M.get x y a

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ take 2 2 m
--  1  2
-- 11 12
take :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Int       -- ^ > index starting from zero
     -> Cube a
     -> Cube a
take x y z = V.farm11 (M.take x y) . V.take z

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ drop 2 2 m
-- 23 24
-- 33 34
drop :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Int       -- ^ Z index starting from zero
     -> Cube a
     -> Cube a
drop x y z = V.farm11 (M.drop x y) . V.drop z


