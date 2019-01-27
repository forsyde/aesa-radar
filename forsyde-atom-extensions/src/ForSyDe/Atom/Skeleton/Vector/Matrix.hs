module ForSyDe.Atom.Skeleton.Vector.Matrix where

import qualified GHC.List as L (take, drop)
import Prelude hiding (take, drop)
import Data.List (intercalate)
import ForSyDe.Atom.Skeleton.Vector (Vector(..), vector, fromVector, (<++>))
import qualified ForSyDe.Atom.Skeleton.Vector as V

-- | 'Matrix' is simply a type synonym for vector of vectors. This
-- means that /any/ function on 'Vector' works also on 'Matrix'.
type Matrix a = Vector (Vector a)

-- | Prints out to the terminal a matrix in a readable format, where
-- all elements are right-aligned and separated by a custom separator.
--
-- >>> let m = matrix 3 3 [1,2,3,3,100,4,12,32,67]
-- >>> pretty "|" m
--  1|  2| 3
--  3|100| 4
-- 12| 32|67
pretty :: Show a
       => String   -- ^ separator string
       -> Matrix a -- ^ input matrix
       -> IO ()
pretty sep mat = mapM_ putStrLn $ fromVector $ printMat maxWdt strMat
  where
    maxWdt = V.reduce (V.farm21 max) $ farm11 length strMat
    strMat = farm11 show mat
    printMat w  = V.farm11 (printRow w)
    printRow w  = intercalate sep . fromVector . V.farm21 align w
    align n str = replicate (n - length str) ' ' ++ str

-- | Checks if a matrix is null. @<>@ and @<<>>@ are both null
-- matrices.
isNull :: Matrix a -> Bool
isNull Null = True
isNull (Null:>Null) = True
isNull _ = False

-- | Returns the X and Y dimensions of matrix and checks if it is well formed.
size :: Matrix a -> (Int,Int)
size m = (x,y)
  where
    y = V.length m
    x = (V.length . V.first) (wellFormed m)

-- | Checks if a matrix is well-formed, meaning that all its rows are
-- of equal length. Returns the same matrix in case it is well-formed
-- or throws an exception if it is ill-formed.
wellFormed :: Matrix a -> Matrix a
wellFormed Null = Null
wellFormed m@(_:>Null) = m
wellFormed m@(x:>xs)
  | V.reduce (&&) (V.farm11 (\r -> V.length r == V.length x) xs) = m
  | otherwise = error "matrix ill-formed: rows are of unequal lengths"

groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n l
  | n < 0        = error $ "cannot group list by negative n: " ++ show n
  | length l < n = error "input list cannot be split into all-equal parts"
  | otherwise    = L.take n l : groupEvery n (L.drop n l)

-- | Converts a list into a 'Matrix'. See example from 'pretty'.
matrix :: Int      -- ^ number of columns (X dimension) @= x@
       -> Int      -- ^ number of rows (Y dimension) @= y@
       -> [a]      -- ^ list of values; /length/ = @x * y@
       -> Matrix a -- ^ 'Matrix' of values; /size/ = @(x,y)@
matrix x y = vector . map vector . groupEvery x . check
  where
    check l | length l == x * y = l
            | otherwise
      = error $ "cannot form matrix (" ++ show x ++ ","
              ++ show y ++ ") from a list with "
              ++ show (length l) ++ " elements"

-- | Converts a matrix back to a list.
fromMatrix :: Matrix a -- ^ /size/ = @(x,y)@
           -> [a]      -- ^ /length/ = @x * y@
fromMatrix = concatMap fromVector . fromVector

-- | Creates a unit (i.e. singleton) matrix, which is a matrix with
-- only one element.
unit :: a -> Matrix a -- ^ /size/ = @(1,1)@
unit a = (a:>Null):>Null

-- | Creates an /infinite matrix/ which repeats one element
fanout :: a -> Matrix a
fanout n = V.fanout $ V.fanout n

-- | Returns an /infinite matrix/ with (X,Y) index pairs. You need to
-- zip it against another (finite) matrix or to extract a finite
-- subset in order to be useful (see example below).
--
-- >>> pretty " " $ takeMat 3 4 indexMat 
-- (0,0) (1,0) (2,0)
-- (0,1) (1,1) (2,1)
-- (0,2) (1,2) (2,2)
-- (0,3) (1,3) (2,3)
indexes :: Matrix (Int, Int)
indexes = farm21 (,) colix rowix
  where
    colix = vector $ repeat $ vector [0..]
    rowix = transpose colix

-- | Maps a function on every value of a matrix.
--
-- __OBS:__ this function does not check if the output matrix is well-formed.
farm11 :: (a -> b)
       -> Matrix a -- ^ /size/ = @(xa,ya)@
       -> Matrix b -- ^ /size/ = @(xa,ya)@
farm11 = V.farm11 . V.farm11

-- | Applies a binary function pair-wise on each element in two matrices.
--
-- __OBS:__ this function does not check if the output matrix is well-formed.
farm21 :: (a -> b -> c)
           -> Matrix a -- ^ /size/ = @(xa,ya)@
           -> Matrix b -- ^ /size/ = @(xb,yb)@
           -> Matrix c -- ^ /size/ = @(minimum [xa,xb], minimum [ya,yb])@
farm21 f = V.farm21 (V.farm21 f)

-- | Applies a function 3-tuple-wise on each element in three matrices.
--
-- __OBS:__ this function does not check if the output matrix is well-formed.
farm31 :: (a -> b -> c -> d)
            -> Matrix a -- ^ /size/ = @(xa,ya)@
            -> Matrix b -- ^ /size/ = @(xb,yb)@
            -> Matrix c -- ^ /size/ = @(xc,yc)@
            -> Matrix d -- ^ /size/ = @(minimum [xa,xb,xc], minimum [ya,yb,yc])@
farm31 f = V.farm31 (V.farm31 f)

-- | Reduces all the elements of a matrix to one element based on a
-- binary function.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> reduce (+) m
-- 108
reduce :: (a -> a -> a) -> Matrix a -> a
reduce f = V.reduce f . V.farm11 (V.reduce f)

-- | Pattern implementing the template for a dot operation between a
-- vector and a matrix.
--
-- >>> let mA = matrix 4 4 [1,-1,1,1,  1,-1,-1,-1,  1,1,-1,1,  1,1,1,-1]
-- >>> let y  = vector[1,0,0,0]
-- >>> dotV (+) (*) mA y
-- <1,1,1,1>
dotV :: (a -> a -> a) -- ^ kernel function for a row/column reduction, e.g. @(+)@ for dot product
          -> (b -> a -> a) -- ^ binary operation for pair-wise elements, e.g. @(*)@ for dot product
          -> Matrix b      -- ^ /size/ = @(xa,ya)@
          -> Vector a      -- ^ /length/ = @xa@
          -> Vector a      -- ^ /length/ = @xa@
dotV f g mA y = V.farm11 (\x -> V.reduce f $ V.farm21 g x y) mA

-- | Pattern implementing the template for a dot operation between two
-- matrices.
--
-- >>> let mA = matrix 4 4 [1,-1,1,1,  1,-1,-1,-1,  1,1,-1,1,  1,1,1,-1]
-- >>> pretty " " $ dot (+) (*) mA mA
-- 2 -2  2  2
-- 2 -2 -2 -2
-- 2  2  2 -2
-- 2  2 -2  2
dot :: (a -> a -> a) -- ^ kernel function for a row/column reduction, e.g. @(+)@ for dot product
    -> (b -> a -> a) -- ^ binary operation for pair-wise elements, e.g. @(*)@ for dot product
    -> Matrix b      -- ^ /size/ = @(xa,ya)@
    -> Matrix a      -- ^ /size/ = @(ya,xa)@
    -> Matrix a      -- ^ /size/ = @(xa,xa)@
dot f g m = V.farm11 (dotV f g m) . transpose

-- | Returns the element of a matrix at a certain position.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> at 2 1 m
-- 13
get :: Int       -- ^ X index starting from zero
    -> Int       -- ^ Y index starting from zero
    -> Matrix a
    -> Maybe a
get x y mat = getMaybe (V.get y mat)
  where getMaybe Nothing = Nothing
        getMaybe (Just a) = V.get x a

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ take 2 2 m
--  1  2
-- 11 12
take :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Matrix a
     -> Matrix a
take x y = V.farm11 (V.take x) . V.take y

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ drop 2 2 m
-- 23 24
-- 33 34
drop :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Matrix a
     -> Matrix a
drop x y = V.farm11 (V.drop x) . V.drop y

-- | Crops a section of a matrix.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " m
--  1  2  3  4
-- 11 12 13 14
-- 21 22 23 24
-- 31 32 33 34
-- >>> pretty " " $ cropMat 2 3 1 1 m
-- 12 13
-- 22 23
-- 32 33
crop :: Int      -- ^ crop width  = @w@
     -> Int      -- ^ crop height = @h@
     -> Int      -- ^ X start position = @x0@
     -> Int      -- ^ Y start position = @y0@
     -> Matrix a -- ^ /size/ = @(xa,ya)@
     -> Matrix a -- ^ /size/ = @(minimum [w,xa-x0], minimum [h,xa-x0])@
crop w h pX pY = take w h . drop pX pY

-- cropMat w h pX pY = V.farm11 (crop w pX) . crop h pY
--   where crop size pos = V.drop pos . V.take (pos + size) 

-- | Groups a matrix into smaller equallly-shaped matrices.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ group 2 2 m
--   <<1,2>,<11,12>>   <<3,4>,<13,14>>
-- <<21,22>,<31,32>> <<23,24>,<33,34>>
group :: Int      -- ^ width of groups = @w@
      -> Int      -- ^ height of groups = @h@
      -> Matrix a -- ^ /size/ = @(xa,ya)@
      -> Matrix (Matrix a) -- ^ /size/ = @(xa `div` w,ya `div` h)@
group w h = V.farm11 transpose . V.group h . V.farm11 (V.group w)


-- | Returns a stencil of neighboring elements for each possible
-- element in a vector.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ stencil 2 2 m
--   <<1,2>,<11,12>>   <<2,3>,<12,13>>   <<3,4>,<13,14>>
-- <<11,12>,<21,22>> <<12,13>,<22,23>> <<13,14>,<23,24>>
-- <<21,22>,<31,32>> <<22,23>,<32,33>> <<23,24>,<33,34>>
stencil :: Int -> Int -> Matrix a -> Matrix (Matrix a)
stencil r c = arrange . groupCols . groupRows
  where
    groupRows =         V.farm11 (V.take r) . dropFromEnd r . V.tails
    groupCols = farm11 (V.farm11 (V.take c) . dropFromEnd c . V.tails)
    arrange   = V.farm11 transpose
    dropFromEnd n v = V.take (V.length v - n) v

-- | Reverses the order of elements in a matrix
reverse :: Matrix a -> Matrix a
reverse = V.reverse . V.farm11 V.reverse

-- | Pattern which "rotates" a matrix. The rotation is controled with
-- the /x/ and /y/ index arguments as following:
--
-- * @(> 0)@ : rotates the matrix right/down with the corresponding
-- number of positions.
-- 
-- * @(= 0)@ : does not modify the position for that axis.
-- 
-- * @(< 0)@ : rotates the matrix left/up with the corresponding
-- number of positions.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> pretty " " $ rotate (-1) 1 m
-- 32 33 34 31
--  2  3  4  1
-- 12 13 14 11
-- 22 23 24 21
rotate :: Int -- ^ index on X axis
       -> Int -- ^ index on Y axis
       -> Matrix a
       -> Matrix a
rotate x y = V.rotate y . V.farm11 (V.rotate x)

transpose :: Matrix a     -- ^ @X:Y@ orientation
          -> Matrix a     -- ^ @Y:X@ orientation
-- transpose = V.reduce (V.farm21 (<++>)) . farm11 V.unit   -- stack overflow!
transpose Null = Null
transpose (Null:>xss) = transpose xss
transpose rows = (V.farm11 V.first rows) :> transpose (V.farm11 V.tail rows)

-- | Replaces a part of matrix with another (smaller) part, starting
-- from an arbitrary position.
--
-- >>> let m  = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> let m1 = matrix 2 2 [101,202,303,404]
-- >>> pretty " " $ replace 1 1 m1 m
--  1   2   3  4
-- 11 101 202 14
-- 21 303 404 24
-- 31  32  33 34
replace :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
replace x y mask = replace y h (V.farm21 (\m o -> replace x w (const m) o) mask)
  where
    (w,h) = size mask
    replace start size replaceF vec
      = let begin  = V.take start vec
            middle = replaceF $ V.drop start $ V.take (start + size) vec
            end    = V.drop (start + size) vec
        in begin <++> middle <++> end

