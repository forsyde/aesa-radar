 ### Data Generators {#sec:prop-gens}

The data generators used to formulate pre-conditions in this section, as well as a
couple of utility functions are defined in this in-house module found at
`aesa-atom/tests`. The documentation for each function is provided as in-line
comments.


> {-# LANGUAGE PackageImports #-}
> module Generators where
> 
> import Test.QuickCheck as QC
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import qualified ForSyDe.Atom.Skeleton.Vector.Matrix as M
> import qualified ForSyDe.Atom.Skeleton.Vector.Cube as C
> import ForSyDe.Atom.MoC.Stream
> import Data.Complex
> import Data.List as L

> -- | Generator for complex numbers within the range $[-1-i,1+i)$
> decimalCpxNum :: Gen (Complex Float)
> decimalCpxNum = do
>   realPart <- choose (-1,0.99999999999)
>   imagPart <- choose (-1,0.99999999999)
>   return (realPart :+ imagPart)


> -- | Generates non-null vectors, i.e. which satisfy 'forall v . length v > 0'.
> nonNullVector :: Gen a -> Gen (Vector a)
> nonNullVector a = do
>   ld <- listOf a `suchThat` (not . L.null)
>   return $ V.vector ld 

> -- | Generator for vector of fixed size.
> sizedVector :: Int -> Gen a -> Gen (Vector a)
> sizedVector n a = do
>   v <- QC.vectorOf n a
>   return $ V.vector v

> -- | Generator for cube of fixed size.
> sizedCube :: Int -> Int -> Int -> Gen a -> Gen (C.Cube a)
> sizedCube z y x  = sizedVector z . sizedVector y . sizedVector x

> -- | Generator for a signal of (small) regular cubes
> sigOfCubes :: Gen a -> Gen (SY.Signal (C.Cube a))
> sigOfCubes g = do
>   x <- choose (2, 20)  -- do not choose too large dimensions otherwise
>   y <- choose (2, 20)  -- the tests will take too long... small tests are
>   z <- choose (2, 20)  -- good enough
>   sigData <- listOf1 $ sizedCube z y x g
>   return (SY.signal sigData)

> -- | Generator for an impulse signal of (small) cubes:
> impulseSigOfCubes :: Int -> Gen (SY.Signal (C.Cube Int))
> impulseSigOfCubes n = do
>   x <- choose (2, 20)  -- do not choose too large dimensions otherwise
>   y <- choose (2, 20)  -- the tests will take too long... small tests are
>   z <- choose (2, 20)  -- good enough
>   impulse <- sizedCube z y x $ elements [1] 
>   trail   <- sizedCube z y x $ elements [0]
>   return (SY.signal (impulse : replicate n trail))

> -- | The default 'Arbitrary' instance for the different ForSyDe-Atom types used in
> -- the report.
> instance Arbitrary a => Arbitrary (V.Vector a) where
>   arbitrary = do
>     x <- arbitrary
>     return (V.vector x)
> 
> instance Arbitrary a => Arbitrary (Stream a) where
>   arbitrary = do
>     x <- arbitrary
>     return (stream x)
> 
> instance Arbitrary a => Arbitrary (SY.SY a) where
>   arbitrary = do
>     x <- arbitrary
>     return (SY.SY x)
>     
> instance Arbitrary a => Arbitrary (SDF.SDF a) where
>   arbitrary = do
>     x <- arbitrary
>     return (SDF.SDF x)

> -- | Utility which tests whether a complex number is within the range $[-1-i,1+i)$
> withinRangeComplex :: Ord a => a -> a -> Complex a -> Bool
> withinRangeComplex a b c
>   | realPart c <  a = False
>   | imagPart c <  a = False
>   | realPart c >= b = False
>   | imagPart c >= b = False
>   | otherwise = True

