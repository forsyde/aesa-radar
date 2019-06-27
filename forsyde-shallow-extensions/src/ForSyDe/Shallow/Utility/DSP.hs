module ForSyDe.Shallow.Utility.DSP where

import Data.Complex
import ForSyDe.Shallow.Core.Vector

-- | Return the Taylor window.
--
-- The Taylor window allows for a selectable sidelobe suppression with a 
-- minimum broadening. This window is commonly used in radar processing [1].
-- Code inspired from a <<https://github.com/scipy/scipy/pull/8032 pull request for SciPy>>.
--
-- Reference:
--
-- [1] W. Carrara, R. Goodman, and R. Majewski "Spotlight Synthetic
-- Aperture Radar: Signal Processing Algorithms" Pages 512-513, July
-- 1995.
taylor :: Int          -- ^ number of points in the output window.
       -> Int          -- ^ Number of nearly constant level sidelobes adjacent to the mainlobe
       -> Float        -- ^ Desired peak sidelobe level in decibels (db) relative to the mainlobe
       -> Vector Float -- ^ The window, with the center value normalized
                       --   to one (the value one appears only if the
                       --   number of samples is odd).
taylor n nbar level = mapV (*scale) w
  where
    -- explicit conversions to floating
    bN   = fromIntegral n
    nBar = fromIntegral nbar
    ma   = [1.0 .. nBar-1.0]
    -- calculate intermediate values
    b  = 10**((-level) / 20)
    a  = log(b + sqrt(b**2 - 1)) / pi
    s2 = nBar ** 2 / (a**2 + (nBar - 0.5)**2)
    -- functions for calculating coefficients
    fmcalc m  = let numer = (-1)**(m+1) * prod([1.0 - m**2/s2/(a**2 + (j - 0.5)**2) | j <- ma])
                    denom = 2 * prod([1 - m**2/j**2 | j <- ma, j /= m])
                in numer / denom
    ccalc m x = cos(2 * pi * x * (m - bN/2 + 1/2) / bN)
    wcalc m   = 2 * dotVV (vector $ map fmcalc ma) (vector $ map (ccalc m) ma) + 1
    -- calculate window coefficients
    w  = vector $ map (wcalc) [0..bN-1]
    -- normalize (Note that this is not described in the original text [1])
    scale = 1 / wcalc (bN - 1) / 2
    -- utility
    prod = foldr1 (*) -- calculates the product of a list

-- | Returns a Taylor window with default arguments: 4 sidelobes, and peak sidelobe level of -30dB
taylor' n = taylor 4 (-30)

-- | Calculates the dot product between two vectors.
dotVV :: Num a => Vector a -> Vector a -> a 
dotVV a b
  | lengthV a == lengthV b = reduceV (+) (zipWithV (*) a b)
  | otherwise                = error "Vector sizes must match"

-- | Higher-order version of 'dotVV'. Applies dot product on vectors of /structures/, e.g. 'Signal's.
dotVV' :: Num a
     => (f a -> f a -> f a)  -- ^ function that performs '+' operation on the structures @f a@
     -> (f a -> f a -> f a)  -- ^ function that performs '+' operation on the structures @f a@
     -> Vector (f a) -> Vector (f a) -> f a 
dotVV' plus times a b
  | lengthV a == lengthV b = reduceV plus $ zipWithV times a b
  | otherwise                = error "Vector sizes must match"

-- [0.49999997,0.85865176,1.3326946,1.7004639,1.8390236,1.7004639,1.3326946,0.85865176,0.49999997]
-- [0.49999997,0.85865176,1.3326946,1.7004639,1.8390236,1.7004639,1.3326946,0.85865176,0.49999997]


-- | Compute a Hanning window.
-- |
-- | Inspired from <<https://hackage.haskell.org/package/sdr-0.1.0.6/src/hs_sources/SDR/FilterDesign.hs>>
hanning :: (Floating n) 
        => Int -- ^ The length of the window
        -> Vector n
hanning size = vector $ map func [1..]
  where
    func idx = let i = fromIntegral idx
                   n = fromIntegral size
               in 0.5 * (1 - cos((2 * pi * i) / (n - 1)))
  
-- | Compute a Hamming window. 
-- |
-- | Inspired from <<https://hackage.haskell.org/package/sdr-0.1.0.6/src/hs_sources/SDR/FilterDesign.hs>>
hamming :: (Floating n) 
        => Int -- ^ The length of the window
        -> Vector n
hamming size =  vector $ map func [1..]
  where
    func idx = let i = fromIntegral idx
                   n = fromIntegral size
               in 0.54 - 0.46 * cos((2 * pi * i) / (n - 1))
   
-- | Compute a Blackman window.
-- |
-- | Inspired from <<https://hackage.haskell.org/package/sdr-0.1.0.6/src/hs_sources/SDR/FilterDesign.hs>>
blackman :: (Floating n) 
        => Int -- ^ The length of the window
        -> Vector n
blackman size =  vector $ map func [1..]
  where
    func idx = let i = fromIntegral idx
                   n = fromIntegral size
               in 0.42 - 0.5 * cos((2 * pi * i) / (n - 1)) + 0.08 * cos((4 * pi * i) / (n - 1))

-- | applied in reverse order (more optimized)
-- >>> let v = vector [0,0,0,0,0,1]
-- >>> let c = vector [1,2,1]
-- >>> mav c v
-- <0,0,0,1,2,1>
mav :: Num a
    => Vector a  -- ^ vector of coefficients
    -> Vector a  -- ^ input vector of numbers; /size/ = @n@
    -> Vector a  -- ^ output vector of numbers; /size/ = @n@
mav coefs = mapV applyFilter . tailsV
  where
    applyFilter = reduceV (+) . zipWithV (*) coefs 

-- -- |
-- -- >>> let c = vector [1,2,1]
-- -- >>> let s = SY.signal [1,0,0,0,0,0,0,0]
-- -- >>> fir' (SY.comb21 (+)) (\c -> SY.comb11 (*c)) (SY.delay 0) c s
-- -- {1,2,1,0,0,0,0,0}
-- fir' :: (a -> a -> a)  -- ^ process/operation replacing '+'
--      -> (c -> a -> a)  -- ^ process/operation replacing '*'
--      -> (a -> a)       -- ^ delay process
--      -> Vector c       -- ^ vector of coefficients
--      -> a              -- ^ input signal/structure 
--      -> a              -- ^ output signal/structure
-- fir' plus times delay coefs =
--   reduceV plus . zipWithV times coefs . recuriV (copyV n delay)
--   where n = lengthV coefs - 1

