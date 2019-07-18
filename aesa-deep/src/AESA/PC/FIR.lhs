> {-# LANGUAGE PackageImports #-} --can be ignored
> module AESA.PC.FIR where

> import AESA.Coefs
> import AESA.Params

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP
> import "forsyde-deep-extensions" ForSyDe.Deep.Fixed
> import AESA.Coefs
> import AESA.Params
> import Data.Complex


> pc :: Fractional a => Vector ( SY.Signal a)
>    -> Vector (SDF.Signal a)
> pc = farm11 (SY.toSDF . pcFirNet nb (mkPcCoefs 5))

> pcFirNet :: Fractional a =>  Int -> Vector a -> SY.Signal a -> SY.Signal a 
> pcFirNet nb coef = fir' sumP mulP resetDly (V.reverse coef)
>   where
>     sumP     = SY.comb21 (+)
>     mulP c   = SY.comb11 (*c)
>     resetDly = SY.moore11 countReset propagate (0,0)
>     countReset (c,_) p | c == nb-1 = (0,0)
>                        | otherwise = (c+1,p)
>     propagate (_,p) = p



> procPC' :: Fractional a => Int -> Vector a -> SDF.Signal a -> SDF.Signal a 
> procPC' nb coef = SDF.comb11 (nb, nb, V.fromVector . V.reverse . fir coef . V.reverse . V.vector)

> coef = vector [1,2,3,4] :: Vector Double
> s1 = SY.signal [1,0,0,0,0,0] :: SY.Signal Double
> s2 = SDF.signal [1,0,0,0,0,0] :: SDF.Signal Double

> infixl 6 ~+, ~-
> infixl 7 ~*
> 
> pcFirNet' :: Int -> Vector (Complex Fixed20)
>           -> SY.Signal (Complex Fixed20)
>           -> SY.Signal (Complex Fixed20) 
> pcFirNet' nb coef = fir' sumP mulP resetDly (V.reverse coef)
>   where
>     sumP     = SY.comb21 (~+)
>     mulP c   = SY.comb11 (~*c)
>     resetDly = SY.moore11 countReset propagate (0,0:+0)
>     countReset (c,_) p | c == nb-1 = (0,0:+0)
>                        | otherwise = (c+1,p)
>     propagate (_,p) = p
 
> (x:+y) ~+ (x':+y') =  (x+x') :+ (y+y')
> (x:+y) ~- (x':+y') =  (x-x') :+ (y-y')
> (x:+y) ~* (x':+y') =  (x*x'-y*y') :+ (x*y'+y*x')
