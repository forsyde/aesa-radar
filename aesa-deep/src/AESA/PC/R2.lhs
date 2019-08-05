 ## R2: Floating Point to Q19



> {-# LANGUAGE PackageImports #-} --can be ignored
> module AESA.PC.R2 where

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP 

> import ForSyDe.Deep.Complex
> import ForSyDe.Deep.Fixed
> import Data.Complex

> import AESA.Coefs (mkPcCoefs)
> import AESA.Params (pcTap, nb)

> import AESA.PC.R1

> coefsR2 = V.farm11 ((:+0) . realToFixed20) coefsR1

> pc'' :: Vector ( SY.Signal (Complex Fixed20))
>      -> Vector (SDF.Signal (Complex Fixed20))
> pc'' = farm11 (SY.toSDF . pcFIR2 coefsR2)

> wrapR2 f = farm11 (SDF.comb11 (1,1,(fmap . fmap) fixed20ToReal))
>            . f . farm11 (SY.comb11 (fmap realToFixed20))

> wrappedPC'' :: Vector ( SY.Signal (Complex Float))
>             -> Vector (SDF.Signal (Complex Float))
> wrappedPC'' = wrapR2 pc''


> pcFIR2 :: Num a
>       => Vector    (Complex a)
>       -> SY.Signal (Complex a)
>       -> SY.Signal (Complex a) 
> pcFIR2 coefs = fir' sumP mulP resetDly coefs
>   where
>     sumP     = SY.comb21 (+:)
>     mulP c   = SY.comb11 (*: c)
>     resetDly = SY.moore11 countReset propagate (0,0:+0)
>     ---------------------------------------------------
>     countReset :: Num a => (Int, Complex a) -> Complex a -> (Int, Complex a)
>     countReset (c,_) p | c == nb-1 = (  0,0:+0)
>                        | otherwise = (c+1,   p)
>     ---------------------------------------------------
>     propagate :: (Int, Complex a) -> Complex a
>     propagate (_,p) = p
