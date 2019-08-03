> {-# LANGUAGE PackageImports #-} --can be ignored
> module AESA.PC.R1 where

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP

> import AESA.Coefs (mkPcCoefs)
> import AESA.Params (nTap, nb)
> import Data.Complex
> import ForSyDe.Deep.Complex


> coefsR1 = V.reverse $ mkPcCoefs nTap

> pc' :: Vector ( SY.Signal (Complex Float))
>     -> Vector (SDF.Signal (Complex Float))
> pc' = farm11 (SY.toSDF . pcFIR coefsR1)

> pcFIR :: Num a
>       => Vector    (Complex a)
>       -> SY.Signal (Complex a)
>       -> SY.Signal (Complex a) 
> pcFIR coefs = fir' sumP mulP resetDly coefs
>   where
>     sumP     = SY.comb21 (+:)
>     mulP c   = SY.comb11 (*: c)
>     resetDly = SY.moore11 countReset propagate (0,0:+0)
> --     countReset :: Num a => (Int, Complex a) -> Complex a -> (Int, Complex a)
>     countReset (c,_) p | c == nb-1 = (0,0:+0)
>                        | otherwise = (c+1,p)
>     propagate (_,p) = p
