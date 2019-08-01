> {-# LANGUAGE PackageImports #-} --can be ignored
> module AESA.PC.R2 where

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V

> import ForSyDe.Deep.Complex
> import ForSyDe.Deep.Fixed

> import AESA.PC.R1

> coefsR2 = V.farm11 (fmap realToFixed20) coefsR1

> pc'' :: Vector ( SY.Signal (Complex Fixed20))
>      -> Vector (SDF.Signal (Complex Fixed20))
> pc'' = farm11 (SY.toSDF . pcFIR coefsR2)

> wrapR2 f = farm11 (SDF.comb11 (1,1,(fmap . fmap) fixed20ToReal))
>            . f . farm11 (SY.comb11 (fmap realToFixed20))

> wrappedPC'' :: Vector ( SY.Signal (Complex Float))
>             -> Vector (SDF.Signal (Complex Float))
> wrappedPC'' = wrapR2 pc''

