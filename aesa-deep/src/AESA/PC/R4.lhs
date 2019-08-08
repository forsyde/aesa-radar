 ## R4: Balancing the FIR Reduction

> {-# LANGUAGE PackageImports, TemplateHaskell, FlexibleContexts #-} --can be ignored
> module AESA.PC.R4 where

> import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import qualified "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V

> import Data.TypeLevel.Num.Sets
> import Data.TypeLevel.Num.Aliases
> import Data.TypeLevel.Num.Reps
> import qualified Data.TypeLevel.Num.Ops as TO 
> import Data.Param.FSVec as FSV
> import Data.List as L
> import ForSyDe.Deep
> import ForSyDe.Deep.Skeleton
> import Data.Complex

> import AESA.PC.R2 as R2
> import AESA.PC.R3 as R3
> import AESA.Params (nb)

> balancedFIR name addSys mulSys dlySys coefs =
>   logReduceV addName addSys . farm21V mulName mulSys coefs . generateV dlyName n dlySys
>   where --n = lengthT coefs
>         n = lengthT coefs
>         dlyName = name L.++ "_dly_"
>         addName = name L.++ "_add_"
>         mulName = name L.++ "_mul_"

> pcFIR' :: Signal (Complex Fixed20)
>       -> Signal (Complex Fixed20) 
> pcFIR' = deepFIR "fir" addSys mulSys rDelaySys coefsR3

> pc4 :: FSVec D8 (Signal (Complex Fixed20))
>        -> FSVec D8 (Signal (Complex Fixed20))
> pc4 = farm11V "pc" (newSysDef pcFIR' "FIR" ["i1"] ["o1"])

> sysPC4 = newSysDef (zipxSY "zip" . pc4 . unzipxSY "unzip") "PC" ["i1"] ["o1"]

> wrappedPC4 = wrapR2 (wrapR3 (simulate sysPC3))

> graphmlPC4 = writeGraphMLOps (defaultGraphMLOps {yFilesMarkup = True})  sysPC4

> vhdlPC4 = writeVHDL sysPC4
