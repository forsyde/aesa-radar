 ## R3: Deep Language Embedding

> {-# LANGUAGE PackageImports, TemplateHaskell, FlexibleContexts #-} --can be ignored
> module AESA.PC.R3 where

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
> import ForSyDe.Deep.Int
> import Data.Complex

> import AESA.PC.R2 as R2
> import AESA.Params (nb)
> nb' = intToInt16 nb
> -- nb' = fromIntegral nb

> addFun :: ProcFun (Complex Fixed20 -> Complex Fixed20 -> Complex Fixed20)
> addFun = $(newProcFun
>            [d|addf :: Complex Fixed20 -> Complex Fixed20 -> Complex Fixed20
>               addf (x :+ y) (x' :+ y') = (x + x') :+ (y + y')  |])

> addProc :: Signal (Complex Fixed20)
>         -> Signal (Complex Fixed20)
>         -> Signal (Complex Fixed20)
> addProc = zipWithSY "addProc" addFun

> addSys :: SysDef (  Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20) )
> addSys = newSysDef addProc "add" ["i1","i2"] ["o1"]


> mulSys :: SysDef (  Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20) )
> mulSys = newSysDef (zipWithSY "mulProc" mulFun) "mul" ["i1","i2"] ["o1"]
>   where 
>     mulFun = $(newProcFun
>                [d|mulf :: Complex Fixed20 -> Complex Fixed20 -> Complex Fixed20 
>                   mulf (x :+ y) (x' :+ y') = (fixmul20 x x' - fixmul20 y y') :+
>                                              (fixmul20 x y' + fixmul20 y x') |])

> rDelaySys :: SysDef (Signal (Complex Fixed20) -> Signal (Complex Fixed20))
> rDelaySys = newSysDef (mooreSY "rDelayProc" countReset propagate (0, 0 :+ 0))
>             "rDelay" ["i1"] ["o1"]
>   where
>     countReset = $(newProcFun
>                    [d|cntf :: (Int16,Complex Fixed20) -> Complex Fixed20
>                            -> (Int16,Complex Fixed20) 
>                       cntf (c,_) p = if c == 1024-1      -- = nb', but templateHaskell does not recognize it 
>                                      then (0, (:+) 0 0)
>                                      else (c+1,p) |])
>     propagate  = $(newProcFun
>                    [d|prpf :: (Int16,Complex Fixed20) -> Complex Fixed20 
>                       prpf (_,p) = p |])

> constSys :: ProcId -> Complex Fixed20 -> SysDef (Signal (Complex Fixed20))
> constSys name c = newSysDef (constSY "const" c) name ["i1"] ["o1"]

> coefsR3 = app11V "coef" constSys coefs
>   where
>     coefs = $(vectorTH (V.fromVector coefsR2 :: [Complex Fixed20]))

> deepFIR :: (SysFun (a -> a -> a), SysFun (c -> a -> a), SysFun (a -> a),
>            Pos s', TO.Succ s' s)
>        => ProcId                -- ^ system ID
>        -> SysDef (a -> a -> a)  -- ^ process/operation replacing '+'
>        -> SysDef (c -> a -> a)  -- ^ process/operation replacing '*'
>        -> SysDef (a -> a)       -- ^ delay process
>        -> FSVec s c             -- ^ vector of coefficients
>        -> a                     -- ^ input signal/structure 
>        -> a                     -- ^ output signal/structure
> deepFIR name addSys mulSys dlySys coefs =
>   reduceV addName addSys . farm21V mulName mulSys coefs . generateV dlyName n dlySys
>   where --n = lengthT coefs
>         n = lengthT coefs
>         dlyName = name L.++ "_dly_"
>         addName = name L.++ "_add_"
>         mulName = name L.++ "_mul_"

> pcFIR :: Signal (Complex Fixed20)
>       -> Signal (Complex Fixed20) 
> pcFIR = deepFIR "fir" addSys mulSys rDelaySys coefsR3

> pc''' :: FSVec D8 (Signal (Complex Fixed20))
>       -> FSVec D8 (Signal (Complex Fixed20))
> pc''' = farm11V "pc" (newSysDef pcFIR "FIR" ["i1"] ["o1"])


> sysPC''' = newSysDef (zipxSY "zip" . pc''' . unzipxSY "unzip") "PC" ["i1"] ["o1"]

> wrapR3 sim = V.vector . L.map SDF.signal . L.transpose . L.map FSV.fromVector . sim
>              . L.map (FSV.unsafeVector d8) . L.transpose . L.map SY.fromSignal . V.fromVector

> wrappedPC''' :: V.Vector ( SY.Signal (Complex Float))
>              -> V.Vector (SDF.Signal (Complex Float))
> wrappedPC''' = wrapR2 (wrapR3 (simulate sysPC'''))

> graphmlPC''' = writeGraphMLOps (defaultGraphMLOps {yFilesMarkup = True})  sysPC'''

> vhdlPC''' = writeVHDLOps (defaultVHDLOps {debugVHDL = VHDLVerbose}) sysPC'''

