 ## R4: Balancing the FIR Reduction. Minimizing the circuit.

The final refinement phase consists in performing some simple semantic-preserving
transformations on the previous model in order to optimize the efficiency of the
generated circuit.

Two (rather evident) optimizations are considered in this phase:

* as seen in [@fig:deep-gramhml-r3;@fig:deep-quartus-r3], as well as in @fig:fir-pc,
  the final stage of the `pcFIR`-instantiated process network consists of a reduction
  pattern. This pattern is instantiated recursively, by taking the result of the
  previous addition and applying it to the next one, thus generating a linear ($O(n)$)
  reduction tree. However, the base operation performed, i.e. addition, is
  commutative, and a well-known catamorphism theorem (see @ungureanu2019 for more
  details) states that the reduction can in fact be pefrormed as a balanced
  logarithmic (($O(\log n)$)) tree which means, in the case of digital hardware
  designs, a shorter combinational critical path.

* having a counter in each delay element, although elegant and modular, is a terible
  waste of silicon when considering that all counters are counting the same thing: how
  many samples have passed. A more reasonable design would take advantage of the
  identical part in all the `"rDelay"` components (i.e. the counter), and separate
  from the non-identical part (i.e. the delay register).

 ### Model

The module for the forth refinement module is the following.

> {-# LANGUAGE PackageImports, TemplateHaskell, FlexibleContexts #-} 
> module AESA.PC.R4 where

We import the same library as before:

> import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import qualified "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import ForSyDe.Deep
> import ForSyDe.Deep.Int
> import ForSyDe.Deep.Skeleton
> import Data.List as L
> import Data.Param.FSVec as FSV
> import Data.TypeLevel.Num hiding ((+), (-), (==))
> 
> import AESA.PC.R3 

As of the second bullet point above, we define one reset counter `rCounter`.

> rCounterSys :: SysDef (Signal (Complex Fixed20) -> Signal Bool)
> rCounterSys = newSysDef (mooreSY "rCounterProc" count reset (1024-1))
>               "rCounter" ["i1"] ["o1"]
>   where
>     count = $(newProcFun
>                [d|cntf :: Int16 -> Complex Fixed20 -> Int16 
>                   cntf c _ = if c == 0 then 1024-1 else c-1 |])
>     reset = $(newProcFun
>                [d|rstf :: Int16 -> Bool 
>                   rstf c = if c==0 then True else False |])

and a resettable delay (state) element

> delaySys :: SysDef (Signal Bool -> Signal (Complex Fixed20) -> Signal (Complex Fixed20))
> delaySys = newSysDef (scanld2SY "rDelayProc" reset (0:+0))
>            "rDelay" ["rst","i1"] ["o1"]
>   where
>     reset = $(newProcFun
>                [d|rstd :: Complex Fixed20 -> Bool -> Complex Fixed20 -> Complex Fixed20 
>                   rstd _ r p = if r then (0:+0) else p |])

To instantiate the logarithmic reduction tree, it is enough to replace the `reduce`
skeleton with the `logReduce` skeleton in the previous `deepFIR` process network
constructor. Furthermore, we decouple the reset counter to be outside the FIR network
thus we pass the reset signal as an argument to the network constructor.

> balancedFIR name addSys mulSys dlySys coefs rstSys =
>   logReduceV addName addSys . farm21V mulName mulSys coefs . generateV dlyName n (dlySys rstSys)
>   where n = lengthT coefs
>         dlyName = name L.++ "_dly_"
>         addName = name L.++ "_add_"
>         mulName = name L.++ "_mul_"

> pcFIR' :: Signal (Complex Fixed20)
>        -> Signal (Complex Fixed20) 
> pcFIR' s = balancedFIR "fir" addSys mulSys delaySys coefsR3 rstSig s
>   where
>     rstSig = (instantiate "rCount" rCounterSys) s

> pc4 :: FSVec D8 (Signal (Complex Fixed20))
>        -> FSVec D8 (Signal (Complex Fixed20))
> pc4 = farm11V "pc" (newSysDef pcFIR' "FIR" ["i1"] ["o1"])

> sysPC4 = newSysDef (zipxSY "zip" . pc4 . unzipxSY "unzip") "PC" ["i1"] ["o1"]

> wrappedPC4 = wrapR2 (wrapR3 (simulate sysPC3))

> graphmlPC4 = writeGraphMLOps (defaultGraphMLOps {yFilesMarkup = True})  sysPC4

> vhdlPC4 = writeVHDL sysPC4
