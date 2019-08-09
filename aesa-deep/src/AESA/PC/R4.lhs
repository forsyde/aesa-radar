 ## R4: Balancing the FIR Reduction {#sec:synth-r4}

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
  details) states that the reduction can in fact be performed as a balanced
  logarithmic ($O(\log n)$) tree which means, in the case of digital hardware
  designs, a shorter combinational critical path.

* having a counter in each delay element, although elegant and modular, is a terrible
  waste of silicon when considering that all counters are counting the same thing: how
  many samples have passed. A more reasonable design would take advantage of the
  identical part in all the `"rDelay"` components (i.e. the counter), and separate
  from the non-identical part (i.e. the delay register).

The second optimization is identified and performed automatically by the Quartus
compiler, hence the specifications in @tbl:deep-spec-r3 includes only one counter
circuit for all the delay elements. Therefore we will only focus on reduction tree
balancing and leave the functional decoupling of the reset-counter as an exercise.

 ### Model

The module for the forth refinement module is the following.

> {-# LANGUAGE PackageImports, TemplateHaskell, FlexibleContexts #-} 
> module AESA.PC.R4 where

We import the same library as before:

> import ForSyDe.Deep
> import ForSyDe.Deep.Skeleton as Sk
> import Data.List as L
> import Data.Param.FSVec as FSV
> import Data.TypeLevel.Num hiding ((+), (-), (==))
> 
> import AESA.PC.R2 (wrapR2)
> import AESA.PC.R3 (wrapR3, coefsR3, addSys, mulSys, rDelaySys)

To instantiate the logarithmic reduction tree, it is enough to replace the `reduce`
skeleton with the `logReduce` skeleton in the previous `deepFIR` process network
constructor. 

> balancedFIR name addSys mulSys dlySys coefs =
>   Sk.logReduce  (name L.++ "_add_") addSys
>   . Sk.farm21   (name L.++ "_mul_") mulSys coefs
>   . Sk.generate (name L.++ "_dly_") n dlySys
>   where n = lengthT coefs

The new `procPC` system definition is created using the skeleton above. 

> procPCSys' :: SysDef (  Signal (Complex Fixed20)
>                      -> Signal (Complex Fixed20) ) 
> procPCSys' = newSysDef (balancedFIR "fir" addSys mulSys rDelaySys coefsR3)
>              "FIR" ["i1"] ["o1"]    

We finally create the refined version of the AESA PC$^{(4)}$ signal processing stage

> pc4 :: FSVec D8 (Signal (Complex Fixed20))
>     -> FSVec D8 (Signal (Complex Fixed20))
> pc4 = Sk.farm11 "pc" procPCSys' 

which is then declared as a component.

> sysPC4 = newSysDef (zipxSY "zip" . pc4 . unzipxSY "unzip") "PC4" ["i1"] ["o1"]

 ### Simulation. Synthesis

Similar to previous refinement phases, we wrap the PC$^{(4)}$ in order to co-simulate
it with the high-level AESA model. As expected, the AESA simulation using this
component gives exactly the same results as the previous simulation, shown in
@fig:deep-r3-odata.

> wrappedPC4 = wrapR2 (wrapR3 (simulate sysPC4))

Dumping the internal structure of PC$^{(4)}$ shows in @fig:deep-gramhml-r4 a more
compact FIR structure, with a shorter combinational depth, as well as one `"rCount"`
component fanning out into all the delay elements.

> graphmlPC4 = writeGraphMLOps (defaultGraphMLOps {yFilesMarkup = True})  sysPC4


The VHDL code can be generated

> vhdlPC4 = writeVHDL sysPC4

simulated

> vhdlSim = writeAndModelsimVHDL Nothing sysPC4
> -- vhdlSim' = writeAndGhdlVHDL Nothing sysPC4 -- alternatively

or synthesized.

> quartusPC4 = writeVHDLOps vhdlOps sysPC4
>   where vhdlOps    = defaultVHDLOps{execQuartus=Just quartusOps}
>         quartusOps = checkSynthesisQuartus

The generated circuit has exactly the same size as the previous one
(@tbl:deep-spec-r4), however, the combinational depth is visibly smaller, as seen in
the RTL plot in @fig:deep-quartus-r4.


![Dumped GraphML structure of the new FIR component](figs/GML_FIR_R4.png){#fig:deep-gramhml-r4}

![Screenshot of the RTL view of FIR](figs/RTLR4.png){#fig:deep-quartus-r4}

|                                    |               |
| ---------------------------------- | ------------- |
| Top-level Entity Name              | PC4           |
| Family                             | Cyclone IV GX |
| Total logic elements               | 3,014         |
| Total combinational functions      | 3,014         |
| Dedicated logic registers          | 976           |
| Total registers                    | 976           |
| Total memory bits                  | 0             |
| Embedded Multiplier 9-bit elements | 160           |
| Total PLLs                         | 0             |

Table: Specifications of generated FPGA design {#tbl:deep-spec-r4}
