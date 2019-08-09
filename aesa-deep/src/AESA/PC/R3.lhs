 ## Refinement 3: Deep Language Embedding {#sec:synth-r3}

In this refinement phase we translate the PC'' system defined in the previous phase to
a language more appropriate for synthesis. Up until now we have used the
shallow-embedded DSLs of ForSyDe, namely ForSyDe-Atom and ForSyDe-Shallow. In this
section we will start using ForSyDe-Deep, a deep-embedded DSL, capable of extracting a
system's internal structure as a netlist and synthesizing it to different backends (at
present GraphML and VHDL).

 ### Crash course in ForSyDe-Deep{#sec:crash-deep}

ForSyDe-Deep was originally created as a replacement for ForSyDe-Shallow. It provides
a sub-set of the ForSyDe-Shallow language and, in principle, is based on the same
modeling concepts. However its deep-embedded features (such as netlist traversing and
language parsing) makes it much more verbose than its shallow counterparts. This is
why its syntax appeals less to new users, hence it became it a fall-back framework for
synthesis purposes only. Nevertheless translating from a shallow ForSyDe model to a
deep one is straightforward, once you understand the principles listed as follows.
For more on ForSyDe-Deep modeling, refer to the beginner tutorials on the ForSyDe
[homepage](https://forsyde.github.io/forsyde-deep/).

![The difference between a shallow ForSyDe system (left) and a deep one (right)](figs/deep-syntax.pdf){#fig:deep-syntax}

* process functions are parsed to their AST using a language extension of Haskell
  called TemplateHaskell. The function parser is able to recognize a subset of the
  Haskell language written between the so-called banana brackets `[d| function |]`,
  and needs to _explicitly specify its type signature_. Whatever code is written
  within the banana brackets is parsed as-is and needs to be self-contained,
  i.e. information cannot be inferred from global/system-wide definitions. In order to
  instantiate a parsable ForSyDe-Deep function from a Haskell function, it needs to be
  wrapped into a `ProcFun` type, using one of the [function
  wrappers](http://hackage.haskell.org/package/forsyde-deep-0.2.0/docs/ForSyDe-Deep-Process.html)
  provided by the API.
  
* processes are instantiated using the ForSyDe-Deep equivalents of the main
  ForSyDe-Shallow [SY process
  constructors](http://hackage.haskell.org/package/forsyde-deep-0.2.0/docs/ForSyDe-Deep-Process-SynchProc.html). The
  main difference is that they need a string identifier as well as `ProcFun`-wrapped
  functions instead of regular Haskell functions.
  
* in order to be able to simulate, parse or synthesize a process, it needs to be
  wrapped within a `SysDef` type, similarly to parsable functions, using one of the
  API-provided [system definition
  wrappers](http://hackage.haskell.org/package/forsyde-deep-0.2.0/docs/ForSyDe-Deep-System.html). These
  system wrappers, apart from a fully applied process, need also a unique string
  identifier for the new system, as well as name identifiers for all its input and
  output ports.

* in order to use a defined system hierarchically as a component, it needs to be
  [instantiated](http://hackage.haskell.org/package/forsyde-deep-0.2.0/docs/ForSyDe-Deep-System.html)
  back to a composable function. A process instance needs a unique name identifier, as
  well as a `SysDef` object.


**N.B.:** That is quite a mouthful of wrappers and, as you will see in the PC example
  below, it implies a quite some unpleasant verbosity, especially for one used to the
  elegant compactness of Haskell programs.  Future iterations of the ForSyDe language
  will most likely have a unified syntax and translation from shallow (for efficient
  simulation) to deep (for structure parsing and synthesis) versions will be done
  automatically. For now the current (type-extended) version of ForSyDe-Deep suffices
  for demonstration purposes since the modeling principles are the same no matter the
  frontend language.

 ### Model

We create a new module for this refinement phase. The `TemplateHaskell` and
`FlexibleContexts` language extensions are mandatory.

> {-# LANGUAGE PackageImports, TemplateHaskell, FlexibleContexts #-}
> module AESA.PC.R3 where

We import some ForSyDe-Atom types, only for co-simulation wrapping.

> import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import qualified "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import qualified "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V

We import the ForSyDe-Deep libraries. The `ForSyDe.Deep.Skeletons` library belongs to
the package extension built for this case study and contains a couple of deep-embedded
(synthesizable) skeletons, as presented in [@sec:atom;@sec:refine].

> import ForSyDe.Deep
> import ForSyDe.Deep.Skeleton as Sk

We need some additional type libraries. `Data.Param.FSVec` defines fixed-size vectors
as compared to the shallow vectors used until now,
e.g. `ForSyDe.Atom.Skeleton.Vector`. Fixed-size vectors have their length captured in
the type signature as type-level numerals imported from `Data.TypeLevel.Num`. For
example `(FSVec D8 a)` denotes a fixed-size vector of type a, with the size of 8
elements. We also use list functions, thus we can tell their functions apart by
loading the two libraries using different aliases.

> import Data.List as L
> import Data.Param.FSVec as FSV
> import Data.TypeLevel.Num hiding ((+), (-), (==)) 

Finally, we import some functions from the previous refinement phase, as well as some
system constants.

> import AESA.PC.R2 (coefsR2, wrapR2)
> import AESA.Params (nb)

For starters, let us define the adder process used in the `fir'` skeleton. According
to the crash course in @sec:crash-deep we need to gradually wrap its elements until it
becomes a system definition `SysDef`. First, we need to declare a `ProcFun` element
from a Haskell addition function. As already mentioned, whatever is written between
the banana brackets needs to be self-contained. This means that the `Num` type
instance of `Complex` which overloads the `(+)` operator is not of much use
here. Complex number addition needs to be explicitly written so that the parser know
what to synthesize (N.B. type class support is listed among the "todo" items of future
ForSyDe language versions).

> addFun :: ProcFun (Complex Fixed20 -> Complex Fixed20 -> Complex Fixed20)
> addFun = $(newProcFun
>            [d|addf :: Complex Fixed20 -> Complex Fixed20 -> Complex Fixed20
>               addf (x :+ y) (x' :+ y') = (x + x') :+ (y + y')  |])

The addition process is created by passing an identifier and the newly defined
`ProcFun` to the process constructor `zipWithSY` (i.e. the equivalent of `SY.comb21`
in ForSyDe-Atom).

> addProc :: Signal (Complex Fixed20)
>         -> Signal (Complex Fixed20)
>         -> Signal (Complex Fixed20)
> addProc = zipWithSY "addProc" addFun

From this process we create the component identified as `"add"`, with two input ports
`"i1"` and `"i2"` and an output port `"o1"`.

> addSys :: SysDef (  Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20) )
> addSys = newSysDef addProc "add" ["i1","i2"] ["o1"]

Similarly, we define the `"mul"` component, but this time in one go. Notice again the
expanded complex number multiplication. Instead of the normal multiplication operator
`(*)` we use the in-house `fixmul20` function, which multiplies two Q19 numbers and
truncates the result back to Q19. In the shallow simulation `fixmul20` $\equiv$ `(*)`,
but in the deep version `fixmul20` synthesizes to a custom VHDL `std_vector` function
instead of the regular VHDL `*` operator.

> mulSys :: SysDef (  Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20)
>                  -> Signal (Complex Fixed20) )
> mulSys = newSysDef (zipWithSY "mulProc" mulFun) "mul" ["i1","i2"] ["o1"]
>   where 
>     mulFun = $(newProcFun
>                [d|mulf :: Complex Fixed20 -> Complex Fixed20 -> Complex Fixed20 
>                   mulf (x :+ y) (x' :+ y') = (fixmul20 x x' - fixmul20 y y') :+
>                                              (fixmul20 x y' + fixmul20 y x') |])

The reset-delay FSM used as a delay element in the `fir'` pattern needs also to be
defined as a component. We define the `"rDelay"` component using the `mooreSY` process
constructors, taking two `ProcFun`s for the next state function `countReset` and
output decoder `propagate`. We fix the counter value type to be `Int16`. The
globally-defined `nb` (i.e. the number of range bins $N_b$) cannot be recognized as-is
inside the banana brackets, and for now all we can do is to write the number
explicitly. (N.B. support for partial function application is listed among the "todo"
items of future ForSyDe language versions).

> rDelaySys :: SysDef (Signal (Complex Fixed20) -> Signal (Complex Fixed20))
> rDelaySys = newSysDef (mooreSY "rDelayProc" countReset propagate(0, 0 :+ 0))
>             "rDelay" ["i1"] ["o1"]
>   where
>     countReset = $(newProcFun
>                    [d|cntf :: (Int16,Complex Fixed20) -> Complex Fixed20
>                            -> (Int16,Complex Fixed20) 
>                       cntf(c,_) p = if c == 1024-1
>                                     then (0, 0 :+ 0)
>                                     else (c+1,p) |])
>     propagate  = $(newProcFun
>                    [d|prpf :: (Int16,Complex Fixed20) -> Complex Fixed20 
>                       prpf (_,p) = p |])

Because partial application is not yet supported we need to instantiate the vector of
coefficients into a farm of `constSY` signal generators. We thus create the "system
constructor":

> constSys :: ProcId -> Complex Fixed20 -> SysDef (Signal (Complex Fixed20))
> constSys name c = newSysDef (constSY "const" c) name ["i1"] ["o1"]

which we then use as argument for the `app11` skeleton (which is in fact a `farm11`
tailored for partial application only) to instantiate a farm of constant signal
generators, i.e. a vector of constant signals. The coefficients are interpreted into a
`FSVec` from their shallow counterparts defined in the second refinement phase.

> coefsR3 = Sk.app11 "coef" constSys coefs
>   where
>     coefs = $(vectorTH (V.fromVector coefsR2 :: [Complex Fixed20]))

**OBS:** ForSyDe-Deep skeletons, as compared to their shallow counterparts, take care
  of _creating instances_ (see @fig:deep-syntax) of defined systems as well as
  coupling them together. This is why they need components as arguments instead of
  simple functions.

Now, for didactic purpose, let us define ForSyDe-Deep equivalent of the `fir'`
skeleton using the base catamorphisms _recur_ (or its specialization _generate_),
_farm_ and _reduce_. We call this process network constructor `deepFIR` and, as you
can see, it is defined similarly to its shallow counterpart, with a few exceptions: 1)
it expects a base identifier, to create unique IDs for all the generated component
instances; 2) it takes `SysDef` components instead of processes as arguments; 3) for
coefficients it requires a fixed-size vector of strictly positive size, containing
constants.

> deepFIR :: (SysFun (a -> a -> a), SysFun (c -> a -> a), SysFun (a -> a),
>            Pos s', Succ s' s)
>        => ProcId                -- ^ system ID
>        -> SysDef (a -> a -> a)  -- ^ process/operation replacing '+'
>        -> SysDef (c -> a -> a)  -- ^ process/operation replacing '*'
>        -> SysDef (a -> a)       -- ^ delay process
>        -> FSVec s c             -- ^ vector of coefficients
>        -> a                     -- ^ input signal/structure 
>        -> a                     -- ^ output signal/structure
> deepFIR name addSys mulSys dlySys coefs =
>   Sk.reduce addName addSys . Sk.farm21 mulName mulSys coefs . Sk.generate dlyName n dlySys
>   where n = lengthT coefs
>         dlyName = name L.++ "_dly_"
>         addName = name L.++ "_add_"
>         mulName = name L.++ "_mul_"

We finally can define a component for the deep equivalent of `procPC` using the
`deepFIR` process network constructor, by passing the above-defined components as
arguments.

> procPCSys :: SysDef (  Signal (Complex Fixed20)
>                     -> Signal (Complex Fixed20) )
> procPCSys = newSysDef (deepFIR "fir" addSys mulSys rDelaySys coefsR3)
>             "FIR" ["i1"] ["o1"]

and the deep equivalent of the PC process, statically defining its size as a
type-level numeral equal to $N_b$

> pc3 :: FSVec D8 (Signal (Complex Fixed20))
>     -> FSVec D8 (Signal (Complex Fixed20))
> pc3 = Sk.farm11 "pc" procPCSys

with its associate system definition. Unfortunately, at the moment processes of type
`FSVec a (Signal a) -> ...` are not part of the `SysFun` class, thus we cannot create
directly a `SysDef` components from `pc3`. What wee _can_ do however is to wrap `pc3`
inside a `zipx . pc . unzipx` pattern which merely transposes the vector and signal
domains, and the synthesizer will simply ignore it. (N.B. vector-based components will
be supported in future iterations of ForSyDe).

> sysPC3 = newSysDef (zipxSY "zip" . pc3 . unzipxSY "unzip") "PC3" ["i1"] ["o1"]

 ### Simulation. Synthesis

Similarly to the previous refinement stages, we further wrap the PC component in order
to co-simulate it with the rest of the AESA system. To simulate a ForSyDe-Deep
`SysDef` component we use the `simulate` function

> wrappedPC3 :: V.Vector ( SY.Signal (Complex Float))
>            -> V.Vector (SDF.Signal (Complex Float))
> wrappedPC3 = wrapR2 (wrapR3 (simulate sysPC3))

where `wrapR3` translates between the shallow ForSyDe-Atom types and the component
simulator types.

> wrapR3 sim = V.vector . L.map SDF.signal . L.transpose . L.map FSV.fromVector . sim
>              . L.map (FSV.unsafeVector d8) . L.transpose . L.map SY.fromSignal . V.fromVector

Please refer to the project's `README` file for how to execute the AESA system
alongside the deep `pc3` component. The plotted output for running the system against
the radar indata generated by the 13 objects is shown in @fig:deep-r3-odata.

![One output cube with radar data](figs/AESA_REFINE_R3.pdf){#fig:deep-r3-odata}

The PC$^{(3)}$ component is not only able to be simulated, but we can also parse its
internal structure. Here we call the command which dumps the internal structure of
`sysPC3` as hierarchical GraphML files, as seen in the plots in @fig:deep-gramhml-r3.

> graphmlPC3 = writeGraphML sysPC3

![Dumped GraphML structure: PC (left); FIR (above right); rDelay (below right).](figs/graphMLR3.png){#fig:deep-gramhml-r3}

Of course, the whole point of writing ForSyDe-Deep is to synthesize to backend code,
in this case to synthesizable VHDL code. The following command generates a VHDL
project folder with the PC architecture files.

> vhdlPC3 = writeVHDLOps (defaultVHDLOps {debugVHDL = VHDLVerbose}) sysPC3

Furthermore, one can simulate the VHDL files using ModelSim (or alternatively Ghdl),
provided their binaries are found in the system `PATH`. The following function can be
wrapped as `wrappedPC3` instead of `simulate sysPC3`.

> vhdlSim = writeAndModelsimVHDL Nothing sysPC3

Finally, we synthesize `sysPC3` to FPGA using the Quartus II tool suite (provided its
binaries are found in `PATH`), e.g.:

> quartusPC3 = writeVHDLOps vhdlOps sysPC3
>   where vhdlOps    = defaultVHDLOps{execQuartus=Just quartusOps}
>         quartusOps = checkSynthesisQuartus

After synthesis it generated the RTL structures in @fig:deep-quartus-r3, with the
specifications in @tbl:deep-spec-r3.

![Screenshots of synthesized RTL components: FIR (above); PC (below right); rDelay and mul (below left).](figs/RTLR3.png){#fig:deep-quartus-r3}


|                                    |               |
| ---------------------------------- | ------------- |
| Top-level Entity Name              | PC3           |
| Family                             | Cyclone IV GX |
| Total logic elements               | 3,014         |
| Total combinational functions      | 3,014         |
| Dedicated logic registers          | 976           |
| Total registers                    | 976           |
| Total memory bits                  | 0             |
| Embedded Multiplier 9-bit elements | 160           |
| Total PLLs                         | 0             |

Table: Specifications of generated FPGA design {#tbl:deep-spec-r3}

