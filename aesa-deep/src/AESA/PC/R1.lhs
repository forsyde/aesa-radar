 ## Refinement 1: Untimed MAV to Timed FIR

The first refinement comes from the observation that the PC block from
@sec:atom-network, although operating on streams, exposes an instantaneous, untimed
behavior over fixed sized-vectors of numbers. In other words, the SDF process
associated with a channel's PC stage is more concerned that a moving average (MAV)
algorithm is applied on a vector of $N_b$ samples as soon as they arrive, but it is
not really concerned on _how_ the MAV is pefrormed. This type of under-specification
can derive a family of possible implementations, but does not bide well with the
register transfer level (RTL) style of modeling specific to hardware implementations,
which requires a more specific total order between actions. For example, translating
the "instantaneous" MAV function (i.e. `fir (mkPcCoef pcTap)`) word-by-word into RTL
would create a terribly un-efficient and wasteful circuit! Luckily, we have already
seen a similar, more fine-grained streaming behavior for the same MAV function in
@sec:cube-int-atom, and respectively @sec:int-atom, namely the $n$-tap systolic FIR
structure created with the `fir'` skeleton[^fn:fir-skel].

[^fn:fir-skel]: in fact both `fir` and `fir'` derive from the same catamorphism. For a formal proof check [@ungureanu2019].

 ### Model

In this first refinement phase we translate the SDF process `procPC` from @sec:pc-atom
into systolic network of SY processes `pcFIR` much more appropriate and efficient for
RTL-based implementations.

> {-# LANGUAGE PackageImports #-}
> module AESA.PC.R1 where

Customary, we import the needed modules from the `ForSyDe.Atom` suite:

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector.DSP (fir')

To make sure we use exactly the same coefficients and constants as the ones used with
the high-level model in @sec:atom-network, we import them directly from the
`aesa-atom` package.

> import AESA.Coefs (mkPcCoefs)
> import AESA.Params (pcTap, nb)

Lastly, we need to import the `Complex` type. However, unlike previously, we now use
our in-house `ForSyDe.Deep.Complex` module for that, which is in fact re-exporting
`Data.Complex` along with some additional instances and utility functions which makes
it synthesizable.

> import ForSyDe.Deep.Complex

First we define the coefficients which will be used throughout all the refinement
phases. If you read the API documentation of `fir'` you will see that the order of
application of FIR tap processes is _from right to left_, thus we need to reverse the
order of PC coefficients.

> coefsR1 :: Fractional a => Vector a
> coefsR1 = V.reverse $ mkPcCoefs pcTap

Then we define the interface for the PC' stage, which is the same as for PC in
@sec:pc-atom. This means that, as far as the type checker is concerned, we can simply
replace `pc` with `pc'` in the original high level model and simulate as part of the
entire AESA signal processing chain.

> -- pc' :: Vector ( SY.Signal (Complex Float))
> --     -> Vector (SDF.Signal (Complex Float))
> -- pc' = farm11 (SY.toSDF . pcFIR coefsR1)

Finally, we define `pcFIR` as a SY `fir'` process network which instantiates a
particular timed (in the causality sense) behavior for the SDF `procPC` in
@sec:pc-atom. However, when we change the time domain of the process we lose the
information on the partial order between events. In SY there is no MoC-induced
mechanism that tells us _when_ $N_b$ events have been consumed/processed: this
mechanism needs to be hand-crafted. _One_ solution is to embed a count-reset Moore
state machine inside each delay element associated with every FIR tap. This way, each
Moore machine stores a counter value $\in [0,N_b)$ along with the previous complex
sample, and would reset its state after after propagating $N_b$ samples.

![PC' process as a FIR network](figs/fir-proc-shallow.pdf){#fig:fir-pc}

> pcFIR :: Fractional a
>       => Vector    a
>       -> SY.Signal a
>       -> SY.Signal a 
> pcFIR coefs = fir' sumP mulP resetDly coefs
>   where
>     sumP     = SY.comb21 (+)
>     mulP c   = SY.comb11 (* c)
>     resetDly = SY.moore11 countReset propagate (0,0)
>     ---------------------------------------------------
>     countReset (c,_) p | c == nb-1 = (  0,0)
>                        | otherwise = (c+1,p)
>     ---------------------------------------------------
>     propagate (_,p) = p

| Function                               | Original module                    | Package                 |
|----------------------------------------|------------------------------------|-------------------------|
| `mealy11`, `comb11`, `comb21`, `toSDF` | [`ForSyDe.Atom.MoC.SY`]            | forsyde-atom            |
| `farm11`                               | [`ForSyDe.Atom.Skeleton.Vector`]   | forsyde-atom            |
| `fir'`                                 | `ForSyDe.Atom.Skeleton.Vector.DSP` | forsyde-atom-extensions |
| `mkPcCoefs`                            | `AESA.Coefs`                       | aesa-atom               |
| `pcTap`, `nb`                          | `AESA.Params`                      | aesa-atom               |

 ### Simulation
 
As mentioned above, the PC' component can be "plugged in" and used with the AESA
signal processing chain. Please refer to the project's `README` file, respectively the
binary help menu instructions, on how to execute the model containing the PC' block
against the same test input data as the prevoius high-level models. For the sake of
space we do not include the output image, however we encourage the reader to try it
out and plot it herself. For now we can only assure you that it looks similar to
@fig:aesa-odata-atom-stream, except for the detection values, whose (slight)
differences are induced by floating point calculation errors.
