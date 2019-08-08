 ## Refinement 2: Floating Point to Q19

Due to it widespread usage with general purpose computing, IEEE floating point number
representation is the de-facto standard for non-integer numerical
applications. However, although common and even taken for granted in CPUs, floating
point units are in general much larger and more expensive circuits than their integer
counterparts. Moreover, judging by the application specification in
@sec:video-chain-spec, synthesizing floating point arithmetics on FPGA would be
overkill, because:

* we do not need a large span of numbers. On the contrary, the specification says that
  at the PC stage the samples are within $[-1-i,1+i)$, i.e. only decimals;
  
* according to @tbl:sizes, for the calculations within PC we do not need a precision
  higher than 20 bits.

Driven by these statements, we decide that a much more appropriate number
representation for synthesizing the PC signal processing stage to FPGA is the fixed
point Q19 representation which, under the hood, consists simply in integer
calculations. We thus refine further the PC' stage as to operate on complex fixed
point Q19 numbers instead of complex floating point numbers, or more specific _refine
only its function layer_ to operate on this type of numbers.

 ### Model

We code the second refinement in its own module.

> {-# LANGUAGE PackageImports #-} --can be ignored
> module AESA.PC.R2 where

We import the ForSyDe-Atom libraries that are used:

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY  as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector as V

We import the `Complex` type. However, unlike previously, we now use our in-house
`ForSyDe.Deep.Complex` module. This module is in fact re-exporting `Data.Complex`
along with some additional instances which makes it synthesizable, plus a couple of
custom arithmetic operators `(+:)`, `(-:)`, `(*:)` which do not force the base type of
`Complex a` to be a part of `RealFloat` class (see [`Data.Complex`
API](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Complex.html)), but
rather any `Num` type.

> import ForSyDe.Deep.Complex

We import our in-house fixed point data types. This module exports the usual types
such as `Fixed8`, `Fixed16`, `Fixed32` and `Fixed64`. For this report we have extended
the `forsyde-deep` package with a custom (synthesizable) `Fixed20` type, hard-coded
across the full compiler stack. Future versions of ForyDe-Deep will most likely
support arbitrary-precision fixed point types using type-level numerals instead,
similar to `FSVec`, but for simple demonstration purposes `Fixed20` suffices at the moment.

> import ForSyDe.Deep.Fixed

Finally, we import the functions defined in the previous refinement stage.

> import AESA.PC.R1 (coefsR1, pcFIR)

We translate the floating point FIR coefficients from the first refinement phase to
Q19 complex numbers.

> coefsR2 = V.farm11 ((:+0) . realToFixed20) coefsR1 :: Vector (Complex Fixed20)

For `procPC''` we use the previously defined `pcFIR` process constructor whose
function layer entities are now replaced with operations over `Complex Fixed20`
numbers. This eases the validation process: we now need to test only that the function
layer refinements respect the specifications, and not the whole system.

> procPC'' :: SY.Signal (Complex Fixed20)
>          -> SY.Signal (Complex Fixed20)
> procPC'' = pcFIR (+:) (*:) (0:+0) coefsR2

The PC'' stage process (network) is the same one as in the previous refinement, except
that it uses the `procPC''` as the base process.

> pc'' :: Vector ( SY.Signal (Complex Fixed20))
>      -> Vector (SDF.Signal (Complex Fixed20))
> pc'' = farm11 (SY.toSDF . procPC'')

 ### Simulation

In order to be able to "plug in" PC'' into the AESA signal processing system, we need
to wrap it so that its type signature is the one expected. We thus define the
following wrapper which translates the floating point numbers fed into PC to fixed
point Q19 and back:

> wrapR2 f = farm11 (SDF.comb11 (1,1,(fmap . fmap) fixed20ToReal))
>            . f . farm11 (SY.comb11 (fmap realToFixed20))

which is then used to wrap `pc''` so that it looks like `pc`.

> wrappedPC'' :: Vector ( SY.Signal (Complex Float))
>             -> Vector (SDF.Signal (Complex Float))
> wrappedPC'' = wrapR2 pc''

The effect of the refined PC'' signal processing stage can be observed by simulating
the AESA application instantiating `wrappedPC''`. Please refer to the project's
`README` file on how to execute the program. When plotting the results against the
same input data, we can see that the same 13 objects are detected, albeit having
different numerical values.
