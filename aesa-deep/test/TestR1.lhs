 ### Properties

The main property we want to test now is that the process `procPC'` defined above is
_sequence equivalent_ with the original `procPC` from @sec:pc-atom and hence, as of
`prop_pc_transf_equiv` defined in @eq:prop_dbf_transf_equiv, the PC' stage is an exact
_functional_ replacement for the high-level PC operating on video cubes. 

We define a new module as part of these refinements' test suite:

> {-# LANGUAGE PackageImports #-}
> module TestR1 where

We need to be able to pack/unpack signals, so we import the `SY` and respectively `SDF` modules.

> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF

We import the QuickCheck DSL, along with a couple of local utilities, mainly arbitrary
data type generators.

> import Test.QuickCheck
> import Generators (largeSdfSigs, rationals)

Finally, we import the DUTs: the SDF `procPC` from the high-level streamed model and
the SY `procPC'` from Refinement 1.

> import AESA.StreamsAtom as M1
> import AESA.PC.R1 as R1

The property we need to test is
$$
\forall c \in \mathbb{C} \Rightarrow
\Sigma(\mathtt{procPC}(\overline{c})) =
\Sigma(\mathtt{procPC'}(\overline{c}))
$$ {#eq:prop_refine1_equiv}


In order to test sequence equivalence between the two processes we need to take into
account two very important matters:

1. a SDF process' output signal contains a number of elements which is a multiple of
   its production rate, whereas a SY process does not have this restriction. Sequence
   equivalence means that both output signals have the same initial segments, property
   which is handled by the `zip` list function.

1. floating point numbers _cannot_ be used to test for equality, especially now since
   the order of operations is different. Luckily we have defined both `procPC` and
   `procPC'`, as well as their coefficient vectors as polymorphic, thus as a practical
   alternative `Complex Float` (i.e. $\mathbb{C}$), we can use `Rational` numbers
   (i.e. $\mathbb{Q}$) which have a strict notion of equality. Sequence equivalence
   over $\mathbb{Q}$ is sufficient to later demonstrate through inference
   @eq:prop_refine1_equiv.

> prop_refine1_equiv = forAll (largeSdfSigs rationals) $ \s -> equiv s (SDF.toSY1 s)
>   where
>     equiv sdf sy = all (\(a,b) -> a == b) $ zip
>                    (SDF.fromSignal $ M1.procPC sdf)
>                    ( SY.fromSignal $ R1.procPC' sy)

When running the test suite (refer to the `README` instructions) `prop_refine1_equiv`
passes all tests, which means that the two behaviors are compatible and PC' can be
used as source for further refinements.
