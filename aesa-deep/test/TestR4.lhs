 ### Properties

Here we test that the ForSyDe-Deep version of PC$^{(4)}$ is the same
as PC$^{(3)}$ defined in the third phase.

> {-# LANGUAGE PackageImports #-}
> module TestR4 where
>
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import Test.QuickCheck
> import ForSyDe.Deep
>
> import Generators (largeSySigs, cpxFixed20)
> import AESA.PC.R3 as R3
> import AESA.PC.R4 as R4

The property
$$
\forall c \in \mathbb{C} \Rightarrow
\Sigma(\mathtt{procPCSys} (\overline{c})) =
\Sigma(\mathtt{procPCSys'}(\overline{c}))
$$ {#eq:prop_refine4_equiv}

is tested by the QuickCheck program

> prop_refine4_equiv = forAll (largeSySigs cpxFixed20)
>                       $ \s -> all (\(a,b) -> a == b) $ zip
>                               (simulate R3.procPCSys  $ SY.fromSignal s)
>                               (simulate R4.procPCSys' $ SY.fromSignal s)

As all tests are passing (check the project's `README` file on how to run tests),
hence we can conclude that PC$^{(4)}$ is a good replacement for the AESA PC stage.
