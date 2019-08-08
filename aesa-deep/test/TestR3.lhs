 ### Properties

The main property we want to check now is that the ForSyDe-Deep version of PC$^{(3)}$
is the same as PC'' defined in the second rfinement phase.

> {-# LANGUAGE PackageImports #-}
> module TestR3 where
>
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import Test.QuickCheck
> import ForSyDe.Deep
>
> import Generators (largeSySigs, cpxFixed20)
> import AESA.PC.R2 as R2
> import AESA.PC.R3 as R3

Since fixed point numbers can be compared exactly, we simply use the processes as they
are, operating on signals of `Fixed20` numbers to test the property

$$
\forall c \in \mathbb{C} \Rightarrow
\Sigma(\mathtt{procPC''}(\overline{c})) =
\Sigma(\mathtt{simulate (procPCSys)}(\overline{c}))
$$ {#eq:prop_refine3_equiv}

> prop_refine3_equiv = forAll (largeSySigs cpxFixed20)
>                       $ \s -> all (\(a,b) -> a == b) $ zip
>                               (SY.fromSignal $ R2.procPC'' s)
>                               (simulate R3.procPCSys $ SY.fromSignal s)

Since the test passes flawlessly we can conclude that in the third refinement phase we
have achieved semantic equivalence between the components written in the two different
languages.
