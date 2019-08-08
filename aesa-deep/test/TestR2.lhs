 ### Properties

In the second refinement phase the properties target mainly the translations between
the floating point and fixed point representations. We define a new module with the
usual preamble:

> {-# LANGUAGE PackageImports #-}
> module TestR2 where
>
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SY as SY
> import "forsyde-atom-extensions" ForSyDe.Atom.MoC.SDF as SDF
> import "forsyde-atom-extensions" ForSyDe.Atom.Skeleton.Vector (farm11)
> import Test.QuickCheck
> import ForSyDe.Deep.Complex
> import ForSyDe.Deep.Fixed
>
> import Generators (sySignals, decimalCpxNum, decimalCpxRat)
> import AESA.PC.R1 as R1
> import AESA.PC.R2 as R2
> import AESA.Params (pcTap)

The first property we check is that even when wrapped (i.e. going through two
translations), the `procPC''` does not overflow its legal input/output value range,
i.e. $[-1-i,1+i)$. For this we write a local process wrapper `wrapProc`.
$$
\forall v \in \langle\mathbb{C}\rangle, a \in v, b \in \mathtt{procPC''}(v) : |v| > 0 \wedge a \in [-1-i,1+i) \Rightarrow b \in [-1-i,1+i)
$$ {#eq:prop_refine2_values}

> prop_refine2_values = forAll (sySignals decimalCpxNum)
>                       $ \s -> all (withinRangeComplex (-1) 1)
>                               $ SY.fromSignal $ wrapProc R2.procPC'' $ s
>   where
>     wrapProc p = SY.comb11 (fmap fixed20ToReal) . p . SY.comb11 (fmap realToFixed20)
>     withinRangeComplex a b c
>       | realPart c <  a = False
>       | imagPart c <  a = False
>       | realPart c >= b = False
>       | imagPart c >= b = False
>       | otherwise = True

The second property we want to test is that the cumulative quantization error stays
within a constant, accepted range. Again, we cannot use floating point numbers as
samples for comparison, due to the dynamic quantization error of their own
representation. As before, we fall back to rationals which constitute a good
model. The cumulative error for one PC'' channel, supposing that both multiplications
and addition results are truncated back to Q19, can be calculated with:

$$
\epsilon = \epsilon_{Q19} * N_{tap} = 2^{-19} \times N_{tap}
$$

> prop_refine2_error = forAll (sySignals decimalCpxRat)
>                      $ \s -> all acceptable $ zip (rationalPcVals s) (fixed20PcVals s)
>   where
>     rationalPcVals s = SY.fromSignal $ procPCRationals s
>     fixed20PcVals  s = SY.fromSignal $ procPC'' $ SY.comb11 (fmap realToFixed20) s
>     procPCRationals  = pcFIR (+:) (*:) (0:+0) (farm11 ((:+0) . realToFrac) coefsR1)
>     -----------------------------------
>     acceptable (rat, f20) = let rp = abs (realPart rat - (fixed20ToReal $ realPart f20))
>                                 ip = abs (imagPart rat - (fixed20ToReal $ imagPart f20))
>                             in rp <= epsilon && ip <= epsilon
>     epsilon = (realToFrac pcTap) * 2^^(-19)

Notice that we have instantiated a clone `procPC''` which works on rational numbers
called `procPCRationals` using the `pcFIR` process constructor. We needed this process
to compare the rational outputs against the fixed point ones.
