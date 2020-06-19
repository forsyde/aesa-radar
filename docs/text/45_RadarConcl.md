## Conclusions

We have introduced a new MoC for describing continuous dynamics as functions of time,
and a new layer for describing probability distributions as functions of a random
number generator. Both have in common the fact that they represent models as pure
functions by virtue of the functional programming host, Haskell, in which functions
are first class citizens. This way their model remains mathematical and pure, by
abstracting away their machine implementation, i.e. time representation, respectively
pseudo-random number generator, these aspects becoming apparent only when sampling and
tracing a simulation.

Using these new EDSLs, we have described the radar environment for a certain
simulation scenario, by modeling the object reflection dynamics as continuous signals,
drowned in white noise. When generating and dumping the generated antenna samples, we
obtain a similar picture to the one in @fig:aesa-indata, respectively the AESA processed output looks similar to @fig:aesa-odata-atom.

\clearpage
