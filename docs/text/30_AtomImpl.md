## The ForSyDe-Atom Implementation of the AESA Model

[ForSyDe-Atom](https://forsyde.github.io/forsyde-atom/) is a spin-off
of [ForSyDe-Shallow](https://forsyde.github.io/forsyde-atom/) which
explores new modeling concepts, and has a fundamentally different
approach to how models are described and instantiated. Historically,
it has been the "playground" for developing and applying modeling
concepts such as algorithmic skeletons (parallel patterns) and
applicative-style modeling in ForSyDe. For more information on the
main concepts behind ForSyDe-Atom see [@ungureanu17]. From the point
of view of user experience though, the API is pretty much identical to
ForSyDe-Shallow's, with the following two main differences:

 * the user has more control of which libraries are
   imported. Functions are not distinguished by their suffix any
   longer, which means that multiple libraries export functions which
   deliberately share the same name. As such, the suggested
   programming style for mixed-library designs is to alias the
   imported library (e.g. `import ForSyDe.Atom.MoC.SY as SY`) and
   reference the function using the alias as a prefix (e.g. `SY.mealy`
   instead of `mealySY` in ForSyDe-Shallow).

 * some of ForSyDe's "canonical" names for process constructors
   inspired from functional programming have been replaced with more
   suggestive names inspired from component-based modeling, which are
   denoting common building blocks, relevant to their domain. For
   example `mapSY` is now called `SY.comb11`; `zipWithSY` is now
   called `SY.comb21`; `zipWithV` is now called `V.farm21`, etc.

The ForSyDe-Atom definition of the Saab-AESA application is found in
`<root>/src/AESAAtom.lhs` and can be imported as a generic library
(e.g. in the interpreter session).
