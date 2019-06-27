# Alternative Modeling Framework: ForSyDe-Shallow{#sec:shallow}

> _This section follows step-by-step the same approach as @sec:atom, but this time
> using the ForSyDe-Shallow modeling framework. The purpose is to familiarize the
> reader to the syntax of ForSyDe-Shallow should the designer prefer it instead of
> ForSyDe-Atom. This section is also meant to show that, except for minor syntactic
> differences, the same modeling concepts are holding and the user experience and API
> design are very similar._

|         |                           |                                                    |
| -----   | ------------------------- | -------------------------------------------------- |
| Package | aesa-shallow-0.1.0        | path: `./aesa-shallow/README.md`                   |
| Deps    | forsyde-shallow-0.2.2     | url:`http://hackage.haskell.org/package/forsyde-shallow` |
|         | forsyde-atom-extensions-0.1.1 | path: `./forsyde-shallow-extensions/README.md` |
|         | aesa-atom-0.1.1           | path: `./aesa-atom/README.md`                      |
| Bin     | aesa-cube-sh              | usage: `aesa-cube-sh --help`                       |

[ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow/) is the flagship and the
oldest modeling language of the ForSyDe methodology [@sander-2004]. It is a domain
specific language (DSL) shallow-embedded into the functional programming language
Haskell and uses the host's type system, lazy evaluation mechanisms and the concept of
higher-order functions to describe the formal modeling framework defined by
ForSyDe. At the moment of writing this report, ForSyDe-Shallow is the more "mature"
counterpart of ForSyDe-Atom. Although some practical modeling concepts such as
*layers*, *patterns* and *skeletons* have been originally developped within
ForSyDe-Atom, they are now well-supported by ForSyDe-Shallow as well. In the future
the modeling frameworks such as ForSyDe-Atom and ForSyDe-Shallow are planned to be
merged into a single one incorporating the main language features of each one and
having a similar user experience as ForSyDe-Shallow.
