# High-Level Model of the AESA Signal Processing Chain in ForSyDe-Atom{#sec:atom}

> _This section guides the reader throughout converting the provided specifications of
> the AESA signal processing chain into a concrete, functionally complete, high-level
> executable model in ForSyDe-Atom. Enough AESA system details are given in order to
> understand the modeling decisions. At the end of this section we simulate this model
> against a realistic input set of complex antenna data, and test if it is sane
> (i.e. provides the expected results)._

|         |                               |                                                    |
| -----   | ----------------------------- | -------------------------------------------------- |
| Package | aesa-atom-0.1.0               | path: `./aesa-atom/README.md`                      |
| Deps    | forsyde-atom-0.2.2            | url: `https://forsyde.github.io/forsyde-atom/api/` |
|         | forsyde-atom-extensions-0.1.1 | path: `./forsyde-atom-extensions/README.md`        |
| Bin     | aesa-hl                       | usage: `aesa-hl --help`                            |


Historically, [ForSyDe-Atom](https://forsyde.github.io/forsyde-atom/) has been a
spin-off of [ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow/) which has
explored new modeling concepts, and had a fundamentally different approach to how
models are described and instantiated. ForSyDe-Atom introduced the concept of
*language layer*, extending the idea of process constructor, and within this setting
it explored the algebra of algorithmic skeletons, employed heavily within this
report. As of today, both ForSyDe-Shallow and ForSyDe-Atom have similar APIs and are
capable of modeling largely the same behaviors. The main syntax differences between the
two are covered in @sec:shallow, where the same high-level model is written in
ForSyDe-Shallow.

## Crash course in ForSyDe-Atom{#sec:crash-atom}

Before proceeding with the modeling of the AESA processing chain, let us consolidate
the main ForSyDe concepts which will be used throughout this report: *layer*, *process
constructor* and *skeleton*. If you are not familiar with ForSyDe nomenclature and
general modeling principles, we recommend consulting the documentation pointed out in
@sec:intro first.

As seen in @sec:video-chain-spec, most of the AESA application consists of typical DSP
algorithms such as matrix multiplications, FFT, moving averages, etc. which lend
themselves to *streaming parallel* implementations. Hence we need to unambiguously
capture the two distinct aspects of the AESA chain components:

* *streaming behavior* is expressed in ForSyDe through processes which operate on
  signals encoding a temporal partitioning of data. Processes are instantiated
  exclusively using *process constructors* which can be regarded as templates
  inferring the semantics of computation, synchronization and concurrency as dictated
  by a certain model of computation (MoC) (@sander-2004,@leeseshia-15).
  
* *parallelism* is expressed through parallel patterns which operate on structured
  types (e.g. vectors) encoding a spatial partitioning of data. These patterns are
  instantiated as skeletons, which are templates inferring the semantics of
  distribution, parallel computation and interaction between elements, as defined by
  the algebra of catamorphisms [@Fischer-2003].

![Depiction of layer usage: (left) skeleton networks of processes; (right) processes of skeleton functions](figs/layers.pdf){#fig:atom-layers}

In order to capture these two[^1] interacting aspects in unison as a complete system,
we describe them in terms of two distinct, orthogonal *layers* within the ForSyDe-Atom
framework. A language layer is a (very small) domain specific language (DSL) heavily
rooted in the functional programming paradigm, which is able to describe one aspect of
a cyber-physical system (CPS). Layers interact with one another by virtue of the
abstraction principle inherent to a functional programming language [@backus-1978]:
each layer defines at least one higher order function that is able to take a function
(i.e. abstraction) from another layer as an argument and to lift it within its own
domain. To nepicture the interaction between layers, consider @fig:video-chain-spec
where, although we use the same `farm` vector skeleton and `comb` synchronous (SY)
process constructor, the two different compositions describe two different (albeit
semantically equivalent) systems: the first one is instantiating a farm network of SY
processes operating on a vector of signals in parallel; whereas the second one is
instantiating a single SY process operating on o  signal where each event is carrying
a vector of values.

[^1]: and other aspects, not covered in this report

<!-- Historically, it has been the "playground" for -->
<!-- developing and applying modeling concepts such as algorithmic skeletons (parallel -->
<!-- patterns) and applicative-style modeling in ForSyDe. For more information on the main -->
<!-- concepts behind ForSyDe-Atom see [@ungureanu17]. From the point of view of user -->
<!-- experience however, the API is pretty much identical to ForSyDe-Shallow's, with the -->
<!-- following two main differences: -->

<!--  * the user has more control of which libraries are -->
<!--    imported. Functions are not distinguished by their suffix any -->
<!--    longer, which means that multiple libraries export functions which -->
<!--    deliberately share the same name. As such, the suggested -->
<!--    programming style for mixed-library designs is to alias the -->
<!--    imported library (e.g. `import ForSyDe.Atom.MoC.SY as SY`) and -->
<!--    reference the function using the alias as a prefix (e.g. `SY.mealy` -->
<!--    instead of `mealySY` in ForSyDe-Shallow). -->

<!--  * some of ForSyDe's "canonical" names for process constructors -->
<!--    inspired from functional programming have been replaced with more -->
<!--    suggestive names inspired from component-based modeling, which are -->
<!--    denoting common building blocks, relevant to their domain. For -->
<!--    example `mapSY` is now called `SY.comb11`; `zipWithSY` is now -->
<!--    called `SY.comb21`; `zipWithV` is now called `V.farm21`, etc. -->

<!-- In this section we approach modeling the AESA application from two -->
<!-- different point of views: the first one is a direct translation of the -->
<!-- ForSyDe-Shallow implementation from [@sec:shallow], and it models the -->
<!-- signal processing chain as a pipe of processes working on _parallel -->
<!-- data_ organinzed in matrices or cubes; the second approach is to model -->
<!-- the data path from each individual antenna element by describing -->
<!-- parallelism at a _process network_ level (e.g. process farms), similar -->
<!-- to how [@sec:int-shallow] has been described. We show that the second -->
<!-- approach enables opportunities for exploitation and design space -->
<!-- exploration at a more fine-grained level. -->

