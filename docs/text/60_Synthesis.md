# Model Synthesis to VHDL {#sec:synth}

> _In this section we choose one sub-system in the AESA signal processing stage and
> gradually refine to synthesizable VHDL code, by applying a series of
> nonsemantic-preserving transformations. In order to validate the resulting design we
> gradually wrap each refined component in order to co-simulate it with the original
> model, test it against the same input data as the previous sections, and formulate
> design properties that ensure the desired behavior is preserved. As input model we
> use the refined streaming behavior from @sec:refine and we focus only on its PC
> stage. Throughout this section we will introduce another framework in the ForSyDe
> ecosystem: ForSyDe-Deep, which is able to parse an input ForSyDe program and
> translate it to different backends._

|         |                               |                                                        |
| -----   | -------------------------     | --------------------------------------------------     |
| Package | aesa-deep-0.1.0               | path: `./aesa-deep/README.md`                          |
| Deps    | forsyde-atom-0.2.2            | url: `https://forsyde.github.io/forsyde-atom/api/`     |
|         | forsyde-atom-extensions-0.1.1 | path: `./forsyde-atom-extensions/README.md`            |
|         | forsyde-deep-0.2.1            | url: `http://hackage.haskell.org/package/forsyde-deep` |
|         |                               | note: 0.2.1 is found in a local experimental branch.   |
|         | QuickCheck-2.13.1             | url: `http://hackage.haskell.org/package/QuickCheck`   |
| Suite   | tests-deep                    | usage: `stack test :tests-deep`                        |
| Bin     | aesa-deep                     | usage: `aesa-deep --help`                              |


The system in @sec:refine, although exposing a more fine-grained streaming behavior
for each data channel, is still far from a physical realization. In order to reach a
suitable implementation one has to take into consideration a multitude of aspects from
one or several target platforms (architectures), which form the _design
space_. Mapping a behavior onto a platform often involves a (series of)
transformation(s) that do not fully preserve the original semantics any longer, yet
still offer a good realization of the intended behavior. We call these
_nonsemantic-preserving_ transformations [@raudvere2008application].

In this section we will focus on synthesizing the PC signal processing stage, as
specified in @sec:video-chain-spec and modeled in @sec:atom-network, into an FPGA
component. We do that by applying four subsequent refinement phases, guided by design
_decisions_ to reach a correct and/or efficient implementation. Each decision is
tested against a set of properties as introduced in @sec:props, and each refined
component is co-simulated within the entire AESA high-level model.

