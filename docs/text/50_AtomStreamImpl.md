# Refining the Model Behavior. A Streaming Interpretation of AESA {#sec:refine}

> _In this section we refine the behavior of the high-level AESA model presented in
> @sec:atom focusing on a more fine-grained timed (i.e. streaming) aspects of the
> computation. We do this in an attempt to expose the inherent parallelism of the AESA
> application, by describing it as parallel networks of concurrent, independent
> processes operating on streaming elements rather than monolithical blocks operating
> on cubes. This perspective should pave the way for a more efficient exploration of
> the available design space in future refinement decisions. We present an alternative
> streaming-oriented model of the AESA system, validate it against test data and
> verify the properties previously formulated._

|         |                               |                                                      |
| -----   | ----------------------------- | --------------------------------------------------   |
| Package | aesa-atom-0.1.0               | path: `./aesa-atom/README.md`                        |
| Deps    | forsyde-atom-0.2.2            | url: `https://forsyde.github.io/forsyde-atom/api/`   |
|         | forsyde-atom-extensions-0.1.1 | path: `./forsyde-atom-extensions/README.md`          |
|         | QuickCheck-2.13.1             | url: `http://hackage.haskell.org/package/QuickCheck` |
| Suite   | tests-stream                  | usage: `stack test :tests-stream`                    |
| Bin     | aesa-stream                   | usage: `aesa-stream --help`                          |

The high-level behavioral model of the AESA signal processing chain presented in this
section is semantically equivalent to the one presented in @sec:atom, however it
exposes a more fine-grained temporal behavior of the individual operations performed
on the complex indata samples. While most design choices are driven by a didactic
purpose to consolidate the new modeling concepts presented in @sec:crash-atom, they
can also be justified by the need to capture the essential properties in a formal way,
in order to be exploitable in future stages of a design flow towards efficient
implementations.  The main design approach is to exploit the relative independence
between each data path associated with each antenna element or beam, and to model
these paths as skeletons (e.g. farms) of (chains of) processes. Each design decision
will infer a re-partitioning of the indata cubes between the time (i.e. causality) and
space dimensions following the design patters depicted earlier in [@fig:atom-layers],
namely:

* _skeletons of processes_ which: 1) express parallelism at the process level; 2)
  depicts processes as operating on elementary streams of data, e.g. originating from
  each antenna element in particular, and skeletons as the structured interactions
  between these streams; 3) expose a fine-grained modular view allowing to quantify
  the potential for _load distribution_, since each "operation" (i.e. process) clearly
  captures the aspect of precedence constraints.

* _process of skeletons_ which : 1) express parallelism at the datum level; 2) depicts
  processes as operating on structures of data (e.g. vectors, matrices or cubes); 3)
  expose a monolithic view of processes where precedence constraints are expressed
  "outside" of the algorithm, and where the algorithm itself expresses potential for
  _data parallelism_.
