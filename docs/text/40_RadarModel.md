# Modeling the Radar Environment {#sec:radar}

> _In this section we model the input signals describing the radar environment,
> containing analog information about reflected objects, respectivey the digital
> acquisition of those signals. The purpose of this section is to introduce new
> modeling "tools" in ForSyDe-Atom: the continuous time (CT) MoC for describing
> signals that evolve continuously in time, and a new layer, the Probability layer,
> for describing random values which are distributed with a certain probability. This
> section can be read independently from the rest._

|         |                               |                                                        |
| -----   | -------------------------     | --------------------------------------------------     |
| Package | aesa-atom-0.2.0               | path: `./aesa-atom/README.md`                          |
| Deps    | forsyde-atom-0.2.2            | url: `https://forsyde.github.io/forsyde-atom/api/`     |
|         | forsyde-atom-extensions-0.2.0 | path: `./forsyde-atom-extensions/README.md`            |
| Bin     | aesa-atom                     | usage: `aesa-atom -g[NUM] -d` (see `--help`)           |


The Probability layer and the CT MoC are highlights of the functional implementation
of ForSyDe-Atom. Both are represented by paradigms which describe dynamics over
continuous domains. However computers, being inherently discrete machines, cannot
represent continuums, hence any computer-aided modeling attempt is heavily dependent
on numerical methods and discrete "interpretations" of continuous domains
(e.g. sampled data). In doing so, we are not only forcing to alter the problem, which
might by analytical in nature to a numerical one, but might also unknowingly propagate
undesired and chaotic behavior given rise by the fundamental incompatibilities of
these two representations. The interested reader is strongly advised to read recent
work on cyber-physical systems such as [@lee2016fundamental; @bourke2013zelus] to understand these phenomenons. 

Being computer programs ForSyDe-Atom models are still dependent on numerical
representations, however ForSyDe-Atom deals with this by simply abstracting away the
continuous domain (e.g. time) as a _function argument_. Therefore during modeling, any
trasformation can be described as a composition of pure functions. The numerical
representation of the domain becomes apparent only at the end, when a model is being
evaluated, e.g. when a signal is being plotted, in which case the pure function
resulted from a chain of transformations is being evaluated for a certain experiment. This principle is depicted in @fig:lazy-plot, and is enforced by the host language's (i.e.e Haskell's) lazy evaluation mechanism.

![The difference between transforming initially sampled data (above) and sampling transformed pure functions during plotting/trancing (below)](figs/lazy-plot.pdf){#fig:lazy-plot}


In ForSyDe-Atom, CT is defined in the MoC layer, and is describing (discrete) timed interactions between continuous sub-signals. Implementation details and example usage are found at [@ungureanu-2018-bridg]. In CT signals each event is described by a discrete tag and a function of time: the tags describe a simple algebra of discrete interactions, whereas event interactions themselves are simply function compositions. 

In probability theory on the other hand, an observed value is described by a certain distribution (e.g. normal, uniform), which is a _function_ of a random experiment. Similarly to the CT MoC, the Probability layer represents values by their "recipe" to achieve a desired distribution, whereas the algebra defined by this layer is describing the composition of such recipes. When we need to evaluate a system though, e.g. to plot/trace the behavior, we need to feed the transformed recipe with a (pseudo)-random seed in order to obtain an experimental value.
