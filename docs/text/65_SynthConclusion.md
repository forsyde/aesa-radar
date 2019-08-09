
## Conclusions

In this section we have shown a series of design transformations on one of the
high-level model components of AESA down to synthesizable VHDL code. Along the way we
have co-simulated the transformed components alongside the original AESA high-level
model, as well as gradually validated the final generated design artifact is a proper
implementation for its behavioural model.

The scope of this section was two-fold: on the one hand we have shown a practical and
didactic example and consolidated the design methodology introduced in the previous
chapters; and on the other hand we have provided a brief glance over the current
status of some of the tools and their position in the ForSyDe ecosystem, as well as
directions for future development. As mentioned several times throughout this document
the ForSyDe ecosystem is actively being developed and the overall vision is to learn
from past and current experiences in order to achieve a correct-by-construction
design flow.

Although all the designs shown in this report, both original and intermediate, were
written manually, the modeling frameworks have provided us with the means to
understand which transformations can be fully- or partially-automated in the
future. For example refinement 3 should be invalidated by a unified high-level,
layered modeling language; refinement 4 could be done automatically once a program
analyzer understands that the core reduction operation is commutative; refinement 2
could be fully- or partially-automated once a clear design specification or constraint
language can be formulated _and_ analyzed. Refinement 1 though, as well as the
transformation from the cube version of AESA in @sec:atom to the streamed version in
@sec:refine is not so trivial to automate. This is because the nonsemantic-preserving
transformations involved admit multiple (maybe better) solutions. Choosing between
alternative solutions, although aided by verification and validation techniques, often
relies on designer experience. In the future some of these decision could be automated
or computer-aided by design space exploration techniques once we understand how to
formulate the search problems.

\clearpage
