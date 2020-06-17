## Conclusion

In this section we have presented an alternative model of the AESA signal processing
chain which refines its behavior in order to expose its streaming characteristics
inferred from the specification statement _"For each antenna the data arrives _pulse
by pulse_, and each pulse arrives _range bin by range bin_. This happens _for all
antennas in parallel_, and all complex samples are synchronized with the same sampling
rate, e.g. of the A/D converter."_ While still regarded from a high abstraction level
(e.g. SDF actors are assumed to execute "instantaneously" once they have enough data
to operate on), the refined processing blocks now describe a more fine-grained causal
ordering between individual samples arriving in streams. This enables their potential
of better resources exploitation (e.g. hardware/software pipelining, or distribution)
during future mapping, scheduling and synthesis design stages. We have (unit) tested
this refined model against the high-level "golden model" presented in @sec:atom, by
confronting the two models' responses against the same set of input stimuli. We have
also formulated a couple of properties to ensure that the functional description of
some of the blocks is still the expected one, and we have not introduced hidden bugs,
or rather flaws in the model description.

In the following section we plan to synthesize one functional block down to VHDL code
targeting FPGA hardware implementation. This means that we will continue gradually
refining the (sub-)system model until we reach enough detail level to start the code
synthesis process. As each refinement process is prone to introducing hidden mistakes
or logical flaws due to its nonsemantic-preserving nature, we will monitor the
correctness of the resulting artifact with the help of the existing, as well as
newly-formulated, properties.

\clearpage
