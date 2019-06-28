
## Model Simulation Against Test Data{#sec:cube-atom-sim}

As a first trial to validate that our AESA high-level model is "sane", i.e. is
modeling the expected behavior, we test it against realistic input data from an array
of antennas detecting some _known_ objects. For this we have provided a set of data
generator and plotter scripts, along with an executable binary created with the
`CubesAtom` module presented in @sec:cube-atom-operation. Please read the project's `README`
file on how to compile and run the necessary software tools.

The input data generator script replicates a situation in which 13 distinct objects,
either near each other or far apart, as seen in @tbl:in-objects, are detected drowned
into -18dB worth of noise by the 16 AESA antenna elements (see
@sec:aesa-parameters). In in @fig:aesa-indata can be seen a plot with the absolute
values of the complex samples in the first video indata cube, comprising of 16 antenna
(pulse $\times$ range) matrices.

In @fig:aesa-odata-atom a cube consisting of 8 beam (Doppler $\times$ range) matrices
from the AESA signal processing output is shown. We can see the 13 objects detected
with different intensities accros the 8 beams. As they are all positioned at
approximately the same angle relative to the antenna (i.e. $\frac{\pi}{3} +
2(\pi-\frac{2\pi}{3})/7$) we can see the maximum correlation values are reflected in
beam 2.



|  # | Distance (m) | Angle ($\theta$)                            | Rel. Speed (m/s) | Rel. Power |
|---:|-------------:|:-------------------------------------------:|-----------------:|-----------:|
|  1 |         12e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   |             0.94 |         -6 |
|  2 |         13e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   |   5 $\cdot$ 0.94 |         -6 |
|  3 |         14e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   |  10 $\cdot$ 0.94 |         -6 |
|  4 |         15e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   |            -0.94 |         -2 |
|  5 |         16e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   |  -2 $\cdot$ 0.94 |         -2 |
|  6 |         17e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   |  -3 $\cdot$ 0.94 |         -2 |
|  7 |         18e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   | -20 $\cdot$ 0.94 |         -4 |
|  8 |         19e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   | -23 $\cdot$ 0.94 |         -4 |
|  9 |         20e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   | -26 $\cdot$ 0.94 |         -4 |
| 10 |         21e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   | -29 $\cdot$ 0.94 |         -4 |
| 11 |         25e3 | $\frac{\pi}{3} + 2(\pi-\frac{2\pi}{3})/7$   | -15 $\cdot$ 0.94 |         -2 |
| 12 |       25.4e3 | $\frac{\pi}{3} + 2.1(\pi-\frac{2\pi}{3})/7$ | -15 $\cdot$ 0.94 |         -4 |
| 13 |       25.2e3 | $\frac{\pi}{3} + 2.2(\pi-\frac{2\pi}{3})/7$ | -15 $\cdot$ 0.94 |         -3 |

Table:  Objects reflected in the generated AESA indata {#tbl:in-objects}

<div id="fig:figureRef" class="subfigures">

![Absolute values for one input video cube with antenna data](figs/AESA_INPUT_C.pdf){#fig:aesa-indata}

![One output cube with radar data](figs/AESA_OUT_C.pdf){#fig:aesa-odata-atom}

AESA data plots

</div>

## Conclusion

In this section we have shown how to write a fully-functional high-level model of an
AESA radar signal processing chain in ForSyDe-Atom. In the process we have exemplified
basic modeling concepts such as *layers*, *process constructors* and *skeletons*. For
simplicity we have employed only two widely-used layers, representing time (through
the MoC layer) and parallelism (through the Skeleton layer) aspects in a
system. Within the MoC layer we used only the SY MoC capturing the passage of data
(structures) in a synchronous pipeline fashion. Within the Skeleton layer we used
algorithmic skeletons on vector data types to capture the inherent parallel
interaction between elements, mainly to build algorithms on cubes. As of
@fig:atom-layers from @sec:crash-atom, this layer setup is represented by right
picture (processes of skeleton functions). However in @sec:cube-int-atom we have given
"an appetizer" on how to instantiate regular, parameterizable process network
structures using the *same* skeletons, thanks to the concept of layers.

In @sec:refine we transform this model to capture some more refined notions of time
and data passage, as well as more complex use of skeletons and patterns, gradually
reaching enough system details to start considering the synthesis of the model in
@sec:synth to a hardware platform. Until then we will introduce an alternative
modeling framework, as well as practical tools to verify the conformance of the
ForSyDe model and all its subsequent refinements to the given specification.

\clearpage
