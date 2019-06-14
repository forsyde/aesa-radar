## Model Simulation Against Test Data{#sec:atom-sim}

As a first trial to validate that our AESA high-level model is "sane", i.e. is
modeling the expected behavior, we test it against realistic input data from an array
of antennas detecting some _known_ objects. For this we have provided a set of data
generator and plotter scripts, along with an executable binary created with the
`HighLevelAtom` module presented in @sec:atom-network. Please read the project's
`README` file on how to compile and run the software tools. 

The input data generator script replicates a situation in which 13 distinct objects,
either near each other or far apart, as seen in @tbl:in-objects, are drowned into
-18dB worth of noise, and then detected by the 16 AESA antenna elements (see
@sec:aesa-parameters). A zoomed snapshot of the generated input data can be seen in
@fig:aesa-indata.

![Zoomed caption of the 16 input channels split into real part ($I$) and imaginary part ($Q$)](figs/AESA_INDATA.pdf){#fig:aesa-indata}

In @fig:aesa-odata-atom a cube consisting of 8 beam (doppler $\times$ range) matrices
from the AESA signal processing output is shown. We can see the 13 objects detected
with different intensities accros the 8 beams. As they are all positioned at
approximately the same angle relative to the antenna (i.e. $\frac{\pi}{3} +
2(\pi-\frac{2\pi}{3})/7$) we can see the maximum correlation values are reflected in
beam 2.

\nopandoc{\begin{landscape}\vfill}

![One output cube with radar data](figs/AESA_OUTPUT.pdf){#fig:aesa-odata-atom}

\nopandoc{\vfill\end{landscape}}


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

\clearpage
