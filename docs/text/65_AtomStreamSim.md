
## Model Simulation Against Test Data{#sec:stream-atom-sim}

Just like before in @sec:cube-atom-sim we test the compiled model built from the
blocks defined in @sec:atom-network within the `AESA.StreamsAtom` module against the
same generated input data. Please refer to the the project's `README` file on how to
compile and run the necessary software tools.

The 13 objects described in @tbl:in-objects and plotted in @fig:aesa-indata are again
identified in the AESA radar processing output plotted in @fig:aesa-odata-atom-stream.
Again we notice a slight difference in the cross-correlation values due to a different
chaining of floating point operations, but in general the detected objects are within
the same range.

![One output cube with radar data](figs/AESA_OUT_S.pdf){#fig:aesa-odata-atom-stream}
