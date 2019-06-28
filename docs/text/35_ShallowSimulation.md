
## Model Simulation Against Test Data{#sec:cube-shallow-sim}

Similarly to the ForSyDe-Atom implementation, we have provided a runner to compile the
model defined in @sec:cube-atom-operation within the `AESA.CubesShallow` module into
an executable binary, in order to tests that it is sane. The same generator an plotter
scripts can be used with this binary, so please read the project's `README` file on
how to compile and run the necessary software tools.

We use the same generated input data reflecting the 13 objects from @tbl:in-objects,
plotted in @fig:aesa-indata. This time the AESA radar processing output shows the
picture in @fig:aesa-odata-shallow.

![One output cube with radar data](figs/AESA_OUT_CS.pdf){#fig:aesa-odata-shallow}

Comparing @fig:aesa-odata-shallow with @fig:aesa-odata-atom one can see that the same
objects have been identified, albeit with slighly different correlation values.  This
is because we use single-precision folating point `Float` as the base number
representation in our model, which are
[well-known](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html) to be
very sensitive to slight variations in arithmetic operations, as far as the order of
evaluation is concerned. The output data graphs reflect the fact that ForSyDe-Shallow
and ForSyDe-Atom have different implementation, but that is the only conclusion that
can be drawn from this.

## Conclusion

In this section we have presented an alternative implementation of the AESA signal
processing high-level model in the ForSyDe-Shallow modeling framework. We have
deliberately implemented the *same* model in order to highlight the API differences
between ForSyDe-Atom and ForSyDe-Shallow.

The next sections will only use ForSyDe-Atom as the main modeling framework, however
users should hopefully have no trouble switching between their modeling library of
choice. The insights in the API of ForSyDe-Shallow will also be useful in @sec:synth
when we introduce ForSyDe-Deep, since the latter has similar constructs.

\clearpage
