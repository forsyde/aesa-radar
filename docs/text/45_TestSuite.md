## Running the Test Suite. Conclusion

The file shown in @sec:prop-defs can be compiled using any means available. We have
included it as a test suite for the `aesa-atom` package. Please refer to the package's `README` file for instructions on how to compile an run the tools.

As expected (in a public report) the test suite output looks like below:

	aesa-atom-0.1.0.0: test (suite: tests-cube)
	
	 Cube HL Model Tests :
	  GENERIC farm does not alter the input structure         : [OK, passed 100 tests]
	  DBF  right number of outputs                            : [OK, passed 100 tests]
	  DBF  legal value range                                  : [OK, passed 200 tests]
	  DBF  equivalence with simple dot product operation      : [OK, passed 200 tests]
	  PC   right number of outputs                            : [OK, passed 100 tests]
	  PC   right unit impulse response                        : [OK, passed 100 tests]
	  PC   legal value range                                  : [OK, passed 200 tests]
	  CT   both channels have cubes of the same dimensions    : [OK, passed 100 tests]
	  DFB  right number of outputs                            : [OK, passed 100 tests]
	  CFAR right number of outputs                            : [OK, passed 100 tests]
	  INT  right unit impulse response                        : [OK, passed 70 tests]
	
	         Properties   Total       
	 Passed  11           11          
	 Failed  0            0           
	 Total   11           11          
	
	aesa-atom-0.1.0.0: Test suite tests-cube passed


However it should not be taken for granted that during the development process the
printed output is *this* flawless. As mentioned in this section's introduction, like
any other software project, a ForSyDe model grows organically with consecutive
iterations between system specification and model implementation, often held
accountable by different parties or groups. Along with traditional unit tests (not
covered by this report), property checking is a powerful tool which shortens these
iteration cycles by setting a formal "legal frame" within which each (sub-)component
in a a model needs to operate. Apart from helping in understanding the specification
better, these properties ensure that further in the refinement process we do not
mistakenly introduce new flaws. This will be demonstrated in the next sections.

\clearpage
