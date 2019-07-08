# ForSyDe AESA Radar Use Case

This repository contains source and documentation code for the AESA radar use case
developped in collaboration with Saab AB, as part of the Swedish Governmental Agency
for Innovation Systems, NFFP7 project: Correct by construction design methodology
#2017-04892. The software projects in this repository support and have been used to generate the technical report of [Ungureanu et al. (2019)](https://www.researchgate.net/publication/334249975_Design_of_Sensor_Signal_Processing_with_ForSyDe_Modeling_Validation_and_Synthesis).

## Scope and Documentation

TBA

## Modules and Usage

This project is structured accross multiple modules as different software "sandboxes", with their own environments and own dependencies. Please read the `README.md` files associated with each subfolder for more details about installation and additional documentation. For convenience, make sure that the executable binaries compiled with Stack [can be accessed from the default `$PATH`](https://docs.haskellstack.org/en/latest/GUIDE/#downloading-and-installation). Also, most of the default settings assume that, for convenience, the (compiled or interpreted) tools are run from *this* root location, meaning that after you build a sub-project you come back to this location to run the different tools.

### Indata Generator

The indata generator is a Python script which generates the input data for the different AESA application. Usage example:

	python3 scripts/generate-input.py -h
	
Use it according to the printed help message. Needless to say, this is the first tool that needs to be used prior to testing the AESA implementations. It currently has limited command-line customization options. If you want to generate different scenarios, you need to modify the source code under the `Add Objects and Noise to the Data` section. 

Dependencies: `python3`, `numpy`

### Data Plotter

We wrote a couple of data plotter scripts written in Python script for user convenience. Different data files are plotted in a pop-up window and in PDF format. Usage example:

    python3 scripts/plot-input.py -h
    python3 scripts/plot-input-cube.py -h
    python3 scripts/plot-output.py -h
	
Dependencies: `python3`, `numpy`, `matplotlib`

### ForSyDe Model Binaries

The [technical report](https://www.researchgate.net/publication/334249975_Design_of_Sensor_Signal_Processing_with_ForSyDe_Modeling_Validation_and_Synthesis) goes through several stages in designing and refining the AESA signal processing system model. Each intermediate step has associated an executable binary putting together a running testbench. 

The following projects produce binaries. Check their `README.md` file for more information.

 * [`aesa-atom`](aesa-atom)
 * [`aesa-shallow`](aesa-atom)

We recommend to place the compiled binaries in the shell path, e.g.

	export PATH=$(stack path --local-bin):$PATH

At the moment these are the generated binaries (and their usage):

	aesa-cube --help
	aesa-shallow --help
	aesa-stream --help
	
These binaries have their default settings as to be run from [this](.) folder, otherwise the paths need to be provided as command line arguments. 

Dependencies: [Haskell](https://www.haskell.org/platform/), [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). The rest is taken care of by Stack.

### Test Suites

Some models are also equipped with test suites. Compiling the test suites is also done through Stack. Running a suite is done from each project folder (e.g. [`./aesa-atom/`](./aesa-atom/)) by typing in the command

	stack test :<suite-name>
	
Check each project's `README`, as well as the technical report to see which tests are relevant and you need to run.

Dependencies: [Haskell](https://www.haskell.org/platform/), [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). The rest is taken care of by Stack.
