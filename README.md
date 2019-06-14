# ForSyDe AESA Radar Use Case

This repository contains code and documentation for the AESA radar use case developped in collaboration with Saab AB, as part of the e Vinnova NFFP7 Swedish National project: Correct by construction design methodology #2017-04892. 

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

The data plotter is a Python script which plots different data files in a pop-up window and in PDF format. Usage example:

    python3 scripts/plot-output.py -h
	
Dependencies: `python3`, `numpy`, `matplotlib`

### High-Level ForSyDe-Atom AESA Model

This is the (initial) high-level model of the AESA signal processing chain in ForSyDe-Atom. It is extensively documented in the technical report. It generates a binary and a suite of tests (check its `README.md` file for more information). The binary works with the files created by the indata generator. Usage example:

	aesa-hl -h
	
Dependencies: [Haskell](https://www.haskell.org/platform/), [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). The rest is taken care of by Stack.
