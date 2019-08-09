---
title: Design of Sensor Signal Processing with ForSyDe
pdfcover: title
author:
  - name: George Ungureanu
    email: ugeorge@kth.se
    affiliation: KTH Royal Institute of Technology, Sweden
  - name: Timmy Sundström
    email: timmy.sundstrom@saabgroup.com
    affiliation: Saab AB, Sweden	
  - name: Anders Åhlander
    email: anders.ahlander@saabgroup.com
    affiliation: Saab AB, Sweden
  - name: Ingo Sander
    email: ingo@kth.se
    affiliation: KTH Royal Institute of Technology, Sweden
  - name: Ingemar Söderquist
    email: ingemar.soderquist@saabgroup.com
    affiliation: Saab AB, Sweden
figPrefix: [Figure, Figures]
secPrefix: [section, sections]
link-citations: true
linkReferences: true
abstract: |
  This document serves as a report and as a step-by-step tutorial for modeling, simulating, testing and synthesizing complex heterogeneous systems in ForSyDe, with special focus on parallel and concurrent systems. The application under test is a radar signal processing chain for an active electronically scanned array (AESA) antenna provided by Saab AB. Throughout this report the application will be modeled using several different frameworks, gradually introducing new modeling concepts and pointing out similarities and differences between them. 
header-includes: |
	\usepackage{pdflscape}
    \usepackage{todonotes}
	\hypersetup{colorlinks = true,
            linkcolor = red,
            urlcolor  = cyan,
            citecolor = blue,
            anchorcolor = blue}
	
---

\clearpage

# Introduction{#sec:intro}

In order to develop more cost-efficient implementation methods for complex systems, we
need to understand and exploit the inherent properties derived from the specification
of the target applications and, based on these properties, be able to explore the
design space offered by alternative platforms. Such is the case of the application
studied in this report: the active electronically scanned array (AESA) radar is a
versatile system that is able to determine both position and direction of incoming
objects, however critical parts of its signal processing has significant demands on
processing and memory bandwidth, making it well out-of reach from the general public
usage. We believe that a proper understanding of the temporal and spatial properties
of the signal processing chain can lead to a better exploration of alternative
solutions, ideally making it an affordable appliance in the context of current
technology limitations. Nevertheless, expressing behaviors and (extra-functional)
properties of systems in a useful way is far from a trivial task and it involves
respecting some key principles:

* the language(s) chosen to represent the models need(s) to be _formally defined_ and
  _unambiguous_ to be able to provide a solid foundation for analysis and subsequent
  synthesis towards implementation.
* the modeling paradigm should offer the _right_ abstraction level for capturing the
  _needed_ properties [@lee-2015]. An improper model might either abstract away
  essential properties or over-specify them in a way that makes analysis
  impossible. In other words it is the engineer's merit to find the right model for
  the right "thing being modeled".
* the models, at least during initial development stages, need to be _deterministic_
  with regard to defining what _correct_ behavior is [@Lee18].
* at a minimum, the models need to be _executable_ in order to verify their
  conformance with the system specification. Ideally they should express operational
  semantics which are traceable across abstraction levels, ultimately being able to be
  synthesized on the desired platform [@Sifakis15].

[ForSyDe](https://forsyde.github.io/) is a design methodology which envisions
"correct-by-construction system design" through formal or rigorous methods. Its
associated modeling frameworks offer means to tackle the challenges enumerated above
by providing well-defined composable building blocks which capture extra-functional
properties in unison with functional
ones. [ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow) is a domain
specific language (DSL) shallow-embedded in the functional programming language
Haskell, meaning that it can be used only for modeling and simulation purposes. It
introduced the concept of _process constructors_ [@sander-2004] as building blocks
that capture the semantics of computation, concurrency and synchronization as dictated
by a certain model of computation
(MoC). [ForSyDe-Atom](https://forsyde.github.io/forsyde-atom) is also a
shallow-embedded (set of) DSL which extends the modeling concepts of ForSyDe-Shallow
to systematically capture the interacting extra-functional aspects of a system in a
disciplined way as interacting _layers_ of minimalistic languages of primitive
operations called _atoms_
[@ungureanu17]. [ForSyDe-Deep](https://forsyde.github.io/forsyde-deep) is a
deep-embedded DSL implementing a synthesizable subset of ForSyDe, meaning that it can
parse the structure of process networks written in this language and operate on their
abstract syntax: either simulate them or further feed them to design flows. Currently
ForSyDe-Deep is able to generate GraphML structure files and synthesizable VHDL code.

This documents presents alternatives ways to modelling the AESA radar signal
processing chain and, using these models, gradually introducing one concept at a time
and pointing towards reference documentation. The final purpose is to refine,
synthesize, and replace parts of the behavioral model down to VHDL implementation on
FPGA hardware platforms, and co-simulate these design artifacts along with the initial
high-level model. The report itself is written using [literate
programming](https://en.wikipedia.org/wiki/Literate_programming), which means that all
code snippets contained are _actual compiled code_ alternating with documentation
text.  Following the report might be difficult without some initial clarification. The
remaining parts of @sec:intro will present a guide to using this document, as well as
an introduction to the AESA application. In @sec:atom a high-level, functionally
complete ForSyDe-Atom model of the application is thoroughly presented with respect to
the specification, and tested against a set of known input data. In @sec:shallow an
equivalent model written in ForSyDe-Shallow is briefly presented and tested, to show
the main similarities and differences between the two modeling APIs. In @sec:props is
introduced the concept of property checking for the purpose of validation of ForSyDe
designs. We formulate a set of properties in the
[QuicCheck](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html) DSL for
each component of the AESA model which are validated against a number of
randomly-generated tests. In @sec:atom-network we focus on refining the behavior of
the initial (high-level) specification model to lower level ones, more suitable for
(backend) implementation synthesis, followed by @sec:stream-prop-defs where we
formulate new properties for validating some of these refinements. All refinements in
@sec:refine happen in the domain(s) of the ForSyDe-Atom DSL. In @sec:synth we switch
the DSL to ForSyDe-Deep, which benefits from automatic synthesis towards VHDL: in each
subsection the refinements are stated, the refined components are modeled, properties
are formulated to validate them, and in [@sec:synth-r3;@sec:synth-r4] VHDL code is
generated and validated.

![Reading order dependencies](figs/reading-order.pdf){#fig:reading-order}

@fig:reading-order depicts a reading order suggestion, based on information
dependencies. The dashed line between @sec:shallow and [@sec:synth-r3;@sec:synth-r4]
suggests that understanding the latter is not directly dependent on the former, but
since ForSyDe-Deep syntax is derived from ForSyDe-Shallow, it is recommended to get
acquainted with the ForSyDe-Shallow syntax and its equivalence with the ForSyDe-Atom
syntax.

## Application Specification{#sec:video-chain-spec label="Application Specification"}

An AESA, see picture below, may consist of thousands of antenna elements. The relative
phases of the pulses of the antenna’s different antenna elements can be set to create
a constructive interference in the chosen main lobe bearing. In this way the pointing
direction can be set without any moving parts. When receiving, the direction can be
steered by following the same principle, as seen the Digital Beam Former below.  One
of the main advantages of the array antennas is the capacity to extract not only
temporal but also spatial information, i.e. the direction of incoming signals.

![Antenna](figs/aesa-antenna.png){ height=3.5cm } \ ![](figs/aesa-beamforming.png){ height=3.5cm }

@fig:video-chain-spec shows a simplified radar signal processing chain that is used to
illustrate the calculations of interest. The input data from the antenna is processed
in a number of steps.

![Overview of the video processing chain](figs/video-chain-spec.pdf){#fig:video-chain-spec}

In this report we assume one stream per antenna element. The indata is organized into
a sequence of “cubes”, each corresponding to a certain integration interval. Each
sample in the cube represents a particular antenna element, pulse and range bin. The
data of the cube arrives pulse by pulse and each pulse arrives range bin by range
bin. This is for all elements in parallel. Between the Pulse Compression (PC) and
Doppler Filter Bank (DFB) steps there is a corner turn of data, i.e. data from all
pulses must be collected before the DFB can execute.

The different steps of the chain, the antenna and the input data format are briefly
described in the following list. For a more detailed description of the processing
chain, please refer to [@sec:atom].

* _Digital Beam Forming (DBF)_: The DBF step creates a number of simultaneous receiver
  beams, or “listening directions”, from the input data. This is done by doing
  weighted combinations of the data from the different antenna elements, so that
  constructive interference is created in the desired bearings of the beams. The
  element samples in the input data set are then transformed into beams.

* *Pulse Compression (PC)*: The goal of the pulse compression is to collect all
  received energy from one target into a single range bin. The received echo of the
  modulated pulse is passed through a matched filter. Here, the matched filtering is
  done digitally.

* *Doppler Filter Bank incl. envelope detection (DFB)*: The DFB gives an estimation of
  the target’s speed relative to the radar. It also gives an improved signal-to-noise
  ratio due to a coherent integration of indata. The pulse bins in the data set are
  transformed into Doppler channels. The envelope detector calculates the absolute
  values of the digital samples. The data is real after this step.

* *Constant False Alarm Ratio (CFAR)*: The CFAR processing is intended to keep the
  number of false targets at an acceptable level while maintaining the best possible
  sensitivity. It normalizes the video in order to maintain a constant false alarm
  rate when the video is compared to a detection threshold. With this normalization
  the sensitivity will be adapted to the clutter situation in the area (around a cell
  under test) of interest.

* *Integrator (INT)* The integrator is an 8-tap unsigned integrator that integrates
  channel oriented video. Integration shall be done over a number of FFT batches of
  envelope detected video. Each Doppler channel, range bin and antenna element shall
  be integrated.
  
The following table briefly presents the dimensions of the primitive functions
associated with each processing stage, where data sets are measured in number of
elements processed organized in channels $\times$ range bins $\times$ pulses; data
precision is measured in bit width; and approximative performance is given in
MOPS. These dimensions represent the scaled-down version of a realistic AESA radar
signal processing system, with 16 input antenna channels forming into 8 beams.

| Block | In Data Set  | Out Data Set | Type     In |        Out | Precision    In |        Out | Approx. perform. |
| ---- | -------------- | -------------- | --- | --- | --- | --- | ------- |
| DBF  | $16 \times 1 \times 1$     | $8 \times 1 \times 1$          | $\mathbb{C}$ | $\mathbb{C}$ | 16 | 20 | 2688  |
| PC   | $1\times 1024 \times1$     | $1 \times 1024 \times 1$       | $\mathbb{C}$ | $\mathbb{C}$ | 20 | 20 | 4608  |
| DFB  | $1\times 1 \times 256$     | $1 \times 1 \times 256$        | $\mathbb{C}$ | $\mathbb{R}$ | 20 | 20 | 7680  |
| CFAR | $1 \times 1024 \times 256$ | $1 \times 1024 \times 256$     | $\mathbb{R}$ | $\mathbb{R}$ | 20 | 16 | 360   |
| INT  | $1\times 1 \times 1$       | $1\times 1 \times 1$           | $\mathbb{R}$ | $\mathbb{R}$ | 16 | 16 | 24576 |
| AESA | $16\times 1024 \times 256$ | $2 (8 \times 1024 \times 256)$ | $\mathbb{C}$ | $\mathbb{R}$ | -- | -- | 39912 |

Table: Dimensions of the AESA case study covered in the report {#tbl:sizes}

## Using This Document{#sec:usage}

**PREREQUISITES:** the document assumes that the reader is familiar with the
functional programming language [Haskell](https://www.haskell.org/), its syntax, and
the usage of a Haskell interpreter
(e.g. [`ghci`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)). Otherwise,
we recommend consulting at least the introductory chapters of one of the following
books by @Lipovaca11 and @hutton-2016 or other recent books in Haskell. The reader
also needs to be familiar with some basic ForSyDe modeling concepts, such as _process
constructor_, _process_ or _signal_. We recommend going through at least the online
getting started [tutorial on
ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow/getting_started) or the one
[on ForSyDe-Atom](https://forsyde.github.io/forsyde-atom/assets/manual.pdf), and if
possible, consulting the (slightly outdated) book chapter on ForSyDe
[@sander-2017-forsy].

This document has been created using literate programming. This means that all code
shown in the listings is compilable and executable. There are two types of code
listing found in this document. This style

``` {.haskell .numberLines}
-- | API documentation comment
myIdFunc :: a -> a
myIdFunc = id
```
shows _source code_ as it is found in the implementation files, where the line numbers correspond to the position in the source file. This style

	Prelude> 1 + 1
	2

suggests _interactive commands_ given by the user in a terminal or an interpreter
session. The listing above shows a typical `ghci` session, where the string after the
prompter symbol `>` suggests the user input (e.g. `1 + 1`). Whenever relevant, the
expected output is printed one row below (e.g. `2`).

The way this document is meant to be parsed efficiently is to load the source files
themselves in an interpreter and test the exported functions gradually, while reading
the document at the same time. Due to multiple (sometimes conflicting) dependencies on
external packages, for convenience the source files are shipped as _multiple_
[Stack](https://docs.haskellstack.org/en/stable/README/) packages each creating an own
sandbox on the user's local machine with all dependencies and requirements taken care
of. Please refer to the project's `README` file for instructions on how to install and
compile or run the Haskell files.

At the beginning of each chapter there is meta-data guiding the reader what tools and
packages are used, like:

|         | | |
| -----   | ----------------------------- | -------------------------------------------------- |
| Package | aesa-atom-0.1.0               | path: `./aesa-atom/README.md`                      |

This table tells that the package where the current chapter's code resides is
`aesa-atom`, version 0.1.0.  This table might contain information on the main
dependency packages which contain important functions, and which should be consulted
while reading the code in the current chapter. If the main package generates an
executable binary or a test suite, these are also pointed out. The third column
provides additional information such as a pointer to documentation (relative path to
the project root, or web URL), or usage suggestions.

It is recommended to read the main package's `README` file which contains instructions
on how to install, compile and test the software, before proceeding with following a
chapter. Each section of a chapter is written within a library *module*, pointed out
in the beginning of the respective section by the line:

``` {.haskell .numberLines}
module ForSyDe.X.Y where
```

The most convenient way to test out all functions used in module `ForSyDe.X.Y` is by
loading its source file in the sandboxed interpreter, i.e. by running the following
command from the project root:

	stack ghci src/ForSyDe/X/Y.lhs
	
An equally convenient way is to create an own `.hs` file somewhere under the project
root, which imports and uses module `ForSyDe.X.Y`, e.g.

``` {.haskell .numberLines}
-- MyTest.hs
import ForSyDe.X.Y

myData = [1,2,3,4,5] :: [Int]
myTest = functionFromForSyDeXY myData
```

This file can be loaded and/or compiled from within the sandbox, e.g. with `stack ghci MyTest.hs`.

\clearpage
