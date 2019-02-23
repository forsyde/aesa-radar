---
title: Modeling Sensor Signal Processing in ForSyDe
author:
  - name: George Ungureanu
    email: ugeorge@kth.se
    affiliation: KTH Royal Institute of Technology, Sweden
figPrefix: [Figure, Figures]
secPrefix: [section, sections]
link-citations: true
linkReferences: true
abstract: |
  This document serves as a report and as a step-by-step tutorial for modeling complex heterogeneous systems in ForSyDe, with special focus on properties of concurrency and parallelism. The application under test is a radar signal processing chain for an active electronically scanned array (AESA) antenna provided by Saab AB. Throughout this report the application will be modeled using [ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow/) and [ForSyDe-Atom](https://forsyde.github.io/forsyde-atom/) modeling frameworks, gradually inreoducing new modeling concepts and pointing out the similarities and differences between the two. In ForSyDe-Atom we cover more advanced usage of models and their interpretation by showing two isomorphic, but intuitively different approaches to describing the same system: as a chain of processes operating on data blocks, vs. as a parallel network of streaming processes.
header-includes: |
    \usepackage{todonotes}
	\usepackage[margin=1in]{geometry}
	\hypersetup{colorlinks=true,citecolor=gray,urlcolor=cyan}
---

# Introduction{#sec:intro}

In order to develop more cost-efficient implementation methods for complex systems, we need to understand and exploit the inherent properties derived from the specification of the target applications and, based on these properties, be able to explore the design space offered by alternative platforms. Such is the case of the application studied in this report: the active electronically scanned array (AESA) radar is a versatile system that is able to determine both position and direction of incoming objects, however critical parts of its signal processing has significant demands on processing and memory bandwith, making it well out-of reach from the general public usage. We believe that a proper understanding of the temporal and spatial properties of the signal processing chain can lead to a better exploration of alternative solutions, ideally making it an affordable appliance in the context of current technology limitations. Nevertheless, expressing behaviors and (extra-functional) properties of systems in a useful way is far from a trivial task and it involves respecting some key principles:

* the language(s) chosen to represent the models need(s) to be _formally defined_ and _unambiguous_ to be able to provide a solid foundation for analysis and subsequent synthesis towards implementation.
* the modeling paradigm should offer the _right_ abstraction level for capturing the _needed_ properties [@lee-2015]. An improper model might either abstract away essential properties or over-specify them in a way that makes anaysis impossible. In other words it is the engineer's merit to find the right model for the right "thing being modeled".
* the models, at least during initial development stages, need to be _deterministic_ with regard to defining what _correct_ behavior is [@Lee18].
* at a minimum, the models need to be _executable_ in order to verify their conformance with the system specification. Ideally they should express operational semantics which are traceable across abstraction levels, ultimately being able to be synthesized on the desired platform [@Sifakis15]. 

[ForSyDe](https://forsyde.github.io/) is a design methodology which envisions "correct-by-constructuion system design" through formal or rigorous methods. Its associated modeling frameworks offer means to tacke the challenges enumerated above by providing well-defined composable building blocks which capture extra-functional properties in unison with functional ones. [ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow) is a domain specific language (DSL) shallow-embedded in the functional programming language Haskell, meaning that it can be used only for modeling and simulation purposes. It introduced the concept of _process constructors_ [@sander-2004] as building blocks that capture the semantics of computation, concurrency and synchronization as dictated by a certain model of computation (MoC). [ForSyDe-Atom](https://forsyde.github.io/forsyde-atom) is also a shallow-embedded (set of) DSL which extends the modeling concepts of ForSyDe-Shallow to systematically capture the interacting extra-functional aspects of a system in a disciplined way as interacting _layers_ of minimalistic languages of primitive operations called _atoms_ [@ungureanu17]. 

This documents presents several alternatives to modelling the AESA radar signal processing chain using ForSyDe-Shallow and ForSyDe-Atom, gradually introducing one modeling concept at a time, and pointing towards reference documentation. The report itself is written using [literate programming](https://en.wikipedia.org/wiki/Literate_programming), which means that all code snippets contained are _actual compiled code_ alternating with documentation text, and following it might require some initial clarifications. The remaining parts of @sec:intro will present a guide to using this document, as well as an introduction to the AESA application. In @sec:shallow we present an initial approach to modeling the AESA application in ForSyDe-Shallow, as chain of processes operation on cubes of data. In @sec:atom we first model the AESA application in ForSyDe-Atom from two different, yet isomorphic perspectives: in @sec:atom-operation we use the same approach as the previous one; followed by @sec:atom-network, where we regard the model as a parallel network of processes operating on individual incoming streams of data. 

## Application Specification{#sec:video-chain-spec label="Application Specification"}



## Using This Document
