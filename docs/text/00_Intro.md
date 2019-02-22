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

# Introduction

In order to develop more cost-efficient implementation methods for complex heterogeneous systems, we need to understand and exploit the inherent properties derived from the specification of the target applications and, based on these properties, be able to explore the design space offered by several alternative heterogeneous platforms. 

## Application Specification{#sec:video-chain-spec label="Application Specification"}


## Using This Document
