# Validating a ForSyDe Model Against the Specification {#sec:props}

> _This section presents a practical and convenient method for verifying the
> conformance of a ForSyDe model against a set of specification properties. In order to
> do so we make use of the [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/)
> framework, which offers an EDSL for specifying system model properties and a random
> test-case generation engine for validating a design under test (DUT) against these
> properties. Although the verification is not exhaustive, it is "smart" enough to
> quickly identify and correct discrepancies between specification and model
> implementation, making QuickCheck an ideal tool in a system designer's toolbox._

|         |                               |                                                      |
| -----   | -------------------------     | --------------------------------------------------   |
| Package | aesa-atom-0.1.0               | path: `./aesa-atom/README.md`                        |
| Deps    | forsyde-atom-0.2.2            | url: `https://forsyde.github.io/forsyde-atom/api/`   |
|         | forsyde-atom-extensions-0.1.1 | path: `./forsyde-atom-extensions/README.md`          |
|         | QuickCheck-2.13.1             | url: `http://hackage.haskell.org/package/QuickCheck` |
| Suite   | tests-cube                    | usage: `stack test :tests-cube`                      |

[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/) [@claessen-2011-quick] is a
DSL embedded in Haskell for specifying denotational properties of functions, and a
test case generator engine based on type algebras to obtain a good coverage of a
DUT. As it is based on type algebras, it uses the insights from the DUT's type
constructors to build automatic or user-guided strategies for identifying edge or
violating cases within a relatively small number of test cases. Due to its random
nature it is capable of finding failing sequences of inputs, which are then
\emph{shrinked} to define minimum violating cases, making it very useful in
development iterations. 

A very good article motivating QuickCheck's usefulness in system design is found on
[Joe Nelson's blog](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html),
from which we extract the following paragraph: "Proponents of formal methods sometimes
stress the notion of specification above that of implementation. However it is the
inconsistencies between these two independent descriptions of the desired behavior
that reveal the truth. We discover incomplete understanding in the specs and bugs in
the implementation. Programming does not flow in a single direction from
specifications to implementation but evolves by cross-checking and updating the
two. Property-based testing quickens this evolution. Of course the only way to truly
guarantee properties of programs is by mathematical proof. However property-based
tests approximate deductive confidence with less work by checking properties for a
finite number of randomized inputs called test cases."

**DISCLAIMER:** this section assumes that the reader is familiar with QuickCheck's
syntax and has gone through a few hands-on tutorials. For more information we
recommend checking out either the article of @hughes-2007, John Nelson's [blog article
on QuickCheck](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html), or
the (slightly outdated) [official QuickCheck
tutorial](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).

## Formal Notation {#sec:prop-notation}

Properties in the QuickCheck DSL are formulated as \emph{pre-condition} $\Rightarrow$
\emph{statement}, where pre-conditions are defining generators for legal inputs under
which the statement is evaluated.

In order to ease the understanding of the QuickCheck code, for each property we will
also provide a short-hand mathematical notation using the \emph{pre-condition}
$\Rightarrow$ \emph{statemen} format, using the following conventions:

| Notation | Meaning             |
|---------:|:--------------------|
| $\langle\alpha\rangle$     | `Vector a`                                                                 |
| $|v|$                      | `length v`                                                                 |
| $\overline{s}$             | a signal with events with the type of `s`                                  |
| $\langle a,b,c,...\rangle$ | `vector [a,b,c,...]`                                                       |
| $\{a,b,c,...\}$            | `signal [a,b,c,...]`                                                       |
| $\Sigma(s)$ or $\Sigma(v)$ | the (ordered) sequence with the elements from a signal `s` or a vector `v` |


