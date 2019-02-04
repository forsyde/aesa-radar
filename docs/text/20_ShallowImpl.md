# Model Implementations in ForSyDe-Shallow

[ForSyDe-Shallow](https://forsyde.github.io/forsyde-shallow/) is the
flagship and the oldest modeling language of the ForSyDe methodology
[@sander-2004]. It is a domain specific language (DSL)
shallow-embedded into the functional programming language Haskell and
uses the host's type system, lazy evaluation mechanisms and the
concept of higher-order functions to describe the formal modeling
framework defined by ForSyDe.

As a first exercise we present a rather naïve model the AESA
application in ForSyDe-Shallow which mainly "translates" specification
model as given and presented in [@fig:video-chain-spec] into
ForSyDe-Haskell. We gradually build the designer's mindset and
introduce modeling concepts while we parse the source code of the
ForSyDe model. The model file is found at
`<root>/src/ForSyDe/Shallow/AESA.lhs` and can be imported as a generic
library (e.g. in the interpreter session).

