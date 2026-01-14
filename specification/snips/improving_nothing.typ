#text("Improving Nothing", size: 17pt)
#linebreak()
The nix programming language is a pure and lazy programming languge with real-world usage in the nix package manager and NixOs operating system, and even though it has existed for over 20 years and is used close to exclusively in the nix-ecosystem with over 100.000 files written in it, it has not received a proper type system yet. The reason for that is not clear to the authors, but we suspect it roots from the unique features of the language, its unintuitive shadowing behaviour and laziness, that complicate principled type inference in a general sense.
Only the recent works of Lionell Parreaux and Stephen Dolan surrounding _algebraic subtyping_ have opened a new perspective to type inference for such an expressive languagage and motiviated this paper where we try to lay out the current state of type inference in nix, define a comprehensive _operational semantic_ and ultimately a _type system_ for a reduced part of the language. We also provide an implementation of a language server written in rust.

== Preface
I refer to this document as "paper" even though it could be a master-project/thesis etc. and the subject might still change to some extend.
