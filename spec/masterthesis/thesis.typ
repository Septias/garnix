

= A Soft-Typing Records Calculus with Asymmetric Concatenation


== Goal
> I want to create a typsystem that handles Nix as best as possible.



== Abstract
Asymmetric record concatenation is a _set-or-replace operation_ that, given two records, extends the fields of one operand with every unique field of the other and replaces fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that can not be statically typed: The expression `a: b: (a ‖ b).l` concatenates two type variables but can not be given a type without instantiating at least b, because of field-precedence and shadowing behaviour.
We propose a novel _soft type system_ based upon the work of Pazske&Xie with scoped-records, row-variables, asymmetric record concatenation, let-polymorphism, row-equivalence and an unknown type that solves the motivating example using a new lookup derivation _Γ ⊢ ρ.l ↓ r_ to delay record lookups and a _refinement technique_ upon typevariable instatiation to narrow types at term application.
We mechanically proof _type safety_ of the declarative system in Lean and give an efficient unification algorithm for a minimal calculus.

== Motivation
- Nix as a language
- Concatenation as a fundamental part of it
- Why a soft typing system is needed


== The Typesystem
- Scoped records as a basis
- An opportunistic lookup relation that can give ★
- Type-refinement upon type-var instantiation to recover from uncertainty
- Basically: I would love to motivate a subtyping hirarchy here


== Formal: The Typesystem
- Why do we need this let-version?


== Formal: Metatheory
- The proofs


== Towards Nix
> Section about extended features, limitations etc.


