
= A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix
== Goal
> I want to create a typsystem that handles Nix as best as possible. It should be efficiently computable and have no "breaking" points. Meaning, there is nothing in it that makes it immediately unfeasible for Nix. This is why we need a soft-typing type as well as row- and label variables. The result should be efficiently computable.


== Abstract
Asymmetric record concatenation with left-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that can not be statically typed: The expression `a: b: (a ‖ b).l` concatenates two type variables but can not be given a type without instantiating at least b, because of field-precedence and shadowing behaviour.
We propose a novel _soft type system_ based upon the work of Paszke&Xie with scoped-records, row-variables, asymmetric record concatenation, let-polymorphism, row-equivalence and an unknown type that solves¿ the motivating example using a new lookup derivation _Γ ⊢ ρ.l ↓ r_ to delay record lookups and a _refinement technique_ upon type variable instantiation to narrow types at term application.
We mechanically prove _type safety_ of the declarative system in Lean and give an efficient unification algorithm for a minimal calculus.


== A Note about Nix
NixLang is the fundamental language of one of the largest bodies of untyped functional code in existence and a language that extends beyond the usual λ-calculus features. The foundational core of the language are records, with many language constructs to create, change and deconstruct these. Two features that make static typing notably hard are first-class labels and the asymmetric record concatenation operation. Only a few systems exist in literature that can handle both features.

A complete typesystem for Nix is not possible due to impurities (in an otherwise pure language) that can poison typeability. Using first-class labels and the impure builtin `builtins.currentTime`, it is possible to form an expression that looks up a record field based on the wall-clock time:

```nix
{ before = "moin"; after = 0; }.${if builtins.currentTime < 1767225600 then "before" else "after"}
```

The type of this selection depends on the moment of evaluation, so this is an obviously untypable operation: typing it would predict the future.

The design constraints for a typesystem that types Nix are as follows: Full record calculus strength with first-class labels and the problematic asymmetric concat operation are essential to provide usable type-inference. Computability is an essential design constraint as backtracking would render type inference unusably slow. Lastly, a typesystem is needed that admits unavoidable uncertainty with an unknown type ★ similar to the one used in TypeScript or occurrence typing spearheaded by Castagna.


== Motivation
Asymmetric record concatenation is a central problem that many record calculi address. Its set-or-update behaviour in combination with polymorphism makes tracking of fields extremely hard, and multiple approaches have been suggested that come at different costs. Row polymorphism is a method to track positive information of records but is unable to track the absence of fields. Without negative information and width-subtyping, overwriting fields is an unrecoverable operation, since width-subtyping can remove a field a: {l: τ} -> a: {} without a trace, and concatenating such a record with b: { l: τ'} can not be clearly resolved due to shadowing.

This unfortunate situation can be remedied by lacks-predicates, or stronger type systems like the one by Ohori or the line of work of (Abstracting Records…) that faithfully track positive and negative information with constraint or dependent types. But both come at the cost of computability. The systems by @? are theoretically astonishing but reduce to System F, where type inference is known to be undecidable and the systems of Ohorie add the full dependend-type complexity to type systems¿.

Our approach, RowNix, positions itself in the middle of both extremes and admits the uncertainty that different kinds of operations can induce by using an unknown type that directly marks uncertainty. Our motivating example admits such a type `a: b: (a ‖ b).l :: ? -> ? -> ★` because it is statically not possible to determine the return type. By surrendering to some form of uncertainty we can adjust the unification algorithm of Paszke&Xie to a system that can be computed efficiently¿.

In our record calculus, uncertainty is recorded during field-lookup and remedied as well as possible upon function application. To retain as much information as possible, we use scoped rows and a concatenation operation that glues together two records without simplifying either side directly. This, in combination with our three-way lookup relation, gives surprising expressiveness to our system. In our system, lookup is a relation that extends the usual negative and positive results of lookup with an unknown marker `?` that is emitted as soon as a row-variable is hit, since shadowing behaviour after that point is not clear.

The concatenation inside the example `a: ({l: τ} ‖ a).l` will produce a row `(α | l: τ)` with a type variable for the function argument. Upon instantiation at the application site, the row-variable can be eliminated such that the lookup relation that was previously stuck before finding a field can advance further into the row, find the l: τ binding, and return a proper type τ.

== Contributions

- *A declarative soft type system for records.* We extend the row theory of Paszke&Xie with _scoped rows_, _asymmetric concatenation_ with left-precedence, _row-equivalence_, _let-polymorphism_ and an _unknown type_ ★ that marks statically unresolvable operations instead of rejecting the program.

- *A best-effort lookup relation.* Our lookup relation `Γ ⊢ ρ.l ↓ r` extends the usual positive and negative results of lookup to a three-way result (τ | ⊥ | ★). The relation can consult row-solutions in the context, and its _monotonicity_ — definite results survive extending the solutions, only ★ can improve — is what makes deferring lookups sound.

- *Type refinement at instantiation.* Uncertainty introduced by lookup is remedied at application sites: instantiating a type variable lets a previously stuck lookup advance further into the row and promote ★ to a definite type.

- *Mechanized type safety.* We prove a form of _progress_ that admits some runtime errors and _preservation_ for the minimal calculus in Lean.

- *An algorithmic system.* We give an efficient unification algorithm for the minimal calculus, extending the algorithm of Paszke&Xie to rows containing the unknown type.


== The Typesystem
#include "./snips/minimal-ts.typ"


== Formal: The Typesystem
- Why do we need this let-version?


== Formal: Metatheory
- The proofs


== Towards Nix
> Section about extended features, limitations etc.


