#import "../functions.typ": *

== TODO
- Are row-varibles actually better in gradual systems?


== Systems
- Parreaux
- Castagna
- Rows(Wand): No principality for record label overwriting
- Lacks(Remy): Only add if some qulifide type fits
- Scoped(Leijin): lifts uniqueness

(Gaster & jones: A polymorphic type system for extensible records and variants)

== Introduction

Records have been studied in a variety of papers [..] and can be partitioned in roughly 3 groups. The first model of records is a syntactic model where the syntax defines what a record is. This approach is conceptually simple but hard to extend because of its verbose nature and exploding rule-complex. To overcome these shortcomings, \@wand studied _row polymorphism_. Row polymorphism extend record with a generic row r, effectively making them polymorphic in their "rest". By extending the row to lacks-predicates not only extension, but also restriction of record types can be achieved, giving a lot of flexibility in theory. While strong in theory, the theory gets complex and unwildy fast, making it hard to integrate into fully-fledged type systems. _Semantic subtyping_, developed over multiple years by Castagna et. al. @gentle_intro @poly_records @typing_records_etc to name a few, tries to remedie this by shortcoming by giving records a set-theoretic semantic model.

Wand: [31] Mitchell Wand. Type inference for record concatenation and multipleinheritance. Information and Computation, 93(1), 1991.

The principal idea of this approach is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, in this regard giving types a semantic meaning. In the set-theoretic model of types, type-union relates to set-unions $τ_1 ∨ t_2 arrow.double ⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $t_1 ∧ t_2 arrow.double ⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $¬τ arrow.long 𝟙 without ⟦τ⟧$.

Stephen Dolan proposed a new family of type systems, named _algebraic type systems_. These systems tackle language construction from a new point of view. Instead of adding types first and then trying to find a semantic model for them, Dolan argues one should pay more attention to finding a semantic model for the types _first_. The types in _algebraic type systems_ form a distributive lattice (thus algebraic) and inherit the lattice' properties. By further restricting the the occurences for union and intersections to positive and negative positions, a distributive lattice can be constructed that allows for lossless reduction of subtyping constraints. In essence, the system is standart ML, with a lattice of types and unification replaced by bi-unification, a subroutine that handles subtyping constraints instead of equality constraints. The final algorithms for subsumption checking and type inference are short as well as simple, all thanks to the initial focus on well-formed types. The final algorithms inherit the standart ML properties, namely _principled type inference_, no need for type annotations and effectiveness i.e no backtracking.

Parreax et al @simplesub @mlstruct @superF showed how to take the idea of boolean algebraic subtyping and implement it based on constraints in the form of lower and uppor bounds on type variables that can be used to implement the approach.
The first thing one needs to do is to form a _boolean algbebra_ of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually leading to primitive  constraints like $"Bool" < top$ that can be added to type variables.



The other idea is from Castagna et. al. @typing_records_etc who implements first-class labels for maps in a _semantic subtyping_ setting. The difference between the two approaches (see [simple essence of boolean algebraic subtyping] aswell) is, that one uses a row variables `{ a = b; | ξ}` while the other uses negation types and boolean reduction `{a: τ₁} ∧ {a: ¬T} ∧ {a: τ₂} -> {a : t₂}`. This allows them to address records. Otherwise, one could just build the conjunction and and give priority to later elements. It is not clear though, this affects the type inference algorithm.



== Comparison-draft
- Dolan: Lattice of types, Extensional,
- Castagna: Sets, Denotational, Universes, Occurrence, Functino-annotations,
- Parreaux: Boolean Algebraic, Syntactic, Verbose, Levels, Bounds, Skolems, Rigid Vars,

It is funny, how the approaches of Parreaux and Castagna are generally similar. They both want to have fully fledged boolean types and they use some strategy to grind down constraints. They also both use some strategy to bring them into a normal form and they use some non-standart¿ subtyping rules to complete the lattice. One difference are upper and lower bounds, that are only used in the Work of Parreaux. On the other hand, I think, Castagna uses constraint types? What are coercion type systems again?

The Workhorse in Parreaux: Carefully crafted inference rules, Normal-Forms
The Workhorse in Castagna: Boolean Formulas, Reduced to Normal-Forms



#bib
