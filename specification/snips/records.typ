#import "functions.typ": *


- Are row-varibles actually better in gradual systems?


== Systems
- parreaux
- castagna
- Rows(Wand): No principality for record label overwriting
- Lacks(Remy): Only add if some qulifide type fits
- Scoped(Leijin): lifts uniqueness

(Gaster & jones: A polymorphic type system for extensible records and variants)

== Misc
The type systems that epitomizes this idea are typesystem called  _semantic subtyping_, mainly developed by Castagna et. al. The principal idea of this approach is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, in this regard giving types a _semantic meaning_. In the set-theoretic model of types, type-union relates to set-unions $⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $𝟙 without ⟦τ⟧$.

Records have been studied in a variety of papers [..] and can be partitioned in roughly 3 groups. The first model of records is a syntactic model where the syntax defines what a record is. This approach is conceptually simple but hard to extend because of its verbose nature and exploding rule-complex.
To overcome these shortcomings, \@? studied _row polymorphism_. Row polymorphism extend record with a generic row r, effectively making them polymorphic in their "rest". By extending the row to lacks-predicates not only extension but also restriction of record types can be achieved, giving a lot of flexibility in theory. While strong in theory, their theory gets complex and unwildy fast, making it hard to integrate into fully-fledged type systems. _Semantic subtyping_, developed over multiple years by Castagna et. al. @gentle_intro @poly_records @typing_records_etc to name a few, tries to remedie this by shortcoming by giving records a set-theoretic semantic model.

TODO: dolan vs. castagna vs. parreaux

Algebraic subtyping \@dolstra_phd is a technique to get well-behaved types and neat type inference. After \@simplesub and \@mlstruct we know how to pratically implement it. The first thing one needs to do is to form a boolean algbebra of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually landing at primitive constraints like $"Bool" < top$ that can be solved trivially.
