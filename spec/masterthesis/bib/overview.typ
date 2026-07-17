== Read
- [x] _A record calculus based on symmetric concatenation_
  - Hilfreich weil: Übersicht records, quantified
- [x] _Local Type inference_
  - Hilfreich weil: Locality, Abstraction, Records, Further Reading
- [ ] _Subtyping recursive types_
  - Hilfreich weil: Wir auch rekursive Typen haben
- [x] _Tabular Data_
  - Hilfreich weil: Ist fast das, was ich möchte
- [~] _Algorithm M_
- [x] _A polymorphic typesystem for_... (section: Inference)
  - Weil Inference
- [x] _Designing Record Systems_
  - Weil inference und FC-Labels
  - Mit HM(x) ein bisschen strange, inference nicht genau klar und nur theoretisch
- [x] Algorithm M

== Records
- Daan Leijen
  - _Extensible records with scoped labels_
  - _First class labels for extensible rows_
- [[Infix-Extensible Record Types for Tabular Data.pdf]]
  - Based On: Daan Leijen
- [[Extensible Data Types with Ad-Hoc Polymorphism.pdf]]
  - Based On: _[[Abstracting Extensible Data Types.pdf]]_ - Morris
- [[Abstracting Extensible Recursive Functions.pdf]] - Alex & Morris
  - Based on: [[Generic Programming with Extensible Data Types.pdf]] - Alex & Morris
    - Based on: _[[Abstracting Extensible Data Types.pdf]]_ - Morris


== Tabular Data
⊕ Unification
⊕ FC-labels
⊕ Polymorphic Rows
⊖ Kein Subtyping
⊖ _Can not model "extend or overwrite"_
- Extension is always to the left
- Uses an _instantation_
- Uses _conditional tail check_


== Abstracting Extensible Data Types
⊕ Variants & Records
⊕ Asymmetric Concat
⊕ Expressive constraints: $\_ ⊙_~ \_$(containment) and $ζ ⧀ ζ$(subtype)
⊖ System F (Explizite type-application)
⊖ _Keine Inferenz für Lets_


== Generic Programming with Extensible Rows
> Map rows to things and back?
⊕ FC-Labels
⊖ Zeigt keine Inferenz


== Local Type Inference
- Overapproximation of types?
- Thats a possible way to calculate best types w/o backtracking
- Bot-type ist schwierig (generell)
⊖ keine let-generalization
⊖ Toplevel annos sind notwendig

== A polymorphic typesystem for
⊖ No double labels?


== Designing Record systems
⊖ No overloading of field variables
⊖ HM(X) does not tell you how to implement X.


== Type systems with constrained types (no marks possible)
- ∃ᾱ binds some subset of variables ᾱ in the constraint
- Constraints are boolean algebras



= Fixed to some properties
> For the comparison section, I want to pick out some? record calculi that are relevant and why they can to be used by us. Basically a few newer record calculi should be there. Record calculi by Castaga and Parreaux.

