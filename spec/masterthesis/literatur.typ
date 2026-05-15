== Read
- A record calculus based on symmetric concatenation
  - Hilfreich weil: Übersicht records, quantified
- [x] Local Type inference
  - Hilfreich weil: Locality, Abstraction, Records, Further Reading
- Subtyping recursive types
  - Hilfreich weil: Wir auch rekursive Typen haben
- [x] Tabular Data
  - Hilfreich weil: Ist fast das, was ich möchte


== Records
- Daan Leijen
  - Extensible records with scoped labels
  - First class labels for extensible rows
- [[Infix-Extensible Record Types for Tabular Data.pdf]]
  - Based On: Daan Leijen
- [[Extensible Data Types with Ad-Hoc Polymorphism.pdf]]
  - Based On: [[Abstracting Extensible Data Types.pdf]]
- [[Abstracting Extensible Recursive Functions.pdf]]
  - Based on: [[Generic Programming with Extensible Data Types.pdf]]
    - Based on: [[Abstracting Extensible Data Types.pdf]]


== Tabular Data
⊕ Unification
⊕ First class labels
⊕ Polymorphic Rows
⊖ Kein Subtyping
⊖ Can not model "extend or overwrite"
- Extension is always to the left
- Uses an _instantation_
- Uses _conditional tail check_


== Abstracting Extensible Data Types
⊕ Variants & Records
⊕ Asymmetric Concat
⊕ Expressive constraints: $\_ ⊙_~ \_$(containment) and $ζ ⧀ ζ$(subtype)
⊖ System F (Explizite type-application)
⊖ Keine Inferenz für Funktionen


== Lacks Predicates
⊖ probably needs explicit types?
