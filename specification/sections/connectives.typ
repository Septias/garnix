#import "../functions.typ": *


#let export = [
  == Type Connectives
  Type connectives—union $τ_1 ∨ τ_2$, intersection $τ_1 ∧ τ_2$, and negation $¬τ$—play a central role in contemporary typed languages @flow @typescript @typed_racket @mlstruct @elixir_design_principles @poly_records @typing_records_etc. They provide a principled means of relating otherwise disjoint types and of expressing control‑flow–sensitive specifications. For example, the function `x: y: z: if x then y else z` returns either y or z depending on the boolean guard x and can be assigned the type $bool -> α -> β -> (α ∨ β)$, capturing that the result ranges over the union of the possible outcomes. Importantly, the connective tracks value flow: $α$ and $β$ are combined only at the output, not conflated at the inputs. This strictly improves on ML‑style unification, which would attempt to identify $α$ and $β$ and thereby fail for, say, integers versus strings @algebraic_subtyping.

  Intersection types support ad‑hoc overloading by ascribing multiple, behavior‑specific function types simultaneously. Consider `x: if isBool(x) then !x else x + 1`, which either negates a boolean or increments an integer. A coarse typing is $(bool ∨ int) -> (bool ∨ int)$; with intersections we refine this to $(int -> int) ∧ (bool -> bool)$, stating that integer inputs yield integers and boolean inputs yield booleans.

  Furthermore, type connectives empower occurrence typing and enable a form of _bounded polymorphsim_ @xie2020row @castagna2023programming.

  Nix provides another motivating instance for type connectives: the addition operator is overloaded across strings and paths. Expressions such as `/home/ + "john"` produce a path (`/home/john`), whereas `"/home/" + "john"` produces a string. A most general specification is therefore $(str -> (str ∨ path) -> str) ∧ (path -> (str ∨ path) -> path)$, which inherently relies on an intersection type.

  Because overloading is expressed via intersections in both input and output positions, Dolan’s polarity discipline for algebraic subtyping—restricting unions to covariant (output) positions and intersections to contravariant (input) positions @mlsub—does not suffice here. Consequently, constraints cannot be eliminated purely by polarity; a normalization procedure akin to that used by Parreaux @mlstruct is required.

]
