#import "../functions.typ": *


#let export = [
  == Type Connectives <connectives>
  Type connectives—union $τ_1 ∨ τ_2$, intersection $τ_1 ∧ τ_2$, and negation $¬τ$—play a central role in contemporary typed languages @flow @typescript @typed_racket @mlstruct @elixir_design_principles @poly_records @typing_records_etc. They provide a principled means of relating otherwise disjoint types and of expressing control‑flow–sensitive specifications. For example, the function `x: y: z: if x then y else z` returns either y or z depending on the boolean guard x and can be assigned the type $bool -> α -> β -> (α ∨ β)$, capturing that the result ranges over the union of the possible outcomes. Importantly, the connective tracks value flow: $α$ and $β$ are combined only at the output, not conflated at the inputs. This strictly improves on ML‑style unification, which would attempt to identify $α$ and $β$ and thereby fail for, say, integers versus strings @algebraic_subtyping.

  Intersection types support ad‑hoc overloading by ascribing multiple, behavior‑specific function types simultaneously. Consider `x: if isBool(x) then !x else x + 1`, which either negates a boolean or increments an integer. A coarse typing that only uses type unions is $(bool ∨ int) -> (bool ∨ int)$ stating that the function accepts either a bool or int and returns either a bool or int. By admitting intersection types, this type can be refined to $(int -> int) ∧ (bool -> bool)$, stating that integer inputs yield integers and boolean inputs yield booleans; the strictly more expressive type.

  Furthermore, type connectives empower _occurrence typing_ and enable a form of _bounded polymorphsim_ @xie2020row @castagna2023programming.
]
