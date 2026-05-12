#import "../functions.typ": *


== Subtyping Relation

{l_i} ≤ {l_i}^true
{l_j} ≤ {l_i}^false | l_i != l_j
{}

== Operator types

$
  #b[add]: str -> str ∨ path -> str
  #b[add]: path -> str ∨ path -> path
  #b[add]: int -> int -> int
  #b[add]: float ∨ int -> float -> float
  #b[add]: float -> float ∨ int -> float
  // records
  #b[?]: record -> label -> bool
  #b[or] null -> τ -> τ
  #b[or] τ₁ ∧ ¬null -> τ₂ -> τ₁
$


== Adjusting for arbitrary types:



== Inference
Basically I argue that we have to add strings as first order primitives that can reduce to labels if used in these locations.

