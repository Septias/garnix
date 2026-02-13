#import "../functions.typ": *


#let export = [
  == Type Connectives
  Logical _type connectives_ like union $τ ∨ τ$, intersection $τ ∧ τ$ and negation $¬τ$ have found their way into many mainstream languages @flow @typescript @typed_racket @mlstruct @elixir_design_principles @poly_records @typing_records_etc, proving their usefulness by giving an intuitive relation of otherwise unrelatable types. For example, a function that uses a conditional `x: y: z: if x then y else z` is a function that returns either y or z based on whether x is true or false. This function can be typed at $bool -> α -> β -> (α ∨ β)$, intersecting the possible return types. This is a great improvement to previous ml-like systems that would have to _unify_ α and β in this situation which is already fails for integers and strings @algebraic_subtyping. The intersection type respects the variable _flow_, only merging α and β because they flow together in the output, not merging them in the input.

  *overloading*: Using intersection types, one can define functions that have many types. For example, the function `if isBool(x) then !x else x + 1` is a function that either inverts a bool or increments an integer. We would like to describe the argument x with an unbound type-variable α, but from the function body it is clear, that this function is only well-behaved on integers and bools. This function can be given two types. The first one $(bool ∨ int) -> (bool ∨ int)$ states that the function accepts argument of either bool or int and will return either an int or bool. But using intersection types, the functiontype can be refined to the more specific type $(int -> int) ∧ (bool -> bool)$, stating that if the function is called with an integer, it will also return one (instead of the union $int ∨ bool$).

  We have already seen their usage in occurence typing systems.

  This form of overloading is needed for nix' addition operator that can be used on strings and paths alike. It is possible to write expressions like `/home/ + "john" -> /home/john` that will return a _path_ and `"/home/" + "john"` which will return a _string_. The most general type is thus `(str -> (str ∨ path) -> string) ∧ (path -> (str ∨ path) -> path)`, needing an intersection type.

  We want to note that because of the overloading usage of intersection types, it is not possible to restrict unions to output positions and intersections to input positions as Dolan did @mlsub. It is thus not possible to easily destructure constraints without a normalization routine similar to the one of Parreaux.
]
