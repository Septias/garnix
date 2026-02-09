#import "../functions.typ": *

#let export = [
  == From Static vs. Dynamic to Gradual to Occurrence
  Static type systems operate on the meta-level to deduce program properties without running the program. This can help to enforce safety properties like _null-saftety_ @pearce_flowtyping, _panic-freeness_, and _no use-after-free_ @rust, assisting programmers to write well-behaved programs. Especially type inference helps programmers to argue about complicated function calls, destructure nested datatypes and interact with unkown libraries. But  static type checking is a compile-time abstraction over a programs runtime behaviour and thus neccessarily an incomplete approximation thereof @coldwar. This leads to valid programs being rejected or lots of work to "make the compiler happy".

  For scripting, many languages have thus turned towards dynamic typing. Dynamic languages form the other side of the spectrum by not imposing any static properties on the program, giving ultimate expressiveness to the programmer at the cost of possibly unexpected runtime errors. This reduces the overhead of creating types and helps in rapid prototyping of applications and features like reflection are handy techniques. Only when programs get bigger and interactions between modules, functions, classes and services more complex, static type systems and their strong guarantees show their strength. The growing popularity of flow and typescript @flow @typescript shows the general trend towards type safe programming in the inherently dynamic languages like javascript and the need to mix the two approaches.

  Gradual type systems @gradual_siek @gradual_tobin try to strike the balance between static type inference and fully dynamic languages by allowing both to coexist. These typesystems accomodate two type systems in their torso, one annotated with static guarantees and a dynamic one. The boundary between the two is formed by an unknown type $star.op$ that is used in the dynamic part and casts that lift any to a concrete type. There exist quite a bit of research around the topic @gradual_siek @gradual_tobin @cantblamethis @agt @gradual_extensible_rows @consistent-subtyping @blame_for_all but most of the type systems come with type annotations, a feature we would like to dodge in this work. (TODO: too casual)

  A more promising approach is thus _flow typing_ and the more rigid _occurrence typing_ @revisiting_occurrence  that narrow a type based on its usage. For example, the function `if isBool(x) then !x else x + 1` checks the runtime value of x to be of type bool. After this conditional check, one can obviously type the positive branch under the assumption, that x is of type bool @typescript. Also, a negative type is needed to type the else-branch under the assumption, that x is not a bool. A similar technique can be used to refine the consecutive branches of pattern matching statements.

  In an example match-statement `match x with bool(x) -> .. | rec(x) -> .. | _ -> x` one matches the single branches in order. The branches behave like conditionals, forcing a type on the variable, the interesting case is the default-case. This one can be typed under the assumption, that it is not of type bool or record and show why one would like to use negation types.

  Using negation types, it is possible to add record field removal to a language like `{a: τ} ∧ ¬{b : τ}`.
  
  == Impurities <impurities>
  On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standart library thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves. For example the expression `g.${builtins.currentSystem}` that takes an attribute from a record `g` based on the currentSystem variable, that is baked into nix, is used 207 times in public code\*.

  While currentSystem could be computed by a typesystem in theory, predicting the variable `builtins.currentTime` is virtually impossible, as it would allow you to predict the future. A list of other impure functions are `currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion`. These functions can return arbitrary values.

  Since typing can not be solved completely (there will always be safe programs that can not be proven save with any typesystem), we need a _gradual type system_.

]

== Graduality
- We need a type system that can approximate (check builtins, untypable)
- If I have an any type it should be applicable to any function, because we can not make assumptions about it.
- It should thus be the subtype of every possible type.
- Should it be a supertype aswell?



