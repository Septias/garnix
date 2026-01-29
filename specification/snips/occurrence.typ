#import "../functions.typ": *

== About Gradual Typing
- Neg: We don't want type annotations
- Neg: We can not insert dynamic checks

== Graduality
- We need a type system that can approximate (check builtins, untypable)
- If I have an any type it should be applicable to any function, because we can not make assumptions about it.
- It should thus be the subtype of every possible type.
- Should it be a supertype aswell?


== Flow Typing
Besides the afore-mentioned record-field-check, nix provides function that can dynamically check the type of expressions. It is thus possible to write expressions like `if isStr t then {} else {}` where we can type t as $t ∧ str$ in the positive case and $t ∧ ¬str$ in the negative else-branch but for this we _need_ negative types.

There at least are no patterns, but one could also argue, that patterns are just a generalization of conditionals.

The last constructs help in narrowing down value types. They actually just check the type of values `isAttrs, isBool, isFloat, isFunction, isInt, isList, isNull, isPath, isString`.

`isAttr(t): And -> bool`, but this should also result in `t ∧ bool` in positive-branch and `t ∧ ¬bool` in the negative.

This can actually implement _type cases_!?


== Impurities <impurities>
On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standard library thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves.

`currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion` these functions can return arbitrary values. Since nix can use them them in combination with dynamic accesses, the type systems becomes an evaluator. We thus _need_ a gradual type system.

