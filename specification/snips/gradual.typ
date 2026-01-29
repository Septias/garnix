#import "../functions.typ": *

== Gradual Type Systems
We are finally arriving at an approximative typesystem that tries to infer types at a best-effort basis. Maybe we can even help the situation with a bit of _bidirectional type systems_? We anyways have to add an `Any` type that allows the system to be inexplicit from time to time. What does that mean for the subtyping hirarchy? If I have an any type it should be applicable to any function, because we can not make assumptions about it. It should thus be the subtype of every possible type. Should it be a supertype aswell?





== Impurities <impurities>
On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standard library thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves.

`currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion` these functions can return arbitrary values. Since nix can use them them in combination with dynamic accesses, the type systems becomes an evaluator. We thus _need_ a gradual type system.

