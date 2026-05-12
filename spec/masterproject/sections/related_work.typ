#import "../functions.typ": *
== Related Work
@verified has made three interesting decisions in their formalization:

1. Rec/nonrec attribution
2. Deep/shallow evaluation
3. Operators as relations
4. Matching

The distinction between rec ond non-rec is due to this problematic example: `rec {inherit x;} -> rec {x = x;}`. If we were to add a simple rewrite rule for inherits, we would falsly add a new recursive binding in the example which is not how the interpreter handles it. The interpreter tries to lookup x in the context without the record-bindings. Meaning if x is defined, there will be a binding `x = x'` and if it is not defined, an error will be thrown. We decided to diverge from the formulation given in @verified by not adding inherit as syntactic sugar but as a inference rule with a premise that ensures x is defined. This is done by checking the context whether this variable exists.

#derive("T-inherit", ($x ∈ Γ$,), $"inherit" x; -> x = x;$)
#derive("T-inherit2", ($x ∈ Γ$,), $"inherit" (ρ) x; -> x = ρ.x;$)

We also yeet the distinction between deep and shallow evaluation. For an interpreter that conforms to the spec it is important to diverge iff the previous implementation diverges. In our typesystem we do not want to derive termination properties (which would solve the halting problem) so we don't need to make this distinction.
Due to this shift of focus, we finally also drop operators that are modeled with relations that accounts for the lazyness of the second operand.

- We want to note that @verified falsly assume that nix does not provide the power of an `eval` function that can interpret arbitrary nix code. Using the builtin functions, this is indeed possible.

```
let code = "let age = 13 in age;" in import (builtins.toFile "dyn.nix" code) → 13
```

#bib
