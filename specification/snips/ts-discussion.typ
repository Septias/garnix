#import "../functions.typ": *

- We don't want type annotations

== Recursiveness
You can decide between equi-recursive and iso-recursive when modeling a typesystem.
Equi-recursive has the benefit that you can just declare the infinite rollout of recursive type _euqal_ to their un-rolled out form whereas in iso-recursive type systems, you need explicit rollout operations. The main drawback of equi-recursiveness is is that it comes with high algorithmic complexity and heavy metatheory due to its co-recursive definition. Zhou et. al showed how to create an efficient algrothim for iso-recursive type systems in their develpment of Quicksub @quicksub. In addition, despite being less convenient, iso-recursive types are known to have the same expressive power as equi-recursive types @quicksub.


== Related Work
@verified has made three interesting decisions in their formalization:

1. Rec/nonrec attribution
2. Deep/shallow evaluation
3. Operators as relations
4. Matching

The distinction between rec ond non-rec is due to this problematic example: `rec {inherit x;} -> rec {x = x;}`. If we were to add a simple rewrite rule for inherits, we would falsly add a new recursive binding in the example which is not how the interpreter handles it. The interpreter tries to lookup x in the context without the record-bindings. Meaning if x is defined, there will be a binding `x = x'` and if it is not defined, an error will be thrown.

We decided to diverge from the formulation given in @verified by not adding inherit as syntactic sugar but as a inference rule with a premise that ensures x is defined. This is done by checking the context whether this variable exists.

#derive("T-inherit", ($x ∈ Γ$), $"inherit" x; -> x = x;$)
#derive("T-inherit2", ($x ∈ Γ$), $"inherit" (ρ) x; -> x = ρ.x;$)

We also yeet the distinction between deep and shallow evaluation. For an interpreter that conforms to the spec it is important to diverge iff the previous implementation diverges. In our typesystem we do not want to derive termination properties (which would solve the halting problem) so we don't need to make this distinction.
Due to this shift of focus, we finally also drop operators that are modeled with relations and replace them with inference rules. We argue that these give a better intuitive understanding of the operators. A set of (key, val) pairs from which the operator value can be taken does not immediately show of which type the returned value is. For example, the addition operator is defined on strings and paths. The returned set would thus be of type {str ∨ path -> str ∨ path}, loosing precision.


and in a record `rec {x = x;}` this will lead to infinite recursion. That is why there needs to be distinction between the two and annotating every field is one way to solve it – albeit a very noisy one. We choose to handle inherit with a reduction rule that makes sure in the side-codition, that make sure x exists in the context.


== Impurities <impurities>
On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standart library thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves. For example the expression `g.${builtins.currentSystem}` that takes an attribute from a record `g` based on the currentSystem variable, that is baked into nix, is used 207 times in public code\*.

While currentSystem could be computed by a typesystem in theory, predicting the variable `builtins.currentTime` is virtually impossible, as it would allow you to predict the future. A list of other impure functions are `currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion`. These functions can return arbitrary values.

Since typing can not be solved completely (there will always be safe programs that can not be proven save with any typesystem), we need a _gradual type system_.

== Unholy, Untypable
`a: b: with a; with b; x`

An idea I had for this one is, that you keep the constraint lying around, that any of the supplied arguments needs to have the argument. Now the one important thing is to see, if this approach is actually applicable during type inference and whether this information leaks to wide and can not be resolved anymore…

One good thing is, that type inference is always as complete as an evaluator could be. We might get non-termination from it, but that is generally not too bad maybe?


== Inspection
There are a few functions that allow the program logic to take into account the actual value at hand. For example te check-operator. This one is well-behaved, so why am I scared of `attrNames`. This gives back alle the keys defined on a record. Since we anyways track explicit records, that should be no problem?

`(f: attrNames f): {} -> {}`
`(b: with attrNames f; b): {} \ {}`


=== Problematic Children

#bib
\*https://sourcegraph.com/search?q=context:global+%24%7Bbuiltins.currentSystem%7D&patternType=keyword&sm=0
