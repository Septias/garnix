#import "../functions.typ": *

== Related Work
We decided to diverge from the formulation given in @verified by not adding inherit as syntactic sugar but as a inference rule with a premise that ensures that no new recursion was introduced during type inference. This is done by checking the context whether this variable exists.

We also yeet the distinction between deep and shallow evaluation as that does not have an effect on type systems. Also, it is to too important for the program language semantics. Due to this shift of focus, we finally drop operators that are modeled with relations and replace them with inference rules. These inference rules

@verified has made three interesting decisions in their formalization:

1. Rec/nonrec attribution
2. Deep/shallow evaluation
3. Operators as relations
4. Matching

We diverge from this representation quite a bit. First and foremost, NixLang @verified follows the first semantic of Dolan @memory_to_software @dolstra_phd and annotates every record field as recursive or not. The reason being a subtlety of the inherit statement. Both systems handle inherit by adding rewriting rules, that turn expressions of the form `inherit (a) x;` into something like `x = a.x;` in records or let-bindings. When used in conjunction with recursive records, this leads to unwanted recursion. The statement `inherit x;` will be desugared into `x = x;` and in a record `rec {x = x;}` this will lead to infinite recursion. That is why there needs to be distinction between the two and annotating every field is one way to solve it – albeit a very noisy one. We choose to handle inherit with a reduction rule that makes sure in the side-codition, that make sure x exists in the context.

@verified also makes a distinction between deep and shallow evaluation. In a lazy equi-recursive language, recursive definitions have to be unrolled one at a time, usually only on the surface level. It might be of interest though to

#derive(
  "T-Inherit",
  (${ p.l = l }$, $p.l ∈ Γ$),
  ${ #b[inherit] (p) l }$,
)

#derive(
  "T-Inherit-2",
  (${ l = l; }$, $l ∈ Γ$),
  ${ #b[inherit] l; }$,
)


== Impurities <impurities>
On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standart library thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves.

`currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion` these functions can return arbitrary values. Since nix can use them them in combination with dynamic accesses, the type systems becomes an evaluator. We thus _need_ a gradual type system.


== Unholy, Untypable
`a: b: with a; with b; x`

An idea I had for this one is, that you keep the constraint lying around, that any of the supplied arguments needs to have the argument. Now the one important thing is to see, if this approach is actually applicable during type inference and whether this information leaks to wide and can not be resolved anymore…

One good thing is, that type inference is always as complete as an evaluator could be. We might get non-termination from it, but that is generally not too bad maybe?


== Inspection
There are a few functions that allow the program logic to take into account the actual value at hand. For example te check-operator. This one is well-behaved, so why am I scared of `attrNames`. This gives back alle the keys defined on a record. Since we anyways track explicit records, that should be no problem?

`(f: attrNames f): {} -> {}`
`(b: with attrNames f; b): {} \ {}`


=== Problematic Children
The standart library extends the languag' features beyond the simply syntactic ones. There are functions that extract the keys and values from records, namely `attrNames` and `attrValues`. These give inspection/reflection like features to the language, since they can be used to access fields: `r: map (attrNames rec) (x: r.x)`.

The `getAttr` field is the simple case of the former attrNames and the `hasAttr` can be used to gain flow information. The other two functions `interspectAttr` and `mapAttr` don't add too much. For arrays, we get some general property asserting functions, but since we are not a depent type system, that does not really help.

Inspecting of function args is also a funny feature. `functionArgs {a, b ? 2}: 3  -> { a = false; b = true;}` so this way one can programatically find out which arguments to supply to a function. An llm-call could then actually synthesis some arguments before calling a function xd.

- *Records*: attrNames, attrValues, getAttr, hasAttr, intersectAttrs, mapAttrs
- *Array*: elem, elemAt, head, length, listToAttrs
- *Inspecting*: functionArgs
- *Impure*: currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion
- *flow*: isAttrs, isBool, isFloat, isFunction, isInt, isList, isNull, isPath, isString



#bib
