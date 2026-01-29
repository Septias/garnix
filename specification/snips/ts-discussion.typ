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


== First Class Labels
First class labels allow computation on record labels. It is thus possible to access record fields with a computed label. In general this lookis like `r.t` where r is a record and t is an arbitrary expression. Since nix' string interpolatio is allowed in paths and as record accesses, we need to add it to the language.

In research there exist different approaches to the problem of first-class labels.

It might be the only reasonable approach to apply something like @verified and put substitions into the syntactic domain. Another possible approach is to actually evaluate stuff, but it is hard to see whether we find any end to it or not. For example, strings could be generated in loop `n: x: if n>0 then x + "x"`. Maybe it is actually a good thing to use semantic subtyping, because we anyways want to use set-theoretic notation for _with_.

The other idea is from @poly_records. This one implements first-class labels for maps. Is it because they are constant in the default value? It might actually be that. The difference between the two approaches (see [simple essence of boolean algebraic subtyping] aswell) is, that one uses a row variables `{ a = b; | ξ}` while the other uses negation types and boolean reduction `{a: τ₁} ∧ {a: ¬T} ∧ {a: τ₂} -> {a : t₂}`. This allows them to address records. Otherwise, one could just build the conjunction and and give priority to later elements. It is not clear though, this affects the type inference algorithm.


== Gradual Type Systems
We are finally arriving at an approximative typesystem that tries to infer types at a best-effort basis. Maybe we can even help the situation with a bit of _bidirectional type systems_? We anyways have to add an `Any` type that allows the system to be inexplicit from time to time. What does that mean for the subtyping hirarchy? If I have an any type it should be applicable to any function, because we can not make assumptions about it. It should thus be the subtype of every possible type. Should it be a supertype aswell?


== Flow Typing
Besides the afore-mentioned record-field-check, nix provides function that can dynamically check the type of expressions. It is thus possible to write expressions like `if isStr t then {} else {}` where we can type t as $t ∧ str$ in the positive case and $t ∧ ¬str$ in the negative else-branch but for this we _need_ negative types.

There at least are no patterns, but one could also argue, that patterns are just a generalization of conditionals.

The last constructs help in narrowing down value types. They actually just check the type of values `isAttrs, isBool, isFloat, isFunction, isInt, isList, isNull, isPath, isString`.

`isAttr(t): And -> bool`, but this should also result in `t ∧ bool` in positive-branch and `t ∧ ¬bool` in the negative.

This can actually implement _type cases_!?


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


== To: Castagna vs. Parreaux vs. Dolan

#figure(caption: "Comparison of typesystems", table(
  columns: (1fr, 2fr, auto, auto),
  inset: 10pt,
  align: (left, left, center, center),
  table.header(
    [*Author*], [*TypeConnectives*], [*Principality*], [*Extensible*],
    [*Effective*],
  ),
  "Castagna", [¬∨∧], [x], [ ],
  [?], [x], "Parreaux", [¬∨∧],
  [x], [x], [-], [x],
  "Dolan", [∨∧], [x], [x],
  [x], [x],
))


- Dolan: Lattice of types, Extensional,
- Castagna: Sets, Denotational, Universes, Occurrence, Functino-annotations,
- Parreaux: Boolean Algebraic, Syntactic, Verbose, Levels, Bounds, Skolems, Rigid Vars,

It is funny, how the approaches of Parreaux and Castagna are generally similar. They both want to have fully fledged boolean types and they use some strategy to grind down constraints. They also both use some strategy to bring them into a normal form and they use some non-standart¿ subtyping rules to complete the lattice. One difference are upper and lower bounds, that are only used in the Work of Parreaux. On the other hand, I think, Castagna uses constraint types? What are coercion type systems again?

The Workhorse in Parreaux: Carefully crafted inference rules, Normal-Forms
The Workhorse in Castagna: Boolean Formulas, Reduced to Normal-Forms


== Dolan
Stephen Dolan proposed a new family of type systems, named _algebraic type systems_. These systems tackle language construction from a new point of view. Instead of adding types first and then trying to find a semantic model for them, Dolan argues one should pay more attention to finding a semantic model for the types _first_. The types in _algebraic type systems_ form a distributive lattice (thus algebraic) and inherit the lattice' properties. By further restricting the the occurences for union and intersections to positive and negative positions, a distributive lattice can be constructed that allows for lossless reduction of subtyping constraints. In essence, the system is standart ML, with a lattice of types and unification replaced by bi-unification, a subroutine that handles subtyping constraints instead of equality constraints. The final algorithms for subsumption checking and type inference are short as well as simple, all thanks to the initial focus on well-formed types. The final algorithms inherit the standart ML properties, namely _principled type inference_, no need for type annotations and effectiveness i.e no backtracking.


#bib
