// DRAFT — replacement for `= A Note about Nix` in thesis.typ (lines 36–137,
// from `= A Note about Nix` up to but excluding `= Motivation <sec-motivation>`).
// Fragment: not compilable on its own (refs @sec-motivation, @sec-extensions,
// @trilemma and the bibliography live in thesis.typ).

= A Note about Nix
> This section motivates our work in regard to practical application, also the nix language features are guiding the features we are exposing.

NixLang @nix-language-2-28 @dolstra_phd is the fundamental language of one of the largest bodies of untyped functional code in existence and a language that extends beyond the usual λ-calculus features. Its foundational data structure is the _attribute set_ — a record — and the language provides a gamut of constructs and builtin functions to create, extend, deconstruct and reflect upon them. NixLang powers nixpkgs, a package repository of more than 100,000 packages that is continuously evaluated, updated and rolled out from one central repository, and the same repository carries the Nix standard library, the NixOS module system and the definition of the NixOS distribution itself @nixos_short @nixos_long. Every one of those artefacts is an attribute set assembled out of other attribute sets. NixLang is thus both the motivation for our work and the guiding principle behind the features our calculus exposes.

This section reads the language as a specification. Every feature we expose is demanded by an expression one can find in nixpkgs, and we give that expression rather than a description of it.

== The operation that drives everything

Nix' update operator `a // b` is a _set-or-replace_ operation: the result carries every field of `a`, every field of `b`, and for colliding labels the binding from `b`. It is _total_ — no disjointness requirement, no partiality, no failure case — and it is the backbone of the idioms that structure nixpkgs.

#figure(
  caption: [The three idioms that make asymmetric concatenation unavoidable.],
  box(width: 100%, align(left, ```nix
  # 1. an overlay: extend or replace fields of a package set given to us
  self: super: { hello = super.hello // { meta = { broken = false; }; }; }

  # 2. the callPackage idiom: override parts of an argument set we did not build
  mkDerivation (args // { buildInputs = args.buildInputs ++ [ extra ]; })

  # 3. the module system: merge configuration fragments from many files
  { config, lib, ... }: { services.nginx = lib.mkMerge [ base config.extra ]; }
  ```)),
)<nix-idioms>

All three collide _on purpose_. An overlay exists in order to replace a `meta` the package already carries; `callPackage` exists in order to override arguments the caller did not construct; the module system exists in order to merge fragments that write to the same option. A calculus that demands disjoint operands does not type a smaller fragment of nixpkgs — it types none of these three, and therefore none of nixpkgs.

Now abstract over the two sides, which is what writing a function does anyway:

```nix
extend = base: patch: (base // patch).meta
```

Nothing in this definition decides whether the result is `base.meta` or `patch.meta`. It depends on whether the record that is eventually passed as `patch` happens to bind `meta`, and both callers exist in the same file:

```nix
extend hello { meta = { broken = false; }; }   # the patch wins
extend hello { pname = "hello-static"; }       # the base wins
```

This is the example the whole thesis is organised around; in the syntax of our calculus it is $a: b: (a ‖ b).l$. It is not a corner case constructed to break a type system — it is idiom 1 of @nix-idioms with its arguments abstracted, which is the shape every overlay helper in the Nix standard library has. @sec-motivation shows why no ordinary row type can answer it, and why the answer must be a _type_ rather than a constraint.

== What makes Nix hard

Static typing of NixLang is hard for more reasons than the one above, and the reasons are what fix our feature set rather than the other way round. @nix-features summarises them; the remainder of this section walks the table one row at a time, each with the expression that forces the entry.

#figure(
  caption: [Language features of NixLang and the demands they place on a type system.],
  table(
    columns: (auto, 1fr, 1fr),
    align: left,
    inset: 6pt,
    stroke: 0.4pt + luma(200),
    table.header([*Feature*], [*Why it is hard*], [*What it forces*]),

    [`a // b`],
    [precedence is a runtime fact],
    [scoped rows, total concatenation],

    [`e.${e'}`, `?`, `getAttr`],
    [labels are ordinary values],
    [a label sort, first-class labels],

    [`with e; body`],
    [the _scope_ is a runtime value],
    [variable lookup itself may be unknown],

    [`rec`, fixpoints, overlays],
    [rows are recursive],
    [an occurs class that is not a technicality],

    [laziness],
    [non-closedness, errors that never fire],
    [soft typing, per-binding uncertainty],

    [`attrNames`, `removeAttrs`, `intersectAttrs`],
    [need negative and label-level information],
    [best-effort signatures, ★ as a sink],

    [no annotations anywhere],
    [everything must be inferred],
    [HM-style inference at whole-fixpoint scale],
  ),
)<nix-features>

_Precedence is a runtime fact._ Which side of a `//` wins is not visible in the expression:

```nix
let
  base  = { pname = "hello"; meta = { broken = true; }; };
  patch = if stdenv.hostPlatform.isDarwin then { meta.broken = false; } else { };
in (base // patch).meta.broken
```

On one platform `patch` binds `meta` and shadows the base; on the other it is empty and the base survives. A record type that flattens `base // patch` into one set of fields has to pick the winner at the moment it is built, which is exactly the moment at which the winner is unknown. Our rows therefore stay _scoped_: the concatenation keeps both contributions and the precedence between them, written $(β | α)$ with the higher-precedence row first, and duplicate labels are legal inside a row rather than an error. Concatenation on such rows is total, like the operation it types.

_Labels are ordinary values._ Field names are computed, passed around as strings and tested for:

```nix
pkgs.${"python" + toString version}                  # the label is computed
builtins.getAttr name pkgs                           # the label is an argument
if args ? buildInputs then args.buildInputs else [ ] # the label is a question
builtins.listToAttrs [ { name = k; value = v; } ]    # the whole record is computed
```

A type system for NixLang can therefore not treat a label as a piece of syntax attached to a selection rule. It needs labels as a _sort_ with variables of its own, so that $ρ.l$ remains meaningful when neither $ρ$ nor $l$ is known. The minimal calculus of @minimal-calculus keeps labels concrete and adds the sort only as an extension (@sec-extensions), but the design is arranged around the harder case from the start: field demands are answered by a _judgement_ with a "don't know" verdict, not by a partial function on row syntax, and that is precisely what a variable label needs.

_The scope itself is a runtime value._ `with e; body` injects the fields of `e` into the scope of `body`, so whether a name is bound at all can depend on a value:

```nix
attrs: with attrs; hello        # is `hello` a variable, or an error?
```

If the record passed for `attrs` binds `hello`, the expression is a field selection; if it does not, evaluation fails with an unbound variable. Lexical bindings still win — `let x = 1; in with { x = 2; }; x` evaluates to `1` — so `with` does not even reliably capture the names it mentions. Variable lookup, usually the one judgement a type system can take for granted, thus becomes the same three-valued question as field lookup, and it is asked against a row that may be a variable. This is also where our closedness assumption in the metatheory gives out, and we say so in @sec-extensions rather than pretend otherwise.

_Rows are recursive._ The central construction of nixpkgs is a fixpoint over a record:

```nix
lib.fix (self: {
  a = { x = 1; };
  b = self.a // { y = 2; };     # the row of `self` occurs in its own definition
})
```

Overlays are the same shape with an extra argument, and `rec { … }` is the same shape with sugar. Typing it produces an equation in which a row variable occurs inside its own solution, $α ≐ (ρ | α)$-shaped, so the occurs check is not a defensive measure against pathological input: it fires on the idiom the repository is built out of. What the algorithm does when it fires — reject, or recognise that the equation has no unifier for a reason it can report — is a genuine design question for this calculus, and we treat it as one in @metatheory.

_Laziness means errors that never fire._ A record may contain bindings that cannot be evaluated, and the program is fine as long as nobody forces them:

```nix
{ keep = 1; boom = builtins.throw "nope"; }.keep   # evaluates to 1
```

nixpkgs relies on this at scale: package sets contain entries that fail to evaluate on the current platform, and `nix build pkgs.hello` must still work. A type system that rejects a record because one of its fields is ill-typed rejects the repository. Uncertainty therefore has to be recorded _per binding_ and has to be a result rather than an abort — which is what makes the system a soft one, and which is why the metatheory proves progress _up to_ an error verdict instead of ruling errors out.

_Some builtins ask what a record does not have._ The reflective builtins do not fit positive row information at all:

```nix
builtins.removeAttrs args [ "buildInputs" ]                  # what does the result lack?
builtins.intersectAttrs (builtins.functionArgs f) autoArgs   # the heart of callPackage
builtins.attrNames pkgs                                      # labels as data
```

`removeAttrs` returns a record characterised by an absence, `intersectAttrs` by a label-level intersection, and `attrNames` turns the label set into a list of strings. None of this is expressible with row variables that only ever say what a row _contains_ — the same asymmetry the motivating example exhibits, met here on the side of the standard library. We give such builtins best-effort signatures whose imprecision is recorded in the type, with ★ as the sink, and we return in @sec-extensions to lacks-predicates as the cheapest principled source of the missing negative information.

_There is nowhere to write a type._ NixLang has no annotation syntax, no signature files and no module interfaces, and nixpkgs is, semantically, one fixpoint: `all-packages.nix` binds the whole repository as a single recursive attribute set. There is neither a place to put an annotation nor a boundary at which to check one. Everything must be inferred, at the scale of that fixpoint, which rules out any approach whose cost is more than unification per constraint.

== What a type system for Nix may not do

Two entries of @nix-features deserve emphasis because they are usually left out of accounts of "typing a dynamic language". The first is that NixLang cannot be _instrumented_. The standard escape hatch of a dynamic language — insert a check where the static knowledge runs out — is unavailable here, for two independent reasons.

A check forces a thunk, and forcing a thunk is observable:

```nix
let attrs = { keep = 1; boom = builtins.throw "nope"; };
in (assertRecordOf attrs).keep   # a cast that inspects `boom` turns 1 into a crash
```

And a check changes what is built. Derivation inputs are hashed, so wrapping a value that flows into a `derivation` changes its store path, and a rewritten `stdenv` rebuilds every package in the repository. Gradual typing @gradual_siek @gradual_tobin @agt, blame @cantblamethis @blame_for_all and every monitoring-based approach are therefore not rejected here on grounds of taste — they are semantically unavailable. We make the requirements explicit:

/ R1 (No source changes): The analysis must run on today's nixpkgs verbatim. Annotations may be admitted, but nothing may be _required_, and no program may need rewriting to become analysable.
/ R2 (No semantic change): No casts, wrappers or runtime checks. Evaluation must produce exactly what it produced before, down to derivation hashes. This is what rules out gradual typing and blame and leaves _soft typing_ @soft_typing @practical_soft_typing @coldwar — static warnings over an untouched dynamic semantics — as the only admissible shape.
/ R3 (Totality): Every program must receive a type. Failure of the analysis is a _result_, not an error: where nothing can be established the system must say so, in the type, rather than refuse the program. This is what forces an unknown type ★, in the spirit of the `any` of TypeScript @typescript and Flow @flow but — as @sec-motivation argues — with a controlled origin.
/ R4 (Effective inference): The analysis must scale to a repository that is, semantically, a single fixpoint. This rules out inference approaches whose constraint language has no solving procedure — the ROSE line @rose @extensible_rec_funcs @generic_with_extensible among them — and it rules out backtracking over alternative typings.

R3 has teeth, because some Nix expressions have no useful static answer at all. Using first-class labels and the impure builtin `builtins.currentTime`, one can select a field by wall-clock time:

```nix
{ before = "moin"; after = 0; }.${if builtins.currentTime < 1767236401 then "before" else "after"}
```

The selected field, and hence the type of the expression, changes at a fixed instant. A set-theoretic system would answer with the union of the two branches; ours has no unions, and answers ★. Neither answer is a failure of the analysis — the point is that _some_ answer is mandatory, and that the interesting design question is not whether uncertainty arises but where it is allowed to enter and whether its origin can be explained.

Note that this expression and the motivating example fail for different reasons, and the difference is what the thesis is about. The wall-clock selection is genuinely undetermined: no static information could settle it, and any system must give up. In $a: b: (a ‖ b).l$ nothing is undetermined — the answer is a perfectly definite function of what `b` binds — and the analysis gives up only because a row variable carries no information about what it _lacks_. The first kind of ★ is unavoidable, the second is a price, and @sec-motivation is the account of what that price buys.

Work on typing Nix itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics but attempt no type system; an earlier system by the author @simplenix applies off-the-shelf Hindley-Milner inference to a Nix subset and fails precisely on the record operations of @nix-idioms; Nickel @nickel, a Nix-inspired configuration language, adopts gradual typing with row polymorphism but forbids the colliding concatenations that make `//` interesting; and the long-standing community issue @nix-ts-issue documents both the demand for and the difficulty of the problem.
