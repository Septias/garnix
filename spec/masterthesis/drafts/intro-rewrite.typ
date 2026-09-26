#import "../text/functions.typ": *
#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

= Introduction <sec-motivation>

The Nix programming language @nix-language-2-28 @dolstra_phd, which we call NixLang, [underlies one of the largest bodies of untyped functional code in existence]¿. Its foundational data structure is the _attribute set_ — a record — and the language provides a wide range of constructs and builtin functions to create, extend, deconstruct and reflect upon them. NixLang powers nixpkgs, a repository of more than 100,000 packages¿ that is continuously evaluated, updated and rolled out from one central place, and the same repository carries the Nix standard library, the NixOS module system and the definition of the NixOS distribution itself @nixos_short @nixos_long. Every one of those artefacts is an attribute set assembled out of other attribute sets.

This thesis asks how much of that code a static type system can understand _as it is_. We do not design a new language that compiles to Nix, as Nickel @nickel does with its own gradually typed language; we want an analysis that runs on existing code, changes nothing about it, and says clearly where it gives up. The single construct that decides whether this is possible is Nix's record update operator `//`.

== Asymmetric concatenation is everywhere

`a // b` is a _set-or-replace_ operation: the result carries every field of `a` and every field of `b`, and where both bind a label, `b` wins. It is _total_ — there is no disjointness requirement and no failure case — and it is the backbone of the idioms that structure nixpkgs (@nix-idioms).

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

All three collide _on purpose_. An overlay exists in order to replace a `meta` the package already carries; `callPackage` exists in order to override arguments the caller did not construct; the module system exists in order to merge fragments that write to the same option. A calculus that demands disjoint operands does not type a smaller fragment of nixpkgs — it types none of these idioms.

== The wand problem

Now abstract over both sides, which is what writing a function does anyway:

```nix
extend = base: patch: (base // patch).meta
```

Nothing in this definition decides whether the result is `base.meta` or `patch.meta`. It depends on whether the record eventually passed as `patch` binds `meta`, and both kinds of callers are perfectly ordinary:

```nix
extend hello { meta = { broken = false; }; }   # the patch wins
extend hello { pname = "hello-static"; }       # the base wins
```

In the syntax of our calculus, where `‖` is Nix's `//`, this is the term $a: b: (a ‖ b).l$, and it is the example this thesis is organised around. Following Wand, who first met it in the typing of multiple inheritance @concat4multiinher, we call the problem it poses the _wand-ambiguity_. With $a : {α}$ and $b : {β}$ for row-variables α and β, the selection has to resolve $l$ in the row $(β | α)$: if β contains $l$, the answer is β's binding; if β definitely does not, the answer is α's. Neither is known when the function is defined, and no amount of positive information about β settles it — what is missing is _negative_ information, and no expression in the program provides it.

== Why existing record calculi do not fit

The literature offers several ways around the wand-ambiguity; each gives up something Nix cannot give up. @related-work discusses them in detail, and @trilemma summarises the positions.

_Symmetric concatenation_ @symm_concat is only defined on records with disjoint fields, which excludes exactly the colliding uses of @nix-idioms. _Width subtyping_ does not combine with asymmetric concatenation at all: if `{ l = 1; }` may be used where `{ }` is expected, then `r: ({ l = true; } // r).l` can be typed as returning a boolean and still return `1`, because the field that decides precedence was forgotten by the type but not by the value. We therefore keep the calculus free of subtyping; the only orderings it has are row equivalence (≈) and the precision gained by instantiation.

_Qualified row predicates_ — lacks-constraints @gaster_jones, or the containment and combination predicates of Rose @rose — can express the missing negative information. Rose types asymmetric concatenation faithfully and has principal types, but they are principal _constrained_ schemes: the row predicates are deferred to an entailment relation that the framework leaves as a parameter, and deciding the predicates inference actually generates amounts to unification modulo associativity and commutativity, which is NP-complete @ac_unification. HM(X)-based record systems @designing_record_systems similarly assume a constraint solver rather than supplying one. Plain lacks-constraints are cheaper, but in the minimal calculus nothing would generate them: they arise in Gaster and Jones's system because record _extension_ is partial and must demand absence, whereas scoped rows make concatenation total, so no typing rule ever demands anything.

_Set-theoretic types_ @castagna2023programming @typing_records_etc express absence directly through negation, at the price of local rather than let-polymorphic inference. Finally, _gradual type inference_ (Siek and Vachharajani¿; Garcia and Cimini¿) combines a dynamic type with unification-based inference, but its guarantees rest on runtime casts, and Nix code cannot be instrumented.

#figure(
  caption: [What each approach gives up to handle concatenation.],
  table(
    columns: (auto, 1fr),
    align: left,
    inset: 6pt,
    stroke: 0.4pt + luma(200),
    table.header([*Approach*], [*What is given up*]),
    [Symmetric concatenation @symm_concat],
    [colliding fields — the idioms of @nix-idioms],

    [Width subtyping @mlstruct @algebraic_subtyping],
    [precedence: forgotten fields still overwrite],

    [Qualified row predicates @rose @designing_record_systems],
    [a decision procedure for the generated predicates],

    [Set-theoretic types @castagna2023programming], [let-polymorphic inference],
    [Gradual type inference¿],
    [uninstrumented execution: guarantees need casts],

    [This work], [precision — ★ wherever a lookup cannot be decided],
  ),
)<trilemma>

== Our approach: two judgements instead of one <our-position>

In a row-based system, a field selection is ordinarily _turned into a row constraint_ and handed to unification: `e.l` demands that the row of `e` contain $l$, and when that row ends in a variable, unification has to guess that the variable contains the field. The search rule of Paszke and Xie @extensible_tabular, the system our calculus builds on, does exactly this.

We keep field demands out of unification. Row unification $scripts(≐)_r$ only ever decides _equality_ of two rows; it never receives a demand of the form "this row must contain $l$". Field demands are answered by a separate _lookup relation_ $Γ ⊢ ρ.l ↓ r$ whose result is three-valued: a definite type $τ$, definite absence $⊥$, or _don't know_ $?$ when the search reaches an unsolved row-variable. A demand that cannot be answered yet is _parked_ in the type scheme and re-asked once instantiation has solved the variable that blocked it.

This is what lets types become more precise on application. The term `x: ({ l = c } ‖ x).l` first types at ${β} → ★$, because β might shadow $l$; applying it to `{}` solves $β := ε$, the lookup advances past β, and the result becomes the type of `c`. Only a lookup that is still unanswered when inference ends is turned into the unknown type ★.

Keeping selection out of unification changes what the algorithm has to do. Qualified-types systems also keep field demands apart from equality, as predicates; what differs here is how the demands are resolved — by running a deterministic lookup, and by recording the cases it cannot decide as a _type_, ★, rather than as a residual constraint or a disjunction of typings. Inference therefore needs first-order unification modulo row equivalence, without backtracking and without a separate constraint solver. Two consequences follow for the unification algorithm. Rows modulo ≈ form a _trace monoid_, so both ends of a row can be cancelled, which replaces the tail-check side condition of Paszke and Xie with an algebraic property. And the only move that invents structure fires when the field it places has a unique possible host, which makes it forced rather than a guess.

== Soft typing and the unknown type ★

A system that admits ★ gives up completeness on purpose, and in Nix that price cannot be avoided. Even the one-liner

```nix
(if builtins.currentTime < 1900000000 then { l = 1; } else { }).l
```

succeeds or fails depending on when it is run, and no static analysis can decide which. Our calculus is therefore a _soft_ type system in the sense of Cartwright and Fagan @soft_typing @practical_soft_typing: every program keeps its untyped semantics, and the type system reports where it cannot vouch for it.

★ looks like the dynamic type of gradual typing @gradual_siek, but it behaves differently in three ways. It is _rigid_: unification matches ★ only against itself, so it is never silently absorbed into another type, and once a position is ★ no substitution refines it again. It is _not a top type_: nothing is a subtype of it and there is no consistency relation. And it _needs no casts_: ★ marks where the analysis gave up, not where a runtime check will be inserted. The cost is stated in the type-safety theorem itself: progress holds up to a lookup error ↯, raised when a selection reaches a record literal that lacks the label, and that is the only way a well-typed closed program can get stuck.

Occurrence typing through Nix's `builtins.is*` predicates would let conditionals recover types from ★; we leave it to @sec-extensions.

== Design goals

The considerations above fix four goals. They describe what the design is built for; the minimal calculus of this thesis is a step towards them, not an evaluation against nixpkgs.

/ R1 (No source changes): The analysis runs on existing code verbatim; no program needs rewriting or annotating to be analysed.
/ R2 (No instrumentation): The analysis only observes. It inserts no casts, contracts or runtime checks, so evaluation is exactly what it was before.
/ R3 (Totality): Every program receives a type. Where the analysis gives up, that is a _result_ (★), not a rejection.
/ R4 (Effective inference): Inference relies on first-order unification rather than on an open constraint-solving problem, so that it can in principle scale to a repository that is, semantically, a single fixpoint.

== Contributions <contributions>

+ *A calculus that separates field demands from row equality.* We define a minimal calculus with scoped records, asymmetric concatenation, row-variables, let-polymorphism and ★, and the three-valued lookup relation at its core. Lookup is deterministic, total under acyclic row solutions, and monotone: solving a row-variable can only refine a typing, and a definite result is never revised.
+ *Qualified schemes are forced.* We prove that no plain scheme $∀macron(α). τ$ is principal for `x: x.l`: its two families of typings cannot be covered without also admitting instances that are not typings. A scheme that carries its pending lookup is principal for this term, and the resulting qualified system is strictly more expressive than the plain one.
+ *Unification on spines, with an exact account of its incompleteness.* We give a unification algorithm that uses trace-monoid cancellation at both ends of a row. A successful run returns a most general unifier and a clash means no unifier exists; neither verdict depends on the recursion budget. The algorithm is incomplete, and we show exactly where: each of its three conservative verdicts comes with a checked example that has a most general unifier nonetheless, and we show that the natural converse — "no move applies, so no most general unifier exists" — is false.
+ *A mechanization in Lean 4.* All of the above, together with progress and preservation for closed programs in both the plain and the qualified system, is mechanized in about 22,000 lines of Lean without `sorry`, with an audit of the axioms used. The mechanization is also a source of results: it surfaced two unification rules — a counting rule and the unique-host expansion — that the paper design lacked, and it refuted several plausible conjectures that we record with their counterexamples.

We are equally explicit about what remains open. Termination of unification and of inference is not proved, so inference is defined as a relation rather than a function. For inference soundness, the case lemmas for twelve of the fifteen inference rules are proved but the theorem as a whole is not, and principality is established for the selection `x: x.l` rather than for all terms. Nix constructs beyond the minimal calculus — first-class labels, patterns, `with` and `inherit` — are discussed but not formalized.

The remainder of this thesis is organised as follows. The declarative system — syntax (@syntax), typing rules (@declarative), lookup and row equivalence — comes first. @unification develops the unification algorithm on spines, and @metatheory presents the metatheory: type safety, refinement, the principality argument that forces qualified schemes, and the incompleteness results. @sec-extensions discusses the way towards full NixLang, and @related-work places the work in the literature.

// ====================== END INTRODUCTION ======================

// Standalone stubs: targets for the cross-references above, which live in
// text/thesis.typ. Not part of the introduction.
#figure(caption: [Stub for the syntax figure.], [])<syntax>
#figure(caption: [Stub for the declarative typing rules.], [])<declarative>
= Unification (stub) <unification>
= Extensions (stub) <sec-extensions>
= Metatheory (stub) <metatheory>
= Related Work (stub) <related-work>

#bib
