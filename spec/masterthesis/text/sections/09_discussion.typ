#import "../functions.typ": *

= Discussion <sec-discussion>

== The Idioms of the Introduction <sec-idioms>

We return to the three idioms of @nix-idioms. The types below are the results of the inference algorithm of @inference, computed by the Lean implementation, with variables renamed. The programs are adapted slightly: free names become parameters (mk, opt, cond), literals are written c, the overlay updates hello instead of copying foo into fields, and the override applies a parameter app where @nix-idioms appends `++ [ extra ]`.


#example(name: [Overlay])[
  $
    #raw("self: super: { hello = super.hello // { meta = { broken = c; }; }; }") \
    quad : quad α → {"hello": {ρ₁} | ρ₂} → {"hello": {"meta": {"broken": 𝓫} | ρ₁}}
  $
  The selection `super.hello` parks a stump on the row of super, and the concatenation spends its promise on a record ${ρ₁}$. Finalization materializes the field $"hello": {ρ₁}$ in the row of super. The result is faithful to the semantics of `//`: the new meta shadows any meta of `super.hello`, and every other field of `super.hello` survives in ρ₁.
] <ex-idiom-overlay>

// PROF: Every nixpkgs file is a top-level function like this; say so in the intro.
#example(name: [Override])[
  $
    #raw("mk: app: args: mk (args // { buildInputs = app args.buildInputs; })") \
    quad : quad ({"buildInputs": β | ρ} → γ) → (★ → β) → {ρ} → γ
  $
  The argument handed to mk carries the new buildInputs in front of the row of args, so it shadows the old one. The selection `args.buildInputs` cannot be answered, since the row of args is open, and it is unspent, since app accepts any argument. Finalization therefore pins it to ★, and the domain of app becomes ★. The type ★ → β leaves app little freedom: since ★ has no elimination form, app can only ignore its argument or pass it on, as `y: y` does at β = ★. Passing `xs: xs ++ [ extra ]` as app clashes, since ★ cannot be appended. Under a `let` the stump would instead travel into the scheme and be answered at each use, as in @principality.
] <ex-idiom-override>

#example(name: [Conditional extension])[
  $
    #raw("opt: cond: { pname = c; } // opt cond { NIX_LDFLAGS = c; }") \
    quad : quad (α → {"NIX_LDFLAGS": 𝓫} → {ρ}) → α → {ρ | "pname": 𝓫}
  $
  The result keeps pname behind the row ρ returned by opt, since that row may carry a pname of its own. A later lookup of pname answers $?$ until opt is known, which is exactly the Wand ambiguity of @sec-motivation. In nixpkgs, opt is `lib.optionalAttrs`, whose body is a conditional, and MacroNix (@app-macronix) rejects the conditional form `{ pname = c; } // (if cond then { NIX_LDFLAGS = c; } else {})`. Inference unifies the branches ${"NIX_LDFLAGS": 𝓫}$ and ${}$ and clashes. The declarative system rejects it as well: the only common type of the branches is ★, and ★ cannot be concatenated.
] <ex-idiom-cond>

// PROF: Here T-sel-⊥'s laziness motivation fails in composition; a bottom type would type it.
Applied to concrete package sets, the overlay shows both sides of G3. Applied to `{}` and `{ hello = { pname = c; }; }`, it types at ${"hello": {"meta": {"broken": 𝓫} | "pname": 𝓫}}$. Applied to `{}` and `{}`, inference reports a clash: the lookup of hello misses, so the promised result is pinned to ★, but the concatenation has already used it as a record. The declarative system rejects the program as well, since ★ cannot be concatenated (@sec-goals). The program nevertheless evaluates without error, because the field hello is never selected.

== Design Goals Revisited <sec-goals>

@sec-motivation stated four design goals for a type system for NixLang. MiniNix meets G2 by construction and the other three in part. This section records how far it reaches each of them.

_G1 (No source changes)._ Inference requires no annotations. Every construct of MiniNix is typed without input from the programmer, and uncertainty is recorded as ★ rather than demanded as a lacks-predicate or a disjointness witness. The goal is thus met for MiniNix, but not yet for NixLang. MacroNix (@app-macronix) adds booleans, conditionals, lists and a set of builtins, and patterns, `inherit` and `or` desugar into it (@fig-nix-parity). Of 21 programs from the Nix test suite, translated by hand, 17 type in agreement with their evaluation. The four exceptions are a conditional whose branches have incompatible types, a list whose elements have different types, `or` applied to a non-record, and a function with pattern defaults that is used at two record shapes but not generalized by A-let (@incompleteness-inference). `with`, recursive records, recursive `let` and the builtins that enumerate or remove labels remain uncovered, and nixpkgs can be analysed verbatim only once they are covered.

_G2 (No semantic change)._ The analysis is purely static. It inserts no casts and does not instrument the program, so evaluation is unchanged, and the goal is met by construction. The price is carried by the metatheory: progress holds only up to lookup errors ↯ (@type-safety), which the analysis cannot prevent. In MacroNix, an error is also a failed assertion or a failing builtin, such as `head []` or a division by zero.


_G3 (Totality)._ This goal is met in part. Uncertainty that originates in a lookup is absorbed: a selection that cannot be answered is typed at ★, and inference defers it rather than failing. Nothing is guaranteed about the value of such a selection, but since ★ has no elimination form, no typed code consumes it. A program that runs is nevertheless rejected in three situations, and in a fourth with first-class labels. First, ★ has no elimination form and no typing rule converts it to another type, so a program that applies ★, selects from it or concatenates it has no typing, even in the declarative system. The motivating example, extended by one selection,

```nix
{ "1759190400" = { m = 1; }; }.${toString builtins.currentTime}.m
```

receives no type, since the dynamic selection under a string key yields ★ by T-sel-dyn-𝓫 (@fc-typing) and ★ cannot be selected from. Second, inference reports a clash on some programs that the declarative system types (@incompleteness-inference), since a clash certifies the absence of a unifier, not the absence of a typing. Third, inference rejects every stuck row equation. Uncertainty that originates in an equation is not absorbed, since the equation lies between an argument and the domain of the function that consumes it (@sec-gradual). Fourth, a key that is used as a label before it is known to be a string is a type error (@sec-fc-labels). The conditional of MacroNix adds instances of the first two situations, since it unifies its branches. For a record as with an integer field z and no field a, `if as ? a then as.a else as.z` clashes: the definite miss `as.a` has type ★ by T-sel-⊥, and ★ does not unify with `int`. The declarative system types it at ★ by T-★-intro on the else branch. The conditional extension of @ex-idiom-cond has no typing at all.

_G4 (Efficient inference)._ The algorithm is designed for this goal but not yet measured against it. Inference stays first-order and never backtracks: there is no constraint solver and no search over alternative typings. Row unification works modulo ≈, but every move of the row pass is forced, and a parked stump is re-examined only when its blocker is solved. These are structural reasons to expect the analysis to scale. Termination is proved, but no complexity bound. Since the algorithm extends Hindley-Milner inference, it inherits its exponential worst case, and the analysis has not yet been run on nixpkgs.

// TODO(structure): admissions (daily 28.10): ★ has no eliminators; ≈ keeps shadowed fields, unlike Nix's set-like records, and the faithful relation is not cancellative. Here, after Incompleteness, or in the Conclusion.
== Outlook: Towards Gradual Typing <sec-gradual>

The remaining gap in G3 has a common cause. ★ is introduced by the lookup relation and by T-★-intro, but it is never consumed, so every use of an unknown value demands a type that ★ cannot supply. Gradual typing @gradual_siek resolves the same situation by replacing type equality with _consistency_, under which the dynamic type is compatible with every type. Reading ★ as the dynamic type would give it elimination forms, so that a ★ could be applied, selected from and concatenated. Sekiyama and Igarashi @gradual_extensible_rows show that consistency extends to extensible rows. Consistency does not join incompatible uses, however: it relates ★ to every type, but two static types such as `int` and `{}` stay inconsistent. The conditional of MacroNix on branches of incompatible types is therefore not completed by consistency alone. It would need inference to choose ★ itself, as T-★-intro does declaratively, and with ★ chosen implicitly the analysis becomes vacuous: `(x: x 4) 3` would type and get stuck. The conditional extension needs more: a dynamic row, since ${"NIX_LDFLAGS": 𝓫}$ and ${}$ must join at a record type that may contain the field, and only a record can be concatenated.

A stuck row equation is a second instance of the same cause. Inference cannot type the application at ★, since the equation lies between an argument and the domain of the function it is passed to, and ★ on the result of the application leaves it unsolved. In

```nix
((g: a: b: { p = g { l = y: y; }; q = g (a // b); }) (x: x.l c) { l = c; } {}).q
```

the first use fixes the domain of g to a record whose field l is a function, and the second use leaves the equation of that record with ${β | α}$ stuck, which is the Wand ambiguity. Typing `q` at ★ and dropping the equation would accept the arguments `{ l = c; }` and `{}`, and evaluation reaches `c c` in the body of g, where `x.l` has a function type and not ★. With elimination forms for ★, the domain of g could be ★ instead, and the failure would occur at an elimination of ★, where a gradual safety theorem accounts for it.

Two obstacles distinguish this setting from the usual one. First, gradual type systems insert casts at the boundary between static and dynamic code, and they rely on these casts for their safety theorem and for the gradual guarantee @gradual_criteria. G2 forbids instrumentation, so a gradual extension of MiniNix would be gradual in its static discipline only. Its safety theorem would acquire a further disjunct for failures at the eliminations of ★, in the same way that ↯ records lookup errors today. Second, the proof of @thm-no-plain uses that ★ is never sharpened (@lem-star-max). A ★ that is consistent with every type can be sharpened, so the result would have to be established anew. There is no general principality theorem to preserve: @principality settles principality only for `x: x.l`.
