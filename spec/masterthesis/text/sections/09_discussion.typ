#import "../functions.typ": *

= Discussion <sec-discussion>

== The Idioms of the Introduction <sec-idioms>

We return to the three idioms of @nix-idioms. MiniNix has no lists, booleans or library, so each idiom is encoded with the missing parts abstracted as parameters: `false` becomes a constant c, `mkDerivation` and `++ [extra]` become parameters mk and app, and `lib.optionalAttrs stdenv.isDarwin` becomes an application of a parameter opt to a parameter cond. The types below are the results of the inference algorithm of @inference, computed by the Lean implementation, with variables renamed.

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
  The argument handed to mk carries the new buildInputs in front of the row of args, so it shadows the old one. The selection `args.buildInputs` cannot be answered, since the row of args is open, and it is unspent, since app accepts any argument. Finalization therefore pins it to ★, and the domain of app becomes ★. Under a `let` the stump would instead travel into the scheme and be answered at each use, as in @principality.
] <ex-idiom-override>

#example(name: [Conditional extension])[
  $
    #raw("opt: cond: { pname = c; } // opt cond { NIX_LDFLAGS = c; }") \
    quad : quad (α → {"NIX_LDFLAGS": 𝓫} → {ρ}) → α → {ρ | "pname": 𝓫}
  $
  The result keeps pname behind the row ρ returned by opt, since that row may carry a pname of its own. A later lookup of pname answers $?$ until opt is known, which is exactly the Wand ambiguity of @sec-motivation.
] <ex-idiom-cond>

// PROF: Here T-sel-⊥'s laziness motivation fails in composition; a bottom type would type it.
Applied to concrete package sets, the overlay shows both sides of G3. Applied to `{}` and `{ hello = { pname = c; }; }`, it types at ${"hello": {"meta": {"broken": 𝓫} | "pname": 𝓫}}$. Applied to `{}` and `{}`, inference reports a clash: the lookup of hello misses, so the promised result is pinned to ★, but the concatenation has already used it as a record. The declarative system rejects the program as well, since ★ cannot be concatenated (@sec-goals). The program nevertheless evaluates without error, because the field hello is never selected.

== Design Goals Revisited <sec-goals>

@sec-motivation stated four design goals for a type system for NixLang. MiniNix meets G2 by construction and the other three in part. This section records how far it reaches each of them.

_G1 (No source changes)._ Inference requires no annotations. Every construct of MiniNix is typed without input from the programmer, and uncertainty is recorded as ★ rather than demanded as a lacks-predicate or a disjointness witness. The goal is thus met for MiniNix, but not yet for NixLang: patterns, `with`, `inherit`, recursive records and the builtins lie outside MiniNix (@sec-extensions), and nixpkgs can be analysed verbatim only once they are covered.

_G2 (No semantic change)._ The analysis is purely static. It inserts no casts and does not instrument the program, so evaluation is unchanged, and the goal is met by construction. The price is carried by the metatheory: progress holds only up to lookup errors ↯ (@type-safety), which the analysis cannot prevent. [](note our further goals?)

// PROF: "A stuck verdict is not a rejection" depends on behaviour the rules do not define.
_G3 (Totality)._ This goal is met in part. Uncertainty that originates in a lookup is absorbed: a selection that cannot be answered is typed at ★, and inference defers it rather than failing. A program that runs is nevertheless rejected in two situations, and in a third with first-class labels. First, ★ is rigid and has no elimination form, so a program that applies ★, selects from it or concatenates it has no typing, even in the declarative system. The motivating example, extended by one selection,

```nix
{ "1759190400" = { m = 1; }; }.${toString builtins.currentTime}.m
```

receives no type, since the dynamic selection under a string key yields ★ by T-sel-dyn-𝓫 (@fc-typing) and ★ cannot be selected from. Second, inference reports a clash on some programs that the declarative system types (@incompleteness-inference), since a clash certifies the absence of a unifier, not the absence of a typing. Third, a key that is used as a label before it is known to be a string is a type error (@sec-fc-labels). A stuck verdict is not a rejection: the analysis reports it, as G3 demands.

// PROF: Row unification is modulo A + partial-C, not syntactic; HM is worst-case exponential.
_G4 (Efficient inference)._ The algorithm is designed for this goal but not yet measured against it. Inference is syntactic first-order unification: there is no constraint solver, no unification modulo associativity and commutativity, and no backtracking over alternative typings. Every move of the row pass is forced, and a parked stump is re-examined only when its blocker is solved. These are structural reasons to expect the analysis to scale. Termination is proved, but no complexity bound, and the analysis has not yet been run on nixpkgs.

== Outlook: Towards Gradual Typing <sec-gradual>

// PROF: Em dashes; use your hyphen/comma style.
The remaining gap in G3 has a common cause. ★ is introduced by the lookup relation and by T-★-intro, but it is never consumed, so every use of an unknown value demands a type that ★ cannot supply. Gradual typing @gradual_siek resolves the same situation by replacing type equality with _consistency_, under which the dynamic type is compatible with every type. Reading ★ as the dynamic type would give it elimination forms — a ★ could be applied, selected from and concatenated — and would allow inference to join incompatible uses at ★ instead of reporting a clash, which is the repair @incompleteness-inference names for unrestricted T-★-intro. Sekiyama and Igarashi @gradual_extensible_rows show that consistency extends to extensible rows.

Two obstacles distinguish this setting from the usual one. First, gradual type systems insert casts at the boundary between static and dynamic code, and they rely on these casts for their safety theorem and for the gradual guarantee @gradual_criteria. G2 forbids instrumentation, so a gradual extension of MiniNix would be gradual in its static discipline only. Its safety theorem would acquire a further disjunct for failures at the eliminations of ★, in the same way that ↯ records lookup errors today. Second, the rigidity of ★ is what the principality result of @principality rests on. A ★ that is consistent with every type can be sharpened, and whether qualified schemes remain principal under consistency is open.
