#import "../functions.typ": *

= Discussion and Conclusion <sec-discussion>

We now show our type inference algorithm in action on the code snippets from the introduction. We then revisit the design goals stated in @sec-motivation, give an outlook for further work and the path towards full type inference for NixLang.

== The Idioms of the Introduction <sec-idioms>

We return to the three expressions from the introduction @nix-idioms and conduct a case study on their typing. The types below are result of the Lean implementation of the inference algorithm from @inference on MacroNix (@app-macronix), which extends MiniNix by booleans, lists and a limited set of builtins. The programs are only adapted slightly to match the syntax of MacroNix and free names are turned into parameters.


#example(name: [Meta update])[
  $
    #raw("drv: drv ‖ {meta = (drv.meta or {}) ‖ {broken = false}}") \
    quad : quad ⟨(ρ₁ | "meta": {ε})."meta" ↓ {ρ₂}⟩ ⇒ {ρ₁} → {"meta": {"broken": "bool" | ρ₂} | ρ₁}
  $
  MacroNix desugars `drv.meta or {}` to the selection of meta from `{meta = {}} ‖ drv` (@fig-nix-parity). Its lookup stops on the row of drv and parks a stump, and the concatenation with `{broken = false}` spends the stump on a record ${ρ₂}$. The stump stays a qualifier of the answer, but its lookup can no longer miss: at a drv with meta it hits that field, and at a drv without meta it reaches the default and answers ${ε}$. The result is faithful to the semantics of `//`: the new broken shadows any broken of the old meta, every other field of the old meta survives in ρ₂, and every other field of drv survives in ρ₁.
] <ex-idiom-meta>


#example(name: [Override])[
  $
    & #raw("mkDerivation: extra: args:") \
    & quad #raw("mkDerivation (args ‖ {") \
    & quad quad #raw("buildInputs = (args.buildInputs or []) ++ (extra :: [])})") \
    & quad : quad ⟨(ρ | "buildInputs": [γ])."buildInputs" ↓ [α]⟩ \
    & quad quad quad ⇒ ({"buildInputs": [α] | ρ} → β) → α → {ρ} → β
  $
  The argument handed to mkDerivation carries the new buildInputs in front of the row of args, so it shadows the old one. The selection `args.buildInputs or []` looks up buildInputs in the row of `{buildInputs = []} ‖ args`, where γ is the element type of the empty default. The lookup stops on the row of args, and `++` spends the stump at the list type $[α]$, so its result shares α with the type of extra. The default ensures that the lookup never misses.
] <ex-idiom-override>

#example(name: [Conditional extension])[
  $
    & #raw("lib: stdenv: {pname = \"hello\"}") \
    & quad #raw("‖ lib.optionalAttrs stdenv.isDarwin {NIX_LDFLAGS = \"-liconv\"}") \
    & quad : quad ⟨ρ₂."isDarwin" ↓ α⟩, ⟨ρ₁."optionalAttrs" ↓ (α → {"NIX_LDFLAGS": "string"} → {ρ₃})⟩ \
    & quad quad quad ⇒ {ρ₁} → {ρ₂} → {ρ₃ | "pname": "string"}
  $
  Both selections park a stump, and the stump on the row of lib is spent, since its result is applied. The result keeps pname behind the row ρ₃ returned by `lib.optionalAttrs`, since that row may carry a pname of its own. A later lookup of pname answers $?$ until lib is known, which is exactly the Wand ambiguity of @sec-motivation. In nixpkgs, `lib.optionalAttrs` is `cond: as: if cond then as else {}`, and MacroNix rejects the program at a lib with this field. Inference unifies the branches of the conditional and types optionalAttrs at $"bool" → {} → {}$, which clashes with ${"NIX_LDFLAGS": "string"}$. ] <ex-idiom-cond>


== Design Goals Revisited <sec-goals>
This section evaluates MiniNix against each design goal from the introduction in @sec-motivation.

_G1 (No source changes)._ Inference types every construct of MiniNix without annotations, because it can mark the limits of its analysis with the unknown type ★. The goal is thus met for MiniNix but not for NixLang. @app-macronix extends MiniNix with simple features such as base types and basic operators, but attribute-set patterns, recursive records and lets, and tricky builtins are not covered.

_G2 (No semantic change)._ The analysis is purely static. It inserts no casts and does not instrument the program, so evaluation is unchanged, and the goal is met by construction. The price is carried by the metatheory: progress holds only up to lookup errors ↯ (@type-safety), which the analysis cannot prevent.

_G3 (Totality)._ This goal is met in part. Uncertainty that originates from undecided lookups is deferred and kept as qualifiers, and a use that leaves it unanswered is typed at ★. There are still reasons for rejection: First, ★ has no eliminators so, for example, a program that selects from it has no typing. Second, inference rejects some programs the declarative system types (@incompleteness-inference).

_G4 (Efficient inference)._ The algorithm is designed with efficiency in mind but not yet measured against that goal. Inference stays first-order and never backtracks over typings. The fillability test of the entry point may try several labels for a key, but only to find a witness, and it never changes the answer (@inference). Row unification works modulo ≈, but every move of row unification is forced, and a parked stump is re-examined only when its blocker is solved. Termination has been proven but the given reasons are structural and no complexity bound is given.


== Outlook <outlook>

The unknown type ★ has a fundamental problem: it has no eliminators so any term typed by it can be passed around but not consumed. This is one major source of incompleteness in the algorithm as builtins, function application, concatenation and other operators do not type on terms of type ★. Not adding eliminators restricts the type system significantly but adding them is not possible either. T-★-intro types any constant c at ★, so an application rule for ★ would accept `c c`. This term is neither a value nor a lookup error, so progress would fail.

The known remedy is gradual typing. Nix provides dynamic tests for primitives (`isBool`, `isString`, `isFunction`, …), which we suggest can be used to simulate casts with conditionals and assertions. A small analysis of these builtins shows that only functions would lose their identity, which leaves us positive about the applicability of casts in Nix. Casts, however, have a problem of their own: in their known form, they can force values that a lazy evaluation strategy would not evaluate and thereby violate G2. Whether this problem can be remedied is an interesting direction for future research but instrumentation may also be avoidable altogether. Castagna and Duboc @gradual_elixir don't instrument but rely on the checks of the runtime and the programmer instead. Nix also checks the tag of every operand of an elimination, so a similar approach could provide eliminators for ★ without changing evaluation. Occurrence typing @typed_racket is an even softer option: a test such as `isString x` refines x in the positive branch of a conditional, which already helps in practice but the negative branch gains nothing.

Our core calculus MiniNix does not include some hard-to-type features of NixLang. Mutually recursive lets and recursion are tricky, but ML-style inference has a standard treatment for them: a recursive binding is monomorphic inside its own definition and generalized after it, so we expect their addition to be benign. The faithful typing of conditionals and lists however, could be tricky without subtyping. Subtyping based systems with the full suite of type connectives (union, intersection and negation) have a natural representation for both which MiniNix lacks. Conditionals, for example, need a union of both branches to be faithful to their semantics but MacroNix can only infer a single type so both branches have to match – a real gap from NixLang. Patterns, in contrast, map onto rows: an open pattern `{a, ...}` types its argument at ${a: τ | ρ}$, and only closed patterns need absence information. On the bright side, negative information, could be encoded as a constraint $⟨ρ.l ↓ ⊥⟩$ and might be a promising direction to narrow the Wand ambiguity.

Finally, our row equivalence is not faithful to the semantics of Nix. We applied the semantics of Leijen @extensible_recs and Paszke & Xie @extensible_tabular, but in their record systems, single labels can be peeled off the row, revealing shadowed occurrences. Nix only provides the builtin `removeAttrs`, which removes all occurrences. Our relation is finer in that it tells more rows apart, and the coarser relation of Nix would not lead to a cancellative trace monoid that unification can use for forced moves. The relation, however, does not equate ${l: τ | l: τ'}$ and ${l: τ}$, a common application in Nix. We have experimented with normalization (the removal of shadowed label occurrences), which works for clean rows, but we are unsure how this approach interacts with row- and label-variables.
