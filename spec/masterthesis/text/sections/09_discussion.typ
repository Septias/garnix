#import "../functions.typ": *

= Discussion and Conclusion <sec-discussion>


== The Idioms of the Introduction <sec-idioms>

We return to the three next expressions from the introduction @nix-idioms and conduct a case study on their typing. The types below are result of the Lean implementation of the inference algorithm from @inference on MacroNix (@app-macronix), which extends MiniNix by booleans, lists and builtins such as `++`. The programs are only adapted slightly to fit the algorithm by turing free names into parameters.


#example(name: [Overlay])[
  $
    #raw("self: super: { fields = super.foo // { meta = { broken = false; }; }; }") \
    quad : quad ⟨ρ₂."foo" ↓ {ρ₁}⟩ ⇒ α → {ρ₂} → {"fields": {"meta": {"broken": "bool"} | ρ₁}}
  $
  The selection `super.foo` parks a stump on the row of super, and the concatenation spends the stump on a record ${ρ₁}$. The stump stays a qualifier of the answer. The instance that places the field $"foo": {ρ₁}$ in the row of super is the typing $α → {"foo": {ρ₁} | ρ₂} → {"fields": {"meta": {"broken": "bool"} | ρ₁}}$. The result is faithful to the semantics of `//`: the new meta shadows any meta of `super.foo`, and every other field of `super.foo` survives in ρ₁.
] <ex-idiom-overlay>


#example(name: [Override])[
  $
    & #raw("mkDerivation: extra: args:") \
    & quad #raw("mkDerivation (args // { buildInputs = args.buildInputs ++ [ extra ]; })") \
    & quad : quad ⟨ρ."buildInputs" ↓ [α]⟩ ⇒ ({"buildInputs": [α] | ρ} → β) → α → {ρ} → β
  $
  The argument handed to mkDerivation carries the new buildInputs in front of the row of args, so it shadows the old one. The selection `args.buildInputs` cannot be answered, since the row of args is open, and it is spent, since `++` uses its result at the list type $[α]$. The stump therefore stays a qualifier of the answer, and its result shares α with the type of extra. Since the stump is spent, every instance must make its lookup hit. At a use whose args has no buildInputs, the lookup misses, its result is ★, and inference reports a clash, since ★ cannot be appended.
] <ex-idiom-override>

#example(name: [Conditional extension])[
  $
    & #raw("lib: stdenv: { pname = \"hello\"; }") \
    & quad #raw("// lib.optionalAttrs stdenv.isDarwin { NIX_LDFLAGS = \"-liconv\"; }") \
    & quad : quad ⟨ρ₂."isDarwin" ↓ α⟩, ⟨ρ₁."optionalAttrs" ↓ (α → {"NIX_LDFLAGS": "string"} → {ρ₃})⟩ \
    & quad quad quad ⇒ {ρ₁} → {ρ₂} → {ρ₃ | "pname": "string"}
  $
  Both selections park a stump, and the stump on the row of lib is spent, since its result is applied. The result keeps pname behind the row ρ₃ returned by `lib.optionalAttrs`, since that row may carry a pname of its own. A later lookup of pname answers $?$ until lib is known, which is exactly the Wand ambiguity of @sec-motivation. In nixpkgs, `lib.optionalAttrs` is `cond: as: if cond then as else {}`, and MacroNix rejects the program at a lib with this field. Inference unifies the branches of the conditional and types optionalAttrs at $"bool" → {} → {}$, which clashes with ${"NIX_LDFLAGS": "string"}$. [The declarative system rejects it as well: the only common type of the branches is ★, and ★ cannot be concatenated.](explain the consequences here)
] <ex-idiom-cond>

// PROF: Here T-sel-⊥'s laziness motivation fails in composition; a bottom type would type it.
Applied to concrete package sets, the overlay shows both sides of G3. Applied to `{}` and `{ foo = { pname = "hello"; }; }`, it types at ${"fields": {"meta": {"broken": "bool"} | "pname": "string"}}$. Applied to `{}` and `{}`, inference reports a clash: the lookup of foo misses, so the result of the stump is pinned to ★, but the concatenation has already used it as a record. The declarative system rejects the program as well, since ★ cannot be concatenated . The program nevertheless evaluates without error, because `fields` is never selected.

== Design Goals Revisited <sec-goals>


_G1 (No source changes)._ Every construct of MiniNix is typed annotation free because inference can explicitly mark its boundaries using the unknown type ★. The goal is thus met for MiniNix, but not for NixLang. @app-macronix extends MiniNix with trivial features such as base-types and trivial operators, but features like pattern matching, recursive records and lets, and tricky builtins remain uncovered. Nixpkgs can only be analysed verbatime once they are covered.

_G2 (No semantic change)._ The analysis is purely static. It inserts no casts and does not instrument the program, so evaluation is unchanged, and the goal is met by construction. The price is carried by the metatheory: progress holds only up to lookup errors ↯ (@type-safety), which the analysis cannot prevent.


_G3 (Totality)._ This goal is met in part. Uncertainty that originates from undecidable lookups are deferred and kept as qualifiers. A use that leaves them unanswered is typed at ★. There are still reasons for rejection: First, ★ has no eliminators so, for example, a program that selects from it has no typing. Second, inference reports a clash on some programs the declarative system types (@incompleteness-inference). Third, inference rejects every stuck row equation. Fourth, a key that is used as a label before it is known to be a string is a type error (@sec-fc-labels).

_G4 (Efficient inference)._ THe algorithm is designed with efficiency in mind but not yet measured. Inference stays first-corder and never backtracks over typings. [The constraints in Q are very simple and solve linearly?]. The inhabitation check of the entry point may try several labels for a key, but only to find a witness, and it never changes the answer (@inference). Row unification works modulo ≈, but every move of the row pass is forced, and a parked stump is re-examined only when its blocker is solved. Termination has been proven but the given reasons are structural and no complexity bound is given. Since the algorithm extends Hindley-Milner inference, its runtim is worst-case exponential in pathological cases.


== Outlook <sec-gradual>

Our error type ★ has a central problem: It has no eleminators such that using it easily leads to [death](wording). [When we add eliminators… everything breaks, so we need gradual typing](wording). A type in ★ has to be converted into a proper type, usually by casts. Sekiyama and Igarashi @gradual_extensible_rows show that consistency extends to extensible rows. `(x: x 4) 3` would type and get stuck. The casts of gradual systems may force types in lazy evaluation, so one would need the lazy version \@thiemann_cite

// There is no general principality theorem to preserve: @principality settles principality only for `x: x.l`.
// - Note negative info here
// - Note occurrence typig
// - Note that ifs usually need a union then


== Conclusion

// Goal type asymmetric record conc and fc-labels
// We created a row calculus for Nix
// It adds the unkown type and stumps to schemes
// Stumps are forced by principality constraints
// We then use the same language for parked stumps
// We notice that row unification can be made forced in a trace monoid
// But that only under ≈ which is _not_ the right semantic for Nix
// This is enabled by stumps
// We then extended to fc-labels and macro-nix

The aim of this thesis was to type asymmetric record concatenation, the operation at the centre of NixLang, without annotations and without changing the meaning of a single program. Its canonical obstacle, `a: b: (a ‖ b).l`, admits no principal typing in a row calculus that must decide where the field comes from. We have taken the decision out of the calculus. Scoped labels make concatenation a total juxtaposition of rows, a lookup relation answers a field demand with a type, with definite absence or with _don't know_, a pending answer is kept as a qualifier that is decided by evaluating the lookup at each use, and an answer that never arrives is read at a use as the unknown type ★.

The resulting calculus, MiniNix, is type-safe up to lookup errors, and progress and preservation are mechanized in Lean. Since a definite miss is typed at ★, principality for `x: x.l` forces schemes to carry their pending lookups: no plain scheme is principal for it, while a qualified one is, and one binding then serves uses that no plain scheme can serve together. Row unification operates on spines, makes only forced moves and returns a most general unifier when it succeeds; the inference algorithm built on it parks unanswered lookups as stumps, terminates and answers schemes whose instances are declarative typings; for `x: x.l` the answer is principal. The places where the algorithm gives up that we know of fall into three groups: row equations without a most general unifier, stops that are the price of forced moves, and limits of inference itself. Whether the three groups are exhaustive is open.

And ★ must become usable rather than merely recorded. A gradual reading of the unknown type without casts is one candidate, but it has to restrict where inference may choose ★: with ★ chosen implicitly, `(x: x 4) 3` types and gets stuck.


-----
We only reached our design goals partly. MiniNix requires no source changes and also forces no semantic changes. Totality is only reached in part, but an extension with gradual typing is seen as a promising direction.
