#import "../functions.typ": *

= Discussion and Conclusion <sec-discussion>


== The Idioms of the Introduction <sec-idioms>

We return to the three expressions from the introduction @nix-idioms and conduct a case study on their typing. The types below are result of the Lean implementation of the inference algorithm from @inference on MacroNix (@app-macronix), which extends MiniNix by booleans, lists and a limmited set of builtins. The programs are only adapted slightly to fit the algorithm by turing free names into parameters.


#example(name: [Meta update])[
  $
    #raw("drv: drv // { meta = (drv.meta or { }) // { broken = false; }; }") \
    quad : quad ⟨(ρ₁ | "meta": {ε})."meta" ↓ {ρ₂}⟩ ⇒ {ρ₁} → {"meta": {"broken": "bool" | ρ₂} | ρ₁}
  $
  MacroNix derives `drv.meta or { }` as the selection of meta from `{ meta = { }; } ‖ drv` (@fig-nix-parity). Its lookup stops on the row of drv and parks a stump, and the concatenation with `{ broken = false; }` spends the stump on a record ${ρ₂}$. The stump stays a qualifier of the answer, but its lookup can no longer miss: at a drv with meta it hits that field, and at a drv without meta it reaches the default and answers ${ε}$. The result is faithful to the semantics of `//`: the new broken shadows any broken of the old meta, every other field of the old meta survives in ρ₂, and every other field of drv survives in ρ₁.
] <ex-idiom-meta>


#example(name: [Override])[
  $
    & #raw("mkDerivation: extra: args:") \
    & quad #raw("mkDerivation (args // {") \
    & quad quad #raw("buildInputs = (args.buildInputs or [ ]) ++ [ extra ]; })") \
    & quad : quad ⟨(ρ | "buildInputs": [γ])."buildInputs" ↓ [α]⟩ \
    & quad quad quad ⇒ ({"buildInputs": [α] | ρ} → β) → α → {ρ} → β
  $
  The argument handed to mkDerivation carries the new buildInputs in front of the row of args, so it shadows the old one. The selection `args.buildInputs or [ ]` looks up buildInputs in the row of `{ buildInputs = [ ]; } ‖ args`, where γ is the element type of the empty default. The lookup stops on the row of args, and `++` spends the stump at the list type $[α]$, so its result shares α with the type of extra. The default makes the lookup total: at an args with buildInputs it hits that field, and at an args without it reaches the default. One binding of the function therefore serves both uses. Without the default, the lookup at an args without buildInputs misses, its result is ★, and inference reports a clash, since ★ cannot be appended.
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
Applied to `{ pname = "hello"; }`, the meta update types at ${"meta": {"broken": "bool"} | "pname": "string"}$, since the lookup of meta reaches the default. Without the default, `drv: drv // { meta = drv.meta // { broken = false; }; }` shows the other side of G3. Applied to the same package, inference reports a clash: the lookup of meta misses, so the result of the stump is pinned to ★, but the concatenation has already used it as a record. The declarative system rejects the program as well, since ★ cannot be concatenated. The program nevertheless evaluates without error, because meta is never selected. In both idioms, the default of `or` turns a possible miss into a definite answer.

== Design Goals Revisited <sec-goals>
We now discuss to which extend every of our design goals was reached by MiniNix.


_G1 (No source changes)._ Every construct of MiniNix is typed annotation free because inference can explicitly mark its boundaries using the unknown type ★. The goal is thus met for MiniNix, but not for NixLang. @app-macronix extends MiniNix with trivial features such as base-types and trivial operators, but features like pattern matching, recursive records and lets, and tricky builtins remain uncovered. Nixpkgs can only be analysed verbatime once they are covered.

_G2 (No semantic change)._ The analysis is purely static. It inserts no casts and does not instrument the program, so evaluation is unchanged, and the goal is met by construction. The price is carried by the metatheory: progress holds only up to lookup errors ↯ (@type-safety), which the analysis cannot prevent.


_G3 (Totality)._ This goal is met in part. Uncertainty that originates from undecidable lookups are deferred and kept as qualifiers. A use that leaves them unanswered is typed at ★. There are still reasons for rejection: First, ★ has no eliminators so, for example, a program that selects from it has no typing. Second, inference reports a clash on some programs the declarative system types (@incompleteness-inference). Third, inference rejects every stuck row equation.

_G4 (Efficient inference)._ THe algorithm is designed with efficiency in mind but not yet measured. Inference stays first-corder and never backtracks over typings. The inhabitation check of the entry point may try several labels for a key, but only to find a witness, and it never changes the answer (@inference). Row unification works modulo ≈, but every move of the row pass is forced, and a parked stump is re-examined only when its blocker is solved. Termination has been proven but the given reasons are structural and no complexity bound is given. Since the algorithm extends Hindley-Milner inference, its runtim is worst-case exponential in pathological cases [\@cite](?).


== Outlook <sec-gradual>

★ has no eleminators such that any term typed by it can be passed around but not consumed. This is one mayor source of incompleteness in the algorithm @[cite](todo). Builtins, function application, concatenation and other operators all fail on these values. Not adding eliminators restricts the typesystem by a lot but adding them is not possible for the current system. [](add example why its unsound here). The known escape is gradual typing.  Sekiyama and Igarashi @gradual_extensible_rows show gradual typing in a. Nix provides the needed primitives `isBool`, `isString`, `isFunction` as dynamic tests that can probably be reused for casts. We have conducted a small analysis for their usability [and only functions would loose their identity?](check this). Nevertheless, an automated instrumentation in the sense of @[cite](castagna work on elixir I think). Casts are therefore possible but have a problem on their own: In their known usage, they may force values in a lazy system on boundaries and thereby violate G2. A version of non-forcing lazy casts would be an interesting research direction. And even if it does not work, the tradeoff would probably be worth it. [](note here occurrence typing as the soft version that only works in the positive case but can help already)

Our core calculus MinNix does not include some hard-to type features of NixLang. We expect the mutual recursiveness of lets and recursiveness not to cause too many problems, Broekhoof and Krebbers already paved the way. The faithful typing of conditionals, lists and patterns could be tricky though. All these features have a natural represenentation in subtyping based systems with the full suite of type connectives (union, intersection and negation). MacroNix can not create the union of two types so both branch of a conditional a have to be equal and lists are homogenous. A pattern in nix can be _open_, meaning a record can supply more than the expected fields. Subtyping usually handles this situation but we have argue against it due to the semantic problems with open record concatenation. Negative information, howerver could be encoded as a constraints $⟨ρ.l ↓ ⊥⟩$ and could be a promising addition to MiniNix.

Finally, the row semantic we use is not faithful to nix' record semantic. We applied the semantic of leijen and P&X but in their record systems, single labels can be pealed of the row, revealing nested occurrences. Nix only provides the `removeAttr` attribute that removes all occurrences. Our relation is finer in that it tells more rows apart and the coarser relation of Nix would not lead to a cancellative trace monoid that unification can use for forced moves. It does, however not unify not unify ({l: τ, l: τ'} = {l: τ}), a common application in Nix. We have experimented with normalization (and the removal of shadowed label occurrences) which works for clean rows but are unsure how this approach interacts with row and label-variables.


== Conclusion

The aim of this thesis was to type asymmetric record concatenation and first-class labels, the operations at the centre of NixLang, without annotations and without changing the meaning of a single program. Its canonical obstacle, `a: b: (a ‖ b).l`, admits no principal typing in a row calculus that must decide where the field comes from. We have taken the decision out of the calculus. Scoped labels make concatenation a total juxtaposition of rows, a lookup relation answers a field demand with a type, with definite absence or with _don't know_, a pending answer is kept as a qualifier that is decided by evaluating the lookup at each use, and an answer that never arrives is turned into the unknown type.

The resulting calculus, MiniNix, is type-safe up to lookup errors, and progress and preservation are mechanized in Lean. Row unification operates on spines, makes only forced moves and returns a most general unifier when it succeeds. This improves the situtation for P&X, but it does not fit the semantic of Nix. The inference algorithm built on unification parks unanswered lookups as stumps, terminates and answers schemes whose instances are declarative typings. We analysed the shortcommings of the algorithm and gave an outlook on how to overcome some of them. We now place our work into the literature.

