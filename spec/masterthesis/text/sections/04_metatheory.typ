#import "../functions.typ": *

= Metatheory <metatheory>

This section gives the metatheoretic results about the declarative system as a whole. The results of this thesis are mechanized in Lean 4; each mechanized result names its Lean counterpart, and @sec-mechanization lists the points where the development encodes the presentation differently.


== Type Safety <type-safety>
We prove type safety up to lookup errors for the declarative system in the syntactic style of Wright and Felleisen @wright_felleisen, by progress and preservation over the reduction of @semantics.

#theorem(name: [Progress], lean: "qProgress")[
  If $∅ ⊢ e : τ$, then $e$ is a value, $e → e′$ for some $e′$, or $e ↯$.
] <thm-progress>

#theorem(name: [Preservation], lean: "qPreservation")[
  If $∅ ⊢ e : τ$ and $e → e′$, then $∅ ⊢ e′ : τ$.
] <thm-preservation>

Both theorems are stated for the qualified system. Progress holds only up to lookup errors: the ↯-disjunct is the price of typing an absent field at ★ by T-sel-⊥. Lookup errors arise only at such a selection and are otherwise only propagated. Preservation holds exactly: the type is unchanged, not merely refined. Its central lemma is substitution for `let`: a value that types at every instance of a scheme may replace a variable bound to that scheme, which is exactly what the instance-closure premise of T-let provides.


== Refinement and the Rigidity of ★ <refinement>

Solving a row-variable never loses a typing.

#lemma(name: [Type substitution], lean: "qtyped_applySubst")[
  Let θ be a substitution that has an image for each scheme in Γ. If $Γ ⊢ e : τ$, then $θ Γ ⊢ e : θ τ$.
] <lem-subst>

It may, however, admit a more precise one in the sense of @def-precision. By @lookup-metatheory, definite lookups survive θ, while a $?$ re-resolves against the substituted row and can become definite.

For `x: ({l = c} ‖ x).l` with $x : {β}$, the typing ${β} → ★$ is carried to ${ε} → ★$ under $[β ≔ ε]$ by @lem-subst, and ${ε} → 𝓫_c ⊑ {ε} → ★$ becomes derivable as well, since the lookup now hits. Conversely, ★ is never sharpened.

#lemma(name: [Rigidity of ★], lean: "finalized_no_blur")[
  Let $τ₀ ≠ ★$. There is no substitution θ with $θ({β} → ★) ⊑ {(l: τ₀)} → τ₀$.
] <lem-rigid>

@lem-rigid drives the principality result below and reappears as the rigidity of ★ in unification (@unification-ty).


== Principality Forces Qualified Schemes <principality>

#definition(name: [Principal scheme])[
  A scheme σ is _principal_ for e in Γ if
  $
    (∀τ. #h(0.3em) σ ≥ τ ⟹ Γ ⊢ e : τ) and (∃τ. #h(0.3em) σ ≥ τ) and (∀τ. #h(0.3em) Γ ⊢ e : τ ⟹ ∃τ′. #h(0.3em) σ ≥ τ′ and τ′ ≼ τ).
  $
] <def-principal>

The three conjuncts are instance-closure, inhabitation and coverage: every instance of σ is a typing of e, σ has an instance, and every typing of e is matched by an instance at least as precise. Coverage is stated up to ≼ rather than equality, since typings are closed under T-eq and T-★-intro while instance sets are not.

#theorem(
  name: [No plain scheme is principal],
  lean: "no_plain_principal_scheme",
)[
  There is no plain scheme σ whose instances are all typings of `x: x.l` and which has both ${(l: {ε})} → {ε}$ and ${ε} → ★$ as instances. In particular, no plain scheme is principal for `x: x.l`.
] <thm-no-plain>

#proof[
  Both instances are typings, so a scheme covering them must have a quantified variable as its result, since ${ε}$ and ★ are rigid. Sending that variable to ${ε}$ in the substitution of the second instance leaves the argument at ${ε}$ and yields the instance ${ε} → {ε}$, which is not a typing. Choosing ★ as result does not help, by @lem-rigid.
]

A qualified scheme is principal. Let

$ "selQ" quad = quad ∀(β: "Row", δ: "Type"). #h(0.3em) ⟨β.l ↓ δ⟩ ⇒ {β} → δ. $

#theorem(name: [Qualified principality], lean: "selQ_principal, selQ_greatest")[
  selQ is principal for `x: x.l` in the empty context. Moreover, selQ is greatest among the schemes whose instances are all typings of `x: x.l`: every instance τ of such a scheme is matched by an instance τ′ of selQ with $τ′ ≼ τ$.
] <thm-selq>

Discharge pins δ to the lookup's verdict, so ${ε} → {ε}$ is excluded. One binding then serves incompatible uses.

#corollary(
  name: [Qualified schemes type strictly more],
  lean: "l1_strictly_weaker",
)[
  Let L₁ be the system with plain schemes and L₂ the qualified one. There are e and τ with $∅ ⊢ e : τ$ in L₂ but not in L₁, for instance
  $
    ∅ ⊢ #h(0.3em) bold("let") f = (x: x.l) bold("in") { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}.
  $
] <cor-strict>

Qualified schemes are thus forced, not chosen.
