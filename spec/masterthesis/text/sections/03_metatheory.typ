#import "../functions.typ": *

= Metatheory <metatheory>
This section gives the metatheoretic results of the declarative system.

== Type Safety <type-safety>
We prove type safety up to lookup errors for the declarative system in the syntactic style of Wright and Felleisen @wright_felleisen, by progress and preservation over the reduction of @semantics.

#theorem(name: [Progress], lean: "qProgress")[
  If $∅ ⊢ e : τ$, then $e$ is a value, $e → e′$ for some $e′$, or $e ↯$.
] <thm-progress>

#theorem(name: [Preservation], lean: "qPreservation")[
  If $∅ ⊢ e : τ$ and $e → e′$, then $∅ ⊢ e′ : τ$.
] <thm-preservation>

Progress holds only up to lookup errors because some lookups might be typed as ★ and we cannot guarantee non-failure over this fragment.

// a derivation without T-sel-★/T-sel-⊥ never produces ↯
== Refinement and the Maximality of ★ <refinement>

T-sel-★, T-sel-⊥ and T-★-intro place ★ where a more informative type may also be derivable. The following order compares typings by how much of this information they keep.

#definition(name: [Precision])[
  The _precision order_ ⊑ @gradual_criteria is the least preorder on types that has ★ as top element and is a congruence for → and for the field types of rows. It is covariant in both positions of →: it measures information content and is not a subtyping relation. We write $τ ≼ τ′$ when $τ ≈ τ″ ⊑ τ′$ for some τ″.
] <def-precision>

Solving a row-variable never loses a typing. The statement needs schemes to follow the substitution: θ _maps schemes_ if every well-formed scheme σ₀ has a well-formed scheme σ₀′ whose instances are exactly the images θτ of the instances τ of σ₀. θΓ replaces every scheme of Γ by such an image.

#lemma(name: [Type substitution], lean: "qtyped_applySubst, SchemeImage")[
  Let θ map schemes. If $Γ ⊢ e : τ$, then $θ Γ ⊢ e : θ τ$.
] <lem-subst>

It may, however, admit a more precise one in the sense of @def-precision. By @lookup-metatheory, definite lookups survive θ, while a $?$ re-resolves against the substituted row and can become definite.

For `x: ({l = c} ‖ x).l` with $x : {β}$, the typing ${β} → ★$ is carried to ${ε} → ★$ under $[β ≔ ε]$ by @lem-subst, and ${ε} → 𝓫_c ⊑ {ε} → ★$ becomes derivable as well, since the lookup now hits. Conversely, ★ is never sharpened.

#lemma(name: [Maximality of ★], lean: "finalized_no_blur")[
  Let $τ₀ ≠ ★$. There is no substitution θ with $θ({β} → ★) ⊑ {(l: τ₀)} → τ₀$.
] <lem-star-max>

@lem-star-max is used in the proof of @thm-no-plain.


== Principality of `x: x.l` and Qualified Schemes <principality>

#definition(name: [Principal scheme])[
  A scheme σ is _principal_ for e in Γ if
  $
    (∀τ. #h(0.3em) σ ≥ τ ⟹ Γ ⊢ e : τ) and (∃τ. #h(0.3em) σ ≥ τ) and (∀τ. #h(0.3em) Γ ⊢ e : τ ⟹ ∃τ′. #h(0.3em) σ ≥ τ′ and τ′ ≼ τ).
  $
] <def-principal>

The three conjuncts are instance-closure, inhabitation and coverage: every instance of σ is a typing of e, σ has an instance, and every typing of e is matched by an instance at least as precise. [Coverage is stated up to ≼ rather than equality, since typings are closed under T-eq and T-★-intro while instance sets are not.](explain this better)

#theorem(
  name: [No plain scheme is principal],
  lean: "no_plain_principal_scheme",
)[
  There is no plain scheme σ whose instances are all typings of `x: x.l` and which has both ${(l: {ε})} → {ε}$ and ${ε} → ★$ as instances. In particular, no plain scheme is principal for `x: x.l`.
] <thm-no-plain>

#proof[
  Both instances are typings, so a scheme covering them must have a quantified variable as its result, since ${ε}$ and ★ are distinct closed types. Sending that variable to ${ε}$ in the substitution of the second instance leaves the argument at ${ε}$ and yields the instance ${ε} → {ε}$, which is not a typing. Choosing ★ as result does not help, by @lem-star-max.
]

A qualified scheme is principal. Let

$ "selQ" quad = quad ∀(β: "Row", δ: "Type"). #h(0.3em) ⟨β.l ↓ δ⟩ ⇒ {β} → δ. $

#theorem(name: [Qualified principality], lean: "selQ_principal, selQ_greatest")[
  selQ is principal for `x: x.l` in the empty context. Moreover, selQ is greatest among the schemes whose instances are all typings of `x: x.l`: every instance τ of such a scheme is matched by an instance τ′ of selQ with $τ′ ≼ τ$.
] <thm-selq>

Discharge pins δ to the lookup's verdict, so ${ε} → {ε}$ is excluded. One binding then serves incompatible uses. The inference algorithm of @inference answers `x: x.l` with a scheme that has the instances of selQ, so its answer is principal (`runF_selEx_principal` in Lean).

#corollary(
  name: [Qualified schemes type strictly more],
  lean: "l1_strictly_weaker",
)[
  Let L₁ be the system with plain schemes and L₂ the qualified one. There are e and τ with $∅ ⊢ e : τ$ in L₂ but not in L₁, for instance
  $
    ∅ ⊢ #h(0.3em) bold("let") f = (x: x.l) bold("in") { a = f {l = c}; b = f {} } quad : quad {a: 𝓫_c | b: ★}.
  $
] <cor-strict>

Qualified schemes are thus forced in our system, not chosen.
