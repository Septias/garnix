#import "../functions.typ": *

= Metatheory <metatheory>
This section gives the metatheory of the declarative system: type safety, the precision of typings and the principality of qualified schemes.

== Type Safety <type-safety>
We prove type safety up to lookup errors in the syntactic style of Wright and Felleisen @wright_felleisen, by progress and preservation over the reduction of @semantics.

#theorem(name: [Progress], lean: "qProgress")[
  If $∅ ⊢ e : τ$, then $e$ is a value, $e → e′$ for some $e′$, or $e ↯$.
] <thm-progress>

#theorem(name: [Preservation], lean: "qPreservation")[
  If $∅ ⊢ e : τ$ and $e → e′$, then $∅ ⊢ e′ : τ$.
] <thm-preservation>

Progress holds only up to lookup errors because some lookups might be typed as ★ and we cannot guarantee non-failure over this fragment. //[A derivation without T-sel-★/T-sel-⊥ never produces ↯.](needs to be proven still I reckon)


== Precision <refinement>

T-sel-★, T-sel-⊥ and T-★-intro place ★ where a more informative type may also be derivable. The following order compares typings by how much of this information they keep.

#definition(name: [Precision])[
  The _precision order_ ⊑ @gradual_criteria is the least preorder on types that has ★ as top element and is a congruence for → and for the field types of rows. It is covariant in both positions of →: it measures information content and is not a subtyping relation. We write $τ ≼ τ′$ when $τ ≈ τ″ ⊑ τ′$ for some τ″.
] <def-precision>

// why would you want to derive {} → ★? It is less precise than the previous one. The example does not really click
A more precise row can make a more precise typing derivable. For `x: ({l = c} ‖ x).l`, the domain ${β}$ only admits the result ★, since the lookup answers $?$. At the domain ${ε}$ the lookup hits, and ${ε} → 𝓫_c ⊑ {ε} → ★$ is derivable. By contrast, a ★ in a scheme stays: $θ ★ = ★$ for every substitution θ, and ★ sits ⊑-below only itself.


== Principality and Qualified Schemes <principality>

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
  Both instances are typings, so a scheme covering them must have a quantified variable as its result, since ${ε}$ and ★ are distinct closed types. Sending that variable to ${ε}$ in the substitution of the second instance leaves the argument at ${ε}$ and yields the instance ${ε} → {ε}$, which is not a typing. Choosing ★ as result does not help, since $θ ★ = ★$.
]

A qualified scheme is principal. Let

$ "selQ" quad = quad ∀(β: "Row", δ: "Type"). #h(0.3em) ⟨β.l ↓ δ⟩ ⇒ {β} → δ. $

#theorem(name: [Qualified principality], lean: "selQ_principal, selQ_greatest")[
  selQ is principal for `x: x.l` in the empty context. Moreover, selQ is greatest among the schemes whose instances are all typings of `x: x.l`: every instance τ of such a scheme is matched by an instance τ′ of selQ with $τ′ ≼ τ$.
] <thm-selq>

Discharge pins δ to the lookup's verdict, so ${ε} → {ε}$ is excluded. One binding then serves incompatible uses. The inference algorithm of @inference answers `x: x.l` with a scheme that has the instances of selQ, so its answer is principal.
