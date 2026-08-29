Two candidate shapes:
1. *Algorithm-W style*: syntax-directed recursion carrying a mutable solver
  state S. Unification happens at application/selection sites *immediately*.
2. *HM(X) style*: generate a constraint set C first, *solve separately*.


== Generalization (A-let) and the stump/scheme interaction
At let x = e₁ in e₂ : infer e₁ ⇒ τ₁, then generalize
ᾱ = fv(θτ₁) ∖ fv(θΓ) (Rémy-style levels for efficiency¿). Pending stumps
whose blocker or δ lies in ᾱ pose THE design fork:

*L1 — force at the boundary*: resolve such stumps to δ ≐ ★ before
generalizing. Schemes stay plain ∀ᾱ.τ.
⊕ simple; matches the declarative system EXACTLY (the regression example
`let f = (x: x.l) in f {}` gets f: ∀β. {β} → ★, as in minimal.lean)
⊖ loses cross-instantiation refinement: `let f = (x: x.l) in f {l = c}`
infers ★ where β ≔ (l: 𝓫_c) could have delivered 𝓫_c

*L2 — stumps in schemes (qualified types)*: generalize the stump with the
scheme, σ = ∀ᾱ. ⟨ρ.l ↓ δ⟩ ⇒ τ; A-var copies the stump with fresh vars per
instantiation, and each instance re-resolves independently.
⊕ strictly more precise; per-instance refinement
⊕ CORRESPONDENCE (the pretty one): the instance-closed T-let premise
"∀ τ₁ ≤ σ. Γ ⊢ e₁: τ₁" quantifies over instances — L2's per-instance
stump re-resolution is its algorithmic image. The Lean regression proof
(three-way lookup split per instance) is literally the L2 execution
trace. L2 is *natively* complete against instance-closed T-let, no
detour through the syntactic ftv-rule
⊖ schemes stop being plain HM schemes; instantiation allocates stumps
(cost); constraint-carrying schemes are the first step down the HM(X)
slope we criticized — how far before we lose "efficiently computable"?
⊖ declarative counterpart of an L2-refined use is a DIFFERENT σ at T-let
per program — completeness statement gets subtle (see below)

DECIDED (26-08-18): L1 principality is REFUTED without attempting the proof
— mechanized in minimal.lean ("Plain schemes are not principal"):
- *finalized_no_blur*: λx. x.l types declaratively at {(l: τ₀)} → τ₀ for EVERY
  τ₀ (T-sel/hit), but no substitution instance of the L1-finalized {β} → ★
  sits ⊑-below any of them with τ₀ ≠ ★ — the frozen ★ result cannot blur
  into a definite type (⊑-rigidity). *The stated principality factoring fails.*
- *no_plain_principal_scheme*: worse, no plain ∀ᾱ.τ scheme AT ALL is
  instance-closed while covering both the found-typing {(l: {ε})} → {ε} and
  the ⊥-typing {ε} → ★: its result position must be a quantified variable,
  and re-pointing that variable at {ε} in the ⊥-instance's substitution
  manufactures the instance {ε} → {ε}, which is NOT a typing (on x: {ε} the
  lookup is ⊥, the body types only at ★).

Consequence: L1 is the soundness-bearing baseline ONLY. Principality must be
stated over L2's qualified schemes — ∀β. ⟨β.l ↓ δ⟩ ⇒ {β} → δ is exactly the
principal type of λx. x.l that plain schemes cannot express (δ stays
writable per instance). L2 is not an optional refinement; it is where the
refinement claim lives.

