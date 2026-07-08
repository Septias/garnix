== Proofs
Given a set of values and smallstep reduction semantic (→):

*Progress*: If ⊢ e: τ then either e ∈ Values or there ∃e' such that e → e'

*Proof*: By induction on the typing derivation.
- *T-cons*: e = c. Constants are values, done.
- *T-λ-I*: e = (x: e'). Abstractions are values, done.
- *T-λ-E*: e = e₁e₂ with ∅ ⊢ e₁: τ₁ → τ₂ and ∅ ⊢ e₂: τ₁.
  By IH on e₁: either e₁ is a value or e₁ → e₁'.
  - If e₁ → e₁': then e₁e₂ → e₁'e₂ (congruence). ✓
  - If e₁ is a value of type τ₁ → τ₂: must be (x: e').
    Then (x: e')e₂ → e'[x ↦ e₂] by β-reduction. ✓
- *T-conc*: e = a ‖ b with ∅ ⊢ a: { ρ₁ } and ∅ ⊢ b: { ρ₂ }.
  By IH on a and b.
  - If a → a': then a ‖ b → a' ‖ b. ✓
  - If b → b': then a ‖ b → a ‖ b'. ✓
  - If both values: a = { ξ₁ }, b = { ξ₂ },
    so { ξ₁ } ‖ { ξ₂ } → { ξ₁ | ξ₂ }. ✓
- *T-sel*: e = e'.l with ∅ ⊢ e': { l: τ | ρ }.
  By IH on e': either e' is a value or e' → e''.
  - If e' → e'': then e'.l → e''.l. ✓
  - If e' is a value of record type: e' = { ξ } and l ∈ ξ,
    so { ξ }.l → b where (l = b) ∈ ξ. ✓
- *T-λ∈-ok / T-λ∈-not-in*: Only applicable under non-empty Γ, vacuous. □



*Preservation*: If Γ ⊢ e: τ and e → e' then Γ ⊢ e': τ

*Proof*: By induction on the typing derivation, with case analysis on the step.
- *T-cons*: e = c, a value — no step possible, vacuous.
- *T-λ-I*: e = (x: e_body), a value — no step possible, vacuous.
- *T-rcd*: e = { ξ }, a value — no step possible, vacuous.
- *T-λ-E*: e = e₁e₂ with Γ ⊢ e₁: τ₁ → τ₂ and Γ ⊢ e₂: τ₁. Case on the step:
  - β: e₁ = (x: e_body) and e₂ value → e₁e₂ → e_body[x ↦ e₂].
    By substitution lemma: Γ ⊢ e_body[x ↦ e₂]: τ₂. ✓
  - appFun: e₁ → e₁' → by IH, Γ ⊢ e₁': τ₁ → τ₂, so Γ ⊢ e₁'e₂: τ₂ by T-λ-E. ✓
  - appArg: e₁ value, e₂ → e₂' → by IH, Γ ⊢ e₂': τ₁, so Γ ⊢ e₁e₂': τ₂ by T-λ-E. ✓
- *T-conc*: e = a ‖ b with Γ ⊢ a: { ρ₁ } and Γ ⊢ b: { ρ₂ }. Case on the step:
  - catVal: { ξ₁ } ‖ { ξ₂ } → { ξ₁ | ξ₂ } → Γ ⊢ { ξ₁ | ξ₂ }: { ρ₁ | ρ₂ } by T-rcd. ✓
  - catLeft: a → a' → by IH, Γ ⊢ a': { ρ₁ }, so Γ ⊢ a' ‖ b: { ρ₁ | ρ₂ }. ✓
  - catRight: b → b' → by IH, Γ ⊢ b': { ρ₂ }, so Γ ⊢ a ‖ b': { ρ₁ | ρ₂ }. ✓
- *T-sel*: e = e'.l with Γ ⊢ e': { l: τ | ρ }. Case on the step:
  - selVal: e' = { ξ } and (l = v) ∈ ξ → { ξ }.l → v.
    From the record typing of { ξ }, field l has type τ, so Γ ⊢ v: τ. ✓
  - selStep: e' → e'' → by IH, Γ ⊢ e'': { l: τ | ρ }, so Γ ⊢ e''.l: τ by T-sel. ✓
- *T-λ∈-ok / T-λ∈-not-in*: Reduces to the underlying T-λ-E case. □

- Problem: T-λ∈-ok: The inner type can change after an application.

*Key lemma — Substitution*: If Γ, x:τ₁ ⊢ e: τ₂ and Γ ⊢ v: τ₁ then Γ ⊢ e[x ↦ v]: τ₂.
- sorry


Given an algorithmic ⊨ and deductive typesystem ⊢ we show:

*Soundness*: If ⊨ e: τ then ⊢ e: τ
- Hier ist die Frage, wie sich die Constraints

*Copmletenes*: If ⊢ e: τ then ⊨ e: τ


