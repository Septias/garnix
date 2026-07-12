== Minimal Calculus
> Functions, scoped records, record concat, row-vars, let-poly


l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x | (x: e) | e₁e₂ | e₁ ‖ e₂ | e.l | { ξ } | let x = e₁ in e₂
ξ := ε | l = e | (ξ₁ | ξ₂)

τ := α | 𝓫 | ★ | τ -> τ | { ρ }
ρ := ε | α | l: τ | (ρ₁ | ρ₂)
σ := ∀ᾱ. τ | τ


== Declarative

----------- T-cons
Γ ⊢ c: 𝓫_c


x: σ ∈ Γ   σ ⊑ τ
------------------ T-var
Γ ⊢ x: τ


Γ·(x: τ₁) ⊢ e: τ₂
--------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: τ₁   ᾱ = ftv(τ₁) ∖ ftv(Γ)   Γ·(x: ∀ᾱ. τ₁) ⊢ e₂: τ₂
------------------------------------------------------------ T-let
Γ ⊢ let x = e₁ in e₂: τ₂


Γ ⊢ e₁: {ρ₁}  Γ ⊢ e₂: {ρ₂}
----------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ τ
------------------------------- T-sel
Γ ⊢ e.l: τ


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ?
------------------------------- T-sel-★
Γ ⊢ e.l: ★


Γ ⊢ ξ: ρ
------------------ T-rec
Γ ⊢ { ξ }: { ρ }


----------- T-ξ-empty
Γ ⊢ ε: ε


Γ ⊢ e: τ
-------------------- T-ξ-field
Γ ⊢ (l = e): (l: τ)


Γ ⊢ ξ₁: ρ₁   Γ ⊢ ξ₂: ρ₂
------------------------- T-ξ-conc
Γ ⊢ (ξ₁ | ξ₂): (ρ₁ | ρ₂)


== Instantiation
- σ ⊑ τ strips quantifiers; α in type position takes a type, in row position a row
- No tail check needed (unlike λ⟨⟩): by monotonicity of ↓, instantiating a
  row-var can never invalidate a definite lookup result — every position it
  could shadow was already ?-poisoned


----------- I-refl
τ ⊑ τ


σ[τ′/α] ⊑ τ
------------- I-ty
(∀α. σ) ⊑ τ


σ[ρ/α] ⊑ τ
------------- I-row
(∀α. σ) ⊑ τ


== Row-Lookup
- Γ ⊢ ρ.l ↓ r with r := τ | ⊥ | ?

- This statement recursively searches rows for a label l
- absent means the label provably does not exist in ρ (T-sel has no rule for it)
- unknown means an unconstrained row-var could contain l, so no sound type can be derived


----------------- L-ε
Γ ⊢ ε.l ↓ ⊥


l₁ = l₂
----------------------------- L-hit
Γ ⊢ (l₁: τ).l₂ ↓ τ


l₁ ≠ l₂
----------------------------- L-miss
Γ ⊢ (l₁: τ).l₂ ↓ ⊥


Γ ⊢ α: {ρ}   Γ ⊢ ρ.l ↓ r
----------------------------- L-α
Γ ⊢ α.l ↓ r


α ∉ Γ
----------------------------- L-α-free
Γ ⊢ α.l ↓ ?


Γ ⊢ ρ₁.l ↓ τ
----------------------------- L-conc-hit
Γ ⊢ (ρ₁ | ρ₂).l ↓ τ


Γ ⊢ ρ₁.l ↓ ⊥   Γ ⊢ ρ₂.l ↓ r
--------------------------------- L-conc-skip
Γ ⊢ (ρ₁ | ρ₂).l ↓ r


Γ ⊢ ρ₁.l ↓ ?
----------------------------- L-conc-★
Γ ⊢ (ρ₁ | ρ₂).l ↓ ?


