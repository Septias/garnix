== Minimal Calculus
> Functions, scoped records, ∈-constraints, record concat, row-vars


l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x | (x: e) | e₁e₂ | e₁ ‖ e₂ | e.l | { ξ }
ξ := ε | l = e | (ξ₁ | ξ₂)

τ := α | 𝓫 | ★ | τ -> τ | { ρ }
ρ := ε | α | l: τ | (ρ₁ | ρ₂)


== Inference

----------- T-cons
Γ ⊢ c: 𝓫_c


x: τ ∈ Γ
----------- T-var
Γ ⊢ x: τ


Γ·(x: τ₁) ⊢ e: τ₂
--------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: {ρ₁}  Γ ⊢ e₂: {ρ₂}
----------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ ρ.l: τ   Γ ⊢ e: {ρ}
------------------------ T-sel
Γ ⊢ e.l: τ


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


== Row-Lookup
- Γ ⊢ ρ.l
- This statement recursively searches rows for a label l
- If no derivation can be found, the label does not exist in ρ
- If a row-var is found, return the unknown type ★


l₁ = l₂
----------------------------- T-look-hit
Γ ⊢ (l₁: τ | ρ).l₂ : τ


l₁ ≠ l₂      Γ ⊢ ρ.l₂: τ₂
----------------------------- T-look-rec
Γ ⊢ (l₁: τ₁ | ρ).l₂: τ₂


Γ ⊢ α: {ρ′}  Γ ⊢ ρ′.l: τ
------------------------------ T-look-Γ-rec
Γ ⊢ (α | ρ).l: τ


Γ ⊢ α: {ρ′}  Γ ⊢ ρ.l: τ
----------------------------------- T-look-Γ-cont
Γ ⊢ (α | ρ).l: τ


α ∉ Γ
----------------------------- T-look-Γ-neg
Γ ⊢ (α | ρ).l: ★


