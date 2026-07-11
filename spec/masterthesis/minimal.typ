== Minimal Calculus
> Functions, scoped records, ∈-constraints, record concat, row-vars


l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

a, b, e := c | x | (x: e) | e₁e₂ | a ‖ b | e.l | { ξ }
ξ := ε | l = b | (l = b | ξ)

π := x ∈ (S₁ ∪ … ∪ Sₙ)
τ := α | 𝓫 | ★ | τ -> τ | { ρ }
ρ := ε | α | (l: τ | ρ)


== Inference

----------- T-cons
Γ ⊢ c: 𝓫_c


Γ x: τ₁ ⊢ e: τ₂
--------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ a: { ρ₁ }  Γ ⊢ b: { ρ₂ }
----------------------------- T-conc
Γ ⊢ a ‖ b: (ρ₁ | ρ₂)


Γ ⊢ ρ.l: τ   Γ ⊢ e: {ρ}
------------------------ T-sel
Γ ⊢ e.l: τ


== Row-Lookup
- Γ ⊢ ρ.l
- This statement recursively searches rows for a label l
- If no derivation can be found, the label does not exist in ρ
- If a row- or label-var is found, return the unknown type ★


l₁ = l₂
----------------------------- T-look-hit
Γ ⊢ (l₁: τ | ρ).l₂ : τ


l₁ ≠ l₂      Γ ⊢ ρ.l₂: τ₂
----------------------------- T-look-rec
Γ ⊢ (l₁: τ₁ | ρ).l₂: τ₂


l₁ ≠ l₂
----------------------------- ↯
Γ ⊢ ( l₁: τ | ε ).l₂: τ


Γ ⊢ α: {ρ}  Γ ⊢ ρ.l: τ
------------------------------ T-look-Γ-rec
Γ ⊢ (α | ρ).l: τ


Γ ⊢ α: {ρ}   Γ ⊢̷ ρ.l: τ₁   ρ.l: τ₂
----------------------------------- T-look-Γ-cont
Γ ⊢ (α | ρ).l: τ₂


α ∉ Γ
----------------------------- T-look-Γ-neg
Γ ⊢ (α | ρ).l: ★


