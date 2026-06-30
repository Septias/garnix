== Minimal Calculus
> Functions, scoped records, ∈-constraints, record concat, row-vars


l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

a, b, e := c | (x: e) | e₁e₂ | a ‖ b | e.l | { ξ }
ξ := ε | l = b | (l = b | ξ)

π := x ∈ (S₁ ∪ … ∪ Sₙ)
τ := α | 𝓫 | ★ | τ -> τ | { ρ }
ρ := ε | α | l: τ | (l: τ | ρ)


=== Inference

----------- T-cons
Γ ⊢ c: 𝓫_c


Γ x: τ₁ ⊢ e: τ₂
--------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


x ∈ e₂    Γ ⊢ (x: e₁)e₂: τ
--------------------------- T-λ∈-ok
Γ · (x ∈ S) ⊢ (x: e₁)e₂: τ


x ∉ e₂    S' = S∖x    Γ · (x ∈ S') ⊢ (x: e₁)e₂: τ
--------------------------------------------------- T-λ∈-not-in
Γ · (x ∈ S) ⊢ (x: e₁)e₂: τ


x ∉ e₂
----------------------------- T-λ∈-err
Γ · (x ∈ {x}) ⊢ (x: e₁)e₂: ★


Γ ⊢ a: { ρ₁ }  Γ ⊢ b: { ρ₂ }
----------------------------- T-conc
Γ ⊢ a ‖ b: (ρ₁ | ρ₂)


Γ ⊢ e: { l: τ | ρ }
-------------------- T-sel
Γ ⊢ e.l: τ


== Spezialisierung

Die neue Relation x ⩪ Γ vereinfacht wenn möglich constraints.
- (x, l ∈ (X ∪ Y)) -> l ∈ Y       :  wenn l ∉ X
- (x, l ∈ (X ∪ Y)) -> ε           :  wenn l ∈ X
- (x, l ∈ (X ∪ Y)) -> l ∈ (X ∪ Y) :  otherwise
