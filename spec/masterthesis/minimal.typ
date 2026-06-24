== Minimal Calculus
> Functions, scoped records ,∈-constraints, record concat


l ∈ 𝓛   x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

a, b, e := c | (x: e) | a ‖ b | e.l | { ξ }
ξ := ε | a = b | (a = b | ξ)

π := x ∈ (S₁ ∪ … ∪ Sₙ)
τ := α | 𝓫 | τ -> τ | { ρ }
ρ := ε | l: τ | (l: τ | ρ)


=== Inference

-----------
Γ ⊢ c: 𝓫_c


Γ x: τ₁ ⊢ e: τ₂   Γ ⊢ x: τ₁    x ⩪ Γ
--------------------------------------
Γ ⊢ (x: e): τ₂


Γ ⊢ a: ρ₁  Γ ⊢ b: ρ₂
-----------------------
Γ ⊢ a || b: (ρ₁ | ρ₂)


Γ ⊢ e: { l: τ | ρ }
---------------------
Γ ⊢ e.l: τ


== Spezialisierung

Γ ⊢          Γ ⊢ α ⩪ x
------------------------
Γ ⊢ (x: e): τ


Die neue Relation x ⩪ Γ vereinfacht wenn möglich constraints.
- (x, l ∈ (X ∪ Y)) -> l ∈ Y      :  wenn l ∉ X
- (x, l ∈ (X ∪ Y)) -> ε          :  wenn l ∈ X
- (x, l ∈ (X ∪ Y)) -> l ∈ (X ∪ Y):  otherwise


- Was machen wir, wenn die Constraints nicht bewiesen werden können?
- Eigentlich wäre es nice, die _sound_ zu beweisen
- Dafür müsste man aber deren Durchdringung verstehen


== ∈-Algorithm


--------------------
Γ ⊢ e: τ
