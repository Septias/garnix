# Formalisierung 2

## Terms & Types
Basetypes : 𝓫 ∈ 𝓑
Labels    : ℓ ∈ 𝓛
Variables : x, y ∈ 𝓧
Typevars  : α, β, γ, …

*Terms*
e := x | e₁e₂  | 𝓹: e₂ | { ρ }
𝓹 := e | { p } | { p, … }
p := ε | x     | ( l ? e )
ρ := ε | (l:τ | ρ)
l := α | ℓ

*Types*
κ := ★ | κ₁ -> κ₂ | Row | Lab
τ := τ₁ -> τ₂ | ⦅l⦆ | ⦃p⦄∓ -> τ | { ρ }
σ := ∀α: κ. σ | τ
p := τ | τ?τ

*Kontext*
Γ := α | Γ · (x: τ)

## Kinding

-----------
Γ ⊢  τ: ★


Γ ⊢ l: Lab
-----------
Γ ⊢ ⦅l⦆: ★



## Rewriting
{l₁ = a; {l₂ = b;}} ≙ {l₁ = a; l₂ = b;}
⟨l₁ = a; ⟨l₂ = b;⟩⟩ ≙ ⟨l₁ = a; l₂ = b;⟩


## Inference

Γ ⊢ a: A  l: ⦅l⦆              
----------------- Prod-I   
Γ ⊢ {l = a}: ⟨l: a⟩        


Γ ⊢ a: A  l: ⦅l⦆
----------------- Row-I
Γ ⊢ {l = a}: ⟨l: a⟩


## Unification

(Γ, α, τ) => (Γ', )
(Γ, τ, τ) => ⊤

(Tapp)   τ₁τ₂, τ₃τ₄         => τ₁ ~ τ₃ ∧ τ₂ ~ τ₄
(Tsolve) (Γ, α, τ)          => Γ · α: κ = [Γ]τ ⊢ ⊤    | if (α : κ ∈ Γ) ∧ (Γ ⊢ τ: κ)         ∧ (α ∉ ftv([Γ]τ))
(Rfield) Γ₁ ⊢ ℓ: τ, ρ₁ ~ ρ₂ => Γ₂ ⊢ τ ~ τ' ∧ ρ₁ ~ ρ'₂ | if (Γ₁ ⊢ [Γ₁]ρ₂ ℓ↪ ℓ: τ., ρ'₂ ⊣ Γ₂) ∧ ([Γ₂]ρ₁ = [Γ₁]ρ₁)
