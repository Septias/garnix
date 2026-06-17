# Formalisierung 2

## Terms & Types
Basetypes : 𝓫 ∈ 𝓑
Labels    : ℓ ∈ 𝓛
Variables : x, y ∈ 𝓧
Typevars  : α, β, γ ∈ 𝓿

*Terms*
e := x | e₁e₂  | ς: e₂ | { e = e; } | let e₁ = e₂ in e₃
ς := e | { ξ } | { ξ, … }
ξ := ε | (x | p) | (x ? e | p)
l := α | ℓ

*Types*
κ := ∗ | κ₁ -> κ₂ | Row | Lab | Pat
σ := ∀α: κ. σ | τ
τ := 𝓫 | ★ | { p }∓ -> τ | ->¿ | ⦅l⦆ | { ρ }
ρ := ε | α | (l:τ | ρ)
p := ε | (l:τ | p) | (l: τ?τ | p)

*Kontext*
Γ := ε | Γ · (x: τ) | Γ · (α : κ)


## Kinding

Γ ⊢ ρ: Row
----------- κ-row
Γ ⊢ {ρ}: ∗

Γ ⊢ l: Lab
----------- κ-lab
Γ ⊢ ⦅l⦆: ∗


## Rewriting
{l₁ = a; {l₂ = b;}} ≙ {l₁ = a; l₂ = b;}


## Inference
*Basics*
x: σ ∈ Γ   Γ ⊢ σ ⊑ τ
--------------------- Var
Γ ⊢ x: τ

--------
Γ ⊢ ℓ: ⦅ℓ⦆

*Equivalences*
Γ ⊢ e₁: τ₁  τ₂ ≤ τ₁ 
--------------------- Sub
Γ ⊢ e₁: τ₂


Γ ⊢ e₁: τ₁  τ₂ ≡ τ₁ 
-------------------- Eq
Γ ⊢ e₁: τ₂


*Records*
Γ ⊢ a: ⦅l⦆ b: τ  
----------------- Rec-I
Γ ⊢ {a = b}: ⟨l: τ⟩ 


Γ ⊢ a: τ₁   Γ ⊢ b: τ₂
--------------------- Rec-Concat
Γ ⊢ a ‖ b: ⟨τ₁ | τ₂⟩


*Functions* (TODO: use actual patterns lule)

Γ ⊢ e₁: { p }⁻ -> τ₂    e₂ ⧀ ⌊p⌋   ⌈p⌉ ⧀ e₂
------------------------------------------- λ-E-1
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: { p }⁺ -> τ₂    e₂ ⧀ ⌊p⌋
------------------------------------------- λ-E-2
Γ ⊢ e₁e₂: τ₂

- Only retain non-optional fields
⌊p⌋ :: Row -> ∗
ε              = ε
(l: τ | p)     = { l = p | ⌊p⌋ }
(l: τ ? τ | p) = ⌊p⌋

- Retain all fields
⌈p⌉ :: Row -> ∗
ε              = ε
(l: τ | p)     = { l = p | ⌈p⌉ }
(l: τ ? τ | p) = { l = p | ⌈p⌉ }


*Let-Poly*
Γ x: ∀ᾱ. τ₁ ⊢ e₂ : τ₂     Γ ⊢ e₁: τ₁    ᾱ ∉ ftv(Γ)
-------------------------------------------------- Let
Γ ⊢ let x = e₁ in e₂: τ₂


## ∈-Solving
> Tries to solve element constraints for rows that can have (multiple) row and label variables

A => B
where A is a tuple of (row, query-label)
and   B can be one of:
- (ρ, b) : To recurse
- τ      : The type of of query-label
- ★      : An unkown type due to missing information
- α ! b  : A new constraint "Label variable α has to be b"
- a !! b : A new constraint "Row variable α has to contain b"

ρ̃: A row that has _label_ or _row variables_

- ------------------------------------------------ -
(recurse) – (⟨ρ | l: τ⟩, q) => (ρ, q)   if  q ≠ l
(solved ) – (⟨ρ | l: τ⟩, q) => τ        if  q = l

(var-lab) – (⟨ε | α: τ⟩, q) => Γ · q ∈ α ⊨ τ
(–––––––) – (⟨ρ̃ | α: τ⟩, q) => ★

(var-row) – (⟨ε | α⟩,    q) => q !! α
(–––––––) – (⟨ρ̃ | α⟩,    q) => ★
- ------------------------------------------------ -
