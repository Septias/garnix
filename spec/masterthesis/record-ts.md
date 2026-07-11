# Formalisierung
> A typesystem with records and function patterns. We use scoped rows with row- and label-variables which can create ambiguous scenarious. An unkown type ★ is used when the typesystem can not derive sound types. ∈-solving is a novel algorithm that tries to solve ∈-constraints of the form `a ∈ (A ∪ B)`` where a is a field and A,B are records. A motivating example is: `x: y: (x ‖ y).a` where x and y are variables and ‖ is the concatenation operatort that concatenates two records with right-preference towards fields in y. 

- Only "hard" rules are given with "easy" rules being skipped

## Legend
¿: Meta-symbol to mark something that is not undoubtetly correct or missing
¡: Meta-symbol to mark something that is wrong and needs fixing
- Can be bracketet with []

## Remarks
- ε verwende ich mehrmals für verschieden syntaktische Objekte
- {} wird auch für Syntax und Typen verwendet

## Terms & Types
Basetypes : 𝓫 ∈ 𝓑
Constants : c ∈ 𝓒
Labels    : ℓ ∈ 𝓛
Variables : x, y ∈ 𝓧
Typevars  : α, β, γ ∈ 𝓿

*Terms*
e := x | c | e₁e₂ | ς: e₂ | { e = e; } | ℓ | e₁ ‖ e₂ | let e₁ = e₂ in e₃
ς := { ξ } | { ξ, … }
ξ := ε | (x | ξ) | (x ? e | ξ)

*Types*
κ := ∗ | κ₁ -> κ₂ | Row | Lab | Pat | Unknown
σ := ∀α: κ. σ | τ
π := x ∈ (S₁ ∪ … ∪ Sₙ)
τ := 𝓫 | ★ | { p }± -> τ | ⦅l⦆ | { ρ }
ρ := ε | α | (l:τ | ρ)
p := ε | (l:τ | p) | (l: τ? | p)
± ∈ {+, -}

*Context*
Γ := • | Γ · (x: σ) | Γ · (α : κ)


## Kinding
*base*
----------- κ-base
Γ ⊢ b ∈ 𝓫: ∗

α: κ ∈ Γ
---------------- κ-var
Γ ⊢ α: κ

----------- κ-base-lab
Γ ⊢ ℓ: Lab

Γ ⊢ l: Lab
----------- κ-lab
Γ ⊢ ⦅l⦆: ∗

*rows & records*
------------ κ-row-empty
ε: Row

Γ ⊢ l: Lab   Γ ⊢ τ: ∗   Γ ⊢ ρ: Row
---------------------------------- κ-row
Γ ⊢ (l: τ | ρ): Row

Γ ⊢ ρ: Row
----------- κ-rec
Γ ⊢ {ρ}: ∗

*patterns*

------------ κ-pat-empty
ε: Pat

Γ ⊢ τ: ∗   Γ ⊢ l: Lab   Γ ⊢ p: Pat 
---------------------------------- κ-pat
Γ ⊢ (l: τ | p): Pat

Γ ⊢ τ: ∗   Γ ⊢ l: Lab   Γ ⊢ p: Pat
---------------------------------- κ-pat-default
Γ ⊢ (l: τ? | p): Pat

Γ ⊢ p: Pat
--------------------- κ-λ
Γ ⊢ {p}± -> τ: ∗


*misc* 
----------- κ-unknown
Γ ⊢ ★: Unknown


## Rewriting
{l₁ = a; {l₂ = b;}} ≙ {l₁ = a; l₂ = b;}
{ε} = {}       (syntax-records & type-records)
{ε}: e = {}: e

## Inference
*Basics*
x: σ ∈ Γ   Γ ⊢ σ ⊑ τ
--------------------- Var
Γ ⊢ x: τ

---------
Γ ⊢ ℓ: ⦅ℓ⦆

-----------
Γ ⊢ c: 𝓫_c

*Equivalences*
- TODO: choose one

Γ ⊢ e₁: τ₁  τ₂ ≤ τ₁ 
--------------------- Sub
Γ ⊢ e₁: τ₂


Γ ⊢ e₁: τ₁  τ₂ ≡ τ₁ 
--------------------- Eq
Γ ⊢ e₁: τ₂


*Records*
Γ ⊢ e₁: ⦅l⦆  e₂: τ  
-------------------- Rec-I
Γ ⊢ {e₁ = e₂}: {l: τ}


Γ ⊢ e₁: {ρ₁}   Γ ⊢ e₂: {ρ₂}
------------------------- Rec-Concat
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢e₂– e₁: τ ↝ Γ'
--------------------- Rec-Acc
Γ ⊢ e₁.e₂: τ


*Functions*
Γ,Δ ⊢ e: τ   ξ ↦ Δ
--------------------------- λ-I-open
Γ ⊢ ({ ξ, … }: e): {p}⁺ -> τ


Γ,Δ ⊢ e: τ   ξ ↦ Δ
--------------------------- λ-I-close
Γ ⊢ ({ ξ }: e): {p}⁻ -> τ

Γ ⊢ e ⧀ τ ≙ (Γ ⊢ e: τ' and τ' ⧀ τ)
Γ ⊢ τ ⧀ e ≙ (Γ ⊢ e: τ' and τ ⧀ τ')

Γ ⊢ e₁: { p }⁻ -> τ₂    e₂ ⧀ ⌊p⌋   Γ ⊢ ⌈p⌉ ⧀ e₂
------------------------------------------------ λ-E-1
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: { p }⁺ -> τ₂    e₂ ⧀ ⌊p⌋
------------------------------------------- λ-E-2
Γ ⊢ e₁e₂: τ₂

*Auxiliary pattern approximation*
- Only retain non-optional fields
⌊p⌋ :: Pat -> ∗
ε              = {}
(l: τ | p)     = { l: τ | ⌊p⌋ }
(l: τ? | p)    = ⌊p⌋

- Retain all fields
⌈p⌉ :: Pat -> ∗
ε              = {}
(l: τ | p)     = { l: τ | ⌈p⌉ }
(l: τ? | p)    = { l: τ | ⌈p⌉ }


*Let-Poly*
Γ e₂: ∀ᾱ: overline(κ). τ₁ ⊢ e₃ : τ₃     Γ ⊢ e₁: τ₁    ᾱ ∈ ftv(τ₁) \ ftv(Γ)
------------------------------------------------------------------------- Let
Γ ⊢ let e₁ = e₂ in e₃: τ₃


## Matching
> An auxiliary judgement that creates a new context Δ with all pattern variables

------ m-empty
ε ↦ •

ξ ↦ Δ'
-------------------------- m-pat
(x | ξ) ↦ Δ, [α: κ]¿, Δ'

- Using the default expressions type here is a deliberate decision
Γ ⊢ e: τ  ξ ↦ Δ'
-------------------------- m-default
(x ? e | ξ) ↦ Δ, x: τ, Δ'

## Instantation
> Instantiate type schemes `∀ᾱ: overline(κ). σ` when taking them out of the context

------ Inst-Refl
τ ⊑ τ


Γ ⊢ τ: ∗   Γ ⊢ σ[τ/α] ⊑  σ'
--------------------------- Inst-∗
Γ ⊢ ∀α: ∗. σ ⊑ σ'


(α ∉ ftv(Γ) or α ⊳ˡ σ)  Γ ⊢ σ[τ/α] ⊑ σ'
--------------------------------------- Inst-Lab
Γ ⊢  ∀α: Lab. σ ⊑ σ'



(α ∉ ftv(Γ) or α ⊳ʳ σ)  Γ ⊢ σ[τ/α] ⊑ σ'
--------------------------------------- Inst-Row
Γ ⊢  ∀α: Row. σ ⊑ σ'


## Tailcheck
> Make sure not to instantiate row and label variables when they could shadow existing fields
- TODO: we should use the subtype relation here

α ⊳ τ₁
--------- go-l
α ⊳ τ₁ τ₂


α ⊳ τ₂
--------- go-r
α ⊳ τ₁ τ₂


α ⊳ σ
------------ go-in
α ⊳ ∀β: κ. σ


------------ ⊳-Lab
α ⊳ˡ ⦅α⦆


ρ ≡ {ρ | α: τ}
--------------- ⊳-Lab-Rec
α ⊳ˡ {ρ}


ρ ≡ {ρ' | α}
------------- ⊳-Row
α ⊳ʳ {ρ}
