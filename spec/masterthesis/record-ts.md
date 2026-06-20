# Formalisierung
> A typesystem with records and function patterns. We use scoped rows with row and label variables which can create ambiguous scenarious. An unkown type ★ is used when the typesystem can not derive sound types. ∈-Solving is a novel algorithm that tries to solve ∈-constraints of the form `a ∈ (A ∪ B)`` where a is a field and A,B are records. A motivating example is: `x: y: (x ‖ y).a` where x and y are variables and ‖ is the concat operator that concatenates two records with right-preference towards fields in y. 

- Only "hard" rules are given with "easy" rules being skipped

## Legend
¿: Meta-symbol to mark something that is not undoubtetly correct or missing
¡: Meta-symbol to mark something that is wrong and needs fixing
- Can be bracketet with []

## Remarks
- ε verwende ich mehrmals für verschieden syntaktische Objekte
- {} wird auch für syntax und typen verwendet

## Terms & Types
Basetypes : 𝓫 ∈ 𝓑
Labels    : ℓ ∈ 𝓛
Variables : x, y ∈ 𝓧
Typevars  : α, β, γ ∈ 𝓿

*Terms*
e := x | e₁e₂  | ς: e₂ | { e = e; } | e₁ ‖ e₂ | let e₁ = e₂ in e₃
ς := { ξ } | { ξ, … }
ξ := ε | (x | ξ) | (x ? e | ξ)
l := α | ℓ

*Types*
κ := ∗ | κ₁ -> κ₂ | Row | Lab | Pat | Unknown
σ := ∀α: κ. σ | τ
τ := 𝓫 | ★ | { p }± -> τ | ⦅l⦆ | { ρ }
ρ := ε | α | (l:τ | ρ)
p := ε | (l:τ | p) | (l: τ?τ | p)
± ∈ {+, -}

*Context*
Γ := • | Γ · (x: τ) | Γ · (α : κ)


## Kinding
*base*
----------- κ-base
Γ ⊢ b ∈ 𝓫: ∗

α: σ ∈ Γ  σ ⊑ τ
----------- κ-var
Γ ⊢ α: τ

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

------------ κ-pat-empty¡
ε: Pat

Γ ⊢ τ: ∗  Γ ⊢ l: Lab Γ ⊢ p: Pat 
-------------------------------- κ-pat
Γ ⊢ (l: τ | p): Pat

Γ ⊢ τ: ∗  Γ ⊢ l: Lab Γ ⊢ p: Pat
-------------------------------- κ-pat-default
Γ ⊢ (l: τ ? τ | p): Pat

Γ ⊢ p: Pat
--------------------- κ-λ
Γ ⊢ {p}± -> τ: ∗


*misc* 
----------- κ-unknown
Γ ⊢ ★: Unknown


## Rewriting
{l₁ = a; {l₂ = b;}} ≙ {l₁ = a; l₂ = b;}
{ε} = {}       (syntax-recors & type-records)
{ε}: e = {}: e

## Inference
*Basics*
x: σ ∈ Γ   Γ ⊢ σ ⊑ τ
--------------------- Var
Γ ⊢ x: τ

--------
Γ ⊢ ℓ: ⦅ℓ⦆

*Equivalences*
- TODO: choose one

Γ ⊢ e₁: τ₁  τ₂ ≤ τ₁ 
--------------------- Sub
Γ ⊢ e₁: τ₂


Γ ⊢ e₁: τ₁  τ₂ ≡ τ₁ 
-------------------- Eq
Γ ⊢ e₁: τ₂


*Records*
Γ ⊢ a: ⦅l⦆ b: τ  
----------------- Rec-I
Γ ⊢ {a = b}: {l: τ}


Γ ⊢ a: {ρ₁}   Γ ⊢ b: {ρ₂}
--------------------- Rec-Concat
Γ ⊢ a ‖ b: {ρ₁ | ρ₂}


Γ · [(b ∈ a) ⊨ˡ a: τ]¿
--------------------- Rec-Acc
Γ ⊢ a.b: τ


*Functions*
Γ,Δ ⊢ e: τ   ξ ↦ Δ
--------------------------- λ-I-open
Γ ⊢ ({ ξ }: e): {p}⁺ -> τ


Γ,Δ ⊢ e: τ   ξ ↦ Δ
--------------------------- λ-I-close
Γ ⊢ ({ξ,…}: e): {p}⁻ -> τ

e ⧀ τ ≙ (Γ ⊢ e: τ' and τ' ⧀ τ)
τ ⧀ e ≙ (Γ ⊢ e: τ' and τ ⧀ τ')


Γ ⊢ e₁: { p }⁻ -> τ₂    e₂ ⧀ ⌊p⌋   ⌈p⌉ ⧀ e₂
------------------------------------------- λ-E-1
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: { p }⁺ -> τ₂    e₂ ⧀ ⌊p⌋
------------------------------------------- λ-E-2
Γ ⊢ e₁e₂: τ₂

*Auxiliary pattern approximation*
- Only retain non-optional fields
⌊p⌋ :: Pat -> ∗
ε              = {}
(l: τ | p)     = { l: τ | ⌊p⌋ }
(l: τ ? τ | p) = ⌊p⌋

- Retain all fields
⌈p⌉ :: Pat -> ∗
ε              = {}
(l: τ | p)     = { l: τ | ⌈p⌉ }
(l: τ ? τ | p) = { l: τ | ⌈p⌉ }


*Let-Poly*
Γ x: ∀ᾱ: overline(κ). τ₁ ⊢ e₂ : τ₂     Γ ⊢ e₁: τ₁    ᾱ ∉ ftv(Γ)
-------------------------------------------------- Let
Γ ⊢ let x = e₁ in e₂: τ₂


## Matching
> An auxiliary judjement that creates a new context Δ with all pattern variables

------ m-empty
ε ↦ •

Γ ⊢ x: τ   ξ ↦ Δ'
-------------------------- m-pat
(x | ξ) ↦ Δ, x: τ, Δ'

- Using the default expressions type here is a deliberate decision
Γ ⊢ d: τ  ξ ↦ Δ'
-------------------------- m-default
(x ? d | ξ)  ↦ Δ, x: τ, Δ'

## Instantation
> Instantiate type schemes `∀ᾱ: overline(κ). σ` when taking them out of the context

------ Inst-Refl
τ ⊑ τ


Γ ⊢ τ: ∗   Γ ⊢ σ[τ/α] ⊑  σ'
--------------------------- Inst-∗
Γ ⊢ ∀α: ∗. σ ⊑ σ'


(α ∉ ftv(Γ) or  α ⊳ˡ σ)  Γ ⊢ σ[τ/α] ⊑ σ'
-------------------------------------- Inst-Lab
Γ ⊢  ∀α: Lab. σ ⊑ σ'



(α ∉ ftv(Γ) or  α ⊳ʳ σ)  Γ ⊢ σ[τ/α] ⊑ σ'
-------------------------------------- Inst-Row
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



## Subtyping
- TODO: das müsste von hinten sein?
- TODO: ≤ nicht definiert

l₁ = l₂    τ₁ ≤ τ₂    ρ₁ ⧀ ρ₂
-------------------------------- - 
{l₁: τ₁ | ρ₁} ⧀ { l₂: τ₂ | ρ₂}


- Für label variablen:
[α = l]¡   τ₁ ≤ τ₂    ρ₁ ⧀ ρ₂
--------------------------------- -
{α: τ₁ | ρ₁} ⧀ { l: τ₂ | ρ₂}


¿
--------------------
{ l₁: τ₁ | ρ₁} ⧀ α


¿
--------------------
α ⧀ { l₁: τ₁ | ρ₁}



## ∈-Solving
> Tries to solve element constraints for rows that can have (multiple) row and label variables

Discharged by (b ∈ a) ⊨ˡ a: τ

A => B
where A is a tuple of (row, query-label) and B can be one of:
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
