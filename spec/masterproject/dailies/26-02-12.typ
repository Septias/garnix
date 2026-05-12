

== With section
How should weak binding of with be handled?
- New context to store for with
- New var rule to extract (just for shadowing)

T-WITH
Γ, Ξ ⊢ a : {α}  Γ, Ξ · α ⊢ b: τ
------------------------------
Γ, Ξ  ⊢ #b[with] a; b : τ


T-VAR1
Γ(x) = x
------------------
Γ ⊢ x: τ


T-VAR2
x ∉ Γ    Ξ(x) = τ
------------------
Γ ⊢ x: τ


== Dunder Typing


{ __overrides = { };  } τ
---------------------------
{ __overrides = {a = 2} }: τ


{ __overrides = { };  } τ
---------------------------
{ __overrides = {a = 2} }: τ


- `__toString` is only

== Occurrence

First iteration:

Γ, x: Γ(x) ∧ bool ⊢ t₁ : τ   Γ, x: Γ(x) ∧ (¬bool) ⊢ t₂ : τ
----------------------------------------------------------
if isBool(t) then t₁ else t₂ : τ


Problem: This approach does not scale so well, maybe we can do better?

`if !isBool(t) then t₁ else t₂ : τ`

Basically we need to solve boolean formulars for this.
