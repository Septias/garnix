
== Topics
- Full record calculus with type connectives?
- Record extension + fc labels?
- With construct typing alg?
- Occurrence typing for ifs?
- Polymorphic records with fc labels?


== With & Inherit

Γ ⊢ t₂ ≤ {}    Γ, Ξ · t₂ ⊢ t₂ : τ
----------------------------------- T-WITH
Γ ⊢ with t₁; t₂ : τ


x ∈ Γ
----------------------------------- R-Inherit1
Γ ⊢ { inherit x; } -> { x = Γ(x);}



x ∈ Γ
----------------------------------- R-Inherit2
Γ ⊢ { inherit (ρ) x; } -> { x = lookup(ρ, x)}


lookup([], x) = Γ(x)
lookup(a:t, x) = lookup()


== Typing Rules Occurrence typing

Γ ⊢ t: true => Ξ    Γ, Ξ ⊢ t_1 : τ
----------------------------------- T-Cond
Γ ⊢ if t_1 then t_2 else t_3: τ



----------------------------------- T-Bool
Γ, Ξ  ⊢ isBool(τ) => Ξ · (τ: bool)



----------------------------------- T-Num
Γ, Ξ  ⊢ isNum(t) => Ξ · (t: num)



----------------------------------- T-And
Γ, Ξ  ⊢ t_1 && t_2 => Ξ · (t: num)



----------------------------------- T-Not
Γ, Ξ  ⊢ !t_2 => Ξ · (t: ¬Ξ(τ))
