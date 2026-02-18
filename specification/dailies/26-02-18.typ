
== Topics
- Full record calculus with type connectives?
- Record extension + fc labels?
- With construct typing alg?
- Occurrence typing for ifs?
- polymorphic records with fc labels?
- parreaux records w/


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



