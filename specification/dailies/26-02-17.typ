

== Topics

- Full record calculus with type connectives?
- Record extension + fc labels?
- With construct typing alg?
- Occurrence typing for ifs?
- polymorphic records with fc labels?
- parreaux records w/

== Function typing rules


Γ, x: τ_1 ⊢ e : τ_2
----------------------- T-Abs1
Γ ⊢ (x: e) : τ_1 → τ_2



Γ, overline(e_i : τ_i) ⊢ e: τ_2
----------------------------------- T-Abs2
Γ ⊢ ({oa(α)}: e) : {α}^- → τ_2



Γ, overline(e_i : τ_i) ⊢ e: τ_2
----------------------------------- T-Abs3
Γ ⊢ ({oa(α),...}: e) : {α}^+ → τ_2



Γ ⊢ e_1: τ_1 → τ_2   Γ ⊢ e2: τ_3 ≤ τ_1
----------------------------------------- T-App1
Γ ⊢ (x: e_1) e_2: τ_2



Γ ⊢ e_1: {overline(α)^-} → τ_2   Γ ⊢ e2: τ_3 ≤ τ_1
-------------------------------------------------- T-App2
Γ ⊢ (x: e_1) e_2: τ_2



Γ ⊢ e_1: {overline(α)^+} → τ_2 Γ ⊢ e_2: τ_1
-------------------------------------------------- T-App3
Γ ⊢ (x: e_1) e_2: τ_2
