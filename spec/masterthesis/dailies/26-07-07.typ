
./26-07-08.typ
== Fäden
- Add Row-based lookup derivation
- Add Subtyping-relation
- Example derivation to see constraint propagation?

== Formalisierung
- Handle the "maybe case" for subtyping in applications
- Subtyping in Application


== Todo
- Problem: ★ should not be an error type



== Abstraction
[x ∈ e₂]¡    Γ ⊢ (x: e₁)e₂: τ
--------------------------- T-λ∈-ok
Γ · (x ∈ S) ⊢ (x: e₁)e₂: τ


[x ∉ e₂]¡    S' = S∖x    Γ · (x ∈ S') ⊢ (x: e₁)e₂: τ
----------------------------------------------------- T-λ∈-not-in
Γ · (x ∈ S) ⊢ (x: e₁)e₂: τ


== Beispiel-Derivations

Example: Unkown due to subtyping
e = `b: (a: a.l)b` :: ★ -> ★
–––––––––––––––––––––––––––––––––––––––––––––––––––––
e₁: τ₁ -> τ₂ ∈ Γ                   b ≤ { l: τ } -> ★  [τ₁ = {}]
---------------     -----------    -----------------------------
Γ ⊢ e₁: τ₁ -> τ₂     Γ ⊢ b: τ₃      τ₃ ≤ τ₁
----------------------------------------------- λ-E-★
Γ ⊢ e₁b: ★ -> ★
---------------
Γ ⊢ e: ★ -> ★
–––––––––––––––––––––––––––––––––––––––––––––––––––––
- Sollte hier nicht ein constraint entstehen?
  - Also: l ∈ b?
- Die Funktion `a: a.l` müsste eigentlich schon mit dem constraint `l ∈ a` im Kontext liegen
- D.h. wir schieben den Constraint einfach auf?
- Dafür müssten wir dann einen check in der λ-I rule adden?
- Was machen wir aus dem Subtyping Ergebnis?


Example: Introduction of ∈-constraints
e = (x: x.l)
–––––––––––––––––––––––––––––––––––––––––––––––––––––
Γ, l ∈ x ⊢ x: {ρ}  Γ ⊢ ρ.l: ★
------------------------------
Γ, l ∈ x, x: τ ⊢ (x.l): τ₂   [τ₂ = ★]
-------------------------------------- λ-I
Γ, l ∈ e ⊢ (x: x.l): ★ -> ★
–––––––––––––––––––––––––––––––––––––––––––––––––––––
- TODO: How to decide when to extend ★?



Example: Refinement
e = `(x: x.l){l: τ}` ::
–––––––––––––––––––––––––––––––––––––––––––––––––––––
e₁: τ₁ -> τ₂ ∈ Γ
---------------                  -----------    ------
Γ ⊢ e₁: r <= {l: τ} => r -> α    Γ ⊢ b: τ₃      τ₃ ≤ τ₁
-------------------------------------------------------- λ-E-★
Γ ⊢ e₁{}: τ
-----------
Γ ⊢ e: τ
–––––––––––––––––––––––––––––––––––––––––––––––––––––


