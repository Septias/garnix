
Example: Unkown due to subtyping
e = `b: (a: a.l)b` :: ★ -> ★
–––––––––––––––––––––––––––––––––––––––––––––––––––––
e₁: τ₁ -> τ₂ ∈ Γ                   b ≤ { l: τ }
---------------     -----------    ------------------
Γ ⊢ e₁: τ₁ -> τ₂     Γ ⊢ b: τ₃      τ₃ ≤ τ₁
----------------------------------------------- λ-E-★
Γ ⊢ e₁b: ★ -> ★
---------------
Γ ⊢ e: ★ -> ★
–––––––––––––––––––––––––––––––––––––––––––––––––––––
- Ist die Frage, ob ich hier subtyping constraints mache oder nicht?
- FALSCH: Eigentlich füge ich hier doch einfach constraint b <= {l: τ} hinzu?
  - Oder halt einfach der Constraint (l ∈ b)?
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
e = `(x: x.l){l: τ}`
––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
…
--------------
e₁: τ₁ -> τ₂
-----------------                 ---------     --------
Γ ⊢ e₁: [r <= {l: τ} => r -> τ]¡  Γ ⊢ b: τ₃      τ₃ ≤ τ₁
-------------------------------------------------------- λ-E-★
Γ ⊢ e₁{}: τ
-----------
Γ ⊢ e: τ
––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––


