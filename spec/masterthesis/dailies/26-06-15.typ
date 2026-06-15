
./26-06-14.typ

== Fragen
- Can we create a typesystem that "tries to make it work"?
- Unification rules aufschreiben
- Wie genau ⧀ auf rows / records?
- Mit der ∈-inferenz-regel mache ich halt jetzt unification _während_ Inferenz


== ∈-Introduction
Γ ⊢ a: τ₁   Γ ⊢ b: τ₂
---------------------
Γ ⊢ a ‖ b: ⟨τ₁ | τ₂⟩


Γ · (b ∈ a) ⊨ τ
---------------
Γ ⊢ a.b: τ


== ∈-Solving
- [x] Muss eigentlich ne Substitution zurück geben, oder?
- [x] Man kann natürlich auch den Kontext füllen

A => B
where A is a tuple of (row, query-label)
and   B can be one of:
- (ρ, b) : To recurse
- τ      : The type of of query-label
- ★      : An unkown type due to missing information
- α ! b  : A new constraint "Label variable α has to be b"
- a !! b : A new constraint "Row variable α has to contain b"

ρ̃: A row that has _label_ or _row variables_

--------------------------------------------------
(recurse) – (⟨ρ | l: τ⟩, q) => (ρ, q)   if  q ≠ l
(solved ) – (⟨ρ | l: τ⟩, q) => τ        if  q = l

(var-lab) – (⟨ε | α: τ⟩, q) => Γ · q ∈ α ⊨ τ
(–––––––) – (⟨ρ̃ | α: τ⟩, q) => ★

(var-row) – (⟨ε | α⟩,    q) => q !! α
(–––––––) – (⟨ρ̃ | α⟩,    q) => ★
--------------------------------------------------


== Matching
- We assume that we are allowed to reorder fields
- Does it happen on term or type level?
  - Happens on term level for the reduction
  - For the typesystem, only on types?
    - What kind of substitution do I use then?
    - Only on types, right? But same mechanism…
- Brauche ich refinement auf den variablen?
  - Ich nehm zum Beispiel ein Record mit feld {x}
  - Bekomme dann aber einen mit {x, y}
  - Der infereriert type ist eigentlich schon _vollständig_, oder?
  - Also muss ich auch nicht refinen, weil ich schon alles _habe_?

(⦃ l: τ₁   | p ⦄, ⟨ l: τ₂ | ρ ⟩) =
(⦃ l: τ?τ  | p ⦄, ⟨ l: t | ρ ⟩) = false
