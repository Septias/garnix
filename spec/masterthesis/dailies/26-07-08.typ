
./26-07-07.typ
== Fragen
- Fm: Wie genau speicher ich die Constraints?
  - opt1: Im selben context
  - opt2: Neuer context
  - opt3: Splitted context (mit extrasymbol?)
- Subtstitution für Row-Vars über unification, nh?
- Instantation muss noch geklärt werden
- (davor aber deklarative Beweise fertig imo.)
- Irgendwann könnte man sich noch überlegen, lacks-predikate hinzuzufügen


== Subtyping
- I need an opportunistic case for when I compare with row variables


------ SR-Refl
τ ≤ τ


τ₁ ≤ τ₂  τ₂ ≤ τ₃
----------------- SR-Trans
τ₁ ≤ τ₃


τ₁ ≤ τ₂  ρ₁ <= ρ₂
------------------ SR-Depth
(l : τ₁ | ρ₁) ≤ (l: τ₂ | ρ₂)


l ∉ dom(ρ₂)
------------------ SR-Width
(l : τ₁ | ρ₁) ≤ ρ₂


-------------------------------------------- SR-Assoc
{l₁: τ₁, l₂: τ₂ | ρ} ≤ {l₂: τ₂, l₁: τ₁ | ρ }



== Lookup
`Γ ⊢ ρ.l: τ → S`
We add a lookup relation on record-types that simultaneously looks up a type and helps in creating an ∈-constraint.
- It trivially traverses the type and acts upon these structs:
  - The lookup-label: Return a type τ
  - A type-variable: Return a type ★
    - And also collect _all_ row variables of the record in S
- Does not work for FC-labels yet
  - Problem: We can not compare (α: τ) to anything
  - This means we also just have to return ★ in this case


Γ ⊢ ρ.l: τ → S   Γ ⊢ e: {ρ}
--------------------------- T-sel-ok
Γ ⊢ e.l: τ
- If type ok, discard S



Γ ⊢ ρ.l: ★ → S  Γ ⊢ e: {ρ}
--------------------------- T-look-★
Γ, (l ∈ S) ⊢ e.l: ★
- If type unknown, add ∈-constraint


l₁ = l₂
----------------------------- T-look-hit
Γ ⊢ ( l₁: τ | ρ ).l₂ : τ → ∅
- Type found, just return it


l₁ ≠ l₂      Γ ⊢ ρ.l: τ → S
----------------------------- T-look-rec
Γ ⊢ ( l₁: τ | ρ ).l₂: τ → S


l₁ ≠ l₂
----------------------------- ↯
Γ ⊢ ( l₁: τ | ε ).l₂: τ → Ξ
- This rule is not added, but shows when lookup ends fatally


Γ ⊢ ρ.l: ★ → S
----------------------------- T-look-rowvar
Γ ⊢ ( α | ρ ).l: ★ → α ∪ S
- When there is a row-variable, the returned type becomes ★


Γ ⊢ ρ.l: ★ → S
--------------------------- T-look-collect
Γ ⊢ ( l: τ | ρ ).l: ★ → S


--------------- T-look-end
Γ ⊢ ε.l: ★ → ∅
