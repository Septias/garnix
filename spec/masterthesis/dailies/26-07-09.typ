
./26-07-08.typ
./26-07-10.typ

== Fragen
- Instantation muss noch geklärt werden (mit let-poly dann)


== Fäden
- Brainstorm Constraintsolving
  - Do I even want to do constraint solving outside of function instantiation?
- Extrakontext hinzufügen
- Subtyping mit lookup-relation
- Fix lookup relation (fixed labels should occur?)
  - Soll ich einfach die ganzen Records rein geben?
    - I mean why not, muss man halt bissel trash raus schmeißen


== Advanced Topics
- Aufschieben von Constraints
- Type Refinements based on T-App
- Constraints during Subtyping
- Subtyping und ★
- Unification


== Misc
- Claude hat gechoked und ein einziger default-type funktioniert nicht


== Subtyping
- I need an opportunistic case for when I compare with row variables
- Should I add subtyping constraints?
  - Not yet I reckon

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
- over-constrains the shadowing-with-concrete-fallback case


Γ ⊢ ρ.l: τ   Γ ⊢ e: {ρ}  τ ≠ ★
----------------------------------- T-sel-τ
Γ ⊢ e.l: τ


Γ ⊢ ρ.l: ★  Γ ⊢ e: {ρ}  rows(ρ) = S
------------------------------------ T-sel-★
Γ, (l ∈ S) ⊢ e.l: ★
- If type unknown, add ∈-constraint


l₁ = l₂
----------------------------- T-look-hit
Γ ⊢ ( l₁: τ | ρ ).l₂ : τ


l₁ ≠ l₂      Γ ⊢ ρ.l₂: τ₂
----------------------------- T-look-rec
Γ ⊢ ( l₁: τ₁ | ρ ).l₂: τ₂


l₁ ≠ l₂
----------------------------- ↯
Γ ⊢ ( l₁: τ | ε ).l₂: τ
- This rule is not added, but shows when lookup ends fatally


----------------------------- T-look-rowvar
Γ ⊢ ( α | ρ ).l: ★



== Row-var collection
`rows(ρ) = S`
Auxiliary judgment that collects all row-variables of a row.

------------ rows-end
rows(ε) = ∅


rows(ρ) = S
--------------------- rows-skip
rows(l: τ | ρ) = S


rows(ρ) = S
-------------------------- rows-collect
rows(α | ρ) = {α} ∪ S
