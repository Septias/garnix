
./26-06-15.typ

== Fäden
- [x] FC-Regeln aufschreiben
- [x] To handle fc labels in unification, we have to… ?
- [s] Generic Access function types aufschreiben?
- Wie kann man _overwrite or extend_ in tabular data hinzufügen?
  - We do not have _lacks-statements_
- Warum genau lacks-statements?
  - Weil wir damit generisch sagen können, dass etwas nicht da ist?

== FC-Labels
l ∈ 𝓛

κ := ∗ | Lab
e := e.\${e} | x: e | e₁e₂
τ := 𝓫 | ⦅l⦆ | →
σ := ∀ᾱ: ol(κ). τ | τ


e₁: ⦅l⦆  e₂: { l: τ }
--------------------- Proj
e₁e₂: τ

Proj-function:
$.l: ∀α. α :: ⟨l: τ⟩ => α -> τ$

Γ ⊢ l: Lab
----------------- ★-l
⦅l⦆: ∗


=== Unification von FC-Labels
1. HM(𝓡): Predikate, die gelöst werden
2. Tabular: Unification vars
3. ROSE: Prädikate (only)
4. Castagna: Semantische Typen


== Subtype Hirarchy
l₁ = l₂    τ₁ ≤ τ₂   l ∉ ρ₁   l ∉ ρ₂
-----------------------------------
{ρ₁ | l₁: τ₁} ≤ { ρ₂ | l₂: τ₂ }


== Function Destructuring Rules
Γ ⊢ e₁: ⦃ p ⦄ -> τ₂    e₂ ⧀ ⌊p⌋   ⌊p⌋ ⧀ e₂
--------------------------------------
Γ ⊢ e₁e₂

- For a closed pattern, fields have to fit _exactly_


Γ ⊢ e₁: ⦃ p, ... ⦄ -> τ₂     e₁ ⧀  ⌊p⌋
---------------------------------------
Γ ⊢ e₁e₂

- For an open pattern, fields only have to be subtype of the _required fields_

== Matching Rules

ρ₁ ⧀  ρ₂
---------------------------------
{ρ₁} ⧀  {ρ₂}


l₁ = l₂    τ₁ < τ₂    ρ₁ ⧀ ρ₂
---------------------------------
⟨l₁: τ₁ | ρ₁⟩ ⧀ ⟨ l₂: τ₂ | ρ₂⟩


[α = l]¡   τ₁ < τ₂    ρ₁ ⧀ ρ₂
---------------------------------
⟨α: τ₁ | ρ₁⟩ ⧀ ⟨ l: τ₂ | ρ₂⟩


??
--------------------
⟨ l₁: τ₁ | ρ₁⟩ ⧀ α


??
--------------------
α ⧀ ⟨ l₁: τ₁ | ρ₁⟩



== ∈-constraints
- Welche Syntax?
  - Die werden angewendet in:
    - Subtyping
    - Lookups
  - Dann eine Formularisierung in:
    - NF-Rules: nicht gut, wenn wir ★ zurück geben wollen
    - Algorithmic?
    - Mit haken `bag(ρ) -> τ`
    - Als substitution?
      - `bag(ρ) ~ p -> σ` returns a substitution or ★
      - Oder da halt die ∈-constraints?
