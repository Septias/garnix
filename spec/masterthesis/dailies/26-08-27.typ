
./26-08-28.typ
== Fragen
- Möchte ich die L-α rule raus nehmen?
  - Eigentlich möchte ich das Armutsmerkmal schon eliminieren…
  - Und im deklarativen System hat sie wirklich nichts zu suchen…


== Claude Prompts
- [ ] Isn't the Discharge rule too permissive because it turns a bottom-lookup into ★?


================================================
Ausschnitt aus `./minimal.typ`
------------------------------------------------
// I think this blur is fine?
Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ?
-------------------------- T-sel-★
Γ ⊢ e.l: ★


// Allows to not error on lazy errors
Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ⊥
-------------------------- T-sel-⊥
Γ ⊢ e.l: ★


Γ ⊢ e: τ
--------- T-★-intro
Γ ⊢ e: ★
===============================================


== Misc
- Patterns sind (noch) nich drin
- Two-directional inversion: Showing equality by profing both directions
