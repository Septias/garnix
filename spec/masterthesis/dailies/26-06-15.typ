
./26-06-14.typ
./26-06-16.typ

== ∈-Solving
- [x] Muss eigentlich ne Substitution zurück geben, oder?
- [x] Man kann natürlich auch den Kontext füllen


== Matching
- We assume that we are allowed to reorder fields
- Does it happen on term or type level?
  - Happens on term level for the reduction
  - For the typesystem, only on types?
    - What kind of Substitution do I use then?
    - Only on types, right? But same mechanism…
- Brauche ich refinement auf den Variablen?
  - Ich nehm zum Beispiel ein Record mit feld {x}
  - Bekomme dann aber einen mit {x, y}
  - Der infereriert type ist eigentlich schon _vollständig_, oder?
  - Also muss ich auch nicht refinen, weil ich schon alles _habe_?

(⦃ l: τ₁   | p ⦄, ⟨ l: τ₂ | ρ ⟩) =
(⦃ l: τ?τ  | p ⦄, ⟨ l: t | ρ ⟩) = false
