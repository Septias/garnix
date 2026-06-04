
== Todo
- Wie werden rows in Nix eingeführt?
  - Durch getAttr functions? - Nicht unbedingt
  - Durch with? - Nope
  - Generell durch Inference? - Brauhct dann trotzdem keine syntaktischen Rows


== Fäden
- Fc-Label Regeln aufschreiben
- Generic access function types aufschreiben?


== Ansätze
- Alles in einer Row sammeln und dann von hinten vorgehen.
- Sollte es unpräzise werden einfach ★ zurückgeben
- Man kann dann um Probleme zu lösen noch diese Gruppenkonstraints machen x ∈ (A ∪ B)
- Alles nur _opportunistisch_


Γ ⊢ a: A  l: ⦅l⦆
----------------- Prod-I
Γ ⊢ {l = a}: ⟨l: a⟩


Γ ⊢ a: A  l: ⦅l⦆
----------------- Row-I
Γ ⊢ {l = a}: ⟨l: a⟩


-- Rewriting
{l₁ = a; {l₂ = b;}} ≡ {l₁ = a; l₂ = b;}


== Set-based Elements

- Wir sagen ein Element muss über zwei Rows existieren. Ein Beispiel ist
- Hier sind a und b rows? Ne eigentlich sind das nur Records. Aber abstrakte Records _müssen_ rows haben?

```nix
a: b: (a // b).c
```

