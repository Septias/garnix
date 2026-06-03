
== Todo
- Wie werden rows in Nix eingeführt?
  - Durch getAttr functions?
  - Durch with?
  - Generell durch Inference?
- Wie genau ist die polare restriktion???????

== Fäden
- Fc-label Regeln aufschreiben


== Idee
- Vielleicht kann man durch die introspectiv-möglichkeiten auch den Merge-operator verwenden??


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

```nix
a: b: (a // b).c
```

- Hier sind a und b rows? Ne eigentlich sind das nur Records. Aber abstrakte Records _müssen_ rows haben?
