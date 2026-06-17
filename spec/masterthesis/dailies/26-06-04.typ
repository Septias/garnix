
./26-06-03.typ
./26-06-08.typ
== Todo
- Wie werden rows in Nix eingeführt?
  - Durch getAttr functions?
    - Nicht unbedingt
  - Durch with?
    - Nope
  - Generell durch Inference?
    - Braucht dann trotzdem keine syntaktischen Rows

== Ansätze
- Alles in einer Row sammeln und dann von hinten vorgehen
- Sollte es unpräzise werden einfach ★ zurückgeben
- Man kann dann um Probleme zu lösen noch diese Gruppenkonstraints machen x ∈ (A ∪ B)
- Alles nur _opportunistisch_


== Set-based Elements

- Wir sagen ein Element muss über zwei Rows existieren.
- Hier sind a und b rows? Ne eigentlich sind das nur Records.

```nix
a: b: (a // b).c
```

