
== Fäden
- Wie würde Subtyping und type connectives funktionieren?
- Beispiel für negative Typen


== Dem Big ones
- Qualified Types oder Unification oder Bi-unification?
- Row Poly or Subtyping Poly?
- Row variablen?
- Vielleicht doch direkt den unknown type rein?
  - Trotz Komplexität?


== TODO: Formalization
- Wie FC labels darstellen?
- Minimal calc braucht
  - Rewriting Rules für Records
  - Kinding
  - Muss man den Kontext noch mit Tyvars erweitern können?
  - ± verwenden für syntax der Funktionen?


== Ansätze
- Alles in einer Row sammeln und dann von hinten vorgehen.
- Sollte es unpräzise werden einfach ★ zurückgeben
- Man kann dann um Probleme zu lösen noch diese Gruppenkonstraints machen x ∈ (A ∪ B)
