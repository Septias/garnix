> Schaffen wir es ein Typsystem zwischen "Tabular Data" und "Rose" zu finden?

= Brainstorming
- Features:
  - Pattern Destructuring
  - FC-Labels
  - Asymmetric Concat
  - With-construct
  - Recursive types
  - Inference


== Records
- Mehrere row-variablen? Normalisieren?
  - Einfach alle sammeln mit rechts-Präzedenz?
  - Brauchen wir evtl. doch nicht, weil wir ja kein remove haben?
- Können wir nominale identities für Nix besorgen? (classes)

== Minimal
- Record Concat
- Function destructuring
- Inference


== Abstriche
- Impurities werden ausgelassen (später kann man auf Gradual erweitern?)


== Fragen
- Was für Constraints und wie kann ich die lösen und inferieren?
- Kann man destructuring und subtyping vereinen?


== Schwierigkeiten
- Dadurch, dass es Typvariablen gibt, können in Rows Unklarheiten entstehen


== Vorgehen
1. Mehr Beispiele finden, die ich typen möchte
2. Algorithmisch ausprobieren, wie die getypt werden können
3. Daran ausgehend Entscheidungen über die Struktur des Typsystems herleiten

- Feststellen, wie Typsysteme mit qualified types funktionieren
- Im Endefekt sind die Typregeln wie ein großes Case-statement
- Vielleicht kann ich ja einen Algorithmus finden, der Funktionen und Record concat kann?


== Read
- A record calculus based on symmetric concatenation
  - Hilfreich weil: Übersicht records, quantified
- Local Type inference
  - Hilfreich weil: Locality, Abstraction, Records, Further Reading
- Subtyping recursive types
  - Hilfreich weil: Wir auch rekursive Typen haben
- Tabular Data
  - Hilfreich weil: Ist fast das, was ich möchte


== Misc
- Local type inference mit Abstrichen?
  - Overapproximation of types?
  - Thats a possible way to calculate best types w/o backtracking
  - Bot-type ist schwierig (generell)


