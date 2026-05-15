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
  - Ich glaube nein!


== Schwierigkeiten
- Dadurch, dass es Typvariablen gibt, können in Rows Unklarheiten entstehen


== Vorgehen
1. Mehr Beispiele finden, die ich typen möchte
2. Algorithmisch ausprobieren, wie die getypt werden können
3. Davon ausgehend Entscheidungen über die Struktur des Typsystems herleiten

- Feststellen, wie Typsysteme mit _qualified types_ funktionieren
- Im Endefekt sind die Typregeln wie ein großes Case-statement
- Vielleicht kann ich ja einen Algorithmus finden, der Funktionen und Record concat kann?


== Misc
- Local type inference mit Abstrichen?
  - Overapproximation of types?
  - Thats a possible way to calculate best types w/o backtracking
  - Bot-type ist schwierig (generell)


