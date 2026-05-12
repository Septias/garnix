= Brainstorming
- Features:
  - Pattern destructuring
  - FC-Labels
  - Asymmetric concat
  - With-construct
  - Recursive types
  - Inference
- Relations
  - subtyping: ≤
  - containment: ≤
  - destructuring: ⊙


== Records
- Mehrere row-variablen?
- Einfach alle sammeln mit rechts-Präzedenz?
- Was ist mit with-statements?
- Dafür eigener Kontext?


=== Minimal
- Record Concat
- Function destructuring
- Inference
- Recursiveness


```nix
let
  arg = {a = 2; c = 3;};
  fun =  {a, ..}: a;
in (fun arg)
```

== Vorgehen: Typ-Algorithmus
1. Wir lesen den Code und erstellen ein Parsetree
  - Debjrujin?
2. Jede Funktion bekommt für ihre Argumente _Typvariablen_
3. Die Nutzung dieser Typvariablen wird im Funktionskörper analysiert
4. Anhand der Nutzung stellen wir dann fest:
  - Welche Felder hat diese Variable
  - Andere Typinformationen
5. Diese Constraints werden gesammelt
  - Auf den Variablen?
  - In einem Context?
6. Und dann gelöst durch
  - Unification?
  - Bi-unification?


== Abstriche
- Impurities werden ausgelassen (später kann man auf gradual erweitern?)

== Fragen
- Was für Constraints und wie kann ich die lösen und inferieren?
- Kann man destructuring und subtyping vereinen?


== Schwierigkeiten
- Dadurch, dass es Typvariablen gibt, können in Rows Unklarheiten entstehen


== Record Model
- Rows?
- Type connectives?
  - Restricted?


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


== Misc
- Local type inference mit Abstrichen?
  - Overapproximation of types?
  - Thats a possible way to calculate best types w/o backtracking
  - Bot-type ist schwierig (generell)


