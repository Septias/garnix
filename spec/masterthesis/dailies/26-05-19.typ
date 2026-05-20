
./26-05-20.typ
== Fragen
- Sind Argumente bei den Funktionsaufrufen schon maximal präzise?
  - Nein: `a: let b = b: a + b; c = a + 1; ...`
  - a ist captured und wird danach noch verfeinert
  - Durch order nicht mehr confluent??
  - Wie würde man überhaupt `a: b: a + b` typen?
- Wie funktioniert subtyping mit rows?
  - Das ist halt schon einfach wieder nicht machbar
    - Oder erstelle ich dann einfach crazy viele constraints?
- Constraints liegen im Kontext und sind bounds für Variablen?
  - Ja
- Wie genau funktionieren Qualified Type Systems?
- Kann man Destructuring und Subtyping vereinen?
  - Sind halt unterschiedliche Arten von Constraints..
  - Mehrere Constraints zu mischen klingt mmn. sehr kompliziert
- An welchen Stellen kommen Typvariablen vor?
  - Let-poly
  - Funktionen (in ML)

== TODO
- Die Systeme von Castagna nochmal wirklich anschauen… ?
  - Oder ist untractability schon zu viel?
  - Aber anscheinend gibt es dazu ja Lösungen…
- Subtyping-Hirarchie versuchen zu erstellen?
  - Danach dann schauen, ob man constraints auflösen kann?


== TODO: Formalization
- Wie FC labels darstellen?
- Minimal calc braucht
  - Rewriting Rules für Records
  - Kinding
  - Muss man den Kontext noch mit Tyvars erweitern können?
  - ± verwenden für syntax der Funktionen?


== Record-calc-options
=== Structure
- Rows ⟨⟩
- Tails ς
- Row Variables ρ
- FC-Labels

=== Impl
- {} ∧ {}
- Boolean formulas
- Rows

=== Capabilities
- Subtyping
- Concatenation


== Von unten nach Oben
1. Records und Function Patterns
  - Inferenz basierend auf Unification?
  - [x] Probleme: Ich brauche schon subtyping?
    - Wäre wahrscheinlich nice - I guess
  - [x] Probleme: Wie genau werden die neuen Variablen assoziiert
2. FC-Labels hinzufügen
  - Neuer Type, Inferenz?
3. Let-Erweiterung
  - Vielleicht reicht dafür auch einfach schon eine Typregel?
4. Recursive-Erweiterung
  - Problem: Was genau sind die Implications?
  - Warum konnte Parreaux nicht die μ-Regel hinzufügen?
5. With-Statement
  - Wie interagieren die mit den Row-variablen?
6. Inherit


== Subtyping für Rows mit Vars
- Wir haben die Row x: { a: int, b: {c : str}} und y: { a: α, β@{γ} }
- Hier muss jetzt einfach α ≤ int oder α ≡ int


== Misc
- Feststellen, wie Typsysteme mit _qualified types_ funktionieren
- Im Endefekt sind die Typregeln wie ein großes Case-statement
- Vielleicht kann ich ja einen Algorithmus finden, der Funktionen und Record concat kann?
