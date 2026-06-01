
= ZIEL
Mein Ziel ist es, ein Record calculus zu erstellen, der Nix so gut wie möglich abdeckt. Dabei können auch Abstriche gemacht werden, aber er sollte zumindest irgendwie sound sein. Darauf kann dann einen Implementation oder auch ein Beweis folgen. Wenn ich einen Beweis machen muss, dann muss ich dafür sicher zumindest mal 3W einplenen. Im Moment ist es noch nicht wirklich drinne, aber ich glaube ich muss das hinzufügen, damit Daddy T happy ist.


= Vorgehen
1. Mehr Beispiele finden, die ich typen möchte
2. Algorithmisch ausprobieren, wie die getypt werden können
3. Algorithmen dafür erfinden/schreiben/brainstormen
4. Davon ausgehend Entscheidungen über die Struktur des Typsystems herleiten

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


= Features
- Pattern Destructuring
- FC-Labels
- Asymmetric Concat
- With-construct
- Recursive types
- Inference


== Dem Big ones
- Row Poly _or_ Subtyping Poly?
  - Row poly wird bei den advanced record calculi (RC) verwendet
  - Subtyping ist genereller, und harmoniert gut mit _type connectives_
    - Dafür ist es aber hart mit den generischen Records
      - Castagna hat gezeigt, wie man _subtyping_ und _row poly_ vereinen kann
        - _asymmetric concat_ still an open question!
- _Row variables_
  - The case for row variables: We need them to make rows generic
    - Do we really need that?
      - I don't think I need fc-rows tbh
- Vielleicht doch direkt den unknown type rein?
  - Hilft bei Abstraktion
    - Tun Top und Bot aber auch


== Records
- Mehrere Row-variablen? Normalisieren?
  - Einfach alle sammeln mit rechts-Präzedenz?
    - Eigentlich eine gute Heuristik 🤔
- Können wir nominale identities für Nix besorgen? (classes)
  - Ich glaube nein!
  - Gegenbeispiel, warum dann Parreaux nicht klappt?


== Schwierigkeiten
- Dadurch, dass es Typvariablen gibt, können in Rows Unklarheiten entstehen


= Archiv
- Was für constraints und wie kann ich die lösen und inferieren?
  - Einfach was Bestehendes verwenden?
- Qualified Types oder Unification oder Bi-unification?
  - Qualified Types: Maximal flexibel, aber hart
  - Unification: Auch constraints, aber easier
  - Bi-unification: Eigentlich nicht viel härter als unification?
