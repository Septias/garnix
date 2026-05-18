
- So ne Typhirarchy zu erstellen klingt schon irgendwie sinnvoll
  - Zumindest wird das in vielen Papern verwendet

== Vorgehen
- Lohnt es sich, dass ich so _Grundlagenforschung_ mache?
  - _Ich hab halt Bock drauf_ anders wäre es ein bisschen unnötig
  - Es ist gut, um die ganzen Konzepte mal zu vereinen
  - Aber sind wir mal ehrlich: Ich werd jetzt nicht nen neuen Algo erfinden...
    - Oder doch? o.o
  - Quantitativ ist halt nicht so gut wie qualitativ
  - Wenn ich jetzt 10 Systeme erdenke, dann werde ich nicht alle zeigen können
  - vielleicht finde ich dann _ein richtig gutes_?
  - Thiemann & Marius würden sich einfach auf die Arbeit stürzen
  - Aber ich drücke mich halt wieder vor Typregeln (kowai)
- Es ist schon gut, sich auf die _Knackpunkte_ zu fokussieren
  - D.h. aber auch keine easy cheats und "nur mal lesen"

== Ziel
- Ich bin nicht mehr so meeega motiviert was krasses zu machen
- Ein System zu formalisieren reicht mir
- Ich bin mir aber nicht sicher, wieviel Zeit ich für eventuelle Beweise brauche
- Ich glaube aber, dass ich mit moderatem Aufwand ne 1.0 oder so bekommen _könnte_
- Deshalb bin ich im Moment auch noch am chillen und ich sag wies ist. 2h pro Tag schon hart lol.


== Syntactic vs. Algebraic vs. Semantic
- Syntaktisch ist halt schon sehr fisselig
- Besonders wenn mein Typsystem dann komplex wird, wirds halt arsch das zu beweisen
- Algebraic habe ich ngl. gar keine Ahnung. Ich glaube die Theoreme von Dolan könnte man recyclen, aber darüber hinaus kann ich halt garnichts.
- Semantic wäre auch möglich irgendwie, aber ich glaube da muss man sich dann schon auf das Vorgehen von Castagna einlassen


== Fragen
- Sind Argumente bei den Funktionsaufrufen schon maximal präzise?
  - Nein:? Bidirectional typing? ← eigentlich nicht?

== Lücken
- Wie genau funktionieren Qualified Type Systems?
- Meine derzeitigen Systeme kann ich alle noch expliziter machen (oh no, work)
- Bidirectional typing?
- Gradual?

== Fäden
- Die Systeme von Castagna nochmal wirklich anschauen… ?
  - Oder ist untractability schon zu viel?
  - Aber anscheinend gibt es dazu ja Lösungen…
- Subtyping hirarchie versuchen zu machen?
  - Danach dann schauen, ob man constraints auflösen kann?
- Vielleicht echt erstmal explizite Records und Destructuring?
  - Ist halt der Bottom-Up approach
- Brauche ich für Funktionen schon Typvariablen?


== Von unten nach Oben
1. Records und Function Patterns
  - Inferenz basierend auf Unification?
  - Probleme: Ich brauche schon subtyping?
  - Probleme: Wie genau werden die neuen Variablen assoziiert?
2.
