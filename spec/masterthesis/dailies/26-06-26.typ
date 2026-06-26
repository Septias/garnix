
./26-06-24.typ

== Todo
- Algorithmische Fragen klären?

== Fragen
- Sollte ich einen access-operator auf rows einfügen?
  - Damit könnte ich direkt die row indizieren
  - Und damit klar machen, dass ich versuche, dort etwas raus zu nehmen
  - Aber andererseits kann ich das doch auch einfach, wenn ich die row assume (als type)
  - Und dann den Type manuell durchsuche?
- Was mache ich mit ★
  - Nun ja, es sollte zu aller erst einmal ein fallback-type sein
  - Und jeder andere Type sollte damit _compatible_ sein
  - Außerdem spreaded der halt ein bisschen würde ich sagen
  - Also wenn etwas damit interagiert, wird es auch zu ★


== Todo
- Wie genau sind Label in der Term-syntax?
  - Ich habe ℓ als base-labels hinzugefügt
- Kann ich mein minimiales TS deklarativ aufschreiben?
  - Anscheinend geht es um well-formedness
  - Aber ich brauche ja trotzdem neue Judgements imo
- ⧀ funktioniert nicht mit depth-subtyping
  - Einfach kein Depth-subtyping dann? xD

