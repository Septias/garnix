
./26-06-21.typ
./26-06-23.typ

== Fragen
- Wie funktionieren Reorderings?
  - Reicht es, wenn ich das möglich mache?
  - Algorithmisch könnte ich das dann einfach lexiografisch z.B umsetzen
- [x] Wie genau machen wir die Zuteilung von Basetypes?
  - `b ∈ 𝓫` und constants `e := … | c` mit `c: b_c`
- Sollen ∈-fun-p-apps schon während Inferenz angewendet werden, oder bei Unification?
  - Ich habe halt ∈-constraints
  - Grundsätzlich kann man die glaub propagieren oder sammeln und später lösen
  - Beim später lösen
    - Können wir die dann nach deren enthaltenen Sets (Records) sortierens
    - Und dann nach Inconsistenzen suchen?
      - _Inconsistent_: Instanzen von Typvariablen haben Felder nicht
- Wie genau übersetze ich Records in Sets? \#algorithm
  - Soll ich die in ihren Rows lassen?
  - Oder deren Identität referenzieren?
- Wenn ich Constraints bei Funktionsaufrufen auflöse
  - D.h. für die Typvariablen der Funktionsargument gibt es _constraints_
  - Dann vergleiche ich die mit den supplied Argumenten (α ⩪ x)
  - Und die Anwendung von expliziten Records führt zu Reduzierung
  - Wenn es sich um andere Typvariablen handelt, werden die Constraints weiter gereicht
  - Wenn leer und nirgendwo drin => fehler
  - Ist ein opportunistischer (sound but not complete) Algorithmus
- Kann ich irgendwie fordern, dass sich die Constraints ausgehen?
  - Oder kann es auch sein, dass ich die nicht weiter lösen kann?
  - Toplevel funktionen z.B. kann man nicht weiter reduzieren
  - Also: Nein
- Wie genau würde das Hochreichen funktionieren?
  - Einfach constraints umschreiben (type-vars austauschen)
- Kommen weitergegebene Constraints immer an expliziten Records an?
  - Bzw. tracke ich Records überhaupt genau genug?
  - Element checks, sind halt ungenau und werden derzeit *schon beim Zugriff überprüft*
    - Sollte ich da einfach die Constraints machen?
    - Aber was für einen Typ gebe ich dann zurück?
      - Es kann halt sein, dass ich dadurch eine Funktion `int -> ★` bekomme
- Wie Constraints auf Label-variablen?
  - Effektiv brauche ich constraints der Form (l = α), die sollte man aber auch easy machen können
  - Die müssen halt nur mit den ∈-constraints passen
  - Kann man das als #1-set umschreiben
