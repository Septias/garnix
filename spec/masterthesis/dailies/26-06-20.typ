
./26-06-18.typ
./26-06-21.typ

== Meeting
- Unfication, Instantation und Subtyping rules schon fertig dann?
  - Vielleicht nicht 100% notwendig aber auf jeden Fall schonmal gut

== Contribution
- ∈-Solving
- Patterns
- Unknown Type Abstraction
- Subtyping?


== Fäden
- [x] Funktions-Einführungsregeln aufschreiben
  - [x] Defaults geben Typ vor
- [x] Instantation Rules
  - [x] Tail-Check


== Fragen
- Werden die Subtyping Regeln schon useless, wenn ich zu liberal mit row-variablen umgehe?
  - Ich glaube das sollte in Ordnung sein
- Wie verwende ich die Typinformation für die Default-argumente?
  - Sollen die den Typen bestimmen? Eigentlich schon, oder?
  - Weil die wahrscheinlich aussagekräftiger sind als der Funktionskörper?
- Wie genau mache ich die Instantiierung?
  - Durch Instantiierungsregeln
- Wo Instantation und wo Unification?
  - _Instantation_: Wenn ich einen poly. Typ aus dem Kontext ziehe
  - _Unification_: Für z.B. Funktionsaufrufe (löst equality constraints)
- Macht schon Sinn, einfach Subtyping zu verwenden, oder?
- Kann ich noch negative Information rein bekommen?


== Subtyping
- Wenn ich durch row variablen nur hinzufügen kann, dann kann ich auch einfach über row-variablen rüber gehen.
  - Und constraints hinzufügen, sollte etwas fehlen
- Negative Informationen
  - Ich könnte zur Row einfach hinzfügen ¬l
  - Dadurch hätte ich dann die negativen Informationen drin
  - Syntaktisch wäre das dann Teil der Row
  - Ist die Frage, ob man damit was anfangen kann
  - `r\l:: r -> l -> r'`
    - Ich könnte dann von hinten beim Check einfach stoppen, sobald ich ein removal gefunden habe
    - Ansonsten wie normal auch bei row-vars einfach ★ zurück
      - und/oder Neuen constraint erstellen


== Todo
- Rolle von Unknown klären
  - Als Type
  - Als "keine Lösung"
  - It can leak using subtyping?

