

./26-06-20.md
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
- Subtyping Rules


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
- Macht schon Sinn, einfach subtyping zu verwenden, oder?

== Todo
- Rolle von Unknown klären

