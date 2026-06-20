
./26-06-18.typ
== Frage
- Ist es problematisch, dass ich Constraints »mitten drin« löse?
- Wie Typvariablen in Patternmatching?
  - Dafür brauche ich eigentlich eine _substitution_ oder?
    - Ne, die leben ja alle _global_ d.h ich muss die nicht neu einführen?
      - Das sehe ich dann, wenn ich die algorithmische Version davon schreibe?
- Pattern Matching auf Subtyping ausweiten?
  - Im Moment mache ich ja eh Subtyping für Records
- Wo wird _Unification_ dispatched (normalerweise)?
  - Bei Instantiierung?
    - Hängt das dann von Algorithm W oder M ab?
  - Wie genau wird _record access_ in unification systems gemacht?
- Wie Syntax von Pattern und Kinding?
  - Tut Kinding die Syntax distinction übernehmen?
    - Zu einem gewissen grad ja (sag ich \@Thiemann)
  - Warum ist -> ein two-way operator
    - Damit ich funktionen von z.B rows auf types schreiben kann?
    - Brauche ich das?
      - z.B eine Funktion, die ein label annimmt?
        - Nein
      - Eine Funktion, die eine Row entfernt
        - Nein (ganz eventuell die mit array removal (removeAttrs))
- Wo können überall Typvariablen sein und wie muss ich die vergleichen?
  - {α}: row-variable `{ a = 2; }` | `x: x.b`
    - Ich glaube, ich führe die einfach ein, um generischen Code schreiben zu können
  - {α : t}: label-variable `{ ${"a"} = 1 }`
  - {l : α}: variable `x: {a = x;}`


