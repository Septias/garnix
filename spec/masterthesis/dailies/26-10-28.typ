
./26-10-29.typ
14 Tage

== Todo
- [x] Check the T-★-intro thingy
- [x] I compare so often to P&X, we should introduce or motivate that I guess


== Fixes
- Stuck has to turn into ★
  - It can not
- Clear up usage of ★ (remove ⊥ from it)
  - Don't remove it, just use it as-is


== Admitting
Ich brauche eine Section nach "Incompleteness", die größere Probleme mit dem Typesystem zugibt. Vielleicht in Conclusion? Beides sind Sachen, die mir hätten schon früher auffallen sollen, aber hätte ich beide angenommen, hätte ich jetzt wahrscheinlich auch gar keine Arbeit. Die Positionierung war von Anfang einfach in einem Feld, das seeeehr wenig hergibt. Vieles wurde schon vor 30 Jahren diskutiert und die Grenzen standen schon damals. Da hätte Thiemann mich vielleicht vor warnen sollen. Die beiden Probleme sind:

- ★ hat keine Eliminatoren, weil sonst das Typesystem zusammenfällt. Dazu gibt es eine gute Lösung mit gradual typing, wobei man da die lazy, nicht-forcierende Version von Thiemann nutzen muss. Ist auch ein guter Ausblick.
- ≈ ist die falsche Relation für unsere Version von asymmetric concatenation. Die richtige wäre natürlich besser, ist aber nicht cancellativen, sodass der Algorithimus nicht forciert ist. Faithful wäre natürlich bessere, aber das würde auch G3 widersprechen. Außerdem bleibt es trotzdem als Lösung bestehen. Die andere Contribution mit delayed stumps ist trotzdem ganz nett und hilft zumindest P&X.


== How to solve dem problems
- Keine Eliminatoren: Note that
- Falsche Relation: Note that
  - Pros: Cancellative
  - Cons: Just pure stupidity
- Role of ★:
  - Used in many places
  - Still sound though
  - It either returns a value or lookup error
- Not alle moves are forced
  - Materialization breaks it
  - Fix: simple caveat in text
- T-★-intro: Only used in the declarative system
  - Motivates our schemes (a bit)
  - Could also be removed, maybe

