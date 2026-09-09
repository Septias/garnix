
./26-09-09.typ

== Record Systems
- Mitchell&Wand: Row-variables (absence flages) @concat4multiinher
- Remy: Principaled rows @remy_natural_ml
- Gaster&Jones: Lacks (with qualified types)

== Comparison
For asymmetric concatenation with shadowing, no system achieves all of: (P) precise result types, (I) complete inference with principal types, (S) solving at unification cost.

- Gaster–Jones lacks-constraints: keep I and S, drop P (no concatenation at all)
- Rose: keeps P and I, drops S (entailment is a parameter; no solver, no bound — likely NP for simple rows)
- MLstruct / algebraic subtyping: keeps P and I, drops S differently (subtyping-constraint solving, not unification) — and by your own width-subtyping argument can't host ‖ soundly anyway
- Castagna: keeps P, drops I (local inference, not let-polymorphic)
- Ur: keeps P and S, drops I (programmer supplies disjointness witnesses)
- Yours: keeps I and S, drops P — but only locally, and marks precisely where (★ introduced solely by the lookup relation)


== Misc
- Die Positionierung meiner Masterarbeit ist irgendwie basierend auf meiner contribution… die irgendwie nicht so stark ist… Dementsprechend kann ich meine Arbeit nicht so "offen" formulieren. Ich will halt eigentlich schon was sinnvolles machen :( Die Sachen von Morris sind halt cool, aber ka, ob das Rank-1 fragment von denen jetzt reichen würde.
  - Nope it does not
- Bei der Introduction und Motivation kann ich schon ein bisschen raus flexen, Masterarbeit hat ja keine vorbestimmte Länge lule.


== Fragen
- Warum bedeutet incompleteness, dass principality nicht geht?
  - Weil ich einen expliten Typen liegen lasse?
    - Actually, ein typ könnte zu ★ werden, was weniger Infos als τ enthält?
      - Aber nur im algorithmischen system
- Do I like the long introduction for the typesystem?
  - Some comparison and references should go into related work
    - But only if they are not guiding
  - Lange Einführung von Giusppe eigentlich immer nice
- Do I need typevar lookup for laziness?

