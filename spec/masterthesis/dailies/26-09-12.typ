
./26-09-13.typ
== Fäden
- Negative type information
- FC labels
- Mechanize infererence


== Misc
- Do the lazy design to unlock the stuck leg first before termination and confluence
  - Potential to destroy more than it gains
- Termination is one of the biggest open things in the thesis
- It might happen, that we can recover the stuck leg
  - Probably not
- Claude does not see the "future", it just always covers what's there "now"
- Fuzzing können wir auch mit in die Masterarbeit aufnehmen
- I should probably write a section on how I used AI


== State
Bevor ich jetzt auf die Fahrradtour gehe, wollte ich mein Projekt noch auf den Berg stellen um gute Ausblicke zu bekommen. Wir sind jetzt an einem Punkt, wo wir `stuck->¬mgu` vorerst aufgeben und dessen Tücken einfach aufschreiben. Die Positionierung der Thesis ist eigentlich auch recht solid, da muss nur die Einleitung noch ausgebessert werden. Das deklarative System mit den Schemes sollte principaled sein und die Lücke zwischen dem und der Implementierung sollte mmn. ganz gut argumentierbar sein.
Dafür hat sich eine neue Lücke aufgetan: Inference ist noch gar nicht bewiesen. Das müssen wir als nächstes angehen, aber ich glaube wenn Unfication läuft, sollte das nicht so wirklich ein Problem sein. Außerdem müssen wir noch s&c für L2 beweisen.


== Fragen
- Why do we get the trace monoid?
  - Because of the row-equivalence structure
- Can we make recursiveness a prominent feature of our work??
  - We already support it partly due to the occurrence check
- What exactly is ⟦S⟧
  - The semantic version of our solver-state
  - Hosts the substitution, stumps, etc.
- What is the best naming-strategy for real?
- What is the covering order?
  - It describes relations between type-schemes
  - Needed to state Principality
- Warum muss der Context acyclic sein?

