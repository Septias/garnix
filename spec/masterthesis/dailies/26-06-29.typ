
./26-06-28.typ
./26-06-30.typ

== Fragen
- Local type inference?
  - Wir gehen halt für den Unknown Type ★ anstatt Fallback auf Annotationen
  - Ist im Kontext von Nix glaube ich auch besser
- Sollen die Constraints local oder global sein?
  - An sich ist das auch ne gute Distinction von
    - An Funktionskörpern
    - vs. global bag, der gelöst wird

== Fäden
- Row equivalence aufschreiben
  - Ist Gleichheit da Subtyping?

== Proofstrat
- Basically create a _deductive_ system and prove _progress and preservation_ wrt. semantics
- Then create _algorithmic system_ and proof _sound & complete_ wrt. _deductive system_


= Vorgehen
Momentan bin ich ein bisschen stuck und sehe den nächsten logischen Schritt nicht.
Ich könnte einfach weiter syntaktische Spitzfingerigkeiten ausgleichen, oder weiter mit unification etc. machen.
Viel wichtiger/interessanter ist aber eigentlich, dass mein Algorithmus funktioniert und gute Ergebnisse liefert.
Aber selbst der minimale Calculus ist jetzt schon ziemlich komplex. D.h. ich könnte den evtl. durch ein duduktives System kleiner bekommen? Aber da bin ich auch ein bisschen stuck; Was darf ich in so einem System alles weglassen? Ansonsten kann ich auch einfach verbal weiter brainstormen (z.B die Proofs oder wie mein Algorithmus funktioniert). Das jetzt in Lean rein zu pasten ist glaub unnötiger Aufwand. Einfach weiter Paper lesen auch wie schon festgestellt nicht mehr der go, auch wenn ich heute ein bisschen gute Inspi bekommen habe und es ist halt immernoch besser als nichts tun. Aber ich schätze mal den Algo auf Herz und Nieren zu prüfen wäre ganz gut. Wenn es halt nicht klappt bin ich wieder bei 0…
