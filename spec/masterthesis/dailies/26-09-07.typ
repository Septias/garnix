
== Einordnung
Okay, ich hab's nochmal durchdacht und meine Contribution ist mehr oder weniger vacuous. Die lookup-relation mit 3 Outcomes ist "ganz nett", weil sie die möglichen Outcomes genau aufzählt, aber im Endefekt gab es die 3 Outcomes auch schon immer. Positiv und negativ ist standart. Das dritte Outcome mit dem Fragezeichen ist einfach nur ein state, der existiert, während die typvariablen noch nicht eingesetzt sind.

Das wirklich einzige, das ich extra beisteuere ist, dass ich die lookup constraints bekomme und auch ein bisschen mein ★-Fragment. Mit den Lookup-constraints kann man ein/zwei extra-Beispiele typen, die bei P&X nicht gehen, aber das war es auch schon. Das ★-Fragment ist relativ statisch ohen Eliminatoren und hält das Programm nur "künstlich am leben". Das ist notwendig in Nix, aber jetzt auch nicht weltbewegend. Mit Occurrence-typing kann man da vielleicht noch was dazu holen…

An sich ist es schon ganz nett, wenn ich später dann einen Beweis für den Unification-Algorithmus mit der lazyness und den rekursiven Typen bekommen, aber da ist beides auch noch ein bisschen in der Schwebe. Eventuell muss ich mit Patternmatching und Occurrence-typing nochmal ein bisschen out-branchen…

== Findings
- Neues Beispiel, warum `.stuck->no mgu` is *incomplete*:  (k:{β|α} | β)  ≐ᵣ  (k:{l:𝓫} | l:𝓫)
  - Maybe salveagable mit anderer Order
- Terminal does not salvage the situation: (l : {w})  ≐ᵣ  (w | v)
  - The occurs check rules out one solution?
  - Maybe salvagable
- Die prämissen für `stuck -> no mgu` sind einfach fasch


== Depth
- `InstanceOfOn V` hätte wahrscheinlich nicht umgeschrieben werden müssen
- `HasMguO` is not load bearing since the step that would consume is got cancelled


== Todo
- Check (`occurs_allVar_hasMgu`, `stuck_masks_mgu`, `terminal_masks_mgu`)


== Misc
- (k: •) wird erst gelöst und ist wand. *Danach* fällt auf, dass `β = l: 𝓫`
  - Hier ist die Reihenfolge das Problem!
  - Deshalb: Neue Reihenfolge. Kann nur bei einem Problem helfen
- Wir haben incompletenessresulte gefunden, die *principality* und *termination* in Frage stellen
- Claude möchte, dass ich meine Thesis über L1 vs. L2 schreiben
  - Aber das hat sich nur so ergeben, das ist eigentlich kein so krasses "Ergebniss"


== Claude Prompts
- [ ] Do you think it is a good idea to show L1 and L2 in the thesis? I think it might be better to only show the current system


== Unification
(α, l:τ)    | (α, l:τ)    => ✔
(α, l:τ)    | (α, m:τ)    => ✘
(α, β, l:τ) | (α, l:τ)    => ✔ (β → ε)
(α, β, l:τ) | (α, l:τ, γ) => ✔ (β → ε, γ → ε)
(α, γ)      | (l: τ)      => ⊗
(α, β, γ)   | (α)         =>

- Nur ein Feld auf der einen Seite und mehrere TY-vars auf der anderen: WAND

== Fragen
- Warum ist bedeutet incompleteness, dass principality nicht geht?
  - Weil ich einen expliten Typen liegen lasse?
