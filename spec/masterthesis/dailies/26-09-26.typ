

./26-09-27.typ
== Vorgehen
1. [!] Neue Ansätze suchen
2. [x] Wert im Bestehenden suchen


== Triage
- What we have:
  - A good placement, strong reasoning for the features
    - Zumindest ein bisschen, das Wording muss halt 3 Gänge runter schalten
  - Type-safety for two systems
    - Misplaced because they don't work in the ★-fragment
  - A novel unification alg with Quantification
  - *Unification rules for unexpected cases*
  - *The formalization as a trace monoid*
- What is missing:
  - Termination proofs für unification
  - Incoming maybe
- What is missing now is a *framing*
- *Approach 1*:
  - Just write down what we have
  - Framing is a calculus that is similar to nix
  - But delineates the exact cases of the wand ambiguity
- *Approach 2*:
  - Try to get into width
  - Add ifs, occurrence, etc.
- *Blockers*:
  - [x] We still have to decide what to do with ★
  - Basically, admitting eliminators is off the table
  - This just collapses progress & preservation
  - *Are my proves compromised?*


== Triage Ideen
- Kann man die Providenz von ★ besser tracken?
- Automatische Instrumentation?
- Maybe put occurrence typing into the mix?
  - Castagna has a paper about the automatic checks of the interpreter
- Maybe frame it as:
  - We can just later add the rules that spread ★ together with gradual typing
  - Nix provides the needed primitives so this is consistent
  - This weakens our claim that we can just infer everything
  - But fits in the picture for leaving it open for the future


== Realisations
- *The applicability premise to real-world nix is dead*
  - The wand ambiguity is unsolvable
  - And the soft-typing interplay is too weak
    - Progress & Preservation die immediately
  - We don't have subtyping and as such, patterns are dead?


== Misc
- Ich wusste nicht, dass *soft typing* soo schwach ist
- Und habe darauf gehofft, dass das durch *occurence typing* einfach ausgeglichen werden kann
- Das bekommt man halt dafür, wenn man sein Typsystem nur am Ende versteht
- Einfach alles als "we spark light on" einführen lule
- What really is the use of a typsystem that types no programs that apply a function to an unknow type that it produced itself??
  - We just delineate it now, *full gradual typing is for later*
- Vielleicht hätte ich doch eher flow-typing machen sollen
  - Dann wärst du aber wack gewesen lule
- "A typesystem that directly shows the wand ambiguity" – what even is that
  - A system with some improvements and a good stepping stone
- *Fuggin substitution hat jetzt auch noch meine delayed stumps in Unification gekillt.*
  - Cry later bitch
- The only value "lives in itself" there is nothing to show for the outside
  - Actually, soundness, termination and principality in some cases
- Maybe that I "carved out another, from the beginning dead, design space"
- Mit ner langen Introduction über Nix kann man schon Seiten füllen xD
- Wenn die Beweisrichtung nicht passt, hilf einem AI gar nichts


== State of Nation
Ja, also der Zustand der Nation ist schlecht. Ich habe schlecht geschlafen und mir fällt nicht ein, was man am besten mit ★ machen soll. Eigentlich brauche ich eine neue Innovation, die einfach so aus dem Nichts auftaucht und mir ★ retten kann. Wie gesagt, einfach *Eliminatoren* hinzufügen ist *keine Option*, weil dadurch Progress und Preservation *direkt stirbt*. Vielleicht kann man die eingrenzen? Wenn wir jetzt keine zulassen, dann können Programme, die auf irgend einen Weise das ★ erhalten, nicht mehr weiter getypt werden. Dadurch sterben direkt riesige Teile aus dem Nix-baum, eigentlich ein *no-go*.

Wenn ich die jetzt aber hinzufüge, typed plötzlich alles und es gibt gar keine Aussagen mehr darüber, was richtig und falsch ist. Einen guten Mittelweg für Anwendbarkeit für Nix gibt es soweit (noch) nicht. Bis jetzt ist ★ halt *rigid* das heißt, es ensteht *explizit nur durch die Wand ambiguity*. Darauf bauen dann auch alle Beweise auf. Kann man die Origin davon tracken? Vielleicht kann man Programme automatisch instrumentieren? Damit könnte man dann positiv auf die Label checken und zumindest im Programm-Verlauf die *ambiguity aufheben*. Was ist der Effekt auf dan tatsächlichen Typen? *Nope*, das ist einfach nur die Explikation der Semantik. Vielleicht ist die Semantik dahingehend auch einfach Atomatic – es lässt sich nicht mehr raus quetschen. Vielleicht war mein Traum davon, das zu lösen, auch einfach der größte Fehler. Und dann auch, dass ich die frühen Warnzeichen ignoriert habe? Ich habe mich halt immer darauf *verlassen*, dass der ★ schon irgendwie zusammen laufen wird… Und derauf, dass *ich mit mehr informationen mehr machen kann*. Ich hätte einfach, als wir die Eliminatoren ausgeschlossen haben, einmal checken sollen, was die Auswirkungen sind und nicht darauf hoffen sollen, dass dann schon alles klappt. *Eine weiter Lektion, nicht scheu zu sein und auch nicht faul*.


```
x: y: (a || b).l

-> x: y: if (a ? l) then a.l else b.l
```

Ich könnte auch das Outcome einfach auf alle Fälle testen lule. Eine Möglichkeit wäre zum Beispiel, wenn der Rückgabewert vom Typsystem ★ ist, diesen "einfach" auf die Well-formedness zu *testen*. Damit kann ich aber keine Funktionen retten, weil es keine Typen gibt. Ich kann damit auch nicht das ★-Fragment begrenzen, in dem ein Zugriff nur auf Grund von der Wand-ambiguity nicht funktioniert.


== Fragen
- Wieviel bedeuten meine *Type Safety* Beweise wirklich?
  - Im soft-typing Sinn sind die ziemlich gut
- Warum sind die von dem Problem mit ★ nicht betroffen?
  - Weil die einfach keine Derivation für solche Programme zulassen
- Sind die qualified schemes ⟨ρ.l ↓ δ⟩ wirklich nur lazyness?
  - Irgendwie schon, die sind anscheinend auch nur substitution
- Wie ernst nimmt Thiemann meine AI-contributions?
  - Wahrscheinlich gar nicht
- Was macht der parked stump wirklich?
  - Das bringt Principality für Selection
  - Das ist eine ganz okaye Contribution
