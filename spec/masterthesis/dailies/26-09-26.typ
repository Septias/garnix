
== Vorgehen
1. Neue Ansätze suchen
2. Wert im Bestehenden suchen


== Triage
- What we have:
  - A good placement, strong reasoning for the features
    - Placement actually nicht sooo peak maybe
  - Type-safety for two systems
    - Misplaced?
  - A novel unification alg with quantification
  - *Unification rules for unexpected cases*
  - *The formalization as a trace monoid*
  - **
- What is missing:
  - Termination proofs für unification
- What is missing now is a *framing*
- Approach 1:
  - Just write down what we have
- Approach 2:
  - Try to get into width
  - Add ifs,
- *Blockers*:
  - We still have to decide what to do with ★
  - Basically, admitting eliminators is off the table
  - This just collapses progress & preservation
  - *Are my proves compromised?*


== Realisations
- *The applicability premise to real-world nix is dead*
  - The wand ambiguity is unsolvable
  - And the soft-typing interplay is too weak
    - progress & preservation die immediately
  - We don't have subtyping and as such, patterns are dead?


== Misc
- Ich wusste auch nicht, dass *soft typing* soo schwach ist
- Und habe darauf gehofft, dass das durch occurence typing einfach ausgeglichen werden kann
- Das bekommt man halt dafür, wenn man sein Typsystem nur am Ende versteht
- Einfach alles als "we spark light on" einführen lule
- What really is the use of a typsystem that types no programs that apply a function to an unknow type that it produced itself??
- Vielleicht hätte ich doch eher flow-typing machen sollen
- The proofs I have written don't bring any value
  - so fühlt sich das also an, wenn einem unter dem Körper die Beine weg gezogen werden
    - Fatal
- "A typesystem that directly shows the wand ambiguity" – what even is that
- Fuggin substitution hat jetzt auch noch meine delayed stumps in Unification gekillt.
- Is it even an option to add the eliminators?
- No pp instantly dies
- Automatic instrumentation: Automatically add the guards
- Maybe add a warning on which kind of guard are needed?
- The programmer can then supply them??
- Fggn unknow where types don't match is VACUOOOOOOOUS
- Everything I have created is fkn explicit type substitution
- Ooooohh myyyy fuuuggin gaaaawd
- that is so shit
- The only value "lives in itself" there is nothing to show for the outside
- Maybe that I "carved out another, from the beginning dead, design space"
- Mit ner langen introduction über Nix kann man schon Seiten füllen xD



== State of Nation
Ja, also der Zustand der Nation ist schlecht. Ich habe schlecht geschlafen und mir fällt nicht ein, was man am besten mit ★ machen soll. Eigentlich brauche ich eine neue Innovation, die einfach so aus dem Nichts auftaucht und mir ★ retten kann. Wie gesagt, einfach *Eliminatoren* hinzufügen ist *keine Option*, weil dadurch Progress und Preservation *direkt stirbt*. Vielleicht kann man die eingrenzen? Wenn wir jetzt keine zulassen, dann können Programme, die auf irgend einen Weise das ★ erhalten, nicht mehr weiter getypt werden. Dadurch sterben direkt riesige Teile aus dem Nix-baum, eigentlich ein *no-go*.

Wenn ich die jetzt aber hinzufüge, typed plötzlich alles und es gibt gar keine Aussagen mehr darüber, was richtig und falsch ist. Einen guten Mittelweg für Anwendbarkeit für Nix gibt es soweit (noch) nicht. Bis jetzt ist ★ halt *rigid* das heißt, es ensteht *explizit nur durch die Wand ambiguity*. Darauf bauen dann auch alle Beweise auf. Kann man die Origin davon tracken? Vielleicht kann man Programme automatisch instrumentieren? Damit könnte man dann dann positiv auf die Label checken und zumindest im Programm-Verlauf die *ambiguity aufheben*. Was ist der Effekt auf dan tatsächlichen Typen? *Nope*, das ist einfach nur die Explikation der Semantik. Vielleicht ist die Semantik dahingehend auch einfach Atomatic – es lässt sich nicht mehr raus quetschen. Vielleicht war mein Traum davon, das zu lösen, auch einfach der größte Fehler. Und dann auch, dass ich die frühen Warnzeichen ignoriert habe? Ich habe mich halt immer darauf *verlassen*, dass der ★ schon irgendwie zusammen laufen wird… Und derauf, dass *ich mit mehr informationen mehr machen kann*. Ich hätte einfach, als wir die Eliminatoren ausgeschlossen haben, einmal checken sollen, was die Auswirkungen sind und nicht darauf hoffen sollen, dass dann schon alles klappt. *Eine weiter Lektion, nicht scheu zu sein und auch nicht faul*.


```
x: y: (a || b).l

-> x: y: if (a ? l) then a.l else b.l
```

Ich könnte auch das Outcome einfach auf alle Fälle testen lule. Eine Möglichkeit wäre zum Beispiel, wenn der Rückgabewert vom Typsystem ★ ist, diesen "einfach" auf die Well-formedness zu *testen*. Damit kann ich aber keine Funktionen retten, weil es keine Typen gibt. Ich kann damit auch nicht das Fragment ★ reduzieren, in dem ein Zugriff nur auf Grund von der Wand-ambiguity nicht funktioniert. Schon krass, dass ich es einfach nicht gemerkt habe, wie AI immer ★ nur als Escape nur für "dieser Record-zugriff hat nicht funktioniert" benutzt hat.


== Triage Ideen
- Kann man die Providenz von ★ besser tracken?
- Automatische Instrumentation?


== Fragen
- Wieviel bedeuten meine *Type Safety* Beweise wirklich?
- Warum sind die von dem Problem mit ★ nicht betroffen?
- Sind die qualified schemes ⟨ρ.l ↓ δ⟩ wirklich nur lazyness?
- Wie ernst nimmt Thiemann meine AI-contributions?
- Was macht der parked stump wirklich?
  - Das bringt Principality für Selection
  - Das ist auch eine eigenständige gute Contribution

