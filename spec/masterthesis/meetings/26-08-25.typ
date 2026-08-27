
> Ich bin erstmal mit "hardest-first" reingestartet
> Das heißt algorithmische Version definieren
> Um frage nach "principality" und "computability" zu lösen
> Wurde dann durch die zwei Beweise auch "notwendig"
> Danach algorithmisch relativ weit mechanisiert
> Ist aber grad noch ein "push", um zu probieren, wie weit wir kommen

- Principality braucht algorithmisches System
  - Zwei mechanisierte Beweise (Type für $x: x.l$)
- Soll Thiemann mal über die Proofs schauen?
  - Poah nicht unbedingt, mach ich ja selber nicht lule
- Okay, dass ich keine Eliminatoren für ★ habe?
- Hab nicht viel weiter geschrieben
- Probleme
  - Instance-closed T-let?
  - Still no renaming (AI wehrt sich)
  - .occurs ist zu kenservativ
- Finding of *trace monoid*
  - Basically we have cancallation on both sides of the row
  - This allows us to remove "LUtail" and give proper solutions in strictly more cases
- Der unification-algorithmus nutzt forced-steps und trichotomy:
  - [x] (success & mgu), [x](conflict & error), [~] (wand & no-mgu)
  - Wand principality restated
    - Klassisches Problem von "Woher kommt dat ding?"


== Result
- L-α anschauen (ist das wirklich so kacke?)
- Intenional Polymorphism?
