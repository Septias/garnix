
./26-07-01.typ
./26-07-03.typ

== Fragen
- Ist ein record ein value, wenn seine Felder values sind?
  - `{ a: (f x)}`
  - Dadurch, dass ich jetzt lazy bin, wird hier (f x) überhaupt nicht evaluiert
- Soll ich für lazy gehen?
- Kann ich die Rocq verification von der Nix Semantik der Niederländern nutzen?

> [Das "reverse direction"-Problem ist: T-λ∈-not-in schaut von Konklusion zu Prämisse - du hast (x ∈ S) in der Konklusion, aber die Prämisse braucht (x ∈ S∖x). Für Preservation (von Konklusion zu Konklusion) ist das okay, aber für Inversion (Prämisse rausziehen) brauchst du Eindeutigkeit der Ableitungen.]¿


== Tricky Steps
- Es wird auf jeden Fall tricky, die ∈-constraints durchzubeweisen
- Besonders _subject-reduction_ könnte tricky sein?
- Ich bin mir halt nicht sicher, wie sich das mit ★ ausgeht

- How does (x ∈ e) work?
  - Basically every lookup of a row is opportunistic
  - But we have to look it up in the supplied argument e₂

---------------------
Γ ⊢ (x: e₁)e₂: τ
-> e₂[x = e₁]

We prove:
1. Reduction happens normally
2. But gets stuck when a needed var is not available in S where |S| = 1
3. How to "capture" the change after we know x ∉ S
  - This has to go in the reverse direction
  - Basically how do we allow for these constraints to go into the other direction?
  - Do we need something like "context weakening?"

- How should we procede, when we can not check our e? (★)
  - Option 1: Just ignore the check
    - This can be safe because "less is always okay"
  - Option 2: Extend the check
    - Just put the new row-vars into the bag


- The final proof should then go along these lines:
  - A check will only give you true positives
  - Unkown turns into ★
    - TODO: Does it break subject reduction?
  - The set of typevars can increase and decrease (application and u-extension)
    - Can it go towards infinity?
      - It should not because the program text is finite
      - But can we create new typevars in a loop?
        - `let f = f f in ()`
        - Ka, wo das genau dann instantiiert wird…
      - TODO: basically, what happens in the recursive case?
        - I think we might need to mark distinct vars here
  - Otherwise it does not have an effect…

- We could get trickiness because the logic is split over multiple rules…
  - Könnte durch Bag-unification gelöst werden…
  - Könnte durch type-lookup gelöst werden?
    - Die sind actually für das deduktive System goated
  - Multiple contexts?

Γ ⊢ e: {ρ}   ρ.l: τ
---------------------
Γ ⊢ e.l: τ

- Wie genau müssen wir hier jetzt die Typvariablen modeln?
  - Eigentlich brauche ich direkt ein deduktives system für »p.l«
  - Oder ich nehme halt an, dass es da den Typen für gibt?
    - Woran mache ich aus, ob solche Annahmen okay sind?
      - Vielleicht, wenn es "genug eingekapselt ist?"


- Can we get problems because of this opportunistic approach?
- I can imagine, that it breaks _subject reduction_ because suddenly a ★ pops up where we did not know it existed
  - Maybe this can be solved by a weaker statement than subject reduction?


- Dinge, die ★ zurück geben können:
  - Lookups
  - Lookups auf rows sind halt actually einfach smart
