
⟨ a:b | ρ₁ | c:d | l₁: e ⟩


== Fragen
- Wie führe ich records mit rows ein?
  - Während Constraining?
  - Von deren Syntax?
- Soll ich mein Zeug in Lean einfügen?
  - Vorerst nein
- Brauchen wir row variablen?
  - Ja, eigentlich schon gut für Expressiveness
- Wenn ich _unknown ⊛_ hinzufüge, was passiert dann?
  - Brauche ich direkt eine ähnlichkeits-relation?
    - Eigentlich Ja
  - Soll ich graduality-guarantee beweisen?
    - Ich habe keinen getypten Code


== Fäden
- Fc-Label Regeln aufschreiben
- Generic Access function types aufschreiben?


== Lesen
- [x] Algorithm M


== Misc
- Sollten die Funktionen schon polymorph sein?
  - Ja
- Wie trackt man record accesses?
  - (bi-)unification



== FC Labels
e := e₁.\${e₂} | { \${e₁} = e₂ }

t := ⦅l⦆

kinding:


-------------
l: Lab


Γ ⊢ l: Lab
----------
Γ ⊢ ⦅l⦆: ★

Wie genau funktioniert das?
```nix
let
 e2 = a: b: c: (a // b).${c};
in ()
```
- Hier wissen wir überhaupt nichts über c
- D.h. der Access ist auch maximal unbestimmt
- Kann man hier mit ∈-Constraints was erreichen?
- Man könnte _bi-directional_ auf c schließen, wenn a und b feststehen
  - Oder halt nur ein `c` in { a: τ, b: τ }


== Unkown Types
ty := ⊛
