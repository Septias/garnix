./26-07-09.typ
./26-07-11.typ

== Fäden
- Brainstorm Constraintsolving
  - Do I even want to do constraint solving outside of function instantiation?
- Subtyping mit lookup-relation
- Fix lookup relation (fixed labels should occur?)
  - Soll ich einfach die ganzen Records rein geben?
    - I mean why not, muss man halt bissel trash raus schmeißen
- Instantation muss noch geklärt werden (mit let-poly dann)

== Todo
- Extrakontext hinzufügen


== Advanced Topics
- Aufschieben von Constraints
- Type Refinements based on T-App
- Constraints during Subtyping
- Subtyping und ★
- Unification


== Subtyping
- Warum brauche ich das genau?
  - ~Patterns
  - ~Nice
  - ~Verwendung von ★


== Möchte ich generell Subtyping-Constraints habe?
- Für unification habe ich dann ja eh constraints
- Aber das sind halt ≡-constraints
- Ich mag halt die Syntax z.B. `r <= {l: int} => r -> int`
- Fürs erste kann ich mich aber eigentlich echt einfach auf ≡-constraints verlassen
- Ich glaube, das war initial über die application function motiviert
- Weil bei Records ist halt depth- und width-subtyping mega convenient


== Refinement
- What kind of refinement
  - Initial: Einfach nur fehler, wenn x nicht in den Sets
  - Neu:
    - Wir merken uns welche Record-lookups zu ★ geführt haben
    - Wenn wir records supplien, überarbeiten wir rows und spezialisieren Typen
      - Das ist halt grundsätzlich deep
      - Es ist dann nicht klar, welche Auswirkung ein neues β hat
        - Das ich bei einem lookup (`r.l`) für den Returnwert nutzen kann
      - Wenn ich jetzt β einfach neu instantiiere, weil aus dem Paar (r,l) klar wird, was für ein Typ das hat, dann substitute ich einfach überall β
      - Aber wer führt das weiter? Also wenn ich dann (β + α) hab?
      - Constraints der Form: `(r, l) => β` Read as: »looking up record r with l gives a type to β«


== ∈₂-constraints
- Effektiv kann ich hier β instantiieren, wenn ρ in r ist und dadurch l freigeschaltet wird.
- Wie mache ich equality?
- ρ₁↓lρ₂ macht mehreres:
  1. Es versucht ρ₂ in ρ₁ zu setzen
    - rows ρ sollten immutable sein btw.
    - Ich glaub das passt, aber ist ρ auch im Kontext zu finden?
    - Wenn ich zwei rows zusammenführe, binde ich sie ja nicht unbedingt an einen Namen
    - Deshalb muss ich die dann in den Kontext aufnehmen
  2. Danach wird ein neuer lookup versucht (ρ'.l)
    - Ergebniss τ: β -> τ
    - Ergebniss ★:
      - Weil nicht eingesetzt: Nichts passiert
      - Trotz expliziter Einsetzung: Nichts passiert
      - Wenn T-var eingesetzt: Constraint muss umgeschrieben werden?
        - Vorher: (α | ρ), aber dann wird α durch β ersetzt, also (β | ρ)
- Ist das nicht ne ganz normale Instantiierung (τ ⊑ σ)?
  - Nee, das ist für let-poly
  - Wobei ich natürlich fast alle Funktionen so verwende
  - Wie mache ich das denn mit anonymen Funktionen?
  - Soll ich die auch einfach type-schemen ?
  - Muss ich ja eigentlich, weil man die in Nix auch so verwenden kann
- Muss ich ein Netz an Row-variablen maintainen?


Γe₁ ~ ((ρ₁, l) ↦ β)   Γ ⊢ e₂: { ρ₂ }    ρ₁↓ₗρ₂
------------------------------------------------ T-App
Γ ⊢ (x: e₁)e₂: τ

`((Γ,e₁) ~ (r, l) => β)`

- Kann ich so die Syntax destructuren?
- Oder fehlen mir dann applications von z.B Typvars
- Also ne Funktion kann ja auch in ner Typvar sein, oder?


Γ ⊢ ρ.l: ★   Γ ⊢ e: {ρ}   C = (ρ, l) => β   β fresh
----------------------------------------------------- T-sel-τ
Γ ⊢ e.l: β => C
