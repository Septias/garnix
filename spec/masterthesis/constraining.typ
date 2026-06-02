#import "./functions.typ": *

== Constraining
This one should be for generic accesses:
#derive("C-with", $$, $ Γ ⊢ (α plus.double β).l => (l ∈ α + β) $)

Like this:?
#derive("C-with", $ "routine"(r) $, $ Γ ⊢ r.l => (l ∈ overline(A)) $)

We need a routine that traverses the record and for every field does:
- Normal field:
  - Is l? => If A is empty then we can just return the type. Otherwise we skip
  - Not l? => Skip
- Row-var:
  - Add to constraint set
- Explicit row:
  - Recurse
- This routine returns overline(A)

== Fragen
- Wie kann man die Constraints wieder auflösen?
  - Heuristik: sobald(von hinten) $∃a ∈ A. a.l↓$ dann $(a).l : τ$
    - *Unsound*: Die späteren nehmen natürlich Präzedenz
      - Können wir alle Constraints soweit aufschieben, bis sie lösbar werden?
        - Wenn pureness: Dann ja?
          - Wie siehts mit Completeness aus?
  - Heuristik: Gute Accesse solange explizit, ansonsten ★
    - Wieder von hinten lesen und expliziten Typ zurück wenn möglich
    - Ist halt direkt _gradual_ (A system based on refinements)

