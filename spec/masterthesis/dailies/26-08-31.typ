
./26-09-01.typ

== Thesis Positioning
- We have the problem of wand (a ‖ b).l
- We can provably not find an mgu for it
- So we surrender to ★, which is final
- We are *explicit about the failure class*
- We admit a new lookup relation ⟨ρ.l ↓ r⟩
  - Which is cool but the nice part (L-α) is basically type-substitution
  - The ?-lookup does not exist though
- The relation is extended to qualified types
- This allows us to type some more examples of P&X
- Finally, we (want) to give a principled unification algorithm


== The constraints in Q
- They contain:
  - The lookup-label
  - The row
  - The outcome
- This fully defines a lookup outcome
  - Positive: `f: ⟨β.l ↓ τ⟩ => β -> ⦅l⦆ -> τ`
  - Negative: `f: ⟨β.l ↓ ⊥⟩ => β -> ¡`
    - Actually can not be stated, because only on explicit rows?


== Todo
- [x] Check why `unify_eq_rescued_stuck` gives incompleteness
- [x] window-only matching is incomplete


== Fragen
- Is ⊑-rigidity what disallows us to add ★-eliminators?
  - Not really, it is more that with eliminators we could type any expression
  - Also, the typsystem would turn vacuous in many cases
    - Consistency à la Siek&Taha would be possible without collapsing typing
      - But this needs casts
    - *Occurrence Typing* with ifs can work
- Was genau ist die Rolle von ↯ und bin ich damit fine?
  - ↯ ist das Fragment von Progress, das in ★ liegt und schiefgehen kann
  - Es ist so ähnlich:
    - Entweder wir machen einen step, value, oder wir bekommen einen ↯
    - Das ist der Fall, wenn wir z.B. einen lookup auf einem Term mit type ★ haben, der nicht existiert
- Was macht Unifications Discharge?
  - Limmitiert die substitution θ, sodass die return-variable fest vom Scheme bestimmt werden kann
- Can I cancel (levi-lemma) the same way with FC-labels?
  - Wahrscheinlich schon, sollte sich ja nicht viel von dem Handling von row-variablen unterscheiden
  - Ähnlicher trace-monoid, keine Switches über diese Variablen
- Isn't `{ε} -> ★` too restrictive?
  - Expecting ε in the case where no lookup can work?
    - Actually, ε is just an instance could also be (m: τ | β | …)
- Do we really want to handle recursive rows yet?
  - Occurs check is dependent on it…
  - Maybe overcomplicates too much, but if it comes naturally…
- Do we really need forced steps?
  - Forced steps help in success → mgu statement
- Wieso ist two-sided-cancellation s&c?
  - Verliert keine solution sets
- Do I really want to strip row-vars to get mgus?
  - Naja, unification entscheidet einfach, wo's ist.
  - Und mgu gibt dafür die Berechtigung (also solution sets)
- Wie "schlimm" ist closedness?
  - Sind nicht alle nix programme eh closed. Also wellformedness -> closedness?
- Was genau könnte ich aus \@extensible_rec_funcs übernehmen?
  - Ist nicht effektiv berechenbar (in der gezeigten Form)
  - Ist von den Constraints auch nochmal komplexer
  - Morris ist bereits an einem Algorithmus
- Ist das Rank 1 fragment von System F stark genug für \@extensible_rec_funcs?
  - Scheint nicht so, weil tuples sind rank-1 wären (die Syntax von ROSE erlaubt dort ∀)
- Warum genau können wir keine Type-Connectives hinzufügen?
  - Keine negativen positionen möglich, weil keine negativen Informationen auf Records
  - Außerdem kann subtyping Felder verschwinden lassen
- Why do we admit ★ when we have definite absence (T-sel-⊥)
  - Because ? can turn into ⊥, but we still need to type at rigid ★

== Misc
- *principality* vs *instance-closed*:
  - Principality heißt halt, dass ein Scheme alle Typings abdeckt (Typings(e) ⊆ ⊑-closure(Inst(σ)))
  - Instance-closed heißt einfach fürs Let, dass der instanziierte Type (durch Substitution) instanze vom Scheme ist (Inst(σ) ⊆ Typings(e))

== Claude Prompts
- [x] Why does not using recursive rows force the fourth leg (occurs)?
- [x] How far are we (in %) to finishing the proofs for this thesis
- [x] Why do we have T-sel-⊥? Isn't it bad to keep bad lookups artifically alive?


== State
- Die Haupt-Contribution ist halt die Idee mit dem ★ und was daraus folgt
- Und dann gibt es halt die three-way lookup-relation
- An sich könnte man die eventuell gänzlich durch Substitution austauschen
  - Aber es ist halt die algorithmische Version, auf die wir ja auch abzielen
  - Dadurch ist kann man das vielleicht schon gut einbauen
  - Ich habe halt Angst vor gebildeten Lesern:
    - Warum macht er das jetzt so non-standart/umständlich?
    - Das könnte man doch auch so… lösen
    - Ist dann halt für den Aufwand an sich okay vielleicht?
    - Muss man vielleicht ein bisschen schauen
  - ⊖ Wir brauchen die eh wegen ?-outcome
- Der interessante Teil sind dann die *Qualified Schemes* mit den lookups
  - Durch die verliert man keine Informationen
  - Algorithm ¿ ist ja auch "Kontextabhängig"
  - Genau das habe ich dann ja durch die Rows in denen typ-variablen stehen können
  - Auf jeden Fall bekomme ich dadurch dann (hoffentlich) *principality*
    - Weil jetzt ist jede instantiierung (Die dann zu neuen Ergebnissen im Lookup führen)
    - Eine Instanze aus dem Scheme
- Was mich jetzt noch stört ist
  - Ich glaube das Typesystem geht in die richtige Richtung und kann aufgehen, aber mir fehlt eine ganz koherente Sicht
  - Wenn jetzt Thiemann wieder anfängt zu kritisieren, kann ich dann jede Designentscheidung verteidigen?
  - Oder hat Claude sich einfach nur *irgendeine* Lösung ausgedacht?
  - ★ als admitted uncertainty ist glaube ich ein gutes Framing
  - Jetzt ist die Frage, wie kann ich das in die Tetrachotomy einbauen?
