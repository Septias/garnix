== Session: Preservation-Einordnung → L1/L2 entschieden

- Ausgangsfrage: ist Preservation "misplaced", weil T-sel-⊥/T-★-intro als
  escape hatch dienen? Antwort: halb. Die Regeln absorbieren die
  Lookup-Kategorie-Wechsel der Instanziierung (?→⊥, ?→τ) — aber NICHT in
  den Step-Fällen von preservation_aux (dort vacuous), sondern in
  typed_applySubst_aux (tSelUnk-Fall) und typed_ext. On-the-nose
  Preservation ist die STÄRKERE Form; die ∃τ′⊑τ-Form ist trivialer Korollar.
- "Reduktion verbessert nur die Typinformation" lebt bei principal types:
  Types(e) ⊆ Types(e′) (Preservation) + Principality ⟹ principal(e′)
  covers principal(e). Improvement ist ein Zweizeiler — ALLES Risiko
  steckt in Principality selbst.

== Entscheidung (mechanisiert)
- L1-Principality WIDERLEGT ohne den Beweis zu versuchen:
  - finalized_no_blur: keine Substitutionsinstanz von {β} → ★ liegt
    ⊑-unter {(l: τ₀)} → τ₀ für τ₀ ≠ ★ (⊑-Rigidität von ★)
  - no_plain_principal_scheme: KEIN plain ∀ᾱ.τ-Schema ist instance-closed
    UND deckt found- + ⊥-Typisierung von λx. x.l — Result-Position muss
    Variable sein, Mixing erzeugt die unableitbare Instanz {ε} → {ε}
  - beide in minimal.lean, kompiliert (Sektion "Plain schemes are not
    principal")
- Konsequenz: L1 nur soundness-bearing, Principality lebt in L2.
  ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ ist der principal type, den plain schemes
  nicht ausdrücken können.
- L2-Draft in algorithmic.typ: instantiation-with-discharge (3-Wege-Regel
  spielt T-sel/T-sel-⊥/T-sel-★ pro Instanz nach — exakt der
  Regression-Beweis), restated Principality, Improvement-Korollar.
- Item 4 (type-substitution lemma) war schon DONE — proof-state war stale.

== Todo
- Claudes Drafts reviewen: DECIDED-Block + L2-Sektion in algorithmic.typ
  (v.a. discharge-Regel r = ? ⟹ θδ = ★ und der covering-order-Kandidat ⊴)
- ≐ᵣ Unifikations-Judgment ausarbeiten (P&X-Adaption, var-blocked
  transpositions) — letzter ungeklärter Baustein, blockt Soundness + T-eq-Fall
- Optional: no_plain_principal_scheme auf blurred Form verstärken
  (found-Familie bei zwei strukturell verschiedenen τ₀)
- Design-Findings in Thesis-Prosa heben (examples.typ TODO + escape-hatch
  Analyse)

== Fragen
- Covering order ⊴ auf qualified schemes: uniform in Γ quantifizieren oder
  Γ fixieren? (braucht das Improvement-Statement)
- Wie viel L2-Metatheorie beweisen vs. skizzieren? (Meeting)
- Discharge: volle Γ oder nur θ-Bild von ρ? (lookup_applySubst deutet auf
  Letzteres)
