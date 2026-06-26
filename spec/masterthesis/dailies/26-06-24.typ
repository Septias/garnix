
== Fäden
- ∈-Algorithmus
- ∈-Reduction
- Rolle von ★ klären


== Fragen
- Wie Constraints auf Label-Variablen?
- Zwei Teile von ∈-constraints:
  - ||     fügt neue hinzu
  - (e: x) löst die auf?
- Wie mache ich


== ∈-constraints
- Ich kann nicht herausfinden, welchen Type ich daraus bekomme
  - Außer ich gehe da auch in Order vor… ?
  - Ja, die erste row-variables kann das bestimmen
  - Aber dann muss ich warten, bis die aufgelöst wird
  - `a: b : c: d: (d ⧺ c ⧺ b ⧺ a)`
  - Werden die immer alle aufgelöst?
  - Toplevel halt wieder nicht
  - Dann kann ich keinen Type geben
  - Solange bleibt der Type dann halt `_ -> ★`


== ∈-Eleminination einfach erklärt
- Die Idee kommt von dem Beispiel `x: y: (x ⧺ y).l`
- Hier können wir nicht festlegen, ob l aus x oder y kommt
- Wir können nur sagen: In einem der beiden muss es sein
- Und: x hat präferenz (was den finalen Typen angeht)
- Deshalb sammeln wir Constraints der Form `l ∈ (x ∪ y)`
- Aber was machen wir dann damit?
  - Auflösen bei Funktionsaufrufen: ?
  - Constraintslover: ?


== Todo
- Predicates not given as term syntax
- Wie genau sind labels in der Term-syntax?
- Matchingregeln auf Types, nicht auf Rows


== Adding Constraints

- TODO: hier müsste man wohl einen constraint-context machen?
- Sonst sehe ich nicht, wie die constraints bestehen bleiben
- Sowas wie: `Γ ⊢ a.l: (l ∈ ρ) ↝ ∇`?

Γ ⊢ a: { ρ }  ∇ += (l ∈ ρ)↓
-----------------------------------------
Γ ⊢ a.l: (l ∈ ρ)

// Collect all the row-variables for the ∈-constrain
(l ∈ ε)↓           = ε
(l ∈ (l': τ | ρ))↓ = ε        if l = l'
(l ∈ (l': τ | α))↓ = (α ∪ p↓)


// See if there is a type
l ∈ {}            = ★
l ∈ { α | ξ}      = ★
l ∈ { l': τ | ξ } = τ         if l = l'


== Solving Constraints
- TODO: jeder Funktionsaufruf braucht seine eigenen ∈-constraints!
- D.h: ich kann die nicht im globalen Kontext Γ speichern


x ∈ e₂    Γ ⊢ (x: e₁)e₂: τ
--------------------------------------------------- T-ok
Γ · (x ∈ S) ⊢ (x: e₁)e₂: τ


x ∉ e₂    S' = S∖x    Γ · (x ∈ S') ⊢ (x: e₁)e₂: τ
--------------------------------------------------- T-not-in
Γ · (x ∈ S) ⊢ (x: e₁)e₂: τ


x ∉ e₂
----------------------------- T-err
Γ · (x ∈ {x}) ⊢ (x: e₁)e₂: ★

