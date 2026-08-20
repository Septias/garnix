
./26-08-19.typ
./26-08-21.typ

== Todo
- [x] Look at them rules in minimal.typ

== Fragen
- Recursive row-lookups due to recursive functions?
  - Yep schon
- Wofür Canonical Forms?
  - Für progress, damit wir steppen können.
- Kann man das Wand-Beispiel noch aufschieben?
  - Wahrscheinlich schon
- Do recursive rows have an effect yet?

== New Advancements
Claude has advanced the work by giving a pretty complete idea for the inference algorithm: the delayed lookups I proposed extend the let-generalized type-schemes (∀ᾱ τ) to qualified schemes [∀ᾱ C => τ]. Unification never sees the pending lookups and never has to guess a lookup (LUtail) — which loses solutions in P&X: (l: Int) ≐ᵣ (α | l: Int) has the mgu α ≔ ε but LUtail fails it; our failures are classified by the trichotomy instead. The unification leans on _forced steps_ due to the trace-monoid structure of rows (cancellativity, every step preserves the solution set) and stumps take guessing out of unification. A scheme with constraints ⟨ρ.l ↓ δ⟩ is the *principal* representation for this situation: ∀βδ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ is the principal type of λx. x.l that plain schemes can't express. Upon instantiation (which needs context Γ and turns into ≥_Γ) the return gets refined which is the design L2. Lookups that block on new variables re-state without committing ★ as the declarative system does. Only finalization commits ★.


== The problem with L1 and Principality
Another option would have been to commit to a type in the type-scheme. But plain schemes do not work together with principal types because they can not represent the multiple classes of types at once. The type-scheme can host both a direct lookup (${l: τ} -> τ$) and the ⊥-typing (${ε} -> ★$, lookup on the empty row is ⊥) but *instance-closedness* then forces a third typing ${ε} → {ε}$ (re-point the result variable at {ε} in the ⊥-instance's substitution) which is *invalid*.


== Misc
- A factoring is something that breaks a bigger thing into parts. It has to be complete: every atom gets paired (unifier = factorization, two distinct ones ⟹ no mgu; see glossar)
- We want to prove that if we terminate successfully, we get the mgu (most general unifier) — plus the trichotomy classifying both failure modes (clash = no unifier, stuck = no unique mgu)


== Fäden: Formalisierung
- Eliminators for ★
  - Können wir nicht wirklich einfügen. Aktuelles Design schneidet (entscheidet sich gegen) type-connectives
- FC-Labels hinzufügen
- Define the algorithmic version


