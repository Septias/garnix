> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative and algorithmic system and provide the usual proofs.

We use a custom lookup relation ⟨ρ.l ↓ r⟩ with return values ⟨τ | ⊥ | ?⟩ to delineate the sources of uncertainty due to the wand-configuration. We want to extend this to proper gradual typing in the future and are still looking for breaking cases and problems with it.


## Related Files
- minimal.typ: provides a semi-formal method of a simpliefed typesystem (L1)
- minimal.lean: provides a fully formal version of minimal.typ
- algorithmic.typ: Algorithmic typesystem with qualified schemes
- algorithmic.lean: Root of the formal algorithmic system with unification
- In the bib/plaintext folder there is the plaintext version of relevant literature


# Progress
- [x] Scoped Records
- [x] Asymmetric Concat
- [x] Row Equivalence ≈
- [x] Refinement ⊑
- [x] Unknown Type Abstraction
- [x] Let-Statements
- [x] Qualified Schemes
- [x] Unification
- [x] Type Inference
- [x] FC-Labels
- [ ] Negative type information
- [ ] Occurrence Typing
- [ ] Patterns
- [ ] Recursive Types
- [ ] With
- [ ] Inherit


# Problems
> Problems found during mechanized proving and their proposed solutions


## Key-blocked spent promise  (open, deliberately kept)
- Only bites when the key is never supplied: applied, `(λr. λa. r.(a) c) {k = λz.z} ⌊k⌋` runs to `𝓫`
- No row to extend: the lookup waits on the KEY, so `Materialize` does not apply
- Declaratively typeable (`a : ⌊foo⌋`, `r : {foo: 𝓫 → β}`), so this is incompleteness, not a rejection
- Fix A — guess the key: F-key binds `α ≔ ⌊ℓ_fresh⌋`, then materialize; small, `runSound` carries over; answer valid but non-principal (made-up label in the type)
- Fix B — qualified top-level type: `Run` reports `∀. ⟨ρ.(α) ↓ 𝓫 → β⟩ ⇒ {ρ} → α → β`; principal (top level = `let main = e in main`), but `Run`/`RunSound`/`runF` and printed answers change; soundness becomes "every instance is typed"
- Guessing is fine inside a proof (inhabitation witness), not in a reported type → B preferred when taken up
- Same idea would extend A-let to key-blocked spent stumps (inhabitation: key ↦ fresh label, then `fillRow`)


## Symbols
- ↓: Row-lookup relation, three-way result r := (τ | ⊥ | ?)
- ★: Definite uncertainty, no elimination
- ⊑: Precision relation for ★
  - Every other type is below ★
- ≈: Row-equivalence relation
- ≤|≥: Instantiation relation for type-schemes
  - τ ≤ σ: τ is an instance of σ
  - σ ≥ τ: σ instantiates as τ
- ≐: Type unification
- ≐ᵣ: Row unification
- ⊴: "At least as general" (covering order on schemes) — σ ⊴ σ' : σ' covers σ
- ⊴⊑: covering up to precision — σ' answers each σ-instance with a ⊑ₜ-sharper one

## Properties
- ↓: deterministic, total, context-free; stable under substitution
- ⊑: reflexive, transitive (limmited)
- ≈: refl, symm, trans, congruence under |; adjacent distinct labels commute, ε is a unit
- ρ: rows mod ≈ form a trace monoid (partially-commutative, cancellative)

# Headliners
 ⊢ₗ₁ = `Typed`, ⊢ = `QTyped`.

**Lookup**
- lookup_det:   ρ.l ↓ r₁ → ρ.l ↓ r₂ → r₁ = r₂
- lookup_total: ∃ r, ρ.l ↓ r   (unconditional: ↓ is context-free, no L-α)
- LookupQ.applySubst: ρ.q ↓ r → r ≠ ? → (θρ).(θq) ↓ θr

**Row equivalence**
- rowEquiv_iff_char: ρ₁ ≈ ρ₂ ↔ Char ρ₁ ρ₂

**Type safety**
- preservation:  ∅ ⊢ₗ₁ e : τ → e ⟶ e' → ∅ ⊢ₗ₁ e' : τ
- qProgress:     ∅ ⊢ e : τ → (∃e', e ⟶ e') ∨ Value e ∨ Err e
  where Err e :≡ e = E[{b}.l] with l ∉ b, or E[{b}.(v)] with v not a label or v = l ∉ b
  (progress up to lookup errors: soft typing types ⊥-lookups at ★)
- qPreservation: ∅ ⊢ e : τ → e ⟶ e' → ∅ ⊢ e' : τ
- l1_strictly_weaker: ∃ e τ, ∅ ⊢ e : τ ∧ ¬ ∅ ⊢ₗ₁ e : τ

**Principality**
- selQ_principal: Principal ∅ (λx.x.l) selQ
    where Principal Γ e σ :≡ (∀τ ≤ σ, Γ ⊢ e : τ) ∧ (∃τ, τ ≤ σ) ∧ (∀τ, Γ ⊢ e : τ → ∃τ' ≤ σ, τ' ≼ τ)

**Unification** ρ₁ ≐ᵣ ρ₂
  - unifyRowM_success_mgu: ≐ᵣ = success s → θ_s ⊨ ρ₁ ≐ ρ₂ ∧ ∀θ ⊨ ρ₁ ≐ ρ₂, ∃θ' =_{ftv ρ₁ρ₂} θ, θ' ⊨ s
  - unifyRowM_success_iff: ≐ᵣ = success s → (∀θ ⊨ s, θ ⊨ ρ₁ ≐ ρ₂) ∧ (∀θ ⊨ ρ₁ ≐ ρ₂, ∃θ' =_{ftv ρ₁ρ₂} θ, θ' ⊨ s)
  - unifyRowM_clash_no_unifier: ≐ᵣ = clash → ∄θ, θ ⊨ ρ₁ ≐ ρ₂
  - unifyM_occurs_no_unifier:   ≐ / ≐ᵣ = occurs → ∄θ unifier   (supply avoids ftv)
  - unifyRowM_terminates / unifyTyM_terminates: ∃ fuel, ≐ᵣ / ≐ ≠ outOfFuel
  - stuck_masks_mgu, terminalNoMgu_false: stuck ⇏ no mgu, terminal ⇏ no mgu

**Inference**  Γ; S ⊢ e ⇒ τ; S′
- inferSound: Γ; S ⊢ e ⇒ τ; S′ → SchemesWF Γ → Clean S → Quiescent S →
  ∀σ, σ absorbs S′ → σ ⊨ S′ → ∀Γ', Γ ⇝_σ Γ' → parked(S′)σ; Γ' ⊢ₐ e : τσ
- runSound: Run e τ S′ → ∅ ⊢ e : τ⟦S′⟧
- runF_terminates: ∃ n, runF n e ≠ oof
- runF_eq_run:     runF n e ≠ oof → runF n e = run e
- run_typed:       run e = ok (τ, S′) → ∅ ⊢ e : τ⟦S′⟧


# Incompleteness
**Irreducible**
- Wand `(β|α) ≐ᵣ (l:𝓫)`: 
- Levi `(α|l) ≐ᵣ (l|β)`: 
- Shift `(α|l:𝓫) ≐ᵣ (l:𝓫|α)`: 
- Fix: negative info (Wand, Levi); row equations as scheme qualifiers

**Stuck with an mgu**
- Crossfield `(l:𝓫|α) ≐ᵣ (m:𝓫|β)`: cost of dropping U-expand
  - Fix: unique-host expansion as an *applied* binding, not a rename
- `stuck_masks_mgu`: stuck payload equation propagates before the residual pins β
  - Fix: defer the stuck equation, retry after the residual

**Spent promise** — 
- key-blocked spent stump never given its key, `λr. λa. r.(a) c` ⇒ no run
  - Fix: guess the key (non-principal) or qualified top-level type — see # Problems

**A-let premises**
- `Infer.letE` = Infer e₁ + `LetAdmissible Γ S S₁ ᾱ` + Infer e₂; Δ_q/Δ_Γ = `letQ`/`letG`
- Admissible: `fresh` (ᾱ ∩ ftv⟦S₁⟧(Γ, Δ_Γ) = ∅), `own`, `res` (results ⊆ ᾱ), `spent` (`LetSpent`)
- A failing condition prunes the offending variables (`greatestAlpha`), not the whole let
- Still lost: key-blocked spent stump; same-key spent results that differ beyond type variables (`LetSpent` is syntactic)
- Justified: Γ-mentioned (HM), Δ_Γ-mentioned, outer stump capture (`own`)
- `own` is NOT derivable from fresh + res + spent: `runSound_false_unowned_let` (LetSound.lean)
- Open: key-blocked spent stump at the let (key ↦ fresh label, then fill); semantic inhabitation not union-closed in general (D-? breaks under later substitution)

**Unrestricted T-★-intro**
- `λg. {a = g {l=c}; b = g {m=c}}` : `(★ → 𝓫) → {a:𝓫 | b:𝓫}`, `run` = clash
- Same trick types every stuck witness above (`f : ★ → 𝓫`)
- "clash is soundness" holds for ≐, not for inference
- Cause: T-★-intro = subsumption into a top; ★ joins any two types, ≐ only computes common instances
- Kept deliberately; thesis argues it as incompleteness
- Restrict to selection results: breaks `qPreservation` — blurred `{a=⌊l⌋}.a` steps to `⌊l⌋` (Qualified.lean:1260/1268)
- Blur only under selection: still incomplete — `λg. {a = g ({x={l=c}}.x); b = g ({x={m=c}}.x)}` clashes
- Fix A — completeness against the blur-free fragment (★ only from lookup verdicts)
- Fix B — join on clash: `τ ≲ α` edges, α ≔ ★ on clashing lower bounds; rigidity of ★ rejects eliminations; principal only on definite clash (unify-vs-★ choice stays open)


# Findings
- Eliminators for ★ can not be added without gradual typing because otherwise progress and preservation die
- D-hit up to ≈ (branch `discharge-equiv`, let-review §4.4): χ-correction gone; A-let drops linear-pattern, nodup, independence, disjoint results (P8d needed a fix in LetCase's inhabitation witness: fill with the ★-substituted result). Nested selection, record literal in a spent result, spent result over an unspent one now generalize.
- A-let in §6 shape: P2 (sorts) and P10 (unsolved) were unused by every proof, dropped; P3-P5 are the filters `letQ`/`letG`; P6 + P9 merged into `LetAdmissible.fresh`; `greatestAlpha` chooses from ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁) (`letCand`)
- `LetSpent` same-key clause: `q.stump = p.stump` → `Parked.spentAlike` (q spent, results equal after all type vars ↦ ★); witness sends generalized type vars to ★. `λx.{p=(x.l) c; q=(x.l) c}` now generalizes
