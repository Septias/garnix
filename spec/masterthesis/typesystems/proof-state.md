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
- [ ] Patterns
- [ ] Occurrence Typing
- [ ] Recursive Types
- [ ] With
- [ ] Inherit


## FC-Labels  (`plans/fc-labels-plan.md`)
- [x] **Phase A — Reading dynamic**
- [x] **Phase B — dynamic construction**  (branch `fc-labels-phase-b-wip`, `plans/fc-labels-phase-b-plan.md`)
  - Keys are their own sort: `Key := l | α`, `⌊k⌋ : Ty`, `${k}: τ : Row`, `TySubst.lab`, `Sol.lab`; no junk keys
  - `{ ${e₁} = e₂ }`: lazy step to `{l = e₂}`, `qRcdDyn`, A-rcd-dyn (fresh label var κ, `τ₁ ≐ ⌊κ⌋`)
  - Headline `rcdDynQ_instance_closed`: `λa. λv. {${a} = v} :: ∀α δ. ⌊α⌋ → δ → {${α}: δ}`; runF infers exactly that
  - qProgress, qPreservation, runSound, runF_terminates: same axioms as before
  - Costs:
    - a non-label key is a type error (`{foo = c}.(c)`, `{${c} = c}` clash), was ★ + W-flag
    - keyed fields are barriers: `(foo: τ) ≐ᵣ (${α}: τ′)` and different unknown keys are stuck; keyed Fuzz universe 67% stuck
  - U-key: same unknown key at the head/tail of both spines → `τ ≐ τ′`, continue (`matchL` arm, `RowEquiv.dsing_cancel_left`)
  - Gained: `KeySafe` gone (F-★ never binds labels); `λr. {x = 1}.(r.a)` runs again; `α ≐ {${α}: σ}` solves (sorts differ)


# Problems
> Problems found during mechanized proving and their proposed solutions

## Spent promise at F-★  (fixed, merged 2026-09-28)
- `λx. λy. (x.l) y`: A-app writes `δ ≔ α → β` into a parked stump's result, F-★'s `δ ≐ ★` clashes (`no_finalize_of_spent`)
- Fix: `Stump.res : Ty B`; new finalization phase `Materialize` (F-hit) before F-★: blocker `r ≔ (l : res | r')`, then saturate
- `Run` = infer → `Materializes` → `Finalizes`; `runSound`, `runF_terminates` re-proved, same axioms
- Parked stumps are retired by stump, not by result: the parked-list invariant (`PInv`) is gone
- A-let generalizes spent stumps: `QScheme.WF` = result vars are binders; `Correctable` = linear pattern results (`Ty.correct`); inhabitation fills spent blockers (`fillRow`)
- What is left: # Incompleteness → Spent promise

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
- Wand `(β|α) ≐ᵣ (l:𝓫)`: `vars_vs_field_no_mgu_on`
- Levi `(α|l) ≐ᵣ (l|β)`: `two_sided_no_mgu_on`; var swap: `allvar_swap_no_mgu_on`
- Shift `(α|l:𝓫) ≐ᵣ (l:𝓫|α)`: `shift_no_finite_complete_set` — survives negative info
- Fix: negative info (Wand, Levi); row equations as scheme qualifiers

**Stuck with an mgu**
- Crossfield `(l:𝓫|α) ≐ᵣ (m:𝓫|β)`: cost of dropping U-expand
  - Fix: unique-host expansion as an *applied* binding, not a rename
- `stuck_masks_mgu`: stuck payload equation propagates before the residual pins β
  - Fix: defer the stuck equation, retry after the residual

**Spent promise** — fixed: `Stump.res : Ty B`, `Materialize`, spent stumps generalize
- `λx.(x.l).m`, `λx. x.l ‖ {m=c}`, `λx y.(x.l) y` now run (was: fail)
- Left: key-blocked spent stump never given its key, `λr. λa. r.(a) c` ⇒ no run
  - Fix: guess the key (non-principal) or qualified top-level type — see # Problems

**A-let premises**
- Γ-freshness, `LetResults`, independence, unresolved key ⇒ monomorphic let ⇒ later clash/stuck
- Spent stumps generalize only if the result is a linear pattern and its blocker can be filled
  - Not: record literal in the result (`(x.l) {a=c}`), same field spent twice on one row, key-blocked
- Independence also bites nested selection: `let g = λx.(x.l).m` used twice ⇒ clash
- Fix: independence could become ordered discharge; the rest are justified

**Unrestricted T-★-intro**
- `λg. {a = g {l=c}; b = g {m=c}}` : `(★ → 𝓫) → {a:𝓫 | b:𝓫}`, `run` = clash
- Same trick types every stuck witness above (`f : ★ → 𝓫`)
- "clash is soundness" holds for ≐, not for inference
