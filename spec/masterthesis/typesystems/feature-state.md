## Todo
- Fix T-let


# Feat: First-class labels
Plan: plans/fc-labels.md. Branch feat/fc-labels, worktree ~/coding/garnix-wt/fc-labels.

# Feat: Macro-Nix


## Fragen
- Was sind die Delta-regeln?
- Warum sind fails in Err gewandert?
  - Ist das um Progress zu cheaten?
- What is prim?
- How do builtins work if I rebind-them?


## Problems
### `Err.primFail` weakens progress (open)
- `primFail`: every saturated call with `δ = none` counts as ↯
- `SigOK` has no δ-progress law: δ need not be defined on well-typed arguments
- So a wrong δ (e.g. `add 1 2 = none`) is hidden as an error; `qProgress` still holds
- Plan D1 had the law ("δ defined, or a declared failure"); dropped during M1, not flagged
- Fix: declared failures in `Sig` (`fails : C → List (Expr C) → Prop`), `primFail` only on them, `SigOK.delta_prog`: well-typed values ⇒ δ defined ∨ declared failure
- MacroNix failures: `head`/`tail []`, `elemAt` out of range, div by 0, non-atomic `==`

### `==` on atomic values only (open)
- Nix compares records and lists structurally; here `==` fails on them (counts as ↯)
- Fix without new machinery: δ returns a term (as `map` does)
  - records with equal label sets: `&&` of field-wise `==`, else `false`
  - lists: `==` on heads `&&` `==` on tails, `[]` against `::` is `false`
  - functions: fail (as Nix)
- Fields stay lazy, forced only by the comparison

# Base-System
## Related Files
- minimal.typ: provides a semi-formal method of a simpliefed typesystem (L1)
- minimal.lean: provides a fully formal version of minimal.typ
- algorithmic.typ: Algorithmic typesystem with qualified schemes
- algorithmic.lean: Root of the formal algorithmic system with unification
- In the bib/plaintext folder there is the plaintext version of relevant literature


## Headliners
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

**Inference**  Γ; S ⊢ e ⇒ τ; S′, entry ⊢ e ⇒ σ
- inferSound: Γ; S ⊢ e ⇒ τ; S′ → SchemesWF Γ → Clean S → Quiescent S →
  ∀σ, σ absorbs S′ → σ ⊨ S′ → ∀Γ', Γ ⇝_σ Γ' → parked(S′)σ; Γ' ⊢ₐ e : τσ
- runSound: Run e σ → (∀τ ≤ σ, ∅ ⊢ e : τ) ∧ (∃τ, τ ≤ σ)
- merge_forced: two stumps asking one lookup get ≈-equal results at every θ discharging both
- runF_selEx_principal: the answer for λx.x.l is principal
- runF_terminates: ∃ n, runF n e ≠ oof
- runF_eq_run:     runF n e ≠ oof → runF n e = run e
- run_typed:       run e = ok (σ, S) → (∀τ ≤ σ, ∅ ⊢ e : τ) ∧ (∃τ, τ ≤ σ)


## Incompleteness
**Irreducible**
- Wand `(β|α) ≐ᵣ (l:𝓫)`: 
- Levi `(α|l) ≐ᵣ (l|β)`: 
- Shift `(α|l:𝓫) ≐ᵣ (l:𝓫|α)`: 

**Stuck with an mgu**
- `stuck_masks_mgu`: stuck payload equation propagates before the residual pins β
  - Fix: defer the stuck equation, retry after the residual
- another one here

**A-let premises**
- only greatest admissible α
 
**Unrestricted T-★-intro**
- Gap: T-★-intro is subsumption into a top, so ★ joins any two types while ≐ only finds common instances. `λg. {a = g {l=c}; b = g {m=c}}` types at `(★ → 𝓫) → {a:𝓫 | b:𝓫}` but clashes, and every stuck witness types the same way: a clash certifies "no unifier", not "no typing"

## Findings
- Eliminators for ★ can not be added without _gradual typing_ because otherwise progress and preservation die
- ≈ is intensional: counts include shadowed l-fields (2026-10-16)
