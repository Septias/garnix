## Todo
- Fix T-let


# Feat: Negative information
Plan: plans/negative-information.md. Branch feat/negative-info.


# Feat: Gradual Typing
Plan: plans/gradual.md. Branch feat/gradual.


# Feat: Macro-Nix
Plan: plans/macronix.md. Branch feat/macronix.

## Todo
- Check Nix examples against MacroNix


## Fragen
- Was sind die Delta-regeln?
- Wie sind builtins implementiert?
- Warum sind fails in Err gewandert?
  - Ist das um Progress zu cheaten?
- `==` on atomic values only!!
- Bool als tt und ff?
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

### Q4: differing `if` branches (open)
- A-if unifies the branches (`τ₁ ≐ τ₂`), A-cons the list elements: HM rule, fails without a common instance
- Incompleteness (declaratively typable, inference clashes):
  - `if as ? a then as.a else as.z`: ★ (definite miss) vs `int`; declaratively at ★
  - `[1 {}]`: `int` vs `{}`; declaratively at `[★]`
  - same root as Unrestricted T-★-intro: ★ joins any two types, ≐ only finds common instances
- Inexpressible (rejected declaratively too):
  - `{pname = "p";} // (if c then {a = 1;} else {})`, the conditional-extension idiom
  - only common type of `{a = 1;}` and `{}` is ★, and ★ cannot be concatenated
  - rows cannot say "a may be there"
- Options:
  - (a) keep the clash: sound, simple, documented. Current, recommended
  - (b) join at ★ in A-if / A-cons (+ warning): completes `as ? a` and `[1 {}]`, not the idiom; changes L2 inference
  - (c) ★ᵣ in Gradual: `{a: int} ⊔ {} = {★ᵣ}`, idiom types at `{★ᵣ | pname: string}`; only option that recovers it
  - (d) occurrence typing on `e ? l` (Negative Information S3): `as ? a` only (then-branch dead)


## Claude Prompts
- [x] Create an overview for feature parity with real nix → text/figures/nix-parity.typ, in the MacroNix appendix
  - Sections: language features, builtins
  - Section for things not yet in place (because of implementation or whatever)


# Feat: Recursive Types
TODO

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

**A-let premises**
- `Infer.letE` = Infer e₁ + `Merges S₁ S₁′` + `LetAdmissible Γ S S₁′ ᾱ` + Infer e₂; Δ_q/Δ_Γ = `letQ`/`letG`
- Admissible: `fresh` (ᾱ ∩ ftv⟦S₁⟧(Γ, Δ_Γ) = ∅), `own`, `res` (results ⊆ ᾱ), `spent` (`LetSpent`)
- A failing condition prunes the offending variables (`greatestAlpha`), not the whole let
- Still lost: spent results on one field that differ beyond type variables and that the merge does not identify (two rows: `let f = x: {p = (({m = c} ‖ x).l) c; q = x.l}` used twice clashes; the entry accepts it through a witness merge)
- Justified: Γ-mentioned (HM), Δ_Γ-mentioned, outer stump capture (`own`)
- `own` is NOT derivable from fresh + res + spent: `runSound_false_unowned_let` (LetSound.lean)
- Done (2026-10-29, syntactic): a key-blocked spent stump at the let, its key variable read as `${κ}`. Semantic inhabitation still has no union-closure proof (D-? breaks under later substitution)

**Unrestricted T-★-intro**
- Gap: T-★-intro is subsumption into a top, so ★ joins any two types while ≐ only finds common instances. `λg. {a = g {l=c}; b = g {m=c}}` types at `(★ → 𝓫) → {a:𝓫 | b:𝓫}` but clashes, and every stuck witness types the same way: a clash certifies "no unifier", not "no typing"
- Kept: inference never builds qUnk; the rule's one real use is lem-subst (a `?` can hit after substitution), and without it `∀β. {β} → ★` is unsound as a plain scheme. Restricting it to selections breaks qPreservation or still joins types. Not vacuous (★ has no eliminator), and the forcing of qualified schemes does not use it
- Fixes: state completeness for the blur-free fragment (A); joining at a clash (B) does not restore principality, since `g: x: {a = g x; b = g {l=c}}` has no covering scheme
- The real Nix gap is the missing join (lists of packages, optionalAttrs, `map (p: p.name) […]`), rejected declaratively too. Gradual consistency fixes ★'s rigidity, not joins, and an implicitly chosen ★ makes typing vacuous (`(x: x 4) 3`). ⊥ instead: analysis/bottom-type.md


## Findings
- Eliminators for ★ can not be added without _gradual typing_ because otherwise progress and preservation die
- ≈ is intensional: counts include shadowed l-fields (2026-10-16)
  - `Row.Char` compares the full `sProj l` (every l-field, shadowed or not), so `sFieldCount` over all fields is right for U-clash / U-ground
  - Leading-only counting is unsound: `⟨α, l:𝓫, β⟩ ≐ᵣ ⟨l:Int, l:𝓫⟩` would clash, but `α ≔ (l:Int), β ≔ ε` unifies (real verdict: stuck)
  - Dead fields (any l right of an earlier l, across vars) are unobservable: no restriction, E-conc only appends; observational ≅ ⊋ ≈
  - Cost: `[ base (base // {name = "y";}) ]` clashes ((name:S) vs (name:S | name:S)); `defaults // overrides` is the common Nix idiom
  - Shift witness `(l:𝓫)^k` (06_incompleteness.typ:21,26, UnifType.lean) is an artefact of ≈; under ≅ all k ≥ 1 collapse. Non-finitary likely survives via other labels (`offSub`), unproven
  - ≅ is not cancellative: `(l:Int | α) ≐ (l:Int)` has unifiers ε and (l:τ) for every τ, so no mgu; U-field / U-ground / lem-cancel need cancellation → keep ≈
  - Conjecture: ≈ is the coarsest lookup-respecting cancellative congruence (unproven)
  - Todo text: say in 02 (~l.104) that ≈ keeps shadowed fields and why; fix shift prose; fill "(explain this)" at 04_unification.typ:228
  - Idea (untested, not in Lean): drop dead fields at T-conc; needs ≅ in declarative T-eq, recheck preservation
