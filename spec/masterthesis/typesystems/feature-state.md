## Todo
- Fix T-let


# Feat: Negative information
Plan: plans/negative-information.md. Branch feat/negative-info.

## Open (for you)
- Q3 Phase C (keyed commuting under `κ ≠ m`) before or after occurrence typing?


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

## Problems
> Problems found during mechanized proving and their proposed solutions
### Key-blocked spent promise  (open, deliberately kept)
- Only bites when the key is never supplied: applied, `(λr. λa. r.(a) c) {k = λz.z} ⌊k⌋` runs to `𝓫`
- No row to extend: the lookup waits on the KEY, so `Materialize` does not apply
- Declaratively typeable (`a : ⌊foo⌋`, `r : {foo: 𝓫 → β}`), so this is incompleteness, not a rejection
- Fix A — guess the key: F-key binds `α ≔ ⌊ℓ_fresh⌋`, then materialize; small, `runSound` carries over; answer valid but non-principal (made-up label in the type)
- Fix B — qualified top-level type: `Run` reports `∀. ⟨ρ.(α) ↓ 𝓫 → β⟩ ⇒ {ρ} → α → β`; principal (top level = `let main = e in main`), but `Run`/`RunSound`/`runF` and printed answers change; soundness becomes "every instance is typed"
- Guessing is fine inside a proof (inhabitation witness), not in a reported type → B preferred when taken up
- Same idea would extend A-let to key-blocked spent stumps (inhabitation: key ↦ fresh label, then `fillRow`)


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

**Inference**  Γ; S ⊢ e ⇒ τ; S′
- inferSound: Γ; S ⊢ e ⇒ τ; S′ → SchemesWF Γ → Clean S → Quiescent S →
  ∀σ, σ absorbs S′ → σ ⊨ S′ → ∀Γ', Γ ⇝_σ Γ' → parked(S′)σ; Γ' ⊢ₐ e : τσ
- runSound: Run e τ S′ → ∅ ⊢ e : τ⟦S′⟧
- runF_terminates: ∃ n, runF n e ≠ oof
- runF_eq_run:     runF n e ≠ oof → runF n e = run e
- run_typed:       run e = ok (τ, S′) → ∅ ⊢ e : τ⟦S′⟧


## Incompleteness
**Irreducible**
- Wand `(β|α) ≐ᵣ (l:𝓫)`: 
- Levi `(α|l) ≐ᵣ (l|β)`: 
- Shift `(α|l:𝓫) ≐ᵣ (l:𝓫|α)`: 

**Stuck with an mgu**
- `stuck_masks_mgu`: stuck payload equation propagates before the residual pins β
  - Fix: defer the stuck equation, retry after the residual

**Spent promise** — 
- key-blocked spent stump never given its key, `λr. λa. r.(a) c` ⇒ no run
  - Fix: guess the key (non-principal) or qualified top-level type

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
- Not vacuous (2026-10-28): the rule has a premise and ★ has no eliminator; qProgress/qPreservation hold with qUnk in the relation. A `★ → β` parameter only accepts functions that ignore their argument
- Joining breaks principality without a clash: `g: x: {a = g x; b = g {l=c}}` runs to `({l:𝓫} → α) → {l:𝓫} → …` but also types at `(★ → 𝓫) → 𝓫 → …`; no scheme covers both (paper argument). Fix B never fires there; applied to `(y: c) c` it clashes
- MacroNix: `1 == true` types only via α ≔ ★, then atomEq = none → primFail. delta_prog will need cross-shape `==` = false
- Barely load-bearing: inference never builds qUnk (runSound lands blur-free; qtyped_sel_star is unused). The 𝓫-key preservation arms are dead (a label value never types at a base type), so 08_extensions.typ:144 is wrong. Real use: lem-subst (QSubst.lean:443/466), since a `?` can hit after substitution. qtyped_applySubst itself has no consumer (only the Axioms guard)
- No middle ground: the smallest blur that keeps lem-subst (blur at selections) already joins distinct types. Without the rule, substitution goes through qualified schemes (top-level schemes)
- The real Nix problem is the missing join: `map (p: p.name) [ {name="a"; x=0;} {name="b";} ]` clashes (checked) and is rejected declaratively with or without the rule (★ cannot be selected). Lists of packages, mkMerge, optionalAttrs. NixSuite (1) and (4)
- Gradual consistency fixes ★'s rigidity, not joins (int ≁ {}). With ★ chosen implicitly (T-★-intro, or ★ as a λ-domain) it becomes vacuous: `(x: x 4) 3` types and gets stuck. Fix A is a prerequisite
- Any join at if/lists brings subsumption back for preservation: `(λg. (λu. {a = g u; b = g (if false then 1 else {})}) (if true then 1 else {}))` steps to `g 1` with g : ★ → β
- Forcing survives removal: no_plain uses T-sel and T-sel-⊥ typings only; selQ_instance_closed (Qualified.lean:308) and qtyped_two_use (:1047) build no qUnk; qUnk/tUnk appear only as inversion arms. Coverage could drop from ≼ to ≈ (unchecked). Without the rule, `∀β. {β} → ★` is not instance-closed (instance `{(l:c)} → ★`), so plain schemes lose soundness, not only principality
- ⊥ plan (analysis/bottom-type.md, B): T-sel-⊥ at any τ makes `{ε} → {ε}` a typing, so no_plain_principal_scheme and l1_rejects_two_use (selEx_dom_empty_res) need a new mix: the hit instance re-pointed, or the `?` witness `{β} → ★`. Forcing then rests on `?` alone
- ⊥ as a declarative type breaks selQ_principal: T-λ-I picks `x : ⊥`, T-⊥-elim gives `x: x.l : ⊥ → τ`, which no record-domain instance covers. Mirror of the ★ gap: `y: [y + 1, y.a] : ⊥ → [int]` types, inference clashes. `⊥ ⊑ τ` (fn covariant) makes `⊥ → ⊥` a ≼-cover of every `τ → τ` for `x: x`. Fix: no ⊥ type, T-sel-⊥ concludes any τ (`undefined : ∀α. α`)


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
