# Negative Information

Branch `feat/negative-info`, worktree `~/coding/garnix-wt/negative-info`.
State reports: `typesystems/feature-state.md` § Feat: Negative information (main).
Spec: `typesystems/negative.typ` (main). Draft: `drafts/negative-information.typ` (main).
Occurrence typing builds on this and starts after M5.


# Motivation
Scoped rows record only positive information: which fields a row has. They cannot say that a row variable has no field l, and that missing fact is exactly what the wand-ambiguity needs. In `a: b: (a ‖ b).l`, whether l comes from a or from b depends on whether b lacks l. Today the lookup stops at b's row variable and answers `?`, the stump is never decided, and the result is ★. The unification wand `(α | β) ≐ᵣ (l:𝓫)` stays stuck for the same reason. Keyed fields have the same problem: without `κ ≠ m`, a keyed field blocks every lookup that passes it.

NixLang programs establish absence routinely: `?`-guards, `removeAttrs`, closed attribute patterns, and the duplicate check of mixed literals. Negative information gives these constructs something to produce, and gives lookup and unification something to consume. We expect:
- definite types instead of ★ for the lookup wand and for lookups past keyed fields once absence is known
- the unification wand (incompleteness.md kind 1) solved by a forced move
- a faithful desugaring of mixed literals (`{ a = …; ${n} = …; }` ⇒ `n ≠ a`) instead of the current shadowing divergence
- the foundation for the next features: occurrence typing (`if x ? l` refines the else-branch with `x ⊬ l`), attribute patterns, and builtin schemes for `hasAttr` and `removeAttrs`
- in the thesis: the Lacks-Predicates section of @sec-extensions as a mechanized result rather than an outlook, and the conclusion's "most promising source of negative information" made concrete

Negative information does not fix everything. With context-free ≈, the Levi, swap and shift cases (incompleteness.md kinds 2-4) stay stuck, and the thesis text has to say so.


# Goal
- Two negative atoms, collected in a set Π:
  - row lacks `α ⊬ l` (row variable α has no l-field)
  - key disequality `κ ≠ l`, `κ ≠ κ′` (label variable is not that key)
- Lookup reads Π: `Π ⊢ ρ.k ↓ r`. Resolves the lookup wand `(a ‖ b).l` under `β_b ⊬ l`
- Unification respects Π. Resolves unification wand (incompleteness.md kind 1)
- Schemes carry Π: `∀ᾱ. Π, Δ ⇒ τ`
- Conservative: Π = ∅ is exactly L2. No existing definition or theorem changes


# Non-goals
- ≈ stays context-free. No Γ-relative ≈ (incompleteness.md kinds 2-4 stay stuck)
- No introduction by occurrence typing yet (`if x ? l`), see § Sources
- No lacks under a label variable (`α ⊬ κ`) before milestone M7 (§ Milestones). Decided in M7


# Design

## D1 Atoms and entailment
- `Neg := lacks (α : TyVar) (l : Label) | neq (κ : TyVar) (k : Key)`
- Π a finite set, entailment = membership plus symmetry of `≠`. No closure rules
- Consistency: no `κ ≠ κ`, no `κ ≠ l` after `κ ≔ l`

## D2 Lookup
- `LookupN Π` = `LookupQ` with two rules changed, everything else copied:
  - L-var-lacks: `α ⊬ l ∈ Π ⟹ Π ⊢ α.l ↓ ⊥` (L-conc-skip then continues past α)
  - key comparison `Key.cmpN Π`: `.undec` becomes `.apart` when `≠` is in Π
- Headliners:
  - `lookupN_empty_iff : LookupN ∅ ρ k r ↔ LookupQ ρ k r`
  - `LookupN.det`, `LookupN.total`, `LookupN.mono` (Π ⊆ Π′ refines `?` only)
  - `LookupN.applySubst : θ ⊨ Π ⇒ Π′ → Π ⊢ ρ.k ↓ r → r ≠ ? → Π′ ⊢ (θρ).(θk) ↓ θr`
  - `LookupN.equiv` (≈ respected, no Π needed since ≈ is Π-free)
- Lookup stays context-free in the thesis sense: Π is part of the judgement, not Γ

## D3 Substitution respects Π
- `θ ⊨ Π ⇒ Π′`: for `α ⊬ l ∈ Π`, `Π′ ⊢ (θα).l ↓ ⊥`; for `κ ≠ k`, `Key.cmpN Π′ (θκ) (θk) = .apart`
- `propagate θ Π : Option (Neg set)` computes the weakest Π′:
  - `α ≔ (m:τ | γ)` with `m ≠ l` ⇒ `γ ⊬ l`
  - `α ≔ (l:τ | …)` ⇒ none (lacks clash)
  - `α ≔ (${κ}:τ | …)` ⇒ `κ ≠ l` (the two atom kinds meet here)
- Headliners: `propagate_sound`, `propagate_complete` (every σ with `σ ∘ θ ⊨ Π` factors through Π′), `propagate_none_no_unifier`

## D4 Unification
- Phase A, no change to RowUnify: `unifyN Π = unify` then `propagate`
  - success: mgu θ of the plain problem; Π-respecting unifiers are exactly instances of θ satisfying Π′
  - propagate fails ⇒ no Π-respecting unifier (persistence of literal fields under substitution)
  - stuck, occurs: unchanged
  - trichotomy relative to Π-respecting unifiers follows from the existing four legs
- Phase B, one move: U-lacks-host
  - `(α | β) ≐ᵣ R`, `β ⊬ l` for every field label l of R, R var-free ⇒ `β ≔ ε`, `α ≐ᵣ R`
  - general: β lacks every label of R ⇒ β must be ε (counting)
  - headliner `wand_lacks_mgu`; regression on incompleteness.md kind 1
  - forced move, so trichotomy legs extend like U-host / U-key-pin did
- Keyed barriers (kind 8, contextual): `κ ≠ m` lets `${κ}:τ` commute past `m:σ` in the matcher only, not in ≈. Phase C, only if A/B are green

## D5 Declarative typing
- `QSchemeN := QScheme + Π_q`; instantiation requires `θ ⊨ Π_q ⇒ Π`
- `QTypedN Π Γ e τ`; T-sel family reads `LookupN Π`
- Headliners:
  - `qTypedN_empty_iff : QTypedN ∅ Γ e τ ↔ QTyped Γ e τ`
  - `qPreservationN`, `qProgressN` for closed e with Π = ∅ at top level
  - Π about λ-bound rows: discharged at application by `propagate` against the argument type
- Top level: a reported type may carry Π (`∀β. β ⊬ l ⇒ …`), same shape as Problems / Fix B

## D6 Inference
- A-sel uses `LookupN Π`; Π threaded like Δ
- Stumps wake on Π growth as well as on substitution (a `?` may become `⊥` or skip)
- Let: atoms on generalized variables move into the scheme, atoms on Γ-variables stay
- F-hit must not extend a blocker that lacks the label (cannot happen: lookup answers `⊥` first, prove it)
- Headliner: `runSoundN`, plus `runN_empty_eq` (Π = ∅ runs coincide with `run`)

## Sources (decision needed)
Nothing in MiniNix generates Π. Candidates:
- S1 mixed literal `{ a = e; ${n} = e′; }` ⇒ `n ≠ a` (Nix fails on duplicates). Fixes fc-labels.md §3.2
- S2 builtin schemes with Π: `removeAttrs`, `hasAttr`, `intersectAttrs`
- S3 `if x ? l then e₁ else e₂`: branch-local Π. This IS occurrence typing
- S4 closed attribute patterns `{a, b}: e`: exact rows, needs patterns first
- Proposal: S1 in M5, S2 as examples, S3 as the occurrence-typing feature


# Milestones
- M0 scaffold: `lean/Negative/`, lib glob, `Negative/Axioms.lean` own guard file, green build
- M1 Π, `Key.cmpN`, `LookupN` + D2 headliners
- M2 `⊨`, `propagate` + D3 headliners
- M3 `unifyN` Phase A + trichotomy relative to Π
- M4 U-lacks-host + `wand_lacks_mgu` + regressions (kind 1, lookup wand)
- M5 `QTypedN`, conservativity, preservation/progress, source S1
- M6 inference: `runN`, `runSoundN`, wake on Π
- M7 decide: lacks under label variables, Phase C keyed commuting


# Files
- new: `lean/Negative/{Atoms,Lookup,Subst,Unify,Typing,Infer,Regressions,Axioms}.lean`, `lean/Negative.lean`
- touched: `lean/lakefile.toml` (globs `Negative`, `Negative.+`)
- untouched: every existing `.lean` file. `Axioms.lean` untouched (own guard file avoids merge conflicts with the other features)
- main: `typesystems/negative.typ`, `drafts/negative-information.typ`, feature-state.md section


# Risks
- R1 Π-relative lookup reads like Γ-relative lookup, which minimal.typ argues against (L-α). Difference: Π is monotone and positive (membership), stability holds. Must be stated, not assumed: `LookupN.mono`
- R2 Phase A completeness hinges on "a literal field survives substitution". False for keyed fields `${κ}`: binding κ can turn a field into l. propagate must emit `κ ≠ l`, not succeed blindly
- R3 Preservation: definite types from L-conc-skip past a lacking variable are only sound if every value instantiating the variable lacks l. Needs Π discharged at application, not merely assumed in Γ
- R4 Stump wake-up on Π growth adds a second trigger; termination of the inference loop must be re-checked (Π over finite labels × vars, monotone ⇒ bounded)
- R5 Thesis claim "negative info solves Levi" is false with context-free ≈ (incompleteness.md kind 2). Fix the text, do not adopt Γ-relative ≈
- R6 Size: RowUnify is ~13k lines. Phase A avoids touching it; Phase B is one move. Do not start Phase C before B is green


# Open questions
- Q1 Sources: S1 + S2 now, S3 with occurrence typing? (proposal above)
- Q2 Report qualified top-level types with Π, or only use Π internally?
