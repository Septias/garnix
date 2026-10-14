# Negative Information

Branch `feat/negative-info`, worktree `~/coding/garnix-wt/negative-info` (fast-forward to main first: MacroNix is in core since `692e664`).
State reports: `typesystems/feature-state.md` § Feat: Negative information (main).
Spec: `typesystems/negative.typ` (main, from M1). Draft: `drafts/negative-information.typ` (main).
Occurrence typing builds on this and starts after M5.


# Motivation
Scoped rows record only positive information: which fields a row has. They cannot say that a row variable has no field l, and that missing fact is exactly what the wand-ambiguity needs. In `a: b: (a ‖ b).l`, whether l comes from a or from b depends on whether b lacks l. Today the lookup stops at b's row variable and answers `?`, the stump is never decided, and the result is ★. The unification wand `(α | β) ≐ᵣ (l:𝓫)` stays stuck for the same reason. Keyed fields have the same problem: without `κ ≠ m`, a keyed field blocks every lookup that passes it.

NixLang programs establish absence routinely: `?`-guards, `removeAttrs`, closed attribute patterns, and the duplicate check of mixed literals. Negative information gives these constructs something to produce, and gives lookup and unification something to consume. We expect:
- definite types instead of ★ for the lookup wand and for lookups past keyed fields once absence is known
- the unification wand (incompleteness.md kind 1) solved by a forced move
- a faithful desugaring of mixed literals (`{ a = …; ${n} = …; }` ⇒ `n ≠ a`) instead of the current shadowing divergence
- the foundation for occurrence typing (`if x ? l` refines the else-branch with `x ⊬ l`), attribute patterns
- in the thesis: the Lacks-Predicates section of @sec-extensions (currently `todo`) as a mechanized result

Negative information does not fix everything. With context-free ≈, the Levi, swap and shift cases (incompleteness.md kinds 2-4) stay stuck. 07_incompleteness.typ:29 already says so.


# Goal
- Two negative atoms, collected in a set Π:
  - row lacks `α ⊬ l` (row variable α has no l-field)
  - key disequality `κ ≠ l`, `κ ≠ κ′` (label variable is not that key)
- Lookup reads Π: `Π ⊢ ρ.k ↓ r`. Resolves the lookup wand `(a ‖ b).l` under `β_b ⊬ l`
- Unification respects Π. Resolves unification wand (kind 1)
- Schemes carry Π: `∀ᾱ. Π, Δ ⇒ τ`; top-level answers report Π
- Conservative: Π = ∅ is exactly L2 (incl. MacroNix forms). No existing theorem statement changes


# Non-goals
- ≈ stays context-free. No Γ-relative ≈ (kinds 2-4 stay stuck)
- No occurrence typing (`if x ? l`) here: next feature (S3)
- No `removeAttrs`: needs row subtraction, scoped rows have none
- No lacks under a label variable (`α ⊬ κ`) before M7. Decided in M7


# Design

## D1 Atoms and entailment
- `Neg := lacks (α : TyVar) (l : Label) | neq (κ : TyVar) (k : Key)`
- Π a finite set, entailment = membership plus symmetry of `≠`. No closure rules
- Consistency: no `κ ≠ κ`, no `κ ≠ l` after `κ ≔ l`

## D2 Lookup
- `LookupN Π` = `LookupQ` (LabelLookup.lean:28) with two rules changed, everything else copied:
  - L-var-lacks: `α ⊬ l ∈ Π ⟹ Π ⊢ α.l ↓ ⊥` (L-conc-skip then continues past α)
  - `Key.cmpN Π`: `.undec` becomes `.apart` when `≠` is in Π
- Headliners:
  - `lookupN_empty_iff : LookupN ∅ ρ k r ↔ LookupQ ρ k r`
  - `LookupN.det`, `LookupN.total`, `LookupN.mono` (Π ⊆ Π′ refines `?` only, via `ResPrec`)
  - `LookupN.applySubst : θ ⊨ Π ⇒ Π′ → Π ⊢ ρ.k ↓ r → r ≠ ? → Π′ ⊢ (θρ).(θk) ↓ θr` (twin of `LookupQ.applySubst`)
  - `LookupN.equiv` (≈ respected, no Π needed since ≈ is Π-free)
- Lookup stays context-free in the thesis sense: Π is part of the judgement, not Γ

## D3 Substitution respects Π
- `θ ⊨ Π ⇒ Π′`: for `α ⊬ l ∈ Π`, `Π′ ⊢ (θα).l ↓ ⊥`; for `κ ≠ k`, `Key.cmpN Π′ (θκ) (θk) = .apart`
- `propagate θ Π : Option (Neg set)`, the weakest Π′, walking θα:
  - literal `m ≠ l`: drop; literal `l`: none
  - `${⌊l⌋}`: none; `${κ}`: `κ ≠ l` (the two atom kinds meet here)
  - variable γ: `γ ⊬ l`
  - `≠` atoms: `Key.cmp (θκ) (θk)`; eq: none, apart: drop, undec: keep
- Headliners:
  - `propagate_sound`
  - `propagate_iff : σ ∘ θ ⊨ Π ⇒ Π″ ↔ σ ⊨ propagate θ Π ⇒ Π″` (compositional over L-conc-skip)
  - `propagate_none_no_unifier`

## D4 Unification
- Phase A, no change to RowUnify: `unifyN Π = unify` then `propagate`
  - success: mgu θ of the plain problem; Π-respecting unifiers are exactly instances of θ satisfying Π′
  - propagate fails ⇒ no Π-respecting unifier
  - stuck, occurs: unchanged
  - needs Supply avoiding `ftv Π` (R7)
- Phase B, U-lacks-host, inside RowUnify
  - `UResM.stuck` carries no payload and the wand arrives nested (fn payload from inference), so no wrapper can reach it
  - oracle `lacks : TyVar → Label → Bool` threaded through `unifyM`; L2 = `fun _ _ => false`, `unifyM_noLacks_eq` by rfl
  - move: one side closed R (no row vars, no label-var keys), any spine variable β on the other side with `β ⊬ l` for every label l of R ⇒ `β ≔ ε`
  - forced (counting, `rowEquiv_iff_char`), so legs extend like U-host / U-key-pin (`keyPin`, Clash.lean:400, Draws.lean:535)
  - headliner `wand_lacks_mgu`; regression on kind 1
- Keyed barriers (kind 8, contextual): `κ ≠ m` lets `${κ}:τ` commute past `m:σ` in the matcher only, not in ≈. Phase C, only if A/B are green

## D5 Declarative typing
- `QSchemeN := QScheme + Π_q`; instantiation requires `θ ⊨ Π_q ⇒ Π`
- `QTypedN sig Π Γ e τ`, copy of `QTyped` incl. MacroNix forms (`prim`, `ite`, `has`); T-sel family reads `LookupN Π`
- T-rcd-mixed (S1): requires `Π ⊢ κ ≠ a` for each static label a
- Headliners:
  - `qTypedN_empty_iff : QTypedN sig ∅ Γ e τ ↔ QTyped sig Γ e τ`
  - `QTypedN.applySubst` under `θ ⊨ Π ⇒ Π′`
  - `qPreservationN`, `qProgressN` for closed e, any consistent Π
- Record values have var-free spines: Π about spine variables is vacuous for values. Preservation reduces to `applySubst` plus the instantiation side condition

## D6 Inference
- A-sel uses `LookupN Π`; Π threaded like Δ
- Binding a variable with atoms runs `propagate`; failure = clash
- Stumps wake on Π growth as well as on substitution (a `?` may become `⊥` or skip)
- Let: atoms on generalized variables move into the scheme, atoms on Γ-variables stay
- F-hit must not extend a blocker that lacks the label (cannot happen: lookup answers `⊥` first, prove it)
- `RunN` reports Π (new def, `Run` unchanged). Forced by S1: `n: {a = c; ${n} = c;}` as `⌊κ⌋ → …` without `κ ≠ a` is unsound at n = a
- Headliners: `runSoundN`, `runN_empty_eq` (Π = ∅ runs coincide with `run`)


# Sources
- S1 mixed literal `{ a = e; ${n} = e′; }` ⇒ `n ≠ a` (Nix fails on duplicates). Fixes fc-labels.md §3.2. In M5
- S3 `if x ? l then e₁ else e₂`: branch-local Π. Occurrence typing, next feature (`has`, `ite` on main)
- `hasAttr` = `?`, folded into S3. `removeAttrs` dropped (non-goal)
- S4 closed attribute patterns `{a, b}: e`: needs patterns first
- Until S3: the lacks half is tested with Π given directly


# Milestones
- M0 fast-forward worktree to main, scaffold `lean/Negative/`, lib glob, `Negative/Axioms.lean` own guard file, green build
- M1 Π, `Key.cmpN`, `LookupN` + D2 headliners
- M2 `⊨`, `propagate` + D3 headliners
- M3 `unifyN` Phase A + trichotomy relative to Π
- M4 lacks oracle in RowUnify, all legs, `unifyM_noLacks_eq`, `wand_lacks_mgu`, regressions (kind 1, lookup wand with Π given)
- M5 `QTypedN`, conservativity, preservation/progress, source S1
- M6 inference: `runN`, `runSoundN`, wake on Π
- M7 decide: lacks under label variables, Phase C keyed commuting


# Files
- new: `lean/Negative/{Atoms,Lookup,Subst,Unify,Typing,Infer,Regressions,Axioms}.lean`, `lean/Negative.lean`
- touched: `lean/lakefile.toml` (globs `Negative`, `Negative.+`), `lean/RowUnify/*` (oracle arm, M4)
- untouched: every other existing `.lean` file, `Axioms.lean` (own guard file)
- main: `typesystems/negative.typ`, `drafts/negative-information.typ`, feature-state.md section


# Risks
- R1 Π-relative lookup reads like Γ-relative lookup, which minimal.typ argues against (L-α). Difference: Π is monotone and positive (membership), stability holds. Must be stated, not assumed: `LookupN.mono`
- R2 keyed fields `${κ}`: binding κ can turn a field into l. propagate emits `κ ≠ l`, never succeeds blindly
- R3 definite types past a lacking variable: sound only via instantiation side condition and `QTypedN.applySubst`; discharge at application is D6
- R4 stump wake-up on Π growth adds a second trigger; re-check termination (Π over finite labels × vars, monotone ⇒ bounded)
- R5 Levi claim: done in 07_incompleteness.typ:29. Keep it that way in § Lacks-Predicates
- R6 RowUnify is ~13k lines. Phase A avoids it; Phase B is one arm. No Phase C before B is green
- R7 Supply must avoid `ftv Π`, else `σ =_{ftv ρ₁ρ₂} σ′` loses atom variables
- R8 oracle arm churns every leg (≈ U-host size). At most 3 attempts per leg, then report
- R9 `QTypedN` copy size. Over ~1.5k lines: ask before parameterizing `QTyped` (touches core)


# Open questions
- Q1 resolved: S1 now, S3 with occurrence typing
- Q2 resolved: report Π at top level (`RunN`)
- Q3 M7 Phase C before or after occurrence typing?
