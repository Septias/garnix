
> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative typesystem and extend it to an algorithmic one in a similar fashion.

Our contribution is a _lookup relation_ that tries to solve one motivating example: `a: b: (a || b).l` which is a lookup on a concatenation of two row-variables that can not be typed easily. This wand-example is actually unsolvable, even with our effort. The novelty of our approach is to lookup a type on a _best-effort_ basis and give back an unkown result ★ in the wand-example. Our lookup relation thus returns a result out of (τ | ⊥ | ?) where ⊥ symbolises definite absence of a field and ? means "we don't know" (yet). Our lookup relation `Γ ⊢ ρ.l ↓ r` is able to lookup row-variables in the context that were instantiated on application. This can also be done with normal substitution of type-variables, but already shows the algorithmic implementation.

This mechanism allows to _refine_ types on function application. See the example `x: ({l: τ} || x).l` of type `{β} → ★` since the lookup-relation can not look past the type-variable introduced by x. Only after instantiation, it becomes clear whether the label is _shadowed_ or not. Applying the argument `x = {}` promotes the unknown type ★ to τ because it becomes clear that x does not shadow the label defined in the literal record.

The type-safety proofs have to account for this new lookup-mechanism in two ways: Progress can only be proven for definite types, but ★ forms a boundary where programs can get stuck. The preservation proof has to account for type refinement by allowing types to become more precise during small steps.

Principality forces qualified schemes that use parked stumps during unification to get mgus in many cases. The algorithm outputs three solution: A sucees with MGU, a failure without MGU, stuck for the wand example and finally an outcome »occurs«, that cuts across the other output paths. The occurs class of outputs is a syntactic check for recursive row-variables that naturally occur in nix.


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
- [~] Unification
- [~] Type Inference
- [ ] FC-Labels
- [ ] Negative type information
- [?] Patterns
- [?] Occurrence Typing
- [?] Recursive Types
- [?] With
- [?] Inherit


# Property-Overview
*Soundness*
degrade rules removed (★ rigid) ✔     ┐
UnifyAcyclic ✔ →  ⟦S⟧ total  →  A-sel  ├→ InferSound ✔ → weaken ✔ + finalization ✔ + χ-correction ✔ → RunSound ✔
generalization lemma → A-let                ┘

*Termination*
unification measure ✔ ┐
A-let ᾱ ✔ (greatest) ├→ inferF ✔ (sound, terminates) → `run` is total → "the scheme W produces" is sayable
↝* wake-up closure ✔ ┘
                      + determinism up to α-renaming → W's output is unique

*Principality*
instance-closed  ←  RunSound
inhabited        ←  "stumps always finalize"  ←  spent promise (FALSE in general — needs one of its three exits)
covers ≼         ←  W exists  +  ⊴≼  +  COMPLETENESS (false 3 ways: spent promise, stuck, A-let premises)
                                    ↓
                        ∀e ∃σ. Principal Γ e σ   (stated: `GeneralPrincipality`)


## Unification
- Unification Outcomes
  - [x] success: *sound & complete, and NON-VACUOUS*
    - `s.toSubst` itself satisfies `s` and unifies the problem (`unifyRowM_success_mgu`)
  - [x] occurs: *no unifier*, through the whole driver (`unifyRowM_occurs_no_unifier`, 2026-09-26)
  - [~] stuck : *conservative, and there is NO general converse*
    - three no-mgu theorems, three witnesses
  
- [x] UnifyWF, UnifyAcyclic — PROVED 2026-09-26 (`RowUnify/Applied.lean`)
  - one invariant `Sol.Good V s`, tagged by sort: keys and mentions ⊆ problem vars, no binding mentions a key, keys consistent
  - gives `Applied` (so ⟦S⟧ = `toSubst`), `WF` at rank ≡ 0, `Sat s.toSubst s`
  - the duplicate-key "keystone" is closed by the same invariant
  - state level: `SolveTy.clean` / `SolveRow.clean` keep ⟦S⟧ idempotent across solved equations
  - lifted over the whole derivation: `Infer.clean`, `Finalizes.clean` (`lean/Absorb.lean`)

- **2026-09-23 — U-expand removed** (Stages 1a + 1b, ledger in `plans/drop-expand.md`)
  - the four expansion arms are `.stuck`; `ExpandR.lean`, `uniqueHost`/`expandL`/`expandR`/`NoHost`, the `DepGraph` and `Θ` are deleted (~2450 lines net)
  - REPRODUCE: `cd lean && lake build && lake build fuzz && lake exe fuzz`
  - *`Sol.Ranked` is vacuous*: 0 successes whose solution has any edge, `Sol.Applied` fails 0 times (was 1036 / 38 808 / 348)
  - *the occurs guard is LOCAL again*: `solveVarM_occurs_no_unifier` is unconditional; the driver-level lift is a plain induction
  - *`TerminalNoMgu` is REFUTED* (`terminalNoMgu_false` restored): neither `.stuck` nor terminality implies no-mgu
  - *cost*: 7.0 / 30.5 / 9.1 % of successes become `.stuck`, all of shape `(l:{a}) ≐ᵣ (b | a)`; tripwires `crossfield_stuck`, `unify_crossfield_mirror_stuck`
  - *on Nix-shaped code* (2026-09-26, `Regressions.nix_*`): only a λ-bound callback applied to two records extended by DIFFERENT fields over INDEPENDENT tails is lost; let-bound builders, same-field uses and literals all succeed


## Termination
- [x] unification terminates — `unifyRowM_terminates` (RowUnify/Termination.lean, 2026-09-26)
  - measure: (problem variables inside a fixed universe, size), lexicographic
  - `unifyRow` is a total function; `unifyRow_eq`: every run that answers agrees with it
- [x] saturation terminates — `satStep_wf` (`lean/OpenEnds.lean`, 2026-09-26): no infinite ↝* run from ANY state. K-repark does have a measure after all: (|Δ|, #unblocked stumps), lexicographic — K-hit/K-⊥ retire a stump, K-repark swaps an unblocked stump for a blocked one
- [x] A-let's ᾱ is canonical — `greatestAlpha_spec` (`lean/LetChoice.lean`, 2026-09-26, branch `infer`): admissible ᾱ are closed under union (`LetAdmissible.union`; correctability is saved by Δγ's own premise), `greatestAlpha` computes the greatest by deleting forced-out variables, `LetAdmissible.letE` bridges to the rule
- [x] inference terminates — PROVED 2026-09-26 (branch `infer`) about a real function. `inferF`/`runF` (`lean/InferFn.lean`): fuelled, executable, answers `ok`/`fail`/`oof`; every `ok` is a derivation (`inferF_sound`, `runF_sound`) and so a declarative typing (`runF_typed`, via `runSound`). `lean/InferFnTerm.lean`: every piece is STABLE (more fuel never changes a verdict) and SETTLES from a clean state (`inferF_terminates`, `runF_terminates`); `run` is the total function (`runF_eq_run`, `run_typed`). Ingredients: `unifyTyF_terminates`, `Sol.rowWF_toCtx` for lookups, `satStep_wf` for saturation
  - REPRODUCE: `lean/InferRuns.lean` — `#guard`ed runs, incl. the spent promise failing at finalization and witnesses for the kinds and Perm fixes
  - A-var's renaming is the supply's next |ᾱ| names; `FreshRenaming` and the binders' kinds are CHECKED at run time (a `fail`, never unsoundness). That they always pass is not proved (it is a completeness question)


## Inference
- [x] InferSound — restated (the `InferSoundA` of the commits up to defc4be; the name is free since the old statement was deleted) and PROVED 2026-09-26 (`inferSound`, `lean/LetCase.lean`). **`RunSound` is PROVED** (`runSound`, `lean/Finalization.lean`, 2026-09-26)
- **2026-09-26 — A-let probe** (`lean/LetSound.lean`). Two things were wrong before the proof could start:
  - *A-let generalized without Γ-freshness.* `λy. let z = y in z` inferred `a → b`; `runSound_false_unguarded_let` (RunSound is false against the unguarded rule) + `letAlias_not_typed`. FIXED: `Infer.letE` now requires `ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅`; `letAlias_infers_guarded` runs the program at `a → a`
  - *`InferSoundC` is false as stated* (`inferSoundC_false`): context under ⟦S′⟧, type under an arbitrary σ ⊨ S′ — `y:a ⊢ y : 𝓫`. Also not inductive at A-lam. The context must be read under the same σ, with the row environment discharged (what every step lemma already assumes)
  - *answer to the probe*: A-let does NOT need the backward half of `QCovers`. The soundness induction carries ∀σ, so qLet's instance-closed premise is fed by the IH for e₁ at σ₁ = (θ′∘ρ)∘⟦S₁⟧, one per instance θ′ — a GENERALIZATION lemma, not a transport. `SchemeImage` is only needed for `qtyped_applySubst`, which is off the critical path
- **2026-09-26 — A-let could generalize an OUTER stump away** (`runSound_false_let_captures`, LetSound.lean). `{a = λx. x.l, b = let y = c in c}`: Γ is empty at the let, so ᾱ = [x's row] passes Γ-freshness, and A-let files field a's stump under y's scheme. y is unused, nothing finalizes the stump, and a ends at `{r} → δ` with δ free; declaratively x.l at a free row is ★ only (`letCapture_not_typed`). RunSound was false even with the Γ-freshness fix. FIXED: `Infer.letE` now also requires (i) Δ_q holds no stump parked before the let, (ii) Δ_q's result variables ∈ ᾱ (`QScheme.WF`, which A-var's bridge needs; since 2026-09-26 read at S₁, `LetResults`), (iii) ᾱ ∩ ftv(Δ_Γ) = ∅ (what the generalization lemma needs; (iii) itself not witnessed)
- **2026-09-26 — the statement, restated** (`lean/InferSoundA.lean`). Items 1–3 of the probe, done:
  1. `InferSound`: Γ and τ under the SAME σ ⊨ S′ (`CtxRead σ Γ Γ′`, row environment discharged). No idempotence of ⟦S′⟧ needed, so the ρ∘⟦S′⟧ form was not necessary
     - `inferSound_of`: the statement IS INDUCTIVE — every rule but A-var and A-let proved, those two plus the parked-list bookkeeping (`InferKeepsA`) as hypotheses
     - `runSoundA_of`: joins it to a plain `QTyped` at σ, given that the final parked stumps hold (finalization) and `InstEquivCorrects`
  2. `SchemeRead`: a scheme enters Γ′ with binders renamed (`QScheme.renameBinders`), σ fixing the new binders and avoiding them. `SchemeRead.inst`: the read scheme's instance at χ is the algorithm's instance read under σ, and its constraints at χ are EXACTLY the σ-readings of the stumps A-var parks. `inferA_sound_var_step` is A-var's typing half
  3. `QTypedA`: assumptions are `ρ.l ↓ τ` with τ a TYPE (`Stump.at σ` reads the result under σ too), and `qVar` accepts assumed constraints. This replaces `QTypedC`, whose raw δ needed "σ has no opinion at δ" — NOT MONOTONE: a stump parked by e₁ and woken later has σδ ≠ δ, so the IH was unusable. Now a woken stump's assumption HOLDS, and `QTypedA.weaken` drops held assumptions: monotonicity and the old `QTypedCDischarge` cash-in are the same lemma, with no χ-transport
     - `qLet`'s premise: e₁ at EVERY instance χ with the constraints at χ assumed. That is what generalization gives, and the probe's item 4 (partial discharge) disappears into it
- what is left for `RunSound`:
  - ~~A-var~~ PROVED 2026-09-26 (`varCase`, via `Wakes.fate` + `KeepsS`). `inferSound_of_let`: **`InferSound` rests on the A-let case alone**
  - ~~A-let~~ PROVED 2026-09-26 (`letCase`, `lean/LetCase.lean`). Needed first:
    - `Absorb.lean`: ⟦S⟧ stays idempotent (`Infer.clean`), every derivation extends its start (`Infer.ext`), and σ∘⟦S′⟧ = σ travels back (`Absorbs.back`); `SoundAt` carries `Absorbs`, `InferSound` assumes a clean, quiescent start
    - `SchemeRead` renames binders and applies σ in ONE substitution (`readSub`), so no condition on σ at the new names; fresh names exist (`fresh_renaming_exists`)
    - `Infer.letE` changed: the scheme reads its constraints under ⟦S₁⟧ (`letScheme` — raw rows would chase the ORIGINAL tail from every instance), ᾱ avoids solved variables, Δ_Γ's ᾱ-freshness is read at S₁, and Γ-freshness is per variable at both sorts
    - the case: generalization is e₁'s IH at σ₁ = (χ ∘ readSub) ∘ ⟦S₁⟧; inhabitation sends every generalized result variable to ★, since each generalized stump is blocked on a generalized row variable (`lookup_blocked_subst`)
  - ~~`InferKeepsA`~~ PROVED 2026-09-26 (`lean/ParkedInv.lean`): `Infer.pinv_keeps` — every reachable state has its parked result variables issued by the supply and one stump per result variable (`PInv`), and every parked stump is kept (same stump) or discharged at any σ ⊨ the later state (`KeepsS`). Needs Γ's schemes well-formed (`QCtx.SchemesWF`), which A-let's new premises maintain. `inferSound_of_cases`: `InferSound` follows from the A-var and A-let cases alone
  - **CONFIRMED 2026-09-26 — A-var's names are not reserved** (`lean/FreshNames.lean`). `FreshRenaming` only avoids names the state already uses, so a later `draw` can reissue one. `let g = λx. x.l in {a = g, b = λz. z.m}` runs from the empty state against the unguarded rule (`nameReuse_infers_unguarded`) and ends with two parked stumps on different rows and labels sharing result `natName 6` (`nameReuse_shared_res`); retiring either retires both (`nameReuse_filter_drops_both`). So the invariant `InferKeepsA` needs was FALSE. FIXED: `Infer.var` draws its renaming from the supply (each new name is `natName k`, k in `[S.supply.next, Sup.next)`, wake-up runs at `Sup`); monotype uses go through `Infer.var_mono`. The invariant is now proved (`Infer.pinv_keeps`)
  - ~~`InstEquivCorrects`~~ — FALSE for arbitrary schemes (`instEquivCorrects_false`: two constraints on one δ can find ≈-equal but different types). PROVED for `QScheme.Correctable` schemes (`QScheme.Correctable.correct`): result variables bound, one constraint per result variable, and no constraint row mentions a result variable — then correcting χ at the result variables changes no lookup. D-hit stays exact (decided 2026-09-26). `QTypedA.qLet` carries `Correctable`; A-let supplies it via a new premise: no generalized stump's row, read at S₁, mentions another generalized stump's result (completeness cost only for let-bound functions that concatenate a selection result into a record they then select from while it is still open)
  - **2026-09-26 — Day 10, cleanup.** `lean_verify` clean (propext, Classical.choice, Quot.sound; no source warnings) on `runSound`, `inferSound`, `unifyRowM_success_iff`, `unifyWF`, `unifyRowM_occurs_no_unifier`, `unifyRowM_terminates`. Superseded statements deleted: the old `InferSound` (parked = [] form), `QTypedCDischarge`, `runSound_of_inferSoundC_nil`, `Finalize.dischargeEquiv`/`discharge_isUnk` (the `hfix` forms), `lookup_unknown_of_blocked`, `inferC_sound_selUnk_step`, `QScheme.ResWF`, `InstStumps.pairwise`. `InferSoundC`/`QTypedC` stay as what `inferSoundC_false` refutes. Renamed: `InferSoundA` → `InferSound` (also `inferSound`, `inferSound_of`, `inferRecSound_of`, `VarCase`, `LetCase`, `varCase`, `letCase`); `QTypedA`, `InstA`, `runSoundA` keep the suffix because the plain name is taken. `Run` no longer requires `S′.parked = []` — `runSound` never used it, so `RunSound` got stronger
  - **`runSound`: `RunSound` holds.** A run from nothing types its program, at the type it reports under its own final substitution, in the empty context
  - ~~finalization~~ PROVED 2026-09-26 (`Finalizes.holds`, `lean/Finalization.lean`): F-★ fires on a stump blocked on a free row variable and writes nothing at the row sort, so at the final ⟦S′⟧ the lookup is still `?` and δ is ★ — D-?. The old `hfix` side condition is now a theorem, at ⟦S′⟧. **`runSound_of_corrects`: `RunSound` holds given `InstEquivCorrects`.** `RunSound` itself was restated in the empty context (it read `S′.applyCtx ∅`, whose row environment is ⟦S′⟧'s row solutions); no refutation depended on that
  

## Principality
- [x] covering order on schemes ⊴ 
- [x] `Principal selQ (λx.x.l)`
- [~] General principality — STATED as `GeneralPrincipality` (`lean/OpenEnds.lean`), not proved. The covering conjunct is algorithmic completeness, which fails three ways (spent promise, stuck, A-let premises), so it can hold only for a fragment; choosing it is a thesis decision


# Problems
> Problems found during mechanized proving and their proposed solutions

- [x] **A-let cannot generalize over an instance's variables** (INCOMPLETENESS, found 2026-09-26; FIXED 2026-09-26 on `infer`: `Infer.var` carries `S.kinds.Assigns σ.vars κs` and records `(σ.vars.map f).zip κs` — a let binder's kind is already in the state, since `Infer.kinds_mono`; no `QScheme` change. Positive witness pending `inferF`).
  A-var draws its renaming from the supply but records no KIND for the drawn
  names, and A-let's `KEnv.Assigns ᾱ κs` premise requires a recorded kind for
  every generalized variable. So in `let h = λy. g y` the variables of g's
  instance can never be generalized, and h is monomorphic in them. Not a
  soundness issue. Fix: `QScheme` carries binder kinds and A-var records them
  on the draw, the way `draw` records `fresh α: κ`.

- [x] **A-let splits Δ₁ as a PREFIX** (INCOMPLETENESS, found 2026-09-26; FIXED 2026-09-26 on `infer`: `S₁.parked.Perm (Δq ++ Δγ)`, proofs went through membership unchanged).
  `Infer.letE` asks `S₁.parked = Δq ++ Δγ`, not a partition. Δ₁ is ordered by
  parking time, so if e₁ parks a Γ-stump in front of a generalizable one, the
  latter cannot be generalized. Sound; read off the rule, no Lean witness yet. Fix: `S₁.parked ~ Δq ++ Δγ`
  (`List.Perm`); the soundness lemmas read Δ₁ through membership, so the proof
  should absorb it.

- [x] **A-let cannot generalize a stump whose RESULT was aliased** (INCOMPLETENESS, found 2026-09-26 by running `inferF`; FIXED 2026-09-26 on `infer`).
  `let g = λx. x.l in let h = λy. g y in {a = h {l = c}, b = h {}}` failed with a clash: inside h, A-app's
  `δ ≐ β` binds δ ≔ β, and A-let wanted each generalized result to be an unsolved binder. Now A-let reads the
  result AT S₁: `SolverState.resVar` (δ itself while unsolved, the variable it was aliased to otherwise),
  `letScheme` puts that variable in the constraint, and the premise is `LetResults` — each generalized result
  reads as a variable in ᾱ, and distinct generalized stumps read as distinct variables (one constraint per
  result; an alias can merge two). The correctability premise reads results at S₁ too. For an unsolved result
  all of this is the old rule. `letCase` needed only the local reading (`(hres.1 p hp).1` in place of
  "δ ∉ dom"); `LetChoice` keeps union closure (the new injectivity is saved by Δγ's premise, as correctability
  was). The program now runs at `{a: 𝓫 | b: ★}` (`InferRuns.lean`).

- [x] **★ in an elimination position** — DECIDED 2026-09-26: ★ stays rigid,
  no ★-eliminators. `A-app-degrade` / `A-sel-degrade` are deleted, so a stuck
  or occurs verdict rejects the program, like a clash. ★-elimination with
  blame is paper-only ("Towards Nix").


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
- ↓: deterministic, monotone, total (under RowWF)
- ⊑: reflexive, transitive (limmited)
- ≈: refl, symm, trans, congruence under |; adjacent distinct labels commute, ε is a unit
- ρ: rows mod ≈ form a trace monoid (partially-commutative, cancellative)

## Proof Overview
Proofs are for _closed_ programs (Γ = ∅). e ↯ marks _lookup-errors_: a selection reached a record literal without the label. ★ makes such programs typeable (now also via T-sel-⊥), so progress only holds up to ↯. 

*Progress*: If Γ = ∅ and Γ ⊢ e: τ, then `Progress e`
  - step: ∃e' with e → e'
  - done: or e ∈ Values
  - err: e ↯

*Preservation*: If ∅ ⊢ e: τ and e → e' then ∅ ⊢ e'
*Soundness*: If ⊨ e: τ then ⊢ e: τ
*Completeness*: If ⊢ e: τ then ⊨ e: τ

## Lemma Overview
### Declarative (L1)
- Progress & Preservation
  - *record inversion*: T-eq and T-★-intro can wrap any derivation and have to be stripped; each inversion gains a `∨ τ = ★` disjunct (harmless for canonical forms since fn/rcd heads ≠ ★).
  - *head rigidity*: ≈ₜ never changes the head constructor, so we can get "back" our shape. Now includes ★-rigidity (★ ≈ σ ⟹ σ = ★) because T-★-intro lives outside ≈.
  - *lookup-equivalence*: Lookup-category (τ | ⊥ | ★) is not changed by row-equivalence.
  - *term/type agreement*: Lookup on types carries over to syntax-lookup
- Progress:
  - *canonical forms*: A value's syntactic shape is determined by its type's head.
  - *scheme non-vacuity*: Every scheme has its own body as instance (θ = id).
- Preservation:
  - *polymorphic substitution*: if x: σ and v types at every instance of σ, then e[x:=v] keeps its type
    - *context conversion*: typing only sees the context through lookups, so contexts that agree on lookups type the same terms. Subsumes weakening, exchange and shadowing.
    - *rowEnv congruence*: lookup only depends on row-solutions, so substitution leaves lookups untouched
  - *spine-var-freeness*: literal rows carry no row-var in their spine, so no ★
- Refinement:
  - *lookup monotonicity in ⊑-vocabulary*: Γ ⊑ Γ' sharpens a lookup — definite results survive on the nose (monotonicity), ? re-resolves via totality (needs Γ'.RowWF).
  - *⊑-rigidity*: below anything but ★ sits only the same head constructor; ★ sits only below itself. 
  - *★-typeability of selections*: a selection on a record-typed term always types at ★
- Standalone Metatheory:
  - *determinism*: lookup is deterministic.
  - *monotonicity*: definite results (τ/⊥) survive extending the row-solutions, only ★ can improve
  - *totality*: under acyclic row-solutions (RowWF) every lookup has a result
  - *substitution stability*: definite lookups survive type substitution
- Type substitution & generalization:
  - *type-substitution lemma*: typing transports along θ into a context whose schemes θ-cover the originals (typed_applySubst_aux); the ?-selection case re-derives through T-sel + T-★-intro / T-sel-⊥ / T-sel-★ per the substituted lookup
  - *scheme renaming*: capture-avoiding renaming of scheme binders against a finite avoid-set (renameScheme) — the only place fresh names are needed
  - *syntactic let*: the standard HM generalization rule (one derivation + ᾱ ∩ ftv(Γ) = ∅) is admissible for instance-closed T-let (tLet_syntactic)
- Principality refutation:
  - *no blur factoring*: no substitution instance of the L1-finalized {β} → ★ sits ⊑-below a found-typing {(l: τ₀)} → τ₀ with τ₀ ≠ ★ (finalized_no_blur)
  - *no plain principal scheme*: no ∀ᾱ.τ scheme is instance-closed while having both the found-typing and the ⊥-typing of λx. x.l as instances (no_plain_principal_scheme) — plain schemes cannot be principal; qualified/stump-carrying schemes (L2) are forced

### Algorithmic (L2) 
- Qualified schemes:
  - *L2 TYPE SAFETY*: qProgress + qPreservation — the qualified system is safe in
    its own right, not via L1. 
  - *plain embedding*: Q = ∅ degenerates ≥\_Γ to the Γ-independent Scheme.Inst
  - *discharge determinism*: Row discharge is deterministic
  - *definite-stability*: a resolved stump never re-checks, wake-up only improves
  - *instance-closedness*: EVERY ≥\_Γ-instance of selQ = ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
    is a declarative typing of λx. x.l, in ANY Γ (selQ_instance_closed) — the three
    discharge cases replay T-sel / T-sel-⊥ / T-sel-★ per instance
  - *L1 ⊆ L2*: Typed.toQ embeds every plain derivation (Q = ∅ instances)
- IS L2 »SOUND & COMPLETE«? — audit 2026-09-13, build green, no sorries
  - *safety*: YES. qProgress/qPreservation are proven over ⊢_Q directly (Step/Value/Err
    reused from L1), preservation ON THE NOSE, axiom-guarded in Axioms.lean
  - *vs. L1*: BOTH directions — DONE 2026-09-18. ⊆ is Typed.toQ; the converse is
    `l1_rejects_two_use` / `l1_strictly_weaker` (Qualified.lean), so **L1 ⊊ L2 is a
    THEOREM**, no longer prose. The proof does NOT reuse no_plain_principal_scheme
    (that one is pinned to τ₀ = {ε} and to syntactic instance types); it re-runs the
    same argument mod ≈:
      * the `a` use pins a DEFINITE result — projecting label a out of the record
        type through `lookup_equiv` + `lookup_det` gives τa ≈ 𝓫_c, so the scheme has
        an instance whose result is 𝓫_c;
      * the `b` use forces an ε DOMAIN, and then instance-closedness forces that
        instance's RESULT to ★ (selEx_dom_empty_res, i.e. sel_var_unk read through
        the λ). A ★ domain is excluded outright: a selection on a ★-bound variable
        has NO typing at all (sel_var_of_unk), since every selection rule demands
        the scrutinee at a record type and ★ is ≈-rigid;
      * so σ.body's result position is a bare quantified variable, the domain cannot
        depend on it, and re-pointing it inside the ⊥-use's substitution yields the
        underivable instance {ε} → 𝓫_c.
    THE ONE STEP THAT IS NOT IN no_plain_principal_scheme is the last: there the
    ⊥-instance's domain was SYNTACTICALLY {ε}, here it is only ≈ {ε}, so "the domain
    does not mention the result variable" had to be earned. `Row.hasSing` +
    `rowEquiv_hasSing` + `hasSing_applySubst` + `applySubst_rowOnly` (Qualified.lean)
    do it: ≈ never creates or destroys a field, so a row ≈ ε is field-free ANYWHERE,
    a field-free row has no TYPE positions at all, and its substitution image
    therefore reads only θ.row — the re-point cannot reach it. That quartet is
    general and reusable; it is the field-count invariant in the form the
    mixed-instance construction needs.
    New L1 inversions this needed, none of which existed: `typed_let_inv'`,
    `typed_app_inv'` (typed_inv_aux only covers con/lam/rcd) and `tvar_inv` (the
    general-scheme `var_inst_inv`). `sel_var_unk` / `var_inst_inv` are no longer
    `private` in minimal.lean. Axiom-clean.
    A second, independent proof of the same theorem existed on
    worktree-prec-equiv-commutation (lean/Strictness.lean, 2026-09-15:
    `l1_strictly_weaker_than_l2` / `no_plain_scheme_two_use`). It was dropped as
    duplicate when that branch was merged — do not re-derive it.
  - *completeness w.r.t. inference*: NO, and not yet stateable. No W (algorithmic.lean is
    an import root); unifyRowM_success_iff is completeness for ≐ᵣ, not for ⊢_Q; and
    principality for L2 exists only as the single-example bookend qualified_principal_scheme
    (λx.x.l), not as "∀e ∃σ principal". The ⊴ order now exists (see Principality); what is
    still missing is a W and the covering conjunct for a scheme it produces
  - *watch item*: qLet's INHABITATION premise (∃τ₁. σ ≥_Γ τ₁) is non-standard — it exists
    because progress is false without it. Any future completeness proof must show inference
    always discharges it ("stumps always finalize")

- ≈-characterization:
  - *normal form*: rows flatten to spines
  - *the characterization*: ρ₁ ≈ ρ₂ iff same var sequence and all l-projections pointwise equal
  - *end-var cancellativity*: shared leading/trailing vars cancel
  - *full cancellativity*: any shared prefix/suffix row cancels 
  - *ground rows*: SpineVarFree ↔ empty var sequence
  - *Some examples*: Wand ambiguity & Regression

- The unification algorithm ≐:
  - *forced steps*: There is always a forced step we can take during unification that keeps mgus
  - *field-count invariant*: ≈ preserves l-field count; substitution only increases count
  - *projClash soundness*: projClash s₁ s₂ → no unifier (projClash_no_unifier)
  - *SUCCESS SOUNDNESS*:
    - *MOVE-REFLECTION lemmas*: θ unifies the residual ofSpine tᵢ ⟹ θ unified the original ofSpine sᵢ"
    - *U-GROUND*: a field does NOT commute past a var, shadowing

