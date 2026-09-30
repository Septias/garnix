# FC-labels Phase B: dynamic construction `{ ${e₁} = e₂ }` (handoff)

**Done and merged into main** (2026-09-30). Was: worktree `garnix-phase-b`, branch
`fc-labels-phase-b-wip` (both removed). Lean project: `spec/masterthesis/lean`.
Standing rules: don't edit `thesis.typ` or `dailies/`; record findings in
`typesystems/proof-state.md` at the end; gate each step on `lake build` green,
no `sorry`, and unchanged Axioms.lean messages. Keep `runSound`, `run_typed`,
`runF_terminates` green.

Background: Phase A (dynamic selection `e.(e')`) is merged (38df0b1). The
original plan is at `git show dcedff4^:spec/masterthesis/plans/fc-labels-plan.md`.
That plan left Phase B on paper only. This one mechanizes it.

## Status

| Step | State |
|---|---|
| B1 syntax | done |
| B2 lookup | done, incl. Infer's blocker relation (`LookupBlockedQ` keyed by `Ty`) |
| B3 ≈ with barriers | done |
| B4 row unifier | done |
| Infer.lean and downstream | done: merged main (Materialize/F-hit, Stump.res : Ty), `lake build` green, no sorry (53d3fe2) |
| key sort | done (39e5718): keys got their own sort, resolves ★ keys |
| B5, B6, B7, B0 | done (9ebcada); `lake build` green, no sorry, Fuzz clean |
| proof-state.md | updated on this branch |

## Done since the handoff (settled)

- `LookupBlockedQ`: varFree / sunk / dunkQ (key var blocks) / dunkF (field key var blocks) / catSkip / catUnk; `LookupVBlocked` is an abbrev; `LookupBlockedQ.lit`/`toLit` bridge to `LookupBlocked` (which gained `dunk`)
- `lookupQ_blocked_subst` now concludes `?` outright. The key blocker must go to a variable nothing else in key position reaches (`KeyFresh`); "not a label" no longer suffices with structural junk
- A-let: a spent stump must be `Parked.fillable` (label key AND blocker not a type-sort variable of its row); LetChoice pruning unchanged otherwise
- F-★ premise `Parked.KeySafe`: a key blocker is no pending answer. `Finalizes.holds` carries `SolverState.Untouched` through later steps
- `lookupQF`: one structural keyed lookup in InferFn; `lookupF`/`lookupVF` gone
- Axioms guards: `lookup_applySubst`, `Sol.lookup_applySubst_closure`, `LookupQ.det` now [propext]; `LookupQ.total` axiom-free; `Ty.mem_tyFtv_iff_sortedFtv` → `Ty.mem_tyFtv_of_sortedFtv`
- Costs: `λr. {x = 1}.(r.a)` no longer runs (outer stump key-blocked on inner's answer, KeySafe refuses; F-★ on inner first leaves outer ⊥, no F-⊥ rule). InferRuns unchanged

## Resolved: ★ keys → key sort

- Problem was: `{${e₁} = e₂}` with `e₁ : ★` breaks qPreservation; junk ≡ junk unsound at run time
- Decision: keys get their own sort. `Key := lit l | var α`; `Ty.lab : Key → Ty`, `Row.dsing : Key → Ty → Row`
- `TySubst.lab : TyVar → Key`, `Sol.lab`, `Srt := ty | row | lab`; `unifyKey`/`bindLab` is the label arm
- `Key.cmp`: eq (same lit / same var), apart (different lits), undec otherwise; no junk
- A-sel-dyn / A-rcd-dyn draw a label var κ and solve `τ ≐ ⌊κ⌋`; non-label keys clash
- `KeySafe` removed: F-★ never binds labels (`solve_star_dom`: `s.row = [] ∧ s.lab = []`)
- Costs: see proof-state.md; same-unknown-key incompleteness closed by U-key (729fd84)

## Design as implemented before the key sort (superseded where it says `Ty` key, keyClass, junk, KeySafe)

- **`Row.dsing : Ty B → Ty B → Row B`** is a keyed field `${q}: τ`. `Row.sing` is unchanged.
  - `applySubst (.dsing q τ) = .dsing (θq) (θτ)`. It does NOT normalize, so
    `applySubst_id` and `applySubst_fixed` survive.
  - ≈ has `dsingLab : .dsing (.lab l) τ ≈ .sing l τ` and the congruence
    `dsing : q.keyClass = q'.keyClass → τ₁ ≈ τ₂ → …`.
- **Key classes** (`Ty.keyClass`, minimal.lean) are `lit l | var α | junk`. All junk
  keys compare **eq** to each other. `KeyClass.cmp` yields `eq | apart | undec`:
  - lit/lit → eq or apart
  - var α/var α → eq
  - lit/junk → apart
  - junk/junk → eq
  - anything else → undec

  `Ty.keyCmp k q := k.keyClass.cmp q.keyClass`. Both eq and apart are stable
  under substitution (`Ty.keyCmp_applySubst`).
- **Lookup.** There is one relation, `LookupQ : Row → Ty → LookupRes → Prop`
  (LabelLookup.lean), with constructors emp/hit/miss/sunk (against `.sing` via
  `keyCmp k (.lab l)`)/varFree/catHit/catSkip/catUnk/dhit/dmiss/dunk.
  - `LookupV ρ α r := LookupQ ρ (.var α) r` is an abbrev. `LookupQ.lab_iff` bridges to
    the label relation `Lookup` (minimal.lean), which also gained dhit/dmiss/dunk.
  - **L-junk is now structural.** A junk key walks the row: against literal fields
    it gets ⊥, against a row variable it gets ?. The old `LookupQ.junk` constructor is
    GONE, so `β.int` is now `?`, not `⊥`.
  - Qualified.lean has `QTypedBody.lookupQ_junk_absent` for the qSelDyn
    preservation case.
- **Spines** (RowEquiv.lean) use `Atom.dfield : Option TyVar → Ty B → Atom B`:
  - `Atom.ofKey` maps lab→field, var α→dfield (some α), junk→dfield none.
  - `Atom.keyTy` maps back (none→`.unk`).
  - dfields are **barriers**: `Barrier = var | key (Option TyVar)`, with
    `sBarSeq` and `sDProj`.
  - `Row.Char := sBarSeq eq ∧ ∀ l, ProjEquiv sProj ∧ ProjEquiv sDProj`, and
    `sProj`'s index counts barriers.
  - `Row.Rigid` is used by `ground_char`.
- **Unifier** (RowUnify/Defs.lean):
  - A dfield counts as sHasVar, so it is a barrier. `sFieldCount` and `sLabels` skip it.
  - New helper `sHasKey`. `allVarsEmpty` returns none on a dfield. `windowExtract` stops at a dfield.
  - Key variables are split out as `Ty.keyFtv`/`Row.keyFtv`. A *weak* occurrence
    (only under a key) gives **stuck**, not occurs:
    - `bindTy`: tyFtv gives occurs, keyFtv gives stuck.
    - `solveVarM`: allRowVars gives occurs, unless the other side `sHasKey`, in which case stuck. Row.keyFtv gives stuck.
    - Reason: `α ≐ {${α}:σ}` IS solvable (by `{${★}:σ}`), so claiming occurs would be unsound.
    - The old `bindTy_ne_stuck`/`solveVarM_ne_stuck` were deleted (Trichotomy.lean).
  - `HostShape` gained `sHasKey s = false`.
- **Uncertainty.** Lookup uncertainty keeps the stump/★ route (no new
  constraint kind). An undecided dfield pairing in ≐ᵣ is **stuck, which rejects**.
  Parking stuck row equations is out of scope.

## (done) Make Infer.lean and downstream build

Infer.lean defines three blocker relations (lines ~40–195):
`LookupBlocked ρ l α` (over `Lookup`), `LookupVBlocked ρ α β`, and `LookupBlockedQ ρ q β`
with constructors `.lit`/`.var`.

In the Infer layer the blocker is only used via `toLookup(Q)`,
`unknown_blocked` (completeness) and `det`. No semantic "unsolved" lemma
remains, so any deterministic choice of blocker works. Recommended rewrite:

1. `LookupBlocked` (label key): add
   `dunk : LookupBlocked (.dsing (.var γ) τ) l γ`. A label against a key is undec
   only when the key is a var, by `Ty.keyClass_var_iff`. Fix `toLookup`,
   `Lookup.unknown_blocked` (add dhit/dmiss/dunk arms) and `det`.
2. Make `LookupBlockedQ` a **single relation keyed by `Ty`** that mirrors
   LookupQ's `?` rules, and drop the `.lit/.var` split:
   - `varFree : LookupBlockedQ (.var β) k β`
   - `sunk : LookupBlockedQ (.sing l τ) (.var α) α`. keyCmp k (lab l) = undec forces k = var α.
   - `dunkQ : keyCmp (.var α) q = .undec → LookupBlockedQ (.dsing q τ) (.var α) α`
   - `dunkF : (∀ α, k ≠ .var α) → keyCmp k (.var γ) = .undec → LookupBlockedQ (.dsing (.var γ) τ) k γ`
   - `catSkip : LookupQ ρ₁ k .absent → LookupBlockedQ ρ₂ k β → …`, `catUnk`

   Then provide the theorems `LookupBlockedQ.lit : LookupBlocked ρ l β → LookupBlockedQ ρ (.lab l) β`
   and its converse, and redefine `LookupVBlocked ρ α β := LookupBlockedQ ρ (.var α) β`
   as an abbrev. Old `.sing` uses become `.sunk`. Keep `LookupBlockedQ.varFree`.
3. Use sites to adapt (counts are from before the rewrite):
   - `LookupBlocked`: Finalization 2, LetCase 3, InferFn 3, Infer 21, InferSound 6, Axioms 3
   - `LookupVBlocked`: LetCase 1, Infer 14
   - `LookupBlockedQ`: LetCase 1, InferFn 5, InferFnTerm 1, Infer 21, InferSound 1

   `.lit h` becomes `LookupBlockedQ.lit h`, and for LookupQ `LookupQ.lab_iff.mpr h`.
4. Adapt the **old junk semantics**:
   - `LetCase.lean:59–108` uses the `.junk` constructor and a disjunction
     `LookupQ ρ q .unknown ∨ (LookupQ ρ q .absent ∧ ¬ q.IsQuery)`. A junk key can now
     be `?` or `⊥` depending on the row, so restate the lemma by cases on the
     LookupQ result (or via `LookupQ.total`).
   - `InferFn.lean:174–206` (`LookupV.not_found`, `.junk`) needs the same treatment:
     compute the result with the structural rules.
5. Then fix the rest of the chain in dependency order:
   - InferSound(A), Finalization, ParkedInv, Absorb, LetSound, InferFnTerm
   - Fuzz, Regressions, InferRuns
   - Axioms.lean: renamed or removed guards include `Ty.mem_tyFtv_iff_sortedFtv`
     (now `Ty/Row.mem_tyFtv_of_sortedFtv`, an implication with a keyFtv disjunct),
     `mem_ftv_iff` (now 3-way), `sSorted_toSpine`/`mem_sFtv_toSpine` (now
     implications), `bindTy_ne_stuck`, `solveVarM_ne_stuck`, `spineVarFree_iff_varSeq_nil`
     and the `LookupQ.junk` constructor.
   - Update the guard list rather than resurrecting old statements. Where a guard
     was deleted, say why in a comment. Fuzz may need `dsing`/`dfield` arms in
     its generators and printers.

## Remaining steps after the build is green

- **B5. Expression and runtime** (minimal.lean, Qualified.lean):
  - `Expr.rcdDyn e₁ e₂`, with RecBody staying literal.
  - Step `rcdDyn (lab l) v ⟶ rcd (field l v)`.
  - Err gains `E[{${v} = v'}]` for a v that is not a label.
  - `qRcdDyn : Γ ⊢ e₁ : q → Γ ⊢ e₂ : τ → Γ ⊢ {${e₁}=e₂} : {.dsing q τ}`.
    Preservation goes via ≈ `dsingLab`.
  - Re-prove qProgress/qPreservation.
- **B6. Inference.** A-rcd-dyn: infer e₁ ⇒ τ₁, e₂ ⇒ τ₂, result `.rcd (.dsing τ₁ τ₂)`
  with no new constraint, plus a W-flag if τ₁ has a non-query head. Cover it
  in Infer, InferSound(A), InferFn (`inferF`/`runF`), InferFnTerm, ParkedInv,
  Absorb and LetSound.
- **B7. Headlines.**
  - `(a: v: {${a}=v}) :: ∀α δ. ⌊α⌋ → δ → {α: δ}`, instance-closed.
  - A `{ ${n} = v; } // rest` run in InferRuns.
  - Extend Fuzz with dfields and record the stuck/rejection ledger.
  - Add Axioms guards.
- **B0. Regressions** (Regressions.lean):
  - `(foo: τ) ≐ᵣ (α: τ′)` does not clash.
  - `⟨⟩ ≐ᵣ (α: τ)` clashes.
  - `(α: τ).α ↓ τ` is stable under `α ↦ int`.
  - `α ≐ {${α}:σ}` is stuck, not occurs.
- **Finally:** update proof-state.md: tick Phase B, add the headlines, and record the new
  costs. Two costs are known already: L-junk is now structural (`β.int` is `?`), and weak key occurrences are stuck.

## Lean gotchas hit so far

- A `by` block inside `⟨…, …⟩` swallows the commas. Write `(by …)`.
- To do induction on a non-variable index, first
  `generalize hk : (Ty.lab l : Ty B) = k at h`.
- `Atom.keyTy` sometimes needs `(B := B)`.
- `RowEquiv.dsing rfl` can elaborate the wrong key. Use `refine .dsing ?_ ?_; rfl`.
- There is no Mathlib, so no `tauto`. For disjunction shuffling use
  `simp only [or_assoc, or_left_comm, or_comm]`.
- In a nested `if` inside unifier proofs, `(try split at h)` before case analysis works well.

## Verification

- `lake build` covers the whole project, including Fuzz and Axioms.
- Run `lean_verify` on `runSound`, `run_typed`, `runF_terminates`, `rowEquiv_iff_char`,
  `unifyRowM_success_mgu`, `qProgress`, `qPreservation` and the B7 headline. Only
  propext, Quot.sound and Classical.choice should appear.
