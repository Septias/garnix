-- INFERENCE SOUNDNESS, CASE BY CASE — THE FIRST ATTEMPT, AND WHAT SURVIVED IT.
--
-- This module is where the soundness proof started: ONE LEMMA PER A-RULE, each
-- stated against plain `QTyped` at a fixed pair (Γ′, σ) with σ ⊨ S′. Most of
-- the soundness chain now runs elsewhere:
--
--   `inferSound`  (InferSoundA.lean + LetCase.lean) — the mutual induction,
--                  over `QTypedA`, with parked stumps as typed assumptions;
--   `runSound`  (Finalization.lean).
--
-- What stays here is what that proof still USES or what is a finding in its own
-- right:
--   * the replay lemmas (`SolveTy.clean`, `SolveTy.unifies_sat`,
--     `Sol.lookup_sat`, `Sol.lookupQ_sat`) and the K-/D- correspondence (`Wake.dischargeEquiv`,
--     `Wakes.dischargeEquiv`), which ParkedInv.lean consumes;
--   * the plain-`QTyped` case lemmas, as the congruence and equation cases read
--     without assumptions;
--   * the saturation witnesses (`fStarEx_*`) and the spent-promise witnesses
--     (`spentEx_*`);
--   * `QTypedC` / `InferSoundC`, only because `inferSoundC_false` refutes them.
--
-- Removed as superseded (2026-09-26): `QTypedCDischarge` (never proved; the
-- `runSound_of_inferSoundC_nil`
-- (its hypothesis is refuted), `Finalize.dischargeEquiv` and its `hfix` side
-- condition, and `QScheme.ResWF` / `InstStumps.pairwise` (now the stump-keyed filters —
-- `Infer.keeps`, ParkedInv.lean). F-★ itself, and the refutations that shaped
-- its premise (`FinalizeUnguarded`, `finalize_star_no_discharge`), went with the
-- top-level schemes (2026-10-29, plans/top-level-schemes.md): a run's answer
-- keeps what is parked, so nothing is finalized.
--
-- ## Where the row environment goes
-- Nowhere: there is no row environment. A context binds term variables only,
-- and the solutions reach every lookup through σ. This used to be a side
-- condition (`Γ′.rowEnv = []`) carried by most statements in this file,
-- justified by what `QSubst.lean` found — after substituting,
-- `(Row.var α).applySubst σ` is `σ.row α` and there is no variable left for
-- `L-α` to chase, so carrying solutions alongside was never an option. With
-- `L-α` removed it is not a side condition but the shape of the relation.

import Infer
import RowUnify.Applied

namespace MinimalCalculus

--------------------- REPLAYING A SOLVED EQUATION UNDER σ ---------------------
-- Every A-rule that emits an equation needs the same three moves:
-- `Sol.Sat.comp_inv` peels the stage's own solution off the composite, success
-- soundness turns the emitted solution into a `TyUnifies`, and
-- `tyUnifies_applySubst_of_sat` strips the substitution the arm unified under.
-- Done once, here, rather than once per rule. Compare `SolveTy.unifies`
-- (Infer.lean), which stops one step earlier — on the types the ARM saw.

/-- ⊢  a solved equation holds under ANY σ satisfying the state it produced, and
holds of the ORIGINAL types, not the ones the arm unified. -/
-- ⊢  THE STATE STAYS IDEMPOTENT. `SolveTy`/`SolveRow` solve the equation on the
--    problem already substituted by ⟦S⟧, so the new solution's keys and
--    mentions avoid the state's keys (`Sol.Clean.clears_*`), and `extend`
--    composes exactly as `Sol.Clean.extend` needs. With this, ⟦S⟧ =
--    `S.subst` is a genuine closure (`Sol.closes_toSubst_of_applied`) at every
--    state these rules reach from a clean one.
theorem SolveTy.clean {B : Type} [DecidableEq B] {S S' : SolverState B} {τ τ' : Ty B}
    (h : SolveTy S τ τ' S') (hc : S.sol.Clean) : S'.sol.Clean := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  have hB := below_above S.supply (Ty.sortedFtv (τ.applySubst S.subst) ++
    Ty.sortedFtv (τ'.applySubst S.subst) ++ S.sol.domS)
  obtain ⟨R, hf, g⟩ := (unifyM_good fuel).1 _ _ _
    (fun x hx => hB x (List.mem_append_left _ hx)) hu
  refine hc.extend g (fun x hx => ?_)
  rcases List.mem_append.mp hx with hx | hx
  · rcases List.mem_append.mp hx with hx | hx
    · exact hc.clears_ty hx
    · exact hc.clears_ty hx
  · -- a drawn name is past ⟦S⟧'s keys
    exact fun hd => absurd (hB x (List.mem_append_right _ hd)) (Nat.not_lt.mpr (hf x hx).1)

theorem SolveRow.clean {B : Type} [DecidableEq B] {S S' : SolverState B} {ρ ρ' : Row B}
    (h : SolveRow S ρ ρ' S') (hc : S.sol.Clean) : S'.sol.Clean := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  have hB := below_above S.supply (Row.sortedFtv (ρ.applySubst S.subst) ++
    Row.sortedFtv (ρ'.applySubst S.subst) ++ S.sol.domS)
  obtain ⟨R, hf, g⟩ := (unifyM_good fuel).2 _ _ _ (fun x hx => hB x (by
    rcases List.mem_append.mp hx with hx | hx
    · exact List.mem_append_left _ (List.mem_append_left _ (sSorted_toSpine _ _ hx))
    · exact List.mem_append_left _ (List.mem_append_right _ (sSorted_toSpine _ _ hx)))) hu
  refine hc.extend g (fun x hx => ?_)
  rcases List.mem_append.mp hx with hx | hx
  · rcases List.mem_append.mp hx with hx | hx
    · exact hc.clears_row (sSorted_toSpine _ _ hx)
    · exact hc.clears_row (sSorted_toSpine _ _ hx)
  · exact fun hd => absurd (hB x (List.mem_append_right _ hd)) (Nat.not_lt.mpr (hf x hx).1)

theorem SolveTy.unifies_sat {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') {σ : TySubst B}
    (hsat : Sol.Sat σ S'.sol) : TyUnifies σ τ τ' := by
  obtain ⟨fuel, t, Sup, hu, rfl⟩ := h
  obtain ⟨hS, ht⟩ := Sol.Sat.comp_inv hsat
  exact (tyUnifies_applySubst_of_sat hS τ τ').mp
    ((unifyM_success_sound fuel).1 _ _ _ hu ht)

--------------------- A DEFINITE LOOKUP SURVIVES A REFINEMENT -----------------
-- This was `Sol.lookup_toCtx_sat`, a nine-case induction. It transported a
-- lookup performed in ⟦S⟧-AS-A-CONTEXT into one performed on the SUBSTITUTED
-- row, under `Sol.Sat σ s` rather than the `Sol.Closes` that
-- `Sol.lookup_toCtx` demanded — because at an INTERMEDIATE state σ is never the
-- closure (later stages bind variables this state left free, and `Closes`
-- explicitly demands σ fix those).
--
-- With ↓ context-free the source lookup is already on a substituted row, and
-- the whole statement factors into two lemmas that existed already:
--   * `lookup_applySubst` — a definite lookup survives applying σ. The `?` case
--     is still excluded, and for the same reason: under a refinement `?`
--     genuinely CAN become `τ`, which is why A-sel-? parks a stump instead of
--     committing to ★.
--   * `Row.applySubst_sat_equiv` — a σ satisfying s changes a row only up to ≈
--     when ⟦s⟧ is applied first. `Sat` is only ≈, not equality, so the result
--     travels with it and the conclusion carries `ResEquiv` where
--     `Sol.lookup_toCtx` had an equation. This is the second place ≈ is
--     CALIBRATED: any coarser and the found types would not match up.
--
-- The `.var` case of the old induction — chase `s.row`, then re-align with
-- `lookup_equiv` — is exactly what `Row.applySubst_sat_equiv` now does once,
-- for the whole row, instead of once per variable.

/-- ⊢  under a σ that satisfies S, substituting with ⟦S⟧ first changes a row
only up to ≈. This is what lets wake-up's lookup — performed on the row the
state had already substituted — be read as a lookup on the raw row. -/
theorem Row.applySubst_sat_equiv {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) (ρ : Row B) :
    RowEquiv ((ρ.applySubst s.toSubst).applySubst σ) (ρ.applySubst σ) := by
  rw [Row.applySubst_applySubst]
  exact (Row.applySubst_substEquiv hsat.substEquiv ρ).symm

/-- ⊢  a DEFINITE lookup under ⟦s⟧ survives refining to any σ that satisfies s,
its result transported by σ and matched up to ≈. -/
theorem Sol.lookup_sat {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) {ρ : Row B} {l : Label} {r : LookupRes B}
    (h : Lookup (ρ.applySubst s.toSubst) l r) (hr : r ≠ .unknown) :
    ∃ r', Lookup (ρ.applySubst σ) l r' ∧ ResEquiv (r.applySubst σ) r' :=
  lookup_equiv (Row.applySubst_sat_equiv hsat ρ) (lookup_applySubst σ h hr)

/-- ⊢  …and a key, likewise: what wake-up reads under ⟦S⟧ is ≈ what σ reads. -/
theorem Ty.applySubst_sat_equiv {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) (τ : Ty B) :
    TyEquiv ((τ.applySubst s.toSubst).applySubst σ) (τ.applySubst σ) := by
  rw [Ty.applySubst_applySubst]
  exact (Ty.applySubst_substEquiv hsat.substEquiv τ).symm

/-- ⊢  …and a key: a solution σ satisfies reads it exactly as ⟦s⟧-then-σ does. -/
theorem Key.applySubst_sat_eq {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) (k : Key) :
    (k.applySubst s.toSubst).applySubst σ = k.applySubst σ := by
  rw [Key.applySubst_applySubst]
  exact (Key.applySubst_substEquiv hsat.substEquiv k).symm

/-- ⊢  …and for a KEYED lookup: the same two steps, with the key substituted
alongside the row. -/
theorem Sol.lookupQ_sat {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) {ρ : Row B} {q : Key} {r : LookupRes B}
    (h : LookupQ (ρ.applySubst s.toSubst) (q.applySubst s.toSubst) r)
    (hr : r ≠ .unknown) :
    ∃ r', LookupQ (ρ.applySubst σ) (q.applySubst σ) r' ∧
      ResEquiv (r.applySubst σ) r' := by
  have h' := LookupQ.applySubst σ h hr
  rw [Key.applySubst_sat_eq hsat q] at h'
  exact LookupQ.equiv (Row.applySubst_sat_equiv hsat ρ) h'

--------------------- SELECTION FROM A RECORD ALWAYS TYPES AT ★ ---------------
-- A record-typed subject can always have a field selected off it at ★, whatever
-- the lookup says: the three T-sel rules between them cover `found`, `⊥` and
-- `?`, and `T-★-intro` blurs the first. Lookup is total unconditionally (bare
-- structural recursion), so no well-formedness side condition is needed. This is
-- the declarative content of "a selection never gets stuck", and A-sel-? leans
-- on it.

/-- ⊢  `Γ′ ⊢ e : {ρ}` ⟹ `Γ′ ⊢ e.l : ★`. -/
theorem qtyped_sel_star {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {e : Expr C} {ρ : Row B} {l : Label}
    (h : QTyped constTy Γ' e (.rcd ρ)) : QTyped constTy Γ' (.sel e l) .unk := by
  obtain ⟨r, hr⟩ := lookup_total ρ l
  cases r with
  | found τ => exact .qUnk (.qSel h hr)
  | absent  => exact .qSelAbs h hr
  | unknown => exact .qSelUnk h hr

--------------------- THE CONGRUENCE CASES ------------------------------------
-- A-cons, A-lam and the three A-ξ rules carry no equation and consult no
-- lookup: the declarative rule is the algorithmic one with σ pushed through.
-- They are listed rather than inlined so that the census at the bottom is
-- complete and every A-rule has a name to point at.

/-- A-cons. -/
theorem infer_sound_con_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {σ : TySubst B} {c : C} :
    QTyped constTy Γ' (.con c) ((Ty.base (constTy c)).applySubst σ) := .qCon

/-- A-lam. The binder's fresh α is read under σ on both sides. -/
theorem infer_sound_lam_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {σ : TySubst B} {x : Var} {e : Expr C} {α : TyVar} {τ : Ty B}
    (h : QTyped constTy (Γ'.bindTy x (σ.ty α)) e (τ.applySubst σ)) :
    QTyped constTy Γ' (.lam x e) ((Ty.fn (.var α) τ).applySubst σ) := .qLam h

/-- A-rec. -/
theorem infer_sound_rcd_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {σ : TySubst B} {ξ : RecBody (Expr C)} {ρ : Row B}
    (h : QTypedBody constTy Γ' ξ (ρ.applySubst σ)) :
    QTyped constTy Γ' (.rcd ξ) ((Ty.rcd ρ).applySubst σ) := .qRcd h

/-- A-ξ-empty. -/
theorem inferRec_sound_empty_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {σ : TySubst B} :
    QTypedBody constTy Γ' (.empty : RecBody (Expr C)) ((Row.empty : Row B).applySubst σ) :=
  .empty

/-- A-ξ-field. -/
theorem inferRec_sound_field_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {σ : TySubst B} {l : Label} {e : Expr C} {τ : Ty B}
    (h : QTyped constTy Γ' e (τ.applySubst σ)) :
    QTypedBody constTy Γ' (.field l e) ((Row.sing l τ).applySubst σ) := .field h

/-- A-ξ-conc. -/
theorem inferRec_sound_cat_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {σ : TySubst B} {ξ₁ ξ₂ : RecBody (Expr C)} {ρ₁ ρ₂ : Row B}
    (h₁ : QTypedBody constTy Γ' ξ₁ (ρ₁.applySubst σ))
    (h₂ : QTypedBody constTy Γ' ξ₂ (ρ₂.applySubst σ)) :
    QTypedBody constTy Γ' (.cat ξ₁ ξ₂) ((Row.cat ρ₁ ρ₂).applySubst σ) := .cat h₁ h₂

--------------------- THE RULES THAT EMIT AN EQUATION -------------------------
-- A-app, A-conc. Both replay their own equation under σ and let `T-eq` absorb
-- the resulting ≈ — which is what T-eq is FOR. A-conc needs `Infer.sat_mono`'s
-- SolveTy half to push σ back one stage, since it solves two equations in
-- sequence and the first one is discharged at the earlier state.

/-- A-app. The premises are the induction hypotheses for `e₁` and `e₂`. -/
theorem infer_sound_app_step {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ' : QCtx B} {σ : TySubst B} {S S' : SolverState B}
    {e₁ e₂ : Expr C} {τ₁ τ₂ : Ty B} {β : TyVar}
    (h₁ : QTyped constTy Γ' e₁ (τ₁.applySubst σ))
    (h₂ : QTyped constTy Γ' e₂ (τ₂.applySubst σ))
    (hs : SolveTy S τ₁ (.fn τ₂ (.var β)) S')
    (hsat : Sol.Sat σ S'.sol) :
    QTyped constTy Γ' (.app e₁ e₂) ((Ty.var β).applySubst σ) :=
  .qApp (.qEq h₁ (hs.unifies_sat hsat)) h₂

/-- A-conc. `{r₁}` and `{r₂}` are the two fresh record rows the rule draws, and
the inferred type is their concatenation IN THE OTHER ORDER — which is exactly
the order `qCat` builds, so the two land on the nose with no ≈ in between. -/
theorem infer_sound_conc_step {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ' : QCtx B} {σ : TySubst B} {Sb S₃ S₄ : SolverState B}
    {e₁ e₂ : Expr C} {τ₁ τ₂ : Ty B} {r₁ r₂ : TyVar}
    (h₁ : QTyped constTy Γ' e₁ (τ₁.applySubst σ))
    (h₂ : QTyped constTy Γ' e₂ (τ₂.applySubst σ))
    (hs₁ : SolveTy Sb τ₁ (.rcd (.var r₁)) S₃)
    (hs₂ : SolveTy S₃ τ₂ (.rcd (.var r₂)) S₄)
    (hsat : Sol.Sat σ S₄.sol) :
    QTyped constTy Γ' (.cat e₁ e₂)
      ((Ty.rcd (.cat (.var r₂) (.var r₁))).applySubst σ) :=
  .qCat (.qEq h₁ (hs₁.unifies_sat (hs₂.satMono σ hsat)))
        (.qEq h₂ (hs₂.unifies_sat hsat))

--------------------- THE RULES THAT CONSULT A LOOKUP -------------------------
-- A-sel and A-sel-⊥. Each solves `τ ≐ {r}` and then reads the field off
-- `r[⟦S₂⟧]`; the declarative side reads it off `σ.row r`. `Sol.lookup_sat` is
-- what moves one to the other, and the ≈ it leaves behind is absorbed by T-eq
-- (A-sel) or is trivial (A-sel-⊥, where `⊥` is rigid). Both premises read the
-- SUBSTITUTED row now — they used to read the bare `.var r` through
-- ⟦S₂⟧-as-a-context, which a context-free ↓ has no way to interpret.

/-- A-sel. -/
theorem infer_sound_sel_step {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ τ' : Ty B} {l : Label} {r : TyVar}
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hlk : Lookup ((Row.var r).applySubst S₂.subst) l (.found τ'))
    (hsat : Sol.Sat σ S₂.sol) :
    QTyped constTy Γ' (.sel e l) (τ'.applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  obtain ⟨r'', hl'', he''⟩ := Sol.lookup_sat hsat hlk (by intro hh; cases hh)
  cases he'' with
  | found hty => exact .qEq (.qSel hrcd hl'') hty.symm

/-- A-sel-⊥ — T-sel-⊥'s algorithmic moment. -/
theorem infer_sound_selAbs_step {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ : Ty B} {l : Label} {r : TyVar}
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hlk : Lookup ((Row.var r).applySubst S₂.subst) l .absent)
    (hsat : Sol.Sat σ S₂.sol) :
    QTyped constTy Γ' (.sel e l) ((Ty.unk : Ty B).applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  obtain ⟨r'', hl'', he''⟩ := Sol.lookup_sat hsat hlk (by intro hh; cases hh)
  cases he'' with
  | absent => exact .qSelAbs hrcd hl''

--------------------- WHAT A PARKED STUMP HAS TO MEAN -------------------------
-- A-sel-? is the case `proof-state.md` calls a DESIGN question rather than a
-- proof-effort one: the rule returns a stump-variable δ and parks
-- `⟨α ⊳ ρ.l ↓ δ⟩`, and δ is a PROMISE, not yet a type, so an inner A-sel-? has
-- no plain declarative reading. The answer is that the promise is worth exactly
-- a DISCHARGE — the declarative side's `Stump.Discharge`, read at the σ the
-- conclusion is stated under. That is the
-- same condition A-var needs of the constraints it submits to wake-up, so the
-- two cases share one notion instead of inventing a second.
--
-- `Stump.Discharge.hit` asks only `θδ ≈ τ`, which is what the algorithm can
-- deliver: K-hit SOLVES the equation `δ ≐ τ`, and a solved equation is only
-- ever an ≈-fact (`SolveTy.unifies_sat`). (It used to pin `θδ = τ` on the nose,
-- and this file carried a separate ≈-relaxed `Stump.Discharge`; with D-hit
-- relaxed the two coincide and only `Discharge` is left.) `⊥` and `?` stay
-- rigid: ★ has no ≈-congruence rule (`TyEquiv.unk_inv_both`).

/-- A-sel-?, given the promise is kept. All three discharge cases land: D-hit
reads off as T-sel with the ≈ absorbed by T-eq, D-⊥ as T-sel-⊥, D-? as T-sel-★. -/
theorem infer_sound_selUnk_step {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ : Ty B} {l : Label} {r δ : TyVar}
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hsat : Sol.Sat σ S₂.sol)
    (hst : Stump.Discharge σ ⟨.var r, .lit l, .var δ⟩) :
    QTyped constTy Γ' (.sel e l) ((Ty.var δ).applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  show QTyped constTy Γ' (.sel e l) (σ.ty δ)
  cases hst with
  | hit hl hty => exact .qEq (.qSel hrcd (LookupQ.lab_iff.mp hl)) hty.symm
  | abs hl hδ  => rw [show σ.ty δ = _ from hδ]; exact .qSelAbs hrcd (LookupQ.lab_iff.mp hl)
  | unk hl hδ  => rw [show σ.ty δ = _ from hδ]; exact .qSelUnk hrcd (LookupQ.lab_iff.mp hl)

--------------------- THE K-/D- CORRESPONDENCE, ONE STEP ----------------------
-- "K-hit / K-⊥ / K-repark are D-hit / D-⊥ / D-? — the difference is WHEN"
-- (Infer.lean). That is the sentence the paper asserts; this is it as a
-- theorem, at the granularity of a single wake-up step.
--
-- Two adjustments the informal statement hides, and both are forced:
--   * the lookup K performs is on the row THE STATE HAS ALREADY SUBSTITUTED
--     (`ρ[⟦S⟧]`), while D looks up `ρ[θ]`. Under a σ that satisfies S the two
--     are ≈-equal (`Sol.Sat.substEquiv` is a ≗, and applySubst is a
--     ≗-congruence), so the lookups transport into each other — but only up to
--     ≈, which is why `lookup_equiv` appears again here.
--   * K-repark corresponds to NOTHING. D-? commits to ★ at once; K-repark
--     commits to nothing and re-parks with a new blocker. So the honest
--     conclusion is a disjunction: a step either discharges the constraint or
--     leaves it parked WITH ITS STUMP INTACT, for a later step or for the
--     answer to keep.

/-- ⊢  **K is D**, one step: a wake-up step either DISCHARGES its constraint
(up to ≈ on the hit payload) or re-parks it with the same stump. -/
theorem Wake.dischargeEquiv {B : Type} [DecidableEq B] {S S₁ : SolverState B}
    {p : Parked B} {σ : TySubst B} :
    Wake S p S₁ → Sol.Sat σ S₁.sol →
    p.stump.Discharge σ ∨ ∃ q ∈ S₁.parked, q.stump = p.stump
  -- K-hit is D-hit: the lookup landed, and the emitted equation `δ ≐ τ` pins δ
  -- to what it found — up to ≈, which is all an equation can pin.
  | .hit hlk hs, hsat => by
      have hS : Sol.Sat σ _ := SolveTy.satMono hs σ hsat
      have hδ := SolveTy.unifies_sat hs hsat
      -- ONE step: `Sol.lookupQ_sat` already lands on `ρ[σ]`, because the
      -- ⟦S⟧-then-σ ≈ σ realignment is the whole of its proof.
      obtain ⟨r₁, hl₁, he₁⟩ := Sol.lookupQ_sat hS hlk (by intro hh; cases hh)
      cases he₁ with
      | found ht₁ => exact .inl (.hit hl₁ (hδ.trans ht₁))
  -- K-⊥ is D-⊥: definite absence, and `δ ≐ ★` forces δ = ★ on the nose, since
  -- ★ has no ≈-congruence rule.
  | .abs hlk hs, hsat => by
      have hS : Sol.Sat σ _ := SolveTy.satMono hs σ hsat
      have hδ := SolveTy.unifies_sat hs hsat
      obtain ⟨r₁, hl₁, he₁⟩ := Sol.lookupQ_sat hS hlk (by intro hh; cases hh)
      cases he₁ with
      | absent => exact .inl (.abs hl₁ ((TyEquiv.unk_inv_both hδ).2 rfl))
  -- K-repark is NOTHING: the blocker moved, the stump did not.
  | .repark _, _ =>
      .inr ⟨{ blocker := _, stump := p.stump }, List.mem_cons_self, rfl⟩

--------------------- …AND OVER A WHOLE RUN -----------------------------------
-- Lifting one step to `Wakes` needs one structural fact, and it is the only
-- place the FILTERS matter. Every rule that retires a stump filters the parked
-- list on `stump.res`, so an entry survives a step exactly when its result
-- variable differs from the one being woken. That is not an accident: it is what
-- `FreshRenaming` arranges at A-var, which draws a fresh result variable per
-- instantiated constraint and keeps them off everything already parked.
--
-- So the run-level statement carries a DISTINCTNESS hypothesis on the list being
-- submitted, and nothing else. Its conclusion is the step's, with the parked
-- alternative now read at the run's final state: every constraint submitted to
-- wake-up is either discharged, or still parked when the run ends — where the
-- answer keeps it as a constraint.

/-- ⊢  a wake-up step leaves every OTHER parked constraint where it was. -/
theorem Wake.parked_preserved {B : Type} [DecidableEq B] {S S₁ : SolverState B}
    {p q : Parked B} (hne : q.stump ≠ p.stump) :
    Wake S p S₁ → q ∈ S.parked → q ∈ S₁.parked := by
  intro hw hq
  cases hw with
  | hit _ hs => obtain ⟨_, _, _, -, rfl⟩ := hs; simp [SolverState.extend, hq, hne]
  | abs _ hs =>
      obtain ⟨_, _, _, -, rfl⟩ := hs
      simp [SolverState.extend, SolverState.flag, hq, hne]
  | repark _ => simp [SolverState.park, hq, hne]

/-- ⊢  …and so does a whole run, for an entry no submitted constraint shares a
result variable with. -/
theorem Wakes.parked_preserved {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} {q : Parked B} :
    Wakes S ps S' → q ∈ S.parked →
    (∀ p ∈ ps, q.stump ≠ p.stump) → q ∈ S'.parked
  | .nil, hq, _ => hq
  | .cons hw hws, hq, hne =>
      Wakes.parked_preserved hws
        (Wake.parked_preserved (hne _ List.mem_cons_self) hw hq)
        (fun p hp => hne p (List.mem_cons_of_mem _ hp))
  | .park _ hws, hq, hne =>
      Wakes.parked_preserved hws (List.mem_cons_of_mem _ hq)
        (fun p hp => hne p (List.mem_cons_of_mem _ hp))

/-- ⊢  **K is D**, over a run: every constraint submitted to wake-up is either
DISCHARGED (up to ≈) or still parked when the run ends. The hypothesis is that
the submitted constraints have pairwise distinct result variables, which is what
`FreshRenaming` gives A-var. -/
theorem Wakes.dischargeEquiv {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} {σ : TySubst B} :
    Wakes S ps S' → Sol.Sat σ S'.sol →
    ps.Pairwise (fun a b => a.stump ≠ b.stump) →
    ∀ p ∈ ps, p.stump.Discharge σ ∨ ∃ q ∈ S'.parked, q.stump = p.stump
  | .nil, _, _, _, hp => absurd hp List.not_mem_nil
  | .cons (p := p) hw hws, hsat, hpw, p', hp' => by
      rcases List.mem_cons.mp hp' with rfl | hp'
      · rcases Wake.dischargeEquiv hw (hws.satMono σ hsat) with hd | ⟨q, hq, hqs⟩
        · exact .inl hd
        · exact .inr ⟨q, Wakes.parked_preserved hws hq
            (fun r hr => hqs ▸ (List.pairwise_cons.mp hpw).1 r hr), hqs⟩
      · exact Wakes.dischargeEquiv hws hsat (List.pairwise_cons.mp hpw).2 p' hp'
  | .park (p := p) _ hws, hsat, hpw, p', hp' => by
      rcases List.mem_cons.mp hp' with rfl | hp'
      · exact .inr ⟨p', Wakes.parked_preserved hws List.mem_cons_self
          (fun r hr => (List.pairwise_cons.mp hpw).1 r hr), rfl⟩
      · exact Wakes.dischargeEquiv hws hsat (List.pairwise_cons.mp hpw).2 p' hp'

--------------------- A-var, GIVEN THE INSTANCE -------------------------------
-- What is left of A-var once the discharge obligation is separated out: `qVar`
-- wants a scheme in Γ′ and an INSTANCE of it, and the instance's own
-- substitution χ is ours to choose — `QScheme.Inst` existentially quantifies
-- it. Since D-hit asks only ≈, χ can be σ itself at the instance.
--
-- Building the σ-image of the scheme it instantiates is done elsewhere:
-- `SchemeRead` (InferSoundA.lean) reads the scheme with its binders renamed
-- apart.

/-- A-var. -/
theorem infer_sound_var_step {B C : Type} {constTy : Sig B C} {Γ' : QCtx B}
    {x : Var} {sc' : QScheme B} {χ : TySubst B} {τ : Ty B}
    (hl : Γ'.lookup x = some sc')
    (hfix : χ.FixedOutside sc'.vars)
    (hdis : ∀ st ∈ sc'.constraints, st.Discharge χ)
    (hbody : TyEquiv (sc'.body.applySubst χ) τ) :
    QTyped constTy Γ' (.var x) τ :=
  .qEq (.qVar hl ⟨χ, hfix, hdis, rfl⟩) hbody

--------------------- A STALE BLOCKER, AND SATURATION -------------------------
-- A-sel-? parks a stump blocked by construction. It did not STAY blocked, and
-- nothing re-checked: `Infer.var` used to be the only rule that ran wake-up, so
-- A-app, A-conc, A-sel and A-let could solve a parked stump's BLOCKER and leave
-- it in Δ with a stale annotation. The thesis' own refinement example, closed,
-- was a witness:
--
--     (λx. {a = x.l}) {l = c}
--
-- Inside the λ this is `selEx_infers` verbatim; the application then solves
-- `r ≔ (l: 𝓫)` through the arrow equation. That state is `fsS` below, and at it
-- the lookup LANDS while the stump still says "blocked". The old F-★ sent δ to ★
-- there, and the program came out `{a: ★}` where the declarative system says
-- `{a: 𝓫}`.
--
-- WHAT CHANGED (Stage 1): `SolveTySat` / `WakesSat` — solve, then wake what the
-- solution staled — replace bare `SolveTy` / `Wakes` in the A-rules, and
-- `Infer.quiescent` proves every reachable state `SolverState.Quiescent`. So
-- `fStarEx_infers` below runs the same program to a state where the stump is
-- DISCHARGED at 𝓫, and `fStarEx_not_quiescent` names what was wrong with the old
-- state.

private def fStarEx : Expr Unit :=
  .app (.lam "x" (.rcd (.field "a" (.sel (.var "x") "l"))))
       (.rcd (.field "l" (.con ())))

-- the four names the run draws, in the order it draws them: the λ-binder, the
-- record's row variable, the selection's result δ, and A-app's β.
private def fsA : TyVar := natName 1
private def fsR : TyVar := natName 2
private def fsD : TyVar := natName 3
private def fsB : TyVar := natName 4

private def fsId : TySubst Unit := ⟨fun x => .var x, fun x => .var x, fun x => .var x⟩

private def fsS0 : SolverState Unit := ⟨Sol.nil, [], [], ⟨1⟩, []⟩
private def fsSlam : SolverState Unit := ⟨Sol.nil, [], [], ⟨2⟩, [(fsA, .ty)]⟩
private def fsSr : SolverState Unit := ⟨Sol.nil, [], [], ⟨3⟩, [(fsR, .row), (fsA, .ty)]⟩
private def fsSol1 : Sol Unit := ⟨[(fsA, .rcd (.var fsR))], [], []⟩
private def fsS2 : SolverState Unit := ⟨fsSol1, [], [], ⟨3⟩, [(fsR, .row), (fsA, .ty)]⟩
private def fsS2' : SolverState Unit :=
  ⟨fsSol1, [], [], ⟨4⟩, [(fsD, .ty), (fsR, .row), (fsA, .ty)]⟩

/-- the stump A-sel-? parks: blocked on the record's row variable, writing δ. -/
private def fsP : Parked Unit := ⟨fsR, ⟨.var fsR, .lit "l", .var fsD⟩⟩

private def fsSp : SolverState Unit :=
  ⟨fsSol1, [fsP], [], ⟨4⟩, [(fsD, .ty), (fsR, .row), (fsA, .ty)]⟩
private def fsSb : SolverState Unit :=
  ⟨fsSol1, [fsP], [], ⟨5⟩, [(fsB, .ty), (fsD, .ty), (fsR, .row), (fsA, .ty)]⟩

/-- what A-app's arrow equation solves: β at the result record, and — the point
— the stump's BLOCKER, at the row sort. -/
private def fsApp : Sol Unit :=
  ⟨[(fsB, .rcd (.sing "a" (.var fsD)))], [(fsR, .cat (.sing "l" (.base ())) .empty)], []⟩

/-- the state the equation leaves behind: blocker solved, stump still parked.
Before saturation was wired into the A-rules this is where the run ENDED, which
is what made the stale blocker reachable. -/
private def fsS : SolverState Unit := fsSb.extend fsApp ⟨5⟩

/-- ⊢  the lookup lands, on the substituted row — which is now the ONLY form
there is, and the form `Wake` and `Quiescent` read it in. The
companion `fStarEx_lands`, which stated the same landing "in ⟦S⟧ read as a
context" via `Lookup.var`, has no counterpart: `.var fsR` on its own is `?`. -/
theorem fStarEx_lands' :
    Lookup ((Row.var fsR).applySubst fsS.subst) "l" (.found (.base ())) :=
  .catHit .hit

/-- ⊢  **the state is not quiescent** — the stump's recorded blocker does not
block its lookup any more. This is the staleness `Saturate` steps on, and the
invariant `plans/inference-gap-analysis.md` §B says nothing enforced. -/
theorem fStarEx_not_blocked :
    ¬ LookupBlockedQ (fsP.stump.row.applySubst fsS.subst)
        (fsP.stump.label.applySubst fsS.subst) fsP.blocker := by
  intro hb
  exact absurd (lookup_det (LookupQ.lab_iff.mp hb.toLookupQ) fStarEx_lands') (by simp)

theorem fStarEx_not_quiescent : ¬ fsS.Quiescent := by
  intro hq
  exact fStarEx_not_blocked (hq fsP (by simp [fsS, fsSb, SolverState.extend]))

/-- what K-hit writes once saturation runs: δ ≔ 𝓫, the stump discharged and
dropped from Δ. -/
private def fsHit : Sol Unit := ⟨[(fsD, .base ())], [], []⟩
private def fsSfix : SolverState Unit :=
  { fsS.extend fsHit ⟨5⟩ with
      parked := (fsS.extend fsHit ⟨5⟩).parked.filter (·.stump != fsP.stump) }

/-- ⊢  **the run, with saturation in the rules.** The same closed program goes
through `Infer`, and A-app's equation no longer leaves a stale stump behind: the
`Saturate` step the rule now carries wakes it with K-hit, on the strength of
`fStarEx_not_blocked`. -/
theorem fStarEx_infers :
    Infer (B := Unit) (C := Unit) (Sig.plain fun _ => ()) QCtx.empty fsS0 fStarEx (.var fsB)
      fsSfix := by
  refine Infer.app (S₁ := fsSp) (S₂ := fsSp) (S₂' := fsSb)
    (τ₁ := .fn (.var fsA) (.rcd (.sing "a" (.var fsD))))
    (τ₂ := .rcd (.sing "l" (.base ()))) ?_ ?_ rfl ?_
  · refine Infer.lam (S₀ := fsSlam) rfl (Infer.rcd (InferRec.field ?_))
    refine Infer.selUnk (τ := .var fsA) (S₁ := fsSlam) (S₁' := fsSr) (S₂ := fsS2)
      (S₂' := fsS2') (r := fsR) (α := fsR) (δ := fsD) ?_ rfl ?_ ?_ rfl
    · exact Infer.var_mono rfl
        ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩
    · exact ⟨_, ⟨5, fsSol1, ⟨3⟩, rfl, rfl⟩, .done (SolverState.Quiescent.nil rfl)⟩
    · exact .varFree
  · exact Infer.rcd (InferRec.field Infer.con)
  · exact ⟨fsS, ⟨20, fsApp, ⟨5⟩, rfl, rfl⟩,
      .step (p := fsP) (by simp [fsS, fsSb, SolverState.extend]) fStarEx_not_blocked
        (Wake.hit (τ := .base ()) (LookupQ.lab_iff.mpr fStarEx_lands') ⟨5, fsHit, ⟨5⟩, rfl, rfl⟩)
        (.done (SolverState.Quiescent.nil rfl))⟩

/-- ⊢  **and the refinement survives.** Δ is empty — the stump discharged, so
the answer has no constraint — and the program's type reads
`{a: 𝓫}` under the final solution, which is exactly what the declarative system
gives it (`fStarEx_declarative`). The saturation step is what
turns A-sel-?'s promise into the type the lookup found. -/
theorem fStarEx_recovers :
    fsSfix.parked = [] ∧
      (Ty.var fsB).applySubst fsSfix.subst = .rcd (.sing "a" (.base ())) :=
  ⟨rfl, rfl⟩

/-- ⊢  what the stale state looked like: the stump still parked, its blocker
SOLVED. Kept as the shape `Saturate` exists to rule out. -/
theorem fStarEx_stale_blocker :
    fsS.parked = [fsP] ∧
      fsS.subst.row fsP.blocker = .cat (.sing "l" (.base ())) .empty :=
  ⟨rfl, rfl⟩

/-- ⊢  **the run, end to end.** The refinement example runs to an EMPTY Δ —
saturation discharged the stump at 𝓫 — so the answer is the plain type
`{a: 𝓫}`. -/
theorem fStarEx_runs :
    Run (B := Unit) (C := Unit) (Sig.plain fun _ => ()) fStarEx
      ⟨[], [], .rcd (.sing "a" (.base ()))⟩ :=
  ⟨_, fsSfix, fsSfix, fsSfix, fStarEx_infers, .refl _, rfl, .refl _, by decide⟩

/-- ⊢  **the refinement, in types**: the declarative system gives the closed
program `{a: 𝓫}`, which is what the run answers (`fStarEx_recovers`). -/
theorem fStarEx_declarative :
    QTyped (B := Unit) (C := Unit) (Sig.plain fun _ => ()) QCtx.empty fStarEx
        (.rcd (.sing "a" (.base ()))) := by
  refine QTyped.qApp (τ₁ := .rcd (.sing "l" (.base ()))) ?_ ?_
  · refine QTyped.qLam (QTyped.qRcd (QTypedBody.field ?_))
    refine QTyped.qSel (ρ := .sing "l" (.base ())) ?_ .hit
    exact QTyped.qVar (σ := ⟨[], [], .rcd (.sing "l" (.base ()))⟩) rfl
      ⟨fsId, ⟨fun _ _ => rfl, fun _ _ => rfl, fun _ _ => rfl⟩,
       (fun s hs => absurd hs List.not_mem_nil), rfl⟩
  · exact QTyped.qRcd (QTypedBody.field QTyped.qCon)

--------------------- THE SPENT PROMISE, RUN ----------------------------------
--     λx. λy. (x.l) y
--
-- A-sel-? parks ⟨r ⊳ r.l ↓ δ⟩ and answers δ; A-app then emits `δ ≐ (α_y → β)`,
-- which SUCCEEDS — δ is an unsolved variable, which is exactly what A-sel-?
-- returned it for. The state is well formed at the end: quiescent, the stump
-- still blocked on an unsolved row variable, nothing stale. ★ cannot answer the
-- stump any more, and the program IS declaratively typeable, at
-- `{(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫` (`spentEx_declarative`).
--
-- The old entry materialized the field (`r ≔ (l : α_y → β | r′)`), a choice. The
-- answer now keeps the constraint: `⟨r.l ↓ (α_y → β)⟩ ⇒ {r} → α_y → β`
-- (InferRuns), and the materialized type is one of its instances.

private def spentEx : Expr Unit :=
  .lam "x" (.lam "y" (.app (.sel (.var "x") "l") (.var "y")))

private def spX : TyVar := natName 1
private def spY : TyVar := natName 2
private def spRow : TyVar := natName 3
private def spD : TyVar := natName 4
private def spB : TyVar := natName 5

private def spS0 : SolverState Unit := ⟨Sol.nil, [], [], ⟨1⟩, []⟩
private def spLx : SolverState Unit := ⟨Sol.nil, [], [], ⟨2⟩, [(spX, .ty)]⟩
private def spLy : SolverState Unit := ⟨Sol.nil, [], [], ⟨3⟩, [(spY, .ty), (spX, .ty)]⟩
private def spDraw : SolverState Unit :=
  ⟨Sol.nil, [], [], ⟨4⟩, [(spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spSol1 : Sol Unit := ⟨[(spX, .rcd (.var spRow))], [], []⟩
private def spS2 : SolverState Unit :=
  ⟨spSol1, [], [], ⟨4⟩, [(spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spS2' : SolverState Unit :=
  ⟨spSol1, [], [], ⟨5⟩, [(spD, .ty), (spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spP : Parked Unit := ⟨spRow, ⟨.var spRow, .lit "l", .var spD⟩⟩
private def spSp : SolverState Unit :=
  ⟨spSol1, [spP], [], ⟨5⟩, [(spD, .ty), (spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spSb : SolverState Unit :=
  ⟨spSol1, [spP], [], ⟨6⟩,
   [(spB, .ty), (spD, .ty), (spRow, .row), (spY, .ty), (spX, .ty)]⟩

/-- what the APPLICATION writes: the promise, spent on an arrow. -/
private def spApp : Sol Unit := ⟨[(spD, .fn (.var spY) (.var spB))], [], []⟩
private def spS : SolverState Unit := spSb.extend spApp ⟨6⟩

/-- ⊢  the state is QUIESCENT — the stump is still blocked on an unsolved row
variable, so nothing about it is stale. The run did nothing wrong. -/
theorem spentEx_quiescent : spS.Quiescent := by
  intro p hp
  cases hp with
  | head      => exact .varFree
  | tail _ h  => exact absurd h List.not_mem_nil

/-- ⊢  …and so is the state A-sel-? parks it in, which is where A-var for `y`
submits its (empty) constraint list. -/
theorem spentEx_quiescent_parked : spSp.Quiescent := by
  intro p hp
  cases hp with
  | head      => exact .varFree
  | tail _ h  => exact absurd h List.not_mem_nil

/-- ⊢  `λx. λy. (x.l) y` infers, with the stump still parked. -/
theorem spentEx_infers :
    Infer (B := Unit) (C := Unit) (Sig.plain fun _ => ()) QCtx.empty spS0 spentEx
      (.fn (.var spX) (.fn (.var spY) (.var spB))) spS := by
  refine Infer.lam (S₀ := spLx) rfl (Infer.lam (S₀ := spLy) rfl ?_)
  refine Infer.app (S₁ := spSp) (S₂ := spSp) (S₂' := spSb)
    (τ₁ := .var spD) (τ₂ := .var spY) ?_ ?_ rfl ?_
  · refine Infer.selUnk (τ := .var spX) (S₁ := spLy) (S₁' := spDraw) (S₂ := spS2)
      (S₂' := spS2') (r := spRow) (α := spRow) (δ := spD) ?_ rfl ?_ ?_ rfl
    · exact Infer.var_mono rfl
        ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩
    · exact ⟨_, ⟨5, spSol1, ⟨4⟩, rfl, rfl⟩, .done (SolverState.Quiescent.nil rfl)⟩
    · exact .varFree
  · exact Infer.var_mono rfl
      ⟨_, .nil, .done spentEx_quiescent_parked⟩
  · exact ⟨spS, ⟨5, spApp, ⟨6⟩, rfl, rfl⟩, .done spentEx_quiescent⟩

theorem spentEx_still_parked : spS.parked = [spP] := rfl

/-- ⊢  **and the program is declaratively typeable**, at `{(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫`,
an instance of the answer's scheme. -/
theorem spentEx_declarative :
    QTyped (B := Unit) (C := Unit) (Sig.plain fun _ => ()) QCtx.empty spentEx
      (.fn (.rcd (.sing "l" (.fn (.base ()) (.base ()))))
        (.fn (.base ()) (.base ()))) := by
  refine QTyped.qLam (QTyped.qLam (QTyped.qApp (τ₁ := .base ()) ?_ ?_))
  · refine QTyped.qSel (ρ := .sing "l" (.fn (.base ()) (.base ()))) ?_ .hit
    exact QTyped.qVar (σ := ⟨[], [], .rcd (.sing "l" (.fn (.base ()) (.base ())))⟩) rfl
      ⟨fsId, ⟨fun _ _ => rfl, fun _ _ => rfl, fun _ _ => rfl⟩,
       (fun s hs => absurd hs List.not_mem_nil), rfl⟩
  · exact QTyped.qVar (σ := ⟨[], [], .base ()⟩) rfl
      ⟨fsId, ⟨fun _ _ => rfl, fun _ _ => rfl, fun _ _ => rfl⟩,
       (fun s hs => absurd hs List.not_mem_nil), rfl⟩

--------------------- WHAT A PARKED STUMP MEANS, AS A JUDGEMENT ---------------
-- `proof-state.md` calls A-sel-? "a design question about what a parked stump
-- MEANS declaratively, not a proof-effort question", and the question is sharp:
-- the rule returns a stump-variable δ, so an INNER A-sel-? types its selection at
-- a variable that has no declarative reading yet, and `S′.parked = []` does not
-- help because it constrains the FINAL state while the parking happens inside.
-- `infer_sound_selUnk_step` above answers it by ASSUMING the promise is kept —
-- it takes a `Discharge` — which is the right content but the wrong
-- bookkeeping: the assumption has to travel with the derivation, and there is
-- nowhere to put it.
--
-- `QTypedC` was the first answer: QTyped plus a list of stump ASSUMPTIONS,
-- threaded unchanged through every rule, and one new rule that consumes one.
-- The promise was to be redeemed once, at the end, by a transport along χ.
-- `InferSoundC` stated over it is FALSE (`inferSoundC_false`), and the working
-- answer is `QTypedA` (InferSoundA.lean): an assumption there is a TYPED stump —
-- row and result both read under σ — which is what makes weakening and the
-- let case go through.
--
-- WHY THE δ's ARE NOT SUBSTITUTED: the assumptions carry the σ-image of the
-- stump's ROW (so the subject's type matches) and the result variable RAW. That
-- is the whole distinction between a stump and a typing: everything else in the
-- derivation is a σ-image, and the δ's are the positions σ is not allowed to have
-- an opinion about yet. `Parked.toStumpC` builds exactly that image.

mutual
  /-- `Γ; Δ ⊢ e : τ` — typing under stump ASSUMPTIONS. Δ is a parameter: every
  rule passes it along untouched, so the only way it is ever used is `stump`. -/
  inductive QTypedC {B C : Type} (constTy : Sig B C) (Δ : List (Stump B)) :
      QCtx B → Expr C → Ty B → Prop where
    | qCon : QTypedC constTy Δ Γ (.con c) (.base (constTy c))
    | qVar : Γ.lookup x = some σ → QScheme.Inst σ τ →
             QTypedC constTy Δ Γ (.var x) τ
    | qEq  : QTypedC constTy Δ Γ e τ₁ → TyEquiv τ₁ τ₂ → QTypedC constTy Δ Γ e τ₂
    | qLam : QTypedC constTy Δ (Γ.bindTy x τ₁) e τ₂ →
             QTypedC constTy Δ Γ (.lam x e) (.fn τ₁ τ₂)
    | qApp : QTypedC constTy Δ Γ e₁ (.fn τ₁ τ₂) → QTypedC constTy Δ Γ e₂ τ₁ →
             QTypedC constTy Δ Γ (.app e₁ e₂) τ₂
    | qLet : σ.WF →
             (∀ τ₁, QScheme.Inst σ τ₁ → QTypedC constTy Δ Γ e₁ τ₁) →
             (∃ τ₁, QScheme.Inst σ τ₁) →
             QTypedC constTy Δ (Γ.bindScheme x σ) e₂ τ₂ →
             QTypedC constTy Δ Γ (.letE x e₁ e₂) τ₂
    | qCat : QTypedC constTy Δ Γ e₁ (.rcd ρ₁) → QTypedC constTy Δ Γ e₂ (.rcd ρ₂) →
             QTypedC constTy Δ Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))
    | qSel : QTypedC constTy Δ Γ e (.rcd ρ) → Lookup ρ l (.found τ) →
             QTypedC constTy Δ Γ (.sel e l) τ
    | qSelUnk : QTypedC constTy Δ Γ e (.rcd ρ) → Lookup ρ l .unknown →
                QTypedC constTy Δ Γ (.sel e l) .unk
    | qSelAbs : QTypedC constTy Δ Γ e (.rcd ρ) → Lookup ρ l .absent →
                QTypedC constTy Δ Γ (.sel e l) .unk
    | qUnk : QTypedC constTy Δ Γ e τ → QTypedC constTy Δ Γ e .unk
    | qRcd : QTypedCBody constTy Δ Γ b ρ → QTypedC constTy Δ Γ (.rcd b) (.rcd ρ)
    | qLab : QTypedC constTy Δ Γ (.lab l) (.lab (.lit l))
    | qSelDyn : QTypedC constTy Δ Γ e₁ (.rcd ρ) → QTypedC constTy Δ Γ e₂ (.lab q) →
                LookupQ ρ q (.found τ) → QTypedC constTy Δ Γ (.selDyn e₁ e₂) τ
    | qSelDynUnk : QTypedC constTy Δ Γ e₁ (.rcd ρ) → QTypedC constTy Δ Γ e₂ (.lab q) →
                   LookupQ ρ q .unknown → QTypedC constTy Δ Γ (.selDyn e₁ e₂) .unk
    | qSelDynAbs : QTypedC constTy Δ Γ e₁ (.rcd ρ) → QTypedC constTy Δ Γ e₂ (.lab q) →
                   LookupQ ρ q .absent → QTypedC constTy Δ Γ (.selDyn e₁ e₂) .unk
    | qRcdDyn : QTypedC constTy Δ Γ e₁ (.lab q) → QTypedC constTy Δ Γ e₂ τ →
                QTypedC constTy Δ Γ (.rcdDyn e₁ e₂) (.rcd (.dsing q τ))
    | qSelDynBase : QTypedC constTy Δ Γ e₁ (.rcd ρ) → QTypedC constTy Δ Γ e₂ (.base b) →
                    QTypedC constTy Δ Γ (.selDyn e₁ e₂) .unk
    | qRcdDynBase : QTypedC constTy Δ Γ e₁ (.base b) → QTypedC constTy Δ Γ e₂ τ →
                    QTypedC constTy Δ Γ (.rcdDyn e₁ e₂) .unk
    -- MacroNix
    | qPrim : constTy.prim p = some τp → τp.applySubst θ = Ty.arrows τs τ →
              QTypedCArgs constTy Δ Γ es τs → QTypedC constTy Δ Γ (.prim p es) τ
    | qBool : constTy.boolB = some bb → QTypedC constTy Δ Γ (.bool b) (.base bb)
    | qIte : constTy.boolB = some bb → QTypedC constTy Δ Γ e (.base bb) →
             QTypedC constTy Δ Γ e₁ τ → QTypedC constTy Δ Γ e₂ τ → QTypedC constTy Δ Γ (.ite e e₁ e₂) τ
    | qAsrt : constTy.boolB = some bb → QTypedC constTy Δ Γ e (.base bb) →
              QTypedC constTy Δ Γ e₂ τ → QTypedC constTy Δ Γ (.asrt e e₂) τ
    | qNil : QTypedC constTy Δ Γ .nil (.list τ)
    | qCons : QTypedC constTy Δ Γ e₁ τ → QTypedC constTy Δ Γ e₂ (.list τ) → QTypedC constTy Δ Γ (.cons e₁ e₂) (.list τ)
    | qHas : constTy.boolB = some bb → QTypedC constTy Δ Γ e (.rcd ρ) → QTypedC constTy Δ Γ (.has e l) (.base bb)
    -- THE ONE NEW RULE: a selection whose lookup is ASSUMED, answering at the
    -- stump's own result variable. This is A-sel-? read declaratively.
    | stump {ρ : Row B} {l : Label} {δ : TyVar} :
             QTypedC constTy Δ Γ e (.rcd ρ) → (⟨ρ, .lit l, .var δ⟩ : Stump B) ∈ Δ →
             QTypedC constTy Δ Γ (.sel e l) (.var δ)

  inductive QTypedCBody {B C : Type} (constTy : Sig B C) (Δ : List (Stump B)) :
      QCtx B → RecBody (Expr C) → Row B → Prop where
    | empty : QTypedCBody constTy Δ Γ .empty .empty
    | field : QTypedC constTy Δ Γ e τ →
              QTypedCBody constTy Δ Γ (.field l e) (.sing l τ)
    | cat : QTypedCBody constTy Δ Γ b₁ ρ₁ → QTypedCBody constTy Δ Γ b₂ ρ₂ →
            QTypedCBody constTy Δ Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)

  inductive QTypedCArgs {B C : Type} (constTy : Sig B C) (Δ : List (Stump B)) :
      QCtx B → List (Expr C) → List (Ty B) → Prop where
    | nil  : QTypedCArgs constTy Δ Γ [] []
    | cons : QTypedC constTy Δ Γ e τ → QTypedCArgs constTy Δ Γ es τs → QTypedCArgs constTy Δ Γ (e :: es) (τ :: τs)
end

mutual
  /-- ⊢  a plain typing is a constrained one, under ANY assumptions: weakening. -/
  theorem QTyped.toC {B C : Type} {constTy : Sig B C} {Δ : List (Stump B)}
      {Γ : QCtx B} {e : Expr C} {τ : Ty B} :
      QTyped constTy Γ e τ → QTypedC constTy Δ Γ e τ
    | .qCon           => .qCon
    | .qVar hl hi     => .qVar hl hi
    | .qEq h he       => .qEq h.toC he
    | .qLam h         => .qLam h.toC
    | .qApp h₁ h₂     => .qApp h₁.toC h₂.toC
    | .qLet hwf hi hin hb => .qLet hwf (fun τ₁ h => (hi τ₁ h).toC) hin hb.toC
    | .qCat h₁ h₂     => .qCat h₁.toC h₂.toC
    | .qSel h hl      => .qSel h.toC hl
    | .qSelUnk h hl   => .qSelUnk h.toC hl
    | .qSelAbs h hl   => .qSelAbs h.toC hl
    | .qUnk h         => .qUnk h.toC
    | .qRcd hb        => .qRcd hb.toC
    | .qLab           => .qLab
    | .qSelDyn h₁ h₂ hl    => .qSelDyn h₁.toC h₂.toC hl
    | .qSelDynUnk h₁ h₂ hl => .qSelDynUnk h₁.toC h₂.toC hl
    | .qSelDynAbs h₁ h₂ hl => .qSelDynAbs h₁.toC h₂.toC hl
    | .qRcdDyn h₁ h₂       => .qRcdDyn h₁.toC h₂.toC
    | .qSelDynBase h₁ h₂   => .qSelDynBase h₁.toC h₂.toC
    | .qRcdDynBase h₁ h₂   => .qRcdDynBase h₁.toC h₂.toC
    | .qPrim hp hi ha => .qPrim hp hi ha.toC
    | .qBool hb => .qBool hb
    | .qIte hb h h₁ h₂ => .qIte hb h.toC h₁.toC h₂.toC
    | .qAsrt hb h h₂ => .qAsrt hb h.toC h₂.toC
    | .qNil => .qNil
    | .qCons h₁ h₂ => .qCons h₁.toC h₂.toC
    | .qHas hb h => .qHas hb h.toC

  theorem QTypedBody.toC {B C : Type} {constTy : Sig B C} {Δ : List (Stump B)}
      {Γ : QCtx B} {b : RecBody (Expr C)} {ρ : Row B} :
      QTypedBody constTy Γ b ρ → QTypedCBody constTy Δ Γ b ρ
    | .empty      => .empty
    | .field h    => .field h.toC
    | .cat h₁ h₂  => .cat h₁.toC h₂.toC

  theorem QTypedArgs.toC {B C : Type} {constTy : Sig B C} {Δ : List (Stump B)}
      {Γ : QCtx B} {es : List (Expr C)} {τs : List (Ty B)} :
      QTypedArgs constTy Γ es τs → QTypedCArgs constTy Δ Γ es τs
    | .nil       => .nil
    | .cons h ha => .cons h.toC ha.toC
end

mutual
  /-- ⊢  …and with NO assumptions the two coincide: `stump` cannot fire, so the
  constrained judgement adds exactly the promises it is given and nothing else. -/
  theorem QTypedC.toQTyped {B C : Type} {constTy : Sig B C} {Γ : QCtx B}
      {e : Expr C} {τ : Ty B} :
      QTypedC constTy [] Γ e τ → QTyped constTy Γ e τ
    | .qCon           => .qCon
    | .qVar hl hi     => .qVar hl hi
    | .qEq h he       => .qEq h.toQTyped he
    | .qLam h         => .qLam h.toQTyped
    | .qApp h₁ h₂     => .qApp h₁.toQTyped h₂.toQTyped
    | .qLet hwf hi hin hb => .qLet hwf (fun τ₁ h => (hi τ₁ h).toQTyped) hin hb.toQTyped
    | .qCat h₁ h₂     => .qCat h₁.toQTyped h₂.toQTyped
    | .qSel h hl      => .qSel h.toQTyped hl
    | .qSelUnk h hl   => .qSelUnk h.toQTyped hl
    | .qSelAbs h hl   => .qSelAbs h.toQTyped hl
    | .qUnk h         => .qUnk h.toQTyped
    | .qRcd hb        => .qRcd hb.toQTyped
    | .qLab           => .qLab
    | .qSelDyn h₁ h₂ hl    => .qSelDyn h₁.toQTyped h₂.toQTyped hl
    | .qSelDynUnk h₁ h₂ hl => .qSelDynUnk h₁.toQTyped h₂.toQTyped hl
    | .qSelDynAbs h₁ h₂ hl => .qSelDynAbs h₁.toQTyped h₂.toQTyped hl
    | .qRcdDyn h₁ h₂       => .qRcdDyn h₁.toQTyped h₂.toQTyped
    | .qSelDynBase h₁ h₂   => .qSelDynBase h₁.toQTyped h₂.toQTyped
    | .qRcdDynBase h₁ h₂   => .qRcdDynBase h₁.toQTyped h₂.toQTyped
    | .qPrim hp hi ha => .qPrim hp hi ha.toQTyped
    | .qBool hb => .qBool hb
    | .qIte hb h h₁ h₂ => .qIte hb h.toQTyped h₁.toQTyped h₂.toQTyped
    | .qAsrt hb h h₂ => .qAsrt hb h.toQTyped h₂.toQTyped
    | .qNil => .qNil
    | .qCons h₁ h₂ => .qCons h₁.toQTyped h₂.toQTyped
    | .qHas hb h => .qHas hb h.toQTyped
    | .stump _ hmem   => absurd hmem List.not_mem_nil

  theorem QTypedCBody.toQTyped {B C : Type} {constTy : Sig B C} {Γ : QCtx B}
      {b : RecBody (Expr C)} {ρ : Row B} :
      QTypedCBody constTy [] Γ b ρ → QTypedBody constTy Γ b ρ
    | .empty      => .empty
    | .field h    => .field h.toQTyped
    | .cat h₁ h₂  => .cat h₁.toQTyped h₂.toQTyped

  theorem QTypedCArgs.toQTyped {B C : Type} {constTy : Sig B C} {Γ : QCtx B}
      {es : List (Expr C)} {τs : List (Ty B)} :
      QTypedCArgs constTy [] Γ es τs → QTypedArgs constTy Γ es τs
    | .nil       => .nil
    | .cons h ha => .cons h.toQTyped ha.toQTyped
end

/-- the σ-image of a parked stump, ROW substituted and result variable RAW — the
form the assumptions take, and the reason δ stays a promise. -/
def Parked.toStumpC {B : Type} (σ : TySubst B) (p : Parked B) : Stump B :=
  ⟨p.stump.row.applySubst σ, p.stump.label, p.stump.res⟩

/-- **Inference soundness, constrained form** — REFUTED as stated
(`inferSoundC_false`, LetSound.lean): the context is read under ⟦S′⟧ but the type
under an arbitrary σ ⊨ S′, so `y:a ⊢ y : 𝓫` would follow. Kept only as the name
the refutation is about. The restatement, PROVED, is `InferSound`
(InferSoundA.lean, `inferSound` in LetCase.lean): Γ is read under the same σ as
the type, and parked stumps are typed assumptions (`QTypedA`) rather than
promises with a raw result variable. -/
def InferSoundC (B C : Type) [DecidableEq B] (constTy : Sig B C) : Prop :=
  ∀ (Γ : QCtx B) (S S' : SolverState B) (e : Expr C) (τ : Ty B),
    Infer constTy Γ S e τ S' →
    ∀ σ : TySubst B, Sol.Sat σ S'.sol →
      (∀ p ∈ S'.parked, p.stump.res.applySubst σ = p.stump.res) →
      QTypedC constTy (S'.parked.map (Parked.toStumpC σ)) (S'.applyCtx Γ) e
        (τ.applySubst σ)

end MinimalCalculus
