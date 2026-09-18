-- INFERENCE SOUNDNESS, CASE BY CASE.
--
-- `InferSound` (Infer.lean) is a `def … : Prop`, not a theorem, and the reason
-- is recorded there. This module builds the proof the way
-- `infer_sound_app_step` started it — ONE LEMMA PER A-RULE, each taking the
-- induction hypotheses the full theorem will supply and discharging that rule's
-- own obligation. Nothing here is axiomatised and nothing is `sorry`: what is
-- not proved is not here, and §WHAT IS NOT PROVED at the bottom says which
-- rules those are and what each is waiting on.
--
-- ## The shape every case lemma has
-- A case lemma is stated at a FIXED pair (Γ′, σ): a declarative context, and a
-- substitution that SATISFIES the state the rule ends in. It is not stated at
-- `⟦S′⟧`, and that is the whole trick. `Infer.sat_mono` says any σ satisfying a
-- later state satisfies an earlier one, so the premises — solved at
-- INTERMEDIATE states — can all be replayed under one σ, and the conclusion can
-- sit at the final state. `⟦S′⟧` is then the instance σ := S′.subst, taken once
-- at the very end, which is also where the `Closes` hypothesis will enter.
--
-- ## Where the row environment goes
-- Γ′ carries NO row solutions (`Γ′.rowEnv = []`). The solutions are discharged
-- into σ instead, exactly as `QSubst.lean` found they must be: after
-- substituting, `(Row.var α).applySubst σ` is `σ.row α` and there is no
-- variable left for `L-α` to chase, so carrying them alongside is not an
-- option. Every lookup obligation below is therefore discharged at the empty
-- row environment.

import Infer

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
theorem SolveTy.unifies_sat {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') {σ : TySubst B}
    (hsat : Sol.Sat σ S'.sol) : TyUnifies σ τ τ' := by
  obtain ⟨fuel, t, Sup, hu, rfl⟩ := h
  obtain ⟨hS, ht⟩ := Sol.Sat.comp_inv hsat
  exact (tyUnifies_applySubst_of_sat hS τ τ').mp
    ((unifyM_success_sound fuel).1 [] S.supply _ _ hu ht)

--------------------- A DEFINITE LOOKUP SURVIVES A REFINEMENT -----------------
-- `Sol.lookup_toCtx` (RowUnify/State.lean) transports a lookup performed in
-- ⟦S⟧-as-a-context into one performed on the SUBSTITUTED row — but it asks for
-- `Sol.Closes`: "σ IS the closure of s". That is too strong for this induction,
-- which has only `Sol.Sat σ s`. At an INTERMEDIATE state σ is never the
-- closure: later stages bind variables this state left free, and `Closes`
-- explicitly demands that σ fix those.
--
-- `Sat` is enough, on DEFINITE results, and the two reasons are worth naming:
--   * `Sat` is only ≈, not equality, so the transported lookup lands on an
--     ≈-equivalent row and the result travels with it — hence `ResEquiv` in the
--     conclusion where `lookup_toCtx` has an equation. `lookup_equiv` is the
--     step, and this is the second place ≈ is CALIBRATED: any coarser and the
--     found types would not match up.
--   * a definite derivation never uses `L-α-free`, the one rule whose result
--     `Sat` cannot control — σ says nothing about variables outside s's domain.
--     Same observation `lookup_applySubst` rests on, and it is exactly why `?`
--     is excluded: under a refinement `?` genuinely CAN become `τ`, which is
--     the whole reason A-sel-? parks a stump instead of committing to ★.
theorem Sol.lookup_toCtx_sat {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) {Γ' : Ctx B} (hrow : Γ'.rowEnv = []) :
    {ρ : Row B} → {l : Label} → {r : LookupRes B} →
    Lookup s.toCtx ρ l r → r ≠ .unknown →
    ∃ r', Lookup Γ' (ρ.applySubst σ) l r' ∧ ResEquiv (r.applySubst σ) r'
  | _, _, _, .emp,       _  => ⟨.absent, .emp, .absent⟩
  | _, _, _, .hit,       _  => ⟨_, .hit, .refl _⟩
  | _, _, _, .miss hne,  _  => ⟨.absent, .miss hne, .absent⟩
  | _, _, _, .varFree _, hr => absurd rfl hr
  | _, _, _, .catUnk _,  hr => absurd rfl hr
  | _, _, _, .var hα h, hr => by
      obtain ⟨-, hmem⟩ := Sol.lookupRow_some hα
      obtain ⟨r', hl', he'⟩ := Sol.lookup_toCtx_sat hsat hrow h hr
      obtain ⟨r'', hl'', he''⟩ := lookup_equiv (RowEquiv.symm (hsat.2 _ hmem)) hl'
      exact ⟨r'', hl'', he'.trans he''⟩
  | _, _, _, .catHit h, _ => by
      obtain ⟨r', hl', he'⟩ := Sol.lookup_toCtx_sat hsat hrow h (by intro hh; cases hh)
      cases he' with
      | found hty => exact ⟨_, .catHit hl', .found hty⟩
  | _, _, _, .catSkip h₁ h₂, hr => by
      obtain ⟨r₁, hl₁, he₁⟩ :=
        Sol.lookup_toCtx_sat hsat hrow h₁ (by intro hh; cases hh)
      obtain ⟨r₂, hl₂, he₂⟩ := Sol.lookup_toCtx_sat hsat hrow h₂ hr
      cases he₁ with
      | absent => exact ⟨r₂, .catSkip hl₁ hl₂, he₂⟩

--------------------- SELECTION FROM A RECORD ALWAYS TYPES AT ★ ---------------
-- A record-typed subject can always have a field selected off it at ★, whatever
-- the lookup says: the three T-sel rules between them cover `found`, `⊥` and
-- `?`, and `T-★-intro` blurs the first. At an EMPTY row environment lookup is
-- total for free (every rank works, since nothing is ever chased), so no
-- well-formedness side condition is needed. This is the declarative content of
-- "a selection never gets stuck", and both A-sel-? and the degradations lean on
-- it.

/-- ⊢  `Γ′ ⊢ e : {ρ}` ⟹ `Γ′ ⊢ e.l : ★`, at a discharged row environment. -/
theorem qtyped_sel_star {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {e : Expr C} {ρ : Row B} {l : Label} (hrow : Γ'.rowEnv = [])
    (h : QTyped constTy Γ' e (.rcd ρ)) : QTyped constTy Γ' (.sel e l) .unk := by
  have hwf : Γ'.ctx.RowWF :=
    ⟨fun _ => 0, fun α ρ' hα => by simp [QCtx.ctx, Ctx.lookupRow, hrow] at hα⟩
  obtain ⟨r, hr⟩ := lookup_total hwf ρ l
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
theorem infer_sound_con_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {σ : TySubst B} {c : C} :
    QTyped constTy Γ' (.con c) ((Ty.base (constTy c)).applySubst σ) := .qCon

/-- A-lam. The binder's fresh α is read under σ on both sides. -/
theorem infer_sound_lam_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {σ : TySubst B} {x : Var} {e : Expr C} {α : TyVar} {τ : Ty B}
    (h : QTyped constTy (Γ'.bindTy x (σ.ty α)) e (τ.applySubst σ)) :
    QTyped constTy Γ' (.lam x e) ((Ty.fn (.var α) τ).applySubst σ) := .qLam h

/-- A-rec. -/
theorem infer_sound_rcd_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {σ : TySubst B} {ξ : RecBody (Expr C)} {ρ : Row B}
    (h : QTypedBody constTy Γ' ξ (ρ.applySubst σ)) :
    QTyped constTy Γ' (.rcd ξ) ((Ty.rcd ρ).applySubst σ) := .qRcd h

/-- A-ξ-empty. -/
theorem inferRec_sound_empty_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {σ : TySubst B} :
    QTypedBody constTy Γ' (.empty : RecBody (Expr C)) ((Row.empty : Row B).applySubst σ) :=
  .empty

/-- A-ξ-field. -/
theorem inferRec_sound_field_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {σ : TySubst B} {l : Label} {e : Expr C} {τ : Ty B}
    (h : QTyped constTy Γ' e (τ.applySubst σ)) :
    QTypedBody constTy Γ' (.field l e) ((Row.sing l τ).applySubst σ) := .field h

/-- A-ξ-conc. -/
theorem inferRec_sound_cat_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
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
theorem infer_sound_app_step {B C : Type} [DecidableEq B] {constTy : C → B}
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
theorem infer_sound_conc_step {B C : Type} [DecidableEq B] {constTy : C → B}
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
-- A-sel and A-sel-⊥. Each solves `τ ≐ {r}` and then reads the field off `r` in
-- ⟦S₂⟧-as-a-context; the declarative side reads it off `σ.row r` at the empty
-- row environment. `Sol.lookup_toCtx_sat` is the bridge, and the ≈ it leaves
-- behind is absorbed by T-eq (A-sel) or is trivial (A-sel-⊥, where `⊥` is rigid).

/-- A-sel. -/
theorem infer_sound_sel_step {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ τ' : Ty B} {l : Label} {r : TyVar}
    (hrow : Γ'.rowEnv = [])
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hlk : Lookup S₂.ctx (.var r) l (.found τ'))
    (hsat : Sol.Sat σ S₂.sol) :
    QTyped constTy Γ' (.sel e l) (τ'.applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  obtain ⟨r'', hl'', he''⟩ :=
    Sol.lookup_toCtx_sat (Γ' := Γ'.ctx) hsat hrow hlk (by intro hh; cases hh)
  cases he'' with
  | found hty => exact .qEq (.qSel hrcd hl'') hty.symm

/-- A-sel-⊥ — T-sel-⊥'s algorithmic moment. -/
theorem infer_sound_selAbs_step {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ : Ty B} {l : Label} {r : TyVar}
    (hrow : Γ'.rowEnv = [])
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hlk : Lookup S₂.ctx (.var r) l .absent)
    (hsat : Sol.Sat σ S₂.sol) :
    QTyped constTy Γ' (.sel e l) ((Ty.unk : Ty B).applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  obtain ⟨r'', hl'', he''⟩ :=
    Sol.lookup_toCtx_sat (Γ' := Γ'.ctx) hsat hrow hlk (by intro hh; cases hh)
  cases he'' with
  | absent => exact .qSelAbs hrcd hl''

--------------------- WHAT A PARKED STUMP HAS TO MEAN -------------------------
-- A-sel-? is the case `proof-state.md` calls a DESIGN question rather than a
-- proof-effort one: the rule returns a stump-variable δ and parks
-- `⟨α ▷ ρ.l ↓ δ⟩`, and δ is a PROMISE, not yet a type, so an inner A-sel-? has
-- no plain declarative reading. The answer is that the promise is worth exactly
-- a DISCHARGE — the declarative side's `Stump.Discharge`, read at the σ the
-- conclusion is stated under, against a discharged row environment. That is the
-- same condition A-var needs of the constraints it submits to wake-up, so the
-- two cases share one notion instead of inventing a second.
--
-- But `Stump.Discharge.hit` pins `θδ = τ` ON THE NOSE, and the algorithm cannot
-- deliver that. K-hit SOLVES the equation `δ ≐ τ`, and a solved equation is only
-- ever an ≈-fact (`SolveTy.unifies_sat`): the state can satisfy it with any σ
-- whose value at δ is ≈-equal to τ, and on records ≈ is not equality. So the
-- correspondence is stated against `Stump.DischargeEquiv`, which is `Discharge`
-- with the hit payload relaxed to ≈, and the gap is absorbed where it belongs —
-- by T-eq in the conclusion, since ≈ is a congruence.

/-- `Stump.Discharge` with the hit payload up to ≈, which is all a SOLVED
equation can ever give. `⊥` and `?` stay rigid: ★ has no ≈-congruence rule, so
`TyEquiv (θδ) ★` already forces `θδ = ★` (`TyEquiv.unk_inv_both`). -/
inductive Stump.DischargeEquiv {B : Type} (Γ : Ctx B) (θ : TySubst B)
    (s : Stump B) : Prop where
  | hit {τ : Ty B} :
      Lookup Γ (s.row.applySubst θ) s.label (.found τ) →
      TyEquiv (θ.ty s.res) τ → DischargeEquiv Γ θ s
  | abs :
      Lookup Γ (s.row.applySubst θ) s.label .absent →
      θ.ty s.res = .unk → DischargeEquiv Γ θ s
  | unk :
      Lookup Γ (s.row.applySubst θ) s.label .unknown →
      θ.ty s.res = .unk → DischargeEquiv Γ θ s

/-- ⊢  a discharge is one, up to ≈. -/
theorem Stump.Discharge.toEquiv {B : Type} {Γ : Ctx B} {θ : TySubst B}
    {s : Stump B} : s.Discharge Γ θ → s.DischargeEquiv Γ θ
  | .hit hl hδ => .hit hl (hδ ▸ .refl _)
  | .abs hl hδ => .abs hl hδ
  | .unk hl hδ => .unk hl hδ

/-- A-sel-?, given the promise is kept. All three discharge cases land: D-hit
reads off as T-sel with the ≈ absorbed by T-eq, D-⊥ as T-sel-⊥, D-? as T-sel-★. -/
theorem infer_sound_selUnk_step {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ : Ty B} {l : Label} {r δ : TyVar}
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hsat : Sol.Sat σ S₂.sol)
    (hst : Stump.DischargeEquiv Γ'.ctx σ ⟨.var r, l, δ⟩) :
    QTyped constTy Γ' (.sel e l) ((Ty.var δ).applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  show QTyped constTy Γ' (.sel e l) (σ.ty δ)
  cases hst with
  | hit hl hty => exact .qEq (.qSel hrcd hl) hty.symm
  | abs hl hδ  => rw [hδ]; exact .qSelAbs hrcd hl
  | unk hl hδ  => rw [hδ]; exact .qSelUnk hrcd hl

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
--     leaves it parked WITH ITS STUMP INTACT, for a later step or for
--     finalization to settle.

/-- ⊢  under a σ that satisfies S, substituting with ⟦S⟧ first changes a row
only up to ≈. This is what lets wake-up's lookup — performed on the row the
state had already substituted — be read as a lookup on the raw row. -/
theorem Row.applySubst_sat_equiv {B : Type} {s : Sol B} {σ : TySubst B}
    (hsat : Sol.Sat σ s) (ρ : Row B) :
    RowEquiv ((ρ.applySubst s.toSubst).applySubst σ) (ρ.applySubst σ) := by
  rw [Row.applySubst_applySubst]
  exact (Row.applySubst_substEquiv hsat.substEquiv ρ).symm

/-- ⊢  **K is D**, one step: a wake-up step either DISCHARGES its constraint
(up to ≈ on the hit payload) or re-parks it with the same stump. -/
theorem Wake.dischargeEquiv {B : Type} [DecidableEq B] {S S₁ : SolverState B}
    {p : Parked B} {σ : TySubst B} {Γ' : QCtx B} (hrow : Γ'.rowEnv = []) :
    Wake S p S₁ → Sol.Sat σ S₁.sol →
    p.stump.DischargeEquiv Γ'.ctx σ ∨ ∃ q ∈ S₁.parked, q.stump = p.stump
  -- K-hit is D-hit: the lookup landed, and the emitted equation `δ ≐ τ` pins δ
  -- to what it found — up to ≈, which is all an equation can pin.
  | .hit hlk hs, hsat => by
      have hS : Sol.Sat σ _ := SolveTy.satMono hs σ hsat
      have hδ := SolveTy.unifies_sat hs hsat
      obtain ⟨r₁, hl₁, he₁⟩ :=
        Sol.lookup_toCtx_sat (Γ' := Γ'.ctx) hS hrow hlk (by intro hh; cases hh)
      cases he₁ with
      | found ht₁ =>
          obtain ⟨_, hl₂, he₂⟩ :=
            lookup_equiv (Row.applySubst_sat_equiv hS p.stump.row) hl₁
          cases he₂ with
          | found ht₂ => exact .inl (.hit hl₂ ((hδ.trans ht₁).trans ht₂))
  -- K-⊥ is D-⊥: definite absence, and `δ ≐ ★` forces δ = ★ on the nose, since
  -- ★ has no ≈-congruence rule.
  | .abs hlk hs, hsat => by
      have hS : Sol.Sat σ _ := SolveTy.satMono hs σ hsat
      have hδ := SolveTy.unifies_sat hs hsat
      obtain ⟨r₁, hl₁, he₁⟩ :=
        Sol.lookup_toCtx_sat (Γ' := Γ'.ctx) hS hrow hlk (by intro hh; cases hh)
      cases he₁ with
      | absent =>
          obtain ⟨_, hl₂, he₂⟩ :=
            lookup_equiv (Row.applySubst_sat_equiv hS p.stump.row) hl₁
          cases he₂ with
          | absent => exact .inl (.abs hl₂ ((TyEquiv.unk_inv_both hδ).2 rfl))
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
-- wake-up is either discharged, or still parked when the run ends — where
-- finalization takes over, and where F-★'s missing premise bites.

/-- ⊢  a wake-up step leaves every OTHER parked constraint where it was. -/
theorem Wake.parked_preserved {B : Type} [DecidableEq B] {S S₁ : SolverState B}
    {p q : Parked B} (hne : q.stump.res ≠ p.stump.res) :
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
    (∀ p ∈ ps, q.stump.res ≠ p.stump.res) → q ∈ S'.parked
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
    {ps : List (Parked B)} {σ : TySubst B} {Γ' : QCtx B} (hrow : Γ'.rowEnv = []) :
    Wakes S ps S' → Sol.Sat σ S'.sol →
    ps.Pairwise (fun a b => a.stump.res ≠ b.stump.res) →
    ∀ p ∈ ps, p.stump.DischargeEquiv Γ'.ctx σ ∨ ∃ q ∈ S'.parked, q.stump = p.stump
  | .nil, _, _, _, hp => absurd hp List.not_mem_nil
  | .cons (p := p) hw hws, hsat, hpw, p', hp' => by
      rcases List.mem_cons.mp hp' with rfl | hp'
      · rcases Wake.dischargeEquiv hrow hw (hws.satMono σ hsat) with hd | ⟨q, hq, hqs⟩
        · exact .inl hd
        · exact .inr ⟨q, Wakes.parked_preserved hws hq
            (fun r hr => hqs ▸ (List.pairwise_cons.mp hpw).1 r hr), hqs⟩
      · exact Wakes.dischargeEquiv hrow hws hsat (List.pairwise_cons.mp hpw).2 p' hp'
  | .park (p := p) _ hws, hsat, hpw, p', hp' => by
      rcases List.mem_cons.mp hp' with rfl | hp'
      · exact .inr ⟨p', Wakes.parked_preserved hws List.mem_cons_self
          (fun r hr => (List.pairwise_cons.mp hpw).1 r hr), rfl⟩
      · exact Wakes.dischargeEquiv hrow hws hsat (List.pairwise_cons.mp hpw).2 p' hp'

--------------------- WHERE THE DISTINCTNESS COMES FROM -----------------------
-- `Wakes.dischargeEquiv` asks the submitted constraints to have pairwise
-- distinct result variables. A-var submits `InstStumps θ f σ.constraints ps`,
-- so the question is whether an INSTANTIATION produces distinct ones, and the
-- answer is: only if the scheme's own constraints already had them, and only if
-- they are BOUND. `FreshRenaming` makes `f` injective on `σ.vars` and keeps its
-- image off everything already parked — but injectivity on `σ.vars` says nothing
-- about a result variable that is not in `σ.vars`, and nothing at all about two
-- constraints that shared one to begin with.
--
-- `QScheme` is a bare structure and carries neither condition. That is a MISSING
-- INVARIANT, not a live bug: the algorithm only ever builds schemes out of
-- stumps whose result variables were drawn fresh (A-sel-?) or renamed apart
-- (A-var), so it never constructs a violating one. But the type permits it, and
-- if one existed wake-up would silently drop a constraint: every rule that
-- retires a stump filters on `stump.res`, so two stumps sharing a result
-- variable are BOTH retired when either fires — the second one's lookup never
-- performed, its δ pinned by the first's, and `Stump.Discharge.det` only makes
-- that sound if the two lookups agree, which nothing requires.

/-- the well-formedness `QScheme` does not carry. Named rather than proved: it
is an invariant of the states inference builds, in the same sense `UnifyWF` is,
and it belongs with that family. -/
def QScheme.ResWF {B : Type} (sc : QScheme B) : Prop :=
  sc.constraints.Pairwise (fun a b => a.res ≠ b.res) ∧
    ∀ st ∈ sc.constraints, st.res ∈ sc.vars

/-- ⊢  a well-formed scheme's instantiated constraints are pairwise distinct —
which is exactly what `Wakes.dischargeEquiv` asks of what A-var submits. -/
theorem InstStumps.pairwise {B : Type} {θ : TySubst B} {f : TyVar → TyVar}
    {Q : List (Stump B)} {ps : List (Parked B)} {vs : List TyVar}
    (hi : InstStumps θ f Q ps)
    (hinj : ∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β)
    (hdist : Q.Pairwise (fun a b => a.res ≠ b.res))
    (hbound : ∀ st ∈ Q, st.res ∈ vs) :
    ps.Pairwise (fun a b => a.stump.res ≠ b.stump.res) := by
  have h : (ps.map Parked.stump).Pairwise (fun a b => a.res ≠ b.res) := by
    rw [hi, List.pairwise_map]
    exact hdist.imp_of_mem
      (fun ha hb hne heq => hne (hinj _ (hbound _ ha) _ (hbound _ hb) heq))
  exact (List.pairwise_map (f := Parked.stump) (R := fun x y => x.res ≠ y.res)).mp h

--------------------- A-var, GIVEN THE INSTANCE -------------------------------
-- What is left of A-var once the discharge obligation is separated out: `qVar`
-- wants a scheme in Γ′ and an INSTANCE of it, and the instance's own
-- substitution χ is ours to choose — `QScheme.Inst` existentially quantifies
-- it. That freedom is exactly what pays for the ≈ that `DischargeEquiv` leaves
-- behind: χ is σ corrected at the constraints' result variables to the types
-- the lookups actually found, and the body then differs from the inferred one
-- by an ≈ that T-eq absorbs.
--
-- Building χ, and the σ-image of the scheme it instantiates, is the piece that
-- is NOT here: it is `SchemeImage`/`QCovers` (QSubst.lean) plus the
-- capture-avoidance `QScheme.applySubst` still lacks. This lemma is the seam,
-- so that what remains is a statement about SCHEMES rather than about typing.

/-- A-var. -/
theorem infer_sound_var_step {B C : Type} {constTy : C → B} {Γ' : QCtx B}
    {x : Var} {sc' : QScheme B} {χ : TySubst B} {τ : Ty B}
    (hl : Γ'.lookup x = some sc')
    (hfix : χ.FixedOutside sc'.vars)
    (hdis : ∀ st ∈ sc'.constraints, st.Discharge Γ'.ctx χ)
    (hbody : TyEquiv (sc'.body.applySubst χ) τ) :
    QTyped constTy Γ' (.var x) τ :=
  .qEq (.qVar hl ⟨χ, hfix, hdis, rfl⟩) hbody

--------------------- WHY F-★ CARRIES ITS PREMISE -----------------------------
-- `Finalize.star` (Infer.lean) now carries `LookupBlocked`. It did not, and this
-- section is the reason — kept checkable, which takes one extra definition: a
-- FIXED rule makes its own counterexample unstateable, so the counterexample has
-- to name the rule it refutes. `FinalizeUnguarded` below IS the old rule, and
-- `Finalize.toUnguarded` says the shipped one is strictly stronger.
--
-- As it stood, `Finalize.star` read
--
--     S ⊢ δ ≐ ★ ⇝ S′
--     ─────────────────────────
--     S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ S′[δ]
--
-- with NO premise about the lookup. `Stump.Discharge` offers ★ only under
-- `D-⊥` (the lookup is `⊥`) or `D-?` (it is still `?`); there is no rule
-- pinning δ to ★ when the lookup LANDS. So F-★ can commit a stump to ★ in a
-- configuration the declarative system cannot read at all — and it is not a
-- corner of the proof, it is the rule. Compare `Wake.abs`, which carries its
-- `Lookup … .absent`, and `Wake.hit`, which carries its `Lookup … (.found τ)`:
-- every OTHER rule that touches a stump's result variable says what the lookup
-- did first.
--
-- The witness is as small as it gets: a stump on the LITERAL row `(l: 𝓫)`,
-- whose lookup lands at every context and under every substitution. F-★ fires
-- on it anyway — nothing in the rule looks — and the resulting state forces
-- σδ = ★ while the lookup says `𝓫`. Not even `DischargeEquiv`, the ≈-relaxed
-- version, survives that: ★ has no ≈-congruence rule, so `★ ≈ 𝓫` is false too.
--
-- THE FIX, now in place, is the premise its siblings have — `LookupBlocked` on
-- the row it is finalizing, which is also exactly the hypothesis A-sel-? already
-- establishes when it parks the stump. `finalize_star_guarded_cannot_fire` below
-- is the other half of each refutation: at the very state where the unguarded
-- rule misfires, the guarded one has no derivation at all.

/-- **F-★ as it was**: no premise about the lookup. Every refutation in this
section is a statement about THIS relation, which is why it outlives the fix. -/
inductive FinalizeUnguarded {B : Type} [DecidableEq B] :
    SolverState B → Parked B → SolverState B → Prop where
  | star {S S' : SolverState B} {p : Parked B} :
      SolveTy S (.var p.stump.res) .unk S' →
      FinalizeUnguarded S p
        ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
          p.stump.label)

/-- ⊢  the shipped rule is strictly stronger: it fires only where the unguarded
one did, and `finalize_star_guarded_cannot_fire` shows the inclusion is proper. -/
theorem Finalize.toUnguarded {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Finalize S p S' → FinalizeUnguarded S p S'
  | .star _ _ hs => .star hs

/-- a stump on a LITERAL row: its lookup lands at every context, under every
substitution. Nothing about it is blocked, and the unguarded rule does not care. -/
private def fStar_p : Parked Unit := ⟨"a", ⟨.sing "l" (.base ()), "l", "d"⟩⟩

private def fStar_S : SolverState Unit :=
  { sol := Sol.nil, parked := [fStar_p], flags := [], supply := ⟨0⟩ }

/-- the equation F-★ emits, run: `δ ≐ ★` binds δ, at any fuel and any supply. -/
example : unifyTyF (B := Unit) [] ⟨0⟩ 0 (.var "d") .unk
    = .success ⟨[("d", (.unk : Ty Unit))], []⟩ ⟨0⟩ := rfl

/-- ⊢  **the unguarded F-★ can fire where nothing discharges.** There is a state,
a parked stump and a finalization step whose result NO substitution satisfying it
can discharge — not even up to ≈. -/
theorem finalize_star_no_discharge :
    ∃ (S S'' : SolverState Unit) (p : Parked Unit),
      FinalizeUnguarded S p S'' ∧
      ∀ (Γ' : Ctx Unit) (σ : TySubst Unit), Sol.Sat σ S''.sol →
        ¬ p.stump.DischargeEquiv Γ' σ := by
  have hfin : FinalizeUnguarded fStar_S fStar_p _ :=
    FinalizeUnguarded.star (S' := fStar_S.extend ⟨[("d", .unk)], []⟩ ⟨0⟩)
      ⟨0, ⟨[("d", .unk)], []⟩, ⟨0⟩, rfl, rfl⟩
  refine ⟨fStar_S, _, fStar_p, hfin, ?_⟩
  intro Γ' σ hsat hdis
  simp only [fStar_p] at hdis
  -- the finalized state binds δ to ★, so every σ satisfying it sends δ to ★
  have hd : σ.ty "d" = (.unk : Ty Unit) :=
    (TyEquiv.unk_inv_both (hsat.1 ("d", .unk) List.mem_cons_self)).2 rfl
  -- …while the lookup on a literal row lands, at every context and every σ
  have hlit : Lookup Γ' ((.sing "l" (.base ())) : Row Unit) "l"
      (.found (.base ())) := .hit
  cases hdis with
  | hit hlk hty =>
      have hτ := lookup_det hlk hlit
      injection hτ with hτ
      rw [hd, hτ] at hty
      exact absurd (TyEquiv.unk_inv hty) (by simp)
  | abs hlk _ => exact absurd (lookup_det hlk hlit) (by simp)
  | unk hlk _ => exact absurd (lookup_det hlk hlit) (by simp)

/-- ⊢  **and the guarded rule cannot fire there.** A `sing` row has no
`LookupBlocked` derivation at all — the relation is built from `L-α-free` and the
`cat`/`var` chases, and a literal field is never "don't know" — so the premise
rules this state out outright. The inclusion `Finalize ⊆ FinalizeUnguarded` is
therefore proper, and this is what the premise BUYS. -/
theorem finalize_star_guarded_cannot_fire :
    ¬ ∃ S', Finalize fStar_S fStar_p S' := by
  rintro ⟨S', h⟩
  cases h with
  | star _ hb _ =>
      have hb' : LookupBlocked fStar_S.ctx ((.sing "l" (.base ())) : Row Unit) "l"
          "a" := hb
      cases hb'

--------------------- …AND THE CONFIGURATION WAS REACHABLE --------------------
-- The witness above is hand-built, so a reader may fairly ask whether any state
-- inference actually PRODUCES looks like it: A-sel-? parks `.var r`, never a
-- literal row, and at the moment of parking the lookup is blocked BY
-- CONSTRUCTION — that is `LookupBlocked`, the rule's own premise. The answer was
-- that it did not STAY blocked and nothing re-checked. `Infer.var` used to be
-- the only rule that ran wake-up, so every other rule that writes a solution —
-- A-app, A-conc, A-sel, A-let — could solve a parked stump's BLOCKER and leave
-- the stump in Δ with a stale annotation. "Wake-up fires when a solution α ≔ ρ
-- is written, and only for stumps blocked on α" (algorithmic.typ) was the
-- invariant; the rules did not maintain it. No exotic state was needed to see
-- it: the thesis' own refinement example, closed, was a witness.
--
--     (λx. {a = x.l}) {l = c}
--
-- Inside the λ this is `selEx_infers` verbatim — `α ≐ {r}` binds α at the TYPE
-- sort, `r` stays unsolved at the ROW sort, A-sel-? parks `⟨r ▷ r.l ↓ δ⟩`. The
-- application then solves `r ≔ (l: 𝓫)` through the arrow equation. That state is
-- `fsS` below, and at it the lookup LANDS while the stump still says "blocked":
-- K-hit and F-★ both fire and commit δ to different types, F-★'s answer
-- discharges nowhere, and the program's type comes out `{a: ★}` where the
-- declarative system says `{a: 𝓫}`.
--
-- WHAT CHANGED (Stage 1): `SolveTySat` / `WakesSat` — solve, then wake what the
-- solution staled — replace bare `SolveTy` / `Wakes` in the A-rules, and
-- `Infer.quiescent` proves every reachable state now satisfies
-- `SolverState.Quiescent`. So `fStarEx_infers` below runs the same program to a
-- state where the stump is DISCHARGED at 𝓫 and the answer is the declarative
-- one. `fsS` itself is still a legal solver state, and F-★ still misfires on it
-- — the rule is still missing the premise its siblings carry — but a RUN no
-- longer hands it one. The two halves of the fix are therefore:
--   * saturation in the rules, which is what makes the configuration
--     unreachable (this section's `fStarEx_infers` is the regression that says
--     so, and `fStarEx_not_quiescent` names what was wrong with the old state);
--   * F-★'s own `LookupBlocked` premise, which is still owed. After Stage 1 it
--     is IMPLIED by quiescence, so it can be carried either as a premise on the
--     rule or as a `Quiescent` hypothesis on the finalization pass — but it has
--     to be somewhere, because the soundness lemma needs the fact in hand, and
--     because nothing stops `Finalize` from being applied at a state no run
--     produced.

private def fStarEx : Expr Unit :=
  .app (.lam "x" (.rcd (.field "a" (.sel (.var "x") "l"))))
       (.rcd (.field "l" (.con ())))

-- the four names the run draws, in the order it draws them: the λ-binder, the
-- record's row variable, the selection's result δ, and A-app's β.
private def fsA : TyVar := natName 1
private def fsR : TyVar := natName 2
private def fsD : TyVar := natName 3
private def fsB : TyVar := natName 4

private def fsId : TySubst Unit := ⟨fun x => .var x, fun x => .var x⟩

private def fsS0 : SolverState Unit := ⟨Sol.nil, [], [], ⟨1⟩, []⟩
private def fsSlam : SolverState Unit := ⟨Sol.nil, [], [], ⟨2⟩, [(fsA, .ty)]⟩
private def fsSr : SolverState Unit := ⟨Sol.nil, [], [], ⟨3⟩, [(fsR, .row), (fsA, .ty)]⟩
private def fsSol1 : Sol Unit := ⟨[(fsA, .rcd (.var fsR))], []⟩
private def fsS2 : SolverState Unit := ⟨fsSol1, [], [], ⟨3⟩, [(fsR, .row), (fsA, .ty)]⟩
private def fsS2' : SolverState Unit :=
  ⟨fsSol1, [], [], ⟨4⟩, [(fsD, .ty), (fsR, .row), (fsA, .ty)]⟩

/-- the stump A-sel-? parks: blocked on the record's row variable, writing δ. -/
private def fsP : Parked Unit := ⟨fsR, ⟨.var fsR, "l", fsD⟩⟩

private def fsSp : SolverState Unit :=
  ⟨fsSol1, [fsP], [], ⟨4⟩, [(fsD, .ty), (fsR, .row), (fsA, .ty)]⟩
private def fsSb : SolverState Unit :=
  ⟨fsSol1, [fsP], [], ⟨5⟩, [(fsB, .ty), (fsD, .ty), (fsR, .row), (fsA, .ty)]⟩

/-- what A-app's arrow equation solves: β at the result record, and — the point
— the stump's BLOCKER, at the row sort. -/
private def fsApp : Sol Unit :=
  ⟨[(fsB, .rcd (.sing "a" (.var fsD)))], [(fsR, .cat (.sing "l" (.base ())) .empty)]⟩

/-- the state the equation leaves behind: blocker solved, stump still parked.
Before saturation was wired into the A-rules this is where the run ENDED, which
is what made the F-★ defect reachable. It is still a legal state, so it is still
where the rules below disagree. -/
private def fsS : SolverState Unit := fsSb.extend fsApp ⟨5⟩

/-- ⊢  the lookup lands, in ⟦S⟧ read as a context… -/
theorem fStarEx_lands : Lookup fsS.ctx (.var fsR) "l" (.found (.base ())) :=
  .var rfl (.catHit .hit)

/-- …and on the substituted row, which is the form `Wake`, `Finalize` and
`Quiescent` all read it in. -/
theorem fStarEx_lands' :
    Lookup fsS.ctx ((Row.var fsR).applySubst fsS.subst) "l" (.found (.base ())) :=
  .catHit .hit

/-- ⊢  **the state is not quiescent** — the stump's recorded blocker does not
block its lookup any more. This is the staleness `Saturate` steps on, and the
invariant `plans/inference-gap-analysis.md` §B says nothing enforced. -/
theorem fStarEx_not_blocked :
    ¬ LookupBlocked fsS.ctx (fsP.stump.row.applySubst fsS.subst) fsP.stump.label
        fsP.blocker := by
  intro hb
  exact absurd (lookup_det hb.toLookup fStarEx_lands') (by simp)

theorem fStarEx_not_quiescent : ¬ fsS.Quiescent := by
  intro hq
  exact fStarEx_not_blocked (hq fsP (by simp [fsS, fsSb, SolverState.extend]))

/-- what K-hit writes once saturation runs: δ ≔ 𝓫, the stump discharged and
dropped from Δ. -/
private def fsHit : Sol Unit := ⟨[(fsD, .base ())], []⟩
private def fsSfix : SolverState Unit :=
  { fsS.extend fsHit ⟨5⟩ with
      parked := (fsS.extend fsHit ⟨5⟩).parked.filter (·.stump.res != fsP.stump.res) }

/-- ⊢  **the run, with saturation in the rules.** The same closed program goes
through `Infer`, and A-app's equation no longer leaves a stale stump behind: the
`Saturate` step the rule now carries wakes it with K-hit, on the strength of
`fStarEx_not_blocked`. -/
theorem fStarEx_infers :
    Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ fsS0 fStarEx (.var fsB)
      fsSfix := by
  refine Infer.app (S₁ := fsSp) (S₂ := fsSp) (S₂' := fsSb)
    (τ₁ := .fn (.var fsA) (.rcd (.sing "a" (.var fsD))))
    (τ₂ := .rcd (.sing "l" (.base ()))) ?_ ?_ rfl ?_
  · refine Infer.lam (S₀ := fsSlam) rfl (Infer.rcd (InferRec.field ?_))
    refine Infer.selUnk (τ := .var fsA) (S₁ := fsSlam) (S₁' := fsSr) (S₂ := fsS2)
      (S₂' := fsS2') (r := fsR) (α := fsR) (δ := fsD) ?_ rfl ?_ ?_ rfl
    · exact Infer.var (σ := ⟨[], [], .var fsA⟩) (θ := fsId) (f := id) (ps := []) rfl
        ⟨⟨fun _ _ => rfl, fun _ _ => rfl⟩, by simp⟩ (by simp [FreshRenaming]) rfl
        ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩
    · exact ⟨_, ⟨5, fsSol1, ⟨3⟩, rfl, rfl⟩, .done (SolverState.Quiescent.nil rfl)⟩
    · exact .varFree rfl
  · exact Infer.rcd (InferRec.field Infer.con)
  · exact ⟨fsS, ⟨20, fsApp, ⟨5⟩, rfl, rfl⟩,
      .step (p := fsP) (by simp [fsS, fsSb, SolverState.extend]) fStarEx_not_blocked
        (Wake.hit (τ := .base ()) fStarEx_lands' ⟨5, fsHit, ⟨5⟩, rfl, rfl⟩)
        (.done (SolverState.Quiescent.nil rfl))⟩

/-- ⊢  **and the refinement survives.** Δ is empty — the stump discharged, so
there is nothing left for finalization to blur — and the program's type reads
`{a: 𝓫}` under the final solution, which is exactly what the declarative system
gives it (`fStarEx_refinement_lost`, first conjunct). The saturation step is what
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

/-- what F-★ emits at that state, and the state it produces. -/
private def fsStar : Sol Unit := ⟨[(fsD, .unk)], []⟩
private def fsS' : SolverState Unit :=
  ({ fsS.extend fsStar ⟨5⟩ with
       parked := (fsS.extend fsStar ⟨5⟩).parked.filter (·.stump.res != fsP.stump.res) }).flag "l"

/-- ⊢  **why determinism was false.** From that state K-hit and the UNGUARDED F-★
both fire on the same stump and commit δ to different types — `𝓫` and `★`. The
"the final θ, W and τ do not depend on the wake-up order" claim
(algorithmic.typ) does not survive a rule that ignores the lookup. -/
theorem fStar_wake_star_disagree :
    ∃ Shit Sstar : SolverState Unit,
      Wake fsS fsP Shit ∧ FinalizeUnguarded fsS fsP Sstar ∧
      Shit.subst.ty fsD = .base () ∧ Sstar.subst.ty fsD = .unk :=
  ⟨_, _, Wake.hit (τ := .base ()) fStarEx_lands'
      ⟨5, fsHit, ⟨5⟩, rfl, rfl⟩,
    FinalizeUnguarded.star (S' := fsS.extend fsStar ⟨5⟩)
      ⟨5, fsStar, ⟨5⟩, rfl, rfl⟩, rfl, rfl⟩

/-- ⊢  **both halves of the fix, at one state.** The unguarded rule fires here;
the shipped one cannot, because the lookup lands and `LookupBlocked` fails
(`fStarEx_not_blocked`). Saturation removes the state from any RUN
(`Infer.quiescent`); the premise removes it from the RULE. Neither alone does
both: saturation leaves `Finalize` applicable at hand-built states, and the
premise alone would have left the run stuck here with no finalization step. -/
theorem fStar_guarded_cannot_fire : ¬ ∃ S', Finalize fsS fsP S' := by
  rintro ⟨S', h⟩
  cases h with
  | star _ hb _ => exact fStarEx_not_blocked hb

/-- ⊢  …and the unguarded rule's step, which the refutation below is about. -/
theorem fStarEx_finalizes_unguarded : FinalizeUnguarded fsS fsP fsS' :=
  FinalizeUnguarded.star (S' := fsS.extend fsStar ⟨5⟩) ⟨5, fsStar, ⟨5⟩, rfl, rfl⟩

/-- ⊢  **and what it produces discharges nowhere** — `finalize_star_no_discharge`
again, at a state with a VARIABLE row rather than a literal one, so the lookup has
to be transported along σ: every σ satisfying the finalized state sends `r` to a
row ≈-equal to `(l: 𝓫)`, and ≈ moves neither the label nor the payload, so the
lookup lands on 𝓫 under every such σ while F-★ has pinned δ to ★. -/
theorem fStar_reachable_no_discharge (Γ' : Ctx Unit) (hrow : Γ'.rowEnv = [])
    (σ : TySubst Unit) (hsat : Sol.Sat σ fsS'.sol) :
    ¬ fsP.stump.DischargeEquiv Γ' σ := by
  have hsat' : Sol.Sat σ (fsStar.comp fsS.sol) := hsat
  obtain ⟨hS, hst⟩ := Sol.Sat.comp_inv hsat'
  -- finalization wrote δ ≔ ★, so every σ satisfying it sends δ to ★
  have hd : σ.ty fsD = (.unk : Ty Unit) :=
    (TyEquiv.unk_inv_both (hst.1 (fsD, .unk) List.mem_cons_self)).2 rfl
  -- …while the lookup lands on 𝓫, under every σ satisfying the state
  obtain ⟨r', hl', he'⟩ :=
    Sol.lookup_toCtx_sat hS hrow fStarEx_lands (by intro h; cases h)
  intro hdis
  have hres : fsP.stump.res = fsD := rfl
  cases hdis with
  | hit hlk hty =>
      cases he' with
      | found hb =>
          have heq := lookup_det hl' hlk
          injection heq with heq
          subst heq
          rw [hres, hd] at hty
          rw [TyEquiv.unk_inv hty] at hb
          exact absurd ((TyEquiv.unk_inv_both hb).2 rfl) (by simp [Ty.applySubst])
  | abs hlk _ => cases he' with | found _ => exact absurd (lookup_det hl' hlk) (by simp)
  | unk hlk _ => cases he' with | found _ => exact absurd (lookup_det hl' hlk) (by simp)

/-- ⊢  **the other shape a run can have: nothing left to finalize.** The
refinement example runs to an EMPTY Δ — saturation discharged the stump at 𝓫 — so
`Finalizes` is `.nil` and F-★ never fires. Compare `selEx_runs` (Infer.lean),
where it does fire: the difference is whether anything ever resolved the blocker,
which is the whole content of A-sel-?'s promise. -/
theorem fStarEx_runs :
    Run (B := Unit) (C := Unit) (fun _ => ()) fStarEx (.var fsB) fsSfix :=
  ⟨fsSfix, fStarEx_infers, .nil, rfl⟩

/-- ⊢  **the refinement, in types.** The declarative system gives the closed
program `{a: 𝓫}`; finalizing the stale state answers `{a: ★}`, strictly blurrier —
and by `finalized_no_blur` (minimal.lean) such a ★ is never sharpened back, so it
is the answer and not a recoverable imprecision. `fStarEx_recovers` is the same
program under the saturating rules, landing on the first of these. -/
theorem fStarEx_refinement_lost :
    QTyped (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ fStarEx
        (.rcd (.sing "a" (.base ()))) ∧
      (Ty.var fsB).applySubst fsS'.subst = .rcd (.sing "a" .unk) ∧
      TyPrec (.rcd (.sing "a" (.base ()))) (.rcd (.sing "a" (.unk : Ty Unit))) ∧
      (Ty.rcd (.sing "a" (.base ())) : Ty Unit) ≠ .rcd (.sing "a" .unk) := by
  refine ⟨?_, rfl, .rcd (.sing (.unk _)), by simp⟩
  refine QTyped.qApp (τ₁ := .rcd (.sing "l" (.base ()))) ?_ ?_
  · refine QTyped.qLam (QTyped.qRcd (QTypedBody.field ?_))
    refine QTyped.qSel (ρ := .sing "l" (.base ())) ?_ .hit
    exact QTyped.qVar (σ := ⟨[], [], .rcd (.sing "l" (.base ()))⟩) rfl
      ⟨fsId, ⟨fun _ _ => rfl, fun _ _ => rfl⟩,
       (fun s hs => absurd hs List.not_mem_nil), rfl⟩
  · exact QTyped.qRcd (QTypedBody.field QTyped.qCon)

--------------------- …AND IT IS REACHABLE, AND IT IS INCOMPLETENESS ----------
-- `no_finalize_of_spent` (Infer.lean) is a verdict about a state. This is a RUN
-- that reaches one, and the news is worse than "the algorithm rejects what the
-- declarative system rejects":
--
--     λx. λy. (x.l) y
--
-- A-sel-? parks ⟨r ▷ r.l ↓ δ⟩ and answers δ; A-app then emits `δ ≐ (α_y → β)`,
-- which SUCCEEDS — δ is an unsolved variable, which is exactly what A-sel-?
-- returned it for. The state is perfectly well formed at the end: quiescent, the
-- stump still blocked on an unsolved row variable, nothing stale. And it cannot
-- be finalized, so `Run` has no answer for this program.
--
-- Yet the program IS declaratively typeable, at `{(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫`
-- (`spentEx_declarative`). So this is INCOMPLETENESS, not a justified rejection —
-- and it is not the ★-elimination gap of `plans/inference-gap-analysis.md` §D
-- either, because no ★ is ever formed here. The algorithm commits `x` to `{r}`
-- with `r` abstract and never guesses a concrete row, so the only way it could
-- answer is to CARRY the constraint `⟨r.l ↓ (α_y → β)⟩` — and it cannot, because
-- `Stump.res` is a `TyVar`: a stump's result position holds a variable, not a
-- type. Three exits, in increasing order of cost:
--   * leave it a hard error, which is what the rules do now, and record the
--     incompleteness (this section);
--   * `Stump.res : Ty B`, so a spent promise is still expressible. The discharge
--     arms survive (`hit` compares up to ≈ already, `abs`/`unk` demand ★, which a
--     spent arrow simply fails) — but it touches `Stump`, `Discharge`,
--     `QScheme.WF`, `selQ` and every principality theorem built on them;
--   * a consistency relation `τ ~ ★` beside `≐`, which §D already prices as a
--     real extension rather than a gap.
-- The generalization boundary inherits the same problem: A-let carries stumps
-- into a scheme whose `QScheme.WF` wants each `res` among the binders, and a
-- spent δ is not a binder. So whichever exit is taken, it is taken for both.

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
private def spSol1 : Sol Unit := ⟨[(spX, .rcd (.var spRow))], []⟩
private def spS2 : SolverState Unit :=
  ⟨spSol1, [], [], ⟨4⟩, [(spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spS2' : SolverState Unit :=
  ⟨spSol1, [], [], ⟨5⟩, [(spD, .ty), (spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spP : Parked Unit := ⟨spRow, ⟨.var spRow, "l", spD⟩⟩
private def spSp : SolverState Unit :=
  ⟨spSol1, [spP], [], ⟨5⟩, [(spD, .ty), (spRow, .row), (spY, .ty), (spX, .ty)]⟩
private def spSb : SolverState Unit :=
  ⟨spSol1, [spP], [], ⟨6⟩,
   [(spB, .ty), (spD, .ty), (spRow, .row), (spY, .ty), (spX, .ty)]⟩

/-- what the APPLICATION writes: the promise, spent on an arrow. -/
private def spApp : Sol Unit := ⟨[(spD, .fn (.var spY) (.var spB))], []⟩
private def spS : SolverState Unit := spSb.extend spApp ⟨6⟩

/-- ⊢  the state is QUIESCENT — the stump is still blocked on an unsolved row
variable, so nothing about it is stale. The run did nothing wrong. -/
theorem spentEx_quiescent : spS.Quiescent := by
  intro p hp
  cases hp with
  | head      => exact .varFree rfl
  | tail _ h  => exact absurd h List.not_mem_nil

/-- ⊢  …and so is the state A-sel-? parks it in, which is where A-var for `y`
submits its (empty) constraint list. -/
theorem spentEx_quiescent_parked : spSp.Quiescent := by
  intro p hp
  cases hp with
  | head      => exact .varFree rfl
  | tail _ h  => exact absurd h List.not_mem_nil

/-- ⊢  `λx. λy. (x.l) y` infers, with the stump still parked. -/
theorem spentEx_infers :
    Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ spS0 spentEx
      (.fn (.var spX) (.fn (.var spY) (.var spB))) spS := by
  refine Infer.lam (S₀ := spLx) rfl (Infer.lam (S₀ := spLy) rfl ?_)
  refine Infer.app (S₁ := spSp) (S₂ := spSp) (S₂' := spSb)
    (τ₁ := .var spD) (τ₂ := .var spY) ?_ ?_ rfl ?_
  · refine Infer.selUnk (τ := .var spX) (S₁ := spLy) (S₁' := spDraw) (S₂ := spS2)
      (S₂' := spS2') (r := spRow) (α := spRow) (δ := spD) ?_ rfl ?_ ?_ rfl
    · exact Infer.var (σ := ⟨[], [], .var spX⟩) (θ := fsId) (f := id) (ps := []) rfl
        ⟨⟨fun _ _ => rfl, fun _ _ => rfl⟩, by simp⟩ (by simp [FreshRenaming]) rfl
        ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩
    · exact ⟨_, ⟨5, spSol1, ⟨4⟩, rfl, rfl⟩, .done (SolverState.Quiescent.nil rfl)⟩
    · exact .varFree rfl
  · exact Infer.var (σ := ⟨[], [], .var spY⟩) (θ := fsId) (f := id) (ps := []) rfl
      ⟨⟨fun _ _ => rfl, fun _ _ => rfl⟩, by simp⟩ (by simp [FreshRenaming]) rfl
      ⟨_, .nil, .done spentEx_quiescent_parked⟩
  · exact ⟨spS, ⟨5, spApp, ⟨6⟩, rfl, rfl⟩, .done spentEx_quiescent⟩

/-- ⊢  **the promise is spent**: the state has written an arrow into δ. -/
theorem spentEx_spent : (spS.subst.ty spP.stump.res).Spent := trivial

/-- ⊢  **so the run cannot be finished.** F-★ has no derivation at this stump, and
it is the only stump there is: `Run` has no answer for the program. -/
theorem spentEx_cannot_finalize : ¬ ∃ S', Finalize spS spP S' :=
  no_finalize_of_spent spentEx_spent

theorem spentEx_still_parked : spS.parked = [spP] := rfl

/-- ⊢  **and the program is declaratively typeable**, at `{(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫`.
So the hard error above is the algorithm's incompleteness, not the declarative
system's rejection — the gap is that a stump's result position holds a VARIABLE,
so the constraint the answer would need (`⟨r.l ↓ (α → β)⟩`) cannot be written. -/
theorem spentEx_declarative :
    QTyped (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ spentEx
      (.fn (.rcd (.sing "l" (.fn (.base ()) (.base ()))))
        (.fn (.base ()) (.base ()))) := by
  refine QTyped.qLam (QTyped.qLam (QTyped.qApp (τ₁ := .base ()) ?_ ?_))
  · refine QTyped.qSel (ρ := .sing "l" (.fn (.base ()) (.base ()))) ?_ .hit
    exact QTyped.qVar (σ := ⟨[], [], .rcd (.sing "l" (.fn (.base ()) (.base ())))⟩) rfl
      ⟨fsId, ⟨fun _ _ => rfl, fun _ _ => rfl⟩,
       (fun s hs => absurd hs List.not_mem_nil), rfl⟩
  · exact QTyped.qVar (σ := ⟨[], [], .base ()⟩) rfl
      ⟨fsId, ⟨fun _ _ => rfl, fun _ _ => rfl⟩,
       (fun s hs => absurd hs List.not_mem_nil), rfl⟩

--------------------- WHAT A PARKED STUMP MEANS, AS A JUDGEMENT ---------------
-- `proof-state.md` calls A-sel-? "a design question about what a parked stump
-- MEANS declaratively, not a proof-effort question", and the question is sharp:
-- the rule returns a stump-variable δ, so an INNER A-sel-? types its selection at
-- a variable that has no declarative reading yet, and `S′.parked = []` does not
-- help because it constrains the FINAL state while the parking happens inside.
-- `infer_sound_selUnk_step` above answers it by ASSUMING the promise is kept —
-- it takes a `DischargeEquiv` — which is the right content but the wrong
-- bookkeeping: the assumption has to travel with the derivation, and there is
-- nowhere to put it.
--
-- `QTypedC` is that nowhere: QTyped plus a list of stump ASSUMPTIONS, threaded
-- unchanged through every rule, and one new rule that consumes one. A parked
-- stump is then exactly a hypothesis of the typing, δ is a variable STANDING FOR
-- its future value, and the promise is redeemed once, at the end, by
-- `QTypedCDischarge` — instead of at every selection site.
--
-- WHY THE δ's ARE NOT SUBSTITUTED: the assumptions carry the σ-image of the
-- stump's ROW (so the subject's type matches) and the result variable RAW. That
-- is the whole distinction between a stump and a typing: everything else in the
-- derivation is a σ-image, and the δ's are the positions σ is not allowed to have
-- an opinion about yet. `Parked.toStumpC` builds exactly that image.

mutual
  /-- `Γ; Δ ⊢ e : τ` — typing under stump ASSUMPTIONS. Δ is a parameter: every
  rule passes it along untouched, so the only way it is ever used is `stump`. -/
  inductive QTypedC {B C : Type} (constTy : C → B) (Δ : List (Stump B)) :
      QCtx B → Expr C → Ty B → Prop where
    | qCon : QTypedC constTy Δ Γ (.con c) (.base (constTy c))
    | qVar : Γ.lookup x = some σ → QScheme.Inst Γ.ctx σ τ →
             QTypedC constTy Δ Γ (.var x) τ
    | qEq  : QTypedC constTy Δ Γ e τ₁ → TyEquiv τ₁ τ₂ → QTypedC constTy Δ Γ e τ₂
    | qLam : QTypedC constTy Δ (Γ.bindTy x τ₁) e τ₂ →
             QTypedC constTy Δ Γ (.lam x e) (.fn τ₁ τ₂)
    | qApp : QTypedC constTy Δ Γ e₁ (.fn τ₁ τ₂) → QTypedC constTy Δ Γ e₂ τ₁ →
             QTypedC constTy Δ Γ (.app e₁ e₂) τ₂
    | qLet : (∀ τ₁, QScheme.Inst Γ.ctx σ τ₁ → QTypedC constTy Δ Γ e₁ τ₁) →
             (∃ τ₁, QScheme.Inst Γ.ctx σ τ₁) →
             QTypedC constTy Δ (Γ.bindScheme x σ) e₂ τ₂ →
             QTypedC constTy Δ Γ (.letE x e₁ e₂) τ₂
    | qCat : QTypedC constTy Δ Γ e₁ (.rcd ρ₁) → QTypedC constTy Δ Γ e₂ (.rcd ρ₂) →
             QTypedC constTy Δ Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))
    | qSel : QTypedC constTy Δ Γ e (.rcd ρ) → Lookup Γ.ctx ρ l (.found τ) →
             QTypedC constTy Δ Γ (.sel e l) τ
    | qSelUnk : QTypedC constTy Δ Γ e (.rcd ρ) → Lookup Γ.ctx ρ l .unknown →
                QTypedC constTy Δ Γ (.sel e l) .unk
    | qSelAbs : QTypedC constTy Δ Γ e (.rcd ρ) → Lookup Γ.ctx ρ l .absent →
                QTypedC constTy Δ Γ (.sel e l) .unk
    | qUnk : QTypedC constTy Δ Γ e τ → QTypedC constTy Δ Γ e .unk
    | qRcd : QTypedCBody constTy Δ Γ b ρ → QTypedC constTy Δ Γ (.rcd b) (.rcd ρ)
    -- THE ONE NEW RULE: a selection whose lookup is ASSUMED, answering at the
    -- stump's own result variable. This is A-sel-? read declaratively.
    | stump {ρ : Row B} {l : Label} {δ : TyVar} :
             QTypedC constTy Δ Γ e (.rcd ρ) → (⟨ρ, l, δ⟩ : Stump B) ∈ Δ →
             QTypedC constTy Δ Γ (.sel e l) (.var δ)

  inductive QTypedCBody {B C : Type} (constTy : C → B) (Δ : List (Stump B)) :
      QCtx B → RecBody (Expr C) → Row B → Prop where
    | empty : QTypedCBody constTy Δ Γ .empty .empty
    | field : QTypedC constTy Δ Γ e τ →
              QTypedCBody constTy Δ Γ (.field l e) (.sing l τ)
    | cat : QTypedCBody constTy Δ Γ b₁ ρ₁ → QTypedCBody constTy Δ Γ b₂ ρ₂ →
            QTypedCBody constTy Δ Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)
end

mutual
  /-- ⊢  a plain typing is a constrained one, under ANY assumptions: weakening. -/
  theorem QTyped.toC {B C : Type} {constTy : C → B} {Δ : List (Stump B)}
      {Γ : QCtx B} {e : Expr C} {τ : Ty B} :
      QTyped constTy Γ e τ → QTypedC constTy Δ Γ e τ
    | .qCon           => .qCon
    | .qVar hl hi     => .qVar hl hi
    | .qEq h he       => .qEq h.toC he
    | .qLam h         => .qLam h.toC
    | .qApp h₁ h₂     => .qApp h₁.toC h₂.toC
    | .qLet hi hin hb => .qLet (fun τ₁ h => (hi τ₁ h).toC) hin hb.toC
    | .qCat h₁ h₂     => .qCat h₁.toC h₂.toC
    | .qSel h hl      => .qSel h.toC hl
    | .qSelUnk h hl   => .qSelUnk h.toC hl
    | .qSelAbs h hl   => .qSelAbs h.toC hl
    | .qUnk h         => .qUnk h.toC
    | .qRcd hb        => .qRcd hb.toC

  theorem QTypedBody.toC {B C : Type} {constTy : C → B} {Δ : List (Stump B)}
      {Γ : QCtx B} {b : RecBody (Expr C)} {ρ : Row B} :
      QTypedBody constTy Γ b ρ → QTypedCBody constTy Δ Γ b ρ
    | .empty      => .empty
    | .field h    => .field h.toC
    | .cat h₁ h₂  => .cat h₁.toC h₂.toC
end

mutual
  /-- ⊢  …and with NO assumptions the two coincide: `stump` cannot fire, so the
  constrained judgement adds exactly the promises it is given and nothing else. -/
  theorem QTypedC.toQTyped {B C : Type} {constTy : C → B} {Γ : QCtx B}
      {e : Expr C} {τ : Ty B} :
      QTypedC constTy [] Γ e τ → QTyped constTy Γ e τ
    | .qCon           => .qCon
    | .qVar hl hi     => .qVar hl hi
    | .qEq h he       => .qEq h.toQTyped he
    | .qLam h         => .qLam h.toQTyped
    | .qApp h₁ h₂     => .qApp h₁.toQTyped h₂.toQTyped
    | .qLet hi hin hb => .qLet (fun τ₁ h => (hi τ₁ h).toQTyped) hin hb.toQTyped
    | .qCat h₁ h₂     => .qCat h₁.toQTyped h₂.toQTyped
    | .qSel h hl      => .qSel h.toQTyped hl
    | .qSelUnk h hl   => .qSelUnk h.toQTyped hl
    | .qSelAbs h hl   => .qSelAbs h.toQTyped hl
    | .qUnk h         => .qUnk h.toQTyped
    | .qRcd hb        => .qRcd hb.toQTyped
    | .stump _ hmem   => absurd hmem List.not_mem_nil

  theorem QTypedCBody.toQTyped {B C : Type} {constTy : C → B} {Γ : QCtx B}
      {b : RecBody (Expr C)} {ρ : Row B} :
      QTypedCBody constTy [] Γ b ρ → QTypedBody constTy Γ b ρ
    | .empty      => .empty
    | .field h    => .field h.toQTyped
    | .cat h₁ h₂  => .cat h₁.toQTyped h₂.toQTyped
end

/-- the σ-image of a parked stump, ROW substituted and result variable RAW — the
form the assumptions take, and the reason δ stays a promise. -/
def Parked.toStumpC {B : Type} (σ : TySubst B) (p : Parked B) : Stump B :=
  ⟨p.stump.row.applySubst σ, p.stump.label, p.stump.res⟩

/-- ⊢  **A-sel-? needs no discharge any more.** Compare `infer_sound_selUnk_step`,
which takes a `DischargeEquiv` it cannot get at an inner selection: here the
stump is an ASSUMPTION, so the case is the record equation followed by one rule.
`hδ` is the only side condition — σ has no opinion at δ yet, which is exactly
what "parked" means. -/
theorem inferC_sound_selUnk_step {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B} {Δ : List (Stump B)}
    {e : Expr C} {τ : Ty B} {l : Label} {r δ : TyVar}
    (h : QTypedC constTy Δ Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hsat : Sol.Sat σ S₂.sol)
    (hmem : (⟨(Row.var r).applySubst σ, l, δ⟩ : Stump B) ∈ Δ)
    (hδ : σ.ty δ = .var δ) :
    QTypedC constTy Δ Γ' (.sel e l) ((Ty.var δ).applySubst σ) := by
  have hrcd : QTypedC constTy Δ Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  show QTypedC constTy Δ Γ' (.sel e l) (σ.ty δ)
  rw [hδ]
  exact .stump hrcd hmem

--------------------- WHAT FINALIZATION IS WORTH ------------------------------
-- F-★'s premise exists to make ONE fact available: the lookup is still `?`, so ★
-- is what `Stump.Discharge.unk` asks for. This is that fact cashed in.
--
-- The first step is general and worth having on its own: BLOCKEDNESS anywhere
-- gives an unknown lookup at a DISCHARGED row environment. The reason is
-- monotonicity read backwards — a definite answer survives extending the row
-- solutions (`lookup_mono`), so if the lookup at the empty environment were
-- definite it would still be definite in ⟦S⟧, contradicting blockedness. The
-- empty environment also makes lookup total for free, so there IS an answer to
-- case on.

/-- ⊢  a blocked lookup is `?` at any discharged row environment. -/
theorem lookup_unknown_of_blocked {B : Type} {Γ' Γ : Ctx B} (hrow : Γ'.rowEnv = [])
    {ρ : Row B} {l : Label} {α : TyVar} (hb : LookupBlocked Γ ρ l α) :
    Lookup Γ' ρ l .unknown := by
  have hwf : Γ'.RowWF :=
    ⟨fun _ => 0, fun a r ha => by simp [Ctx.lookupRow, hrow] at ha⟩
  have hext : Ctx.RowExt Γ' Γ := fun a r ha => by simp [Ctx.lookupRow, hrow] at ha
  obtain ⟨r, hr⟩ := lookup_total hwf ρ l
  cases r with
  | unknown => exact hr
  | found τ =>
      exact absurd (lookup_det (lookup_mono hext hr (by intro h; cases h)) hb.toLookup)
        (by simp)
  | absent =>
      exact absurd (lookup_det (lookup_mono hext hr (by intro h; cases h)) hb.toLookup)
        (by simp)

-- The second step is the ★ itself, and it comes from the equation rather than from
-- the rule: `δ ≐ ★` was SOLVED, so every σ satisfying the state it produced sends
-- δ to something ≈-equal to ★ — and ★ is ≈-rigid, so to ★ on the nose.

/-- ⊢  **finalization discharges the stump it finalizes.** At the state's own
reading of the stump (`Parked.toStumpC S.subst`), the `?` arm of
`DischargeEquiv` applies: the lookup is blocked by F-★'s premise, and δ is ★ by
its equation.

`hfix` is the one side condition, and it is the ONE place the ?-arm differs from
the definite arms: a definite lookup transports along any refinement
(`Sol.lookup_toCtx_sat`), a `?` does not — a later σ can solve the blocker and
make the lookup land, which is exactly why A-sel-? parks a stump instead of
committing to ★ and why finalization runs LAST. So σ may not refine the row this
stump is blocked on; `hfix` says it does not. It is discharged by an idempotent
solution (`Sol.Applied`, i.e. what `UnifyWF` is for) at σ := ⟦S⟧, and by
`FixedOutside` at any χ that moves only the δ's. -/
theorem Finalize.dischargeEquiv {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} (hf : Finalize S p S') {Γ' : Ctx B} (hrow : Γ'.rowEnv = [])
    {σ : TySubst B} (hsat : Sol.Sat σ S'.sol)
    (hfix : (Parked.toStumpC S.subst p).row.applySubst σ
              = (Parked.toStumpC S.subst p).row) :
    (Parked.toStumpC S.subst p).DischargeEquiv Γ' σ := by
  cases hf with
  | star _ hb hs =>
      refine .unk ?_ ?_
      · rw [hfix]; exact lookup_unknown_of_blocked hrow hb
      · exact (TyEquiv.unk_inv_both (hs.unifies_sat hsat)).2 rfl

/-- ⊢  …and that is precisely the arm the DECLARATIVE side calls `D-?`: F-★ is
`Stump.Discharge.unk` with the lookup performed by the algorithm. -/
theorem Finalize.discharge_isUnk {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} (hf : Finalize S p S') {Γ' : Ctx B} (hrow : Γ'.rowEnv = [])
    {σ : TySubst B} (hsat : Sol.Sat σ S'.sol)
    (hfix : (Parked.toStumpC S.subst p).row.applySubst σ
              = (Parked.toStumpC S.subst p).row) :
    (Parked.toStumpC S.subst p).Discharge Γ' σ := by
  cases hf with
  | star _ hb hs =>
      refine .unk ?_ ?_
      · rw [hfix]; exact lookup_unknown_of_blocked hrow hb
      · exact (TyEquiv.unk_inv_both (hs.unifies_sat hsat)).2 rfl


--------------------- THE TWO HALVES, AND WHAT JOINS THEM ---------------------
-- With `Run` (Infer.lean) in place, algorithm soundness factors, and each factor
-- is now a statement rather than a hope:
--
--   RunSound  =  InferSoundC  ∘  Finalize.dischargeEquiv  ∘  QTypedCDischarge
--
-- The first hands back a CONSTRAINED typing whose assumptions are exactly the
-- stumps the run left parked — no `parked = []` hypothesis, which is what made
-- `InferSound` unprovable at an inner A-sel-? (`inferC_sound_selUnk_step` is that
-- case, and it needs no discharge). The second discharges one assumption per
-- finalization step. The third cashes the discharges in, once, at the end.

/-- **Inference soundness, constrained form** — the shape the induction can
actually carry. Every rule but A-sel-? is the old case lemma with `QTyped.toC`
applied; A-sel-? is `inferC_sound_selUnk_step`, whose only side condition is that
σ has no opinion at the δ's yet — `hδ` below, which is what "still parked" means.

Compare `InferSound`: that statement is stuck at an inner A-sel-? no matter how
much is proved around it, because the inner δ has no declarative reading and the
`parked = []` hypothesis speaks about the wrong state. This one has no such
hypothesis. -/
def InferSoundC (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ (Γ : QCtx B) (S S' : SolverState B) (e : Expr C) (τ : Ty B),
    Infer constTy Γ S e τ S' →
    ∀ σ : TySubst B, Sol.Sat σ S'.sol →
      (∀ p ∈ S'.parked, σ.ty p.stump.res = .var p.stump.res) →
      QTypedC constTy (S'.parked.map (Parked.toStumpC σ)) (S'.applyCtx Γ) e
        (τ.applySubst σ)

/-- **Redeeming the promises** — a constrained typing whose assumptions all
discharge is a plain typing, at the type the discharges give it. This is the
QTypedC counterpart of `qtyped_applySubst` (QSubst.lean) and the same kind of
work: a transport of the whole derivation along χ. The hypotheses are what the
`stump` case needs (the discharge) and what every other case needs (χ moves
nothing but the δ's, so the scheme instances and lookups the derivation performed
are untouched — the δ's are names the ALGORITHM invented, so Γ-freshness is the
form that fact takes).

Named, not proved. It is the honest remaining obligation of Stage 3: the two
cheap halves above are theorems, this one is a module's worth of transport. -/
def QTypedCDischarge (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ (Γ : QCtx B) (Δ : List (Stump B)) (e : Expr C) (τ : Ty B) (χ : TySubst B),
    QTypedC constTy Δ Γ e τ →
    χ.FixedOutside (Δ.map Stump.res) →
    (∀ α ∈ Δ.map Stump.res, α ∉ Γ.ftv) →
    (∀ s ∈ Δ, s.DischargeEquiv Γ.ctx χ) →
    QTyped constTy Γ e (τ.applySubst χ)

/-- ⊢  **the join, where it is cheap: no promises left.** If the run's Δ is empty
the constrained typing IS a plain one, so `InferSoundC` alone gives `RunSound` for
every program whose stumps all resolved — `fStarEx_runs` is one
(`fStarEx_recovers`: Δ empty, the refinement kept). The general case is the
`QTypedCDischarge` transport. -/
theorem runSound_of_inferSoundC_nil {B C : Type} [DecidableEq B] {constTy : C → B}
    (hc : InferSoundC B C constTy) {Γ : QCtx B} {S S' : SolverState B}
    {e : Expr C} {τ : Ty B} (h : Infer constTy Γ S e τ S') (hnil : S'.parked = [])
    {σ : TySubst B} (hsat : Sol.Sat σ S'.sol) :
    QTyped constTy (S'.applyCtx Γ) e (τ.applySubst σ) := by
  have := hc Γ S S' e τ h σ hsat (by intro p hp; rw [hnil] at hp; exact absurd hp List.not_mem_nil)
  rw [hnil] at this
  exact this.toQTyped

--------------------- WHAT IS NOT PROVED --------------------------------------
-- Eight of the thirteen A-rules are above outright, A-sel-? twice over (modulo a
-- discharge in `infer_sound_selUnk_step`, and outright in
-- `inferC_sound_selUnk_step` once the promise is a hypothesis of the judgement),
-- and A-var down to a statement about SCHEMES. What is left:
--
-- * THE SHAPE OF THE WHOLE, so the items below have somewhere to sit:
--       RunSound = InferSoundC ∘ Finalize.dischargeEquiv ∘ QTypedCDischarge
--   The middle factor is PROVED (one stump per finalization step, modulo the
--   `hfix` side condition it names). The outer two are statements: `InferSoundC`
--   is the induction, whose cases are the lemmas above with `QTyped.toC` applied;
--   `QTypedCDischarge` is the transport that cashes the discharges in.
--
-- * A-var — the typing half is `infer_sound_var_step`; the constraint half is
--   `Wakes.dischargeEquiv`, over a whole run, with `InstStumps.pairwise`
--   supplying its distinctness side condition from the rule's own
--   `FreshRenaming`. What is left:
--     - `QScheme.ResWF`, the invariant `QScheme` does not carry. Stated, not
--       proved: it is a property of the schemes inference BUILDS, in the same
--       sense `UnifyWF` is, and it belongs with that family.
--     - FINALIZATION now DOES discharge: `Finalize.dischargeEquiv` (and
--       `Finalize.discharge_isUnk`, on the nose rather than up to ≈, because the
--       ?-arm is rigid). Every constraint `Wakes.dischargeEquiv` leaves in the
--       parked alternative is handed to finalization, and finalization can now
--       honour it. Getting there took both halves of the F-★ fix: the A-rules
--       saturate (`Infer.quiescent`), and the rule carries `LookupBlocked`.
--       What the theorem still NAMES rather than discharges is `hfix` — σ may not
--       refine the row the stump is blocked on, which is the ?-arm's one
--       asymmetry against the definite arms and the reason finalization runs
--       last. Idempotence (`Sol.Applied`, i.e. `UnifyWF`) gives it at ⟦S⟧.
--     - A-var's premise is now `WakesSat`, so this case peels it into `Wakes`
--       (which `Wakes.dischargeEquiv` consumes unchanged) and a `Saturate`,
--       whose own steps are `Wake`s — so `Wake.dischargeEquiv` covers them too,
--       one stump at a time.
--     - the σ-IMAGE OF THE SCHEME, which is `SchemeImage`/`QCovers`
--       (QSubst.lean) plus the capture-avoidance `QScheme.applySubst` lacks.
--       This is also where χ is built: σ corrected at the constraints' result
--       variables to the types the lookups actually found, which is what pays
--       for the ≈ that `DischargeEquiv` leaves behind.
--
-- * A-let — needs `SchemeImage` (QSubst.lean) and the Δ-split. `qLet` also
--   carries an INHABITATION premise, and the algorithmic side satisfies it "by
--   construction" only because a parked stump always finalizes: so A-let
--   depends on the discharge condition too, at the generalization boundary
--   rather than at the selection site.
--
-- * A-app-degrade and A-sel-degrade — NOT a proof-effort gap, and not the gap
--   `plans/inference-gap-analysis.md` records either. That file asks for
--   "replacing a position by ★ preserves declarative typeability", which is a
--   statement about a term that ALREADY types. Here the term does not: when
--   `τ₁ ≐ τ₂ → β` is stuck, `e₁` has some type, `e₂` has some type, and QTyped
--   has no rule that applies one to the other. `qApp` needs a literal arrow,
--   `qEq` only moves along ≈, and ≈ relates ★ to nothing but itself
--   (`TyEquiv.unk_inv`) — so `qUnk` cannot manufacture the arrow either. The
--   missing piece is a DECLARATIVE rule: application at ★, the ★-elimination
--   the failure policy silently assumes and the type system does not have.
--   Until that exists (or the degradations are proved unreachable, which
--   A-sel-degrade may well be — `r` is drawn fresh immediately before, so
--   `τ ≐ {r}` can only clash or succeed), the two degradation rules are
--   soundness-gapped by construction, and no amount of work on this file
--   closes them.

end MinimalCalculus
