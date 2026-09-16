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

--------------------- F-★ IS DEFECTIVE, AND HERE IS THE WITNESS --------------
-- `Finalize.star` (Infer.lean) reads
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
-- THE FIX is to give F-★ the premise its siblings have — `LookupBlocked` on the
-- row it is finalizing, which is also exactly the hypothesis A-sel-? already
-- establishes when it parks the stump. Nothing below depends on this; it is
-- recorded so the rule is not shipped as it stands.

/-- a stump on a LITERAL row: its lookup lands at every context, under every
substitution. Nothing about it is blocked, and F-★ does not care. -/
private def fStar_p : Parked Unit := ⟨"a", ⟨.sing "l" (.base ()), "l", "d"⟩⟩

private def fStar_S : SolverState Unit := ⟨Sol.nil, [fStar_p], [], ⟨0⟩⟩

/-- the equation F-★ emits, run: `δ ≐ ★` binds δ, at any fuel and any supply. -/
example : unifyTyF (B := Unit) [] ⟨0⟩ 0 (.var "d") .unk
    = .success ⟨[("d", (.unk : Ty Unit))], []⟩ ⟨0⟩ := rfl

/-- ⊢  **F-★ can fire where nothing discharges.** There is a state, a parked
stump and a finalization step whose result NO substitution satisfying it can
discharge — not even up to ≈. -/
theorem finalize_star_no_discharge :
    ∃ (S S'' : SolverState Unit) (p : Parked Unit),
      Finalize S p S'' ∧
      ∀ (Γ' : Ctx Unit) (σ : TySubst Unit), Sol.Sat σ S''.sol →
        ¬ p.stump.DischargeEquiv Γ' σ := by
  have hfin : Finalize fStar_S fStar_p _ :=
    Finalize.star (S' := fStar_S.extend ⟨[("d", .unk)], []⟩ ⟨0⟩)
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

--------------------- WHAT IS NOT PROVED --------------------------------------
-- Eight of the thirteen A-rules are above outright, A-sel-? modulo a discharge,
-- and A-var down to a statement about SCHEMES. What is left:
--
-- * A-var — the typing half is `infer_sound_var_step`; the constraint half is
--   `Wakes.dischargeEquiv`, over a whole run, with `InstStumps.pairwise`
--   supplying its distinctness side condition from the rule's own
--   `FreshRenaming`. What is left:
--     - `QScheme.ResWF`, the invariant `QScheme` does not carry. Stated, not
--       proved: it is a property of the schemes inference BUILDS, in the same
--       sense `UnifyWF` is, and it belongs with that family.
--     - FINALIZATION does not discharge, and that is a defect in F-★ rather
--       than a gap here — see `finalize_star_no_discharge` above. Every
--       constraint `Wakes.dischargeEquiv` leaves in the parked alternative is
--       handed to finalization, and finalization cannot honour it as written.
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
