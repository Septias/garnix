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
-- no plain declarative reading. It is still possible to say exactly what the
-- promise has to be worth by the time σ is fixed, and that turns the design
-- question into a statable obligation on wake-up and finalization.
--
-- There are only two honest outcomes, and they are precisely the two rules that
-- can retire a stump:
--   * K-hit fired — the lookup landed, and δ IS what it found;
--   * K-⊥ or F-★ fired — δ is ★.
-- K-repark retires nothing: it moves the blocker and the stump lives on, which
-- is why this is a condition on the FINAL σ and not on any one step.

/-- what a parked stump must be worth under σ for A-sel-? to be sound. -/
def StumpHonest {B : Type} (Γ' : QCtx B) (σ : TySubst B) (st : Stump B) : Prop :=
  Lookup Γ'.ctx (st.row.applySubst σ) st.label (.found (σ.ty st.res)) ∨
    σ.ty st.res = .unk

/-- A-sel-?, given the promise is kept. Both halves land: `K-hit` reads off as
`T-sel`, and `★` reads off as whatever the lookup now says, blurred by
`T-★-intro` — `qtyped_sel_star` is what makes the second half free. -/
theorem infer_sound_selUnk_step {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ' : QCtx B} {σ : TySubst B} {S₁' S₂ : SolverState B}
    {e : Expr C} {τ : Ty B} {l : Label} {r δ : TyVar}
    (hrow : Γ'.rowEnv = [])
    (h : QTyped constTy Γ' e (τ.applySubst σ))
    (hs : SolveTy S₁' τ (.rcd (.var r)) S₂)
    (hsat : Sol.Sat σ S₂.sol)
    (hst : StumpHonest Γ' σ ⟨.var r, l, δ⟩) :
    QTyped constTy Γ' (.sel e l) ((Ty.var δ).applySubst σ) := by
  have hrcd : QTyped constTy Γ' e ((Ty.rcd (.var r)).applySubst σ) :=
    .qEq h (hs.unifies_sat hsat)
  cases hst with
  | inl hf => exact .qSel hrcd hf
  | inr hu =>
      show QTyped constTy Γ' (.sel e l) (σ.ty δ)
      rw [hu]
      exact qtyped_sel_star hrow hrcd

--------------------- WHAT IS NOT PROVED --------------------------------------
-- Eight of the thirteen A-rules are above, plus A-sel-? modulo `StumpHonest`.
-- The remaining four, and what each is actually waiting on:
--
-- * A-var — the K-/D- correspondence. The instantiated constraints were
--   submitted to WAKE-UP; the declarative `QScheme.Inst` demands they
--   DISCHARGE. Relating the two is what the paper asserts and nobody has
--   proved. This is also the case that fixes the shape of the context
--   correspondence — `QSubst.lean`'s `QCovers` is the instance-level half of
--   it — so it should be attacked BEFORE the induction is assembled, not after.
--
-- * A-let — needs `SchemeImage` (QSubst.lean) and the Δ-split. `qLet` also
--   carries an INHABITATION premise, and the algorithmic side satisfies it "by
--   construction" only because a parked stump always finalizes at ★: so A-let
--   depends on `StumpHonest` too, at the generalization boundary rather than at
--   the selection site.
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
