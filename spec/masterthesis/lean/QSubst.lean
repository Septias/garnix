-- TYPE SUBSTITUTION FOR L2  —  transporting a `QTyped` derivation along ⟦S⟧.
--
-- L1 has `typed_applySubst_aux` (minimal.lean); `QTyped` had only TERM
-- substitution (`qsubst_preserves_typing`). This is the lemma
-- `plans/inference-gap-analysis.md` §B lists as "type substitution for L2", and
-- it is what `InferSound` runs on: inference's conclusion is stated at the
-- FINAL state while its premises give typings at INTERMEDIATE ones, and getting
-- from one to the other IS "transport a derivation along the extra solution".
--
-- ## The shape, and why the obvious one is wrong
-- The first attempt was a `RowEnvMap`: "every row-solution of Γ survives into
-- Γ′ with θ applied", so that `L-α` could chase the θ-image of what it chased
-- before. That is WRONG, and the `.var` case says so — `(Row.var α).applySubst θ`
-- is `θ.row α`, so after substituting there is no variable left to chase. The
-- solution has to be DISCHARGED into the substitution rather than carried
-- alongside it, which is exactly `Sol.Closes`, and the transport is then
-- `Sol.lookup_toCtx` (RowUnify/State.lean) — already proved.
--
-- So the target context has an EMPTY row environment and σ closes the state.
-- That is also why `InferSound` carries a `Closes` hypothesis.

import Qualified
import RowUnify.State

namespace MinimalCalculus

--------------------- WHEN Γ′ IS Γ READ UNDER σ ------------------------------

/-- `QCovers s σ σ₀ σ₀'` — σ₀′ is σ₀ read under σ, at the level of INSTANCES.
Two-sided on purpose: the forward half moves `qVar`, and the backward half is
what `qLet` needs, since its premise quantifies over all instances of the
scheme and must be re-fed an instance of the IMAGE scheme. -/
def QCovers {B : Type} (s : Sol B) (σ : TySubst B) (σ₀ σ₀' : QScheme B) : Prop :=
  (∀ τ, QScheme.Inst s.toCtx σ₀ τ →
      QScheme.Inst (⟨[], []⟩ : Ctx B) σ₀' (τ.applySubst σ)) ∧
  (∀ τ', QScheme.Inst (⟨[], []⟩ : Ctx B) σ₀' τ' →
      ∃ τ, QScheme.Inst s.toCtx σ₀ τ ∧ τ' = τ.applySubst σ)

/-- `Γ′` is `Γ` closed by σ: same term variables, each scheme covered, the row
environment discharged into σ. -/
structure QInstMap {B : Type} (s : Sol B) (σ : TySubst B) (Γ Γ' : QCtx B) : Prop where
  src   : Γ.rowEnv = s.row
  tgt   : Γ'.rowEnv = []
  schem : ∀ x σ₀, Γ.lookup x = some σ₀ →
            ∃ σ₀', Γ'.lookup x = some σ₀' ∧ QCovers s σ σ₀ σ₀'

-- ⊢  Γ.ctx is literally ⟦s⟧ when the row environments agree
theorem QInstMap.ctx_src {B : Type} {s : Sol B} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap s σ Γ Γ') : Γ.ctx = s.toCtx := by
  unfold QCtx.ctx Sol.toCtx; rw [hm.src]

theorem QInstMap.ctx_tgt {B : Type} {s : Sol B} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap s σ Γ Γ') : Γ'.ctx.rowEnv = [] := hm.tgt

theorem QInstMap.ctx_tgt_eq {B : Type} {s : Sol B} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap s σ Γ Γ') : Γ'.ctx = (⟨[], []⟩ : Ctx B) := by
  unfold QCtx.ctx; rw [hm.tgt]

-- ⊢  a MONOTYPE scheme has exactly one instance — its own body
theorem inst_mono_self {B : Type} (Γ : Ctx B) (τ : Ty B) :
    QScheme.Inst Γ ⟨[], [], τ⟩ τ :=
  QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], τ⟩)

theorem inst_mono_eq {B : Type} {Γ : Ctx B} {τ τ' : Ty B}
    (h : QScheme.Inst Γ ⟨[], [], τ⟩ τ') : τ' = τ := by
  obtain ⟨θ, hfix, -, hb⟩ := h
  rw [← hb]
  exact Ty.applySubst_fixed_ftv τ
    (fun α _ => ⟨hfix.1 α (by simp), hfix.2 α (by simp)⟩)

-- ⊢  a MONOTYPE binding is covered for free: with no quantifiers its only
--    instance is its own body, at either context
theorem QCovers.mono {B : Type} (s : Sol B) (σ : TySubst B) (τ : Ty B) :
    QCovers s σ ⟨[], [], τ⟩ ⟨[], [], τ.applySubst σ⟩ := by
  refine ⟨fun τ₁ h => ?_, fun τ' h => ?_⟩
  · rw [inst_mono_eq h]; exact inst_mono_self _ _
  · exact ⟨τ, inst_mono_self _ _, by rw [inst_mono_eq h]⟩

-- ⊢  extending both sides with a λ-bound monotype keeps the map
theorem QInstMap.bindTy {B : Type} {s : Sol B} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap s σ Γ Γ') (x : Var) (τ : Ty B) :
    QInstMap s σ (Γ.bindTy x τ) (Γ'.bindTy x (τ.applySubst σ)) where
  src := hm.src
  tgt := hm.tgt
  schem := by
    intro y σ₀ h
    simp only [QCtx.bindTy, QCtx.lookup_bindScheme] at h ⊢
    by_cases hxy : (x == y) = true
    · rw [if_pos hxy] at h ⊢
      injection h with h; subst h
      exact ⟨_, rfl, QCovers.mono s σ τ⟩
    · rw [if_neg hxy] at h ⊢
      obtain ⟨σ₀', hl, hc⟩ := hm.schem y σ₀ h
      exact ⟨σ₀', hl, hc⟩

-- ⊢  …and with a let-bound scheme, given the covering
theorem QInstMap.bindScheme {B : Type} {s : Sol B} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap s σ Γ Γ') (x : Var) {σ₀ σ₀' : QScheme B}
    (hc : QCovers s σ σ₀ σ₀') :
    QInstMap s σ (Γ.bindScheme x σ₀) (Γ'.bindScheme x σ₀') where
  src := hm.src
  tgt := hm.tgt
  schem := by
    intro y σ₁ h
    simp only [QCtx.lookup_bindScheme] at h ⊢
    by_cases hxy : (x == y) = true
    · rw [if_pos hxy] at h ⊢
      injection h with h; subst h
      exact ⟨σ₀', rfl, hc⟩
    · rw [if_neg hxy] at h ⊢
      obtain ⟨σ₁', hl, hcc⟩ := hm.schem y σ₁ h
      exact ⟨σ₁', hl, hcc⟩


--------------------- THE TRANSPORT ------------------------------------------
-- The one construction this does NOT do is build the σ-image of a LET-BOUND
-- scheme. `QInstMap` supplies images for the schemes already in Γ; `A-let` /
-- `qLet` introduces a fresh one, and σ has to be pushed under its binders
-- without capture. That is the L2 analogue of L1's `renameScheme`, and it is
-- taken as a hypothesis here so the rest of the transport can be proved and the
-- remaining obligation named exactly.

/-- every scheme has a σ-image that covers it. The capture-avoiding
construction; L1's counterpart is `renameScheme`. -/
def SchemeImage {B : Type} (s : Sol B) (σ : TySubst B) : Prop :=
  ∀ σ₀ : QScheme B, ∃ σ₀', QCovers s σ σ₀ σ₀'

mutual

/-- ⊢  **L2 type substitution**: a `QTyped` derivation transports along a
solution's closure, into the context read under it. -/
theorem qtyped_applySubst {B C : Type} {constTy : C → B} {s : Sol B}
    {σ : TySubst B} (hcl : s.Closes σ) (him : SchemeImage s σ) :
    {Γ Γ' : QCtx B} → {e : Expr C} → {τ : Ty B} →
    QTyped constTy Γ e τ → QInstMap s σ Γ Γ' →
    QTyped constTy Γ' e (τ.applySubst σ)
  | _, _, _, _, .qCon, _ => .qCon
  | _, _, _, _, .qVar hl hi, hm => by
      obtain ⟨σ₀', hl', hc⟩ := hm.schem _ _ hl
      refine .qVar hl' ?_
      rw [hm.ctx_tgt_eq]
      exact hc.1 _ (by rw [← hm.ctx_src]; exact hi)
  | _, _, _, _, .qEq h heq, hm =>
      .qEq (qtyped_applySubst hcl him h hm) (TyEquiv.applySubst σ heq)
  | _, _, _, _, .qLam h, hm =>
      .qLam (qtyped_applySubst hcl him h (hm.bindTy _ _))
  | _, _, _, _, .qApp h₁ h₂, hm =>
      .qApp (qtyped_applySubst hcl him h₁ hm) (qtyped_applySubst hcl him h₂ hm)
  | _, _, _, _, .qLet (σ := σ₀) hinst hinh hbody, hm => by
      obtain ⟨σ₀', hc⟩ := him σ₀
      have hctx := hm.ctx_tgt_eq
      refine .qLet (σ := σ₀') (fun τ₁' hi => ?_) ?_ ?_
      · rw [hctx] at hi
        obtain ⟨τ₁, hi₁, rfl⟩ := hc.2 _ hi
        exact qtyped_applySubst hcl him (hinst τ₁ (by rw [hm.ctx_src]; exact hi₁)) hm
      · obtain ⟨τ₁, hi₁⟩ := hinh
        exact ⟨_, by rw [hctx]; exact hc.1 _ (by rw [← hm.ctx_src]; exact hi₁)⟩
      · exact qtyped_applySubst hcl him hbody (hm.bindScheme _ hc)
  | _, _, _, _, .qCat h₁ h₂, hm =>
      .qCat (qtyped_applySubst hcl him h₁ hm) (qtyped_applySubst hcl him h₂ hm)
  | _, _, _, _, .qSel h hlk, hm => by
      refine .qSel (qtyped_applySubst hcl him h hm) ?_
      exact Sol.lookup_toCtx hcl hm.ctx_tgt
        (by rw [← hm.ctx_src]; exact hlk)
  | _, _, _, _, .qSelUnk h hlk, hm => by
      refine .qSelUnk (qtyped_applySubst hcl him h hm) ?_
      exact Sol.lookup_toCtx hcl hm.ctx_tgt
        (by rw [← hm.ctx_src]; exact hlk)
  | _, _, _, _, .qSelAbs h hlk, hm => by
      refine .qSelAbs (qtyped_applySubst hcl him h hm) ?_
      exact Sol.lookup_toCtx hcl hm.ctx_tgt
        (by rw [← hm.ctx_src]; exact hlk)
  | _, _, _, _, .qUnk h, hm => .qUnk (qtyped_applySubst hcl him h hm)
  | _, _, _, _, .qRcd h, hm => .qRcd (qtypedBody_applySubst hcl him h hm)

theorem qtypedBody_applySubst {B C : Type} {constTy : C → B} {s : Sol B}
    {σ : TySubst B} (hcl : s.Closes σ) (him : SchemeImage s σ) :
    {Γ Γ' : QCtx B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    QTypedBody constTy Γ ξ ρ → QInstMap s σ Γ Γ' →
    QTypedBody constTy Γ' ξ (ρ.applySubst σ)
  | _, _, _, _, .empty, _ => .empty
  | _, _, _, _, .field h, hm => .field (qtyped_applySubst hcl him h hm)
  | _, _, _, _, .cat h₁ h₂, hm =>
      .cat (qtypedBody_applySubst hcl him h₁ hm) (qtypedBody_applySubst hcl him h₂ hm)

end

end MinimalCalculus
