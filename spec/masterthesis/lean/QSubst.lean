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


--------------------- PUSHING σ UNDER A SCHEME'S BINDERS ----------------------
-- `QScheme.applySubst` (Qualified.lean) substitutes under `σ₀.vars` without
-- renaming. `QScheme.Avoiding` is the side condition that makes that safe, and
-- this is what it buys: every instance of σ₀ has its σ-image as an instance of
-- the image scheme. That is the FORWARD half of `QCovers` — the half `qVar`
-- consumes — and it is the substantive content of the capture-avoidance note
-- that used to sit on `QScheme.applySubst`.
--
-- The instantiation that witnesses it: on a binder, θ followed by σ; elsewhere
-- the identity. `Avoiding` is what makes this agree with "σ first, then θ".

private def instSub {B : Type} (vs : List TyVar) (θ σ : TySubst B) : TySubst B :=
  ⟨fun α => if α ∈ vs then (θ.ty  α).applySubst σ else .var α,
   fun α => if α ∈ vs then (θ.row α).applySubst σ else .var α⟩

private theorem instSub_fixed {B : Type} (vs : List TyVar) (θ σ : TySubst B) :
    (instSub vs θ σ).FixedOutside vs :=
  ⟨fun _ h => by simp [instSub, h], fun _ h => by simp [instSub, h]⟩

private theorem instSub_mem {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    {α : TyVar} (h : α ∈ vs) :
    (instSub vs θ σ).ty α = (θ.ty α).applySubst σ := by simp [instSub, h]

-- ⊢ under `Avoiding`, "σ then the witness" and "θ then σ" agree at every
--   variable the scheme can reach.
private theorem comp_agree {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α)
    {α : TyVar}
    (hav : α ∉ vs → (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs)) :
    ((instSub vs θ σ).comp σ).ty α = (σ.comp θ).ty α ∧
    ((instSub vs θ σ).comp σ).row α = (σ.comp θ).row α := by
  by_cases hα : α ∈ vs
  · obtain ⟨ht, hr⟩ := hσfix α hα
    refine ⟨?_, ?_⟩
    · show (σ.ty α).applySubst _ = (θ.ty α).applySubst σ
      rw [ht]; show (instSub vs θ σ).ty α = _; simp [instSub, hα]
    · show (σ.row α).applySubst _ = (θ.row α).applySubst σ
      rw [hr]; show (instSub vs θ σ).row α = _; simp [instSub, hα]
  · obtain ⟨hvt, hvr⟩ := hav hα
    refine ⟨?_, ?_⟩
    · show (σ.ty α).applySubst _ = (θ.ty α).applySubst σ
      rw [hfix.1 α hα]
      show _ = σ.ty α
      exact Ty.applySubst_fixed_ftv _
        (fun β hβ => by simp [instSub, hvt β hβ])
    · show (σ.row α).applySubst _ = (θ.row α).applySubst σ
      rw [hfix.2 α hα]
      show _ = σ.row α
      exact Row.applySubst_fixed_ftv _
        (fun β hβ => by simp [instSub, hvr β hβ])

-- the two congruence corollaries, at each sort
private theorem swap_ty {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α)
    (τ : Ty B)
    (hav : ∀ α ∈ τ.ftv, α ∉ vs →
      (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs)) :
    (τ.applySubst σ).applySubst (instSub vs θ σ) = (τ.applySubst θ).applySubst σ := by
  rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]
  exact Ty.applySubst_congr τ (fun α hα => comp_agree hfix hσfix (hav α hα))

private theorem swap_row {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α)
    (ρ : Row B)
    (hav : ∀ α ∈ ρ.ftv, α ∉ vs →
      (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs)) :
    (ρ.applySubst σ).applySubst (instSub vs θ σ) = (ρ.applySubst θ).applySubst σ := by
  rw [Row.applySubst_applySubst, Row.applySubst_applySubst]
  exact Row.applySubst_congr ρ (fun α hα => comp_agree hfix hσfix (hav α hα))

/-- ⊢  **the forward half of `QCovers`, proved.** Under `Avoiding`, the σ-image
of every instance of σ₀ is an instance of `σ₀.applySubst σ` — discharge and all.
The discharge cases go through `Sol.lookup_toCtx`, which is what carries a lookup
out of ⟦s⟧ and into the empty row environment the image scheme instantiates at. -/
theorem QCovers.forward_of_avoiding {B : Type} {s : Sol B} {σ : TySubst B}
    (hcl : s.Closes σ) {σ₀ : QScheme B} (hwf : σ₀.WF) (hav : σ₀.Avoiding σ) :
    ∀ τ, QScheme.Inst s.toCtx σ₀ τ →
      QScheme.Inst (⟨[], []⟩ : Ctx B) (σ₀.applySubst σ) (τ.applySubst σ) := by
  rintro τ ⟨θ, hfix, hdis, rfl⟩
  obtain ⟨hσfix, havf⟩ := hav
  refine ⟨instSub σ₀.vars θ σ, instSub_fixed _ _ _, ?_, ?_⟩
  · -- every substituted constraint discharges under the witness
    intro st' hst'
    simp only [QScheme.applySubst, List.mem_map] at hst'
    obtain ⟨st, hst, rfl⟩ := hst'
    -- the row of this stump is reachable, so the swap applies to it
    have hrow : (st.row.applySubst σ).applySubst (instSub σ₀.vars θ σ)
        = (st.row.applySubst θ).applySubst σ :=
      swap_row hfix hσfix st.row (fun α hα =>
        havf α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, hα⟩)))
    -- δ is a binder, so the witness reads θ at it
    have hres : (instSub σ₀.vars θ σ).ty st.res = (θ.ty st.res).applySubst σ :=
      instSub_mem (hwf st hst)
    cases hdis st hst with
    | @hit τr hlk hδ =>
        refine .hit (τ := τr.applySubst σ) ?_ ?_
        · show Lookup _ ((st.row.applySubst σ).applySubst _) _ _
          rw [hrow]
          exact Sol.lookup_toCtx hcl rfl hlk
        · rw [hres, hδ]
    | abs hlk hδ =>
        refine .abs ?_ ?_
        · show Lookup _ ((st.row.applySubst σ).applySubst _) _ _
          rw [hrow]
          exact Sol.lookup_toCtx (r := .absent) hcl rfl hlk
        · rw [hres, hδ]; rfl
    | unk hlk hδ =>
        refine .unk ?_ ?_
        · show Lookup _ ((st.row.applySubst σ).applySubst _) _ _
          rw [hrow]
          exact Sol.lookup_toCtx (r := .unknown) hcl rfl hlk
        · rw [hres, hδ]; rfl
  · -- and the body lands where it should
    exact swap_ty hfix hσfix σ₀.body (fun α hα =>
      havf α (List.mem_append_right _ hα))

--------------------- …AND THE BACKWARD HALF IS FALSE -------------------------
-- `QCovers` is two-sided, and the other half does NOT hold for the naive image,
-- `Avoiding` or not. The obstruction is not capture: it is that `applySubst σ`
-- is not surjective, while an instance set always is "as large as its binders".
--
--   s = (b ≔ 𝓫)      σ = ⟦s⟧      σ₀ = ∀a. a
--
-- σ₀ is `Avoiding` σ (σ fixes `a`), and `σ₀.applySubst σ` is σ₀ again. Its
-- instances are ALL types — `b` among them. But `b` is not the σ-image of
-- anything: σ sends `b` to 𝓫 and fixes every other variable, so nothing maps
-- onto `b`. The backward half demands a preimage and there is none.
--
-- This is why `SchemeImage` was right to be a hypothesis rather than a
-- definition, and it says something sharper than "unproved": the obvious
-- witness is refuted, so a proof must produce a DIFFERENT scheme. See the note
-- after the refutation for how far that looks like it can go.

private def refuteSol : Sol Unit := ⟨[("b", .base ())], []⟩
private def refuteSub : TySubst Unit := refuteSol.toSubst
private def refuteScheme : QScheme Unit := ⟨["a"], [], .var "a"⟩

private theorem refuteSol_applied : refuteSol.Applied := by
  refine ⟨fun p hp => ?_, fun _ hp => nomatch hp⟩
  obtain rfl := List.mem_singleton.mp hp
  rfl

private theorem refuteSol_closes : refuteSol.Closes refuteSub :=
  Sol.closes_toSubst_of_applied refuteSol_applied

private theorem refuteScheme_wf : refuteScheme.WF := fun _ hst => nomatch hst

private theorem refuteScheme_avoiding : refuteScheme.Avoiding refuteSub := by
  refine ⟨fun α hα => ?_, fun α hα hn => ?_⟩
  · obtain rfl := List.mem_singleton.mp hα
    exact ⟨rfl, rfl⟩
  · exact absurd hα hn

-- the image scheme is σ₀ itself: σ fixes `a`
private theorem refuteScheme_image :
    refuteScheme.applySubst refuteSub = refuteScheme := rfl

-- `b` IS an instance of the image scheme…
private def bWitness : TySubst Unit :=
  ⟨fun α => if α = "a" then .var "b" else .var α, fun α => .var α⟩

private theorem b_inst :
    QScheme.Inst (⟨[], []⟩ : Ctx Unit) (refuteScheme.applySubst refuteSub)
      (.var "b") := by
  refine ⟨bWitness, ⟨fun α h => ?_, fun _ _ => rfl⟩, ?_, rfl⟩
  · have hne : α ≠ "a" := by
      intro he
      exact h (by rw [he]; show "a" ∈ ["a"]; simp)
    simp [bWitness, hne]
  · intro st hst
    exact nomatch hst

-- …and nothing maps onto it
private theorem no_preimage (τ : Ty Unit) : τ.applySubst refuteSub ≠ .var "b" := by
  cases τ with
  | base _ => exact fun h => nomatch h
  | unk    => exact fun h => nomatch h
  | fn _ _ => exact fun h => nomatch h
  | rcd _  => exact fun h => nomatch h
  | var c  =>
      show tyLookup c [("b", (.base () : Ty Unit))] ≠ _
      by_cases hc : ("b" : TyVar) = c
      · subst hc
        simp only [tyLookup]
        exact fun h => nomatch h
      · simp only [tyLookup, if_neg hc]
        intro h
        injection h with h'
        exact hc h'.symm

/-- ⊢  **the backward half of `QCovers` fails for `QScheme.applySubst`**, even
under `Closes`, `WF` and `Avoiding`. So `σ₀.applySubst σ` is not a `SchemeImage`
witness, and the hypothesis cannot be discharged by pushing σ through the
scheme. -/
theorem qcovers_backward_false_for_applySubst :
    ¬ (∀ (s : Sol Unit) (σ : TySubst Unit), s.Closes σ →
        ∀ σ₀ : QScheme Unit, σ₀.WF → σ₀.Avoiding σ →
          ∀ τ', QScheme.Inst (⟨[], []⟩ : Ctx Unit) (σ₀.applySubst σ) τ' →
            ∃ τ, QScheme.Inst s.toCtx σ₀ τ ∧ τ' = τ.applySubst σ) := by
  intro h
  obtain ⟨τ, -, hτ⟩ :=
    h refuteSol refuteSub refuteSol_closes refuteScheme
      refuteScheme_wf refuteScheme_avoiding (.var "b") b_inst
  exact no_preimage τ hτ.symm

-- WHAT THIS LEAVES. The same example looks fatal for `SchemeImage` itself, not
-- just for this witness: forward forces the image scheme's body to be one of its
-- own binders (its instances must include both 𝓫 and an arrow), and a scheme
-- whose body is a bound variable has EVERY type as an instance unless a stump
-- blocks it — while the σ-image of ∀a.a is exactly the types not mentioning `b`.
-- Ruling out every stump configuration is what a full refutation still owes, so
-- this is recorded as the shape of the obstruction, not as a theorem.

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
