-- TYPE SUBSTITUTION FOR L2  —  transporting a `QTyped` derivation along ⟦S⟧.
--
-- ## The shape
-- Historically this module's difficulty WAS the row environment. The first
-- attempt was a `RowEnvMap` — "every row-solution of Γ survives into Γ′ with θ
-- applied", so `L-α` could chase the θ-image of what it chased before — and the
-- `.var` case refuted it: `(Row.var α).applySubst θ` is `θ.row α`, so after
-- substituting there is no variable left to chase. The conclusion drawn then
-- was that the solution must be DISCHARGED into the substitution rather than
-- carried alongside it.
--
-- With `L-α` gone that conclusion is the only option there is, and it costs
-- nothing: a context carries no solutions to carry along, `QScheme.Inst` reads
-- no context, so `QCovers` compares instance sets outright and `QInstMap` is
-- pure term-environment bookkeeping. The θ ↦ rowEnv bridge that used to open
-- this file (`Sol.lookupV_toCtx`, `Sol.lookupQ_toCtx`,
-- `Sol.lookupQ_toCtx_definite`) is deleted: `LookupQ.applySubst` is what it
-- was an implementation of.

import Qualified
import RowUnify.State

namespace MinimalCalculus

--------------------- WHEN Γ′ IS Γ READ UNDER σ ------------------------------

/-- `QCovers σ σ₀ σ₀'` — σ₀′ is σ₀ read under σ, at the level of INSTANCES.
Two-sided on purpose: the forward half moves `qVar`, and the backward half is
what `qLet` needs, since its premise quantifies over all instances of the
scheme and must be re-fed an instance of the IMAGE scheme. -/
def QCovers {B : Type} (σ : TySubst B) (σ₀ σ₀' : QScheme B) : Prop :=
  (∀ τ, QScheme.Inst σ₀ τ → QScheme.Inst σ₀' (τ.applySubst σ)) ∧
  (∀ τ', QScheme.Inst σ₀' τ' → ∃ τ, QScheme.Inst σ₀ τ ∧ τ' = τ.applySubst σ)

/-- `Γ′` is `Γ` closed by σ: same term variables, each scheme covered. -/
structure QInstMap {B : Type} (σ : TySubst B) (Γ Γ' : QCtx B) : Prop where
  schem : ∀ x σ₀, Γ.lookup x = some σ₀ →
            ∃ σ₀', Γ'.lookup x = some σ₀' ∧ QCovers σ σ₀ σ₀' 

-- ⊢  a MONOTYPE scheme has exactly one instance — its own body
theorem inst_mono_self {B : Type} (τ : Ty B) :
    QScheme.Inst ⟨[], [], τ⟩ τ :=
  QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], τ⟩)

theorem inst_mono_eq {B : Type} {τ τ' : Ty B}
    (h : QScheme.Inst ⟨[], [], τ⟩ τ') : τ' = τ := by
  obtain ⟨θ, hfix, -, hb⟩ := h
  rw [← hb]
  exact Ty.applySubst_fixed_ftv τ
    (fun α _ => ⟨hfix.1 α (by simp), hfix.2.1 α (by simp), hfix.2.2 α (by simp)⟩)

-- ⊢  a MONOTYPE binding is covered for free: with no quantifiers its only
--    instance is its own body, at either context
theorem QCovers.mono {B : Type} (σ : TySubst B) (τ : Ty B) :
    QCovers σ ⟨[], [], τ⟩ ⟨[], [], τ.applySubst σ⟩ := by
  refine ⟨fun τ₁ h => ?_, fun τ' h => ?_⟩
  · rw [inst_mono_eq h]; exact inst_mono_self _
  · exact ⟨τ, inst_mono_self _, by rw [inst_mono_eq h]⟩

-- ⊢  extending both sides with a λ-bound monotype keeps the map
theorem QInstMap.bindTy {B : Type} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap σ Γ Γ') (x : Var) (τ : Ty B) :
    QInstMap σ (Γ.bindTy x τ) (Γ'.bindTy x (τ.applySubst σ)) where
  schem := by
    intro y σ₀ h
    simp only [QCtx.bindTy, QCtx.lookup_bindScheme] at h ⊢
    by_cases hxy : (x == y) = true
    · rw [if_pos hxy] at h ⊢
      injection h with h; subst h
      exact ⟨_, rfl, QCovers.mono σ τ⟩
    · rw [if_neg hxy] at h ⊢
      obtain ⟨σ₀', hl, hc⟩ := hm.schem y σ₀ h
      exact ⟨σ₀', hl, hc⟩

-- ⊢  …and with a let-bound scheme, given the covering
theorem QInstMap.bindScheme {B : Type} {σ : TySubst B} {Γ Γ' : QCtx B}
    (hm : QInstMap σ Γ Γ') (x : Var) {σ₀ σ₀' : QScheme B}
    (hc : QCovers σ σ₀ σ₀') :
    QInstMap σ (Γ.bindScheme x σ₀) (Γ'.bindScheme x σ₀') where
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
   fun α => if α ∈ vs then (θ.row α).applySubst σ else .var α,
   fun α => if α ∈ vs then (θ.lab α).applySubst σ else .var α⟩

private theorem instSub_fixed {B : Type} (vs : List TyVar) (θ σ : TySubst B) :
    (instSub vs θ σ).FixedOutside vs :=
  ⟨fun _ h => by simp [instSub, h], fun _ h => by simp [instSub, h],
   fun _ h => by simp [instSub, h]⟩

private theorem instSub_mem {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    {α : TyVar} (h : α ∈ vs) :
    (instSub vs θ σ).ty α = (θ.ty α).applySubst σ := by simp [instSub, h]

-- ⊢ under `Avoiding`, "σ then the witness" and "θ then σ" agree at every
--   variable the scheme can reach.
private theorem comp_agree {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α ∧ σ.lab α = .var α)
    {α : TyVar}
    (hav : α ∉ vs → (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs) ∧
      (∀ β ∈ (σ.lab α).ftv, β ∉ vs)) :
    ((instSub vs θ σ).comp σ).ty α = (σ.comp θ).ty α ∧
    ((instSub vs θ σ).comp σ).row α = (σ.comp θ).row α ∧
    ((instSub vs θ σ).comp σ).lab α = (σ.comp θ).lab α := by
  by_cases hα : α ∈ vs
  · obtain ⟨ht, hr, hl⟩ := hσfix α hα
    refine ⟨?_, ?_, ?_⟩
    · show (σ.ty α).applySubst _ = (θ.ty α).applySubst σ
      rw [ht]; show (instSub vs θ σ).ty α = _; simp [instSub, hα]
    · show (σ.row α).applySubst _ = (θ.row α).applySubst σ
      rw [hr]; show (instSub vs θ σ).row α = _; simp [instSub, hα]
    · show (σ.lab α).applySubst _ = (θ.lab α).applySubst σ
      rw [hl]; show (instSub vs θ σ).lab α = _; simp [instSub, hα]
  · obtain ⟨hvt, hvr, hvl⟩ := hav hα
    refine ⟨?_, ?_, ?_⟩
    · show (σ.ty α).applySubst _ = (θ.ty α).applySubst σ
      rw [hfix.1 α hα]
      show _ = σ.ty α
      exact Ty.applySubst_fixed_ftv _
        (fun β hβ => by simp [instSub, hvt β hβ])
    · show (σ.row α).applySubst _ = (θ.row α).applySubst σ
      rw [hfix.2.1 α hα]
      show _ = σ.row α
      exact Row.applySubst_fixed_ftv _
        (fun β hβ => by simp [instSub, hvr β hβ])
    · show (σ.lab α).applySubst _ = (θ.lab α).applySubst σ
      rw [hfix.2.2 α hα]
      show _ = σ.lab α
      cases hk : σ.lab α with
      | lit _ => rfl
      | var γ =>
          have := hvl γ (by simp [hk, Key.ftv])
          simp [instSub, this]

-- the congruence corollaries, at each sort
private theorem swap_ty {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α ∧ σ.lab α = .var α)
    (τ : Ty B)
    (hav : ∀ α ∈ τ.ftv, α ∉ vs →
      (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs) ∧
      (∀ β ∈ (σ.lab α).ftv, β ∉ vs)) :
    (τ.applySubst σ).applySubst (instSub vs θ σ) = (τ.applySubst θ).applySubst σ := by
  rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]
  exact Ty.applySubst_congr τ (fun α hα => comp_agree hfix hσfix (hav α hα))

private theorem swap_row {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α ∧ σ.lab α = .var α)
    (ρ : Row B)
    (hav : ∀ α ∈ ρ.ftv, α ∉ vs →
      (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs) ∧
      (∀ β ∈ (σ.lab α).ftv, β ∉ vs)) :
    (ρ.applySubst σ).applySubst (instSub vs θ σ) = (ρ.applySubst θ).applySubst σ := by
  rw [Row.applySubst_applySubst, Row.applySubst_applySubst]
  exact Row.applySubst_congr ρ (fun α hα => comp_agree hfix hσfix (hav α hα))

private theorem swap_key {B : Type} {vs : List TyVar} {θ σ : TySubst B}
    (hfix : θ.FixedOutside vs)
    (hσfix : ∀ α ∈ vs, σ.ty α = .var α ∧ σ.row α = .var α ∧ σ.lab α = .var α)
    (k : Key)
    (hav : ∀ α ∈ k.ftv, α ∉ vs →
      (∀ β ∈ (σ.ty α).ftv, β ∉ vs) ∧ (∀ β ∈ (σ.row α).ftv, β ∉ vs) ∧
      (∀ β ∈ (σ.lab α).ftv, β ∉ vs)) :
    (k.applySubst σ).applySubst (instSub vs θ σ) = (k.applySubst θ).applySubst σ := by
  rw [Key.applySubst_applySubst, Key.applySubst_applySubst]
  exact Key.applySubst_congr k (fun α hα => (comp_agree hfix hσfix (hav α hα)).2.2)

/-- σ resolves no stump that the instantiation left PARKED: wherever its lookup
came out `?`, it is still `?` after σ.

THIS PREMISE IS NEW, and it is the one place where removing the row environment
costs something rather than saving something. The old statement asked for
`s.Closes σ` instead, and that hypothesis was doing hidden work in the `unk`
arm below: a lookup that is `?` in ⟦s⟧-AS-A-CONTEXT is `?` after applying the
closure, because the closure has nothing left to solve. Read the solution as a
substitution and the source lookup no longer knows that — a parked `?` on a free
row-variable CAN become a definite hit under σ — and the arm is genuinely FALSE
without a premise saying it does not. Same mechanism as
`covered_not_applySubst_stable` (Qualified.lean): instance sets MOVE when a
parked stump wakes up, so the forward half of `QCovers` holds exactly for the
substitutions that wake nothing. `Sol.Closes` implies it; so does any σ whose
domain misses the stumps' spine variables and keys, which is what A-var's
`FreshRenaming` arranges. -/
def QScheme.ParkStable {B : Type} (σ₀ : QScheme B) (σ : TySubst B) : Prop :=
  ∀ θ : TySubst B, ∀ st ∈ σ₀.constraints,
    LookupQ (st.row.applySubst θ) (st.label.applySubst θ) .unknown →
    LookupQ ((st.row.applySubst θ).applySubst σ)
      ((st.label.applySubst θ).applySubst σ) .unknown

/-- ⊢  **the forward half of `QCovers`, proved.** Under `Avoiding` and
`ParkStable`, the σ-image of every instance of σ₀ is an instance of
`σ₀.applySubst σ` — discharge and all. The two definite discharge cases go
through `LookupQ.applySubst`, the `?` case through `ParkStable`.

For EVERY key. This used to be restricted to static keys: ⟦s⟧-as-a-context held
row solutions but no label solutions, so a `?` on a solved label variable could
not be transported, and lifting it needed `Γ·(α = ℓ)` in `Ctx`. With lookup
read by substitution, a key is substituted exactly like a row, and the
restriction has nothing left to say. -/
theorem QCovers.forward_of_avoiding {B : Type} {σ : TySubst B}
    {σ₀ : QScheme B} (hwf : σ₀.WF) (hav : σ₀.Avoiding σ)
    (hpark : σ₀.ParkStable σ) :
    ∀ τ, QScheme.Inst σ₀ τ →
      QScheme.Inst (σ₀.applySubst σ) (τ.applySubst σ) := by
  rintro τ ⟨θ, hfix, hdis, rfl⟩
  obtain ⟨hσfix, havf⟩ := hav
  refine ⟨instSub σ₀.vars θ σ, instSub_fixed _ _ _, ?_, ?_⟩
  · -- every substituted constraint discharges under the witness
    intro st' hst'
    simp only [QScheme.applySubst, List.mem_map] at hst'
    obtain ⟨st, hst, rfl⟩ := hst'
    -- the row and the key of this stump are reachable, so the swap applies
    have hrow : (st.row.applySubst σ).applySubst (instSub σ₀.vars θ σ)
        = (st.row.applySubst θ).applySubst σ :=
      swap_row hfix hσfix st.row (fun α hα =>
        havf α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_left _ hα⟩)))
    have hlab : (st.label.applySubst σ).applySubst (instSub σ₀.vars θ σ)
        = (st.label.applySubst θ).applySubst σ :=
      swap_key hfix hσfix st.label (fun α hα =>
        havf α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_right _ hα⟩)))
    -- δ is a binder, so the witness reads θ at it
    have hres : st.res.applySubst (instSub σ₀.vars θ σ) = (st.res.applySubst θ).applySubst σ := by
      rw [Ty.applySubst_applySubst]
      exact Ty.applySubst_congr _ (fun δ hδ => by
        have hm := hwf st hst δ hδ
        exact ⟨by simp [instSub, hm, TySubst.comp], by simp [instSub, hm, TySubst.comp],
          by simp [instSub, hm, TySubst.comp]⟩)
    cases hdis st hst with
    | @hit τr hlk hδ =>
        refine .hit (τ := τr.applySubst σ) ?_ ?_
        · show LookupQ ((st.row.applySubst σ).applySubst _)
            ((st.label.applySubst σ).applySubst _) _
          rw [hrow, hlab]
          exact LookupQ.applySubst σ hlk (by intro hc; cases hc)
        · rw [hres, hδ]
    | abs hlk hδ =>
        refine .abs ?_ ?_
        · show LookupQ ((st.row.applySubst σ).applySubst _)
            ((st.label.applySubst σ).applySubst _) _
          rw [hrow, hlab]
          exact LookupQ.applySubst (r := .absent) σ hlk (by intro hc; cases hc)
        · rw [hres, hδ]; rfl
    | unk hlk hδ =>
        refine .unk ?_ ?_
        · show LookupQ ((st.row.applySubst σ).applySubst _)
            ((st.label.applySubst σ).applySubst _) _
          rw [hrow, hlab]
          exact hpark θ st hst hlk
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

private def refuteSol : Sol Unit := ⟨[("b", .base ())], [], []⟩
private def refuteSub : TySubst Unit := refuteSol.toSubst
private def refuteScheme : QScheme Unit := ⟨["a"], [], .var "a"⟩

private theorem refuteSol_applied : refuteSol.Applied := by
  refine ⟨fun p hp => ?_, fun _ hp => (nomatch hp), fun _ hp => (nomatch hp)⟩
  obtain rfl := List.mem_singleton.mp hp
  rfl

private theorem refuteSol_closes : refuteSol.Closes refuteSub :=
  Sol.closes_toSubst_of_applied refuteSol_applied

private theorem refuteScheme_wf : refuteScheme.WF := fun _ hst => nomatch hst

private theorem refuteScheme_avoiding : refuteScheme.Avoiding refuteSub := by
  refine ⟨fun α hα => ?_, fun α hα hn => ?_⟩
  · obtain rfl := List.mem_singleton.mp hα
    exact ⟨rfl, rfl, rfl⟩
  · exact absurd hα hn

-- the image scheme is σ₀ itself: σ fixes `a`
private theorem refuteScheme_image :
    refuteScheme.applySubst refuteSub = refuteScheme := rfl

-- `b` IS an instance of the image scheme…
private def bWitness : TySubst Unit :=
  ⟨fun α => if α = "a" then .var "b" else .var α, fun α => .var α, fun x => .var x⟩

private theorem b_inst :
    QScheme.Inst (refuteScheme.applySubst refuteSub)
      (.var "b") := by
  refine ⟨bWitness, ⟨fun α h => ?_, fun _ _ => rfl, fun _ _ => rfl⟩, ?_, rfl⟩
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
  | lab _ => exact fun h => nomatch h
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
under `WF`, `Avoiding` and `ParkStable`. So `σ₀.applySubst σ` is not a `SchemeImage`
witness, and the hypothesis cannot be discharged by pushing σ through the
scheme. -/
theorem qcovers_backward_false_for_applySubst :
    ¬ (∀ σ : TySubst Unit,
        ∀ σ₀ : QScheme Unit, σ₀.WF → σ₀.Avoiding σ → σ₀.ParkStable σ →
          ∀ τ', QScheme.Inst (σ₀.applySubst σ) τ' →
            ∃ τ, QScheme.Inst σ₀ τ ∧ τ' = τ.applySubst σ) := by
  intro h
  obtain ⟨τ, -, hτ⟩ :=
    h refuteSub refuteScheme
      refuteScheme_wf refuteScheme_avoiding
      (fun _ st hst => nomatch hst) (.var "b") b_inst
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

/-- every well-formed scheme has a well-formed σ-image that covers it. The
capture-avoiding construction; L1's counterpart is `renameScheme`. -/
def SchemeImage {B : Type} (σ : TySubst B) : Prop :=
  ∀ σ₀ : QScheme B, σ₀.WF → ∃ σ₀', σ₀'.WF ∧ QCovers σ σ₀ σ₀'

mutual

/-- ⊢  **L2 type substitution**: a `QTyped` derivation transports along ANY σ
for which every scheme has an image.
--
NOTE WHAT IS NO LONGER ASKED FOR. This used to carry `s.Closes σ` — "σ IS the
closure of the state" — because the selection cases moved their lookups with
`Sol.lookup_toCtx`, which needed it. They now move with `lookup_applySubst`
(definite results) and `lookup_total` (the `?` case, which re-splits three ways
exactly as L1's `typed_applySubst_aux` does), and neither knows about a
solution. So `Closes` has left this statement entirely; what survives of it is
the `ParkStable` premise inside `QCovers.forward_of_avoiding`, i.e. inside
`SchemeImage`, which is where the obligation always belonged. -/
theorem qtyped_applySubst {B C : Type} {constTy : C → B}
    {σ : TySubst B} (him : SchemeImage σ) :
    {Γ Γ' : QCtx B} → {e : Expr C} → {τ : Ty B} →
    QTyped constTy Γ e τ → QInstMap σ Γ Γ' →
    QTyped constTy Γ' e (τ.applySubst σ)
  | _, _, _, _, .qCon, _ => .qCon
  | _, _, _, _, .qVar hl hi, hm => by
      obtain ⟨σ₀', hl', hc⟩ := hm.schem _ _ hl
      exact .qVar hl' (hc.1 _ hi)
  | _, _, _, _, .qEq h heq, hm =>
      .qEq (qtyped_applySubst him h hm) (TyEquiv.applySubst σ heq)
  | _, _, _, _, .qLam h, hm =>
      .qLam (qtyped_applySubst him h (hm.bindTy _ _))
  | _, _, _, _, .qApp h₁ h₂, hm =>
      .qApp (qtyped_applySubst him h₁ hm) (qtyped_applySubst him h₂ hm)
  | _, _, _, _, .qLet (σ := σ₀) hwf hinst hinh hbody, hm => by
      obtain ⟨σ₀', hwf', hc⟩ := him σ₀ hwf
      refine .qLet (σ := σ₀') hwf' (fun τ₁' hi => ?_) ?_ ?_
      · obtain ⟨τ₁, hi₁, rfl⟩ := hc.2 _ hi
        exact qtyped_applySubst him (hinst τ₁ hi₁) hm
      · obtain ⟨τ₁, hi₁⟩ := hinh
        exact ⟨_, hc.1 _ hi₁⟩
      · exact qtyped_applySubst him hbody (hm.bindScheme _ hc)
  | _, _, _, _, .qCat h₁ h₂, hm =>
      .qCat (qtyped_applySubst him h₁ hm) (qtyped_applySubst him h₂ hm)
  | _, _, _, _, .qSel h hlk, hm => by
      have ih := qtyped_applySubst him h hm
      simp only [Ty.applySubst] at ih
      exact .qSel ih (lookup_applySubst σ hlk (by intro hc; cases hc))
  -- the `?` case: substituting may RESOLVE the lookup, so re-split on the
  -- lookup of the substituted row. T-sel-★ / T-sel-⊥ / T-sel + T-★-intro cover
  -- the three verdicts and the conclusion stays ★ — exactly L1's tSelUnk case.
  | _, _, _, _, .qSelUnk (ρ := ρ) (l := l) h _, hm => by
      have ih := qtyped_applySubst him h hm
      simp only [Ty.applySubst] at ih ⊢
      obtain ⟨r, hr⟩ := lookup_total (ρ.applySubst σ) l
      cases r with
      | found _ => exact .qUnk (.qSel ih hr)
      | absent  => exact .qSelAbs ih hr
      | unknown => exact .qSelUnk ih hr
  | _, _, _, _, .qSelAbs h hlk, hm => by
      have ih := qtyped_applySubst him h hm
      simp only [Ty.applySubst] at ih ⊢
      exact .qSelAbs ih (lookup_applySubst (r := .absent) σ hlk (by intro hc; cases hc))
  | _, _, _, _, .qUnk h, hm => .qUnk (qtyped_applySubst him h hm)
  | _, _, _, _, .qRcd h, hm => .qRcd (qtypedBody_applySubst him h hm)
  | _, _, _, _, .qLab, _ => .qLab
  | _, _, _, _, .qSelDyn h₁ h₂ hlk, hm =>
      .qSelDyn (qtyped_applySubst him h₁ hm) (qtyped_applySubst him h₂ hm)
        (LookupQ.applySubst σ hlk (by intro h; cases h))
  | _, _, _, _, .qSelDynAbs h₁ h₂ hlk, hm =>
      .qSelDynAbs (qtyped_applySubst him h₁ hm) (qtyped_applySubst him h₂ hm)
        (LookupQ.applySubst (r := .absent) σ hlk (by intro h; cases h))
  -- a `?` need not survive (σ may have chosen the key), but every lookup has
  -- SOME verdict, and each one types at ★
  | _, _, _, _, .qSelDynUnk h₁ h₂ _, hm => by
      have h₁' := qtyped_applySubst him h₁ hm
      have h₂' := qtyped_applySubst him h₂ hm
      obtain ⟨r, hr⟩ := LookupQ.total _ _
      cases r with
      | found τ => exact .qUnk (.qSelDyn h₁' h₂' hr)
      | absent => exact .qSelDynAbs h₁' h₂' hr
      | unknown => exact .qSelDynUnk h₁' h₂' hr

theorem qtypedBody_applySubst {B C : Type} {constTy : C → B}
    {σ : TySubst B} (him : SchemeImage σ) :
    {Γ Γ' : QCtx B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    QTypedBody constTy Γ ξ ρ → QInstMap σ Γ Γ' →
    QTypedBody constTy Γ' ξ (ρ.applySubst σ)
  | _, _, _, _, .empty, _ => .empty
  | _, _, _, _, .field h, hm => .field (qtyped_applySubst him h hm)
  | _, _, _, _, .cat h₁ h₂, hm =>
      .cat (qtypedBody_applySubst him h₁ hm) (qtypedBody_applySubst him h₂ hm)

end

end MinimalCalculus
