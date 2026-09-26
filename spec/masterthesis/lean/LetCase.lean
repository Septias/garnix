-- THE A-let CASE, AND WITH IT `InferSound`.
--
-- qLet asks for three things, and each has its own argument:
--
--   * the BODY — e₂'s induction hypothesis at the same σ, in Γ′ extended by the
--     let scheme READ under σ (binders renamed fresh, `SchemeRead`);
--   * the INSTANCES — for every instance χ of the read scheme, e₁ at the body
--     at χ, assuming the constraints at χ. This is generalization: e₁'s
--     induction hypothesis at σ₁ = (χ ∘ readSub) ∘ ⟦S₁⟧, which is the instance on
--     ᾱ and agrees with σ everywhere Γ and Δ_Γ can see — exactly, because σ
--     ABSORBS ⟦S₁⟧ and the binder names are fresh for σ's reach;
--   * INHABITATION — one instance whose constraints discharge exactly: every
--     generalized stump is blocked on a generalized row variable, so with the
--     binders left in place each lookup is `?`, and sending every result
--     variable to ★ is D-? for all of them at once.

import InferSoundA

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- BLOCKED LOOKUPS SURVIVE A SUBSTITUTION --------------------
-- If no row variable of ρ is chased, a definite absence does not depend on
-- anything a substitution can change, and a lookup blocked on β stays `?` as
-- long as β is sent to a variable.

/-- no row variable of ρ has a solution in Γ. -/
def NoChase (Γ : Ctx B) (ρ : Row B) : Prop :=
  ∀ α, (true, α) ∈ Row.sortedFtv ρ → Γ.lookupRow α = none

private theorem noChase_cat {Γ : Ctx B} {ρ₁ ρ₂ : Row B} (h : NoChase Γ (.cat ρ₁ ρ₂)) :
    NoChase Γ ρ₁ ∧ NoChase Γ ρ₂ :=
  ⟨fun α hα => h α (List.mem_append_left _ hα),
   fun α hα => h α (List.mem_append_right _ hα)⟩

theorem lookup_absent_subst {Γ : Ctx B} {ρ : Row B} {l : Label} {r : LookupRes B}
    (h : Lookup Γ ρ l r) (hr : r = .absent) (hn : NoChase Γ ρ) (θ : TySubst B) :
    Lookup (⟨[], []⟩ : Ctx B) (ρ.applySubst θ) l .absent := by
  induction h with
  | emp => exact .emp
  | hit => exact nomatch hr
  | miss hne => exact .miss hne
  | var hα _ _ =>
      have := hn _ (List.mem_singleton_self _)
      rw [this] at hα; exact nomatch hα
  | varFree _ => exact nomatch hr
  | catHit _ _ => exact nomatch hr
  | catSkip _ _ ih₁ ih₂ =>
      exact .catSkip (ih₁ rfl (noChase_cat hn).1) (ih₂ hr (noChase_cat hn).2)
  | catUnk _ _ => exact nomatch hr

theorem lookup_blocked_subst {Γ : Ctx B} {ρ : Row B} {l : Label} {β : TyVar}
    (h : LookupBlocked Γ ρ l β) (hn : NoChase Γ ρ) (θ : TySubst B) {β' : TyVar}
    (hβ : θ.row β = .var β') :
    Lookup (⟨[], []⟩ : Ctx B) (ρ.applySubst θ) l .unknown := by
  induction h with
  | varFree _ =>
      show Lookup _ (θ.row _) _ _
      rw [hβ]; exact .varFree rfl
  | var hα _ _ =>
      have := hn _ (List.mem_singleton_self _)
      rw [this] at hα; exact nomatch hα
  | catSkip ha _ ih =>
      exact .catSkip (lookup_absent_subst ha rfl (noChase_cat hn).1 θ)
        (ih (noChase_cat hn).2 hβ)
  | catUnk _ ih => exact .catUnk (ih (noChase_cat hn).1 hβ)

-- ⊢  a clean state chases nothing in a row it has already substituted
theorem noChase_of_clean {S : SolverState B} (hc : S.sol.Clean) (ρ : Row B) :
    NoChase S.ctx (ρ.applySubst S.subst) := by
  intro α hα
  cases hl : S.ctx.lookupRow α with
  | none => rfl
  | some ρ₀ =>
      obtain ⟨-, hmem⟩ := Sol.lookupRow_some hl
      exact absurd (List.mem_append_right _ (List.mem_map_of_mem (f := fun p => (true, p.1)) hmem))
        (hc.clears_row hα)

--------------------- THE CASE -------------------------------------------------

/-- ⊢  **the A-let case.** -/
theorem letCase {C : Type} {constTy : C → B} : LetCase B C constTy := by
  intro Γ S S₁ S₂ x e₁ e₂ τ₁ τ₂ Δq Δγ ᾱ κs h₁ _ hsplit hbq _ hfresh _ hres hdis hdom hind h₂
    h hΓ hc hq IH₁ IH₂ σ hab hσ Γ' hr
  -- the state e₁ ends in
  have c₁ := Infer.clean h₁ hc
  have q₁ := Infer.quiescent h₁ hq
  obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
  obtain ⟨i₁', hΓ'⟩ := let_body_inv (x := x) (τ₁ := τ₁) i₁ hΓ hsplit hres
  obtain ⟨-, k₂⟩ := Infer.pinv_keeps h₂ i₁' hΓ'
  have x₁₂ : S₁.Ext S₂ := (SolverState.Ext.of_sol_eq (S := S₁)
    (S' := { S₁ with parked := Δγ }) rfl).trans (Infer.ext h₂)
  have hab₁ : Absorbs σ S₁ := hab.back x₁₂ c₁
  have hΔq : ∀ p ∈ Δq, p ∈ S₁.parked := fun p hp => by
    exact hsplit.mem_iff.mpr <| List.mem_append_left _ hp
  have hΔγ : ∀ p ∈ Δγ, p ∈ S₁.parked := fun p hp => by
    exact hsplit.mem_iff.mpr <| List.mem_append_right _ hp
  -- fresh binder names, avoiding everything σ can reach from what stays outside
  let sc := letScheme S₁ ᾱ Δq τ₁
  let LΓ : List TyVar := Γ.ftv.flatMap (fun β => (S₁.subst.ty β).ftv ++ (S₁.subst.row β).ftv)
  let LΔ : List TyVar := Δγ.flatMap (fun p =>
    (p.stump.row.applySubst S₁.subst).ftv ++ (S₁.subst.ty p.stump.res).ftv)
  obtain ⟨f, hinj, -, havL⟩ := fresh_renaming_exists σ ᾱ (sc.freeFtv ++ LΓ ++ LΔ)
  let sc' := sc.readAt σ f
  have hread : SchemeRead σ sc sc' :=
    ⟨f, hinj, fun α hα _ => havL α (List.mem_append_left _ (List.mem_append_left _ hα)), rfl⟩
  refine .qLet (σ := sc') ?_ ?_ ?_ (IH₂ σ hab hσ _ (hr.bindScheme x hread))
  · ------------------------------------------------ the read scheme is correctable
    have hfree : ∀ p ∈ Δq, ∀ α ∈ (p.stump.row.applySubst S₁.subst).ftv,
        α ∈ sc.freeFtv ++ LΓ ++ LΔ := fun p hp α hα =>
      List.mem_append_left _ (List.mem_append_left _ (List.mem_append_left _
        (List.mem_flatMap.mpr ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, hα⟩)))
    refine ⟨?_, ?_, ?_⟩
    · intro st hst
      obtain ⟨st₀, hst₀, rfl⟩ := List.mem_map.mp hst
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst₀
      show (if S₁.resVar p.stump.res ∈ ᾱ then f (S₁.resVar p.stump.res)
        else S₁.resVar p.stump.res) ∈ ᾱ.map f
      rw [if_pos (hres.1 p hp).2]; exact List.mem_map_of_mem (hres.1 p hp).2
    · intro a ha b hb he
      obtain ⟨a₀, ha₀, rfl⟩ := List.mem_map.mp ha
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha₀
      obtain ⟨b₀, hb₀, rfl⟩ := List.mem_map.mp hb
      obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hb₀
      have he' : f (S₁.resVar p.stump.res) = f (S₁.resVar q.stump.res) := by
        have := he
        simp only [if_pos (show S₁.resVar p.stump.res ∈ sc.vars from (hres.1 p hp).2),
          if_pos (show S₁.resVar q.stump.res ∈ sc.vars from (hres.1 q hq).2)] at this
        exact this
      have hpq := hres.2 p hp q hq (hinj _ (hres.1 p hp).2 _ (hres.1 q hq).2 he')
      rw [hpq]
    · intro a ha b hb hm
      obtain ⟨a₀, ha₀, rfl⟩ := List.mem_map.mp ha
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha₀
      obtain ⟨b₀, hb₀, rfl⟩ := List.mem_map.mp hb
      obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hb₀
      simp only [if_pos (show S₁.resVar q.stump.res ∈ sc.vars from (hres.1 q hq).2)] at hm
      obtain ⟨α, hα, hγ⟩ := Row.ftv_applySubst _ _ _ hm
      by_cases hαv : α ∈ ᾱ
      · have hfα : f (S₁.resVar q.stump.res) = f α := by
          rcases hγ with hγ | hγ <;>
          · simp only [readSub, if_pos (show α ∈ sc.vars from hαv), Ty.ftv, Row.ftv, List.mem_singleton] at hγ
            exact hγ
        have := hinj _ (hres.1 q hq).2 _ hαv hfα
        exact hind p hp q hq (this ▸ hα)
      · have hnot := havL α (hfree p hp α hα)
        have hin : f (S₁.resVar q.stump.res) ∈ ᾱ.map f := List.mem_map_of_mem (hres.1 q hq).2
        rcases hγ with hγ | hγ
        · simp only [readSub, if_neg (show α ∉ sc.vars from hαv)] at hγ; exact hnot.1 _ hγ hin
        · simp only [readSub, if_neg (show α ∉ sc.vars from hαv)] at hγ; exact hnot.2 _ hγ hin
  · ------------------------------------------------ the instances
    intro χ hχ
    let ρ : TySubst B := χ.comp (readSub σ ᾱ f)
    let σ₁ : TySubst B := ρ.comp S₁.subst
    -- ρ is σ wherever the outside can see
    have hρ : ∀ β ∈ sc.freeFtv ++ LΓ ++ LΔ, β ∉ ᾱ →
        ρ.ty β = σ.ty β ∧ ρ.row β = σ.row β := by
      intro β hβ hn
      obtain ⟨hvt, hvr⟩ := havL β hβ
      refine ⟨?_, ?_⟩
      · show ((readSub σ ᾱ f).ty β).applySubst χ = _
        simp only [readSub, if_neg hn]
        exact Ty.applySubst_fixed_ftv _ (fun γ hγ =>
          ⟨hχ.1 γ (hvt γ hγ), hχ.2 γ (hvt γ hγ)⟩)
      · show ((readSub σ ᾱ f).row β).applySubst χ = _
        simp only [readSub, if_neg hn]
        exact Row.applySubst_fixed_ftv _ (fun γ hγ =>
          ⟨hχ.1 γ (hvr γ hγ), hχ.2 γ (hvr γ hγ)⟩)
    -- so σ₁ is σ on Γ's variables …
    have hΓag : ∀ β ∈ Γ.ftv, σ₁.ty β = σ.ty β ∧ σ₁.row β = σ.row β := by
      intro β hβ
      refine ⟨?_, ?_⟩
      · show (S₁.subst.ty β).applySubst ρ = _
        rw [← (hab₁ β).1]
        exact Ty.applySubst_congr _ (fun γ hγ => hρ γ
          (List.mem_append_left _ (List.mem_append_right _
            (List.mem_flatMap.mpr ⟨β, hβ, List.mem_append_left _ hγ⟩)))
          (fun hm => (hfresh γ hm β hβ).1 hγ))
      · show (S₁.subst.row β).applySubst ρ = _
        rw [← (hab₁ β).2]
        exact Row.applySubst_congr _ (fun γ hγ => hρ γ
          (List.mem_append_left _ (List.mem_append_right _
            (List.mem_flatMap.mpr ⟨β, hβ, List.mem_append_right _ hγ⟩)))
          (fun hm => (hfresh γ hm β hβ).2 hγ))
    -- … and σ₁ absorbs S₁
    have hab₁' : Absorbs σ₁ S₁ := by
      intro α
      obtain ⟨hi₁, hi₂⟩ := SolverState.idem c₁ α
      refine ⟨?_, ?_⟩
      · show (S₁.subst.ty α).applySubst (ρ.comp S₁.subst) = (S₁.subst.ty α).applySubst ρ
        rw [← Ty.applySubst_applySubst, hi₁]
      · show (S₁.subst.row α).applySubst (ρ.comp S₁.subst) = (S₁.subst.row α).applySubst ρ
        rw [← Row.applySubst_applySubst, hi₂]
    have hr₁ : CtxRead σ₁ Γ Γ' := ⟨hr.row, fun y sc₀ hy => by
      obtain ⟨sc₀', hl', hs⟩ := hr.schem y sc₀ hy
      refine ⟨sc₀', hl', hs.congr (fun α hα _ => hΓag α ?_)⟩
      refine QCtx.lookup_ftv_subset hy α ?_
      unfold QScheme.freeFtv at hα; unfold QScheme.ftv
      rcases List.mem_append.mp hα with hα | hα
      · obtain ⟨st, hst, hα⟩ := List.mem_flatMap.mp hα
        exact List.mem_append_left _ (List.mem_append_right _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_cons_of_mem _ hα⟩))
      · exact List.mem_append_right _ hα⟩
    have ih := IH₁ σ₁ hab₁' (hab₁'.sat c₁) Γ' hr₁
    -- the type e₁ gets is the read body at χ
    have hty : τ₁.applySubst σ₁ = sc'.body.applySubst χ := by
      show τ₁.applySubst (ρ.comp S₁.subst)
        = ((τ₁.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
      rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]
    rw [hty] at ih
    refine ih.weaken (fun a ha => ?_)
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    replace hp := hsplit.mem_iff.mp hp
    rcases List.mem_append.mp hp with hp | hp
    · -- a generalized stump: it IS the read constraint at χ
      refine .inl (List.mem_append_left _ (List.mem_map.mpr ⟨_, List.mem_map.mpr
        ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, rfl⟩, ?_⟩))
      have hr' := (hres.1 p hp).2
      simp only [Stump.at, if_pos hr']
      congr 1
      · show ((p.stump.row.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
          = p.stump.row.applySubst (ρ.comp S₁.subst)
        rw [Row.applySubst_applySubst, Row.applySubst_applySubst]
      · rw [if_pos (show S₁.resVar p.stump.res ∈ sc.vars from hr')]
        show χ.ty (f (S₁.resVar p.stump.res)) = (S₁.subst.ty p.stump.res).applySubst ρ
        rw [(hres.1 p hp).1]
        show χ.ty (f (S₁.resVar p.stump.res))
          = ((readSub σ ᾱ f).ty (S₁.resVar p.stump.res)).applySubst χ
        simp only [readSub, if_pos hr', Ty.applySubst]
    · -- a stump that stays: its σ₁-reading is its σ-reading, and it is kept
      have hsame : p.stump.at σ₁ = p.stump.at σ := by
        have hLΔ : ∀ γ, γ ∈ (p.stump.row.applySubst S₁.subst).ftv ++
            (S₁.subst.ty p.stump.res).ftv → γ ∈ sc.freeFtv ++ LΓ ++ LΔ :=
          fun γ hγ => List.mem_append_right _ (List.mem_flatMap.mpr ⟨p, hp, hγ⟩)
        simp only [Stump.at]
        congr 1
        · show p.stump.row.applySubst (ρ.comp S₁.subst) = _
          rw [← Row.applySubst_applySubst, ← hab₁.row]
          exact Row.applySubst_congr _ (fun γ hγ => hρ γ (hLΔ γ (List.mem_append_left _ hγ))
            (fun hm => (hdis γ hm p hp).1 hγ))
        · show (S₁.subst.ty p.stump.res).applySubst ρ = _
          rw [← (hab₁ p.stump.res).1]
          exact Ty.applySubst_congr _ (fun γ hγ => hρ γ (hLΔ γ (List.mem_append_right _ hγ))
            (fun hm => (hdis γ hm p hp).2 hγ))
      rw [hsame, hr.ctx_eq]
      rcases k₂ σ hσ p hp with ⟨q, hq', hqs⟩ | hd
      · exact .inl (List.mem_append_right _ (List.mem_map.mpr ⟨q, hq', by rw [hqs]⟩))
      · exact .inr (Stump.dischargeEquiv_iff_holds.mp hd)
  · ------------------------------------------------ inhabitation
    let resF : List TyVar := Δq.map (fun p => f (S₁.resVar p.stump.res))
    let χ₀ : TySubst B := ⟨fun β => if β ∈ resF then .unk else .var β, fun β => .var β⟩
    refine ⟨_, χ₀, ⟨fun β hβ => ?_, fun _ _ => rfl⟩, fun st hst => ?_, rfl⟩
    · have : β ∉ resF := fun hm => by
        obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hm
        exact hβ (List.mem_map_of_mem (hres.1 p hp).2)
      simp only [χ₀, if_neg this]
    · obtain ⟨st₀, hst₀, rfl⟩ := List.mem_map.mp hst
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst₀
      have hr' := (hres.1 p hp).2
      refine .unk ?_ ?_
      · rw [hr.ctx_eq]
        show Lookup _ (((p.stump.row.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀) _ _
        rw [Row.applySubst_applySubst]
        refine lookup_blocked_subst (q₁ p (hΔq p hp)) (noChase_of_clean c₁ _) _ (β' := f p.blocker) ?_
        show ((readSub σ ᾱ f).row p.blocker).applySubst χ₀ = _
        simp only [readSub, if_pos (hbq p hp), Row.applySubst]
        rfl
      · show χ₀.ty (if S₁.resVar p.stump.res ∈ ᾱ then f (S₁.resVar p.stump.res)
          else S₁.resVar p.stump.res) = .unk
        simp only [if_pos hr', χ₀]
        rw [if_pos (List.mem_map.mpr ⟨p, hp, rfl⟩)]

--------------------- …AND THE STATEMENT ---------------------------------------

/-- ⊢  **Inference soundness, assumption form — PROVED.** Every derivation from a
clean, quiescent state with the parked-list invariant, in a context of
well-formed schemes, is a typing under the assumptions it leaves parked, at every
σ that absorbs and satisfies its final state, in Γ read under σ. -/
theorem inferSound {C : Type} {constTy : C → B} : InferSound B C constTy :=
  inferSound_of_let letCase

/-- ⊢  a run from nothing, at any σ that absorbs and satisfies the state
inference ends in, is a plain L2 typing — given that the stumps still parked
hold at σ, which is finalization's job (`Finalizes.holds`). -/
theorem runSoundA {C : Type} {constTy : C → B}
    {e : Expr C} {τ : Ty B} {S₁ : SolverState B}
    (h : Infer constTy ⟨[], []⟩ ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁)
    {σ : TySubst B} (hab : Absorbs σ S₁) (hsat : Sol.Sat σ S₁.sol)
    (hfin : ∀ p ∈ S₁.parked, (p.stump.at σ).Holds (⟨[], []⟩ : Ctx B)) :
    QTyped constTy ⟨[], []⟩ e (τ.applySubst σ) :=
  runSoundA_of inferSound h hab hsat hfin

end MinimalCalculus
