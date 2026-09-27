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
-- A definite absence survives any substitution (`lookup_applySubst`), and a
-- lookup blocked on β stays `?` as long as β is sent to a variable. The blocker
-- of a row-blocked lookup is a row variable ON THE SPINE of the row looked up,
-- which is what callers use to show it is sent to a variable.
--
-- (This section used to carry `NoChase Γ ρ` — "no row variable of ρ has a
-- solution in Γ" — on every lemma, to rule out the L-α chase. There is no chase
-- any more, so there is nothing to rule out.)

-- ⊢  the blocker of a row lookup is a row variable of the row's spine
theorem LookupBlocked.mem_sortedFtv {ρ : Row B} {l : Label} {β : TyVar}
    (h : LookupBlocked ρ l β) : (true, β) ∈ ρ.sortedFtv := by
  induction h with
  | varFree => exact List.mem_singleton_self _
  | catSkip _ _ ih => exact List.mem_append_right _ ih
  | catUnk _ ih => exact List.mem_append_left _ ih

theorem lookup_blocked_subst {ρ : Row B} {l : Label} {β : TyVar}
    (h : LookupBlocked ρ l β) (θ : TySubst B) {β' : TyVar}
    (hβ : θ.row β = .var β') :
    Lookup (ρ.applySubst θ) l .unknown := by
  induction h with
  | varFree =>
      show Lookup (θ.row _) _ _
      rw [hβ]; exact .varFree
  | catSkip ha _ ih =>
      exact .catSkip (lookup_applySubst (r := .absent) θ ha (by intro h; cases h)) (ih hβ)
  | catUnk _ ih => exact .catUnk (ih hβ)

-- …and for a KEYED lookup. A variable key is definitely ⊥ only on a hollow row,
-- and it is blocked either on a row variable — which, sent to a variable, blocks
-- every key but a non-label one (L-junk: ⊥) — or on the key itself, which must
-- then stay a variable.

theorem lookupV_absent_subst {ρ : Row B} {α : TyVar}
    (h : LookupV ρ α .absent) (θ : TySubst B) : (ρ.applySubst θ).Hollow :=
  (h.hollow rfl).applySubst θ

private theorem lookupQ_catSkip_hollow {ρ₁ ρ₂ : Row B} {q : Ty B}
    {r : LookupRes B} (h₁ : ρ₁.Hollow) (h₂ : LookupQ ρ₂ q r) :
    LookupQ (.cat ρ₁ ρ₂) q r := by
  cases h₂ with
  | lit h => exact .lit (.catSkip (h₁.lookup _) h)
  | var h => exact .var (.catSkip (h₁.lookupV _) h)
  | junk h => exact .junk h

private theorem lookupQ_catUnk {ρ₁ ρ₂ : Row B} {q : Ty B}
    (h₁ : LookupQ ρ₁ q .unknown) : LookupQ (.cat ρ₁ ρ₂) q .unknown := by
  cases h₁ with
  | lit h => exact .lit (.catUnk h)
  | var h => exact .var (.catUnk h)

/-- blocked, read under a substitution: `?`, or `⊥` because the key stopped being
a label. Either way the stump discharges at ★. -/
def BlockedAfter (ρ : Row B) (q : Ty B) : Prop :=
  LookupQ ρ q .unknown ∨ (LookupQ ρ q .absent ∧ ¬ q.IsQuery)

theorem lookupV_blocked_subst {ρ : Row B} {α β : TyVar}
    (h : LookupVBlocked ρ α β) (θ : TySubst B)
    (hβ : (true, β) ∈ ρ.sortedFtv → ∃ β', θ.row β = .var β')
    (hk : β = α → ∀ l, θ.ty α ≠ .lab l) :
    BlockedAfter (ρ.applySubst θ) (θ.ty α) := by
  induction h with
  | @sing α l τ =>
      cases hq : θ.ty α with
      | lab l' => exact absurd hq (hk rfl l')
      | var γ => exact .inl (.var .sing)
      | _ => exact .inr ⟨.junk (by simp [Ty.IsQuery]), by simp [Ty.IsQuery]⟩
  | @varFree α β₀ =>
      obtain ⟨β', hβ'⟩ := hβ (List.mem_singleton_self _)
      show BlockedAfter (θ.row _) _
      rw [hβ']
      cases hq : θ.ty α with
      | lab l => exact .inl (.lit (.varFree))
      | var γ => exact .inl (.var (.varFree))
      | _ => exact .inr ⟨.junk (by simp [Ty.IsQuery]), by simp [Ty.IsQuery]⟩
  | catSkip ha _ ih =>
      have hh := lookupV_absent_subst ha θ
      rcases ih (fun hm => hβ (List.mem_append_right _ hm)) hk with h₂ | ⟨h₂, hq⟩
      · exact .inl (lookupQ_catSkip_hollow hh h₂)
      · exact .inr ⟨.junk hq, hq⟩
  | catUnk _ ih =>
      rcases ih (fun hm => hβ (List.mem_append_left _ hm)) hk with h₁ | ⟨_, hq⟩
      · exact .inl (lookupQ_catUnk h₁)
      · exact .inr ⟨.junk hq, hq⟩

theorem lookupQ_blocked_subst {ρ : Row B} {q : Ty B} {β : TyVar}
    (h : LookupBlockedQ ρ q β) (θ : TySubst B)
    (hβ : (true, β) ∈ ρ.sortedFtv → ∃ β', θ.row β = .var β')
    (hk : q = .var β → ∀ l, θ.ty β ≠ .lab l) :
    BlockedAfter (ρ.applySubst θ) (q.applySubst θ) := by
  cases h with
  | lit h =>
      obtain ⟨β', hβ'⟩ := hβ h.mem_sortedFtv
      exact .inl (.lit (lookup_blocked_subst h θ hβ'))
  | var h =>
      exact lookupV_blocked_subst h θ hβ (fun he => by subst he; exact hk rfl)

-- ⊢  a clean state leaves every spine variable of a row it has substituted alone
theorem SolverState.row_var_of_clean {S : SolverState B} (hc : S.sol.Clean) {ρ : Row B}
    {α : TyVar} (hα : (true, α) ∈ (ρ.applySubst S.subst).sortedFtv) :
    S.subst.row α = .var α := by
  apply rowLookup_not_mem
  intro hm
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hm
  exact hc.clears_row hα
    (List.mem_append_right _ (List.mem_map_of_mem (f := fun p => (true, p.1)) hp))

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
    (p.stump.row.applySubst S₁.subst).ftv ++ (S₁.subst.ty p.stump.res).ftv ++
      (p.stump.label.applySubst S₁.subst).ftv)
  obtain ⟨f, hinj, -, havL⟩ := fresh_renaming_exists σ ᾱ (sc.freeFtv ++ LΓ ++ LΔ)
  let sc' := sc.readAt σ f
  have hread : SchemeRead σ sc sc' :=
    ⟨f, hinj, fun α hα _ => havL α (List.mem_append_left _ (List.mem_append_left _ hα)), rfl⟩
  refine .qLet (σ := sc') ?_ ?_ ?_ (IH₂ σ hab hσ _ (hr.bindScheme x hread))
  · ------------------------------------------------ the read scheme is correctable
    have hfree : ∀ p ∈ Δq, ∀ α ∈ (p.stump.row.applySubst S₁.subst).ftv,
        α ∈ sc.freeFtv ++ LΓ ++ LΔ := fun p hp α hα =>
      List.mem_append_left _ (List.mem_append_left _ (List.mem_append_left _
        (List.mem_flatMap.mpr ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, List.mem_append_left _ hα⟩)))
    have hfreeK : ∀ p ∈ Δq, ∀ α ∈ (p.stump.label.applySubst S₁.subst).ftv,
        α ∈ sc.freeFtv ++ LΓ ++ LΔ := fun p hp α hα =>
      List.mem_append_left _ (List.mem_append_left _ (List.mem_append_left _
        (List.mem_flatMap.mpr ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, List.mem_append_right _ hα⟩)))
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
    · intro a ha b hb
      obtain ⟨a₀, ha₀, rfl⟩ := List.mem_map.mp ha
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha₀
      obtain ⟨b₀, hb₀, rfl⟩ := List.mem_map.mp hb
      obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hb₀
      simp only [if_pos (show S₁.resVar q.stump.res ∈ sc.vars from (hres.1 q hq).2)]
      -- the same argument at the row and at the key
      have hin : f (S₁.resVar q.stump.res) ∈ ᾱ.map f := List.mem_map_of_mem (hres.1 q hq).2
      refine ⟨fun hm => ?_, fun hm => ?_⟩
      · obtain ⟨α, hα, hγ⟩ := Row.ftv_applySubst _ _ _ hm
        by_cases hαv : α ∈ ᾱ
        · have hfα : f (S₁.resVar q.stump.res) = f α := by
            rcases hγ with hγ | hγ <;>
            · simp only [readSub, if_pos (show α ∈ sc.vars from hαv), Ty.ftv, Row.ftv, List.mem_singleton] at hγ
              exact hγ
          have := hinj _ (hres.1 q hq).2 _ hαv hfα
          exact (hind p hp q hq).1 (this ▸ hα)
        · have hnot := havL α (hfree p hp α hα)
          rcases hγ with hγ | hγ
          · simp only [readSub, if_neg (show α ∉ sc.vars from hαv)] at hγ; exact hnot.1 _ hγ hin
          · simp only [readSub, if_neg (show α ∉ sc.vars from hαv)] at hγ; exact hnot.2 _ hγ hin
      · obtain ⟨α, hα, hγ⟩ := Ty.ftv_applySubst _ _ _ hm
        by_cases hαv : α ∈ ᾱ
        · have hfα : f (S₁.resVar q.stump.res) = f α := by
            rcases hγ with hγ | hγ <;>
            · simp only [readSub, if_pos (show α ∈ sc.vars from hαv), Ty.ftv, Row.ftv, List.mem_singleton] at hγ
              exact hγ
          have := hinj _ (hres.1 q hq).2 _ hαv hfα
          exact (hind p hp q hq).2 (this ▸ hα)
        · have hnot := havL α (hfreeK p hp α hα)
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
    have hr₁ : CtxRead σ₁ Γ Γ' := ⟨fun y sc₀ hy => by
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
      · show ((p.stump.label.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
          = p.stump.label.applySubst (ρ.comp S₁.subst)
        rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]
      · rw [if_pos (show S₁.resVar p.stump.res ∈ sc.vars from hr')]
        show χ.ty (f (S₁.resVar p.stump.res)) = (S₁.subst.ty p.stump.res).applySubst ρ
        rw [(hres.1 p hp).1]
        show χ.ty (f (S₁.resVar p.stump.res))
          = ((readSub σ ᾱ f).ty (S₁.resVar p.stump.res)).applySubst χ
        simp only [readSub, if_pos hr', Ty.applySubst]
    · -- a stump that stays: its σ₁-reading is its σ-reading, and it is kept
      have hsame : p.stump.at σ₁ = p.stump.at σ := by
        have hLΔ : ∀ γ, γ ∈ (p.stump.row.applySubst S₁.subst).ftv ++
            (S₁.subst.ty p.stump.res).ftv ++ (p.stump.label.applySubst S₁.subst).ftv →
            γ ∈ sc.freeFtv ++ LΓ ++ LΔ :=
          fun γ hγ => List.mem_append_right _ (List.mem_flatMap.mpr ⟨p, hp, hγ⟩)
        simp only [Stump.at]
        congr 1
        · show p.stump.row.applySubst (ρ.comp S₁.subst) = _
          rw [← Row.applySubst_applySubst, ← hab₁.row]
          exact Row.applySubst_congr _ (fun γ hγ =>
            hρ γ (hLΔ γ (List.mem_append_left _ (List.mem_append_left _ hγ)))
            (fun hm => (hdis γ hm p hp).1 hγ))
        · show p.stump.label.applySubst (ρ.comp S₁.subst) = _
          rw [← Ty.applySubst_applySubst, ← hab₁.ty]
          exact Ty.applySubst_congr _ (fun γ hγ =>
            hρ γ (hLΔ γ (List.mem_append_right _ hγ))
            (fun hm => (hdis γ hm p hp).2.2 hγ))
        · show (S₁.subst.ty p.stump.res).applySubst ρ = _
          rw [← (hab₁ p.stump.res).1]
          exact Ty.applySubst_congr _ (fun γ hγ =>
            hρ γ (hLΔ γ (List.mem_append_left _ (List.mem_append_right _ hγ)))
            (fun hm => (hdis γ hm p hp).2.1 hγ))
      rw [hsame]
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
      have hδ : χ₀.ty (if S₁.resVar p.stump.res ∈ ᾱ then f (S₁.resVar p.stump.res)
          else S₁.resVar p.stump.res) = .unk := by
        simp only [if_pos hr', χ₀]
        rw [if_pos (List.mem_map.mpr ⟨p, hp, rfl⟩)]
      -- the blocker is generalized, so it is read as a fresh variable; a KEY
      -- blocker is moreover no generalized answer, so χ₀ leaves it a variable
      have hblk : BlockedAfter
          ((p.stump.row.applySubst S₁.subst).applySubst (χ₀.comp (readSub σ ᾱ f)))
          ((p.stump.label.applySubst S₁.subst).applySubst (χ₀.comp (readSub σ ᾱ f))) := by
        refine lookupQ_blocked_subst (q₁ p (hΔq p hp)) _ ?_ ?_
        · intro _
          refine ⟨f p.blocker, ?_⟩
          show ((readSub σ ᾱ f).row p.blocker).applySubst χ₀ = _
          simp only [readSub, if_pos (hbq p hp), Row.applySubst]
          rfl
        · intro hkey l
          have hv : (TySubst.comp χ₀ (readSub σ ᾱ f)).ty p.blocker = .var (f p.blocker) := by
            show ((readSub σ ᾱ f).ty p.blocker).applySubst χ₀ = _
            simp only [readSub, if_pos (hbq p hp), Ty.applySubst, χ₀]
            rw [if_neg]
            rintro hm
            obtain ⟨q, hq, hfq⟩ := List.mem_map.mp hm
            have hb := hinj _ (hres.1 q hq).2 _ (hbq p hp) hfq
            exact (hind p hp q hq).2 (by rw [hkey, hb]; simp [Ty.ftv])
          rw [hv]; exact nofun
      rcases hblk with hu | ⟨ha, -⟩
      · refine .unk ?_ hδ
        show LookupQ (((p.stump.row.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀)
          (((p.stump.label.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀) _
        rw [Row.applySubst_applySubst, Ty.applySubst_applySubst]
        exact hu
      · refine .abs ?_ hδ
        show LookupQ (((p.stump.row.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀)
          (((p.stump.label.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀) _
        rw [Row.applySubst_applySubst, Ty.applySubst_applySubst]
        exact ha

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
    (h : Infer constTy QCtx.empty ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁)
    {σ : TySubst B} (hab : Absorbs σ S₁) (hsat : Sol.Sat σ S₁.sol)
    (hfin : ∀ p ∈ S₁.parked, (p.stump.at σ).Holds) :
    QTyped constTy QCtx.empty e (τ.applySubst σ) :=
  runSoundA_of inferSound h hab hsat hfin

end MinimalCalculus
