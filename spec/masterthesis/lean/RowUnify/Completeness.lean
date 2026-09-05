-- P5: solution boundedness (unifyM_bounded) and success completeness.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Soundness

namespace MinimalCalculus


-- ## What a solution mentions  (`SolMentions`, `SolBelow`, Defs.lean)
-- The freshness bookkeeping solve-and-apply forced: a run only ever mentions
-- names below the supply it returns.
theorem SolBelow.mono {B : Type} {s : Sol B} {W W' : List TyVar}
    (h : SolBelow s W) (hW : W ⊆ W') : SolBelow s W' := fun γ hγ => hW (h γ hγ)

theorem SolBelow.nil {B : Type} (W : List TyVar) : SolBelow (Sol.nil (B := B)) W := by
  rintro γ (⟨p, hp, -⟩ | ⟨p, hp, -⟩) <;> cases hp

-- ⊢  a solution's substitution only ever produces names it mentions (or the
--    variable it was asked about, which it left alone)
theorem SolMentions.toSubst {B : Type} (s : Sol B) (α γ : TyVar)
    (h : γ ∈ (s.toSubst.ty α).ftv ∨ γ ∈ (s.toSubst.row α).ftv) :
    γ = α ∨ SolMentions s γ := by
  rcases h with h | h
  · rcases tyLookup_spec (B := B) α s.ty with he | hm
    · left; rw [show (s.toSubst.ty α) = tyLookup α s.ty from rfl, he] at h
      simpa [Ty.ftv] using h
    · exact .inr (.inl ⟨(α, tyLookup α s.ty), hm, .inr h⟩)
  · rcases rowLookup_spec (B := B) α s.row with he | hm
    · left; rw [show (s.toSubst.row α) = rowLookup α s.row from rfl, he] at h
      simpa [Row.ftv] using h
    · exact .inr (.inr ⟨(α, rowLookup α s.row), hm, .inr h⟩)

-- ⊢  applying a W-bounded solution to a W-bounded spine stays inside W
theorem sFtv_sApplySubst_sub {B : Type} {s : Sol B} {t : List (Atom B)} {W : List TyVar}
    (ht : sFtv t ⊆ W) (hs : SolBelow s W) : sFtv (sApplySubst s.toSubst t) ⊆ W := by
  intro γ hγ
  obtain ⟨α, hα, hg⟩ := sFtv_sApplySubst s.toSubst t γ hγ
  rcases SolMentions.toSubst s α γ hg with rfl | hm
  · exact ht hα
  · exact hs γ hm

-- ⊢  … and applying it to a W-bounded TYPE
theorem Ty_ftv_applySubst_sub {B : Type} {s : Sol B} {τ : Ty B} {W : List TyVar}
    (ht : τ.ftv ⊆ W) (hs : SolBelow s W) : (τ.applySubst s.toSubst).ftv ⊆ W := by
  intro γ hγ
  obtain ⟨α, hα, hg⟩ := Ty.ftv_applySubst s.toSubst τ γ hγ
  rcases SolMentions.toSubst s α γ hg with rfl | hm
  · exact ht hα
  · exact hs γ hm

theorem Row_ftv_applySubst_sub {B : Type} {s : Sol B} {ρ : Row B} {W : List TyVar}
    (ht : ρ.ftv ⊆ W) (hs : SolBelow s W) : (ρ.applySubst s.toSubst).ftv ⊆ W := by
  intro γ hγ
  obtain ⟨α, hα, hg⟩ := Row.ftv_applySubst s.toSubst ρ γ hγ
  rcases SolMentions.toSubst s α γ hg with rfl | hm
  · exact ht hα
  · exact hs γ hm

-- ⊢  composing two W-bounded solutions stays W-bounded
theorem SolBelow.comp {B : Type} {s₁ s₂ : Sol B} {W : List TyVar}
    (h₁ : SolBelow s₁ W) (h₂ : SolBelow s₂ W) : SolBelow (s₂.comp s₁) W := by
  rintro γ (⟨p, hp, hγ⟩ | ⟨p, hp, hγ⟩)
  · rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      rcases hγ with rfl | hγ
      · exact h₁ _ (.inl ⟨q, hq, .inl rfl⟩)
      · exact Ty_ftv_applySubst_sub
          (fun _ hx => h₁ _ (.inl ⟨q, hq, .inr hx⟩)) h₂ hγ
    · exact h₂ _ (.inl ⟨p, hp, hγ⟩)
  · rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      rcases hγ with rfl | hγ
      · exact h₁ _ (.inr ⟨q, hq, .inl rfl⟩)
      · exact Row_ftv_applySubst_sub
          (fun _ hx => h₁ _ (.inr ⟨q, hq, .inr hx⟩)) h₂ hγ
    · exact h₂ _ (.inr ⟨p, hp, hγ⟩)


------------------ P5: THE FRESHNESS INVARIANT, TRANSPORTED -----------------
-- The obligation the section above exists for: a successful run only ever
-- mentions names below the supply it RETURNS. `W` is the enlarged avoid-set —
-- the original `V` plus whatever the run invented — and the three conclusions
-- say it is an enlargement, that the returned supply is fresh for it, and that
-- the solution lives inside it. That is exactly what an eq-emitting arm needs to
-- hand the invariant on to the substituted residual.

-- ⊢  U-ε-var binds exactly the side's variables, each to ε
theorem allVarsEmpty_mem {B : Type} : (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → ∀ p ∈ σ, p.1 ∈ sFtv s ∧ p.2 = Row.empty
  | [], σ, h, p, hp => by simp only [allVarsEmpty, Option.some.injEq] at h; cases h; cases hp
  | .field _ _ :: _, σ, h, p, hp => by simp [allVarsEmpty] at h
  | .var α :: s, σ, h, p, hp => by
      simp only [allVarsEmpty, Option.map_eq_some_iff] at h
      obtain ⟨σ', hs, rfl⟩ := h
      rcases List.mem_cons.mp hp with rfl | hp
      · exact ⟨List.mem_cons_self, rfl⟩
      · obtain ⟨h₁, h₂⟩ := allVarsEmpty_mem s hs p hp
        exact ⟨List.mem_cons_of_mem _ h₁, h₂⟩

theorem SolBelow_ofRow {B : Type} {σ : List (TyVar × Row B)} {W : List TyVar}
    (h : ∀ p ∈ σ, p.1 ∈ W ∧ p.2.ftv ⊆ W) : SolBelow (Sol.ofRow σ) W := by
  rintro γ (⟨p, hp, -⟩ | ⟨p, hp, hγ⟩)
  · cases hp
  · rcases hγ with rfl | hγ
    · exact (h p hp).1
    · exact (h p hp).2 hγ


-- ⊢  U-var-solve stays inside the problem's variables
theorem solveVarM_bounded {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    {s : Sol B} {S' : Supply} {V : List TyVar}
    (h : solveVarM S s₁ s₂ = some (.success s S'))
    (hS : S.Avoids V) (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) :
    ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
  cases s₁ with
  | nil => simp [solveVarM] at h
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVarM] at h
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVarM] at h
      | nil =>
        simp only [solveVarM] at h
        split at h
        · simp at h
        · simp only [Option.some.injEq, UResM.success.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          refine ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp => ?_)⟩
          obtain rfl := List.mem_singleton.mp hp
          exact ⟨hV (List.mem_append_left _ List.mem_cons_self),
                 fun x hx => hV (List.mem_append_right _ (by rw [sFtv_ofSpine]; exact hx))⟩

-- ⊢  U-expand's own solution mentions only the problem plus the two names it
--    just invented, and both are inside the enlarged avoid-set
theorem expand_bounded {B : Type} [DecidableEq B] {fuel : Nat}
    (ih : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) (V : List TyVar) {s : Sol B} {S' : Supply},
      S.Avoids V → (sFtv u₁ ++ sFtv u₂) ⊆ V →
      unifySpineMF S fuel u₁ u₂ = .success s S' →
      ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W)
    {S : Supply} {u₁ u₂ : List (Atom B)} {V : List TyVar}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hS : S.Avoids V) (hV : (sFtv u₁ ++ sFtv u₂) ⊆ V)
    (he : expandL S u₁ u₂ = some (β, l, τ, t₁, t₂))
    (h : expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂) = .success s S') :
    ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
  obtain ⟨s', hrec, rfl⟩ := expandResM_success h
  obtain ⟨hs1, hvv, hcc, hren⟩ := expandL_spec he
  have hτV : τ.ftv ⊆ V := fun _ hx =>
    sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_left _ hx)
  have hβV : β ∈ V :=
    sFtv_sub_right hV (mem_sFtv_of_mem_sVarSeq u₂ (by rw [hvv]; exact List.mem_cons_self))
  have hS' : S.fresh.2.fresh.2.Avoids (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    hS.cons_fresh.cons_fresh
  have hV' : (sFtv t₁ ++ sFtv t₂) ⊆ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) := by
    intro x hx
    rcases List.mem_append.mp hx with hh | hh
    · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _
        (sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_right _ hh)))
    · rw [hren] at hh
      rcases List.mem_cons.mp (sFtv_renameVar _ _ u₂ x hh) with rfl | hh'
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (sFtv_sub_right hV hh'))
  obtain ⟨W, hVW, hSW, hbW⟩ := ih S.fresh.2.fresh.2 t₁ t₂ _ hS' hV' hrec
  have hVsub : V ⊆ W := fun _ hx => hVW (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ hx))
  have hdW : S.fresh.1 ∈ W := hVW (List.mem_cons_of_mem _ List.mem_cons_self)
  have hbW' : S.fresh.2.fresh.1 ∈ W := hVW List.mem_cons_self
  refine ⟨W, hVsub, hSW, SolBelow.comp ?_ hbW⟩
  rintro γ (⟨p, hp, hγ⟩ | ⟨p, hp, hγ⟩)
  · obtain rfl := List.mem_singleton.mp hp
    rcases hγ with rfl | hγ
    · exact hdW
    · exact hVsub (hτV hγ)
  · obtain rfl := List.mem_singleton.mp hp
    rcases hγ with rfl | hγ
    · exact hVsub hβV
    · simp only [Row.ftv, Ty.ftv, List.mem_append, List.mem_singleton] at hγ
      rcases hγ with rfl | rfl
      · exact hdW
      · exact hbW'

-- THE BOUNDEDNESS INVARIANT, both sorts at once.
theorem unifyM_bounded {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) (V : List TyVar) {s : Sol B} {S' : Supply},
        S.Avoids V → (τ.ftv ++ τ'.ftv) ⊆ V →
        unifyTyF S fuel τ τ' = .success s S' →
        ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) (V : List TyVar) {s : Sol B} {S' : Supply},
        S.Avoids V → (sFtv s₁ ++ sFtv s₂) ⊆ V →
        unifySpineMF S fuel s₁ s₂ = .success s S' →
        ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W) := by
  -- the shape shared by bindTy's two orientations
  have hbind : ∀ (S : Supply) (α : TyVar) (τ : Ty B) (V : List TyVar) {s : Sol B}
      {S' : Supply}, S.Avoids V → α ∈ V → τ.ftv ⊆ V → bindTy S α τ = .success s S' →
      ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
    intro S α τ V s S' hS hα hτ h
    unfold bindTy at h
    split at h
    · simp only [UResM.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
    · split at h
      · cases h
      · simp only [UResM.success.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        refine ⟨V, fun _ hx => hx, hS, ?_⟩
        rintro γ (⟨p, hp, hγ⟩ | ⟨p, hp, -⟩)
        · obtain rfl := List.mem_singleton.mp hp
          rcases hγ with rfl | hγ
          · exact hα
          · exact hτ hγ
        · cases hp
  -- the shape shared by the six eq-emitting arms
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' V s S' hS hV h => ?_, fun S s₁ s₂ V s S' hS hV h => ?_⟩
      · cases τ with
        | var α =>
            exact hbind S α τ' V hS (hV (List.mem_append_left _ List.mem_cons_self))
              (fun _ hx => hV (List.mem_append_right _ hx)) h
        | base b =>
            cases τ' with
            | var α =>
                exact hbind S α (.base b) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base b' =>
                by_cases hb : b = b'
                · subst hb
                  have hred : unifyTyF S 0 (Ty.base b) (Ty.base b)
                      = .success (Sol.nil (B := B)) S := by simp [unifyTyF]
                  rw [hred] at h
                  simp only [UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
                · simp [unifyTyF, hb] at h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α =>
                exact hbind S α .unk V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk =>
                simp only [unifyTyF, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.fn a₁ b₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.rcd ρ₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => simp [hae] at h
            | some σ' =>
                simp only [hae, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                  ⟨hV (List.mem_append_right _ (allVarsEmpty_mem s₂ hae p hp).1),
                   by rw [(allVarsEmpty_mem s₂ hae p hp).2]; exact fun _ hx => by cases hx⟩)⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => simp [hae] at h
              | some σ' =>
                  simp only [hae, UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                    ⟨hV (List.mem_append_left _ (allVarsEmpty_mem (a :: s₁) hae p hp).1),
                     by rw [(allVarsEmpty_mem (a :: s₁) hae p hp).2]
                        exact fun _ hx => by cases hx⟩)⟩
          | cons b s₂ => cases h
  | succ fuel ih =>
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          {s : Sol B} {S' : Supply}, S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' →
          ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
        intro S τ τ' t₁ t₂ V s S' hS hVt hVr h
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := ih.1 S τ τ' V hS hVt hty
        have hres : (sFtv (sApplySubst s₁.toSubst t₁) ++
                     sFtv (sApplySubst s₁.toSubst t₂)) ⊆ W₁ := fun x hx => by
          rcases List.mem_append.mp hx with hh | hh
          · exact sFtv_sApplySubst_sub
              (fun _ hy => hVW₁ (hVr (List.mem_append_left _ hy))) hb₁ hh
          · exact sFtv_sApplySubst_sub
              (fun _ hy => hVW₁ (hVr (List.mem_append_right _ hy))) hb₁ hh
        obtain ⟨W₂, hW₁W₂, hS₂, hb₂⟩ := ih.2 S₁ _ _ W₁ hS₁ hres hrow
        exact ⟨W₂, fun _ hx => hW₁W₂ (hVW₁ hx), hS₂, (hb₁.mono hW₁W₂).comp hb₂⟩
      refine ⟨fun S τ τ' V s S' hS hV h => ?_, fun S s₁ s₂ V s S' hS hV h => ?_⟩
      · cases τ with
        | var α =>
            exact hbind S α τ' V hS (hV (List.mem_append_left _ List.mem_cons_self))
              (fun _ hx => hV (List.mem_append_right _ hx)) h
        | base b =>
            cases τ' with
            | var α =>
                exact hbind S α (.base b) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base b' =>
                by_cases hb : b = b'
                · subst hb
                  have hred : unifyTyF S (fuel + 1) (Ty.base b) (Ty.base b)
                      = .success (Sol.nil (B := B)) S := by simp [unifyTyF]
                  rw [hred] at h
                  simp only [UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
                · simp [unifyTyF, hb] at h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α =>
                exact hbind S α .unk V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk =>
                simp only [unifyTyF, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.fn a₁ b₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn a₂ b₂ =>
                replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .success s S' := h
                obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
                obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := ih.1 S a₁ a₂ V hS
                  (fun x hx => by
                    rcases List.mem_append.mp hx with hh | hh
                    · exact hV (List.mem_append_left _ (List.mem_append_left _ hh))
                    · exact hV (List.mem_append_right _ (List.mem_append_left _ hh))) hty
                have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                             (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁
                      (hV (List.mem_append_left _ (List.mem_append_right _ hy)))) hb₁ hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁
                      (hV (List.mem_append_right _ (List.mem_append_right _ hy)))) hb₁ hh
                obtain ⟨W₂, hW₁W₂, hS₂, hb₂⟩ := ih.1 S₁ _ _ W₁ hS₁ hres hrow
                exact ⟨W₂, fun _ hx => hW₁W₂ (hVW₁ hx), hS₂, (hb₁.mono hW₁W₂).comp hb₂⟩
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.rcd ρ₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
                refine ih.2 S _ _ V hS (fun x hx => ?_) h
                rcases List.mem_append.mp hx with hh | hh
                · exact hV (List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh))
                · exact hV (List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh))
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => simp [hae] at h
            | some σ' =>
                simp only [hae, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                  ⟨hV (List.mem_append_right _ (allVarsEmpty_mem s₂ hae p hp).1),
                   by rw [(allVarsEmpty_mem s₂ hae p hp).2]; exact fun _ hx => by cases hx⟩)⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => simp [hae] at h
              | some σ' =>
                  simp only [hae, UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                    ⟨hV (List.mem_append_left _ (allVarsEmpty_mem (a :: s₁) hae p hp).1),
                     by rw [(allVarsEmpty_mem (a :: s₁) hae p hp).2]
                        exact fun _ hx => by cases hx⟩)⟩
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_bounded (hv1.trans (congrArg some h)) hS hV
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact solveVarM_bounded (hv2.trans (congrArg some h)) hS (sFtv_sub_swap hV)
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchL_ftv hml).1 hh)
                  · exact sFtv_sub_right hV ((matchL_ftv hml).2.2.1 hh))
                (sFtv_sub_residual hV (matchL_ftv hml).2.1 (matchL_ftv hml).2.2.2) h
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchL_ftv hml2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((matchL_ftv hml2).1 hh))
                (sFtv_sub_residual hV (matchL_ftv hml2).2.2.2 (matchL_ftv hml2).2.1) h
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchR_ftv hmr).1 hh)
                  · exact sFtv_sub_right hV ((matchR_ftv hmr).2.2.1 hh))
                (sFtv_sub_residual hV (matchR_ftv hmr).2.1 (matchR_ftv hmr).2.2.2) h
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchR_ftv hmr2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((matchR_ftv hmr2).1 hh))
                (sFtv_sub_residual hV (matchR_ftv hmr2).2.2.2 (matchR_ftv hmr2).2.1) h
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((groundMatch_ftv hg).1 hh)
                  · exact sFtv_sub_right hV ((groundMatch_ftv hg).2.2.1 hh))
                (sFtv_sub_residual hV (groundMatch_ftv hg).2.1 (groundMatch_ftv hg).2.2.2) h
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((groundMatch_ftv hg2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((groundMatch_ftv hg2).1 hh))
                (sFtv_sub_residual hV (groundMatch_ftv hg2).2.2.2 (groundMatch_ftv hg2).2.1) h
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact expand_bounded ih.2 hS hV he1 h
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              exact expand_bounded ih.2 hS (sFtv_sub_swap hV) he2 h
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h <;> cases h


------------------ P5: SUCCESS COMPLETENESS (mgu), MUTUALLY -----------------
-- Success completeness at both sorts. What a unifier must satisfy is the
-- SOLUTION and nothing else — the sharp form of "≐ᵣ computes an mgu".
-- Together with unifyM_success_sound the unifier set
-- of the problem is EXACTLY {θ : Sol.Sat θ s}, up to the fresh names U-expand
-- invents (the ∃θ'/AgreeOn form)).
--
-- The new work is in the eq-emitting arms: the recursive call is on the
-- SUBSTITUTED residual, so a unifier has to be pushed through the solution
-- (unifies_sApplySubst_of_sat) and the extension the sub-call returns has to be
-- carried back over the solution the arm already had (Sol.Sat.congrAgree, and
-- the boundedness invariant to know it may be).

-- ⊢  the empty solution is met by everything
theorem Sol.Sat_nil {B : Type} {θ : TySubst B} : Sol.Sat θ (Sol.nil (B := B)) :=
  ⟨fun _ hp => (nomatch hp), fun _ hp => (nomatch hp)⟩

-- ⊢  the base and ★ arms bind nothing, so any unifier meets them vacuously
theorem base_arm_complete {B : Type} [DecidableEq B] {θ : TySubst B} {b b' : B}
    {S : Supply} {fuel : Nat} {s : Sol B} {S' : Supply}
    (h : unifyTyF S fuel (.base b) (.base b') = .success s S') : Sol.Sat θ s := by
  by_cases hb : b = b'
  · subst hb
    have hred : unifyTyF S fuel (Ty.base b) (Ty.base b)
        = .success (Sol.nil (B := B)) S := by simp [unifyTyF]
    rw [hred] at h
    simp only [UResM.success.injEq] at h
    obtain ⟨rfl, -⟩ := h
    exact Sol.Sat_nil
  · simp [unifyTyF, hb] at h

theorem unk_arm_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    {S : Supply} {fuel : Nat} {s : Sol B} {S' : Supply}
    (h : unifyTyF S fuel (.unk : Ty B) .unk = .success s S') : Sol.Sat θ s := by
  simp only [unifyTyF, UResM.success.injEq] at h
  obtain ⟨rfl, -⟩ := h
  exact Sol.Sat_nil

theorem AgreeOn.rowEq {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {ρ : Row B} (hsub : ρ.ftv ⊆ V) :
    ρ.applySubst θ = ρ.applySubst θ' :=
  Row.applySubst_congr ρ (fun α hα => h α (hsub hα))

-- ⊢  a unifier transports along agreement, at both sorts
theorem AgreeOn.unifiesSpine {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {t₁ t₂ : List (Atom B)}
    (h₁ : sFtv t₁ ⊆ V) (h₂ : sFtv t₂ ⊆ V)
    (hu : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ') ((ofSpine t₂).applySubst θ') := by
  rw [← h.rowEq (by rw [← sFtv_ofSpine]; exact h₁),
      ← h.rowEq (by rw [← sFtv_ofSpine]; exact h₂)]
  exact hu

theorem AgreeOn.tyUnifies {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {τ τ' : Ty B}
    (h₁ : τ.ftv ⊆ V) (h₂ : τ'.ftv ⊆ V) (hu : TyUnifies θ τ τ') : TyUnifies θ' τ τ' := by
  unfold TyUnifies; rw [← h.tyEq h₁, ← h.tyEq h₂]; exact hu

-- ⊢  meeting a solution transports along agreement, provided the agreement
--    covers every name the solution mentions
theorem Sol.Sat.congrAgree {B : Type} {θ θ' : TySubst B} {s : Sol B} {W : List TyVar}
    (h : Sol.Sat θ s) (hag : AgreeOn θ θ' W) (hb : SolBelow s W) : Sol.Sat θ' s := by
  constructor
  · intro p hp
    rw [← (hag p.1 (hb p.1 (.inl ⟨p, hp, .inl rfl⟩))).1,
        ← hag.tyEq (fun x hx => hb x (.inl ⟨p, hp, .inr hx⟩))]
    exact h.1 p hp
  · intro p hp
    rw [← (hag p.1 (hb p.1 (.inr ⟨p, hp, .inl rfl⟩))).2,
        ← hag.rowEq (fun x hx => hb x (.inr ⟨p, hp, .inr hx⟩))]
    exact h.2 p hp

-- ⊢  meeting both halves of a composite means meeting the composite
--    (the converse of Sol.Sat.comp_inv, and what the arms actually build)
theorem Sol.Sat.comp {B : Type} {θ : TySubst B} {s₁ s₂ : Sol B}
    (h₁ : Sol.Sat θ s₁) (h₂ : Sol.Sat θ s₂) : Sol.Sat θ (s₂.comp s₁) := by
  constructor
  · intro p hp
    rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      show TyEquiv (θ.ty q.1) ((q.2.applySubst s₂.toSubst).applySubst θ)
      rw [Ty.applySubst_applySubst]
      exact (h₁.1 q hq).trans (Ty.applySubst_substEquiv h₂.substEquiv q.2)
    · exact h₂.1 p hp
  · intro p hp
    rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      show RowEquiv (θ.row q.1) ((q.2.applySubst s₂.toSubst).applySubst θ)
      rw [Row.applySubst_applySubst]
      exact (h₁.2 q hq).trans (Row.applySubst_substEquiv h₂.substEquiv q.2)
    · exact h₂.2 p hp

-- ⊢  a unifier of α and τ meets the binding α ≔ τ
theorem bindTy_complete {B : Type} {θ : TySubst B} {S : Supply} {α : TyVar} {τ : Ty B}
    {s : Sol B} {S' : Supply}
    (h : bindTy S α τ = .success s S') (hu : TyEquiv (θ.ty α) (τ.applySubst θ)) :
    Sol.Sat θ s := by
  unfold bindTy at h
  split at h
  · simp only [UResM.success.injEq] at h
    obtain ⟨rfl, -⟩ := h
    exact Sol.Sat_nil
  · split at h
    · cases h
    · simp only [UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      refine ⟨fun p hp => ?_, fun _ hp => (nomatch hp)⟩
      obtain rfl := List.mem_singleton.mp hp
      exact hu

theorem solveVarM_complete {B : Type} {θ : TySubst B} {S : Supply}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hsolve : solveVarM S s₁ s₂ = some (.success s S'))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    Sol.Sat θ s := by
  cases s₁ with
  | nil => simp [solveVarM] at hsolve
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVarM] at hsolve
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVarM] at hsolve
      | nil =>
        simp only [solveVarM] at hsolve
        split at hsolve
        · simp at hsolve
        · simp only [Option.some.injEq, UResM.success.injEq] at hsolve
          obtain ⟨rfl, -⟩ := hsolve
          refine Sol.Sat_ofRow.mpr (fun p hp => ?_)
          obtain rfl := List.mem_singleton.mp hp
          simp only [ofSpine, Row.applySubst] at hu
          exact RowEquiv.unitR.symm.trans hu

theorem unifySpineMF_nil_left_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (s₂ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel [] s₂ = .success s S')
    (hu : RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ)
                   ((ofSpine s₂).applySubst θ)) : Sol.Sat θ s := by
  simp only [unifySpineMF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact Sol.Sat_ofRow.mpr (allVarsEmpty_complete s₂ hae hu.symm)

theorem unifySpineMF_cons_nil_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (a : Atom B) (s₁ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel (a :: s₁) [] = .success s S')
    (hu : RowEquiv ((ofSpine (a :: s₁)).applySubst θ)
                   ((ofSpine ([] : List (Atom B))).applySubst θ)) : Sol.Sat θ s := by
  simp only [unifySpineMF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact Sol.Sat_ofRow.mpr (allVarsEmpty_complete (a :: s₁) hae hu)

-- THE U-EXPAND ARM, at both orientations.
theorem expand_completeM {B : Type} [DecidableEq B] {fuel : Nat}
    (ih : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) (V : List TyVar) {s : Sol B}
      {S' : Supply} {θ : TySubst B},
      S.Avoids V → (sFtv u₁ ++ sFtv u₂) ⊆ V →
      unifySpineMF S fuel u₁ u₂ = .success s S' →
      RowEquiv ((ofSpine u₁).applySubst θ) ((ofSpine u₂).applySubst θ) →
      ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s)
    {S : Supply} {u₁ u₂ : List (Atom B)} {V : List TyVar} {θ : TySubst B}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hS : S.Avoids V) (hV : (sFtv u₁ ++ sFtv u₂) ⊆ V)
    (he : expandL S u₁ u₂ = some (β, l, τ, t₁, t₂))
    (h : expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂) = .success s S')
    (hu : RowEquiv ((ofSpine u₁).applySubst θ) ((ofSpine u₂).applySubst θ)) :
    ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s := by
  obtain ⟨s', hrec, rfl⟩ := expandResM_success h
  obtain ⟨hs1, hvv, hcc, hren⟩ := expandL_spec he
  have hdV := Supply.fresh_not_mem hS
  have hbV := Supply.fresh_not_mem hS.advance
  have hd₁ : S.fresh.1 ∉ sFtv u₁ := fun hm => hdV (sFtv_sub_left hV hm)
  have hd₂ : S.fresh.1 ∉ sFtv u₂ := fun hm => hdV (sFtv_sub_right hV hm)
  have hb₁ : S.fresh.2.fresh.1 ∉ sFtv u₁ := fun hm => hbV (sFtv_sub_left hV hm)
  have hb₂ : S.fresh.2.fresh.1 ∉ sFtv u₂ := fun hm => hbV (sFtv_sub_right hV hm)
  have hτV : τ.ftv ⊆ V := fun _ hx =>
    sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_left _ hx)
  have hβV : β ∈ V :=
    sFtv_sub_right hV (mem_sFtv_of_mem_sVarSeq u₂ (by rw [hvv]; exact List.mem_cons_self))
  rw [hs1] at hu hd₁ hb₁
  obtain ⟨θ₀, hβ0, hty0, hrec0, -, hag0⟩ :=
    expand_reflect_fwd hvv hcc hd₁ hd₂ hb₁ hb₂ hu
  have hS' : S.fresh.2.fresh.2.Avoids (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    hS.cons_fresh.cons_fresh
  have hV' : (sFtv t₁ ++ sFtv t₂) ⊆ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) := by
    intro x hx
    rcases List.mem_append.mp hx with hh | hh
    · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _
        (sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_right _ hh)))
    · rw [hren] at hh
      rcases List.mem_cons.mp (sFtv_renameVar _ _ u₂ x hh) with rfl | hh'
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (sFtv_sub_right hV hh'))
  have hrec' : RowEquiv ((ofSpine t₁).applySubst θ₀) ((ofSpine t₂).applySubst θ₀) := by
    rw [hren]; exact hrec0
  obtain ⟨θ', hag', hsat'⟩ := ih S.fresh.2.fresh.2 t₁ t₂ _ hS' hV' hrec hrec'
  have hVsub : V ⊆ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    fun _ hx => List.mem_cons_of_mem _ (List.mem_cons_of_mem _ hx)
  have hdvV' : S.fresh.1 ∈ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    List.mem_cons_of_mem _ List.mem_cons_self
  have hb'V' : S.fresh.2.fresh.1 ∈ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    List.mem_cons_self
  have hagθ : AgreeOn θ θ₀ V := fun α hα =>
    hag0 α (fun hh => hdV (by rw [← hh]; exact hα)) (fun hh => hbV (by rw [← hh]; exact hα))
  refine ⟨θ', hagθ.trans' hag' hVsub, Sol.Sat.comp ⟨fun p hp => ?_, fun p hp => ?_⟩ hsat'⟩
  · obtain rfl := List.mem_singleton.mp hp
    show TyEquiv (θ'.ty S.fresh.1) (τ.applySubst θ')
    rw [← hag'.tyEq (fun _ hx => hVsub (hτV hx)), ← (hag' _ hdvV').1]
    exact hty0.symm
  · obtain rfl := List.mem_singleton.mp hp
    show RowEquiv (θ'.row β)
      (.cat (.sing l (θ'.ty S.fresh.1)) (θ'.row S.fresh.2.fresh.1))
    rw [← (hag' β (hVsub hβV)).2, ← (hag' _ hdvV').1, ← (hag' _ hb'V').2]
    exact hβ0

-- THE COMPLETENESS LEG, both sorts at once.
theorem unifyM_success_complete {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) (V : List TyVar) {s : Sol B} {S' : Supply}
        {θ : TySubst B},
        S.Avoids V → (τ.ftv ++ τ'.ftv) ⊆ V →
        unifyTyF S fuel τ τ' = .success s S' → TyUnifies θ τ τ' →
        ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) (V : List TyVar) {s : Sol B} {S' : Supply}
        {θ : TySubst B},
        S.Avoids V → (sFtv s₁ ++ sFtv s₂) ⊆ V →
        unifySpineMF S fuel s₁ s₂ = .success s S' →
        RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) →
        ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s) := by
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' V s S' θ hS hV h hu => ?_, fun S s₁ s₂ V s S' θ hS hV h hu => ?_⟩
      · cases τ with
        | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu⟩
        | base b =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base b' => exact ⟨θ, AgreeOn.refl θ V, base_arm_complete h⟩
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => exact ⟨θ, AgreeOn.refl θ V, unk_arm_complete h⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
      · cases s₁ with
        | nil => exact ⟨θ, AgreeOn.refl θ V, unifySpineMF_nil_left_complete S 0 s₂ h hu⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              exact ⟨θ, AgreeOn.refl θ V, unifySpineMF_cons_nil_complete S 0 a s₁ h hu⟩
          | cons b s₂ => cases h
  | succ fuel ih =>
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          {s : Sol B} {S' : Supply} {θ : TySubst B}, S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' →
          TyUnifies θ τ τ' →
          RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) →
          ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s := by
        intro S τ τ' t₁ t₂ V s S' θ hS hVt hVr h hty hru
        obtain ⟨s₁, S₁, s₂, hsty, hsrow, rfl⟩ := UResM.seq_success h
        obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := (unifyM_bounded fuel).1 S τ τ' V hS hVt hsty
        obtain ⟨θ₁, hag₁, hsat₁⟩ := ih.1 S τ τ' V hS hVt hsty hty
        have hres : (sFtv (sApplySubst s₁.toSubst t₁) ++
                     sFtv (sApplySubst s₁.toSubst t₂)) ⊆ W₁ := fun x hx => by
          rcases List.mem_append.mp hx with hh | hh
          · exact sFtv_sApplySubst_sub
              (fun _ hy => hVW₁ (hVr (List.mem_append_left _ hy))) hb₁ hh
          · exact sFtv_sApplySubst_sub
              (fun _ hy => hVW₁ (hVr (List.mem_append_right _ hy))) hb₁ hh
        have hru₁ : RowEquiv ((ofSpine (sApplySubst s₁.toSubst t₁)).applySubst θ₁)
                             ((ofSpine (sApplySubst s₁.toSubst t₂)).applySubst θ₁) :=
          (unifies_sApplySubst_of_sat hsat₁ t₁ t₂).mpr
            (hag₁.unifiesSpine (sFtv_sub_left hVr) (sFtv_sub_right hVr) hru)
        obtain ⟨θ₂, hag₂, hsat₂⟩ := ih.2 S₁ _ _ W₁ hS₁ hres hsrow hru₁
        exact ⟨θ₂, hag₁.trans' hag₂ hVW₁,
               Sol.Sat.comp (hsat₁.congrAgree hag₂ hb₁) hsat₂⟩
      refine ⟨fun S τ τ' V s S' θ hS hV h hu => ?_, fun S s₁ s₂ V s S' θ hS hV h hu => ?_⟩
      · cases τ with
        | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu⟩
        | base b =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base b' => exact ⟨θ, AgreeOn.refl θ V, base_arm_complete h⟩
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => exact ⟨θ, AgreeOn.refl θ V, unk_arm_complete h⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn a₂ b₂ =>
                replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .success s S' := h
                obtain ⟨σ₁, σ₂, heq, hA, hB⟩ :=
                  TyEquiv.fn_inv (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ))
                    (Ty.fn (a₂.applySubst θ) (b₂.applySubst θ)) from hu)
                simp only [Ty.fn.injEq] at heq
                obtain ⟨rfl, rfl⟩ := heq
                have hVa : (a₁.ftv ++ a₂.ftv) ⊆ V := fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact hV (List.mem_append_left _ (List.mem_append_left _ hh))
                  · exact hV (List.mem_append_right _ (List.mem_append_left _ hh))
                have hVb₁ : b₁.ftv ⊆ V := fun _ hy =>
                  hV (List.mem_append_left _ (List.mem_append_right _ hy))
                have hVb₂ : b₂.ftv ⊆ V := fun _ hy =>
                  hV (List.mem_append_right _ (List.mem_append_right _ hy))
                obtain ⟨s₁, S₁, s₂, hsty, hsrow, rfl⟩ := UResM.seq_success h
                obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ :=
                  (unifyM_bounded fuel).1 S a₁ a₂ V hS hVa hsty
                obtain ⟨θ₁, hag₁, hsat₁⟩ := ih.1 S a₁ a₂ V hS hVa hsty hA
                have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                             (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₁ hy)) hb₁ hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₂ hy)) hb₁ hh
                have hB₁ : TyUnifies θ₁ (b₁.applySubst s₁.toSubst)
                    (b₂.applySubst s₁.toSubst) :=
                  (tyUnifies_applySubst_of_sat hsat₁ b₁ b₂).mpr
                    (hag₁.tyUnifies hVb₁ hVb₂ hB)
                obtain ⟨θ₂, hag₂, hsat₂⟩ := ih.1 S₁ _ _ W₁ hS₁ hres hsrow hB₁
                exact ⟨θ₂, hag₁.trans' hag₂ hVW₁,
                       Sol.Sat.comp (hsat₁.congrAgree hag₂ hb₁) hsat₂⟩
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
                obtain ⟨ρ', heq, hR⟩ :=
                  TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ))
                    (Ty.rcd (ρ₂.applySubst θ)) from hu)
                simp only [Ty.rcd.injEq] at heq
                obtain rfl := heq
                have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
                have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
                refine ih.2 S _ _ V hS (fun x hx => ?_) h (e₁.symm.trans (hR.trans e₂))
                rcases List.mem_append.mp hx with hh | hh
                · exact hV (List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh))
                · exact hV (List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh))
      · cases s₁ with
        | nil =>
            exact ⟨θ, AgreeOn.refl θ V,
              unifySpineMF_nil_left_complete S (fuel + 1) s₂ h hu⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              exact ⟨θ, AgreeOn.refl θ V,
                unifySpineMF_cons_nil_complete S (fuel + 1) a s₁ h hu⟩
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
                (stripL_reflect_fwd hsl hu)
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
                (stripR_reflect_fwd hsr hu)
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact ⟨θ, AgreeOn.refl θ V,
                solveVarM_complete (hv1.trans (congrArg some h)) hu⟩
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact ⟨θ, AgreeOn.refl θ V,
                solveVarM_complete (hv2.trans (congrArg some h)) hu.symm⟩
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml hu
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchL_ftv hml).1 hh)
                  · exact sFtv_sub_right hV ((matchL_ftv hml).2.2.1 hh))
                (sFtv_sub_residual hV (matchL_ftv hml).2.1 (matchL_ftv hml).2.2.2)
                h hty hru
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchL_ftv hml2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((matchL_ftv hml2).1 hh))
                (sFtv_sub_residual hV (matchL_ftv hml2).2.2.2 (matchL_ftv hml2).2.1)
                h hty.symm hru.symm
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr hu
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchR_ftv hmr).1 hh)
                  · exact sFtv_sub_right hV ((matchR_ftv hmr).2.2.1 hh))
                (sFtv_sub_residual hV (matchR_ftv hmr).2.1 (matchR_ftv hmr).2.2.2)
                h hty hru
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchR_ftv hmr2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((matchR_ftv hmr2).1 hh))
                (sFtv_sub_residual hV (matchR_ftv hmr2).2.2.2 (matchR_ftv hmr2).2.1)
                h hty.symm hru.symm
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg hu
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((groundMatch_ftv hg).1 hh)
                  · exact sFtv_sub_right hV ((groundMatch_ftv hg).2.2.1 hh))
                (sFtv_sub_residual hV (groundMatch_ftv hg).2.1 (groundMatch_ftv hg).2.2.2)
                h hty hru
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((groundMatch_ftv hg2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((groundMatch_ftv hg2).1 hh))
                (sFtv_sub_residual hV (groundMatch_ftv hg2).2.2.2 (groundMatch_ftv hg2).2.1)
                h hty.symm hru.symm
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact expand_completeM ih.2 hS hV he1 h hu
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              exact expand_completeM ih.2 hS (sFtv_sub_swap hV) he2 h hu.symm
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h <;> cases h

-- ≐ᵣ SUCCESS COMPLETENESS under the mutual driver. With unifyRowM_success_sound
-- the unifier set of ρ₁ ≐ᵣ ρ₂ is EXACTLY {θ : Sol.Sat θ s}, modulo the fresh
-- names the run invented: the algorithm computes a most general unifier, and
-- there are no leftover equations to qualify that any more.
theorem unifyRowM_success_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    {fuel : Nat} {ρ₁ ρ₂ : Row B} {s : Sol B} {S' : Supply}
    (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') (hu : Unifies θ ρ₁ ρ₂) :
    ∃ θ' : TySubst B,
      AgreeOn θ θ' (sFtv ρ₁.toSpine ++ sFtv ρ₂.toSpine) ∧ Sol.Sat θ' s := by
  unfold unifyRowM unifySpineM at h
  unfold Unifies at hu
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact (unifyM_success_complete fuel).2 _ ρ₁.toSpine ρ₂.toSpine _
    (localSupply_avoids _ _) (fun _ hx => hx) h (e₁.symm.trans (hu.trans e₂))


end MinimalCalculus
