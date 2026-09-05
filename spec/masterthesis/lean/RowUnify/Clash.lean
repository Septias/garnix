-- P5: clash soundness for the mutual driver.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Completeness

namespace MinimalCalculus

------------------ P5: CLASH SOUNDNESS, MUTUALLY ----------------------------
-- Clash soundness at both sorts. The interesting case is
-- **a clash inside a field type is a clash of the whole**. It splits in two —
-- the sub-unification clashed (the type IH kills it outright), or it SUCCEEDED
-- and the substituted residual clashed. The second is why the plan orders
-- completeness before clash: to hand a unifier to the residual one must first
-- EXTEND it to meet the solution the arm just applied, which is exactly
-- unifyM_success_complete plus unifyM_bounded.

theorem UResM.seq_clash {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (h : r.seq k = .clash) :
    r = .clash ∨ ∃ s S, r = .success s S ∧ k s.toSubst S = .clash := by
  cases r with
  | success s S =>
      refine .inr ⟨s, S, rfl, ?_⟩
      simp only [UResM.seq] at h
      revert h; cases hk : k s.toSubst S with
      | success s' S' => intro h; cases h
      | clash => intro _; rfl
      | occurs => intro h; cases h
      | stuck => intro h; cases h
      | outOfFuel => intro h; cases h
  | clash => exact .inl rfl
  | occurs => cases h
  | stuck => cases h
  | outOfFuel => cases h

theorem bindTy_ne_clash {B : Type} {S : Supply} {α : TyVar} {τ : Ty B} :
    bindTy S α τ ≠ .clash := by
  intro h; unfold bindTy at h
  split at h
  · cases h
  · split at h <;> cases h

theorem solveVarM_ne_clash {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)} :
    solveVarM S s₁ s₂ ≠ some .clash := by
  intro h
  cases s₁ with
  | nil => simp [solveVarM] at h
  | cons a r =>
    cases a with
    | field _ _ => simp [solveVarM] at h
    | var α =>
      cases r with
      | cons _ _ => simp [solveVarM] at h
      | nil => simp only [solveVarM] at h; split at h <;> simp at h

theorem expandResM_clash {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} (h : expandResM S β l τ r = .clash) : r = .clash := by
  cases r with
  | success _ _ => cases h
  | clash => rfl
  | occurs => cases h
  | stuck => cases h
  | outOfFuel => cases h

-- ≐ dispatches on the head constructors: every MISMATCH is refuted here once
-- and for all (≈ₜ never changes a head, minimal.lean:163ff), and only the two
-- recursive pairs are handed back to the caller.
theorem tyClash_dispatch {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {θ : TySubst B}
    (h : unifyTyF S fuel τ τ' = .clash) (hu : TyUnifies θ τ τ')
    (hfn : ∀ a₁ b₁ a₂ b₂ : Ty B, τ = .fn a₁ b₁ → τ' = .fn a₂ b₂ → False)
    (hrcd : ∀ ρ₁ ρ₂ : Row B, τ = .rcd ρ₁ → τ' = .rcd ρ₂ → False) : False := by
  cases τ with
  | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := τ') h
  | base b =>
      have hb := TyEquiv.base_inv (show TyEquiv (Ty.base b) _ from hu)
      cases τ' with
      | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := Ty.base b) h
      | base b' =>
          simp only [Ty.applySubst, Ty.base.injEq] at hb
          subst hb; cases fuel <;> simp [unifyTyF] at h
      | unk => simp only [Ty.applySubst] at hb; cases hb
      | fn _ _ => simp only [Ty.applySubst] at hb; cases hb
      | rcd _ => simp only [Ty.applySubst] at hb; cases hb
  | unk =>
      have hb := TyEquiv.unk_inv (show TyEquiv (Ty.unk : Ty B) _ from hu)
      cases τ' with
      | var α =>
          cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := (Ty.unk : Ty B)) h
      | base _ => simp only [Ty.applySubst] at hb; cases hb
      | unk => cases fuel <;> simp [unifyTyF] at h
      | fn _ _ => simp only [Ty.applySubst] at hb; cases hb
      | rcd _ => simp only [Ty.applySubst] at hb; cases hb
  | fn a₁ b₁ =>
      have hb := TyEquiv.fn_inv
        (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from hu)
      cases τ' with
      | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := Ty.fn a₁ b₁) h
      | base _ => obtain ⟨_, _, he, -, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | unk => obtain ⟨_, _, he, -, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | fn a₂ b₂ => exact hfn a₁ b₁ a₂ b₂ rfl rfl
      | rcd _ => obtain ⟨_, _, he, -, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
  | rcd ρ₁ =>
      have hb := TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from hu)
      cases τ' with
      | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := Ty.rcd ρ₁) h
      | base _ => obtain ⟨_, he, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | unk => obtain ⟨_, he, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | fn _ _ => obtain ⟨_, he, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | rcd ρ₂ => exact hrcd ρ₁ ρ₂ rfl rfl

-- THE CLASH LEG, both sorts at once. `V` is the problem's own variables at every
-- call, so the statement stays free of a set parameter; the arms below hand the
-- enlarged set W to the sub-calls internally.
theorem unifyM_clash_no_unifier {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B),
        S.Avoids (τ.ftv ++ τ'.ftv) → unifyTyF S fuel τ τ' = .clash →
        ¬ ∃ θ : TySubst B, TyUnifies θ τ τ') ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)),
        S.Avoids (sFtv s₁ ++ sFtv s₂) → unifySpineMF S fuel s₁ s₂ = .clash →
        ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s₁) (ofSpine s₂)) := by
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' _ h => ?_, fun S s₁ s₂ _ h => ?_⟩
      · rintro ⟨θ, hu⟩
        exact tyClash_dispatch h hu
          (fun _ _ _ _ e₁ e₂ => by subst e₁; subst e₂; cases h)
          (fun _ _ e₁ e₂ => by subst e₁; subst e₂; cases h)
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
            | some => simp [hae] at h
        | cons a s₁ =>
            cases s₂ with
            | nil =>
                simp only [unifySpineMF] at h
                cases hae : allVarsEmpty (a :: s₁) with
                | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
                | some => simp [hae] at h
            | cons b s₂ => cases h
  | succ fuel ih =>
      -- the shape every eq-emitting arm produces
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          (θ : TySubst B), S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂)) = .clash →
          TyUnifies θ τ τ' →
          RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) → False := by
        intro S τ τ' t₁ t₂ V θ hS hVt hVr h hty hru
        rcases UResM.seq_clash h with hc | ⟨s₁, S₁, hs, hk⟩
        · exact ih.1 S τ τ' (hS.mono hVt) hc ⟨θ, hty⟩
        · obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := (unifyM_bounded fuel).1 S τ τ' V hS hVt hs
          obtain ⟨θ₁, hag₁, hsat₁⟩ :=
            (unifyM_success_complete fuel).1 S τ τ' V hS hVt hs hty
          have hres : (sFtv (sApplySubst s₁.toSubst t₁) ++
                       sFtv (sApplySubst s₁.toSubst t₂)) ⊆ W₁ := fun x hx => by
            rcases List.mem_append.mp hx with hh | hh
            · exact sFtv_sApplySubst_sub
                (fun _ hy => hVW₁ (hVr (List.mem_append_left _ hy))) hb₁ hh
            · exact sFtv_sApplySubst_sub
                (fun _ hy => hVW₁ (hVr (List.mem_append_right _ hy))) hb₁ hh
          exact ih.2 S₁ _ _ (hS₁.mono hres) hk
            ⟨θ₁, (unifies_sApplySubst_of_sat hsat₁ t₁ t₂).mpr
              (hag₁.unifiesSpine (sFtv_sub_left hVr) (sFtv_sub_right hVr) hru)⟩
      refine ⟨fun S τ τ' hS h => ?_, fun S s₁ s₂ hS h => ?_⟩
      · rintro ⟨θ, hu⟩
        refine tyClash_dispatch h hu (fun a₁ b₁ a₂ b₂ e₁ e₂ => ?_)
          (fun ρ₁ ρ₂ e₁ e₂ => ?_)
        · subst e₁; subst e₂
          replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
              unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ')) = .clash := h
          obtain ⟨σ₁, σ₂, heq, hA, hB⟩ := TyEquiv.fn_inv
            (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from hu)
          simp only [Ty.applySubst, Ty.fn.injEq] at heq
          obtain ⟨rfl, rfl⟩ := heq
          have hVa : (a₁.ftv ++ a₂.ftv) ⊆ ((Ty.fn a₁ b₁).ftv ++ (Ty.fn a₂ b₂).ftv) :=
            fun x hx => by
              rcases List.mem_append.mp hx with hh | hh
              · exact List.mem_append_left _ (List.mem_append_left _ hh)
              · exact List.mem_append_right _ (List.mem_append_left _ hh)
          have hVb₁ : b₁.ftv ⊆ ((Ty.fn a₁ b₁).ftv ++ (Ty.fn a₂ b₂).ftv) :=
            fun _ hy => List.mem_append_left _ (List.mem_append_right _ hy)
          have hVb₂ : b₂.ftv ⊆ ((Ty.fn a₁ b₁).ftv ++ (Ty.fn a₂ b₂).ftv) :=
            fun _ hy => List.mem_append_right _ (List.mem_append_right _ hy)
          rcases UResM.seq_clash h with hc | ⟨s₁, S₁, hs, hk⟩
          · exact ih.1 S a₁ a₂ (hS.mono hVa) hc ⟨θ, hA⟩
          · obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ :=
              (unifyM_bounded fuel).1 S a₁ a₂ _ hS hVa hs
            obtain ⟨θ₁, hag₁, hsat₁⟩ :=
              (unifyM_success_complete fuel).1 S a₁ a₂ _ hS hVa hs hA
            have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                         (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
              rcases List.mem_append.mp hx with hh | hh
              · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₁ hy)) hb₁ hh
              · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₂ hy)) hb₁ hh
            exact ih.1 S₁ _ _ (hS₁.mono hres) hk
              ⟨θ₁, (tyUnifies_applySubst_of_sat hsat₁ b₁ b₂).mpr
                (hag₁.tyUnifies hVb₁ hVb₂ hB)⟩
        · subst e₁; subst e₂
          replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .clash := h
          obtain ⟨ρ', heq, hR⟩ := TyEquiv.rcd_inv
            (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from hu)
          simp only [Ty.applySubst, Ty.rcd.injEq] at heq
          obtain rfl := heq
          have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
          have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
          refine ih.2 S _ _ (hS.mono (fun x hx => ?_)) h
            ⟨θ, e₁.symm.trans (hR.trans e₂)⟩
          rcases List.mem_append.mp hx with hh | hh
          · exact List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh)
          · exact List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh)
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
            | some => simp [hae] at h
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
              | some => simp [hae] at h
          | cons b s₂ =>
            rintro ⟨θ, hu⟩
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ (hS.residual (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
                ⟨θ, stripL_reflect_fwd hsl hu⟩
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ (hS.residual (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
                ⟨θ, stripR_reflect_fwd hsr hu⟩
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_ne_clash (hv1.trans (congrArg some h))
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact solveVarM_ne_clash (hv2.trans (congrArg some h))
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml hu
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml).1 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml).2.2.1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml).2.1 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml).2.2.2 hh))
                h hty hru
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml2).2.2.1 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml2).1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml2).2.2.2 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml2).2.1 hh))
                h hty.symm hru.symm
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr hu
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr).1 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr).2.2.1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr).2.1 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr).2.2.2 hh))
                h hty hru
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr2).2.2.1 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr2).1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr2).2.2.2 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr2).2.1 hh))
                h hty.symm hru.symm
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg hu
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg).1 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg).2.2.1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg).2.1 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg).2.2.2 hh))
                h hty hru
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg2).2.2.1 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg2).1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg2).2.2.2 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg2).2.1 hh))
                h hty.symm hru.symm
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact ih.2 S.fresh.2.fresh.2 t₁ t₂ (expandL_avoids hS he1)
                (expandResM_clash h) (expandL_reflect_fwd hS he1 hu)
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              exact ih.2 S.fresh.2.fresh.2 t₁ t₂ (expandL_avoids hS.swap he2)
                (expandResM_clash h) (expandL_reflect_fwd hS.swap he2 hu.symm)
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h
              · rename_i hpc; exact projClash_no_unifier hpc ⟨θ, hu⟩
              · cases h

-- ≐ᵣ CLASH is SOUND under the mutual driver: a clash verdict means the two rows
-- have no unifier — including when the clash was found inside a field type.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = clash   ⟹   ¬ ∃ θ. θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRowM_clash_no_unifier {B : Type} [DecidableEq B] {fuel : Nat}
    {ρ₁ ρ₂ : Row B} (h : unifyRowM fuel ρ₁ ρ₂ = .clash) :
    ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ := by
  rintro ⟨θ, hu⟩
  unfold unifyRowM unifySpineM at h
  refine (unifyM_clash_no_unifier fuel).2 _ ρ₁.toSpine ρ₂.toSpine
    (localSupply_avoids _ _) h ⟨θ, ?_⟩
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.symm.trans (hu.trans e₂)




end MinimalCalculus
