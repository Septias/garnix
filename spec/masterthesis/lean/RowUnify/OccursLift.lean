-- `.occurs ⟹ no unifier`, FOR THE WHOLE DRIVER.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- The two local guards were already sound: `bindTy_occurs_no_unifier` at the
-- type sort and `solveVarM_occurs_no_unifier` at the row sort (NoMgu.lean).
-- What was missing is the lift through the driver, and it was blocked only by
-- the `depReach Θ` disjunct U-expand's dependency graph put into the row guard
-- (a fact about the solver state, from which no no-unifier theorem follows).
-- With the graph gone, the lift is a plain induction:
--
--  * every non-solving move is FORCED — a unifier of the problem unifies the
--    residual (`stripL_reflect_fwd`, `matchL_reflect_fwd`, …), so `.occurs` on
--    the residual refutes the problem;
--  * in an eq-emitting arm, `.occurs` either comes from the emitted equation
--    (which a unifier of the problem would solve), or from the SUBSTITUTED
--    residual — and success completeness carries a unifier of the problem over
--    to one of that residual, exactly as in `unifyM_success_complete`'s arm.
--
-- So `.occurs` is now a genuine rejection at every depth, and the trichotomy's
-- three legs are: success ⟹ mgu (non-vacuous, `unifyRowM_success_mgu`),
-- clash ⟹ no unifier, occurs ⟹ no unifier. `.stuck` alone is conservative.

import RowUnify.Applied

namespace MinimalCalculus

-- ⊢  seq inverts at `.occurs`: the first stage reported it, or it succeeded
--    and the second stage did
theorem UResM.seq_occurs {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (h : r.seq k = .occurs) :
    r = .occurs ∨ ∃ s₁ S₁, r = .success s₁ S₁ ∧ k s₁.toSubst S₁ = .occurs := by
  cases r with
  | success s₁ S₁ =>
      simp only [UResM.seq] at h
      revert h; cases hk : k s₁.toSubst S₁ with
      | success _ _ => intro h; cases h
      | occurs => intro _; exact .inr ⟨s₁, S₁, rfl, hk⟩
      | clash  => intro h; cases h
      | stuck  => intro h; cases h
      | outOfFuel => intro h; cases h
  | occurs => exact .inl rfl
  | clash  => cases h
  | stuck  => cases h
  | outOfFuel => cases h

-- ⊢  U-var-solve's local guard, read on spines
theorem solveVarM_occurs_no_unifier' {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    (h : solveVarM S s₁ s₂ = some .occurs) :
    ¬ ∃ θ : TySubst B, RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  match s₁, h with
  | [.var α], h =>
      rintro ⟨θ, hu⟩
      have e := RowEquiv.applySubst θ (Row.toSpine_equiv (Row.var (B := B) α))
      exact solveVarM_occurs_no_unifier h ⟨θ, e.trans hu⟩

theorem bindTy_occurs_no_unifier' {B : Type} {S : Supply} {α : TyVar} {τ : Ty B}
    (h : bindTy S α τ = .occurs) : ¬ ∃ θ : TySubst B, TyUnifies θ τ (.var α) :=
  fun ⟨θ, hu⟩ => bindTy_occurs_no_unifier h ⟨θ, hu.symm⟩

-- ⊢  THE LIFT, at both sorts. The `V`/`Avoids` hypotheses are success
--    completeness's, which the eq-emitting arms call on their first stage.
theorem unifyM_occurs_no_unifier {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) (V : List TyVar),
        S.Avoids V → (τ.ftv ++ τ'.ftv) ⊆ V →
        unifyTyF S fuel τ τ' = .occurs → ¬ ∃ θ : TySubst B, TyUnifies θ τ τ') ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) (V : List TyVar),
        S.Avoids V → (sFtv s₁ ++ sFtv s₂) ⊆ V →
        unifySpineMF S fuel s₁ s₂ = .occurs →
        ¬ ∃ θ : TySubst B,
          RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) := by
  -- the empty-spine arms answer success or clash, never occurs
  have hnilL : ∀ (S : Supply) (s₂ : List (Atom B)) (fuel : Nat),
      unifySpineMF S fuel [] s₂ ≠ .occurs := by
    intro S s₂ fuel h
    simp only [unifySpineMF] at h
    cases hae : allVarsEmpty s₂ <;> simp [hae] at h
  have hnilR : ∀ (S : Supply) (a : Atom B) (s₁ : List (Atom B)) (fuel : Nat),
      unifySpineMF S fuel (a :: s₁) [] ≠ .occurs := by
    intro S a s₁ fuel h
    simp only [unifySpineMF] at h
    cases hae : allVarsEmpty (a :: s₁) <;> simp [hae] at h
  -- the type-sort arms that do not recurse: only a variable can report occurs
  have hflat : ∀ (fuel : Nat) (S : Supply) (τ τ' : Ty B),
      tyRec τ τ' = false → unifyTyF S fuel τ τ' = .occurs →
      ¬ ∃ θ : TySubst B, TyUnifies θ τ τ' := by
    intro fuel S τ τ' hrec h
    cases τ with
    | var α => cases fuel <;> exact bindTy_occurs_no_unifier h
    | base b =>
        cases τ' with
        | var α => cases fuel <;> exact bindTy_occurs_no_unifier' h
        | base b' =>
            by_cases hb : b = b'
            · subst hb; cases fuel <;> simp [unifyTyF] at h
            · cases fuel <;> simp [unifyTyF, hb] at h
        | unk => cases fuel <;> cases h
        | fn _ _ => cases fuel <;> cases h
        | rcd _ => cases fuel <;> cases h
    | unk =>
        cases τ' with
        | var α => cases fuel <;> exact bindTy_occurs_no_unifier' h
        | base _ => cases fuel <;> cases h
        | unk => cases fuel <;> simp [unifyTyF] at h
        | fn _ _ => cases fuel <;> cases h
        | rcd _ => cases fuel <;> cases h
    | fn a₁ b₁ =>
        cases τ' with
        | var α => cases fuel <;> exact bindTy_occurs_no_unifier' h
        | base _ => cases fuel <;> cases h
        | unk => cases fuel <;> cases h
        | fn _ _ => simp [tyRec] at hrec
        | rcd _ => cases fuel <;> cases h
    | rcd ρ₁ =>
        cases τ' with
        | var α => cases fuel <;> exact bindTy_occurs_no_unifier' h
        | base _ => cases fuel <;> cases h
        | unk => cases fuel <;> cases h
        | fn _ _ => cases fuel <;> cases h
        | rcd _ => simp [tyRec] at hrec
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' V _ _ h => ?_, fun S s₁ s₂ V _ _ h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => exact hflat 0 S τ τ' hrec h
        | true =>
          rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
          · cases h
          · cases h
      · cases s₁ with
        | nil => exact absurd h (hnilL S s₂ 0)
        | cons a s₁ =>
          cases s₂ with
          | nil => exact absurd h (hnilR S a s₁ 0)
          | cons b s₂ => cases h
  | succ fuel ih =>
      -- an eq-emitting arm: occurs in the equation, or in the substituted residual
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          {θ : TySubst B}, S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .occurs →
          TyUnifies θ τ τ' →
          RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) → False := by
        intro S τ τ' t₁ t₂ V θ hS hVt hVr h hty hru
        rcases UResM.seq_occurs h with h₁ | ⟨s₁, S₁, hsty, hsrow⟩
        · exact ih.1 S τ τ' V hS hVt h₁ ⟨θ, hty⟩
        · obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := (unifyM_bounded fuel).1 S τ τ' V hS hVt hsty
          obtain ⟨θ₁, hag₁, hsat₁⟩ :=
            (unifyM_success_complete fuel).1 S τ τ' V hS hVt hsty hty
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
          exact ih.2 S₁ _ _ W₁ hS₁ hres hsrow ⟨θ₁, hru₁⟩
      refine ⟨fun S τ τ' V hS hV h => ?_, fun S s₁ s₂ V hS hV h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => exact hflat (fuel + 1) S τ τ' hrec h
        | true =>
        rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
        · replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
              unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ')) = .occurs := h
          rintro ⟨θ, hu⟩
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
          rcases UResM.seq_occurs h with h₁ | ⟨s₁, S₁, hsty, hsrow⟩
          · exact ih.1 S a₁ a₂ V hS hVa h₁ ⟨θ, hA⟩
          · obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := (unifyM_bounded fuel).1 S a₁ a₂ V hS hVa hsty
            obtain ⟨θ₁, hag₁, hsat₁⟩ :=
              (unifyM_success_complete fuel).1 S a₁ a₂ V hS hVa hsty hA
            have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                         (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
              rcases List.mem_append.mp hx with hh | hh
              · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₁ hy)) hb₁ hh
              · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₂ hy)) hb₁ hh
            have hB₁ : TyUnifies θ₁ (b₁.applySubst s₁.toSubst)
                (b₂.applySubst s₁.toSubst) :=
              (tyUnifies_applySubst_of_sat hsat₁ b₁ b₂).mpr
                (hag₁.tyUnifies hVb₁ hVb₂ hB)
            exact ih.1 S₁ _ _ W₁ hS₁ hres hsrow ⟨θ₁, hB₁⟩
        · replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .occurs := h
          rintro ⟨θ, hu⟩
          obtain ⟨ρ', heq, hR⟩ :=
            TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ))
              (Ty.rcd (ρ₂.applySubst θ)) from hu)
          simp only [Ty.rcd.injEq] at heq
          obtain rfl := heq
          have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
          have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
          refine ih.2 S _ _ V hS (fun x hx => ?_) h ⟨θ, e₁.symm.trans (hR.trans e₂)⟩
          rcases List.mem_append.mp hx with hh | hh
          · exact hV (List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh))
          · exact hV (List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh))
      · cases s₁ with
        | nil => exact absurd h (hnilL S s₂ _)
        | cons a s₁ =>
          cases s₂ with
          | nil => exact absurd h (hnilR S a s₁ _)
          | cons b s₂ =>
            rintro ⟨θ, hu⟩
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
                ⟨θ, stripL_reflect_fwd hsl hu⟩
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
                ⟨θ, stripR_reflect_fwd hsr hu⟩
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_occurs_no_unifier' (hv1.trans (congrArg some h)) ⟨θ, hu⟩
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact solveVarM_occurs_no_unifier' (hv2.trans (congrArg some h))
                ⟨θ, hu.symm⟩
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
            cases hpc : projClash (a :: s₁) (b :: s₂) with
            | true =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc] at h
              cases h
            | false =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc] at h
              cases h

-- ⊢  …at the entry points: an `.occurs` verdict means NO unifier exists
theorem unifyRowM_occurs_no_unifier {B : Type} [DecidableEq B] {fuel : Nat}
    {ρ₁ ρ₂ : Row B} (h : unifyRowM fuel ρ₁ ρ₂ = .occurs) :
    ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ := by
  rintro ⟨θ, hu⟩
  unfold unifyRowM unifySpineM at h
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact (unifyM_occurs_no_unifier fuel).2 _ _ _ _ (localSupply_avoids _ _)
    (fun _ hx => hx) h ⟨θ, e₁.symm.trans (hu.trans e₂)⟩

theorem unifyTyM_occurs_no_unifier {B : Type} [DecidableEq B] {fuel : Nat}
    {τ τ' : Ty B} (h : unifyTyM fuel τ τ' = .occurs) :
    ¬ ∃ θ : TySubst B, TyUnifies θ τ τ' :=
  (unifyM_occurs_no_unifier fuel).1 _ _ _ _ (Nat.lt_succ_self _) (fun _ hx => hx) h

end MinimalCalculus
