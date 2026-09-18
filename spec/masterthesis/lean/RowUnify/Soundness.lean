-- P5: success soundness for the mutual driver.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Driver

namespace MinimalCalculus

--------------------- P5: SUCCESS SOUNDNESS, MUTUALLY -----------------------
-- Success soundness at both sorts: a θ
-- that meets the solution unifies the problem.
--
-- An eq-emitting arm's success is a COMPOSITE, and Sol.Sat.comp_inv splits it
-- into the type solution and the residual solution; the type IH turns the first
-- into the `heq` those reflection lemmas want, and unifies_sApplySubst_of_sat
-- (P1's apply-then-unify bridge) undoes the substitution the arm applied to the
-- residual.
--
-- Fuel is arbitrary: a success means the same thing whatever the budget was, so
-- unifyM_fuel_mono is not needed here.

-- ⊢  α ≔ τ, once met, unifies α with τ
theorem bindTy_sound {B : Type} {θ : TySubst B}  {S : Supply} {α : TyVar} {τ : Ty B}
    {s : Sol B} {S' : Supply}
    (h : bindTy S α τ = .success s S') (hsat : Sol.Sat θ s) :
    TyUnifies θ (.var α) τ := by
  unfold bindTy at h
  split at h
  · next hv => rw [tyIsVar_eq hv]; exact TyEquiv.refl _
  · split at h
    · exact absurd h (by simp)
    · simp only [UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      exact hsat.1 (α, τ) List.mem_cons_self

-- ⊢  U-var-solve, at the mutual result type
theorem solveVarM_reflect {B : Type} {θ : TySubst B} {Θ : DepGraph} {S : Supply}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hsolve : solveVarM Θ S s₁ s₂ = some (.success s S')) (hsat : Sol.Sat θ s) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
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
        · -- the ε-COLLAPSE arm: the emitted bindings ARE a unifier
          next σ hc =>
            simp only [Option.some.injEq, UResM.success.injEq] at hsolve
            obtain ⟨rfl, -⟩ := hsolve
            simp only [ofSpine, Row.applySubst]
            exact RowEquiv.unitR.trans
              (collapseSol_reflect hc (fun p hp => hsat.2 p hp))
        · split at hsolve
          · simp at hsolve
          · simp only [Option.some.injEq, UResM.success.injEq] at hsolve
            obtain ⟨rfl, -⟩ := hsolve
            have hbind := hsat.2 (α, ofSpine s₂) List.mem_cons_self
            simp only [ofSpine, Row.applySubst]
            exact RowEquiv.unitR.trans hbind

-- ⊢  the U-expand wrapper inverts: a success came from a success underneath
theorem expandResM_success {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} {s : Sol B} {S' : Supply}
    (h : expandResM S β l τ r = .success s S') :
    ∃ s', r = .success s' S' ∧
      s = s'.comp ⟨[(S.fresh.1, τ)],
                   [(β, .cat (.sing l (.var S.fresh.1)) (.var S.fresh.2.fresh.1))]⟩ := by
  cases r with
  | success s₀ S₀ =>
      simp only [expandResM, UResM.success.injEq] at h
      exact ⟨s₀, by rw [h.2], h.1.symm⟩
  | clash     => cases h
  | occurs    => cases h
  | stuck     => cases h
  | outOfFuel => cases h

-- ⊢  … and so does the RIGHT-end wrapper
theorem expandResRM_success {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} {s : Sol B} {S' : Supply}
    (h : expandResRM S β l τ r = .success s S') :
    ∃ s', r = .success s' S' ∧
      s = s'.comp ⟨[(S.fresh.1, τ)],
                   [(β, .cat (.var S.fresh.2.fresh.1) (.sing l (.var S.fresh.1)))]⟩ := by
  cases r with
  | success s₀ S₀ =>
      simp only [expandResRM, UResM.success.injEq] at h
      exact ⟨s₀, by rw [h.2], h.1.symm⟩
  | clash     => cases h
  | occurs    => cases h
  | stuck     => cases h
  | outOfFuel => cases h

-- ## THE SUPPLY ONLY ADVANCES
-- Every name the driver invents is drawn from the threaded supply, and a
-- successful run never hands one back. This is what "inferred variables are
-- fresh" rests on one level up (Infer.lean): an A-rule that took a supply from
-- a sub-call and then drew from it would otherwise re-issue a live name.
--
-- Every arm has one of three shapes: it returns the supply it was given
-- (bindTy, solveVarM, allVarsEmpty, the ★/base arms), it SEQUENCES two calls
-- (the eq-emitting moves and ≐'s congruences), or it advances by two and
-- recurses (the four expansions).
private theorem bindTy_supply {B : Type} {S : Supply} {α : TyVar} {τ : Ty B}
    {s : Sol B} {S' : Supply} (h : bindTy S α τ = .success s S') :
    S.next ≤ S'.next := by
  unfold bindTy at h
  split at h
  · simp only [UResM.success.injEq] at h; obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _
  · split at h
    · cases h
    · simp only [UResM.success.injEq] at h; obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _

private theorem solveVarM_supply {B : Type} {Θ : DepGraph} {S : Supply}
    {u₁ u₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (h : solveVarM Θ S u₁ u₂ = some (.success s S')) : S.next ≤ S'.next := by
  match u₁ with
  | [] => simp [solveVarM] at h
  | .field _ _ :: _ => simp [solveVarM] at h
  | [.var α] =>
      simp only [solveVarM] at h
      split at h
      · simp only [Option.some.injEq, UResM.success.injEq] at h
        obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _
      · split at h
        · simp at h
        · simp only [Option.some.injEq, UResM.success.injEq] at h
          obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _
  | .var _ :: _ :: _ => simp [solveVarM] at h

private theorem allVarsEmpty_supply {B : Type} {S : Supply} {u : List (Atom B)}
    {σ : List (TyVar × Row B)} {s : Sol B} {S' : Supply}
    (hae : allVarsEmpty u = some σ)
    (h : (match allVarsEmpty u with
          | some σ => UResM.success (Sol.ofRow σ) S
          | none => .clash) = .success s S') : S.next ≤ S'.next := by
  rw [hae] at h
  simp only [UResM.success.injEq] at h
  obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _

private theorem unifySpine_supply {B : Type} [DecidableEq B] {f : Nat}
    (ih : (∀ (Θ : DepGraph) (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
              unifyTyF Θ S f τ τ' = .success s S' → S.next ≤ S'.next) ∧
          (∀ (Θ : DepGraph) (S : Supply) (u₁ u₂ : List (Atom B))
              {s : Sol B} {S' : Supply},
              unifySpineMF Θ S f u₁ u₂ = .success s S' → S.next ≤ S'.next))
    (Θ : DepGraph) (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF Θ S (f + 1) s₁ s₂ = .success s S') : S.next ≤ S'.next := by
  -- the shape every eq-emitting arm produces
  have arm : ∀ (Θ : DepGraph) (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B))
      {s : Sol B} {S' : Supply},
      ((unifyTyF Θ S f τ τ').seq fun θ' S'' =>
          unifySpineMF Θ S'' f (sApplySubst θ' t₁) (sApplySubst θ' t₂))
        = .success s S' → S.next ≤ S'.next := by
    intro Θ S τ τ' t₁ t₂ s S' hh
    obtain ⟨s₁, S₁, s₂, h₁, h₂, -⟩ := UResM.seq_success hh
    exact Nat.le_trans (ih.1 Θ S _ _ h₁) (ih.2 Θ S₁ _ _ h₂)
  -- …and the shape the four expansions produce: advance by two, then recurse
  have expL : ∀ (Θ' : DepGraph) (β : TyVar) (l : Label) (τ : Ty B)
      (t₁ t₂ : List (Atom B)) {s : Sol B} {S' : Supply},
      expandResM S β l τ (unifySpineMF Θ' S.fresh.2.fresh.2 f t₁ t₂)
        = .success s S' → S.next ≤ S'.next := by
    intro Θ' β l τ t₁ t₂ s S' hh
    obtain ⟨s', hrec, -⟩ := expandResM_success hh
    have := ih.2 Θ' S.fresh.2.fresh.2 t₁ t₂ hrec
    simp only [Supply.fresh] at this
    omega
  have expR : ∀ (Θ' : DepGraph) (β : TyVar) (l : Label) (τ : Ty B)
      (t₁ t₂ : List (Atom B)) {s : Sol B} {S' : Supply},
      expandResRM S β l τ (unifySpineMF Θ' S.fresh.2.fresh.2 f t₁ t₂)
        = .success s S' → S.next ≤ S'.next := by
    intro Θ' β l τ t₁ t₂ s S' hh
    obtain ⟨s', hrec, -⟩ := expandResRM_success hh
    have := ih.2 Θ' S.fresh.2.fresh.2 t₁ t₂ hrec
    simp only [Supply.fresh] at this
    omega
  cases s₁ with
  | nil =>
      simp only [unifySpineMF] at h
      cases hae : allVarsEmpty s₂ with
      | some σ => exact allVarsEmpty_supply hae h
      | none => rw [hae] at h; cases h
  | cons a s₁ =>
    cases s₂ with
    | nil =>
        simp only [unifySpineMF] at h
        cases hae : allVarsEmpty (a :: s₁) with
        | some σ => exact allVarsEmpty_supply hae h
        | none => rw [hae] at h; cases h
    | cons b s₂ =>
      unfold unifySpineMF at h
      cases hsl : stripL (a :: s₁) (b :: s₂) with
      | some p => obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h; exact ih.2 Θ S t₁ t₂ h
      | none =>
      cases hsr : stripR (a :: s₁) (b :: s₂) with
      | some p =>
          obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h; exact ih.2 Θ S t₁ t₂ h
      | none =>
      cases hv1 : solveVarM Θ S (a :: s₁) (b :: s₂) with
      | some r =>
          simp only [hsl, hsr, hv1] at h
          exact solveVarM_supply (hv1.trans (congrArg some h))
      | none =>
      cases hv2 : solveVarM Θ S (b :: s₂) (a :: s₁) with
      | some r =>
          simp only [hsl, hsr, hv1, hv2] at h
          exact solveVarM_supply (hv2.trans (congrArg some h))
      | none =>
      cases hml : matchL (a :: s₁) (b :: s₂) with
      | some p =>
          obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
          exact arm Θ S τ0 τ0' t₁ t₂ h
      | none =>
      cases hml2 : matchL (b :: s₂) (a :: s₁) with
      | some p =>
          obtain ⟨τ0', τ0, t₂, t₁⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
          exact arm Θ S τ0 τ0' t₁ t₂ h
      | none =>
      cases hmr : matchR (a :: s₁) (b :: s₂) with
      | some p =>
          obtain ⟨τ0, τ0', t₁, t₂⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
          exact arm Θ S τ0 τ0' t₁ t₂ h
      | none =>
      cases hmr2 : matchR (b :: s₂) (a :: s₁) with
      | some p =>
          obtain ⟨τ0', τ0, t₂, t₁⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
          exact arm Θ S τ0 τ0' t₁ t₂ h
      | none =>
      cases hg : groundMatch (a :: s₁) (b :: s₂) with
      | some p =>
          obtain ⟨τ0, τ0', t₁, t₂⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
          exact arm Θ S τ0 τ0' t₁ t₂ h
      | none =>
      cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
      | some p =>
          obtain ⟨τ0', τ0, t₂, t₁⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
          exact arm Θ S τ0 τ0' t₁ t₂ h
      | none =>
      cases he1 : expandL Θ S (a :: s₁) (b :: s₂) with
      | some p =>
          obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
          exact expL _ β0 l0 τ0 t₁ t₂ h
      | none =>
      cases he2 : expandL Θ S (b :: s₂) (a :: s₁) with
      | some p =>
          obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
          exact expL _ β0 l0 τ0 t₁ t₂ h
      | none =>
      cases hpc : projClash (a :: s₁) (b :: s₂) with
      | true =>
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
            hpc] at h
          cases h
      | false =>
      cases he3 : expandR Θ S (a :: s₁) (b :: s₂) with
      | some p =>
          obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
            hpc, he3] at h
          exact expR _ β0 l0 τ0 t₁ t₂ h
      | none =>
      cases he4 : expandR Θ S (b :: s₂) (a :: s₁) with
      | some p =>
          obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
            hpc, he3, he4] at h
          exact expR _ β0 l0 τ0 t₁ t₂ h
      | none =>
          simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
            hpc, he3, he4] at h
          cases h

theorem unifyM_supply_mono {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (Θ : DepGraph) (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
        unifyTyF Θ S fuel τ τ' = .success s S' → S.next ≤ S'.next) ∧
    (∀ (Θ : DepGraph) (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply},
        unifySpineMF Θ S fuel s₁ s₂ = .success s S' → S.next ≤ S'.next) := by
  induction fuel with
  | zero =>
      refine ⟨fun Θ S τ τ' s S' h => ?_, fun Θ S s₁ s₂ s S' h => ?_⟩
      · cases τ <;> cases τ' <;> first
          | exact bindTy_supply h
          | (simp only [unifyTyF, UResM.success.injEq] at h
             obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _)
          | (simp only [unifyTyF] at h
             split at h
             · simp only [UResM.success.injEq] at h
               obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _
             · cases h)
          | cases h
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | some σ => exact allVarsEmpty_supply hae h
            | none => rw [hae] at h; cases h
        | cons a t =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: t) with
              | some σ => exact allVarsEmpty_supply hae h
              | none => rw [hae] at h; cases h
          | cons b t' => cases h
  | succ f ih =>
      refine ⟨fun Θ S τ τ' s S' h => ?_, fun Θ S s₁ s₂ s S' h => ?_⟩
      · cases τ <;> cases τ' <;>
          first
            | exact bindTy_supply h
            | (simp only [unifyTyF, UResM.success.injEq] at h
               obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _)
            | (simp only [unifyTyF] at h
               split at h
               · simp only [UResM.success.injEq] at h
                 obtain ⟨-, rfl⟩ := h; exact Nat.le_refl _
               · cases h)
            | cases h
            | (obtain ⟨sa, Sa, sb, h₁, h₂, -⟩ := UResM.seq_success h
               exact Nat.le_trans (ih.1 Θ S _ _ h₁) (ih.1 Θ Sa _ _ h₂))
            | exact ih.2 Θ S _ _ h
      · exact unifySpine_supply ih Θ S s₁ s₂ h

-- Base cases: one side exhausted ⟹ allVarsEmpty forces the other's vars to ε.
theorem unifySpineMF_nil_left_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (s₂ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF Θ S fuel [] s₂ = .success s S') (hsat : Sol.Sat θ s) :
    RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  simp only [unifySpineMF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst]
      exact (allVarsEmpty_sound s₂ hae (Sol.Sat_ofRow.mp hsat)).symm

theorem unifySpineMF_cons_nil_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (a : Atom B) (s₁ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF Θ S fuel (a :: s₁) [] = .success s S') (hsat : Sol.Sat θ s) :
    RowEquiv ((ofSpine (a :: s₁)).applySubst θ)
             ((ofSpine ([] : List (Atom B))).applySubst θ) := by
  simp only [unifySpineMF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst]
      exact allVarsEmpty_sound (a :: s₁) hae (Sol.Sat_ofRow.mp hsat)

-- ⊢  the base arm succeeds only on equal base types
theorem base_arm_sound {B : Type} [DecidableEq B] {θ : TySubst B} {b b' : B}
    {S : Supply} {fuel : Nat} {s : Sol B} {S' : Supply}
    (h : unifyTyF Θ S fuel (.base b) (.base b') = .success s S') :
    TyUnifies θ (.base b) (.base b') := by
  by_cases hb : b = b'
  · subst hb; exact TyEquiv.refl _
  · simp [unifyTyF, hb] at h

-- THE SOUNDNESS LEG, both sorts at once.
-- ⊢  unifyTyF Θ S fuel τ τ' = success s _,  θ ⊨ s   ⟹   θ ⊨ τ ≐ τ'
-- ⊢  unifySpineMF Θ S fuel s₁ s₂ = success s _,  θ ⊨ s
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem unifyM_success_sound {B : Type} [DecidableEq B] {θ : TySubst B} (fuel : Nat) :
    (∀ (Θ : DepGraph) (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
        unifyTyF Θ S fuel τ τ' = .success s S' → Sol.Sat θ s → TyUnifies θ τ τ') ∧
    (∀ (Θ : DepGraph) (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply},
        unifySpineMF Θ S fuel s₁ s₂ = .success s S' → Sol.Sat θ s →
        RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) := by
  induction fuel with
  | zero =>
      refine ⟨fun Θ S τ τ' s S' h hsat => ?_, fun Θ S s₁ s₂ s S' h hsat => ?_⟩
      · cases τ with
        | var α => exact bindTy_sound h hsat
        | base b =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base b' => exact base_arm_sound h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => exact TyEquiv.refl _
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
      · cases s₁ with
        | nil => exact unifySpineMF_nil_left_sound S 0 s₂ h hsat
        | cons a s₁ =>
          cases s₂ with
          | nil => exact unifySpineMF_cons_nil_sound S 0 a s₁ h hsat
          | cons b s₂ => cases h
  | succ fuel ih =>
      -- the shape every eq-emitting arm produces: split the composite, then
      -- undo the substitution the arm applied to the residual
      have arm : ∀ (Θ : DepGraph) (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B))
          {s : Sol B} {S' : Supply},
          ((unifyTyF Θ S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF Θ S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' → Sol.Sat θ s →
          TyUnifies θ τ τ' ∧
            RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
        intro Θ S τ τ' t₁ t₂ s S' h hsat
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        obtain ⟨h₁, h₂⟩ := hsat.comp_inv
        exact ⟨ih.1 Θ S τ τ' hty h₁,
               (unifies_sApplySubst_of_sat h₁ t₁ t₂).mp (ih.2 Θ S₁ _ _ hrow h₂)⟩
      refine ⟨fun Θ S τ τ' s S' h hsat => ?_, fun Θ S s₁ s₂ s S' h hsat => ?_⟩
      · cases τ with
        | var α => exact bindTy_sound h hsat
        | base b =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base b' => exact base_arm_sound h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => exact TyEquiv.refl _
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => cases h
            | fn a₂ b₂ =>
                replace h : ((unifyTyF Θ S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF Θ S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .success s S' := h
                obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
                obtain ⟨h₁, h₂⟩ := hsat.comp_inv
                exact TyEquiv.fn (ih.1 Θ S a₁ a₂ hty h₁)
                  ((tyUnifies_applySubst_of_sat h₁ b₁ b₂).mp (ih.1 Θ S₁ _ _ hrow h₂))
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF Θ S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
                exact TyEquiv.rcd
                  (((RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)).trans
                      (ih.2 Θ S _ _ h hsat)).trans
                    (RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)).symm)
      · cases s₁ with
        | nil => exact unifySpineMF_nil_left_sound S (fuel + 1) s₂ h hsat
        | cons a s₁ =>
          cases s₂ with
          | nil => exact unifySpineMF_cons_nil_sound S (fuel + 1) a s₁ h hsat
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact stripL_reflect hsl (ih.2 Θ S t₁ t₂ h hsat)
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact stripR_reflect hsr (ih.2 Θ S t₁ t₂ h hsat)
            | none =>
            cases hv1 : solveVarM Θ S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_reflect (hv1.trans (congrArg some h)) hsat
            | none =>
            cases hv2 : solveVarM Θ S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact (solveVarM_reflect (hv2.trans (congrArg some h)) hsat).symm
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              obtain ⟨he, hr⟩ := arm Θ S τ0 τ0' t₁ t₂ h hsat
              exact matchL_reflect hml he hr
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              obtain ⟨he, hr⟩ := arm Θ S τ0 τ0' t₁ t₂ h hsat
              exact (matchL_reflect hml2 he.symm hr.symm).symm
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              obtain ⟨he, hr⟩ := arm Θ S τ0 τ0' t₁ t₂ h hsat
              exact matchR_reflect hmr he hr
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              obtain ⟨he, hr⟩ := arm Θ S τ0 τ0' t₁ t₂ h hsat
              exact (matchR_reflect hmr2 he.symm hr.symm).symm
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              obtain ⟨he, hr⟩ := arm Θ S τ0 τ0' t₁ t₂ h hsat
              exact groundMatch_reflect hg he hr
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              obtain ⟨he, hr⟩ := arm Θ S τ0 τ0' t₁ t₂ h hsat
              exact (groundMatch_reflect hg2 he.symm hr.symm).symm
            | none =>
            cases he1 : expandL Θ S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              obtain ⟨s', hrec, rfl⟩ := expandResM_success h
              obtain ⟨h₀, h'⟩ := hsat.comp_inv
              obtain ⟨hs1, hshape, hren⟩ := expandL_spec he1
              rw [hs1]
              refine expand_reflect hshape
                (h₀.2 _ List.mem_cons_self) (h₀.1 _ List.mem_cons_self).symm ?_
              rw [← hren]
              exact ih.2 (expandDeps Θ S β0 τ0) S.fresh.2.fresh.2 t₁ t₂ hrec h'
            | none =>
            cases he2 : expandL Θ S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              obtain ⟨s', hrec, rfl⟩ := expandResM_success h
              obtain ⟨h₀, h'⟩ := hsat.comp_inv
              obtain ⟨hs2, hshape, hren⟩ := expandL_spec he2
              rw [hs2]
              refine (expand_reflect hshape
                (h₀.2 _ List.mem_cons_self) (h₀.1 _ List.mem_cons_self).symm ?_).symm
              rw [← hren]
              exact ih.2 (expandDeps Θ S β0 τ0) S.fresh.2.fresh.2 t₁ t₂ hrec h'
            | none =>
            cases hpc : projClash (a :: s₁) (b :: s₂) with
            | true =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
                hpc] at h
              cases h
            | false =>
            cases he3 : expandR Θ S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
                hpc, he3] at h
              obtain ⟨s', hrec, rfl⟩ := expandResRM_success h
              obtain ⟨h₀, h'⟩ := hsat.comp_inv
              obtain ⟨hs1, hshape, hren⟩ := expandR_spec he3
              rw [hs1]
              refine expandR_reflect' hshape
                (h₀.2 _ List.mem_cons_self) (h₀.1 _ List.mem_cons_self).symm ?_
              rw [← hren]
              exact ih.2 (expandDeps Θ S β0 τ0) S.fresh.2.fresh.2 t₁ t₂ hrec h'
            | none =>
            cases he4 : expandR Θ S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
                hpc, he3, he4] at h
              obtain ⟨s', hrec, rfl⟩ := expandResRM_success h
              obtain ⟨h₀, h'⟩ := hsat.comp_inv
              obtain ⟨hs2, hshape, hren⟩ := expandR_spec he4
              rw [hs2]
              refine (expandR_reflect' hshape
                (h₀.2 _ List.mem_cons_self) (h₀.1 _ List.mem_cons_self).symm ?_).symm
              rw [← hren]
              exact ih.2 (expandDeps Θ S β0 τ0) S.fresh.2.fresh.2 t₁ t₂ hrec h'
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2,
                hpc, he3, he4] at h
              cases h

-- The ≐ᵣ success case is SOUND under the mutual driver, with NO residual
-- equations: the solution is the whole story.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = success s _,  θ ⊨ s   ⟹   θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRowM_success_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    {fuel : Nat} {ρ₁ ρ₂ : Row B} {s : Sol B} {S' : Supply}
    (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') (hsat : Sol.Sat θ s) :
    Unifies θ ρ₁ ρ₂ := by
  unfold unifyRowM unifySpineM at h
  have key := (unifyM_success_sound fuel).2 _ _ ρ₁.toSpine ρ₂.toSpine h hsat
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.trans (key.trans e₂.symm)

-- … and so is ≐ itself.
-- ⊢  unifyTyM fuel τ τ' = success s _,  θ ⊨ s   ⟹   θ ⊨ τ ≐ τ'
theorem unifyTyM_success_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    {fuel : Nat} {τ τ' : Ty B} {s : Sol B} {S' : Supply}
    (h : unifyTyM fuel τ τ' = .success s S') (hsat : Sol.Sat θ s) :
    TyUnifies θ τ τ' :=
  (unifyM_success_sound fuel).1 _ _ τ τ' h hsat



------------------ P5: WHERE A SOLUTION'S VARIABLES LIVE --------------------
-- An eq-emitting arm recurses on the SUBSTITUTED residual, so the freshness
-- invariant `S.Avoids …` does not transport by "the residual is a sub-problem":
-- the substitution can put names into the residual that the original problem
-- never had — precisely the ones a nested U-expand invented.
-- To carry the invariant across such an arm one has to know that those names
-- are bounded by the supply the sub-call RETURNED. That is what this section
-- sets up: where the variables of a substituted term come from, and what a
-- solution is allowed to mention.

-- ## Substitution moves variables only along θ
-- γ survives into τθ only through some free variable of τ. (Both sorts are
-- allowed on the right: Ty.ftv does not record which positions are row
-- positions, and the union is all the bound we need.)
mutual
theorem Ty.ftv_applySubst {B : Type} (θ : TySubst B) : (τ : Ty B) →
    ∀ γ, γ ∈ (τ.applySubst θ).ftv →
      ∃ α, α ∈ τ.ftv ∧ (γ ∈ (θ.ty α).ftv ∨ γ ∈ (θ.row α).ftv)
  | .var α => fun _ h => ⟨α, List.mem_cons_self, .inl h⟩
  | .base _ => fun _ h => by simp [Ty.applySubst, Ty.ftv] at h
  | .unk => fun _ h => by simp [Ty.applySubst, Ty.ftv] at h
  | .fn τ₁ τ₂ => fun γ h => by
      simp only [Ty.applySubst, Ty.ftv, List.mem_append] at h ⊢
      rcases h with h | h
      · obtain ⟨α, hα, hγ⟩ := Ty.ftv_applySubst θ τ₁ γ h; exact ⟨α, .inl hα, hγ⟩
      · obtain ⟨α, hα, hγ⟩ := Ty.ftv_applySubst θ τ₂ γ h; exact ⟨α, .inr hα, hγ⟩
  | .rcd ρ => fun γ h => Row.ftv_applySubst θ ρ γ h

theorem Row.ftv_applySubst {B : Type} (θ : TySubst B) : (ρ : Row B) →
    ∀ γ, γ ∈ (ρ.applySubst θ).ftv →
      ∃ α, α ∈ ρ.ftv ∧ (γ ∈ (θ.ty α).ftv ∨ γ ∈ (θ.row α).ftv)
  | .empty => fun _ h => by simp [Row.applySubst, Row.ftv] at h
  | .var α => fun _ h => ⟨α, List.mem_cons_self, .inr h⟩
  | .sing _ τ => fun γ h => Ty.ftv_applySubst θ τ γ h
  | .cat ρ₁ ρ₂ => fun γ h => by
      simp only [Row.applySubst, Row.ftv, List.mem_append] at h ⊢
      rcases h with h | h
      · obtain ⟨α, hα, hγ⟩ := Row.ftv_applySubst θ ρ₁ γ h; exact ⟨α, .inl hα, hγ⟩
      · obtain ⟨α, hα, hγ⟩ := Row.ftv_applySubst θ ρ₂ γ h; exact ⟨α, .inr hα, hγ⟩
end

-- ⊢  a row and its spine have the same variables
theorem mem_sFtv_toSpine {B : Type} : (ρ : Row B) → ∀ γ, (γ ∈ sFtv ρ.toSpine ↔ γ ∈ ρ.ftv)
  | .empty => fun _ => Iff.rfl
  | .var _ => fun _ => Iff.rfl
  | .sing _ τ => fun γ => by
      simp only [Row.toSpine, sFtv, Row.ftv, List.append_nil]
  | .cat ρ₁ ρ₂ => fun γ => by
      rw [Row.toSpine, sFtv_append, Row.ftv]
      simp only [List.mem_append, mem_sFtv_toSpine ρ₁ γ, mem_sFtv_toSpine ρ₂ γ]

-- ⊢  … and the same, for a substituted SPINE
theorem sFtv_sApplySubst {B : Type} (θ : TySubst B) : (t : List (Atom B)) →
    ∀ γ, γ ∈ sFtv (sApplySubst θ t) →
      ∃ α, α ∈ sFtv t ∧ (γ ∈ (θ.ty α).ftv ∨ γ ∈ (θ.row α).ftv)
  | [] => fun _ h => by simp [sApplySubst, sFtv] at h
  | .field _ τ :: t => fun γ h => by
      simp only [sApplySubst, sFtv, List.mem_append] at h ⊢
      rcases h with h | h
      · obtain ⟨α, hα, hγ⟩ := Ty.ftv_applySubst θ τ γ h; exact ⟨α, .inl hα, hγ⟩
      · obtain ⟨α, hα, hγ⟩ := sFtv_sApplySubst θ t γ h; exact ⟨α, .inr hα, hγ⟩
  | .var α :: t => fun γ h => by
      rw [sApplySubst, sFtv_append] at h
      rcases List.mem_append.mp h with h | h
      · exact ⟨α, List.mem_cons_self, .inr ((mem_sFtv_toSpine _ γ).mp h)⟩
      · obtain ⟨β, hβ, hγ⟩ := sFtv_sApplySubst θ t γ h
        exact ⟨β, List.mem_cons_of_mem _ hβ, hγ⟩


end MinimalCalculus
