-- P6: the mgu statement and the stuck leg, plus the NEXT roadmap.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Clash

namespace MinimalCalculus

-- ## The mgu statement, in one place
-- Soundness and completeness together: a success DESCRIBES the unifier set.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = success s _   ⟹
--      every θ meeting s unifies ρ₁, ρ₂,  and every unifier of ρ₁, ρ₂ extends
--      (without moving on the problem's own variables) to one meeting s
theorem unifyRowM_success_iff {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    (∀ θ : TySubst B, Sol.Sat θ s → Unifies θ ρ₁ ρ₂) ∧
    (∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ → ∃ θ' : TySubst B,
        AgreeOn θ θ' (sFtv ρ₁.toSpine ++ sFtv ρ₂.toSpine) ∧ Sol.Sat θ' s) :=
  ⟨fun _ hsat => unifyRowM_success_sound h hsat,
   fun _ hu => unifyRowM_success_complete h hu⟩


------------------ P6: THE BASE-ARM DISPATCH, STEP 2 ------------------------
-- proof-plan.md §3 / §4-P6. `hbase` — "a terminal stuck configuration has no
-- mgu" — is the one hypothesis the trichotomy still reduces to. Step 1 of its
-- dispatch is stuck_leading_shape (:4078): with stripL and both matchL
-- directions dead, the two leading atoms take one of four shapes. Step 2 is
-- here, and it is the payoff of P3b: a terminal configuration ALSO has both
-- U-expand directions dead, and that refusal is informative.
--
-- U-expand refuses for exactly two reasons (uniqueHost, :194): the host side has
-- no UNIQUE variable, or the label is already present there. So whenever a
-- leading field faces the other side, the terminal configuration is
--
--   * ≥ 2 candidate hosts — Wand's shape, killed by count-shrink
--     (vars_vs_field_no_mgu, :663), or
--   * the label already occurs on the other side, necessarily BEHIND a variable
--     (matchL is dead, so it is not in the window) — the two-sided shape, killed
--     by rigidity (two_sided_no_mgu, :469).
--
-- That is exactly proof-plan.md §1.4's claim ("this shrinks the stuck class to
-- precisely what the three base techniques can kill"), now mechanized. What
-- remains for hbase is step 3: running those two witnesses at the general shape
-- rather than at the canonical examples.

-- ⊢  uniqueHost refuses for exactly two reasons
theorem uniqueHost_none {B : Type} {l : Label} {s : List (Atom B)}
    (h : uniqueHost l s = none) :
    (∀ β, sVarSeq s ≠ [β]) ∨ 0 < sFieldCount l s := by
  unfold uniqueHost at h
  cases hvs : sVarSeq s with
  | nil => exact .inl (fun _ hb => by cases hb)
  | cons γ t =>
      cases t with
      | cons _ _ => exact .inl (fun _ hb => by cases hb)
      | nil =>
          right
          rw [hvs] at h
          simp only at h
          split at h
          · cases h
          · next hc => exact Nat.pos_of_ne_zero hc

-- ⊢  … and so does U-expand, when the leading atom IS a field
theorem expandL_none_field {B : Type} {S : Supply} {l : Label} {τ : Ty B}
    {t₁ s₂ : List (Atom B)} (h : expandL S (.field l τ :: t₁) s₂ = none) :
    (∀ β, sVarSeq s₂ ≠ [β]) ∨ 0 < sFieldCount l s₂ := by
  simp only [expandL] at h
  cases hh : uniqueHost l s₂ with
  | none => exact uniqueHost_none hh
  | some γ => rw [hh] at h; cases h

-- ⊢  "no unique variable" plus "at least one variable" means TWO candidate hosts
theorem two_vars_of_not_singleton {vs : List TyVar}
    (h : ∀ γ, vs ≠ [γ]) (hne : vs ≠ []) : 2 ≤ vs.length := by
  cases vs with
  | nil => exact absurd rfl hne
  | cons x t =>
      cases t with
      | nil => exact absurd rfl (h x)
      | cons y u => simp only [List.length_cons]; omega

theorem sVarSeq_var_cons {B : Type} (β : TyVar) (s : List (Atom B)) :
    sVarSeq (Atom.var β :: s) = β :: sVarSeq s := rfl

-- STEP 2 OF THE DISPATCH. Every shape stuck_leading_shape allows, refined by
-- what U-expand's refusal adds. Shape (1) — two distinct leading variables — is
-- the only one U-expand says nothing about; it is the non-commutativity
-- territory (allvar_swap_no_mgu, :801), where no leading field exists to host.
theorem stuck_leading_shape_expand {B : Type} {S : Supply} {a b : Atom B}
    {s₁ s₂ : List (Atom B)}
    (hsl : stripL (a :: s₁) (b :: s₂) = none)
    (hml : matchL (a :: s₁) (b :: s₂) = none)
    (hml2 : matchL (b :: s₂) (a :: s₁) = none)
    (he1 : expandL S (a :: s₁) (b :: s₂) = none)
    (he2 : expandL S (b :: s₂) (a :: s₁) = none) :
    (∃ α β, a = .var α ∧ b = .var β ∧ α ≠ β) ∨
    (∃ α l' τ', a = .var α ∧ b = .field l' τ' ∧
       ((∀ γ, sVarSeq (a :: s₁) ≠ [γ]) ∨ 0 < sFieldCount l' (a :: s₁))) ∨
    (∃ l τ β, a = .field l τ ∧ b = .var β ∧
       ((∀ γ, sVarSeq (b :: s₂) ≠ [γ]) ∨ 0 < sFieldCount l (b :: s₂))) ∨
    (∃ l τ l' τ', a = .field l τ ∧ b = .field l' τ' ∧ l ≠ l' ∧
      windowExtract l (b :: s₂) = none ∧ windowExtract l' (a :: s₁) = none ∧
      ((∀ γ, sVarSeq (b :: s₂) ≠ [γ]) ∨ 0 < sFieldCount l (b :: s₂)) ∧
      ((∀ γ, sVarSeq (a :: s₁) ≠ [γ]) ∨ 0 < sFieldCount l' (a :: s₁))) := by
  rcases stuck_leading_shape hsl hml hml2 with
    ⟨α, β, ha, hb, hne⟩ | ⟨α, l', τ', ha, hb⟩ | ⟨l, τ, β, ha, hb⟩
    | ⟨l, τ, l', τ', ha, hb, hlne, hw1, hw2⟩
  · exact .inl ⟨α, β, ha, hb, hne⟩
  · subst ha; subst hb
    exact .inr (.inl ⟨α, l', τ', rfl, rfl, expandL_none_field he2⟩)
  · subst ha; subst hb
    exact .inr (.inr (.inl ⟨l, τ, β, rfl, rfl, expandL_none_field he1⟩))
  · subst ha; subst hb
    exact .inr (.inr (.inr ⟨l, τ, l', τ', rfl, rfl, hlne, hw1, hw2,
      expandL_none_field he1, expandL_none_field he2⟩))

-- The reading of shape (3), spelled out: a leading field facing a leading
-- VARIABLE leaves exactly the two configurations the base techniques handle.
-- ⊢  (l:τ | s₁) ≐ᵣ (β | s₂) terminal  ⟹
--      the right side has ≥ 2 variables (count-shrink), or it already carries
--      an l-field — necessarily behind a variable (rigidity)
theorem stuck_field_vs_var {B : Type} {S : Supply} {l : Label} {τ : Ty B}
    {β : TyVar} {s₁ s₂ : List (Atom B)}
    (he1 : expandL S (.field l τ :: s₁) (.var β :: s₂) = none) :
    2 ≤ (sVarSeq (Atom.var β :: s₂)).length ∨ 0 < sFieldCount l (Atom.var β :: s₂) := by
  rcases expandL_none_field he1 with h | h
  · exact .inl (two_vars_of_not_singleton h (by rw [sVarSeq_var_cons]; exact fun hc => by cases hc))
  · exact .inr h



------------------ P6: STUCK ⟹ NO-MGU, MUTUALLY ----------------------------
-- The fourth leg on the mutual driver, still stated HONESTLY as a REDUCTION.
-- One thing gets strictly better and one thing gets harder.
--
-- BETTER: no fuel guard. `outOfFuel` is its own verdict now, so `.stuck` can
-- never be a budget artefact and the induction needs no `|s₁|+|s₂| ≤ fuel`
-- premise — the old statement's (:4143) most awkward hypothesis is gone.
--
-- HARDER: an eq-emitting arm has TWO ways to be stuck, and the second is new.
-- Either the type sub-call is stuck — handled here, by the ≐ half of the same
-- induction — or it SUCCEEDED and the substituted residual is stuck. The second
-- is not a pointwise iff: relating the substituted residual's unifiers to the
-- original's needs `Sol.Sat θ s₁`, which only an EXTENSION of θ satisfies. So it
-- is parked as `hsolve` / `hsolveTy`, alongside `hbase` and `hexp`.
--
-- The root obstruction behind all three parked hypotheses is now identified and
-- recorded in proof-plan.md §4-P6: `HasMgu`, defined with strict `InstanceOf`
-- over ALL variables, is the wrong notion for an algorithm that invents
-- variables — the same mismatch `AgreeOn` fixed for completeness in §4-P3b(2),
-- one level up. Relativizing it is P6's first task; these four hypotheses are
-- what it has to discharge.

-- ## ≐'s congruence arms are pointwise iffs (so they need no hypothesis)
theorem tyUnifies_fn_iff {B : Type} (θ : TySubst B) (a₁ b₁ a₂ b₂ : Ty B) :
    TyUnifies θ (.fn a₁ b₁) (.fn a₂ b₂) ↔ (TyUnifies θ a₁ a₂ ∧ TyUnifies θ b₁ b₂) := by
  constructor
  · intro h
    obtain ⟨σ₁, σ₂, heq, hA, hB⟩ := TyEquiv.fn_inv
      (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from h)
    simp only [Ty.applySubst, Ty.fn.injEq] at heq
    obtain ⟨rfl, rfl⟩ := heq
    exact ⟨hA, hB⟩
  · exact fun ⟨hA, hB⟩ => TyEquiv.fn hA hB

theorem tyUnifies_rcd_iff {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) :
    TyUnifies θ (.rcd ρ₁) (.rcd ρ₂) ↔ Unifies θ ρ₁ ρ₂ := by
  constructor
  · intro h
    obtain ⟨ρ', heq, hR⟩ :=
      TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from h)
    simp only [Ty.applySubst, Ty.rcd.injEq] at heq
    obtain rfl := heq
    exact hR
  · exact fun h => TyEquiv.rcd h

theorem unifies_toSpine_iff {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) :
    Unifies θ (ofSpine ρ₁.toSpine) (ofSpine ρ₂.toSpine) ↔ Unifies θ ρ₁ ρ₂ := by
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact ⟨fun h => e₁.trans (h.trans e₂.symm), fun h => e₁.symm.trans (h.trans e₂)⟩

-- ## Which arms can answer `stuck` at all
theorem bindTy_ne_stuck {B : Type} {S : Supply} {α : TyVar} {τ : Ty B} :
    bindTy S α τ ≠ .stuck := by
  intro h; unfold bindTy at h
  split at h
  · cases h
  · split at h <;> cases h

theorem solveVarM_ne_stuck {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)} :
    solveVarM S s₁ s₂ ≠ some .stuck := by
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

theorem expandResM_stuck {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} (h : expandResM S β l τ r = .stuck) : r = .stuck := by
  cases r with
  | success _ _ => cases h
  | clash => cases h
  | occurs => cases h
  | stuck => rfl
  | outOfFuel => cases h

theorem UResM.seq_stuck {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (h : r.seq k = .stuck) :
    r = .stuck ∨ ∃ s S, r = .success s S ∧ k s.toSubst S = .stuck := by
  cases r with
  | success s S =>
      refine .inr ⟨s, S, rfl, ?_⟩
      simp only [UResM.seq] at h
      revert h; cases hk : k s.toSubst S with
      | success s' S' => intro h; cases h
      | clash => intro h; cases h
      | occurs => intro h; cases h
      | stuck => intro _; rfl
      | outOfFuel => intro h; cases h
  | clash => cases h
  | occurs => cases h
  | stuck => exact .inl rfl
  | outOfFuel => cases h

-- THE STUCK LEG, both sorts at once, as a reduction to four named hypotheses.
theorem unifyM_stuck_no_mgu {B : Type} [DecidableEq B]
    (hbase : ∀ (S : Supply) (a : Atom B) (s₁ : List (Atom B)) (b : Atom B)
              (s₂ : List (Atom B)) (Q : TySubst B → Prop),
      stripL (a :: s₁) (b :: s₂) = none → stripR (a :: s₁) (b :: s₂) = none →
      solveVarM S (a :: s₁) (b :: s₂) = none → solveVarM S (b :: s₂) (a :: s₁) = none →
      matchL (a :: s₁) (b :: s₂) = none → matchL (b :: s₂) (a :: s₁) = none →
      matchR (a :: s₁) (b :: s₂) = none → matchR (b :: s₂) (a :: s₁) = none →
      groundMatch (a :: s₁) (b :: s₂) = none → groundMatch (b :: s₂) (a :: s₁) = none →
      expandL S (a :: s₁) (b :: s₂) = none → expandL S (b :: s₂) (a :: s₁) = none →
      projClash (a :: s₁) (b :: s₂) = false →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine (a :: s₁)) (ofSpine (b :: s₂)) ∧ Q θ))
    (hexp : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) (Q : TySubst B → Prop)
              (β : TyVar) (l : Label) (τ : Ty B) (t₁ t₂ : List (Atom B)),
      expandL S u₁ u₂ = some (β, l, τ, t₁, t₂) →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ) →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine u₁) (ofSpine u₂) ∧ Q θ))
    (hsolve : ∀ (s : Sol B) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (Q : TySubst B → Prop),
      ¬ HasMguP (fun θ => Unifies θ (ofSpine (sApplySubst s.toSubst t₁))
                                    (ofSpine (sApplySubst s.toSubst t₂)) ∧ Q θ) →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                          (TyUnifies θ τ τ' ∧ Q θ)))
    (hsolveTy : ∀ (s : Sol B) (a₁ a₂ b₁ b₂ : Ty B) (Q : TySubst B → Prop),
      ¬ HasMguP (fun θ => TyUnifies θ (b₁.applySubst s.toSubst)
                                      (b₂.applySubst s.toSubst) ∧ Q θ) →
      ¬ HasMguP (fun θ => TyUnifies θ b₁ b₂ ∧ (TyUnifies θ a₁ a₂ ∧ Q θ))) :
    ∀ (fuel : Nat),
      (∀ (S : Supply) (τ τ' : Ty B) (Q : TySubst B → Prop),
        unifyTyF S fuel τ τ' = .stuck →
        ¬ HasMguP (fun θ => TyUnifies θ τ τ' ∧ Q θ)) ∧
      (∀ (S : Supply) (s₁ s₂ : List (Atom B)) (Q : TySubst B → Prop),
        unifySpineMF S fuel s₁ s₂ = .stuck →
        ¬ HasMguP (fun θ => Unifies θ (ofSpine s₁) (ofSpine s₂) ∧ Q θ)) := by
  intro fuel
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' Q h => ?_, fun S s₁ s₂ Q h => ?_⟩
      · cases τ with
        | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := τ'))
        | base b =>
            cases τ' with
            | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := Ty.base b))
            | base b' => by_cases hb : b = b' <;> simp [unifyTyF, hb] at h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α =>
                exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := (Ty.unk : Ty B)))
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn c d =>
            cases τ' with
            | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := Ty.fn c d))
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | rcd ρ =>
            cases τ' with
            | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := Ty.rcd ρ))
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => rw [hae] at h; simp at h
            | some σ => rw [hae] at h; simp at h
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rw [hae] at h; simp at h
              | some σ => rw [hae] at h; simp at h
          | cons b s₂ => cases h
  | succ fuel ih =>
      -- the shape every eq-emitting arm produces, at both stuck sources
      have armStuck : ∀ (S : Supply) (τ0 τ0' : Ty B) (t₁ t₂ u₁ u₂ : List (Atom B))
          (Q : TySubst B → Prop),
          ((unifyTyF S fuel τ0 τ0').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂)) = .stuck →
          (∀ θ : TySubst B, Unifies θ (ofSpine u₁) (ofSpine u₂) →
              TyUnifies θ τ0 τ0' ∧ Unifies θ (ofSpine t₁) (ofSpine t₂)) →
          (∀ θ : TySubst B, TyUnifies θ τ0 τ0' →
              Unifies θ (ofSpine t₁) (ofSpine t₂) → Unifies θ (ofSpine u₁) (ofSpine u₂)) →
          ¬ HasMguP (fun θ => Unifies θ (ofSpine u₁) (ofSpine u₂) ∧ Q θ) := by
        intro S τ0 τ0' t₁ t₂ u₁ u₂ Q h hfwd hbwd
        rcases UResM.seq_stuck h with hc | ⟨s, S₁, hs, hk⟩
        · refine hasMguP_not_of_iff (P' := fun θ =>
              TyUnifies θ τ0 τ0' ∧ (Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ)) ?_
              (ih.1 S τ0 τ0' _ hc)
          exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨(hfwd θ hu).1, (hfwd θ hu).2, hq⟩,
                          fun ⟨he, hr, hq⟩ => ⟨hbwd θ he hr, hq⟩⟩
        · refine hasMguP_not_of_iff (P' := fun θ =>
              Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ (TyUnifies θ τ0 τ0' ∧ Q θ)) ?_
              (hsolve s τ0 τ0' t₁ t₂ Q (ih.2 S₁ _ _ _ hk))
          exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨(hfwd θ hu).2, (hfwd θ hu).1, hq⟩,
                          fun ⟨hr, he, hq⟩ => ⟨hbwd θ he hr, hq⟩⟩
      refine ⟨fun S τ τ' Q h => ?_, fun S s₁ s₂ Q h => ?_⟩
      · cases τ with
        | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := τ'))
        | base b =>
            cases τ' with
            | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := Ty.base b))
            | base b' => by_cases hb : b = b' <;> simp [unifyTyF, hb] at h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α =>
                exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := (Ty.unk : Ty B)))
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := Ty.fn a₁ b₁))
            | base _ => cases h
            | unk => cases h
            | fn a₂ b₂ =>
                replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .stuck := h
                rcases UResM.seq_stuck h with hc | ⟨s, S₁, hs, hk⟩
                · refine hasMguP_not_of_iff (P' := fun θ =>
                      TyUnifies θ a₁ a₂ ∧ (TyUnifies θ b₁ b₂ ∧ Q θ)) ?_
                      (ih.1 S a₁ a₂ _ hc)
                  exact fun θ => ⟨fun ⟨hu, hq⟩ =>
                      ⟨((tyUnifies_fn_iff θ a₁ b₁ a₂ b₂).mp hu).1,
                       ((tyUnifies_fn_iff θ a₁ b₁ a₂ b₂).mp hu).2, hq⟩,
                    fun ⟨hA, hB, hq⟩ => ⟨(tyUnifies_fn_iff θ a₁ b₁ a₂ b₂).mpr ⟨hA, hB⟩, hq⟩⟩
                · refine hasMguP_not_of_iff (P' := fun θ =>
                      TyUnifies θ b₁ b₂ ∧ (TyUnifies θ a₁ a₂ ∧ Q θ)) ?_
                      (hsolveTy s a₁ a₂ b₁ b₂ Q (ih.1 S₁ _ _ _ hk))
                  exact fun θ => ⟨fun ⟨hu, hq⟩ =>
                      ⟨((tyUnifies_fn_iff θ a₁ b₁ a₂ b₂).mp hu).2,
                       ((tyUnifies_fn_iff θ a₁ b₁ a₂ b₂).mp hu).1, hq⟩,
                    fun ⟨hB, hA, hq⟩ => ⟨(tyUnifies_fn_iff θ a₁ b₁ a₂ b₂).mpr ⟨hA, hB⟩, hq⟩⟩
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact absurd h (bindTy_ne_stuck (S := S) (α := α) (τ := Ty.rcd ρ₁))
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .stuck := h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine ρ₁.toSpine) (ofSpine ρ₂.toSpine) ∧ Q θ) ?_
                    (ih.2 S _ _ Q h)
                exact fun θ => ⟨fun ⟨hu, hq⟩ =>
                    ⟨(unifies_toSpine_iff θ ρ₁ ρ₂).mpr ((tyUnifies_rcd_iff θ ρ₁ ρ₂).mp hu), hq⟩,
                  fun ⟨hu, hq⟩ =>
                    ⟨(tyUnifies_rcd_iff θ ρ₁ ρ₂).mpr ((unifies_toSpine_iff θ ρ₁ ρ₂).mp hu), hq⟩⟩
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => rw [hae] at h; simp at h
            | some σ => rw [hae] at h; simp at h
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rw [hae] at h; simp at h
              | some σ => rw [hae] at h; simp at h
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              refine hasMguP_not_of_iff (P' := fun θ =>
                  Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ) ?_ (ih.2 S t₁ t₂ Q h)
              exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨stripL_reflect_fwd hsl hu, hq⟩,
                              fun ⟨hu, hq⟩ => ⟨stripL_reflect hsl hu, hq⟩⟩
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              refine hasMguP_not_of_iff (P' := fun θ =>
                  Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ) ?_ (ih.2 S t₁ t₂ Q h)
              exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨stripR_reflect_fwd hsr hu, hq⟩,
                              fun ⟨hu, hq⟩ => ⟨stripR_reflect hsr hu, hq⟩⟩
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact absurd (hv1.trans (congrArg some h)) solveVarM_ne_stuck
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact absurd (hv2.trans (congrArg some h)) solveVarM_ne_stuck
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              exact armStuck S τ0 τ0' t₁ t₂ (a :: s₁) (b :: s₂) Q h
                (fun _ hu => matchL_reflect_fwd hml hu)
                (fun _ he hr => matchL_reflect hml he hr)
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              exact armStuck S τ0 τ0' t₁ t₂ (a :: s₁) (b :: s₂) Q h
                (fun _ hu => let ⟨he, hr⟩ := matchL_reflect_fwd hml2 hu.symm; ⟨he.symm, hr.symm⟩)
                (fun _ he hr => (matchL_reflect hml2 he.symm hr.symm).symm)
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              exact armStuck S τ0 τ0' t₁ t₂ (a :: s₁) (b :: s₂) Q h
                (fun _ hu => matchR_reflect_fwd hmr hu)
                (fun _ he hr => matchR_reflect hmr he hr)
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              exact armStuck S τ0 τ0' t₁ t₂ (a :: s₁) (b :: s₂) Q h
                (fun _ hu => let ⟨he, hr⟩ := matchR_reflect_fwd hmr2 hu.symm; ⟨he.symm, hr.symm⟩)
                (fun _ he hr => (matchR_reflect hmr2 he.symm hr.symm).symm)
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              exact armStuck S τ0 τ0' t₁ t₂ (a :: s₁) (b :: s₂) Q h
                (fun _ hu => groundMatch_reflect_fwd hg hu)
                (fun _ he hr => groundMatch_reflect hg he hr)
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              exact armStuck S τ0 τ0' t₁ t₂ (a :: s₁) (b :: s₂) Q h
                (fun _ hu =>
                  let ⟨he, hr⟩ := groundMatch_reflect_fwd hg2 hu.symm; ⟨he.symm, hr.symm⟩)
                (fun _ he hr => (groundMatch_reflect hg2 he.symm hr.symm).symm)
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact hexp S (a :: s₁) (b :: s₂) Q β0 l0 τ0 t₁ t₂ he1
                (ih.2 S.fresh.2.fresh.2 t₁ t₂ Q (expandResM_stuck h))
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              refine hasMguP_not_of_iff (P' := fun θ =>
                  Unifies θ (ofSpine (b :: s₂)) (ofSpine (a :: s₁)) ∧ Q θ) ?_
                  (hexp S (b :: s₂) (a :: s₁) Q β0 l0 τ0 t₁ t₂ he2
                    (ih.2 S.fresh.2.fresh.2 t₁ t₂ Q (expandResM_stuck h)))
              exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨hu.symm, hq⟩,
                              fun ⟨hu, hq⟩ => ⟨hu.symm, hq⟩⟩
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h
              · cases h
              · rename_i hpc
                exact hbase S a s₁ b s₂ Q hsl hsr hv1 hv2 hml hml2 hmr hmr2 hg hg2
                  he1 he2 (by simpa using hpc)


-- Row-level reduction on the mutual driver. Note there is NO fuel premise: with
-- `outOfFuel` split out, a `.stuck` verdict is a genuine terminal ambiguity at
-- whatever budget it was reached — the old statement (`unifyRow_stuck_no_mgu`)
-- had to carry `|s₁|+|s₂| ≤ fuel` to say that.
-- ⊢  (hbase, hexp, hsolve, hsolveTy)  →  unifyRowM fuel ρ₁ ρ₂ = stuck
--        →  ¬ HasMgu ρ₁ ρ₂
theorem unifyRowM_stuck_no_mgu {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    (hbase : ∀ (S : Supply) (a : Atom B) (s₁ : List (Atom B)) (b : Atom B)
              (s₂ : List (Atom B)) (Q : TySubst B → Prop),
      stripL (a :: s₁) (b :: s₂) = none → stripR (a :: s₁) (b :: s₂) = none →
      solveVarM S (a :: s₁) (b :: s₂) = none → solveVarM S (b :: s₂) (a :: s₁) = none →
      matchL (a :: s₁) (b :: s₂) = none → matchL (b :: s₂) (a :: s₁) = none →
      matchR (a :: s₁) (b :: s₂) = none → matchR (b :: s₂) (a :: s₁) = none →
      groundMatch (a :: s₁) (b :: s₂) = none → groundMatch (b :: s₂) (a :: s₁) = none →
      expandL S (a :: s₁) (b :: s₂) = none → expandL S (b :: s₂) (a :: s₁) = none →
      projClash (a :: s₁) (b :: s₂) = false →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine (a :: s₁)) (ofSpine (b :: s₂)) ∧ Q θ))
    (hexp : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) (Q : TySubst B → Prop)
              (β : TyVar) (l : Label) (τ : Ty B) (t₁ t₂ : List (Atom B)),
      expandL S u₁ u₂ = some (β, l, τ, t₁, t₂) →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ) →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine u₁) (ofSpine u₂) ∧ Q θ))
    (hsolve : ∀ (s : Sol B) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (Q : TySubst B → Prop),
      ¬ HasMguP (fun θ => Unifies θ (ofSpine (sApplySubst s.toSubst t₁))
                                    (ofSpine (sApplySubst s.toSubst t₂)) ∧ Q θ) →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                          (TyUnifies θ τ τ' ∧ Q θ)))
    (hsolveTy : ∀ (s : Sol B) (a₁ a₂ b₁ b₂ : Ty B) (Q : TySubst B → Prop),
      ¬ HasMguP (fun θ => TyUnifies θ (b₁.applySubst s.toSubst)
                                      (b₂.applySubst s.toSubst) ∧ Q θ) →
      ¬ HasMguP (fun θ => TyUnifies θ b₁ b₂ ∧ (TyUnifies θ a₁ a₂ ∧ Q θ)))
    (h : unifyRowM fuel ρ₁ ρ₂ = .stuck) : ¬ HasMgu ρ₁ ρ₂ := by
  intro hmgu
  unfold unifyRowM unifySpineM at h
  refine (unifyM_stuck_no_mgu hbase hexp hsolve hsolveTy fuel).2
    _ ρ₁.toSpine ρ₂.toSpine (fun _ => True) h ?_
  rw [hasMgu_eq_hasMguP] at hmgu
  refine (hasMguP_congr (fun θ => ?_)).mp hmgu
  exact ⟨fun hu => ⟨(unifies_toSpine_iff θ ρ₁ ρ₂).mpr hu, trivial⟩,
         fun ⟨hu, _⟩ => (unifies_toSpine_iff θ ρ₁ ρ₂).mp hu⟩



------------------------------------ NEXT ------------------------------------
-- WHERE ≐ / ≐ᵣ STANDS (proof-plan.md is the live plan; §4 records each phase).
--
-- The algorithm is `unifyTyF` / `unifySpineMF` — one mutual block, structurally
-- recursive on fuel, so every worked example is a kernel-checked `rfl`
-- (Regressions.lean). Five verdicts: success / clash / occurs / stuck /
-- outOfFuel. Three of the four legs are THEOREMS, one is a reduction:
--
--  * SUCCESS SOUNDNESS — unifyM_success_sound / unifyRowM_/unifyTyM_. A θ that
--    meets the solution unifies the problem. No residual equations: the driver
--    solves them. Any fuel.
--  * SUCCESS COMPLETENESS — unifyM_success_complete, in the ∃θ′/AgreeOn form
--    (a unifier cannot constrain names the run invented). With soundness:
--    unifyRowM_success_iff — the solution DESCRIBES the unifier set, i.e. ≐ᵣ
--    computes an mgu. Any fuel.
--  * CLASH — unifyM_clash_no_unifier, including a clash found inside a field
--    type; tyClash_dispatch refutes all twelve head mismatches at once. Any fuel.
--  * STUCK ⟹ NO-MGU — unifyM_stuck_no_mgu, a REDUCTION to four named
--    hypotheses: hbase (a terminal row configuration has no mgu), hexp (the
--    U-expand arm), hsolve/hsolveTy (the solve-and-apply arms). No fuel premise:
--    `outOfFuel` is a separate verdict, so `.stuck` is never a budget artefact.
--
-- Supporting invariants: unifyM_fuel_mono (a verdict that was REACHED never
-- changes when the budget grows — this replaces a termination measure, see §1.3)
-- and unifyM_bounded (a run only mentions names below the supply it returns —
-- the freshness invariant solve-and-apply forced).
--
-- OPEN, in the order the plan wants them:
--  * `HasMguOn V` / `InstanceOfOn V` — mgu RELATIVIZED to a variable set. The
--    strict `InstanceOf` over all variables is the wrong notion for an algorithm
--    that invents variables (the mismatch AgreeOn fixed for completeness, one
--    level up), and it is what blocks all four parked hypotheses above.
--  * hbase, step 3: run the three base-witness techniques (count-shrink,
--    rigidity, non-commutativity) at the GENERAL terminal shape. Steps 1 and 2
--    are done — stuck_leading_shape and stuck_leading_shape_expand, the latter
--    using U-expand's refusal to show a leading field faces either ≥ 2 candidate
--    hosts or a label already present behind a variable.
--  * TERMINATION: a fuel that provably suffices. The naive Rémy measure does not
--    close (renaming adds no fields, so the host side keeps count_l = 0 and the
--    same variable can be re-expanded at the same label); the bound has to come
--    from the other side's l-fields, which solve-and-apply can add.
--  * The occurs guard stays deliberately conservative (occurs_allVar_hasMgu).
--
-- MILESTONES ELSEWHERE THAT BUILD ON THIS FILE (algorithmic.typ, Open questions):
--  * Non-vacuity of qualified schemes: needs lookup_total (RowWF) plus a
--    freshness discipline for the result variables δ — P2/P5 now supply that
--    discipline (Supply/Avoids, SolBelow, AgreeOn, the substitution-ftv toolkit).
--  * STRICTNESS of the QTyped extension, and type safety for QTyped itself.
--  * The covering order ⊴ on qualified schemes (needed to STATE "the principal
--    type improves under reduction").
--  * Solver state S = (θ, Δ, W), stump wake-up, and the confluence argument that
--    the final state is independent of wake-up scheduling
--    (lookup_det + Discharge.mono_of_definite are the two pillars).


end MinimalCalculus
