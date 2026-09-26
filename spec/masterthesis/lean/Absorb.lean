-- ⟦S⟧ STAYS IDEMPOTENT, EVERY STATE EXTENDS THE LAST, AND σ ABSORBS BACKWARDS.
--
-- A-let's generalization lemma uses e₁'s induction hypothesis at σ₁ = ρ ∘ ⟦S₁⟧,
-- and needs σ₁ to agree with the outer σ EXACTLY on Γ and on the stumps that
-- stay parked — `CtxRead` and assumption membership are equalities. `Sat σ S`
-- only gives σ ≈ σ∘⟦S⟧, so the statement carries the exact form as well:
--
--   `Absorbs σ S` — σ ∘ ⟦S⟧ = σ, pointwise.
--
-- It is available where it is needed because
--   * every derivation keeps the solution CLEAN (`Infer.clean`), so ⟦S⟧ is
--     idempotent at every reachable state, and
--   * every derivation EXTENDS its start state: ⟦S′⟧ = r ∘ ⟦S⟧ (`Infer.ext`),
-- and those two make absorption travel backwards (`Absorbs.back`).

import InferSound

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- CLEAN OVER EVERY STEP ------------------------------------

theorem Wake.clean {S S' : SolverState B} {p : Parked B} :
    Wake S p S' → S.sol.Clean → S'.sol.Clean
  | .hit _ hs, hc => hs.clean hc
  | .abs _ hs, hc => hs.clean hc
  | .repark _, hc => hc

theorem Wakes.clean {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → S.sol.Clean → S'.sol.Clean
  | .nil, hc => hc
  | .cons hw hws, hc => Wakes.clean hws (hw.clean hc)
  | .park _ hws, hc => Wakes.clean hws hc

theorem Saturate.clean {S S' : SolverState B} :
    Saturate S S' → S.sol.Clean → S'.sol.Clean
  | .done _, hc => hc
  | .step _ _ hw hsat, hc => Saturate.clean hsat (hw.clean hc)

theorem SolveTySat.clean {S S' : SolverState B} {τ τ' : Ty B} :
    SolveTySat S τ τ' S' → S.sol.Clean → S'.sol.Clean
  | ⟨_, hs, hsat⟩, hc => hsat.clean (hs.clean hc)

theorem Finalize.clean {S S' : SolverState B} {p : Parked B} :
    Finalize S p S' → S.sol.Clean → S'.sol.Clean
  | .star _ _ hs, hc => hs.clean hc

theorem Finalizes.clean {S S' : SolverState B} {ps : List (Parked B)} :
    Finalizes S ps S' → S.sol.Clean → S'.sol.Clean
  | .nil, hc => hc
  | .cons hf hfs, hc => Finalizes.clean hfs (hf.clean hc)

private theorem draw_sol' {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.sol = S.sol := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  subst h2; rfl

mutual

/-- ⊢  **inference keeps ⟦S⟧ idempotent.** -/
theorem Infer.clean {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {e : Expr C} → {τ : Ty B} →
    Infer constTy Γ S e τ S' → S.sol.Clean → S'.sol.Clean
  | _, _, _, _, _, .con, hc => hc
  | _, _, _, _, _, .var _ _ _ _ _ _ _ ⟨_, hws, hsat⟩, hc => hsat.clean (hws.clean hc)
  | _, _, _, _, _, .lam hd hb, hc => Infer.clean hb ((draw_sol' hd) ▸ hc)
  | _, _, _, _, _, .app h₁ h₂ hd hs, hc =>
      hs.clean ((draw_sol' hd) ▸ Infer.clean h₂ (Infer.clean h₁ hc))
  | _, _, _, _, _, .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂, hc =>
      hs₂.clean (hs₁.clean ((draw_sol' hd₂) ▸ (draw_sol' hd₁) ▸
        Infer.clean h₂ (Infer.clean h₁ hc)))
  | _, _, _, _, _, .sel h₁ hd hs _, hc =>
      hs.clean ((draw_sol' hd) ▸ Infer.clean h₁ hc)
  | _, _, _, _, _, .selAbs h₁ hd hs _, hc =>
      hs.clean ((draw_sol' hd) ▸ Infer.clean h₁ hc)
  | _, _, _, _, _, .selUnk h₁ hd hs _ hd₂, hc =>
      (draw_sol' hd₂) ▸ hs.clean ((draw_sol' hd) ▸ Infer.clean h₁ hc)
  | _, _, _, _, _, .rcd hb, hc => InferRec.clean hb hc
  | _, _, _, _, _, .letE h₁ _ _ _ _ _ _ _ _ _ _ h₂, hc => by
      have := Infer.clean h₁ hc
      exact Infer.clean h₂ this

theorem InferRec.clean {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    InferRec constTy Γ S ξ ρ S' → S.sol.Clean → S'.sol.Clean
  | _, _, _, _, _, .empty, hc => hc
  | _, _, _, _, _, .field h, hc => Infer.clean h hc
  | _, _, _, _, _, .cat h₁ h₂, hc => InferRec.clean h₂ (InferRec.clean h₁ hc)

end

--------------------- EXTENSION -----------------------------------------------

/-- ⟦S′⟧ = r ∘ ⟦S⟧ for some r. -/
def SolverState.Ext (S S' : SolverState B) : Prop :=
  ∃ r : TySubst B, ∀ α,
    S'.subst.ty α = (S.subst.ty α).applySubst r ∧
    S'.subst.row α = (S.subst.row α).applySubst r

theorem SolverState.Ext.refl (S : SolverState B) : S.Ext S :=
  ⟨TySubst.id B, fun _ => ⟨(Ty.applySubst_id _).symm, (Row.applySubst_id _).symm⟩⟩

theorem SolverState.Ext.trans {S₁ S₂ S₃ : SolverState B} :
    S₁.Ext S₂ → S₂.Ext S₃ → S₁.Ext S₃
  | ⟨r₁, h₁⟩, ⟨r₂, h₂⟩ => ⟨r₂.comp r₁, fun α => by
      refine ⟨?_, ?_⟩
      · rw [(h₂ α).1, (h₁ α).1, Ty.applySubst_applySubst]
      · rw [(h₂ α).2, (h₁ α).2, Row.applySubst_applySubst]⟩

theorem SolverState.Ext.of_sol_eq {S S' : SolverState B} (h : S'.sol = S.sol) :
    S.Ext S' := by
  refine ⟨TySubst.id B, fun α => ?_⟩
  show (S'.sol.toSubst.ty α) = _ ∧ (S'.sol.toSubst.row α) = _
  rw [h]
  exact ⟨(Ty.applySubst_id _).symm, (Row.applySubst_id _).symm⟩

private theorem tyLookup_comp (s : Sol B) (α : TyVar) :
    (l : List (TyVar × Ty B)) →
    tyLookup α (l.map (fun p => (p.1, p.2.applySubst s.toSubst)) ++ s.ty)
      = (tyLookup α l).applySubst s.toSubst
  | [] => rfl
  | (β, τ) :: t => by
      by_cases h : β = α
      · simp [tyLookup, h]
      · simp only [List.map_cons, List.cons_append, tyLookup, if_neg h]
        exact tyLookup_comp s α t

private theorem rowLookup_comp (s : Sol B) (α : TyVar) :
    (l : List (TyVar × Row B)) →
    rowLookup α (l.map (fun p => (p.1, p.2.applySubst s.toSubst)) ++ s.row)
      = (rowLookup α l).applySubst s.toSubst
  | [] => rfl
  | (β, ρ) :: t => by
      by_cases h : β = α
      · simp [rowLookup, h]
      · simp only [List.map_cons, List.cons_append, rowLookup, if_neg h]
        exact rowLookup_comp s α t

-- ⊢  ⟦S.extend s⟧ = ⟦s⟧ ∘ ⟦S⟧, pointwise
theorem SolverState.extend_subst_ty (S : SolverState B) (s : Sol B) (Sup : Supply)
    (α : TyVar) : (S.extend s Sup).subst.ty α = (S.subst.ty α).applySubst s.toSubst :=
  tyLookup_comp s α S.sol.ty

theorem SolverState.extend_subst_row (S : SolverState B) (s : Sol B) (Sup : Supply)
    (α : TyVar) : (S.extend s Sup).subst.row α = (S.subst.row α).applySubst s.toSubst :=
  rowLookup_comp s α S.sol.row

theorem SolverState.Ext.extend (S : SolverState B) (s : Sol B) (Sup : Supply) :
    S.Ext (S.extend s Sup) :=
  ⟨s.toSubst, fun α => ⟨tyLookup_comp s α S.sol.ty, rowLookup_comp s α S.sol.row⟩⟩

theorem SolveTy.ext {S S' : SolverState B} {τ τ' : Ty B} (h : SolveTy S τ τ' S') :
    S.Ext S' := by
  obtain ⟨_, s, Sup, _, rfl⟩ := h
  exact SolverState.Ext.extend S s Sup

theorem Wake.ext {S S' : SolverState B} {p : Parked B} : Wake S p S' → S.Ext S'
  | .hit _ hs => by
      obtain ⟨_, s, Sup, _, rfl⟩ := hs
      exact SolverState.Ext.extend S s Sup
  | .abs _ hs => by
      obtain ⟨_, s, Sup, _, rfl⟩ := hs
      exact SolverState.Ext.extend S s Sup
  | .repark _ => .of_sol_eq rfl

theorem Wakes.ext {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → S.Ext S'
  | .nil => .refl _
  | .cons hw hws => hw.ext.trans (Wakes.ext hws)
  | .park (S := S) (p := p) _ hws =>
      (SolverState.Ext.of_sol_eq (S := S) (S' := S.park p) rfl).trans (Wakes.ext hws)

theorem Saturate.ext {S S' : SolverState B} : Saturate S S' → S.Ext S'
  | .done _ => .refl _
  | .step _ _ hw hsat => hw.ext.trans (Saturate.ext hsat)

theorem SolveTySat.ext {S S' : SolverState B} {τ τ' : Ty B} :
    SolveTySat S τ τ' S' → S.Ext S'
  | ⟨_, hs, hsat⟩ => hs.ext.trans hsat.ext

theorem draw_ext {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S.Ext S₀ := .of_sol_eq (draw_sol' h)

mutual

/-- ⊢  **every derivation extends its start state.** -/
theorem Infer.ext {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {e : Expr C} → {τ : Ty B} →
    Infer constTy Γ S e τ S' → S.Ext S'
  | _, _, _, _, _, .con => .refl _
  | _, _, _, _, _, .var _ _ _ _ _ _ _ ⟨_, hws, hsat⟩ =>
      ((SolverState.Ext.of_sol_eq rfl).trans hws.ext).trans hsat.ext
  | _, _, _, _, _, .lam hd hb => (draw_ext hd).trans (Infer.ext hb)
  | _, _, _, _, _, .app h₁ h₂ hd hs =>
      ((Infer.ext h₁).trans (Infer.ext h₂)).trans ((draw_ext hd).trans hs.ext)
  | _, _, _, _, _, .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂ =>
      ((Infer.ext h₁).trans (Infer.ext h₂)).trans ((draw_ext hd₁).trans
        ((draw_ext hd₂).trans (hs₁.ext.trans hs₂.ext)))
  | _, _, _, _, _, .sel h₁ hd hs _ => (Infer.ext h₁).trans ((draw_ext hd).trans hs.ext)
  | _, _, _, _, _, .selAbs h₁ hd hs _ =>
      (Infer.ext h₁).trans ((draw_ext hd).trans (hs.ext.trans (.of_sol_eq rfl)))
  | _, _, _, _, _, .selUnk h₁ hd hs _ hd₂ =>
      (Infer.ext h₁).trans ((draw_ext hd).trans (hs.ext.trans
        ((draw_ext hd₂).trans (.of_sol_eq rfl))))
  | _, _, _, _, _, .rcd hb => InferRec.ext hb
  | _, _, _, _, _, .letE (S₁ := S₁) (Δγ := Δγ) h₁ _ _ _ _ _ _ _ _ _ _ h₂ =>
      (Infer.ext h₁).trans ((SolverState.Ext.of_sol_eq (S := S₁)
        (S' := { S₁ with parked := Δγ }) rfl).trans (Infer.ext h₂))

theorem InferRec.ext {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    InferRec constTy Γ S ξ ρ S' → S.Ext S'
  | _, _, _, _, _, .empty => .refl _
  | _, _, _, _, _, .field h => Infer.ext h
  | _, _, _, _, _, .cat h₁ h₂ => (InferRec.ext h₁).trans (InferRec.ext h₂)

end

--------------------- ABSORPTION -----------------------------------------------

/-- σ ∘ ⟦S⟧ = σ, pointwise: σ has already done everything ⟦S⟧ does. -/
def Absorbs (σ : TySubst B) (S : SolverState B) : Prop :=
  ∀ α, (S.subst.ty α).applySubst σ = σ.ty α ∧ (S.subst.row α).applySubst σ = σ.row α

-- ⊢  ⟦S⟧ is idempotent at a clean state
theorem SolverState.idem {S : SolverState B} (hc : S.sol.Clean) (α : TyVar) :
    (S.subst.ty α).applySubst S.subst = S.subst.ty α ∧
    (S.subst.row α).applySubst S.subst = S.subst.row α := by
  have hcl := Sol.closes_toSubst_of_applied hc.applied
  exact ⟨((hcl.1 α)).symm, ((hcl.2.1 α)).symm⟩

-- ⊢  a clean state's own substitution absorbs it
theorem Absorbs.self {S : SolverState B} (hc : S.sol.Clean) : Absorbs S.subst S :=
  SolverState.idem hc

theorem Absorbs.ty {σ : TySubst B} {S : SolverState B} (h : Absorbs σ S) (τ : Ty B) :
    (τ.applySubst S.subst).applySubst σ = τ.applySubst σ := by
  rw [Ty.applySubst_applySubst]
  exact Ty.applySubst_congr τ (fun α _ => h α)

theorem Absorbs.row {σ : TySubst B} {S : SolverState B} (h : Absorbs σ S) (ρ : Row B) :
    (ρ.applySubst S.subst).applySubst σ = ρ.applySubst σ := by
  rw [Row.applySubst_applySubst]
  exact Row.applySubst_congr ρ (fun α _ => h α)

/-- ⊢  **absorption travels backwards** along an extension, from an idempotent
start. -/
theorem Absorbs.back {σ : TySubst B} {S S' : SolverState B} (h : Absorbs σ S')
    (hx : S.Ext S') (hc : S.sol.Clean) : Absorbs σ S := by
  obtain ⟨r, hr⟩ := hx
  intro α
  obtain ⟨hi₁, hi₂⟩ := SolverState.idem hc α
  refine ⟨?_, ?_⟩
  · -- (⟦S⟧α)[σ] = (⟦S⟧α)[⟦S′⟧][σ] = (⟦S⟧α)[⟦S⟧][r][σ] = (⟦S′⟧α)[σ] = σ α
    have e1 : (S.subst.ty α).applySubst S'.subst = S'.subst.ty α := by
      rw [(hr α).1, show (S.subst.ty α).applySubst S'.subst
          = (S.subst.ty α).applySubst (r.comp S.subst) from
            Ty.applySubst_congr _ (fun β _ => ⟨(hr β).1, (hr β).2⟩),
        ← Ty.applySubst_applySubst, hi₁]
    rw [← h.ty (S.subst.ty α), e1]; exact (h α).1
  · have e1 : (S.subst.row α).applySubst S'.subst = S'.subst.row α := by
      rw [(hr α).2, show (S.subst.row α).applySubst S'.subst
          = (S.subst.row α).applySubst (r.comp S.subst) from
            Row.applySubst_congr _ (fun β _ => ⟨(hr β).1, (hr β).2⟩),
        ← Row.applySubst_applySubst, hi₂]
    rw [← h.row (S.subst.row α), e1]; exact (h α).2

/-- ⊢  absorbing a clean state means satisfying it. -/
theorem Absorbs.sat {σ : TySubst B} {S : SolverState B} (h : Absorbs σ S)
    (hc : S.sol.Clean) : Sol.Sat σ S.sol := by
  have hs := hc.sat
  refine ⟨fun p hp => ?_, fun p hp => ?_⟩
  · have := TyEquiv.applySubst σ (hs.1 p hp)
    change TyEquiv ((S.subst.ty p.1).applySubst σ) ((p.2.applySubst S.subst).applySubst σ)
      at this
    rw [(h p.1).1, h.ty] at this; exact this
  · have := RowEquiv.applySubst σ (hs.2 p hp)
    change RowEquiv ((S.subst.row p.1).applySubst σ) ((p.2.applySubst S.subst).applySubst σ)
      at this
    rw [(h p.1).2, h.row] at this; exact this

end MinimalCalculus
