-- FINALIZATION DISCHARGES, AND `RunSound` — PROVED.
--
-- F-★ fires on a parked stump whose lookup is BLOCKED on a free row variable β,
-- and solves `δ ≐ ★`. That equation writes nothing at the row sort, so β stays
-- free through every later finalization step, and at the final ⟦S′⟧:
--
--   * the lookup, read under ⟦S′⟧, is still blocked on β — so it is `?` at the
--     discharged context (`lookup_blocked_subst`), and
--   * δ is ★, since ⟦S′⟧ satisfies the equation F-★ solved and ★ is ≈-rigid.
--
-- That is D-?. So every stump a run hands to finalization HOLDS at ⟦S′⟧, and
-- `inferSound` plus the χ-correction gives `RunSound`.
--
-- This is also where the old `hfix` side condition of `Finalize.dischargeEquiv`
-- went: "σ does not refine the blocked row" is exactly "β is still free at
-- ⟦S′⟧", and that is now proved rather than assumed — at ⟦S′⟧, not at an
-- arbitrary σ, which is the only reading under which it is true.

import LetCase

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- THE BLOCKER IS FREE, AND STAYS FREE ----------------------

-- ⊢  a lookup blocked on β has β unsolved in the context it was blocked in
theorem LookupBlocked.free {Γ : Ctx B} {ρ : Row B} {l : Label} {β : TyVar}
    (h : LookupBlocked Γ ρ l β) : Γ.lookupRow β = none := by
  induction h with
  | varFree hα => exact hα
  | var _ _ ih => exact ih
  | catSkip _ _ ih => exact ih
  | catUnk _ ih => exact ih

theorem SolverState.row_var_of_free {S : SolverState B} {β : TyVar}
    (h : S.ctx.lookupRow β = none) : S.subst.row β = .var β := by
  apply rowLookup_not_mem
  intro hm
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hm
  have : (S.sol.row.find? (·.1 == p.1)).isSome := by
    rw [List.find?_isSome]; exact ⟨p, hp, by simp⟩
  simp only [SolverState.ctx, Sol.toCtx, Ctx.lookupRow] at h
  rw [Option.map_eq_none_iff] at h
  rw [h] at this; exact nomatch this

-- ⊢  `δ ≐ ★` binds nothing at the row sort
private theorem solve_star_row_free {S S₀ : SolverState B} {δ : TyVar}
    (hs : SolveTy S (.var δ) .unk S₀) {β : TyVar} (hβ : S.subst.row β = .var β) :
    S₀.subst.row β = .var β := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := hs
  have hg := (unifyM_good fuel).1 S.supply _ _ hu
  have hkey : β ∉ s.row.map Prod.fst := by
    intro hm
    obtain ⟨p, hp, hpe⟩ := List.mem_map.mp hm
    have hd : (true, β) ∈ s.domS :=
      List.mem_append_right _ (List.mem_map.mpr ⟨p, hp, by rw [hpe]⟩)
    have := hg.dom _ hd
    revert this hu
    simp only [Ty.applySubst]
    cases S.subst.ty δ with
    | var γ => intro _ h; simp [Ty.sortedFtv] at h
    | unk => intro _ h; simp [Ty.sortedFtv] at h
    | base b => intro hu; simp [unifyTyF] at hu
    | fn a b => intro hu; simp [unifyTyF] at hu
    | rcd ρ => intro hu; simp [unifyTyF] at hu
  rw [SolverState.extend_subst_row, hβ]
  exact rowLookup_not_mem _ hkey

theorem Finalize.row_free {S S' : SolverState B} {p : Parked B} {β : TyVar}
    (hf : Finalize S p S') (hβ : S.subst.row β = .var β) : S'.subst.row β = .var β := by
  cases hf with
  | star _ _ hs => exact solve_star_row_free hs hβ

theorem Finalizes.row_free {S S' : SolverState B} {ps : List (Parked B)} {β : TyVar} :
    Finalizes S ps S' → S.subst.row β = .var β → S'.subst.row β = .var β
  | .nil, h => h
  | .cons hf hfs, h => Finalizes.row_free hfs (hf.row_free h)

theorem Finalize.ext {S S' : SolverState B} {p : Parked B} (hf : Finalize S p S') :
    S.Ext S' := by
  cases hf with
  | star _ _ hs => exact hs.ext.trans (.of_sol_eq rfl)

theorem Finalizes.ext {S S' : SolverState B} {ps : List (Parked B)} :
    Finalizes S ps S' → S.Ext S'
  | .nil => .refl _
  | .cons hf hfs => hf.ext.trans (Finalizes.ext hfs)

--------------------- ONE STEP DISCHARGES ------------------------------------

/-- ⊢  **F-★ is D-?**, read at any σ that absorbs the step's start, satisfies
where it lands, and leaves the blocker a variable. -/
theorem Finalize.holds {S S' : SolverState B} {p : Parked B} (hf : Finalize S p S')
    (hc : S.sol.Clean) {σ : TySubst B} (hab : Absorbs σ S) (hsat : Sol.Sat σ S'.sol)
    {β' : TyVar} (hβ : σ.row p.blocker = .var β') :
    (p.stump.at σ).Holds (⟨[], []⟩ : Ctx B) := by
  cases hf with
  | star _ hb hs =>
      refine .unk ?_ ?_
      · show Lookup _ (p.stump.row.applySubst σ) _ _
        rw [← hab.row]
        exact lookup_blocked_subst hb (noChase_of_clean hc _) σ hβ
      · exact (TyEquiv.unk_inv_both (hs.unifies_sat hsat)).2 rfl

/-- ⊢  **…and so does a whole finalization run**, at the state it ends in. -/
theorem Finalizes.holds {S S' : SolverState B} {ps : List (Parked B)} :
    Finalizes S ps S' → S.sol.Clean →
    ∀ p ∈ ps, (p.stump.at S'.subst).Holds (⟨[], []⟩ : Ctx B)
  | .nil, _, p, hp => absurd hp List.not_mem_nil
  | .cons (S := S₀) (S₁ := S₁) (p := p) hf hfs, hc, p', hp' => by
      have c₁ := hf.clean hc
      have c' := hfs.clean c₁
      rcases List.mem_cons.mp hp' with rfl | hp'
      · have hab : Absorbs S'.subst S₀ := (Absorbs.self c').back (hf.ext.trans hfs.ext) hc
        have hab₁ : Absorbs S'.subst S₁ := (Absorbs.self c').back hfs.ext c₁
        have hfree : S₀.subst.row p'.blocker = .var p'.blocker := by
          cases hf with
          | star _ hb _ => exact SolverState.row_var_of_free hb.free
        exact hf.holds hc hab (hab₁.sat c₁) (hfs.row_free (hf.row_free hfree))
      · exact Finalizes.holds hfs c₁ p' hp'

--------------------- RunSound -------------------------------------------------

/-- ⊢  **ALGORITHM SOUNDNESS — `RunSound`.** A run from nothing — inference,
then finalization of everything still parked — types its program at the type
it reports, read under its own final substitution, in the empty context. -/
theorem runSound {C : Type} {constTy : C → B} : RunSound B C constTy := by
  rintro e τ S' ⟨S₁, hinf, hfins⟩
  have c₁ := Infer.clean hinf Sol.clean_nil
  have c' := hfins.clean c₁
  have hab₁ : Absorbs S'.subst S₁ := (Absorbs.self c').back hfins.ext c₁
  exact runSoundA hinf hab₁ (hab₁.sat c₁) (fun p hp => hfins.holds c₁ p hp)

--------------------- THE χ-CORRECTION IS FALSE IN GENERAL ----------------------
-- Two constraints on the SAME result variable whose lookups find ≈-equal but
-- syntactically different types: one χ discharges both up to ≈, but an exact
-- instance would have to send δ to both at once.

private def icA : Ty Unit := .rcd (.cat (.sing "a" (.base ())) (.sing "b" (.base ())))
private def icB : Ty Unit := .rcd (.cat (.sing "b" (.base ())) (.sing "a" (.base ())))
private def icSc : QScheme Unit :=
  ⟨["d"], [⟨.sing "l" icA, "l", "d"⟩, ⟨.sing "l" icB, "l", "d"⟩], .var "d"⟩
private def icχ : TySubst Unit := ⟨fun x => if x = "d" then icA else .var x, fun x => .var x⟩

private theorem icA_equiv_icB : TyEquiv icA icB :=
  .rcd (.comm (by decide))

/-- ⊢  **`InstEquivCorrects` is false**: the correction needs one constraint per
result variable, at least — which is why it is stated for `Correctable` schemes
(`QScheme.Correctable.correct`) and A-let builds only those. -/
theorem instEquivCorrects_false : ¬ InstEquivCorrects Unit := by
  intro h
  obtain ⟨τ', ⟨θ, -, hdis, -⟩, -⟩ := h ⟨[], []⟩ icSc icχ
    ⟨fun α hα => by
        have : α ≠ "d" := fun he => hα (by simp [icSc, he])
        simp [icχ, this], fun _ _ => rfl⟩
    (fun st hst => by
      simp only [icSc, List.mem_cons, List.not_mem_nil, or_false] at hst
      rcases hst with rfl | rfl
      · exact .hit (τ := icA) .hit (by simp [icχ]; exact .refl _)
      · exact .hit (τ := icB) .hit (by simp [icχ]; exact icA_equiv_icB))
  have h1 := hdis ⟨.sing "l" icA, "l", "d"⟩ (by simp [icSc])
  have h2 := hdis ⟨.sing "l" icB, "l", "d"⟩ (by simp [icSc])
  cases h1 with
  | hit hl₁ he₁ =>
    cases h2 with
    | hit hl₂ he₂ =>
        cases lookup_det hl₁ .hit
        cases lookup_det hl₂ .hit
        have : icA = icB := he₁.symm.trans he₂
        simp only [icA, icB, Ty.rcd.injEq, Row.cat.injEq, Row.sing.injEq] at this
        exact absurd this.1.1 (by decide)
    | abs hl _ => exact nomatch lookup_det hl .hit
    | unk hl _ => exact nomatch lookup_det hl .hit
  | abs hl _ => exact nomatch lookup_det hl .hit
  | unk hl _ => exact nomatch lookup_det hl .hit

end MinimalCalculus
