-- FINALIZATION DISCHARGES, AND `RunSound` — PROVED.
--
-- F-★ fires on a parked stump whose lookup is BLOCKED on a row variable β,
-- and solves `δ ≐ ★`. That equation writes nothing at the row sort, so β stays
-- free through every later finalization step, and at the final ⟦S′⟧:
--
--   * the lookup, read under ⟦S′⟧, is still blocked on β — so it is `?`
--     (`lookup_blocked_subst`), and
--   * δ is ★, since ⟦S′⟧ satisfies the equation F-★ solved and ★ is ≈-rigid.
--
-- That is D-?. So every stump a run hands to finalization HOLDS at ⟦S′⟧, and
-- `inferSound` plus the cash-in (`QTypedA.toQTyped`) gives `RunSound`.
--
-- This is also where the old `hfix` side condition of `Finalize.dischargeEquiv`
-- went: "σ does not refine the blocked row" is exactly "β is still free at
-- ⟦S′⟧", and that is now proved rather than assumed — at ⟦S′⟧, not at an
-- arbitrary σ, which is the only reading under which it is true.

import LetCase

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- THE BLOCKER IS FREE, AND STAYS FREE ----------------------

-- (`LookupBlocked.free` — "a lookup blocked on β has β unsolved in the context
-- it was blocked in" — and `SolverState.row_var_of_free`, which read that off
-- ⟦S⟧-as-a-context, went with L-α. Their content is now
-- `LookupBlocked.mem_sortedFtv` plus `SolverState.row_var_of_clean`
-- (LetCase.lean): the blocker sits on the spine of a row ⟦S⟧ has already
-- substituted, and a clean ⟦S⟧ leaves such a variable alone.)

-- ⊢  `δ ≐ ★` binds nothing at the row sort
private theorem solve_star_row_free {S S₀ : SolverState B} {τ : Ty B}
    (hs : SolveTy S τ .unk S₀) {β : TyVar} (hβ : S.subst.row β = .var β) :
    S₀.subst.row β = .var β := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := hs
  have hg := unifyTyF_flat_good fuel _ _ _
    (by generalize τ.applySubst S.subst = t; cases t <;> rfl) hu
  have hkey : β ∉ s.row.map Prod.fst := by
    intro hm
    obtain ⟨p, hp, hpe⟩ := List.mem_map.mp hm
    have hd : (.row, β) ∈ s.domS :=
      Sol.mem_domS_row (List.mem_map.mpr ⟨p, hp, hpe⟩)
    have := hg.dom _ hd
    revert this hu
    simp only [Ty.applySubst]
    cases τ.applySubst S.subst with
    | var γ => intro _ h; simp [Ty.sortedFtv] at h
    | unk => intro _ h; simp [Ty.sortedFtv] at h
    | base b => intro hu; simp [unifyTyF] at hu
    | lab b => intro hu; simp [unifyTyF] at hu
    | fn a b => intro hu; simp [unifyTyF] at hu
    | rcd ρ => intro hu; simp [unifyTyF] at hu
  rw [SolverState.extend_subst_row, hβ]
  exact rowLookup_not_mem _ hkey

-- ⊢  …and at the type sort it binds only to ★: a variable's image stays a
--    variable or becomes ★, and in particular never becomes a LABEL. That is
--    what a key-blocked stump needs of the steps finalizing the others.
/-- a variable, or ★ — what F-★ can make of a variable. -/
def Ty.StarOrVar (τ : Ty B) : Prop := τ = .unk ∨ ∃ γ, τ = .var γ

private theorem starOrVar_tyLookup {s : List (TyVar × Ty B)}
    (hs : ∀ p ∈ s, p.2 = .unk) (γ : TyVar) : Ty.StarOrVar (tyLookup γ s) := by
  induction s with
  | nil => exact .inr ⟨γ, rfl⟩
  | cons p t ih =>
      simp only [tyLookup]
      split
      · exact .inl (hs p List.mem_cons_self)
      · exact ih (fun q hq => hs q (List.mem_cons_of_mem _ hq))

private theorem solve_star_sol {S S₀ : SolverState B} {τ : Ty B}
    (hs : SolveTy S τ .unk S₀) :
    ∃ s Sup, S₀ = S.extend s Sup ∧ ∀ p ∈ s.ty, p.2 = .unk := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := hs
  refine ⟨s, Sup, rfl, ?_⟩
  revert hu
  simp only [Ty.applySubst]
  cases τ.applySubst S.subst with
  | var γ =>
      intro hu
      cases fuel <;>
      · simp only [unifyTyF, bindTy, tyIsVar, Ty.tyFtv] at hu
        simp at hu
        obtain ⟨rfl, -⟩ := hu
        intro p hp; simp at hp; rw [hp]
  | unk =>
      intro hu
      cases fuel <;>
      · simp only [unifyTyF] at hu
        simp only [UResM.success.injEq] at hu
        obtain ⟨rfl, -⟩ := hu
        intro p hp; exact nomatch hp
  | base b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | lab b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | fn a b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | rcd ρ => intro hu; cases fuel <;> simp [unifyTyF] at hu

theorem Finalize.starOrVar {S S' : SolverState B} {p : Parked B} {β : TyVar}
    (hf : Finalize S p S') (hβ : (S.subst.ty β).StarOrVar) : (S'.subst.ty β).StarOrVar := by
  cases hf with
  | star _ _ hs =>
      obtain ⟨s, Sup, rfl, hstar⟩ := solve_star_sol hs
      show ((S.extend s Sup).subst.ty β).StarOrVar
      rw [SolverState.extend_subst_ty]
      rcases hβ with h | ⟨γ, h⟩ <;> rw [h]
      · exact .inl rfl
      · exact starOrVar_tyLookup hstar γ

theorem Finalizes.starOrVar {S S' : SolverState B} {ps : List (Parked B)} {β : TyVar} :
    Finalizes S ps S' → (S.subst.ty β).StarOrVar → (S'.subst.ty β).StarOrVar
  | .nil, h => h
  | .cons hf hfs, h => Finalizes.starOrVar hfs (hf.starOrVar h)

--------------------- A KEY BLOCKER STAYS A VARIABLE ---------------------------
-- F-★ solves `δ ≐ ★`, a TYPE equation: its solution binds no label variable.
-- So a key blocker — a label variable — is never touched by finalization, and
-- neither is any other label variable of the lookup.

-- ⊢  what `δ ≐ ★` writes: no row, and no label
private theorem solve_star_dom {S S₀ : SolverState B} {τ : Ty B}
    (hs : SolveTy S τ .unk S₀) :
    ∃ s Sup, S₀ = S.extend s Sup ∧ s.row = [] ∧ s.lab = [] := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := hs
  refine ⟨s, Sup, rfl, ?_⟩
  revert hu
  simp only [Ty.applySubst]
  cases hτ : τ.applySubst S.subst with
  | var γ =>
      intro hu
      cases fuel <;>
      · simp only [unifyTyF, bindTy, tyIsVar, Ty.tyFtv] at hu
        simp at hu
        obtain ⟨rfl, -⟩ := hu
        exact ⟨rfl, rfl⟩
  | unk =>
      intro hu
      cases fuel <;>
      · simp only [unifyTyF] at hu
        simp only [UResM.success.injEq] at hu
        obtain ⟨rfl, -⟩ := hu
        exact ⟨rfl, rfl⟩
  | base b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | lab b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | fn a b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | rcd ρ => intro hu; cases fuel <;> simp [unifyTyF] at hu

theorem Finalize.lab_free {S S' : SolverState B} {p : Parked B} {β : TyVar}
    (hf : Finalize S p S') (hβ : S.subst.lab β = .var β) : S'.subst.lab β = .var β := by
  cases hf with
  | star _ _ hs =>
      obtain ⟨s, Sup, rfl, -, hlab⟩ := solve_star_dom hs
      show (S.extend s Sup).subst.lab β = _
      rw [SolverState.extend_subst_lab, hβ]
      simp [Sol.toSubst, hlab, labLookup]

theorem Finalizes.lab_free {S S' : SolverState B} {ps : List (Parked B)} {β : TyVar} :
    Finalizes S ps S' → S.subst.lab β = .var β → S'.subst.lab β = .var β
  | .nil, h => h
  | .cons hf hfs, h => Finalizes.lab_free hfs (hf.lab_free h)

-- ⊢  a clean state leaves every label variable of what it substituted alone
omit [DecidableEq B] in
theorem SolverState.lab_var_of_clean_row {S : SolverState B} (hc : S.sol.Clean) {ρ : Row B}
    {α : TyVar} (hα : (.lab, α) ∈ (ρ.applySubst S.subst).sortedFtv) :
    S.subst.lab α = .var α := by
  apply labLookup_not_mem
  intro hm
  exact hc.clears_row hα (Sol.mem_domS_lab hm)

omit [DecidableEq B] in
theorem SolverState.lab_var_of_clean_key {S : SolverState B} (hc : S.sol.Clean) {k : Key}
    {α : TyVar} (hα : (.lab, α) ∈ Key.sortedFtv (k.applySubst S.subst)) :
    S.subst.lab α = .var α := by
  apply labLookup_not_mem
  intro hm
  have hd : (.lab, α) ∈ S.sol.domS := Sol.mem_domS_lab hm
  cases k with
  | lit _ => simp [Key.sortedFtv] at hα
  | var β =>
      change (.lab, α) ∈ Key.sortedFtv (labLookup β S.sol.lab) at hα
      rcases labLookup_cases S.sol.lab β with ⟨hnm, he⟩ | ⟨q, hq, -, he⟩
      · rw [he] at hα
        simp [Key.sortedFtv] at hα
        subst hα
        exact hnm hm
      · rw [he] at hα
        exact hc.1 _ (.inr (.inr ⟨q, hq, hα⟩)) hd

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
where it lands, and leaves the blocker a variable — at a key, one of its own. -/
theorem Finalize.holds {S S' : SolverState B} {p : Parked B} (hf : Finalize S p S')
    {σ : TySubst B} (hab : Absorbs σ S) (hsat : Sol.Sat σ S'.sol)
    (hβ : (.row, p.blocker) ∈ (p.stump.row.applySubst S.subst).sortedFtv →
      ∃ β', σ.row p.blocker = .var β')
    (hk : (.lab, p.blocker) ∈ Key.sortedFtv (p.stump.label.applySubst S.subst) ++
        (p.stump.row.applySubst S.subst).sortedFtv →
      KeyFresh σ (Key.sortedFtv (p.stump.label.applySubst S.subst) ++
        (p.stump.row.applySubst S.subst).sortedFtv) p.blocker) :
    (p.stump.at σ).Holds := by
  cases hf with
  | star _ hb hs =>
      have hδ := (TyEquiv.unk_inv_both (hs.unifies_sat hsat)).2 rfl
      have hu := lookupQ_blocked_subst hb σ hβ hk
      rw [hab.row, hab.key] at hu
      exact .unk hu hδ

/-- ⊢  **…and so does a whole finalization run**, at the state it ends in. -/
theorem Finalizes.holds {S S' : SolverState B} {ps : List (Parked B)} :
    Finalizes S ps S' → S.sol.Clean →
    ∀ p ∈ ps, (p.stump.at S'.subst).Holds
  | .nil, _, p, hp => absurd hp List.not_mem_nil
  | .cons (S := S₀) (S₁ := S₁) (p := p) hf hfs, hc, p', hp' => by
      have c₁ := hf.clean hc
      have c' := hfs.clean c₁
      rcases List.mem_cons.mp hp' with rfl | hp'
      · have hab : Absorbs S'.subst S₀ := (Absorbs.self c').back (hf.ext.trans hfs.ext) hc
        have hab₁ : Absorbs S'.subst S₁ := (Absorbs.self c').back hfs.ext c₁
        -- a label variable of the looked-up row or key is unsolved at S₀ (clean),
        -- and finalization binds no label: it is still itself at S′
        have hv : ∀ {γ}, (.lab, γ) ∈ Key.sortedFtv (p'.stump.label.applySubst S₀.subst) ++
            (p'.stump.row.applySubst S₀.subst).sortedFtv → S'.subst.lab γ = .var γ := by
          intro γ h
          refine hfs.lab_free (hf.lab_free ?_)
          rcases List.mem_append.mp h with h | h
          · exact SolverState.lab_var_of_clean_key hc h
          · exact SolverState.lab_var_of_clean_row hc h
        refine hf.holds hab (hab₁.sat c₁) (fun hmem => ⟨p'.blocker,
          hfs.row_free (hf.row_free (SolverState.row_var_of_clean hc hmem))⟩) ?_
        intro hkey
        refine ⟨p'.blocker, hv hkey, fun γ hγ he => ?_⟩
        rw [hv hγ] at he; injection he
      · exact Finalizes.holds hfs c₁ p' hp'

--------------------- MATERIALIZATION KEEPS ------------------------------------
-- F-hit is an equation plus saturation, so it inherits `KeepsS` from
-- `SolveTySat`: every stump parked before it is still parked after it — the same
-- stump — or discharged under any σ satisfying the state it ends in. Nothing
-- about the materialized stump itself is needed: saturation wakes it, and
-- `KeepsS` already says what that means.

private theorem draw_sol {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.sol = S.sol := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  subst h2; rfl

/-- ⊢  one F-hit step keeps every stump and cleanliness, and extends. -/
theorem Materialize.keeps {S S' : SolverState B} {p : Parked B} (hm : Materialize S p S')
    (hc : S.sol.Clean) :
    S.KeepsS S' ∧ S'.sol.Clean ∧ S.Ext S' ∧ S.SatMono S' := by
  cases hm with
  | hit _ _ _ _ hd hs =>
      obtain ⟨k₀, m₀, -⟩ := draw_keeps hd
      have k' := hs.keeps
      exact ⟨k₀.trans k' hs.satMono, hs.clean (draw_sol hd ▸ hc),
        (draw_ext hd).trans hs.ext, m₀.trans hs.satMono⟩

theorem Materialize.clean {S S' : SolverState B} {p : Parked B} (hm : Materialize S p S')
    (hc : S.sol.Clean) : S'.sol.Clean := by
  cases hm with
  | hit _ _ _ _ hd hs => exact hs.clean (draw_sol hd ▸ hc)

theorem Materializes.clean {S S' : SolverState B} {ps : List (Parked B)} :
    Materializes S ps S' → S.sol.Clean → S'.sol.Clean
  | .nil, hc => hc
  | .skip hms, hc => Materializes.clean hms hc
  | .cons hm hms, hc => Materializes.clean hms (hm.clean hc)

/-- ⊢  …and so does the whole materialization phase. -/
theorem Materializes.keeps {S S' : SolverState B} {ps : List (Parked B)} :
    Materializes S ps S' → S.sol.Clean →
      S.KeepsS S' ∧ S'.sol.Clean ∧ S.Ext S' ∧ S.SatMono S'
  | .nil, hc => ⟨.refl _, hc, .refl _, .refl _⟩
  | .skip hms, hc => Materializes.keeps hms hc
  | .cons hm hms, hc => by
      obtain ⟨k₁, c₁, x₁, m₁⟩ := hm.keeps hc
      obtain ⟨k₂, c₂, x₂, m₂⟩ := Materializes.keeps hms c₁
      exact ⟨k₁.trans k₂ m₂, c₂, x₁.trans x₂, m₁.trans m₂⟩

--------------------- RunSound -------------------------------------------------

/-- ⊢  **ALGORITHM SOUNDNESS — `RunSound`.** A run from nothing — inference,
materialization of spent promises, then F-★ on everything still parked — types
its program at the type it reports, read under its own final substitution, in
the empty context.

A stump parked at the end of inference either survives materialization — and
F-★ discharges it (`Finalizes.holds`) — or materialization's saturation
discharged it already (`KeepsS`). -/
theorem runSound {C : Type} {constTy : C → B} : RunSound B C constTy := by
  rintro e τ S' ⟨S₁, S₂, hinf, hmat, hfins⟩
  have c₁ := Infer.clean hinf Sol.clean_nil
  obtain ⟨k₁₂, c₂, x₁₂, -⟩ := hmat.keeps c₁
  have c' := hfins.clean c₂
  have hab₁ : Absorbs S'.subst S₁ := (Absorbs.self c').back (x₁₂.trans hfins.ext) c₁
  have hab₂ : Absorbs S'.subst S₂ := (Absorbs.self c').back hfins.ext c₂
  refine runSoundA hinf hab₁ (hab₁.sat c₁) (fun p hp => ?_)
  rcases k₁₂ S'.subst (hab₂.sat c₂) p hp with ⟨q, hq, hqs⟩ | hd
  · have := hfins.holds c₂ q hq
    rwa [hqs] at this
  · exact Stump.dischargeEquiv_iff_holds.mp hd

--------------------- ≈-EQUAL HITS ON ONE RESULT ------------------------------
-- Two constraints on the SAME result variable whose lookups find ≈-equal but
-- syntactically different types. With an exact D-hit no instance existed (δ
-- would go to both at once), which refuted the χ-correction for arbitrary
-- schemes. With D-hit up to ≈, one δ serves both.

private def icA : Ty Unit := .rcd (.cat (.sing "a" (.base ())) (.sing "b" (.base ())))
private def icB : Ty Unit := .rcd (.cat (.sing "b" (.base ())) (.sing "a" (.base ())))
private def icSc : QScheme Unit :=
  ⟨["d"], [⟨.sing "l" icA, .lit "l", .var "d"⟩, ⟨.sing "l" icB, .lit "l", .var "d"⟩], .var "d"⟩
private def icχ : TySubst Unit :=
  ⟨fun x => if x = "d" then icA else .var x, fun x => .var x, fun x => .var x⟩

private theorem icA_equiv_icB : TyEquiv icA icB :=
  .rcd (.comm (by decide))

/-- ⊢  the scheme with two ≈-equal hits on one result is instantiable. -/
theorem icSc_inst : QScheme.Inst icSc icA :=
  ⟨icχ, ⟨fun α hα => by
        have : α ≠ "d" := fun he => hα (by simp [icSc, he])
        simp [icχ, this], fun _ _ => rfl, fun _ _ => rfl⟩,
    fun st hst => by
      simp only [icSc, List.mem_cons, List.not_mem_nil, or_false] at hst
      rcases hst with rfl | rfl
      · exact .hit (τ := icA) (LookupQ.lab_iff.mpr .hit) (by simp [icχ]; exact .refl _)
      · exact .hit (τ := icB) (LookupQ.lab_iff.mpr .hit) (by simp [icχ]; exact icA_equiv_icB),
    by simp [icSc, icχ, Ty.applySubst]⟩

end MinimalCalculus
