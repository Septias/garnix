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
  have hg := (unifyM_good fuel).1 S.supply _ _ hu
  have hkey : β ∉ s.row.map Prod.fst := by
    intro hm
    obtain ⟨p, hp, hpe⟩ := List.mem_map.mp hm
    have hd : (true, β) ∈ s.domS :=
      List.mem_append_right _ (List.mem_map.mpr ⟨p, hp, by rw [hpe]⟩)
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
  | star _ _ _ hs =>
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
-- F-★ binds exactly the variables of the answer it commits, and only to ★. So a
-- type variable that is no parked stump's answer is left alone by a step, and is
-- still no answer after it: the remaining answers only lose variables. That is
-- what `Parked.KeySafe` buys a key-blocked stump for every step after its own.

private theorem idOrStar_tyLookup {s : List (TyVar × Ty B)}
    (hs : ∀ p ∈ s, p.2 = .unk) (γ : TyVar) :
    tyLookup γ s = .unk ∨ tyLookup γ s = .var γ := by
  induction s with
  | nil => exact .inr rfl
  | cons p t ih =>
      simp only [tyLookup]
      split
      · exact .inl (hs p List.mem_cons_self)
      · exact ih (fun q hq => hs q (List.mem_cons_of_mem _ hq))

-- ⊢  what `δ ≐ ★` writes: ★, at the answer's own variables, and no row
private theorem solve_star_dom {S S₀ : SolverState B} {τ : Ty B}
    (hs : SolveTy S τ .unk S₀) :
    ∃ s Sup, S₀ = S.extend s Sup ∧ (∀ p ∈ s.ty, p.2 = .unk) ∧
      (∀ p ∈ s.ty, p.1 ∈ (τ.applySubst S.subst).ftv) ∧ s.row = [] := by
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
        refine ⟨fun p hp => ?_, fun p hp => ?_, rfl⟩ <;> simp at hp <;> subst hp <;>
          simp [Ty.ftv]
  | unk =>
      intro hu
      cases fuel <;>
      · simp only [unifyTyF] at hu
        simp only [UResM.success.injEq] at hu
        obtain ⟨rfl, -⟩ := hu
        exact ⟨fun p hp => (nomatch hp), fun p hp => (nomatch hp), rfl⟩
  | base b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | lab b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | fn a b => intro hu; cases fuel <;> simp [unifyTyF] at hu
  | rcd ρ => intro hu; cases fuel <;> simp [unifyTyF] at hu

/-- β reads as itself, and no parked answer mentions it. -/
def SolverState.Untouched (S : SolverState B) (β : TyVar) : Prop :=
  S.subst.ty β = .var β ∧ ∀ q ∈ S.parked, β ∉ (q.stump.res.applySubst S.subst).ftv

theorem Finalize.untouched {S S' : SolverState B} {p : Parked B} {β : TyVar}
    (hf : Finalize S p S') (h : S.Untouched β) : S'.Untouched β := by
  obtain ⟨hv, hn⟩ := h
  cases hf with
  | star hp _ _ hs =>
      obtain ⟨s, Sup, rfl, hstar, hdom, hrow⟩ := solve_star_dom hs
      have hβs : β ∉ s.ty.map Prod.fst := fun hm => by
        obtain ⟨e, he, rfl⟩ := List.mem_map.mp hm
        exact hn p hp (hdom e he)
      refine ⟨?_, fun q hq hm => ?_⟩
      · show (S.extend s Sup).subst.ty β = _
        rw [SolverState.extend_subst_ty, hv]
        exact tyLookup_not_mem _ hβs
      · have hq' : q ∈ S.parked := (List.mem_filter.mp hq).1
        change β ∈ (q.stump.res.applySubst (S.extend s Sup).subst).ftv at hm
        have heq : q.stump.res.applySubst (S.extend s Sup).subst =
            (q.stump.res.applySubst S.subst).applySubst s.toSubst := by
          rw [Ty.applySubst_applySubst]
          exact Ty.applySubst_congr _ (fun γ _ =>
            ⟨SolverState.extend_subst_ty S s Sup γ, SolverState.extend_subst_row S s Sup γ⟩)
        rw [heq] at hm
        obtain ⟨α, hα, hγ⟩ := Ty.ftv_applySubst s.toSubst _ β hm
        rcases hγ with hγ | hγ
        · rcases idOrStar_tyLookup hstar α with h | h
          · simp [Sol.toSubst, h, Ty.ftv] at hγ
          · simp only [Sol.toSubst, h, Ty.ftv, List.mem_singleton] at hγ
            subst hγ; exact hn q hq' hα
        · simp only [Sol.toSubst, hrow, rowLookup, Row.ftv, List.mem_singleton] at hγ
          subst hγ; exact hn q hq' hα

theorem Finalizes.untouched {S S' : SolverState B} {ps : List (Parked B)} {β : TyVar} :
    Finalizes S ps S' → S.Untouched β → S'.Untouched β
  | .nil, h => h
  | .cons hf hfs, h => Finalizes.untouched hfs (hf.untouched h)

-- ⊢  a variable reads as itself or as ★, before and after a step
theorem Finalize.idOrStar {S S' : SolverState B} {p : Parked B} {γ : TyVar}
    (hf : Finalize S p S') (h : S.subst.ty γ = .unk ∨ S.subst.ty γ = .var γ) :
    S'.subst.ty γ = .unk ∨ S'.subst.ty γ = .var γ := by
  cases hf with
  | star _ _ _ hs =>
      obtain ⟨s, Sup, rfl, hstar, -⟩ := solve_star_dom hs
      show (S.extend s Sup).subst.ty γ = _ ∨ (S.extend s Sup).subst.ty γ = _
      rw [SolverState.extend_subst_ty]
      rcases h with h | h <;> rw [h]
      · exact .inl rfl
      · exact idOrStar_tyLookup hstar γ

theorem Finalizes.idOrStar {S S' : SolverState B} {ps : List (Parked B)} {γ : TyVar} :
    Finalizes S ps S' → (S.subst.ty γ = .unk ∨ S.subst.ty γ = .var γ) →
      (S'.subst.ty γ = .unk ∨ S'.subst.ty γ = .var γ)
  | .nil, h => h
  | .cons hf hfs, h => Finalizes.idOrStar hfs (hf.idOrStar h)

-- ⊢  a clean state leaves every type-sort variable of what it substituted alone
omit [DecidableEq B] in
theorem SolverState.ty_var_of_clean_row {S : SolverState B} (hc : S.sol.Clean) {ρ : Row B}
    {α : TyVar} (hα : (false, α) ∈ (ρ.applySubst S.subst).sortedFtv) :
    S.subst.ty α = .var α := by
  apply tyLookup_not_mem
  intro hm
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hm
  exact hc.clears_row hα
    (List.mem_append_left _ (List.mem_map_of_mem (f := fun p => (false, p.1)) hp))

omit [DecidableEq B] in
theorem SolverState.ty_var_of_clean_ty {S : SolverState B} (hc : S.sol.Clean) {τ : Ty B}
    {α : TyVar} (hα : (false, α) ∈ (τ.applySubst S.subst).sortedFtv) :
    S.subst.ty α = .var α := by
  apply tyLookup_not_mem
  intro hm
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hm
  exact hc.clears_ty hα
    (List.mem_append_left _ (List.mem_map_of_mem (f := fun p => (false, p.1)) hp))

theorem Finalize.row_free {S S' : SolverState B} {p : Parked B} {β : TyVar}
    (hf : Finalize S p S') (hβ : S.subst.row β = .var β) : S'.subst.row β = .var β := by
  cases hf with
  | star _ _ _ hs => exact solve_star_row_free hs hβ

theorem Finalizes.row_free {S S' : SolverState B} {ps : List (Parked B)} {β : TyVar} :
    Finalizes S ps S' → S.subst.row β = .var β → S'.subst.row β = .var β
  | .nil, h => h
  | .cons hf hfs, h => Finalizes.row_free hfs (hf.row_free h)

theorem Finalize.ext {S S' : SolverState B} {p : Parked B} (hf : Finalize S p S') :
    S.Ext S' := by
  cases hf with
  | star _ _ _ hs => exact hs.ext.trans (.of_sol_eq rfl)

theorem Finalizes.ext {S S' : SolverState B} {ps : List (Parked B)} :
    Finalizes S ps S' → S.Ext S'
  | .nil => .refl _
  | .cons hf hfs => hf.ext.trans (Finalizes.ext hfs)

--------------------- ONE STEP DISCHARGES ------------------------------------

/-- ⊢  **F-★ is D-?**, read at any σ that absorbs the step's start, satisfies
where it lands, and leaves the blocker a variable — at a key, one of its own. -/
theorem Finalize.holds {S S' : SolverState B} {p : Parked B} (hf : Finalize S p S')
    {σ : TySubst B} (hab : Absorbs σ S) (hsat : Sol.Sat σ S'.sol)
    (hβ : (true, p.blocker) ∈ (p.stump.row.applySubst S.subst).sortedFtv →
      ∃ β', σ.row p.blocker = .var β')
    (hk : (false, p.blocker) ∈ (p.stump.label.applySubst S.subst).sortedFtv ++
        (p.stump.row.applySubst S.subst).sortedFtv →
      KeyFresh σ ((p.stump.label.applySubst S.subst).sortedFtv ++
        (p.stump.row.applySubst S.subst).sortedFtv) p.blocker) :
    (p.stump.at σ).Holds := by
  cases hf with
  | star _ hb _ hs =>
      have hδ := (TyEquiv.unk_inv_both (hs.unifies_sat hsat)).2 rfl
      have hu := lookupQ_blocked_subst hb σ hβ hk
      rw [hab.row, hab.ty] at hu
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
        -- a type variable of the looked-up row or key is unsolved at S₀ (clean)
        have hv : ∀ {γ}, (false, γ) ∈ (p'.stump.label.applySubst S₀.subst).sortedFtv ++
            (p'.stump.row.applySubst S₀.subst).sortedFtv → S₀.subst.ty γ = .var γ := by
          intro γ h
          rcases List.mem_append.mp h with h | h
          · exact SolverState.ty_var_of_clean_ty hc h
          · exact SolverState.ty_var_of_clean_row hc h
        refine hf.holds hab (hab₁.sat c₁) (fun hmem => ⟨p'.blocker,
          hfs.row_free (hf.row_free (SolverState.row_var_of_clean hc hmem))⟩) ?_
        -- a key blocker is no pending answer (KeySafe), so no later step touches
        -- it; every other variable becomes itself or ★
        intro hkey
        have hsafe : p'.KeySafe S₀ := by cases hf with | star _ _ h _ => exact h
        have hu := hfs.untouched (hf.untouched ⟨hv hkey, hsafe hkey⟩)
        refine ⟨p'.blocker, hu.1, fun γ hγ he => ?_⟩
        rcases hfs.idOrStar (hf.idOrStar (.inr (hv hγ))) with h | h
        · rw [h] at he; cases he
        · rw [h] at he; injection he
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

--------------------- THE χ-CORRECTION IS FALSE IN GENERAL ----------------------
-- Two constraints on the SAME result variable whose lookups find ≈-equal but
-- syntactically different types: one χ discharges both up to ≈, but an exact
-- instance would have to send δ to both at once.

private def icA : Ty Unit := .rcd (.cat (.sing "a" (.base ())) (.sing "b" (.base ())))
private def icB : Ty Unit := .rcd (.cat (.sing "b" (.base ())) (.sing "a" (.base ())))
private def icSc : QScheme Unit :=
  ⟨["d"], [⟨.sing "l" icA, .lab "l", .var "d"⟩, ⟨.sing "l" icB, .lab "l", .var "d"⟩], .var "d"⟩
private def icχ : TySubst Unit := ⟨fun x => if x = "d" then icA else .var x, fun x => .var x⟩

private theorem icA_equiv_icB : TyEquiv icA icB :=
  .rcd (.comm (by decide))

/-- ⊢  **`InstEquivCorrects` is false**: the correction needs one constraint per
result variable, at least — which is why it is stated for `Correctable` schemes
(`QScheme.Correctable.correct`) and A-let builds only those. -/
theorem instEquivCorrects_false : ¬ InstEquivCorrects Unit := by
  intro h
  obtain ⟨τ', ⟨θ, -, hdis, -⟩, -⟩ := h icSc icχ
    ⟨fun α hα => by
        have : α ≠ "d" := fun he => hα (by simp [icSc, he])
        simp [icχ, this], fun _ _ => rfl⟩
    (fun st hst => by
      simp only [icSc, List.mem_cons, List.not_mem_nil, or_false] at hst
      rcases hst with rfl | rfl
      · exact .hit (τ := icA) (LookupQ.lab_iff.mpr .hit) (by simp [icχ]; exact .refl _)
      · exact .hit (τ := icB) (LookupQ.lab_iff.mpr .hit) (by simp [icχ]; exact icA_equiv_icB))
  have h1 := hdis ⟨.sing "l" icA, .lab "l", .var "d"⟩ (by simp [icSc])
  have h2 := hdis ⟨.sing "l" icB, .lab "l", .var "d"⟩ (by simp [icSc])
  cases h1 with
  | hit hl₁ he₁ =>
    cases h2 with
    | hit hl₂ he₂ =>
        cases lookup_det (LookupQ.lab_iff.mp hl₁) .hit
        cases lookup_det (LookupQ.lab_iff.mp hl₂) .hit
        have : icA = icB := he₁.symm.trans he₂
        simp only [icA, icB, Ty.rcd.injEq, Row.cat.injEq, Row.sing.injEq] at this
        exact absurd this.1.1 (by decide)
    | abs hl _ => exact nomatch lookup_det (LookupQ.lab_iff.mp hl) .hit
    | unk hl _ => exact nomatch lookup_det (LookupQ.lab_iff.mp hl) .hit
  | abs hl _ => exact nomatch lookup_det (LookupQ.lab_iff.mp hl) .hit
  | unk hl _ => exact nomatch lookup_det (LookupQ.lab_iff.mp hl) .hit

end MinimalCalculus
