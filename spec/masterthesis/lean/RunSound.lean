-- THE TOP LEVEL, AND `RunSound` — PROVED.
--
-- A run answers the scheme read at the merged state (`Run`, Infer.lean). Two
-- things are owed: every instance types the program, and there is one.
--
--   * an instance χ is the substitution χ ∘ ⟦S₂⟧, which absorbs S₂ and every
--     state before it. A stump still parked after the merge discharges at χ;
--     one the merge's saturation discharged holds at every σ that satisfies S₂
--     (`Merges.keeps`). `runSoundA` (LetCase.lean) does the rest.
--   * the instance is `LetResults.inhabited` at the identity reading, found at
--     a scratch copy of S₂ (`Guesses`) and carried back (`inhabited_of_ext`).
--
-- The merge is forced (`merge_forced`): two stumps that ask one lookup get
-- ≈-equal results at every instance that discharges both.
-- (The file was Finalization.lean while the top level still finalized.)

import LetCase

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- THE MERGE KEEPS, AND IS FORCED ---------------------------
-- M-merge is an equation plus saturation, and inherits `KeepsS` from
-- `SolveTySat`: every stump parked before it is still parked after it, or
-- discharged under any σ satisfying the state it ends in.

/-- ⊢  one merge keeps every stump and cleanliness, and extends. -/
theorem Merge.keeps {S S' : SolverState B} (hm : Merge S S') (hc : S.sol.Clean) :
    S.KeepsS S' ∧ S'.sol.Clean ∧ S.Ext S' ∧ S.SatMono S' := by
  cases hm with
  | merge _ _ _ _ hs => exact ⟨hs.keeps, hs.clean hc, hs.ext, hs.satMono⟩

/-- ⊢  …and so do any number of them. -/
theorem Merges.keeps {S S' : SolverState B} :
    Merges S S' → S.sol.Clean →
      S.KeepsS S' ∧ S'.sol.Clean ∧ S.Ext S' ∧ S.SatMono S'
  | .refl _, hc => ⟨.refl _, hc, .refl _, .refl _⟩
  | .step hm hms, hc => by
      obtain ⟨k₁, c₁, x₁, m₁⟩ := hm.keeps hc
      obtain ⟨k₂, c₂, x₂, m₂⟩ := Merges.keeps hms c₁
      exact ⟨k₁.trans k₂ m₂, c₂, x₁.trans x₂, m₁.trans m₂⟩

/-- ⊢  …and so does the witness search: every step is an equation plus
saturation. -/
theorem Guesses.keeps {S S' : SolverState B} :
    Guesses S S' → S.sol.Clean →
      S.KeepsS S' ∧ S'.sol.Clean ∧ S.Ext S' ∧ S.SatMono S'
  | .refl _, hc => ⟨.refl _, hc, .refl _, .refl _⟩
  | .step (.solve hs) hgs, hc => by
      obtain ⟨k₂, c₂, x₂, m₂⟩ := Guesses.keeps hgs (hs.clean hc)
      exact ⟨hs.keeps.trans k₂ m₂, c₂, hs.ext.trans x₂, hs.satMono.trans m₂⟩

/-- ⊢  **the merge is forced.** Two stumps with one spine and one key ask one
lookup, so every θ that discharges both sends their results to ≈-equal types:
`LookupQ` respects ≈ and is deterministic, D-hit holds up to ≈, and D-⊥ and D-?
both give ★. M-merge applies this to the stumps read at the state. -/
theorem merge_forced {st₁ st₂ : Stump B} (hrow : st₁.row.toSpine = st₂.row.toSpine)
    (hkey : st₁.label = st₂.label) {θ : TySubst B}
    (h₁ : st₁.Discharge θ) (h₂ : st₂.Discharge θ) :
    TyEquiv (st₁.res.applySubst θ) (st₂.res.applySubst θ) := by
  have he : RowEquiv (st₁.row.applySubst θ) (st₂.row.applySubst θ) :=
    RowEquiv.applySubst θ ((Row.toSpine_equiv st₁.row).trans
      (hrow ▸ (Row.toSpine_equiv st₂.row).symm))
  -- st₁'s lookup, carried to st₂'s row
  have carry : ∀ {r}, LookupQ (st₁.row.applySubst θ) (st₁.label.applySubst θ) r →
      ∀ {r₂}, LookupQ (st₂.row.applySubst θ) (st₂.label.applySubst θ) r₂ → ResEquiv r r₂ := by
    intro r hl r₂ hl₂
    obtain ⟨r', hl', hr'⟩ := LookupQ.equiv he hl
    rw [hkey] at hl'
    exact (LookupQ.det hl' hl₂) ▸ hr'
  cases h₁ with
  | hit hl₁ e₁ =>
      cases h₂ with
      | hit hl₂ e₂ =>
          cases carry hl₁ hl₂ with
          | found e => exact e₁.trans (e.trans e₂.symm)
      | abs hl₂ _ => cases carry hl₁ hl₂
      | unk hl₂ _ => cases carry hl₁ hl₂
  | abs hl₁ e₁ =>
      cases h₂ with
      | hit hl₂ _ => cases carry hl₁ hl₂
      | abs _ e₂ => rw [e₁, e₂]; exact .refl _
      | unk hl₂ _ => cases carry hl₁ hl₂
  | unk hl₁ e₁ =>
      cases h₂ with
      | hit hl₂ _ => cases carry hl₁ hl₂
      | abs hl₂ _ => cases carry hl₁ hl₂
      | unk _ e₂ => rw [e₁, e₂]; exact .refl _

--------------------- RunSound -------------------------------------------------
-- Every instance: an instance χ of the scheme read at S₂ is the substitution
-- χ ∘ ⟦S₂⟧, which absorbs S₂ and so every state before it. A stump parked at
-- the end of inference either is still parked after the merge, and its
-- constraint discharges at χ, or the merge's saturation discharged it.
-- One instance: `LetResults.inhabited` at the identity reading, which renames
-- nothing because nothing outside the scheme shares its names.

/-- ⊢  χ ∘ ⟦S⟧ absorbs a clean S. -/
theorem Absorbs.comp {χ : TySubst B} {S : SolverState B} (hc : S.sol.Clean) :
    Absorbs (χ.comp S.subst) S := by
  intro α
  obtain ⟨hi₁, hi₂, hi₃⟩ := SolverState.idem hc α
  refine ⟨?_, ?_, ?_⟩
  · show (S.subst.ty α).applySubst (χ.comp S.subst) = (S.subst.ty α).applySubst χ
    rw [← Ty.applySubst_applySubst, hi₁]
  · show (S.subst.row α).applySubst (χ.comp S.subst) = (S.subst.row α).applySubst χ
    rw [← Row.applySubst_applySubst, hi₂]
  · show (S.subst.lab α).applySubst (χ.comp S.subst) = (S.subst.lab α).applySubst χ
    rw [← Key.applySubst_applySubst, hi₃]

/-- ⊢  a stump read at S, then at χ, is the stump at χ ∘ ⟦S⟧. -/
theorem Stump.at_read (χ : TySubst B) (S : SolverState B) (st : Stump B) :
    Stump.at χ ⟨st.row.applySubst S.subst, st.label.applySubst S.subst,
      st.res.applySubst S.subst⟩ = st.at (χ.comp S.subst) := by
  simp only [Stump.at, Row.applySubst_applySubst, Key.applySubst_applySubst,
    Ty.applySubst_applySubst]

/-- ⊢  read under the identity, with its binders kept, a scheme is itself. -/
theorem QScheme.readAt_id (sc : QScheme B) : sc.readAt (TySubst.id B) id = sc := by
  have h : readSub (TySubst.id B) sc.vars id = TySubst.id B := by
    simp only [readSub, TySubst.id, id, ite_self]
  obtain ⟨vs, cs, b⟩ := sc
  simp only [QScheme.readAt, h, Ty.applySubst_id, Row.applySubst_id, Key.applySubst_id,
    List.map_id_fun, id_eq]
  congr 1
  conv => rhs; rw [← List.map_id cs]
  rfl

/-- ⊢  **the top-level scheme has an instance** when `LetSpent` holds:
`LetResults.inhabited` at the identity reading. Every blocker and every result
variable is bound, since ᾱ is all of ftv. -/
theorem top_inhabited {S : SolverState B} (q : S.Quiescent) (τ : Ty B)
    (hsp : LetSpent S (letCand S τ) S.parked) :
    ∃ τ', (letScheme S (letCand S τ) S.parked τ).Inst τ' := by
  obtain ⟨τ', h⟩ := LetResults.inhabited τ
    ⟨fun p hp δ hδ => mem_letCand_parked hp (.inr (.inl hδ)), hsp⟩
    (fun p hp => (q p hp).mem_ftv.elim (fun h => mem_letCand_parked hp (.inl h))
      (fun h => mem_letCand_parked hp (.inr (.inr h))))
    q (TySubst.id B) id (fun _ _ _ _ h => h)
    (fun p hp γ hγ hn β hβ hm => by
      simp only [TySubst.id, Key.ftv, List.mem_singleton] at hβ
      subst hβ
      exact hn (by simpa using hm))
  rw [QScheme.readAt_id] at h
  exact ⟨τ', h⟩

/-- ⊢  **a witness at an extension is a witness.** If S′ extends S, keeping its
stumps, an instance χ′ of the scheme read at S′ gives one of the scheme read at
S: χ′ ∘ ⟦S′⟧ on S's variables. A stump of S still parked at S′ discharges by χ′;
one that S′'s saturation discharged holds at every substitution that satisfies
S′. So the search for a witness may change the state at will (`Guesses`). -/
theorem inhabited_of_ext {S S' : SolverState B} (hc : S.sol.Clean) (hk : S.KeepsS S')
    (hx : S.Ext S') (hc' : S'.sol.Clean) (τ : Ty B)
    (h : ∃ τ', (letScheme S' (letCand S' τ) S'.parked τ).Inst τ') :
    ∃ τ', (letScheme S (letCand S τ) S.parked τ).Inst τ' := by
  obtain ⟨-, χ', -, hdis, -⟩ := h
  have hab' : Absorbs (χ'.comp S'.subst) S' := Absorbs.comp hc'
  have hab : Absorbs (χ'.comp S'.subst) S := hab'.back hx hc
  let σ' := χ'.comp S'.subst
  let χ : TySubst B :=
    ⟨fun α => if α ∈ letCand S τ then σ'.ty α else .var α,
     fun α => if α ∈ letCand S τ then σ'.row α else .var α,
     fun α => if α ∈ letCand S τ then σ'.lab α else .var α⟩
  have hag : ∀ α ∈ letCand S τ, χ.ty α = σ'.ty α ∧ χ.row α = σ'.row α ∧ χ.lab α = σ'.lab α :=
    fun α h => by simp [χ, h]
  refine ⟨(τ.applySubst S.subst).applySubst χ, χ,
    ⟨fun α h => if_neg h, fun α h => if_neg h, fun α h => if_neg h⟩, fun st hst => ?_, rfl⟩
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst
  rw [Stump.dischargeEquiv_iff_holds]
  have e : Stump.at χ ⟨p.stump.row.applySubst S.subst, p.stump.label.applySubst S.subst,
      p.stump.res.applySubst S.subst⟩ = p.stump.at σ' := by
    simp only [Stump.at]
    congr 1
    · rw [Row.applySubst_congr _ (fun α hα => hag α (mem_letCand_parked hp (.inl hα))), hab.row]
    · rw [Key.applySubst_congr _ (fun α hα =>
        (hag α (mem_letCand_parked hp (.inr (.inr hα)))).2.2), hab.key]
    · rw [Ty.applySubst_congr _ (fun α hα => hag α (mem_letCand_parked hp (.inr (.inl hα)))),
        hab.ty]
  rw [e]
  rcases hk σ' (hab'.sat hc') p hp with ⟨q, hq, hqs⟩ | hd
  · rw [← hqs, ← Stump.at_read]
    exact Stump.dischargeEquiv_iff_holds.mp (hdis _ (List.mem_map.mpr ⟨q, hq, rfl⟩))
  · exact Stump.dischargeEquiv_iff_holds.mp hd

/-- ⊢  **ALGORITHM SOUNDNESS — `RunSound`.** Every instance
of the scheme a run answers types the program in the empty context, and the
scheme has an instance. -/
theorem runSound {C : Type} {constTy : Sig B C} : RunSound B C constTy := by
  rintro e σ ⟨τ, S₁, S₂, S₃, hinf, hmer, rfl, hkg, hsp⟩
  have c₁ := Infer.clean hinf Sol.clean_nil
  obtain ⟨k₁₂, c₂, x₁₂, -⟩ := hmer.keeps c₁
  have q₂ : S₂.Quiescent := hmer.quiescent (Infer.quiescent_of_nil hinf rfl)
  refine ⟨fun τ' ⟨χ, _, hdis, hbody⟩ => ?_, ?_⟩
  · ---------------------------------------------- every instance types e
    have hab₂ : Absorbs (χ.comp S₂.subst) S₂ := Absorbs.comp c₂
    have hab₁ := hab₂.back x₁₂ c₁
    have ht := runSoundA hinf hab₁ (hab₁.sat c₁) (fun p hp => ?_)
    · rw [← Ty.applySubst_applySubst] at ht
      exact hbody ▸ ht
    · rcases k₁₂ _ (hab₂.sat c₂) p hp with ⟨q, hq, hqs⟩ | hd
      · rw [← hqs, ← Stump.at_read]
        exact Stump.dischargeEquiv_iff_holds.mp (hdis _ (List.mem_map.mpr ⟨q, hq, rfl⟩))
      · exact Stump.dischargeEquiv_iff_holds.mp hd
  · ---------------------------------------------- the scheme has an instance
    obtain ⟨k₂₃, -, x₂₃, -⟩ := hkg.keeps c₂
    exact inhabited_of_ext c₂ k₂₃ x₂₃ (hkg.keeps c₂).2.1 τ
      (top_inhabited (hkg.quiescent q₂) τ hsp)

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
