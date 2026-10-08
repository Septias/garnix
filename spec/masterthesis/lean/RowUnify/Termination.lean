-- TERMINATION: every problem has a fuel at which the driver answers.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- ## The measure
-- Lexicographic, (distinct problem variables, problem size):
--
--  * a FORCED move (strip, match, ground-match, the first stage of an arm)
--    returns sub-spines and payloads of its input — no new variable, strictly
--    smaller size;
--  * a SOLVED first stage with at least one binding drops its keys from the
--    residual: `Sol.Good.clears` (Applied.lean) says every residual variable
--    is a problem variable or a drawn name, and not a key. A nested U-host may
--    have drawn names, but fewer than it bound keys (`unifyM_draws`,
--    Draws.lean), so the first component drops (`nv_residual`), however much
--    `sApplySubst` grows the spine;
--  * a solved first stage with NO binding is the identity, so the residual is
--    the unsubstituted one, which is strictly smaller;
--  * U-HOST trades its host β for the draw β′ and drops the leading field: the
--    count stays, the size drops.
--
-- Variables are counted on the problem itself (`nv`, distinct tagged names),
-- from a supply above it (`Below`): the draws are then new names, and the
-- count of a residual can be compared with its problem's.
--
-- ## The statement
-- `outOfFuel` is the driver's only non-answer, and the fuel lemma
-- (`unifyM_fuel_mono`) makes every other verdict fuel-independent. So
-- termination is `∃ fuel, … ≠ .outOfFuel`, and it turns ≐ᵣ into a total
-- function (`unifyRow`, below) whose verdict is THE verdict.

import RowUnify.Draws

namespace MinimalCalculus

------------------------- SIZE ---------------------------------------------------

mutual
def Ty.usize {B : Type} : Ty B → Nat
  | .var _   => 1
  | .base _  => 1
  | .lab _  => 1
  | .unk     => 1
  | .fn a b  => 1 + Ty.usize a + Ty.usize b
  | .rcd ρ   => 1 + Row.usize ρ

def Row.usize {B : Type} : Row B → Nat
  | .empty     => 0
  | .var _     => 1
  | .sing _ τ  => 1 + Ty.usize τ
  | .cat ρ₁ ρ₂ => Row.usize ρ₁ + Row.usize ρ₂
  | .dsing _ τ => 1 + Ty.usize τ    -- a key is read by class only: not counted
end

def spineSize {B : Type} : List (Atom B) → Nat
  | [] => 0
  | .field _ τ :: s => 1 + τ.usize + spineSize s
  | .var _ :: s     => 1 + spineSize s
  | .dfield _ τ :: s => 1 + τ.usize + spineSize s

theorem spineSize_append {B : Type} :
    (s t : List (Atom B)) → spineSize (s ++ t) = spineSize s + spineSize t
  | [], _ => by simp [spineSize]
  | .field _ τ :: s, t => by
      simp only [List.cons_append, spineSize, spineSize_append s t]; omega
  | .var _ :: s, t => by
      simp only [List.cons_append, spineSize, spineSize_append s t]; omega
  | .dfield _ τ :: s, t => by
      simp only [List.cons_append, spineSize, spineSize_append s t]; omega

theorem spineSize_toSpine {B : Type} : (ρ : Row B) → spineSize ρ.toSpine = ρ.usize
  | .empty => rfl
  | .var _ => rfl
  | .sing _ τ => by simp [Row.toSpine, spineSize, Row.usize]
  | .cat ρ₁ ρ₂ => by
      simp only [Row.toSpine, spineSize_append, spineSize_toSpine ρ₁, spineSize_toSpine ρ₂,
        Row.usize]
  | .dsing q τ => by cases q <;> simp [Row.toSpine, Atom.ofKey, spineSize, Row.usize]

theorem spineSize_reverse {B : Type} : (s : List (Atom B)) →
    spineSize s.reverse = spineSize s
  | [] => rfl
  | .field _ τ :: s => by
      simp only [List.reverse_cons, spineSize_append, spineSize_reverse s, spineSize]; omega
  | .var _ :: s => by
      simp only [List.reverse_cons, spineSize_append, spineSize_reverse s, spineSize]; omega
  | .dfield _ τ :: s => by
      simp only [List.reverse_cons, spineSize_append, spineSize_reverse s, spineSize]; omega

------------------------- THE DETECTORS SHRINK THE PROBLEM ---------------------

theorem stripL_size {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripL s₁ s₂ = some (t₁, t₂)) :
    spineSize s₁ = spineSize t₁ + 1 ∧ spineSize s₂ = spineSize t₂ + 1 := by
  match s₁, s₂ with
  | .var α :: u₁, .var β :: u₂ =>
      simp only [stripL] at h
      by_cases hab : α = β
      · rw [if_pos hab] at h; cases h
        simp only [spineSize]; omega
      · rw [if_neg hab] at h; cases h

theorem stripR_size {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripR s₁ s₂ = some (t₁, t₂)) :
    spineSize s₁ = spineSize t₁ + 1 ∧ spineSize s₂ = spineSize t₂ + 1 := by
  unfold stripR at h
  revert h
  cases hl : stripL s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      obtain ⟨g₁, g₂⟩ := stripL_size hl
      rw [spineSize_reverse] at g₁ g₂
      rw [spineSize_reverse, spineSize_reverse]
      exact ⟨g₁, g₂⟩

theorem windowExtract_size {B : Type} {l : Label} :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') → spineSize s = spineSize s' + 1 + τ.usize
  | .field l' τ' :: t, τ, s', h => by
      simp only [windowExtract] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h
        cases h
        simp only [spineSize]; omega
      · rw [if_neg hl] at h
        revert h
        cases hw : windowExtract l t with
        | none => intro h; cases h
        | some p =>
            intro h
            have e := windowExtract_size t hw
            cases h
            simp only [spineSize]; omega

theorem removeField_size {B : Type} {l : Label} :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    removeField l s = some (τ, s') → spineSize s = spineSize s' + 1 + τ.usize
  | .var β :: t, τ, s', h => by
      simp only [removeField] at h
      revert h
      cases hw : removeField l t with
      | none => intro h; cases h
      | some p =>
          intro h
          have e := removeField_size t hw
          cases h
          simp only [spineSize]; omega
  | .dfield o σ :: t, τ, s', h => by
      simp only [removeField] at h
      revert h
      cases hw : removeField l t with
      | none => intro h; cases h
      | some p =>
          intro h
          have e := removeField_size t hw
          cases h
          simp only [spineSize]; omega
  | .field l' τ' :: t, τ, s', h => by
      simp only [removeField] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h
        cases h
        simp only [spineSize]; omega
      · rw [if_neg hl] at h
        revert h
        cases hw : removeField l t with
        | none => intro h; cases h
        | some p =>
            intro h
            have e := removeField_size t hw
            cases h
            simp only [spineSize]; omega

/-- An eq-emitting detector removes one field from each side. -/
def EqSize {B : Type} (s₁ s₂ : List (Atom B)) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) :
    Prop :=
  spineSize s₁ = spineSize t₁ + 1 + τ.usize ∧ spineSize s₂ = spineSize t₂ + 1 + τ'.usize

theorem EqSize.swap {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : EqSize s₁ s₂ τ τ' t₁ t₂) : EqSize s₂ s₁ τ' τ t₂ t₁ :=
  ⟨h.2, h.1⟩

theorem matchL_size {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    EqSize s₁ s₂ τ τ' t₁ t₂ := by
  match s₁ with
  | .field l σ :: u₁ =>
      simp only [matchL] at h
      revert h
      cases hw : windowExtract l s₂ with
      | none => intro h; cases h
      | some p =>
          intro h
          cases h
          exact ⟨by simp only [spineSize]; omega, windowExtract_size s₂ hw⟩
  | .dfield α σ :: u₁ =>
      simp only [matchL] at h
      revert h
      cases hw : keyExtract α s₂ with
      | none => intro h; cases h
      | some p =>
          intro h
          cases h
          rw [keyExtract_inv hw]
          exact ⟨by simp only [spineSize]; omega, by simp only [spineSize]; omega⟩

theorem matchR_size {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    EqSize s₁ s₂ τ τ' t₁ t₂ := by
  unfold matchR at h
  revert h
  cases hl : matchL s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨σ0, σ0', u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl, rfl⟩ := h
      obtain ⟨g₁, g₂⟩ := matchL_size hl
      rw [spineSize_reverse] at g₁ g₂
      exact ⟨by rw [spineSize_reverse]; exact g₁, by rw [spineSize_reverse]; exact g₂⟩

theorem groundMatchAux_size {B : Type} {s₁ s₂ : List (Atom B)} :
    (ls : List Label) → {τ τ' : Ty B} → {t₁ t₂ : List (Atom B)} →
    groundMatchAux s₁ s₂ ls = some (τ, τ', t₁, t₂) → EqSize s₁ s₂ τ τ' t₁ t₂
  | l :: ls, τ, τ', t₁, t₂, h => by
      simp only [groundMatchAux] at h
      by_cases hc : sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁
      · rw [if_pos hc] at h
        revert h
        cases h₁ : removeField l s₁ with
        | none => intro h; exact groundMatchAux_size ls (by simpa [h₁] using h)
        | some p₁ =>
            cases h₂ : removeField l s₂ with
            | none => intro h; exact groundMatchAux_size ls (by simpa [h₁, h₂] using h)
            | some p₂ =>
                intro h
                cases h
                exact ⟨removeField_size s₁ h₁, removeField_size s₂ h₂⟩
      · rw [if_neg hc] at h
        exact groundMatchAux_size ls h

theorem groundMatch_size {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    EqSize s₁ s₂ τ τ' t₁ t₂ := by
  simp only [groundMatch] at h
  by_cases hv : sHasVar s₂
  · rw [if_pos hv] at h; cases h
  · rw [if_neg hv] at h; exact groundMatchAux_size _ h

------------------------- THE PROBLEM'S VARIABLES -------------------------------

def TyP {B : Type} (τ τ' : Ty B) : List (Srt × TyVar) := Ty.sortedFtv τ ++ Ty.sortedFtv τ'
def SpP {B : Type} (s₁ s₂ : List (Atom B)) : List (Srt × TyVar) := sSorted s₁ ++ sSorted s₂

------------------------- A KEYLESS SOLUTION IS THE IDENTITY -------------------

theorem Sol.toSubst_of_domS_nil {B : Type} {s : Sol B} (h : s.domS = []) :
    (∀ α, s.toSubst.ty α = .var α) ∧ (∀ α, s.toSubst.row α = .var α) ∧
      (∀ α, s.toSubst.lab α = .var α) := by
  simp only [Sol.domS, List.append_eq_nil_iff, List.map_eq_nil_iff] at h
  obtain ⟨⟨h₁, h₂⟩, h₃⟩ := h
  exact ⟨fun α => by simp [Sol.toSubst, h₁, tyLookup],
         fun α => by simp [Sol.toSubst, h₂, rowLookup],
         fun α => by simp [Sol.toSubst, h₃, labLookup]⟩

theorem sApplySubst_fixed {B : Type} {θ : TySubst B} (ht : ∀ α, θ.ty α = .var α)
    (hr : ∀ α, θ.row α = .var α) (hl : ∀ α, θ.lab α = .var α) :
    (t : List (Atom B)) → sApplySubst θ t = t
  | [] => rfl
  | .field l τ :: t => by
      simp only [sApplySubst, Ty.applySubst_fixed_sorted τ (fun α _ => ht α) (fun α _ => hr α)
        (fun α _ => hl α), sApplySubst_fixed ht hr hl t]
  | .var α :: t => by
      simp only [sApplySubst, hr α, Row.toSpine, List.singleton_append,
        sApplySubst_fixed ht hr hl t]
  | .dfield o τ :: t => by
      simp only [sApplySubst, Ty.applySubst_fixed_sorted τ (fun α _ => ht α) (fun α _ => hr α)
        (fun α _ => hl α), sApplySubst_fixed ht hr hl t, hl o, Atom.ofKey]

------------------------- VERDICTS THAT ARE NOT `outOfFuel` --------------------

theorem UResM.seq_ne_oof {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (hr : r ≠ .outOfFuel) (hk : ∀ s S, r = .success s S → k s.toSubst S ≠ .outOfFuel) :
    r.seq k ≠ .outOfFuel := by
  cases r with
  | success s S =>
      have := hk s S rfl
      simp only [UResM.seq]
      revert this
      cases k s.toSubst S <;> simp
  | clash => simp [UResM.seq]
  | occurs => simp [UResM.seq]
  | stuck => simp [UResM.seq]
  | outOfFuel => exact absurd rfl hr

theorem bindTy_ne_oof {B : Type} (S : Supply) (α : TyVar) (τ : Ty B) :
    bindTy S α τ ≠ .outOfFuel := by
  unfold bindTy; split
  · simp
  · split <;> simp

theorem solveVarM_ne_oof {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)} {r : UResM B}
    (h : solveVarM S s₁ s₂ = some r) : r ≠ .outOfFuel := by
  match s₁, h with
  | [.var α], h =>
      simp only [solveVarM, Option.some.injEq] at h
      subst h
      split
      · simp
      · split
        · split <;> simp
        · simp

theorem mono_ty_eq {B : Type} [DecidableEq B] {S : Supply} {f F : Nat} {τ τ' : Ty B}
    (hle : f ≤ F) (h : unifyTyF S f τ τ' ≠ .outOfFuel) :
    unifyTyF S F τ τ' = unifyTyF S f τ τ' := by
  rcases (unifyM_fuel_mono f f (Nat.le_refl _) F hle).1 S τ τ' with h' | h'
  · exact absurd h' h
  · exact h'

theorem mono_sp_eq {B : Type} [DecidableEq B] {S : Supply} {f F : Nat}
    {s₁ s₂ : List (Atom B)} (hle : f ≤ F) (h : unifySpineMF S f s₁ s₂ ≠ .outOfFuel) :
    unifySpineMF S F s₁ s₂ = unifySpineMF S f s₁ s₂ := by
  rcases (unifyM_fuel_mono f f (Nat.le_refl _) F hle).2 S s₁ s₂ with h' | h'
  · exact absurd h' h
  · exact h'

------------------------- THE INDUCTION ----------------------------------------

/-- Every problem below the supply with at most `n` distinct variables and size
at most `m` has a fuel at which the driver answers — at both sorts. -/
def TermR (B : Type) [DecidableEq B] (n m : Nat) : Prop :=
  (∀ (S : Supply) (τ τ' : Ty B), Below S (TyP τ τ') → nv (TyP τ τ') ≤ n →
      τ.usize + τ'.usize ≤ m → ∃ f, unifyTyF S f τ τ' ≠ .outOfFuel) ∧
  (∀ (S : Supply) (s₁ s₂ : List (Atom B)), Below S (SpP s₁ s₂) → nv (SpP s₁ s₂) ≤ n →
      spineSize s₁ + spineSize s₂ ≤ m → ∃ f, unifySpineMF S f s₁ s₂ ≠ .outOfFuel)

-- ⊢  A SOLVED STAGE WITH A KEY leaves a residual with fewer variables than the
--    stage's problem `Pv`: residual and keys lie in `Pv` plus the draws, and
--    there are fewer draws than keys
theorem stage_count {B : Type} {S S₁ : Supply} {Pv Res R₁ : List (Srt × TyVar)} {s₁ : Sol B}
    (hf₁ : FreshIn S S₁ R₁) (g₁ : s₁.Good (Pv ++ R₁))
    (hRes : ∀ x ∈ Res, x ∈ Pv ++ R₁ ∧ x ∉ s₁.domS) (hS₁ : S.next ≤ S₁.next)
    (hdr : S₁.next < S.next + nv s₁.domS) : nv Res < nv Pv := by
  have h := nv_residual (Q := Pv) (D := drawn S S₁) (K := s₁.domS) (Res := Res)
    (fun x hx => (List.mem_append.mp (g₁.dom x hx)).imp id hf₁.mem_drawn)
    (fun x hx => ⟨(List.mem_append.mp (hRes x hx).1).imp id hf₁.mem_drawn, (hRes x hx).2⟩)
  rw [drawn_length] at h
  omega

-- ⊢  AN EQ-EMITTING ARM whose residual is a spine pair. The first stage is a
--    strictly smaller problem over no new variables; the second is either the
--    unsubstituted residual (no key: strictly smaller) or has fewer variables.
theorem arm_sp_terminates {B : Type} [DecidableEq B] {Pv : List (Srt × TyVar)} {n m : Nat}
    (ihn : ∀ n' < n, ∀ m', TermR B n' m') (ihm : ∀ m' < m, TermR B n m')
    {S : Supply} (hB : Below S Pv) (hPn : nv Pv ≤ n) (τ τ' : Ty B)
    (t₁ t₂ : List (Atom B)) (hT : TyP τ τ' ⊆ Pv) (hR : SpP t₁ t₂ ⊆ Pv)
    (hsT : τ.usize + τ'.usize < m) (hsR : spineSize t₁ + spineSize t₂ < m) :
    ∃ F, ((unifyTyF S F τ τ').seq fun θ S'' =>
        unifySpineMF S'' F (sApplySubst θ t₁) (sApplySubst θ t₂)) ≠ .outOfFuel := by
  have hBT : Below S (TyP τ τ') := fun x hx => hB x (hT hx)
  obtain ⟨f₁, h₁⟩ := (ihm _ hsT).1 S τ τ' hBT (Nat.le_trans (nv_mono hT) hPn) (Nat.le_refl _)
  cases hr : unifyTyF S f₁ τ τ' with
  | success s₁ S₁ =>
      have hS₁ := (unifyM_supply_mono f₁).1 _ _ _ hr
      obtain ⟨R₁, hf₁, g₁⟩ := (unifyM_good f₁).1 S τ τ' hBT hr
      have g₁' := g₁.freshMono hT
      have hRes : ∀ x ∈ SpP (sApplySubst s₁.toSubst t₁) (sApplySubst s₁.toSubst t₂),
          x ∈ Pv ++ R₁ ∧ x ∉ s₁.domS := by
        intro x hx
        rcases List.mem_append.mp hx with hx | hx
        · exact g₁'.clears_spine (fun _ hy => List.mem_append_left _
            (hR (List.mem_append_left _ hy))) hx
        · exact g₁'.clears_spine (fun _ hy => List.mem_append_left _
            (hR (List.mem_append_right _ hy))) hx
      obtain ⟨f₂, h₂⟩ : ∃ f₂, unifySpineMF S₁ f₂ (sApplySubst s₁.toSubst t₁)
          (sApplySubst s₁.toSubst t₂) ≠ .outOfFuel := by
        by_cases hd : s₁.domS = []
        · obtain ⟨ht, hrw, hlb⟩ := Sol.toSubst_of_domS_nil hd
          rw [sApplySubst_fixed ht hrw hlb, sApplySubst_fixed ht hrw hlb]
          exact (ihm _ hsR).2 S₁ t₁ t₂
            (Below.residual hB hS₁ hf₁ (fun x hx => List.mem_append_left _ (hR hx)))
            (Nat.le_trans (nv_mono hR) hPn) (Nat.le_refl _)
        · have hdr := ((unifyM_draws f₁).1 S τ τ' hBT hr).resolve_right (fun h => hd h.1)
          exact (ihn _ (Nat.lt_of_lt_of_le (stage_count hf₁ g₁' hRes hS₁ hdr) hPn) _).2 S₁ _ _
            (Below.residual hB hS₁ hf₁ (fun x hx => (hRes x hx).1)) (Nat.le_refl _)
            (Nat.le_refl _)
      refine ⟨max f₁ f₂, ?_⟩
      have e₁ : unifyTyF S (max f₁ f₂) τ τ' = .success s₁ S₁ := by
        rw [mono_ty_eq (Nat.le_max_left _ _) h₁, hr]
      rw [e₁]
      refine UResM.seq_ne_oof (by simp) (fun s S he => ?_)
      simp only [UResM.success.injEq] at he
      obtain ⟨rfl, rfl⟩ := he
      rw [mono_sp_eq (Nat.le_max_right _ _) h₂]
      exact h₂
  | clash => exact ⟨f₁, by rw [hr]; simp [UResM.seq]⟩
  | occurs => exact ⟨f₁, by rw [hr]; simp [UResM.seq]⟩
  | stuck => exact ⟨f₁, by rw [hr]; simp [UResM.seq]⟩
  | outOfFuel => exact absurd hr h₁

-- ⊢  …and the arrow arm, whose residual is a type pair
theorem arm_ty_terminates {B : Type} [DecidableEq B] {Pv : List (Srt × TyVar)} {n m : Nat}
    (ihn : ∀ n' < n, ∀ m', TermR B n' m') (ihm : ∀ m' < m, TermR B n m')
    {S : Supply} (hB : Below S Pv) (hPn : nv Pv ≤ n) (a₁ a₂ b₁ b₂ : Ty B)
    (hT : TyP a₁ a₂ ⊆ Pv) (hR : TyP b₁ b₂ ⊆ Pv)
    (hsT : a₁.usize + a₂.usize < m) (hsR : b₁.usize + b₂.usize < m) :
    ∃ F, ((unifyTyF S F a₁ a₂).seq fun θ S'' =>
        unifyTyF S'' F (b₁.applySubst θ) (b₂.applySubst θ)) ≠ .outOfFuel := by
  have hBT : Below S (TyP a₁ a₂) := fun x hx => hB x (hT hx)
  obtain ⟨f₁, h₁⟩ := (ihm _ hsT).1 S a₁ a₂ hBT (Nat.le_trans (nv_mono hT) hPn) (Nat.le_refl _)
  cases hr : unifyTyF S f₁ a₁ a₂ with
  | success s₁ S₁ =>
      have hS₁ := (unifyM_supply_mono f₁).1 _ _ _ hr
      obtain ⟨R₁, hf₁, g₁⟩ := (unifyM_good f₁).1 S a₁ a₂ hBT hr
      have g₁' := g₁.freshMono hT
      have hRes : ∀ x ∈ TyP (b₁.applySubst s₁.toSubst) (b₂.applySubst s₁.toSubst),
          x ∈ Pv ++ R₁ ∧ x ∉ s₁.domS := by
        intro x hx
        rcases List.mem_append.mp hx with hx | hx
        · exact g₁'.clears_ty (fun _ hy => List.mem_append_left _
            (hR (List.mem_append_left _ hy))) hx
        · exact g₁'.clears_ty (fun _ hy => List.mem_append_left _
            (hR (List.mem_append_right _ hy))) hx
      obtain ⟨f₂, h₂⟩ : ∃ f₂, unifyTyF S₁ f₂ (b₁.applySubst s₁.toSubst)
          (b₂.applySubst s₁.toSubst) ≠ .outOfFuel := by
        by_cases hd : s₁.domS = []
        · obtain ⟨ht, hrw, hlb⟩ := Sol.toSubst_of_domS_nil hd
          rw [Ty.applySubst_fixed_sorted b₁ (fun α _ => ht α) (fun α _ => hrw α) (fun α _ => hlb α),
              Ty.applySubst_fixed_sorted b₂ (fun α _ => ht α) (fun α _ => hrw α) (fun α _ => hlb α)]
          exact (ihm _ hsR).1 S₁ b₁ b₂
            (Below.residual hB hS₁ hf₁ (fun x hx => List.mem_append_left _ (hR hx)))
            (Nat.le_trans (nv_mono hR) hPn) (Nat.le_refl _)
        · have hdr := ((unifyM_draws f₁).1 S a₁ a₂ hBT hr).resolve_right (fun h => hd h.1)
          exact (ihn _ (Nat.lt_of_lt_of_le (stage_count hf₁ g₁' hRes hS₁ hdr) hPn) _).1 S₁ _ _
            (Below.residual hB hS₁ hf₁ (fun x hx => (hRes x hx).1)) (Nat.le_refl _)
            (Nat.le_refl _)
      refine ⟨max f₁ f₂, ?_⟩
      have e₁ : unifyTyF S (max f₁ f₂) a₁ a₂ = .success s₁ S₁ := by
        rw [mono_ty_eq (Nat.le_max_left _ _) h₁, hr]
      rw [e₁]
      refine UResM.seq_ne_oof (by simp) (fun s S he => ?_)
      simp only [UResM.success.injEq] at he
      obtain ⟨rfl, rfl⟩ := he
      rw [mono_ty_eq (Nat.le_max_right _ _) h₂]
      exact h₂
  | clash => exact ⟨f₁, by rw [hr]; simp [UResM.seq]⟩
  | occurs => exact ⟨f₁, by rw [hr]; simp [UResM.seq]⟩
  | stuck => exact ⟨f₁, by rw [hr]; simp [UResM.seq]⟩
  | outOfFuel => exact absurd hr h₁

------------------------- U-HOST SHRINKS THE PROBLEM ----------------------------

theorem spineSize_renameVar {B : Type} (β β' : TyVar) :
    (s : List (Atom B)) → spineSize (renameVar β β' s) = spineSize s
  | [] => rfl
  | .var γ :: s => by
      simp only [renameVar]
      split <;> simp only [spineSize, spineSize_renameVar β β' s]
  | .field _ _ :: s => by simp only [renameVar, spineSize, spineSize_renameVar β β' s]
  | .dfield _ _ :: s => by simp only [renameVar, spineSize, spineSize_renameVar β β' s]

-- ⊢  U-HOST'S RESIDUAL: below the advanced supply, no more variables (β′ for
--    β), and a field fewer
theorem host_step {B : Type} {S : Supply} {u₁ u₂ : List (Atom B)} {β : TyVar} {l : Label}
    {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (hB : Below S (SpP u₁ u₂)) (he : hostL S u₁ u₂ = some (β, l, τ, t₁, t₂)) :
    Below S.fresh.2 (SpP t₁ t₂) ∧ nv (SpP t₁ t₂) ≤ nv (SpP u₁ u₂) ∧
      spineSize t₁ + spineSize t₂ < spineSize u₁ + spineSize u₂ := by
  obtain ⟨hs1, -, -, -, hren⟩ := hostL_spec he
  obtain ⟨hβV, -, hres⟩ := host_residual hB he
  have hf₁ : FreshIn S S.fresh.2 [(.row, S.fresh.1)] := by
    intro z hz
    obtain rfl := List.mem_singleton.mp hz
    simp only [Supply.fresh, natName_length]
    exact ⟨Nat.le_refl _, Nat.lt_succ_self _, trivial⟩
  refine ⟨Below.residual hB (Nat.le_succ _) hf₁ (fun z hz => (hres z hz).1), ?_, ?_⟩
  · have h := nv_residual (Q := SpP u₁ u₂) (D := [(.row, S.fresh.1)]) (K := [(.row, β)])
      (Res := SpP t₁ t₂)
      (fun x hx => .inl (by rw [List.mem_singleton.mp hx]; exact hβV))
      (fun x hx => ⟨List.mem_append.mp (hres x hx).1,
        fun hk => (hres x hx).2 (List.mem_singleton.mp hk)⟩)
    have h1 : nv [((.row : Srt), β)] = 1 := by simp [nv, dd]
    simp only [List.length_singleton] at h
    omega
  · rw [hs1, hren, spineSize_renameVar]
    simp only [spineSize]
    omega

theorem hostResM_ne_oof {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} (h : r ≠ .outOfFuel) : hostResM S β l τ r ≠ .outOfFuel := by
  cases r <;> simp_all [hostResM]

-- the arms that do not recurse answer at fuel 0
theorem unifyTyF_flat_ne_oof {B : Type} [DecidableEq B] (S : Supply) (τ τ' : Ty B)
    (hrec : tyRec τ τ' = false) : unifyTyF S 0 τ τ' ≠ .outOfFuel := by
  cases τ with
  | var α => exact bindTy_ne_oof S α τ'
  | base b =>
      cases τ' with
      | var α => exact bindTy_ne_oof S α _
      | base b' => by_cases hb : b = b' <;> simp [unifyTyF, hb]
      | lab _ => simp [unifyTyF]
      | unk => simp [unifyTyF]
      | fn _ _ => simp [unifyTyF]
      | rcd _ => simp [unifyTyF]
  | lab b =>
      cases τ' with
      | var α => exact bindTy_ne_oof S α _
      | base _ => simp [unifyTyF]
      | lab b' =>
          simp only [unifyTyF]
          rcases unifyKey_cases (B := B) S b b' with ⟨_, _, he⟩ | he <;> rw [he] <;> simp
      | unk => simp [unifyTyF]
      | fn _ _ => simp [unifyTyF]
      | rcd _ => simp [unifyTyF]
  | unk =>
      cases τ' with
      | var α => exact bindTy_ne_oof S α _
      | _ => simp [unifyTyF]
  | fn a₁ b₁ =>
      cases τ' with
      | var α => exact bindTy_ne_oof S α _
      | fn _ _ => simp [tyRec] at hrec
      | _ => simp [unifyTyF]
  | rcd ρ₁ =>
      cases τ' with
      | var α => exact bindTy_ne_oof S α _
      | rcd _ => simp [tyRec] at hrec
      | _ => simp [unifyTyF]

theorem unifySpineMF_nil_ne_oof {B : Type} [DecidableEq B] (S : Supply) (f : Nat)
    (s₂ : List (Atom B)) : unifySpineMF S f [] s₂ ≠ .outOfFuel := by
  simp only [unifySpineMF]
  cases allVarsEmpty s₂ <;> simp

theorem unifySpineMF_cons_nil_ne_oof {B : Type} [DecidableEq B] (S : Supply) (f : Nat)
    (a : Atom B) (s₁ : List (Atom B)) : unifySpineMF S f (a :: s₁) [] ≠ .outOfFuel := by
  simp only [unifySpineMF]
  cases allVarsEmpty (a :: s₁) <;> simp

theorem termR_all {B : Type} [DecidableEq B] : ∀ n m, TermR B n m := by
  intro n
  induction n using Nat.strongRecOn with
  | ind n ihn =>
  intro m
  induction m using Nat.strongRecOn with
  | ind m ihm =>
  refine ⟨fun S τ τ' hB hn hm => ?_, fun S s₁ s₂ hB hn hm => ?_⟩
  · cases hrec : tyRec τ τ' with
    | false => exact ⟨0, unifyTyF_flat_ne_oof S τ τ' hrec⟩
    | true =>
      rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
      · have hsz : (Ty.fn a₁ b₁).usize + (Ty.fn a₂ b₂).usize =
            1 + a₁.usize + b₁.usize + (1 + a₂.usize + b₂.usize) := rfl
        obtain ⟨F, hF⟩ := arm_ty_terminates ihn ihm hB hn a₁ a₂ b₁ b₂
          (fun x hx => by
            simp only [TyP, Ty.sortedFtv, List.mem_append] at hx ⊢
            rcases hx with hx | hx
            · exact .inl (.inl hx)
            · exact .inr (.inl hx))
          (fun x hx => by
            simp only [TyP, Ty.sortedFtv, List.mem_append] at hx ⊢
            rcases hx with hx | hx
            · exact .inl (.inr hx)
            · exact .inr (.inr hx))
          (by omega) (by omega)
        exact ⟨F + 1, hF⟩
      · have hsz : (Ty.rcd ρ₁).usize + (Ty.rcd ρ₂).usize = 1 + ρ₁.usize + (1 + ρ₂.usize) := rfl
        have hP : SpP (B := B) ρ₁.toSpine ρ₂.toSpine ⊆ TyP (Ty.rcd ρ₁) (Ty.rcd ρ₂) := by
          intro x hx
          simp only [SpP, TyP, Ty.sortedFtv, List.mem_append] at hx ⊢
          rcases hx with hx | hx
          · exact .inl (sSorted_toSpine _ _ hx)
          · exact .inr (sSorted_toSpine _ _ hx)
        obtain ⟨f, hf⟩ := (ihm (ρ₁.usize + ρ₂.usize) (by omega)).2 S ρ₁.toSpine ρ₂.toSpine
          (fun x hx => hB x (hP hx)) (Nat.le_trans (nv_mono hP) hn)
          (by rw [spineSize_toSpine, spineSize_toSpine]; exact Nat.le_refl _)
        exact ⟨f + 1, hf⟩
  · cases s₁ with
    | nil => exact ⟨0, unifySpineMF_nil_ne_oof S 0 s₂⟩
    | cons a s₁ =>
      cases s₂ with
      | nil => exact ⟨0, unifySpineMF_cons_nil_ne_oof S 0 a s₁⟩
      | cons b s₂ =>
        -- a residual inside the problem, strictly smaller: the inner hypothesis
        have strip : ∀ t₁ t₂ : List (Atom B), (∀ x ∈ t₁, x ∈ a :: s₁) →
            (∀ x ∈ t₂, x ∈ b :: s₂) →
            spineSize t₁ + spineSize t₂ < m → ∃ f, unifySpineMF S f t₁ t₂ ≠ .outOfFuel :=
          fun t₁ t₂ h₁ h₂ hs =>
            (ihm _ hs).2 S t₁ t₂ (fun x hx => hB x (sSorted_sub_pair h₁ h₂ hx))
              (Nat.le_trans (nv_mono (sSorted_sub_pair h₁ h₂)) hn) (Nat.le_refl _)
        have arm : ∀ τ τ' t₁ t₂, EqEmit (a :: s₁) (b :: s₂) τ τ' t₁ t₂ →
            EqSize (a :: s₁) (b :: s₂) τ τ' t₁ t₂ →
            ∃ F, ((unifyTyF S F τ τ').seq fun θ S'' =>
              unifySpineMF S'' F (sApplySubst θ t₁) (sApplySubst θ t₂)) ≠ .outOfFuel :=
          fun τ τ' t₁ t₂ he hs =>
            arm_sp_terminates ihn ihm hB hn τ τ' t₁ t₂ he.ty_sub he.res_sub
              (by have := hs.1; have := hs.2; omega)
              (by have := hs.1; have := hs.2; omega)
        cases hsl : stripL (a :: s₁) (b :: s₂) with
        | some p =>
          obtain ⟨t₁, t₂⟩ := p
          obtain ⟨f, hf⟩ := strip t₁ t₂ (stripL_atoms hsl).1 (stripL_atoms hsl).2
            (by have := (stripL_size hsl).1; have := (stripL_size hsl).2; omega)
          exact ⟨f + 1, by unfold unifySpineMF; simp only [hsl]; exact hf⟩
        | none =>
        cases hsr : stripR (a :: s₁) (b :: s₂) with
        | some p =>
          obtain ⟨t₁, t₂⟩ := p
          obtain ⟨f, hf⟩ := strip t₁ t₂ (stripR_atoms hsr).1 (stripR_atoms hsr).2
            (by have := (stripR_size hsr).1; have := (stripR_size hsr).2; omega)
          exact ⟨f + 1, by unfold unifySpineMF; simp only [hsl, hsr]; exact hf⟩
        | none =>
        cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
        | some r =>
          exact ⟨0 + 1, by unfold unifySpineMF; simp only [hsl, hsr, hv1]
                           exact solveVarM_ne_oof hv1⟩
        | none =>
        cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
        | some r =>
          exact ⟨0 + 1, by unfold unifySpineMF; simp only [hsl, hsr, hv1, hv2]
                           exact solveVarM_ne_oof hv2⟩
        | none =>
        cases hml : matchL (a :: s₁) (b :: s₂) with
        | some p =>
          obtain ⟨τ0, τ0', t₁, t₂⟩ := p
          obtain ⟨F, hF⟩ := arm τ0 τ0' t₁ t₂ (matchL_atoms hml) (matchL_size hml)
          exact ⟨F + 1, by unfold unifySpineMF; simp only [hsl, hsr, hv1, hv2, hml]; exact hF⟩
        | none =>
        cases hml2 : matchL (b :: s₂) (a :: s₁) with
        | some p =>
          obtain ⟨τ0', τ0, t₂, t₁⟩ := p
          obtain ⟨F, hF⟩ := arm τ0 τ0' t₁ t₂ (matchL_atoms hml2).swap (matchL_size hml2).swap
          exact ⟨F + 1, by
            unfold unifySpineMF; simp only [hsl, hsr, hv1, hv2, hml, hml2]; exact hF⟩
        | none =>
        cases hmr : matchR (a :: s₁) (b :: s₂) with
        | some p =>
          obtain ⟨τ0, τ0', t₁, t₂⟩ := p
          obtain ⟨F, hF⟩ := arm τ0 τ0' t₁ t₂ (matchR_atoms hmr) (matchR_size hmr)
          exact ⟨F + 1, by
            unfold unifySpineMF; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr]; exact hF⟩
        | none =>
        cases hmr2 : matchR (b :: s₂) (a :: s₁) with
        | some p =>
          obtain ⟨τ0', τ0, t₂, t₁⟩ := p
          obtain ⟨F, hF⟩ := arm τ0 τ0' t₁ t₂ (matchR_atoms hmr2).swap (matchR_size hmr2).swap
          exact ⟨F + 1, by
            unfold unifySpineMF; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2]
            exact hF⟩
        | none =>
        cases hg : groundMatch (a :: s₁) (b :: s₂) with
        | some p =>
          obtain ⟨τ0, τ0', t₁, t₂⟩ := p
          obtain ⟨F, hF⟩ := arm τ0 τ0' t₁ t₂ (groundMatch_atoms hg) (groundMatch_size hg)
          exact ⟨F + 1, by
            unfold unifySpineMF; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg]
            exact hF⟩
        | none =>
        cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
        | some p =>
          obtain ⟨τ0', τ0, t₂, t₁⟩ := p
          obtain ⟨F, hF⟩ := arm τ0 τ0' t₁ t₂ (groundMatch_atoms hg2).swap
            (groundMatch_size hg2).swap
          exact ⟨F + 1, by
            unfold unifySpineMF
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2]
            exact hF⟩
        | none =>
        cases hpc : projClash (a :: s₁) (b :: s₂) with
        | true =>
          exact ⟨0 + 1, by
            unfold unifySpineMF
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc]
            simp⟩
        | false =>
        cases hh1 : hostL S (a :: s₁) (b :: s₂) with
        | some p =>
          obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
          obtain ⟨hB', hn', hs'⟩ := host_step hB hh1
          obtain ⟨f, hf⟩ := (ihm _ (by omega)).2 S.fresh.2 t₁ t₂ hB'
            (Nat.le_trans hn' hn) (Nat.le_refl _)
          exact ⟨f + 1, by
            unfold unifySpineMF
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1,
              Bool.false_eq_true, ite_false]
            exact hostResM_ne_oof hf⟩
        | none =>
        cases hh2 : hostL S (b :: s₂) (a :: s₁) with
        | some p =>
          obtain ⟨β0, l0, τ0, t₂, t₁⟩ := p
          have hsw : ∀ {u v : List (Atom B)} {x}, x ∈ SpP u v → x ∈ SpP v u :=
            fun hx => append_sub_swap (fun _ h => h) hx
          obtain ⟨hB', hn', hs'⟩ := host_step (fun x hx => hB x (hsw hx)) hh2
          obtain ⟨f, hf⟩ := (ihm _ (by omega)).2 S.fresh.2 t₁ t₂
            (fun x hx => hB' x (hsw hx))
            (Nat.le_trans (nv_mono fun _ hx => hsw hx)
              (Nat.le_trans hn' (Nat.le_trans (nv_mono fun _ hx => hsw hx) hn)))
            (Nat.le_refl _)
          exact ⟨f + 1, by
            unfold unifySpineMF
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1, hh2,
              Bool.false_eq_true, ite_false]
            exact hostResM_ne_oof hf⟩
        | none =>
          exact ⟨0 + 1, by
            unfold unifySpineMF
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1, hh2,
              Bool.false_eq_true, ite_false]
            simp⟩

------------------------- TERMINATION ------------------------------------------

-- ⊢  EVERY PROBLEM HAS A FUEL AT WHICH THE DRIVER ANSWERS, at both sorts,
--    from any supply above the problem. The proviso is U-host's: a draw that
--    collides with a problem variable is not a new name, and the count of a
--    residual is no longer bounded by its keys.
theorem unifyTyF_terminates {B : Type} [DecidableEq B] {S : Supply} {τ τ' : Ty B}
    (hB : Below S (TyP τ τ')) : ∃ fuel, unifyTyF S fuel τ τ' ≠ .outOfFuel :=
  (termR_all _ _).1 S τ τ' hB (Nat.le_refl _) (Nat.le_refl _)

theorem unifySpineMF_terminates {B : Type} [DecidableEq B] {S : Supply}
    {s₁ s₂ : List (Atom B)} (hB : Below S (SpP s₁ s₂)) :
    ∃ fuel, unifySpineMF S fuel s₁ s₂ ≠ .outOfFuel :=
  (termR_all _ _).2 S s₁ s₂ hB (Nat.le_refl _) (Nat.le_refl _)

-- ⊢  the type entry point's supply is above its problem
theorem below_tySupply {B : Type} (τ τ' : Ty B) :
    Below ⟨lenBound (τ.ftv ++ τ'.ftv) + 1⟩ (TyP τ τ') := fun x hx => by
  show x.2.length < lenBound (τ.ftv ++ τ'.ftv) + 1
  refine Nat.lt_succ_of_le (length_le_lenBound ?_)
  rcases List.mem_append.mp hx with hx | hx
  · exact List.mem_append_left _ (Ty.mem_ftv_of_mem_sortedFtv _ hx)
  · exact List.mem_append_right _ (Ty.mem_ftv_of_mem_sortedFtv _ hx)

theorem unifyRowM_terminates {B : Type} [DecidableEq B] (ρ₁ ρ₂ : Row B) :
    ∃ fuel, unifyRowM fuel ρ₁ ρ₂ ≠ .outOfFuel :=
  unifySpineMF_terminates (below_localSupply _ _)

theorem unifyTyM_terminates {B : Type} [DecidableEq B] (τ τ' : Ty B) :
    ∃ fuel, unifyTyM fuel τ τ' ≠ .outOfFuel :=
  unifyTyF_terminates (below_tySupply τ τ')

-- ⊢  …and from there on the verdict is FIXED: every larger fuel gives the
--    same answer (`unifyM_fuel_mono`). So `≐ᵣ` is a function.
theorem unifyRowM_verdict_stable {B : Type} [DecidableEq B] {ρ₁ ρ₂ : Row B} {f F : Nat}
    (hle : f ≤ F) (h : unifyRowM f ρ₁ ρ₂ ≠ .outOfFuel) :
    unifyRowM F ρ₁ ρ₂ = unifyRowM f ρ₁ ρ₂ :=
  mono_sp_eq hle h


/-- `≐ᵣ` AS A TOTAL FUNCTION: the verdict at any fuel that suffices. There is no
closed-form bound (a solved stage can grow the spine), so the fuel is chosen,
not computed; the executable regressions keep using explicit fuel, and
`unifyRow_eq` says they compute the same thing. -/
noncomputable def unifyRow {B : Type} [DecidableEq B] (ρ₁ ρ₂ : Row B) : UResM B :=
  unifyRowM (Classical.choose (unifyRowM_terminates ρ₁ ρ₂)) ρ₁ ρ₂

theorem unifyRow_ne_oof {B : Type} [DecidableEq B] (ρ₁ ρ₂ : Row B) :
    unifyRow ρ₁ ρ₂ ≠ .outOfFuel :=
  Classical.choose_spec (unifyRowM_terminates ρ₁ ρ₂)

-- ⊢  every run that answers, answers `unifyRow`
theorem unifyRow_eq {B : Type} [DecidableEq B] {ρ₁ ρ₂ : Row B} {f : Nat}
    (h : unifyRowM f ρ₁ ρ₂ ≠ .outOfFuel) : unifyRowM f ρ₁ ρ₂ = unifyRow ρ₁ ρ₂ := by
  unfold unifyRow
  have hc := Classical.choose_spec (unifyRowM_terminates ρ₁ ρ₂)
  rcases Nat.le_total f (Classical.choose (unifyRowM_terminates ρ₁ ρ₂)) with hle | hle
  · exact (unifyRowM_verdict_stable hle h).symm
  · exact unifyRowM_verdict_stable hle hc

end MinimalCalculus
