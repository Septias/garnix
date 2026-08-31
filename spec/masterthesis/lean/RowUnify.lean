-- Row unification ≐ᵣ: the executable algorithm (unifyRow/unifySpine), the
-- field-count invariant, and the trichotomy legs — success soundness &
-- completeness (mgu), clash soundness, fuel sufficiency, terminal-stuck
-- structure, and stuck ⟹ no-mgu (as a reduction). Builds on RowEquiv.

import minimal
import RowEquiv

namespace MinimalCalculus

------------------------ THE ROW UNIFICATION ALGORITHM ------------------------
-- ≐ᵣ Works on spines; every
-- step is FORCED (solution-set preserving):
--   * strip a shared var off either end            (U-var-refl; cancel_cat_*)
--   * solve a whole-var remainder, occurs-checked  (U-var-solve)
--   * match a leading/trailing field against the other side's window
--     (= leading/trailing segment)                 (U-field, both ends)
--   * one side exhausted: remaining vars ≔ ε,
--     remaining fields clash                       (U-ε-var)
--   * ground-side counting: if one side is var-free and a label has EQUAL
--     positive field-counts on both sides, the other side's vars are l-free
--     by counting and the pairing is positional — match first occurrences.
--     (U-ground: this is worked example 2's "var count must collapse" made
--     into an explicit rule; the window rules alone do not cover it, which
--     the mechanization surfaced.)
--   * global projection clash                      (U-clash)
--   * otherwise stuck                              (U-stuck, Wand ambiguity)
--
-- No LUtail: field demands never flow through ≐ᵣ (they park as stumps), so
-- the algorithm never guesses a field into a var. Type equations are EMITTED
-- (τ ≐ τ' pairs), not solved — *the type-level driver is future work*.
--
-- Presentation uses fuel (structural recursion ⟹ the algorithm computes by
-- rfl; the regressions below are kernel-checked executions). Every recursive
-- call consumes ≥ 2 atoms, so fuel |s₁| + |s₂| never runs out.

inductive URes (B : Type) : Type where
  | success : List (TyVar × Row B) → List (Ty B × Ty B) → URes B
  | clash   : URes B   -- no unifier (projection clash)
  | occurs  : URes B   -- no finite unifier (recursive row)
  | stuck   : URes B   -- solutions may exist, no unique mgu (Wand ambiguity)

def URes.addEq {B : Type} (τ τ' : Ty B) : URes B → URes B
  | .success σ eqs => .success σ ((τ, τ') :: eqs)
  | r => r

-- ## Spine measurements
def sHasVar {B : Type} : List (Atom B) → Bool
  | [] => false
  | .var _ :: _ => true
  | .field _ _ :: s => sHasVar s

def sFieldCount {B : Type} (l : Label) : List (Atom B) → Nat
  | [] => 0
  | .field l' _ :: s => (if l' = l then 1 else 0) + sFieldCount l s
  | .var _ :: s => sFieldCount l s

def sLabels {B : Type} : List (Atom B) → List Label
  | [] => []
  | .field l _ :: s => l :: sLabels s
  | .var _ :: s => sLabels s

-- U-clash, projection-based (checked globally, not per-window — cf. the
-- (β | l: Int | α) ≐ᵣ (l′: Bool) example for why).
def projClash {B : Type} (s₁ s₂ : List (Atom B)) : Bool :=
  (sLabels s₁ ++ sLabels s₂).any fun l =>
    (decide (sFieldCount l s₂ < sFieldCount l s₁) && !sHasVar s₂) ||
    (decide (sFieldCount l s₁ < sFieldCount l s₂) && !sHasVar s₁)

-- ## Move detectors
-- U-ε-var: every var of an exhausted-side remainder is forced to ε; a
-- leftover field has nowhere to come from.
def allVarsEmpty {B : Type} : List (Atom B) → Option (List (TyVar × Row B))
  | [] => some []
  | .var α :: s => (allVarsEmpty s).map ((α, Row.empty) :: ·)
  | .field _ _ :: _ => none

-- U-var-refl at the left end.
def stripL {B : Type} : List (Atom B) → List (Atom B) →
    Option (List (Atom B) × List (Atom B))
  | .var α :: t₁, .var β :: t₂ => if α = β then some (t₁, t₂) else none
  | _, _ => none

-- … and at the right end (trace monoids cancel on both sides).
def stripR {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (List (Atom B) × List (Atom B)) :=
  match stripL s₁.reverse s₂.reverse with
  | some (t₁, t₂) => some (t₁.reverse, t₂.reverse)
  | none => none

-- U-var-solve: a whole-var remainder, occurs-checked.
def solveVar {B : Type} : List (Atom B) → List (Atom B) → Option (URes B)
  | [.var α], s₂ =>
      some (if (sVarSeq s₂).contains α then .occurs
            else .success [(α, ofSpine s₂)] [])
  | _, _ => none

-- First l-field of the WINDOW (leading segment): the search stops at a var.
def windowExtract {B : Type} (l : Label) :
    List (Atom B) → Option (Ty B × List (Atom B))
  | [] => none
  | .var _ :: _ => none
  | .field l' τ :: s =>
      if l' = l then some (τ, s)
      else match windowExtract l s with
        | some (τ', s') => some (τ', .field l' τ :: s')
        | none => none

-- U-field at the left end: leading field of one side matched against the
-- first same-label occurrence in the other side's window.
def matchL {B : Type} : List (Atom B) → List (Atom B) →
    Option (Ty B × Ty B × List (Atom B) × List (Atom B))
  | .field l τ :: t₁, s₂ =>
      match windowExtract l s₂ with
      | some (τ', s₂') => some (τ, τ', t₁, s₂')
      | none => none
  | _, _ => none

-- … and at the right end.
def matchR {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (Ty B × Ty B × List (Atom B) × List (Atom B)) :=
  match matchL s₁.reverse s₂.reverse with
  | some (τ, τ', t₁, t₂) => some (τ, τ', t₁.reverse, t₂.reverse)
  | none => none

-- First l-field ANYWHERE in the spine (vars skipped), removed — the U-ground
-- pairing is positional among concrete fields once counting rules the vars out.
def removeField {B : Type} (l : Label) :
    List (Atom B) → Option (Ty B × List (Atom B))
  | [] => none
  | .var β :: s =>
      match removeField l s with
      | some (τ, s') => some (τ, .var β :: s')
      | none => none
  | .field l' τ :: s =>
      if l' = l then some (τ, s)
      else match removeField l s with
        | some (τ', s') => some (τ', .field l' τ :: s')
        | none => none

def groundMatchAux {B : Type} (s₁ s₂ : List (Atom B)) :
    List Label → Option (Ty B × Ty B × List (Atom B) × List (Atom B))
  | [] => none
  | l :: ls =>
      if sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ then
        match removeField l s₁, removeField l s₂ with
        | some (τ, t₁), some (τ', t₂) => some (τ, τ', t₁, t₂)
        | _, _ => groundMatchAux s₁ s₂ ls
      else groundMatchAux s₁ s₂ ls

-- U-ground: s₂ var-free, some label with equal positive counts.
def groundMatch {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (Ty B × Ty B × List (Atom B) × List (Atom B)) :=
  if sHasVar s₂ then none else groundMatchAux s₁ s₂ (sLabels s₁)

-- ## The algorithm
def unifySpineF {B : Type} : Nat → List (Atom B) → List (Atom B) → URes B
  | _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => .success σ []
      | none   => .clash
  | _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => .success σ []
      | none   => .clash
  | 0, _, _ => .stuck   -- unreachable at fuel ≥ |s₁| + |s₂| (each move eats ≥ 2 atoms)
  | fuel+1, s₁, s₂ =>
      match stripL s₁ s₂ with
      | some (t₁, t₂) => unifySpineF fuel t₁ t₂
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => unifySpineF fuel t₁ t₂
      | none =>
      match solveVar s₁ s₂ with
      | some r => r
      | none =>
      match solveVar s₂ s₁ with
      | some r => r
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      if projClash s₁ s₂ then .clash else .stuck

def unifySpine {B : Type} (s₁ s₂ : List (Atom B)) : URes B :=
  unifySpineF (s₁.length + s₂.length) s₁ s₂

def unifyRow {B : Type} (ρ₁ ρ₂ : Row B) : URes B :=
  unifySpine ρ₁.toSpine ρ₂.toSpine


--------------------- ≐ᵣ METATHEORY: FIELD-COUNT INVARIANT -------------------
-- The l-field count is a ≈-invariant: ≈ preserves projection-list lengths.
-- Substitution can only ADD l-fields (vars expand to ≥ 0 new fields); for
-- var-free rows the count is fixed. These three facts together prove the
-- U-clash direction of the trichotomy (projClash_no_unifier).

-- Spine roundtrip: toSpine . ofSpine = id.
-- ⊢  spine(ofSpine s) = s
private theorem ofSpine_toSpine {B : Type} : (s : List (Atom B)) → (ofSpine s).toSpine = s
  | [] => rfl
  | .field l τ :: s => by
      simp only [ofSpine, Row.toSpine, List.singleton_append]
      exact congrArg (.field l τ :: ·) (ofSpine_toSpine s)
  | .var α :: s => by
      simp only [ofSpine, Row.toSpine, List.singleton_append]
      exact congrArg (.var α :: ·) (ofSpine_toSpine s)

-- l-field count distributes over spine append.
-- ⊢  count_l(s₁ ++ s₂) = count_l(s₁) + count_l(s₂)
private theorem sFieldCount_append {B : Type} (l : Label) : (s₁ s₂ : List (Atom B)) →
    sFieldCount l (s₁ ++ s₂) = sFieldCount l s₁ + sFieldCount l s₂
  | [], _ => by simp [sFieldCount]
  | .field l' _ :: s₁, s₂ => by
      simp only [List.cons_append, sFieldCount]
      rw [sFieldCount_append l s₁ s₂]; omega
  | .var _ :: s₁, s₂ => by
      simp only [List.cons_append, sFieldCount]
      rw [sFieldCount_append l s₁ s₂]

-- (sProj l s).length = sFieldCount l s (projection list length = concrete count).
-- ⊢  |proj_l(s)| = count_l(s)
private theorem sProj_length_eq_sFieldCount {B : Type} (l : Label) : (s : List (Atom B)) →
    (sProj l s).length = sFieldCount l s
  | [] => rfl
  | .field l' _ :: s => by
      simp only [sProj, sFieldCount]
      by_cases h : l' = l <;> simp [h, sProj_length_eq_sFieldCount l s] <;> omega
  | .var _ :: s => by
      simp only [sProj, sFieldCount, List.length_map, sProj_length_eq_sFieldCount l s]

-- ProjEquiv preserves list length.
-- ⊢  ps ≈ₚ qs   ⟹   |ps| = |qs|
private theorem ProjEquiv.length_eq {B : Type} : {ps qs : List (Nat × Ty B)} →
    ProjEquiv ps qs → ps.length = qs.length
  | _, _, .nil => rfl
  | _, _, .cons _ _ h => congrArg Nat.succ h.length_eq

-- ≈ preserves the l-field count (rowEquiv_iff_char + projection-length).
-- ⊢  ρ₁ ≈ᵣ ρ₂   ⟹   count_l(spine ρ₁) = count_l(spine ρ₂)
theorem rowEquiv_fieldCount_eq {B : Type} {ρ₁ ρ₂ : Row B} (l : Label) (h : ρ₁ ≈ᵣ ρ₂) :
    sFieldCount l ρ₁.toSpine = sFieldCount l ρ₂.toSpine := by
  rw [← sProj_length_eq_sFieldCount, ← sProj_length_eq_sFieldCount]
  exact (h.char.2 l).length_eq

-- sHasVar s = false ↔ sVarSeq s = [] (bridge from Bool to Prop).
-- ⊢  hasVar(s) = false   ↔   vars(s) = []
private theorem sHasVar_false_iff {B : Type} : (s : List (Atom B)) →
    (sHasVar s = false ↔ sVarSeq s = [])
  | [] => ⟨fun _ => rfl, fun _ => rfl⟩
  | .var _ :: _ => by simp [sHasVar, sVarSeq]
  | .field _ _ :: s => by simp [sHasVar, sVarSeq, sHasVar_false_iff s]

-- !sHasVar s → SpineVarFree (ofSpine s) — used in projClash_no_unifier.
-- ⊢  hasVar(s) = false   ⟹   (ofSpine s).SpineVarFree
private theorem spineVarFree_ofSpine {B : Type} {s : List (Atom B)}
    (h : sHasVar s = false) : (ofSpine s).SpineVarFree :=
  (spineVarFree_iff_varSeq_nil (ofSpine s)).mpr
    (by rw [ofSpine_toSpine]; exact (sHasVar_false_iff s).mp h)

-- Substitution can only add l-fields: the count weakly increases.
-- ⊢  count_l(spine ρ) ≤ count_l(spine (θρ))
theorem sFieldCount_applySubst_le {B : Type} (θ : TySubst B) (l : Label) :
    (ρ : Row B) → sFieldCount l ρ.toSpine ≤ sFieldCount l (ρ.applySubst θ).toSpine
  | .empty => Nat.le_refl _
  | .var _ => Nat.zero_le _
  | .sing l' _ => by simp [Row.applySubst, Row.toSpine, sFieldCount]
  | .cat ρ₁ ρ₂ => by
      simp only [Row.applySubst, Row.toSpine, sFieldCount_append]
      exact Nat.add_le_add
        (sFieldCount_applySubst_le θ l ρ₁) (sFieldCount_applySubst_le θ l ρ₂)

-- For var-free rows substitution fixes the l-field count exactly.
-- ⊢  ρ var-free   ⟹   count_l(spine (θρ)) = count_l(spine ρ)
theorem sFieldCount_applySubst_varFree {B : Type} (θ : TySubst B) (l : Label) :
    {ρ : Row B} → ρ.SpineVarFree →
    sFieldCount l (ρ.applySubst θ).toSpine = sFieldCount l ρ.toSpine
  | .empty, _ => rfl
  | .var _, h => nomatch h
  | .sing l' _, _ => by simp [Row.applySubst, Row.toSpine, sFieldCount]
  | .cat ρ₁ ρ₂, .cat hv₁ hv₂ => by
      simp only [Row.applySubst, Row.toSpine, sFieldCount_append]
      rw [sFieldCount_applySubst_varFree θ l hv₁, sFieldCount_applySubst_varFree θ l hv₂]

-- ## The stuck ⟹ no-unique-mgu direction, via field-count monotonicity
-- Substitution never DELETES an l-field (sFieldCount_applySubst_le) and ≈
-- preserves counts (rowEquiv_fieldCount_eq). So if θ' factors through θ
-- (θ' ⊑ θ), then θ carries no more l-fields at any single variable than θ'
-- does: an mgu is pointwise-MINIMAL in every label count. A unifier that
-- strictly undercuts a candidate somewhere therefore refutes its maximality —
-- this single fact is the engine behind Wand's ambiguity (U-stuck).

-- ⊢  θ' ⊑ θ   ⟹   count_l(θ x) ≤ count_l(θ' x)
theorem instanceOf_fieldCount_mono {B : Type} {θ' θ : TySubst B}
    (h : InstanceOf θ' θ) (x : TyVar) (l : Label) :
    sFieldCount l (θ.row x).toSpine ≤ sFieldCount l (θ'.row x).toSpine := by
  obtain ⟨σ, hrow, -⟩ := h
  rw [rowEquiv_fieldCount_eq l (hrow x)]
  exact sFieldCount_applySubst_le σ l (θ.row x)

-- The reusable no-mgu principle: if every unifier is strictly undercut at some
-- variable/label by another unifier, then no unifier is most general.
-- ⊢  (∀ unifier θ. ∃ unifier u, x, l.  count_l(u x) < count_l(θ x))
--        ⟹   ¬ ∃ mgu
theorem no_mgu_of_witness_shrinks {B : Type} {ρ₁ ρ₂ : Row B}
    (H : ∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ →
         ∃ (u : TySubst B) (x : TyVar) (l : Label),
           Unifies u ρ₁ ρ₂ ∧
           sFieldCount l (u.row x).toSpine < sFieldCount l (θ.row x).toSpine) :
    ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ ∧
        ∀ θ' : TySubst B, Unifies θ' ρ₁ ρ₂ → InstanceOf θ' θ := by
  rintro ⟨θ, hu, hmgu⟩
  obtain ⟨u, x, l, huni, hlt⟩ := H θ hu
  exact absurd (instanceOf_fieldCount_mono (hmgu u huni) x l) (by omega)

-- Wand's example, re-proved through the counting principle: in (β | α) ≐ᵣ (l:𝓫)
-- the single l-field must be hosted by θβ or θα (their counts sum to 1); the
-- witness that hosts it in the OTHER variable undercuts the chosen one's count
-- from 1 to 0, so no mgu exists. Cleaner and more uniform than the direct
-- projection argument (and the template for every field-vs-vars stuck case).
-- ⊢  ¬ ∃ mgu for (β | α) ≐ᵣ (l:𝓫)
theorem wand_no_mgu_count {B : Type} (b : B) (l : Label) :
    ¬ ∃ θ : TySubst B,
        Unifies θ (.cat (.var "β") (.var "α")) (.sing l (.base b)) ∧
        ∀ θ' : TySubst B,
          Unifies θ' (.cat (.var "β") (.var "α")) (.sing l (.base b)) →
          InstanceOf θ' θ := by
  apply no_mgu_of_witness_shrinks
  intro θ hu
  have hu' : RowEquiv (Row.cat (θ.row "β") (θ.row "α")) (Row.sing l (.base b)) := by
    have h := hu; unfold Unifies at h
    simpa only [Row.applySubst, Ty.applySubst] using h
  have hcount : sFieldCount l (θ.row "β").toSpine
              + sFieldCount l (θ.row "α").toSpine = 1 := by
    have h := rowEquiv_fieldCount_eq l hu'
    rw [show (Row.cat (θ.row "β") (θ.row "α")).toSpine
          = (θ.row "β").toSpine ++ (θ.row "α").toSpine from rfl,
        sFieldCount_append] at h
    simpa [Row.toSpine, sFieldCount] using h
  by_cases hα : sFieldCount l (θ.row "α").toSpine = 0
  · -- the field is in β (count β = 1); empty-β witness undercuts at "β"
    refine ⟨⟨fun x => .var x, fun x => if x = "β" then .empty
              else if x = "α" then .sing l (.base b) else .var x⟩, "β", l, ?_, ?_⟩
    · unfold Unifies; simp [Row.applySubst, Ty.applySubst]; exact RowEquiv.unitL
    · show 0 < sFieldCount l (θ.row "β").toSpine; omega
  · -- the field is in α (count α = 1); empty-α witness undercuts at "α"
    refine ⟨⟨fun x => .var x, fun x => if x = "β" then .sing l (.base b)
              else if x = "α" then .empty else .var x⟩, "α", l, ?_, ?_⟩
    · unfold Unifies; simp [Row.applySubst, Ty.applySubst]; exact RowEquiv.unitR
    · show 0 < sFieldCount l (θ.row "α").toSpine; omega

-- The dual of the count bound: a VAR-FREE component of θ is RIGID — every
-- instance has the exact same l-count there, because with no variables to
-- expand, substitution can neither add nor delete fields
-- (sFieldCount_applySubst_varFree turns the ≤ into an =).
-- ⊢  θ' ⊑ θ,  (θ x) var-free   ⟹   count_l(θ' x) = count_l(θ x)
theorem instanceOf_fieldCount_eq_of_varFree {B : Type} {θ' θ : TySubst B}
    (h : InstanceOf θ' θ) {x : TyVar} (hvf : (θ.row x).SpineVarFree) (l : Label) :
    sFieldCount l (θ'.row x).toSpine = sFieldCount l (θ.row x).toSpine := by
  obtain ⟨σ, hrow, -⟩ := h
  rw [rowEquiv_fieldCount_eq l (hrow x), sFieldCount_applySubst_varFree σ l hvf]

-- The OTHER canonical stuck shape: (α | l:𝓫) ≐ᵣ (l:𝓫 | β), α ≠ β. Counting alone
-- CANNOT fire here — the ε,ε unifier has count 0 at every variable, so nothing
-- can be undercut. The kill is RIGIDITY. The empty witness (α,β ↦ ε) pins
-- count_l(θα) = count_l(θβ) = 0; the unifier equation then forces θα var-free
-- (its lone l-projection would sit at segment |vars θα|, which must match
-- segment 0 on the right — so |vars θα| = 0); and a var-free θα is rigid, so the
-- doubling witness (α,β ↦ l:𝓫), which needs count 1 at α, cannot factor through
-- any such θ. Complements wand_no_mgu_count: together the two canonical stuck
-- shapes (field-vs-vars and two-sided) are both proven ambiguous.
-- ⊢  ¬ ∃ mgu for (α | l:𝓫) ≐ᵣ (l:𝓫 | β)
theorem two_sided_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ ∃ θ : TySubst B,
        Unifies θ (.cat (.var "α") (.sing l (.base b)))
                  (.cat (.sing l (.base b)) (.var "β")) ∧
        ∀ θ' : TySubst B,
          Unifies θ' (.cat (.var "α") (.sing l (.base b)))
                     (.cat (.sing l (.base b)) (.var "β")) →
          InstanceOf θ' θ := by
  rintro ⟨θ, hu, hmgu⟩
  have hu' : RowEquiv (Row.cat (θ.row "α") (Row.sing l (.base b)))
                      (Row.cat (Row.sing l (.base b)) (θ.row "β")) := by
    have h := hu; unfold Unifies at h
    simpa only [Row.applySubst, Ty.applySubst] using h
  -- witness u₁ = (α,β ↦ ε): a unifier
  have hu1 : Unifies
      (⟨fun x => .var x, fun x => if x = "α" then .empty
        else if x = "β" then .empty else .var x⟩ : TySubst B)
      (.cat (.var "α") (.sing l (.base b))) (.cat (.sing l (.base b)) (.var "β")) := by
    unfold Unifies
    show RowEquiv (Row.cat Row.empty (Row.sing l (.base b)))
                  (Row.cat (Row.sing l (.base b)) Row.empty)
    exact RowEquiv.unitL.trans RowEquiv.unitR.symm
  have hI1 := hmgu _ hu1
  have hcα : sFieldCount l (θ.row "α").toSpine = 0 := by
    have h : sFieldCount l (θ.row "α").toSpine ≤ 0 := instanceOf_fieldCount_mono hI1 "α" l
    omega
  have hcβ : sFieldCount l (θ.row "β").toSpine = 0 := by
    have h : sFieldCount l (θ.row "β").toSpine ≤ 0 := instanceOf_fieldCount_mono hI1 "β" l
    omega
  -- the l-projections are empty (length = count = 0)
  have hπα : sProj l (θ.row "α").toSpine = [] := by
    rcases hh : sProj l (θ.row "α").toSpine with _ | ⟨p, ps⟩
    · rfl
    · exfalso
      have hlen : (sProj l (θ.row "α").toSpine).length = 0 := by
        rw [sProj_length_eq_sFieldCount]; exact hcα
      rw [hh] at hlen; simp at hlen
  have hπβ : sProj l (θ.row "β").toSpine = [] := by
    rcases hh : sProj l (θ.row "β").toSpine with _ | ⟨p, ps⟩
    · rfl
    · exfalso
      have hlen : (sProj l (θ.row "β").toSpine).length = 0 := by
        rw [sProj_length_eq_sFieldCount]; exact hcβ
      rw [hh] at hlen; simp at hlen
  -- θα is var-free: its single l-field would sit at segment |vars θα|, forced 0
  obtain ⟨-, hp⟩ := hu'.char
  have hpl := hp l
  have hsing : sProj l [Atom.field l (.base b)] = [(0, (.base b : Ty B))] := by
    simp [sProj]
  rw [show (Row.cat (θ.row "α") (Row.sing l (.base b))).toSpine
        = (θ.row "α").toSpine ++ [Atom.field l (.base b)] from rfl,
      show (Row.cat (Row.sing l (.base b)) (θ.row "β")).toSpine
        = [Atom.field l (.base b)] ++ (θ.row "β").toSpine from rfl,
      sProj_append, sProj_append, hπα, hπβ, hsing] at hpl
  simp only [List.nil_append, List.append_nil, List.map_cons, List.map_nil] at hpl
  have hvnil : sVarSeq (θ.row "α").toSpine = [] := by
    have hv0 : (sVarSeq (θ.row "α").toSpine).length = 0 := by
      obtain ⟨τ', rest, heq, -, -⟩ := hpl.cons_inv
      rw [List.cons.injEq, Prod.mk.injEq] at heq
      omega
    rcases hh : sVarSeq (θ.row "α").toSpine with _ | ⟨x, xs⟩
    · rfl
    · rw [hh] at hv0; simp at hv0
  have hvfα : (θ.row "α").SpineVarFree :=
    (spineVarFree_iff_varSeq_nil (θ.row "α")).mpr hvnil
  -- witness u₂ = (α,β ↦ l:𝓫): needs count 1 at α, which rigidity forbids
  have hu2 : Unifies
      (⟨fun x => .var x, fun x => if x = "α" then .sing l (.base b)
        else if x = "β" then .sing l (.base b) else .var x⟩ : TySubst B)
      (.cat (.var "α") (.sing l (.base b))) (.cat (.sing l (.base b)) (.var "β")) := by
    unfold Unifies
    show RowEquiv (Row.cat (Row.sing l (.base b)) (Row.sing l (.base b)))
                  (Row.cat (Row.sing l (.base b)) (Row.sing l (.base b)))
    exact RowEquiv.refl _
  have hI2 := hmgu _ hu2
  have hrig := instanceOf_fieldCount_eq_of_varFree hI2 hvfα l
  rw [hcα] at hrig
  simp [Row.toSpine, sFieldCount] at hrig

-- ## No-mgu depends only on the unifier SET
-- `HasMgu` packages "a most general unifier exists". Crucially InstanceOf never
-- mentions the rows — only θ's action on variables — so two unification problems
-- with the SAME unifiers (as substitutions) have the SAME mgu-status. This is the
-- vehicle for lifting stuck⟹no-mgu through the unifier-set-PRESERVING moves.
def HasMgu {B : Type} (ρ₁ ρ₂ : Row B) : Prop :=
  ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ ∧
    ∀ θ' : TySubst B, Unifies θ' ρ₁ ρ₂ → InstanceOf θ' θ

-- ⊢  (∀θ. θ ⊨ ρ₁≐ᵣρ₂  ↔  θ ⊨ ρ₁'≐ᵣρ₂')   ⟹   (HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂')
theorem hasMgu_congr {B : Type} {ρ₁ ρ₂ ρ₁' ρ₂' : Row B}
    (h : ∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ ↔ Unifies θ ρ₁' ρ₂') :
    HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂' :=
  ⟨fun ⟨θ, hu, hmax⟩ => ⟨θ, (h θ).mp hu, fun θ' hu' => hmax θ' ((h θ').mpr hu')⟩,
   fun ⟨θ, hu, hmax⟩ => ⟨θ, (h θ).mpr hu, fun θ' hu' => hmax θ' ((h θ').mp hu')⟩⟩

-- No-mgu is a ≈-INVARIANT: replacing either side by a ≈-equal row preserves the
-- unifier set (applySubst is a ≈-congruence), hence mgu-status. Lets the base
-- no-mgu theorems (stated on plain rows) transfer to the ofSpine forms the
-- algorithm produces.
-- ⊢  ρ₁ ≈ᵣ ρ₁',  ρ₂ ≈ᵣ ρ₂'   ⟹   (HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂')
theorem hasMgu_rowEquiv {B : Type} {ρ₁ ρ₂ ρ₁' ρ₂' : Row B}
    (h₁ : RowEquiv ρ₁ ρ₁') (h₂ : RowEquiv ρ₂ ρ₂') :
    HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂' :=
  hasMgu_congr (fun θ =>
    ⟨fun hu => ((RowEquiv.applySubst θ h₁).symm.trans hu).trans (RowEquiv.applySubst θ h₂),
     fun hu => ((RowEquiv.applySubst θ h₁).trans hu).trans (RowEquiv.applySubst θ h₂).symm⟩)

-- No-mgu is SYMMETRIC in the two rows: `Unifies` is `RowEquiv` of the two
-- substituted rows, which is symmetric, so the unifier sets of ρ₁≐ᵣρ₂ and
-- ρ₂≐ᵣρ₁ coincide. Lets a base no-mgu theorem stated with the field on the LEFT
-- discharge the mirror config with the field on the RIGHT (and vice versa) — the
-- var-vs-field leading shape and its field-vs-var mirror are the same kill.
-- ⊢  HasMgu ρ₁ ρ₂  ↔  HasMgu ρ₂ ρ₁
theorem hasMgu_symm {B : Type} {ρ₁ ρ₂ : Row B} : HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₂ ρ₁ := by
  apply hasMgu_congr; intro θ; unfold Unifies
  exact ⟨RowEquiv.symm, RowEquiv.symm⟩

-- ## Predicate-based mgu: lifting no-mgu through the eq-EMITTING moves
-- The strip moves preserve the unifier set exactly, so `hasMgu_congr` (stated on
-- two ROW problems) discharges them. matchL/matchR/groundMatch instead emit a
-- type equation: a unifier of the original is EXACTLY a unifier of the residual
-- row problem that ALSO satisfies `τ ≐ τ'`. That is still a set of substitutions,
-- just not the unifier set of a bare row equation. Since `InstanceOf` mentions
-- only the substitutions (never the rows), mgu-status is a property of that SET,
-- whatever cuts it out. So we generalize `HasMgu` to an arbitrary unifier
-- PREDICATE and get a congruence that covers the eq-emitting moves for free.
def HasMguP {B : Type} (P : TySubst B → Prop) : Prop :=
  ∃ θ : TySubst B, P θ ∧ ∀ θ' : TySubst B, P θ' → InstanceOf θ' θ

-- `HasMgu` is the predicate version instantiated at the row-unifier predicate.
theorem hasMgu_eq_hasMguP {B : Type} (ρ₁ ρ₂ : Row B) :
    HasMgu ρ₁ ρ₂ ↔ HasMguP (fun θ => Unifies θ ρ₁ ρ₂) := Iff.rfl

-- ⊢  (∀θ. P θ ↔ Q θ)   ⟹   (HasMguP P ↔ HasMguP Q)
-- Pointwise-equal unifier predicates have the same mgu-status. This is the whole
-- reason no-mgu lifts through moves: they only ever REDESCRIBE the unifier set.
theorem hasMguP_congr {B : Type} {P Q : TySubst B → Prop}
    (h : ∀ θ : TySubst B, P θ ↔ Q θ) : HasMguP P ↔ HasMguP Q :=
  ⟨fun ⟨θ, hp, hmax⟩ => ⟨θ, (h θ).mp hp, fun θ' hp' => hmax θ' ((h θ').mpr hp')⟩,
   fun ⟨θ, hp, hmax⟩ => ⟨θ, (h θ).mpr hp, fun θ' hp' => hmax θ' ((h θ').mp hp')⟩⟩

-- ## Generalizing Wand: n distinct variables vs a single field has no mgu
-- The whole family (α₁ | … | αₙ) ≐ᵣ (l:𝓫), n ≥ 2 distinct vars — the general
-- "field with many variable hosts" stuck class. Same counting kill as
-- wand_no_mgu_count (which is the n=2 instance): the field's counts across the
-- vars sum to 1, so exactly one var hosts it; the witness that hosts it in ANY
-- other (distinct) var undercuts that one's count 1→0.

-- A substitution that fields w' and empties everything else sends a pure-var
-- spine NOT mentioning w' to ε (every factor is empty).
private theorem subst_empty_of_notMem {B : Type} (b : B) (l : Label) (w' : TyVar) :
    (ws : List TyVar) → w' ∉ ws →
    RowEquiv ((ofSpine (ws.map Atom.var)).applySubst
        ⟨fun x => .var x, fun x => if x = w' then .sing l (.base b) else .empty⟩)
      Row.empty
  | [], _ => by simp only [List.map_nil, ofSpine, Row.applySubst]; exact RowEquiv.refl _
  | c :: rest, hmem => by
      have hc : c ≠ w' := fun h => hmem (h ▸ List.mem_cons_self)
      have hrest : w' ∉ rest := fun h => hmem (List.mem_cons_of_mem _ h)
      simp only [List.map_cons, ofSpine, Row.applySubst]
      rw [if_neg hc]
      exact RowEquiv.unitL.trans (subst_empty_of_notMem b l w' rest hrest)

-- … and sends a pure-var spine that DOES mention w' (once, by nodup) to (l:𝓫).
private theorem witness_unifies {B : Type} (b : B) (l : Label) (w' : TyVar) :
    (ws : List TyVar) → ws.Nodup → w' ∈ ws →
    RowEquiv ((ofSpine (ws.map Atom.var)).applySubst
        ⟨fun x => .var x, fun x => if x = w' then .sing l (.base b) else .empty⟩)
      (.sing l (.base b))
  | [], _, hmem => by simp at hmem
  | c :: rest, hnd, hmem => by
      have hnd' := List.nodup_cons.mp hnd
      simp only [List.map_cons, ofSpine, Row.applySubst]
      by_cases hc : c = w'
      · subst hc
        rw [if_pos rfl]
        exact (RowEquiv.cat (RowEquiv.refl _)
                 (subst_empty_of_notMem b l c rest hnd'.1)).trans RowEquiv.unitR
      · rw [if_neg hc]
        have hmem' : w' ∈ rest := (List.mem_cons.mp hmem).resolve_left (fun h => hc h.symm)
        exact RowEquiv.unitL.trans (witness_unifies b l w' rest hnd'.2 hmem')

-- The l-count of a substituted pure-var spine is the sum of the vars' l-counts.
private theorem sFieldCount_ofSpine_map_var {B : Type} (θ : TySubst B) (l : Label) :
    (vs : List TyVar) →
    sFieldCount l ((ofSpine (vs.map Atom.var)).applySubst θ).toSpine
      = (vs.map (fun v => sFieldCount l (θ.row v).toSpine)).sum
  | [] => rfl
  | v :: vs => by
      simp only [List.map_cons, ofSpine, Row.applySubst, Row.toSpine,
                 sFieldCount_append, List.sum_cons]
      rw [sFieldCount_ofSpine_map_var θ l vs]

-- ⊢  vs nodup,  |vs| ≥ 2   ⟹   ¬ HasMgu (v₁ | … | vₙ) (l:𝓫)
theorem vars_vs_field_no_mgu {B : Type} {vs : List TyVar} (hnd : vs.Nodup)
    (hlen : 2 ≤ vs.length) (b : B) (l : Label) :
    ¬ HasMgu (ofSpine (vs.map Atom.var)) (.sing l (.base b)) := by
  apply no_mgu_of_witness_shrinks
  intro θ hu
  -- a pure-var spine whose l-counts sum to ≥1 has a variable carrying ≥1 l-field
  have hpos : ∀ (ns : List TyVar),
      1 ≤ (ns.map (fun v => sFieldCount l (θ.row v).toSpine)).sum →
      ∃ w ∈ ns, 1 ≤ sFieldCount l (θ.row w).toSpine := by
    intro ns
    induction ns with
    | nil => intro h; simp at h
    | cons a t ih =>
        intro h
        simp only [List.map_cons, List.sum_cons] at h
        by_cases ha : 1 ≤ sFieldCount l (θ.row a).toSpine
        · exact ⟨a, List.mem_cons_self, ha⟩
        · obtain ⟨w, hw, hwc⟩ := ih (by omega)
          exact ⟨w, List.mem_cons_of_mem a hw, hwc⟩
  have hsum : (vs.map (fun v => sFieldCount l (θ.row v).toSpine)).sum = 1 := by
    have h := rowEquiv_fieldCount_eq l hu
    rw [sFieldCount_ofSpine_map_var] at h
    simpa [Row.applySubst, Ty.applySubst, Row.toSpine, sFieldCount] using h
  obtain ⟨w, hwmem, hwc⟩ := hpos vs (by omega)
  obtain ⟨a, b', rest, rfl⟩ : ∃ a b' rest, vs = a :: b' :: rest := by
    match vs, hlen with
    | a :: b' :: rest, _ => exact ⟨a, b', rest, rfl⟩
  have hab : a ≠ b' := by
    intro h
    exact (List.nodup_cons.mp hnd).1 (by rw [h]; exact List.mem_cons_self)
  refine ⟨⟨fun x => .var x, fun x => if x = (if w = a then b' else a)
            then .sing l (.base b) else .empty⟩, w, l, ?_, ?_⟩
  · exact witness_unifies b l _ (a :: b' :: rest) hnd (by
      split
      · exact List.mem_cons_of_mem _ List.mem_cons_self
      · exact List.mem_cons_self)
  · have hne : (if w = a then b' else a) ≠ w := by
      split
      · rename_i h; subst h; exact fun hh => hab hh.symm
      · rename_i h; exact fun hh => h hh.symm
    show sFieldCount l (if w = (if w = a then b' else a) then Row.sing l (.base b)
                        else Row.empty).toSpine < sFieldCount l (θ.row w).toSpine
    rw [if_neg (fun hh => hne hh.symm)]
    simp only [Row.toSpine, sFieldCount]
    omega

-- Mirror of vars_vs_field_no_mgu (via hasMgu_symm): a single field facing ≥2
-- distinct variable hosts on the RIGHT. For a lone field to have any unifier the
-- other side must be var-free of foreign labels — any concrete k≠l field there is
-- an outright clash — so unifier-existence already forces the var-side spine
-- all-vars; this is exactly that canonical shape, flipped. Discharges the
-- var-vs-field leading shape of stuck_leading_shape (side-2 lone field, side-1
-- all-vars).
-- ⊢  vs nodup,  |vs| ≥ 2   ⟹   ¬ HasMgu (l:𝓫) (v₁ | … | vₙ)
theorem field_vs_vars_no_mgu {B : Type} {vs : List TyVar} (hnd : vs.Nodup)
    (hlen : 2 ≤ vs.length) (b : B) (l : Label) :
    ¬ HasMgu (.sing l (.base b)) (ofSpine (vs.map Atom.var)) :=
  fun h => vars_vs_field_no_mgu hnd hlen b l (hasMgu_symm.mp h)

-- ## The all-variable stuck class: (α | β) ≐ᵣ (β | α) has no mgu
-- The THIRD base technique: variable non-commutativity. Counting/rigidity cannot
-- fire (no fields), so the kill is combinatorial. Witnesses pin θα, θβ field-free;
-- the unifier equation gives their var-sequences A, B with A ++ B = B ++ A; the
-- position argument forces vars(A) ⊆ vars(B); then the (α↦l, β↦ε) witness — which
-- empties every var of B, hence of A — collapses θα to ε, contradicting the
-- l-field it must carry.

-- Reusable: proj is empty exactly when the l-count is 0 (length = count).
private theorem sProj_nil_of_fieldCount_zero {B : Type} {l : Label} {s : List (Atom B)}
    (h : sFieldCount l s = 0) : sProj l s = [] := by
  rcases hh : sProj l s with _ | ⟨p, ps⟩
  · rfl
  · exact absurd (by rw [← sProj_length_eq_sFieldCount, hh] at h; exact h)
      (by simp)

-- The combinatorial heart (Lyndon–Schützenberger, the membership fragment):
-- if A ++ B = B ++ A and B ≠ [], every element of A occurs in B. Proof by
-- first-occurrence index: x∈A sits at idxOf x A in A++B; if x∉B it sits at
-- idxOf x A + |B| in B++A; equal lists ⟹ |B| = 0.
theorem append_comm_subset {A Bl : List TyVar} (h : A ++ Bl = Bl ++ A) (hB : Bl ≠ [])
    {x : TyVar} (hxA : x ∈ A) : x ∈ Bl := by
  by_cases hxB : x ∈ Bl
  · exact hxB
  · exfalso
    have h1 : List.idxOf x (A ++ Bl) = List.idxOf x A := by
      rw [List.idxOf_append, if_pos hxA]
    have h2 : List.idxOf x (Bl ++ A) = List.idxOf x A + Bl.length := by
      rw [List.idxOf_append, if_neg hxB]
    rw [h, h2] at h1
    have hlen : Bl.length = 0 := by omega
    cases Bl with
    | nil => exact hB rfl
    | cons b bs => simp at hlen

-- A row vanishes under σ ⟹ every variable in its spine vanishes under σ.
theorem applySubst_empty_forces_vars {B : Type} (σ : TySubst B) :
    (R : Row B) → RowEquiv (R.applySubst σ) Row.empty →
    ∀ v ∈ sVarSeq R.toSpine, RowEquiv (σ.row v) Row.empty
  | .empty, _ => by intro v hv; simp [Row.toSpine, sVarSeq] at hv
  | .var α, h => by
      intro v hv
      simp only [Row.toSpine, sVarSeq, List.mem_singleton] at hv
      subst hv; simpa only [Row.applySubst] using h
  | .sing _ _, _ => by intro v hv; simp [Row.toSpine, sVarSeq] at hv
  | .cat R₁ R₂, h => by
      intro v hv
      simp only [Row.applySubst] at h
      obtain ⟨h₁, h₂⟩ := h.cat_empty_split
      simp only [Row.toSpine, sVarSeq_append, List.mem_append] at hv
      rcases hv with hv | hv
      · exact applySubst_empty_forces_vars σ R₁ h₁ v hv
      · exact applySubst_empty_forces_vars σ R₂ h₂ v hv

-- The converse for a FIELD-FREE row: if every variable of its spine vanishes
-- under σ, the whole row does.
theorem varsEmpty_forces_applySubst_empty {B : Type} (σ : TySubst B) :
    (R : Row B) → (∀ v ∈ sVarSeq R.toSpine, RowEquiv (σ.row v) Row.empty) →
    (∀ l, sFieldCount l R.toSpine = 0) → RowEquiv (R.applySubst σ) Row.empty
  | .empty, _, _ => RowEquiv.refl _
  | .var α, hv, _ => by
      simp only [Row.applySubst]
      exact hv α (by simp [Row.toSpine, sVarSeq])
  | .sing l _, _, hf => by
      exfalso; have := hf l; simp [Row.toSpine, sFieldCount] at this
  | .cat R₁ R₂, hv, hf => by
      simp only [Row.applySubst]
      have hv1 : ∀ v ∈ sVarSeq R₁.toSpine, RowEquiv (σ.row v) Row.empty := fun v hm =>
        hv v (by simp only [Row.toSpine, sVarSeq_append, List.mem_append]; exact Or.inl hm)
      have hv2 : ∀ v ∈ sVarSeq R₂.toSpine, RowEquiv (σ.row v) Row.empty := fun v hm =>
        hv v (by simp only [Row.toSpine, sVarSeq_append, List.mem_append]; exact Or.inr hm)
      have hf1 : ∀ l, sFieldCount l R₁.toSpine = 0 := fun l => by
        have := hf l; simp only [Row.toSpine, sFieldCount_append] at this; omega
      have hf2 : ∀ l, sFieldCount l R₂.toSpine = 0 := fun l => by
        have := hf l; simp only [Row.toSpine, sFieldCount_append] at this; omega
      exact (RowEquiv.cat (varsEmpty_forces_applySubst_empty σ R₁ hv1 hf1)
              (varsEmpty_forces_applySubst_empty σ R₂ hv2 hf2)).trans RowEquiv.unitL

-- ⊢  ¬ HasMgu (α | β) (β | α)      (the all-variable stuck class)
theorem allvar_swap_no_mgu {B : Type} :
    ¬ HasMgu (.cat (.var "α") (.var "β") : Row B) (.cat (.var "β") (.var "α")) := by
  rintro ⟨θ, hu, hmax⟩
  unfold Unifies at hu
  simp only [Row.applySubst] at hu
  -- the two witnesses (α↦l, β↦ε) and (α↦ε, β↦l), for any label l
  have hUu : ∀ l : Label, Unifies
      (⟨fun x => .var x, fun x => if x = "α" then .sing l .unk else .empty⟩ : TySubst B)
      (.cat (.var "α") (.var "β")) (.cat (.var "β") (.var "α")) := fun l => by
    unfold Unifies
    show RowEquiv (Row.cat (Row.sing l .unk) Row.empty) (Row.cat Row.empty (Row.sing l .unk))
    exact RowEquiv.unitR.trans RowEquiv.unitL.symm
  have hUu' : ∀ l : Label, Unifies
      (⟨fun x => .var x, fun x => if x = "α" then .empty else .sing l .unk⟩ : TySubst B)
      (.cat (.var "α") (.var "β")) (.cat (.var "β") (.var "α")) := fun l => by
    unfold Unifies
    show RowEquiv (Row.cat Row.empty (Row.sing l .unk)) (Row.cat (Row.sing l .unk) Row.empty)
    exact RowEquiv.unitL.trans RowEquiv.unitR.symm
  -- both images are field-free (an mgu is pointwise count-minimal, witnesses give 0)
  have hβfree : ∀ l, sFieldCount l (θ.row "β").toSpine = 0 := fun l => by
    have hle : sFieldCount l (θ.row "β").toSpine ≤ 0 :=
      instanceOf_fieldCount_mono (hmax _ (hUu l)) "β" l
    omega
  have hαfree : ∀ l, sFieldCount l (θ.row "α").toSpine = 0 := fun l => by
    have hle : sFieldCount l (θ.row "α").toSpine ≤ 0 :=
      instanceOf_fieldCount_mono (hmax _ (hUu' l)) "α" l
    omega
  -- the var-sequences commute: A ++ B = B ++ A
  obtain ⟨hvarseq, -⟩ := hu.char
  simp only [Row.toSpine, sVarSeq_append] at hvarseq
  -- B ≠ [] : else θβ (field-free + var-free) ≈ ε, refuting the (α↦ε,β↦l) witness
  have hBne : sVarSeq (θ.row "β").toSpine ≠ [] := by
    intro hBnil
    have hθβε : RowEquiv (θ.row "β") (Row.empty : Row B) :=
      RowEquiv.ofChar ⟨by rw [hBnil]; rfl,
        fun l => by rw [sProj_nil_of_fieldCount_zero (hβfree l)]; exact .nil⟩
    obtain ⟨σ', hrowσ', -⟩ := hmax _ (hUu' "x")
    have hfield : RowEquiv (Row.sing "x" (.unk : Ty B)) ((θ.row "β").applySubst σ') := by
      have := hrowσ' "β"; simpa using this
    have hemp : RowEquiv ((θ.row "β").applySubst σ') Row.empty := RowEquiv.applySubst σ' hθβε
    have hc := rowEquiv_fieldCount_eq "x" (hfield.trans hemp)
    simp [Row.toSpine, sFieldCount] at hc
  -- the (α↦l, β↦ε) witness factors: θβ vanishes, θα becomes (l:unk)
  obtain ⟨σ, hrowσ, -⟩ := hmax _ (hUu "x")
  have hσα : RowEquiv (Row.sing "x" (.unk : Ty B)) ((θ.row "α").applySubst σ) := by
    have := hrowσ "α"; simpa using this
  have hσβ : RowEquiv (Row.empty : Row B) ((θ.row "β").applySubst σ) := by
    have := hrowσ "β"; simpa using this
  -- every var of B vanishes under σ; by A ⊆ B so does every var of A
  have hBvars := applySubst_empty_forces_vars σ (θ.row "β") hσβ.symm
  have hAvars : ∀ v ∈ sVarSeq (θ.row "α").toSpine, RowEquiv (σ.row v) Row.empty := fun v hv =>
    hBvars v (append_comm_subset hvarseq hBne hv)
  -- so θα collapses to ε — contradicting the l-field it carries
  have hαε : RowEquiv ((θ.row "α").applySubst σ) Row.empty :=
    varsEmpty_forces_applySubst_empty σ (θ.row "α") hAvars hαfree
  have hc := rowEquiv_fieldCount_eq "x" (hσα.trans hαε)
  simp [Row.toSpine, sFieldCount] at hc

-- ## projClash soundness: the projection-clash direction of the trichotomy.
-- If projClash s₁ s₂, no θ can unify (ofSpine s₁) with (ofSpine s₂).
-- The ground side's l-count is fixed under substitution; the other side can
-- only grow; ≈ forces equality; omega closes the Nat arithmetic.
-- ⊢  projClash s₁ s₂   ⟹   ¬ ∃ θ. θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
theorem projClash_no_unifier {B : Type} {s₁ s₂ : List (Atom B)}
    (hclash : projClash s₁ s₂ = true) :
    ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s₁) (ofSpine s₂) := by
  unfold projClash at hclash
  rw [List.any_eq_true] at hclash
  obtain ⟨l, -, hl⟩ := hclash
  simp only [Bool.or_eq_true, Bool.and_eq_true, decide_eq_true_eq] at hl
  rintro ⟨θ, hunify⟩
  unfold Unifies at hunify
  have hfc := rowEquiv_fieldCount_eq l hunify
  have hle₁ : sFieldCount l s₁ ≤ sFieldCount l ((ofSpine s₁).applySubst θ).toSpine := by
    have h := sFieldCount_applySubst_le θ l (ofSpine s₁); rwa [ofSpine_toSpine] at h
  have hle₂ : sFieldCount l s₂ ≤ sFieldCount l ((ofSpine s₂).applySubst θ).toSpine := by
    have h := sFieldCount_applySubst_le θ l (ofSpine s₂); rwa [ofSpine_toSpine] at h
  rcases hl with ⟨hlt, hvf⟩ | ⟨hlt, hvf⟩
  · -- s₂ var-free: count fixed at sFieldCount l s₂ < sFieldCount l s₁
    have h₂ : sHasVar s₂ = false := by
      cases h : sHasVar s₂ with | false => rfl | true => simp [h] at hvf
    have heq₂ : sFieldCount l ((ofSpine s₂).applySubst θ).toSpine = sFieldCount l s₂ := by
      rw [sFieldCount_applySubst_varFree θ l (spineVarFree_ofSpine h₂), ofSpine_toSpine]
    omega
  · -- s₁ var-free: count fixed at sFieldCount l s₁ < sFieldCount l s₂
    have h₁ : sHasVar s₁ = false := by
      cases h : sHasVar s₁ with | false => rfl | true => simp [h] at hvf
    have heq₁ : sFieldCount l ((ofSpine s₁).applySubst θ).toSpine = sFieldCount l s₁ := by
      rw [sFieldCount_applySubst_varFree θ l (spineVarFree_ofSpine h₁), ofSpine_toSpine]
    omega

-- ## occurs: what the recursive-row check really guarantees
-- The occurs verdict (solveVar hitting `(sVarSeq s₂).contains α`) is CONSERVATIVE.
-- It is genuinely no-unifier only when the recursive variable is pinned by a
-- FIELD; an all-variable interior occurrence is unifiable by collapsing the
-- surrounding variables to ε.

-- A recursive variable α of ρ contributes its whole l-field count to (θρ) on top
-- of ρ's own explicit l-fields — because α occurs inside ρ, so θα is spliced in.
-- ⊢  α ∈ vars(spine ρ)   ⟹   count_l(spine ρ) + count_l(spine (θα)) ≤ count_l(spine (θρ))
theorem fieldCount_var_lower {B : Type} (θ : TySubst B) (l : Label) (α : TyVar) :
    (ρ : Row B) → α ∈ sVarSeq ρ.toSpine →
    sFieldCount l ρ.toSpine + sFieldCount l (θ.row α).toSpine
      ≤ sFieldCount l (ρ.applySubst θ).toSpine
  | .empty, h => by simp [Row.toSpine, sVarSeq] at h
  | .sing _ _, h => by simp [Row.toSpine, sVarSeq] at h
  | .var β, h => by
      simp only [Row.toSpine, sVarSeq, List.mem_singleton] at h
      subst h; simp [Row.applySubst, Row.toSpine, sFieldCount]
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.applySubst, Row.toSpine, sFieldCount_append, sVarSeq_append,
        List.mem_append] at h ⊢
      rcases h with h | h
      · have ih := fieldCount_var_lower θ l α ρ₁ h
        have hle := sFieldCount_applySubst_le θ l ρ₂
        omega
      · have ih := fieldCount_var_lower θ l α ρ₂ h
        have hle := sFieldCount_applySubst_le θ l ρ₁
        omega

-- The GENUINE occurs case: α occurs recursively in s₂ AND some label l has a
-- field there. ≈ pins l's count, but θα would have to hold that field's l-count
-- both on its own (as the lhs) and again inside the rhs — an impossible strict
-- growth. Mirrors projClash_no_unifier (the clash direction).
-- ⊢  α ∈ vars s₂,  0 < count_l s₂   ⟹   ¬ ∃ θ. θ ⊨ α ≐ᵣ ofSpine s₂
theorem occurs_field_no_unifier {B : Type} {α : TyVar} {s₂ : List (Atom B)}
    {l : Label} (hmem : α ∈ sVarSeq s₂) (hfield : 0 < sFieldCount l s₂) :
    ¬ ∃ θ : TySubst B, Unifies θ (.var α) (ofSpine s₂) := by
  rintro ⟨θ, hu⟩
  unfold Unifies at hu
  have hfc := rowEquiv_fieldCount_eq l hu
  simp only [Row.applySubst] at hfc
  have hlb := fieldCount_var_lower θ l α (ofSpine s₂) (by rw [ofSpine_toSpine]; exact hmem)
  rw [ofSpine_toSpine] at hlb
  omega

-- occurs is CONSERVATIVE, not sound-for-no-unifier: the all-variable interior
-- occurrence α ≐ᵣ (β | α | γ) is REPORTED occurs yet IS unifiable — take β,γ ↦ ε.
-- Only the field-pinned case (occurs_field_no_unifier) is a real non-unifier.
-- ⊢  unifyRow α (β|α|γ) = occurs   ∧   ∃ θ. θ ⊨ α ≐ᵣ (β|α|γ)
theorem occurs_allVar_unifiable {B : Type} :
    unifyRow (B := B) (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c")))
        = .occurs
    ∧ ∃ θ : TySubst B,
        Unifies θ (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c"))) :=
  ⟨rfl,
   ⟨⟨(.var ·), fun x => if x = "b" then .empty else if x = "c" then .empty else .var x⟩,
    by unfold Unifies
       simp only [Row.applySubst]
       exact (RowEquiv.unitL.trans RowEquiv.unitR).symm⟩⟩

-- ## Base-case clash: the OTHER place the algorithm answers clash
-- unifySpineF returns clash when one side is exhausted but the other still
-- carries a field (`allVarsEmpty = none`). This is the second half of clash
-- soundness (projClash_no_unifier is the interior/projection half): a field on
-- the leftover side has nowhere to go against the empty row.

-- allVarsEmpty fails exactly when a field remains: it walks vars, stops at a field.
-- ⊢  allVarsEmpty s = none   ⟹   ∃ l. 0 < count_l s
theorem allVarsEmpty_none_field {B : Type} :
    (s : List (Atom B)) → allVarsEmpty s = none → ∃ l, 0 < sFieldCount l s
  | [], h => by simp [allVarsEmpty] at h
  | .field l _ :: s, _ => ⟨l, by
      show 0 < (if l = l then 1 else 0) + sFieldCount l s
      rw [if_pos rfl]; omega⟩
  | .var _ :: s, h => by
      cases hs : allVarsEmpty s with
      | some σ => simp [allVarsEmpty, hs] at h
      | none =>
        obtain ⟨l, hl⟩ := allVarsEmpty_none_field s hs
        exact ⟨l, by simpa [sFieldCount] using hl⟩

-- ε cannot unify a row that still holds a field: ≈ pins the l-count at 0 on the
-- left, but substitution can only keep the leftover field's count positive.
-- ⊢  0 < count_l s   ⟹   ¬ ∃ θ. θ ⊨ ε ≐ᵣ ofSpine s
theorem empty_no_unifier {B : Type} {s : List (Atom B)} {l : Label}
    (hfield : 0 < sFieldCount l s) :
    ¬ ∃ θ : TySubst B, Unifies θ Row.empty (ofSpine s) := by
  rintro ⟨θ, hu⟩
  unfold Unifies at hu
  have hfc := rowEquiv_fieldCount_eq l hu
  simp only [Row.applySubst, Row.toSpine, sFieldCount] at hfc
  have hle := sFieldCount_applySubst_le θ l (ofSpine s)
  rw [ofSpine_toSpine] at hle
  omega

-- The base-case clash is sound (both orientations, by ≈-symmetry).
-- ⊢  allVarsEmpty s = none   ⟹   ¬ ∃ θ. θ ⊨ ε ≐ᵣ ofSpine s
theorem allVarsEmpty_none_no_unifier {B : Type} {s : List (Atom B)}
    (h : allVarsEmpty s = none) :
    ¬ ∃ θ : TySubst B, Unifies θ Row.empty (ofSpine s) :=
  let ⟨_, hl⟩ := allVarsEmpty_none_field s h
  empty_no_unifier hl

-- ⊢  allVarsEmpty s = none   ⟹   ¬ ∃ θ. θ ⊨ ofSpine s ≐ᵣ ε
theorem allVarsEmpty_none_no_unifier' {B : Type} {s : List (Atom B)}
    (h : allVarsEmpty s = none) :
    ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s) Row.empty := by
  rintro ⟨θ, hu⟩
  refine allVarsEmpty_none_no_unifier h ⟨θ, ?_⟩
  unfold Unifies at hu ⊢
  exact hu.symm

------------------------- ≐ᵣ SUCCESS SOUNDNESS (SETUP) -----------------------
-- The success case emits a row-var solution list σ and residual type
-- equations eqs. A substitution θ "extends σ" when it agrees with every
-- binding (α ≔ ρ) up to ≈ under θ, and "satisfies eqs" when it makes every
-- emitted pair ≈-equal. Soundness (below/next): under both, θ unifies the
-- original rows. The individual MOVE-REFLECTION lemmas here are the reusable
-- content — each says "if θ unifies the residual, it unified the original".

def SolSat {B : Type} (θ : TySubst B) (σ : List (TyVar × Row B)) : Prop :=
  ∀ p ∈ σ, RowEquiv (θ.row p.1) (p.2.applySubst θ)

def EqsSat {B : Type} (θ : TySubst B) (eqs : List (Ty B × Ty B)) : Prop :=
  ∀ p ∈ eqs, TyEquiv (p.1.applySubst θ) (p.2.applySubst θ)

theorem EqsSat.cons {B : Type} {θ : TySubst B} {τ τ' : Ty B} {eqs : List (Ty B × Ty B)}
    (hty : TyEquiv (τ.applySubst θ) (τ'.applySubst θ)) (h : EqsSat θ eqs) :
    EqsSat θ ((τ, τ') :: eqs) := by
  intro p hp
  rcases List.mem_cons.mp hp with rfl | hp'
  · exact hty
  · exact h p hp'

-- addEq only prepends to the eqs of a success; it inverts cleanly.
-- ⊢  r.addEq τ τ' = success σ eqs
--        ⟹  ∃ eqs'. r = success σ eqs' ∧ eqs = (τ,τ')::eqs'
theorem URes.addEq_success {B : Type} {τ τ' : Ty B} {r : URes B}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)} :
    r.addEq τ τ' = .success σ eqs →
    ∃ eqs', r = .success σ eqs' ∧ eqs = (τ, τ') :: eqs' := by
  cases r with
  | success σ₀ eqs₀ =>
      intro h; simp only [URes.addEq] at h
      obtain ⟨hσ, heq⟩ := URes.success.inj h
      exact ⟨eqs₀, by rw [hσ], heq.symm⟩
  | clash  => intro h; cases (h : URes.clash = .success σ eqs)
  | occurs => intro h; cases (h : URes.occurs = .success σ eqs)
  | stuck  => intro h; cases (h : URes.stuck = .success σ eqs)

-- ## ofSpine cons is definitionally a cat (kept as named rewrites).
-- ⊢  ofSpine (α :: s) = (α | ofSpine s)
theorem ofSpine_var_cons {B : Type} (α : TyVar) (s : List (Atom B)) :
    ofSpine (.var α :: s) = .cat (.var α) (ofSpine s) := rfl

-- ⊢  ofSpine ((l:τ) :: s) = (l:τ | ofSpine s)
theorem ofSpine_field_cons {B : Type} (l : Label) (τ : Ty B) (s : List (Atom B)) :
    ofSpine (.field l τ :: s) = .cat (.sing l τ) (ofSpine s) := rfl

-- ## Move-reflection lemmas
-- Each: "if θ unifies the residual (and satisfies the emitted binding/eq),
-- then θ unified the original pair." These are the per-rule soundness steps.

-- U-var-refl (left): a shared leading var contributes θα to both sides, so it
-- drops out of the equivalence — no constraint on θ.
-- Inversion: stripL succeeds only on a shared leading var.
-- ⊢  stripL s₁ s₂ = some (t₁,t₂)   ⟹   ∃ α. s₁ = α::t₁ ∧ s₂ = α::t₂
theorem stripL_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} :
    stripL s₁ s₂ = some (t₁, t₂) → ∃ α, s₁ = .var α :: t₁ ∧ s₂ = .var α :: t₂ := by
  cases s₁ with
  | nil => simp [stripL]
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [stripL]
    | var α =>
      cases s₂ with
      | nil => simp [stripL]
      | cons a₂ r₂ =>
        cases a₂ with
        | field _ _ => simp [stripL]
        | var β =>
          intro h
          simp only [stripL] at h
          split at h
          · rename_i hαβ
            simp only [Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            exact ⟨α, rfl, by rw [hαβ]⟩
          · simp at h

-- Inversion: stripR succeeds only on a shared trailing var.
-- ⊢  stripR s₁ s₂ = some (t₁,t₂)   ⟹   ∃ α. s₁ = t₁++[α] ∧ s₂ = t₂++[α]
theorem stripR_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} :
    stripR s₁ s₂ = some (t₁, t₂) → ∃ α, s₁ = t₁ ++ [.var α] ∧ s₂ = t₂ ++ [.var α] := by
  intro h
  unfold stripR at h
  cases hsl : stripL s₁.reverse s₂.reverse with
  | none => rw [hsl] at h; simp at h
  | some p =>
    rw [hsl] at h
    obtain ⟨u₁, u₂⟩ := p
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    obtain ⟨α, hr₁, hr₂⟩ := stripL_inv hsl
    refine ⟨α, ?_, ?_⟩
    · rw [← List.reverse_reverse s₁, hr₁]; simp
    · rw [← List.reverse_reverse s₂, hr₂]; simp

-- U-var-refl (left): a shared leading var contributes θα to both sides, so it
-- drops out of the equivalence — no constraint on θ.
-- ⊢  stripL s₁ s₂ = some (t₁,t₂),  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem stripL_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv hstrip
  simp only [ofSpine_var_cons, Row.applySubst]
  exact RowEquiv.cat (.refl _) hrec

-- U-var-refl (right): the same at the trailing end, via ofSpine over append.
-- ⊢  stripR s₁ s₂ = some (t₁,t₂),  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem stripR_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv hstrip
  have e₁ := RowEquiv.applySubst θ (ofSpine_append t₁ [Atom.var α])
  have e₂ := RowEquiv.applySubst θ (ofSpine_append t₂ [Atom.var α])
  simp only [ofSpine, Row.applySubst] at e₁ e₂
  exact e₁.trans ((RowEquiv.cat hrec (.refl _)).trans e₂.symm)

-- U-var-solve: s₁ = [var α], θ satisfies α ≔ ofSpine s₂ ⟹ θ unifies.
-- ⊢  solveVar s₁ s₂ = some (success σ eqs),  θ ⊨ σ
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem solveVar_reflect {B : Type} {θ : TySubst B} {s₁ s₂ : List (Atom B)}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (hsolve : solveVar s₁ s₂ = some (.success σ eqs))
    (hsol : SolSat θ σ) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  cases s₁ with
  | nil => simp [solveVar] at hsolve
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVar] at hsolve
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVar] at hsolve
      | nil =>
        simp only [solveVar] at hsolve
        split at hsolve
        · simp at hsolve
        · simp only [Option.some.injEq, URes.success.injEq] at hsolve
          obtain ⟨rfl, -⟩ := hsolve
          have hbind := hsol (α, ofSpine s₂) (by simp)
          simp only [ofSpine, Row.applySubst]
          exact RowEquiv.unitR.trans hbind

-- U-field (left): a leading field matched against the other side's window.
-- windowExtract bubbles the matched field to the front (distinct-label comm).
-- ⊢  windowExtract l s = some (τ,s')   ⟹   ofSpine s ≈ᵣ (l:τ | ofSpine s')
theorem windowExtract_equiv {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') →
    RowEquiv (ofSpine s) (.cat (.sing l τ) (ofSpine s'))
  | [], _, _, h => by simp [windowExtract] at h
  | .var β :: s, _, _, h => by simp [windowExtract] at h
  | .field l' τ₀ :: s, τ, s', h => by
      simp only [windowExtract] at h
      split at h
      · rename_i hl
        subst hl
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact RowEquiv.refl _
      · rename_i hl
        split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := windowExtract_equiv l s hwe
          simp only [ofSpine_field_cons]
          exact (RowEquiv.cat (.refl _) ih).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (.comm hl) (.refl _)).trans RowEquiv.assoc))
        · simp at h

-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem matchL_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  cases s₁ with
  | nil => simp [matchL] at hmatch
  | cons a₁ r₁ =>
    cases a₁ with
    | var α => simp [matchL] at hmatch
    | field l τ₀ =>
      simp only [matchL] at hmatch
      split at hmatch
      · rename_i τ'' s₂' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
        obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
        have hw := RowEquiv.applySubst θ (windowExtract_equiv l s₂ hwe)
        simp only [ofSpine_field_cons, Row.applySubst]
        refine RowEquiv.trans ?_ hw.symm
        simp only [Row.applySubst]
        exact RowEquiv.cat (.sing heq) hrec
      · simp at hmatch

-- Base case: an all-vars remainder, each solved to ε, unifies with the empty
-- side (allVarsEmpty forces every var to ε).
-- ⊢  allVarsEmpty s = some σ,  θ ⊨ σ   ⟹   θ(ofSpine s) ≈ᵣ ε
theorem allVarsEmpty_sound {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → SolSat θ σ →
    RowEquiv ((ofSpine s).applySubst θ) .empty
  | [], _, hσ, _ => by
      simp only [allVarsEmpty, Option.some.injEq] at hσ; subst hσ; exact .refl _
  | .field _ _ :: _, _, hσ, _ => by simp [allVarsEmpty] at hσ
  | .var α :: s, σ, hσ, hsol => by
      simp only [allVarsEmpty] at hσ
      cases hs : allVarsEmpty s with
      | none => rw [hs] at hσ; simp at hσ
      | some σ' =>
        rw [hs] at hσ
        have hσ' : σ = (α, Row.empty) :: σ' := (Option.some.inj hσ).symm
        subst hσ'
        have hhead := hsol (α, Row.empty) (by simp)
        have htail : SolSat θ σ' := fun p hp => hsol p (by simp [hp])
        have ih := allVarsEmpty_sound s hs htail
        simp only [ofSpine_var_cons, Row.applySubst]
        exact (RowEquiv.cat hhead ih).trans RowEquiv.unitL

-- Completeness counterpart: if θ makes ofSpine s collapse to ε (the exhausted-
-- side unifier), then θ satisfies every ε-binding allVarsEmpty emitted.
-- ⊢  allVarsEmpty s = some σ,  θ(ofSpine s) ≈ᵣ ε   ⟹   SolSat θ σ
theorem allVarsEmpty_complete {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → RowEquiv ((ofSpine s).applySubst θ) .empty →
    SolSat θ σ
  | [], _, hσ, _ => by
      simp only [allVarsEmpty, Option.some.injEq] at hσ; subst hσ
      intro p hp; simp at hp
  | .field _ _ :: _, _, hσ, _ => by simp [allVarsEmpty] at hσ
  | .var α :: s, σ, hσ, hu => by
      simp only [allVarsEmpty] at hσ
      cases hs : allVarsEmpty s with
      | none => rw [hs] at hσ; simp at hσ
      | some σ' =>
        rw [hs] at hσ
        have hσ' : σ = (α, Row.empty) :: σ' := (Option.some.inj hσ).symm
        subst hσ'
        simp only [ofSpine_var_cons, Row.applySubst] at hu
        obtain ⟨hα, hrest⟩ := hu.cat_empty_split
        have ih := allVarsEmpty_complete s hs hrest
        intro p hp
        rcases List.mem_cons.mp hp with rfl | hp'
        · simp only [Row.applySubst]; exact hα
        · exact ih p hp'

-- Row reversal (reverses cat order): the algebraic image of List.reverse on
-- spines. Used only to transport windowExtract_equiv from s.reverse to s — no
-- List.reverseRecOn is available (no Batteries dep), so we go through revRow.
def revRow {B : Type} : Row B → Row B
  | .empty     => .empty
  | .var α     => .var α
  | .sing l τ  => .sing l τ
  | .cat ρ₁ ρ₂ => .cat (revRow ρ₂) (revRow ρ₁)

-- ⊢  revRow (revRow ρ) = ρ
theorem revRow_involutive {B : Type} : (ρ : Row B) → revRow (revRow ρ) = ρ
  | .empty     => rfl
  | .var _     => rfl
  | .sing _ _  => rfl
  | .cat a b   => by simp only [revRow, revRow_involutive a, revRow_involutive b]

-- ⊢  a ≈ᵣ b   ⟹   revRow a ≈ᵣ revRow b
theorem RowEquiv.revRow {B : Type} :
    {a b : Row B} → RowEquiv a b → RowEquiv (revRow a) (revRow b)
  | _, _, .refl _      => .refl _
  | _, _, .symm h      => (RowEquiv.revRow h).symm
  | _, _, .trans h₁ h₂ => (RowEquiv.revRow h₁).trans (RowEquiv.revRow h₂)
  | _, _, .sing hty    => .sing hty
  | _, _, .cat h₁ h₂   => .cat (RowEquiv.revRow h₂) (RowEquiv.revRow h₁)
  | _, _, .assoc       => RowEquiv.assoc.symm
  | _, _, .unitL       => RowEquiv.unitR
  | _, _, .unitR       => RowEquiv.unitL
  | _, _, .comm hne    => RowEquiv.comm (fun h => hne h.symm)

-- ofSpine of a reversed spine is the row-reversal of ofSpine (mod ≈).
-- ⊢  ofSpine (reverse as) ≈ᵣ revRow (ofSpine as)
theorem ofSpine_reverse_equiv {B : Type} : (as : List (Atom B)) →
    RowEquiv (ofSpine as.reverse) (revRow (ofSpine as))
  | [] => .refl _
  | .field l τ :: t => by
      rw [List.reverse_cons]
      refine (ofSpine_append t.reverse [Atom.field l τ]).trans ?_
      simp only [ofSpine, revRow]
      exact RowEquiv.cat (ofSpine_reverse_equiv t) RowEquiv.unitR
  | .var α :: t => by
      rw [List.reverse_cons]
      refine (ofSpine_append t.reverse [Atom.var α]).trans ?_
      simp only [ofSpine, revRow]
      exact RowEquiv.cat (ofSpine_reverse_equiv t) RowEquiv.unitR

-- U-field (right): the trailing-end mirror of matchL. matchR runs windowExtract
-- on the REVERSED spines, so the matched field sits in the trailing var-free
-- window and bubbles to the RIGHT end past distinct-label fields only. We get
-- this by transporting windowExtract_equiv through revRow.
-- ⊢  windowExtract l (reverse s) = some (τ,q)
--        ⟹   ofSpine s ≈ᵣ (ofSpine (reverse q) | l:τ)
theorem windowExtract_reverse_equiv {B : Type} (l : Label) (s : List (Atom B))
    {τ : Ty B} {q : List (Atom B)}
    (h : windowExtract l s.reverse = some (τ, q)) :
    RowEquiv (ofSpine s) (.cat (ofSpine q.reverse) (.sing l τ)) := by
  have hwe := windowExtract_equiv l s.reverse h
  have h1 : RowEquiv (revRow (ofSpine s)) (.cat (.sing l τ) (ofSpine q)) :=
    (ofSpine_reverse_equiv s).symm.trans hwe
  have h2 := RowEquiv.revRow h1
  rw [revRow_involutive (ofSpine s)] at h2
  simp only [revRow] at h2
  exact h2.trans (RowEquiv.cat (ofSpine_reverse_equiv q).symm (.refl _))

-- Inversion of a leading field-match.
-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂)
--        ⟹  ∃ l. s₁ = (l:τ)::t₁ ∧ windowExtract l s₂ = some (τ',t₂)
theorem matchL_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B} :
    matchL s₁ s₂ = some (τ, τ', t₁, t₂) →
    ∃ l, s₁ = .field l τ :: t₁ ∧ windowExtract l s₂ = some (τ', t₂) := by
  cases s₁ with
  | nil => simp [matchL]
  | cons a₁ r₁ =>
    cases a₁ with
    | var α => simp [matchL]
    | field l τ₀ =>
      intro h
      simp only [matchL] at h
      split at h
      · rename_i τ'' s₂' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl, rfl⟩ := h
        exact ⟨l, rfl, hwe⟩
      · simp at h

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)      (trailing-field mirror)
theorem matchR_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  -- matchR = matchL on the reverses; recover the leading-field inversion there.
  unfold matchR at hmatch
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at hmatch; simp at hmatch
  | some p =>
    rw [hml] at hmatch
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    -- matchR returns (τa, τb, u₁.reverse, u₂.reverse); pin the theorem vars.
    simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
    obtain ⟨l, hrev, hwe⟩ := matchL_inv hml
    -- hrev : s₁.reverse = field l τa :: u₁ ; so s₁ = u₁.reverse ++ [field l τa]
    have hs₁ : s₁ = u₁.reverse ++ [Atom.field l τa] := by
      rw [← List.reverse_reverse s₁, hrev]; simp
    -- windowExtract l s₂.reverse = some (τb, u₂); right-bubble on s₂.
    have hs₂equiv := windowExtract_reverse_equiv l s₂ hwe
    have hs₁equiv : RowEquiv (ofSpine s₁) (.cat (ofSpine u₁.reverse) (.sing l τa)) := by
      rw [hs₁]
      exact (ofSpine_append u₁.reverse [Atom.field l τa]).trans
        (RowEquiv.cat (.refl _) RowEquiv.unitR)
    have e₁ := RowEquiv.applySubst θ hs₁equiv
    have e₂ := RowEquiv.applySubst θ hs₂equiv
    simp only [Row.applySubst] at e₁ e₂
    refine e₁.trans (RowEquiv.trans ?_ e₂.symm)
    exact RowEquiv.cat hrec (.sing heq)

-- ## FORWARD reflection: a unifier of the ORIGINAL unifies the RESIDUAL (+ eqs)
-- The converse of the *_reflect lemmas — the COMPLETENESS direction each move
-- needs. For strip this is just cancellativity (shared θα prefix/suffix). For
-- match it is leading/trailing-field cancellation (field_cancel_left/right),
-- which additionally EXTRACTS the emitted type equation θτ ≈ θτ'. Together these
-- say every move preserves the unifier set, so no-unifier propagates backwards.

theorem RowEquiv.field_cancel_right {B : Type} {l : Label} {τ₁ τ₂ : Ty B}
    {R₁ R₂ : Row B}
    (h : RowEquiv (.cat R₁ (.sing l τ₁)) (.cat R₂ (.sing l τ₂))) :
    TyEquiv τ₁ τ₂ ∧ RowEquiv R₁ R₂ := by
  obtain ⟨hty, hR⟩ := (h.revRow).field_cancel_left
  refine ⟨hty, ?_⟩
  have hRR := hR.revRow
  rwa [revRow_involutive, revRow_involutive] at hRR

-- ⊢  stripL s₁ s₂ = some (t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem stripL_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv hstrip
  simp only [ofSpine, Row.applySubst] at hu
  exact hu.cancel_cat_left

-- ⊢  stripR s₁ s₂ = some (t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem stripR_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv hstrip
  have e₁ := RowEquiv.applySubst θ (ofSpine_append t₁ [Atom.var α])
  have e₂ := RowEquiv.applySubst θ (ofSpine_append t₂ [Atom.var α])
  simp only [ofSpine, Row.applySubst] at e₁ e₂
  exact (e₁.symm.trans (hu.trans e₂)).cancel_cat_right

-- ## Strip moves reflect no-mgu (the unifier-set-preserving case of the lift)
-- stripL/stripR cancel a shared end-var, and reflect + reflect_fwd together show
-- they preserve the unifier set EXACTLY. So mgu-status transfers both ways: a
-- config that gets stuck purely by end-stripping inherits the no-mgu verdict of
-- its stripped core. (matchL/matchR/groundMatch emit a type equation, so they do
-- NOT preserve the unifier set — those need the harder augmented-witness lift.)
-- ⊢  stripL s₁ s₂ = some (t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--                                       HasMgu (ofSpine t₁)(ofSpine t₂))
theorem stripL_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔ HasMgu (ofSpine t₁) (ofSpine t₂) :=
  hasMgu_congr (fun _ => ⟨fun hu => stripL_reflect_fwd hstrip hu,
                          fun hu => stripL_reflect hstrip hu⟩)

-- ⊢  stripR s₁ s₂ = some (t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--                                       HasMgu (ofSpine t₁)(ofSpine t₂))
theorem stripR_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔ HasMgu (ofSpine t₁) (ofSpine t₂) :=
  hasMgu_congr (fun _ => ⟨fun hu => stripR_reflect_fwd hstrip hu,
                          fun hu => stripR_reflect hstrip hu⟩)

-- End-to-end demonstration that the pieces compose: the Wand core wrapped in a
-- shared leading var. stripL peels γ, the stripped core is Wand's no-mgu config,
-- so the whole thing has no mgu — obtained mechanically from stripL_hasMgu_iff +
-- hasMgu_rowEquiv + wand_no_mgu_count. (This is exactly how the eventual lift
-- discharges its strip arms.)
-- ⊢  ¬ HasMgu (γ | β | α) (γ | l:𝓫)
theorem wand_under_strip_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ HasMgu (ofSpine [Atom.var "γ", .var "β", .var "α"])
             (ofSpine [Atom.var "γ", .field l (.base b)]) := by
  have hs : stripL ([Atom.var "γ", .var "β", .var "α"] : List (Atom B))
                   [Atom.var "γ", .field l (.base b)]
          = some ([.var "β", .var "α"], [.field l (.base b)]) := rfl
  rw [stripL_hasMgu_iff hs]
  simp only [ofSpine]
  rw [hasMgu_rowEquiv (ρ₁' := .cat (.var "β") (.var "α")) (ρ₂' := .sing l (.base b))
        (RowEquiv.cat (RowEquiv.refl _) RowEquiv.unitR) RowEquiv.unitR]
  exact wand_no_mgu_count b l

-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem matchL_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨l, rfl, hwe⟩ := matchL_inv hmatch
  have hs₂ := RowEquiv.applySubst θ (windowExtract_equiv l s₂ hwe)
  simp only [ofSpine, Row.applySubst] at hu hs₂
  exact (hu.trans hs₂).field_cancel_left

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem matchR_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  unfold matchR at hmatch
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at hmatch; simp at hmatch
  | some p =>
    rw [hml] at hmatch
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
    obtain ⟨l, hrev, hwe⟩ := matchL_inv hml
    have hs₁ : s₁ = u₁.reverse ++ [Atom.field l τa] := by
      rw [← List.reverse_reverse s₁, hrev]; simp
    have hs₂equiv := windowExtract_reverse_equiv l s₂ hwe
    have hs₁equiv : RowEquiv (ofSpine s₁) (.cat (ofSpine u₁.reverse) (.sing l τa)) := by
      rw [hs₁]
      exact (ofSpine_append u₁.reverse [Atom.field l τa]).trans
        (RowEquiv.cat (.refl _) RowEquiv.unitR)
    have e₁ := RowEquiv.applySubst θ hs₁equiv
    have e₂ := RowEquiv.applySubst θ hs₂equiv
    simp only [Row.applySubst] at e₁ e₂
    exact (e₁.symm.trans (hu.trans e₂)).field_cancel_right

-- ## U-ground: the reusable algebraic core
-- A field ≈-commutes past a row that is BOTH var-free and l-free. (Past a var
-- it would NOT commute — shadowing — so both hypotheses are essential; this is
-- exactly why groundMatch's soundness is conditional on the skipped vars being
-- l-free under θ, which the counting forces.)
-- ⊢  R var-free,  count_l(spine R) = 0   ⟹   (l:τ | R) ≈ᵣ (R | l:τ)
theorem field_comm_lfree {B : Type} (l : Label) (τ : Ty B) :
    (R : Row B) → R.SpineVarFree → sFieldCount l R.toSpine = 0 →
    RowEquiv (.cat (.sing l τ) R) (.cat R (.sing l τ))
  | .empty, _, _ => RowEquiv.unitR.trans RowEquiv.unitL.symm
  | .var _, hv, _ => nomatch hv
  | .sing l' τ', _, hc => by
      have hne : l' ≠ l := by
        intro h; subst h; simp [Row.toSpine, sFieldCount] at hc
      exact RowEquiv.comm (fun h => hne h.symm)
  | .cat R₁ R₂, .cat hv₁ hv₂, hc => by
      rw [Row.toSpine, sFieldCount_append] at hc
      have hc₁ : sFieldCount l R₁.toSpine = 0 := by omega
      have hc₂ : sFieldCount l R₂.toSpine = 0 := by omega
      have IH₁ := field_comm_lfree l τ R₁ hv₁ hc₁
      have IH₂ := field_comm_lfree l τ R₂ hv₂ hc₂
      exact RowEquiv.assoc.symm.trans
        ((RowEquiv.cat IH₁ (.refl _)).trans
          (RowEquiv.assoc.trans
            ((RowEquiv.cat (.refl _) IH₂).trans RowEquiv.assoc.symm)))

-- removeField pulls the matched l-field to the front — UNDER θ, provided every
-- variable of the spine is var-free and l-free under θ (vacuous when the spine
-- is var-free, e.g. the ground side). The var case is the only one that needs
-- field_comm_lfree; the field case only crosses distinct labels (comm).
-- ⊢  removeField l s = some (τ,t),
--       (∀ β ∈ vars(s). θβ var-free ∧ count_l(spine θβ) = 0)
--        ⟹   θ(ofSpine s) ≈ᵣ (l:θτ | θ(ofSpine t))
theorem removeField_equiv_of {B : Type} {θ : TySubst B} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) →
    (∀ β ∈ sVarSeq s, (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0) →
    RowEquiv ((ofSpine s).applySubst θ)
             (.cat (.sing l (τ.applySubst θ)) ((ofSpine t).applySubst θ))
  | [], _, _, h, _ => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h, hvars => by
      simp only [removeField] at h
      split at h
      · rename_i hl'
        subst hl'
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [ofSpine_field_cons, Row.applySubst]
        exact .refl _
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have hvars' : ∀ β ∈ sVarSeq s,
              (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0 :=
            fun β hβ => hvars β (by simp only [sVarSeq]; exact hβ)
          have IH := removeField_equiv_of l s hrem hvars'
          simp only [ofSpine_field_cons, Row.applySubst]
          exact (RowEquiv.cat (.refl _) IH).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (RowEquiv.comm hl') (.refl _)).trans RowEquiv.assoc))
        · simp at h
  | .var β :: s, τ, t, h, hvars => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have hβ : (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0 :=
          hvars β (by simp [sVarSeq])
        have hvars' : ∀ γ ∈ sVarSeq s,
            (θ.row γ).SpineVarFree ∧ sFieldCount l (θ.row γ).toSpine = 0 :=
          fun γ hγ => hvars γ (by simp only [sVarSeq]; exact List.mem_cons_of_mem β hγ)
        have IH := removeField_equiv_of l s hrem hvars'
        simp only [ofSpine_var_cons, Row.applySubst]
        exact (RowEquiv.cat (.refl _) IH).trans
          (RowEquiv.assoc.symm.trans
            ((RowEquiv.cat
              (field_comm_lfree l (τ''.applySubst θ) (θ.row β) hβ.1 hβ.2).symm (.refl _)).trans
              RowEquiv.assoc))
      · simp at h

-- ## U-ground: the counting (measurement lemmas)
-- removeField keeps the variable sequence (it only deletes a concrete field).
-- ⊢  removeField l s = some (τ,t)   ⟹   vars(t) = vars(s)
theorem removeField_sVarSeq {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) → sVarSeq t = sVarSeq s
  | [], _, _, h => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          simp only [sVarSeq]; exact removeField_sVarSeq l s hrem
        · simp at h
  | .var β :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sVarSeq]; rw [removeField_sVarSeq l s hrem]
      · simp at h

-- removeField deletes exactly one l-field.
-- ⊢  removeField l s = some (τ,t)   ⟹   count_l(s) = count_l(t) + 1
theorem removeField_sFieldCount {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) → sFieldCount l s = sFieldCount l t + 1
  | [], _, _, h => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i hl'
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sFieldCount, if_pos hl']; omega
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have hcnt := removeField_sFieldCount l s hrem
          simp only [sFieldCount, if_neg hl']
          omega
        · simp at h
  | .var β :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sFieldCount]
        rw [removeField_sFieldCount l s hrem]
      · simp at h

-- A var-free spine stays var-free under θ (θ introduces vars only via row-vars).
-- ⊢  vars(s) = []   ⟹   vars(spine (θ(ofSpine s))) = []
theorem varSeq_applySubst_nil {B : Type} (θ : TySubst B) :
    (s : List (Atom B)) → sVarSeq s = [] →
    sVarSeq ((ofSpine s).applySubst θ).toSpine = []
  | [], _ => rfl
  | .field l τ :: s, hs => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sVarSeq_append,
        sVarSeq, List.nil_append]
      exact varSeq_applySubst_nil θ s (by simpa [sVarSeq] using hs)
  | .var β :: s, hs => by simp [sVarSeq] at hs

-- If the θ-image of ofSpine s carries NO spine variable, every variable of s is
-- var-free under θ.
-- ⊢  vars(spine (θ(ofSpine s))) = []   ⟹   ∀ β ∈ vars(s). θβ var-free
theorem allVars_varFree_of {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) →
    sVarSeq ((ofSpine s).applySubst θ).toSpine = [] →
    ∀ β ∈ sVarSeq s, (θ.row β).SpineVarFree
  | [], _, β, hβ => by simp [sVarSeq] at hβ
  | .field l τ :: s, h, β, hβ => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sVarSeq_append,
        sVarSeq, List.nil_append] at h
      exact allVars_varFree_of s h β (by simpa [sVarSeq] using hβ)
  | .var γ :: s, h, β, hβ => by
      simp only [ofSpine_var_cons, Row.applySubst, Row.toSpine, sVarSeq_append] at h
      obtain ⟨hγ, hs⟩ := List.append_eq_nil_iff.mp h
      simp only [sVarSeq] at hβ
      rcases List.mem_cons.mp hβ with rfl | hβ'
      · exact (spineVarFree_iff_varSeq_nil _).mpr hγ
      · exact allVars_varFree_of s hs β hβ'

-- If θ does not INCREASE the l-count of ofSpine s (it never decreases it), then
-- every variable of s is l-free under θ — the U-ground counting fact.
-- ⊢  count_l(spine (θ(ofSpine s))) = count_l(s)
--        ⟹   ∀ β ∈ vars(s). count_l(spine θβ) = 0
theorem allVars_lfree_of {B : Type} {θ : TySubst B} (l : Label) :
    (s : List (Atom B)) →
    sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s →
    ∀ β ∈ sVarSeq s, sFieldCount l (θ.row β).toSpine = 0
  | [], _, β, hβ => by simp [sVarSeq] at hβ
  | .field l' τ :: s, h, β, hβ => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sFieldCount_append,
        sFieldCount] at h
      have h' : sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s := by omega
      exact allVars_lfree_of l s h' β (by simpa [sVarSeq] using hβ)
  | .var γ :: s, h, β, hβ => by
      simp only [ofSpine_var_cons, Row.applySubst, Row.toSpine, sFieldCount_append,
        sFieldCount] at h
      have hmono : sFieldCount l s ≤ sFieldCount l ((ofSpine s).applySubst θ).toSpine := by
        have hle := sFieldCount_applySubst_le θ l (ofSpine s); rwa [ofSpine_toSpine] at hle
      have hs : sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s := by omega
      simp only [sVarSeq] at hβ
      rcases List.mem_cons.mp hβ with rfl | hβ'
      · omega
      · exact allVars_lfree_of l s hs β hβ'

-- ## U-ground: inversion + the reflection lemma
-- ⊢  groundMatchAux s₁ s₂ ls = some (τ,τ',t₁,t₂)   ⟹   ∃ l.
--       count_l(s₁) = count_l(s₂) ∧ 0 < count_l(s₁)
--       ∧ removeField l s₁ = some (τ,t₁) ∧ removeField l s₂ = some (τ',t₂)
theorem groundMatchAux_inv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} :
    (ls : List Label) → groundMatchAux s₁ s₂ ls = some (τ, τ', t₁, t₂) →
    ∃ l, sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ ∧
         removeField l s₁ = some (τ, t₁) ∧ removeField l s₂ = some (τ', t₂)
  | [], h => by simp [groundMatchAux] at h
  | l :: ls, h => by
      simp only [groundMatchAux] at h
      split at h
      · rename_i hcond
        cases hr₁ : removeField l s₁ with
        | none => simp only [hr₁] at h; exact groundMatchAux_inv ls h
        | some p₁ =>
          cases hr₂ : removeField l s₂ with
          | none => simp only [hr₁, hr₂] at h; exact groundMatchAux_inv ls h
          | some p₂ =>
            obtain ⟨τa, ta⟩ := p₁
            obtain ⟨τb, tb⟩ := p₂
            simp only [hr₁, hr₂, Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl, rfl, rfl⟩ := h
            exact ⟨l, hcond.1, hcond.2, hr₁, hr₂⟩
      · exact groundMatchAux_inv ls h

-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   vars(s₂) = [] ∧ ∃ l.
--       count_l(s₁) = count_l(s₂) ∧ 0 < count_l(s₁)
--       ∧ removeField l s₁ = some (τ,t₁) ∧ removeField l s₂ = some (τ',t₂)
theorem groundMatch_inv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} :
    groundMatch s₁ s₂ = some (τ, τ', t₁, t₂) →
    sVarSeq s₂ = [] ∧ ∃ l, sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ ∧
      removeField l s₁ = some (τ, t₁) ∧ removeField l s₂ = some (τ', t₂) := by
  intro h
  unfold groundMatch at h
  split at h
  · simp at h
  · rename_i hnv
    exact ⟨(sHasVar_false_iff s₂).mp (by simpa using hnv), groundMatchAux_inv (sLabels s₁) h⟩

-- U-ground reflection: the paired l-field bubbles out of BOTH sides. The ground
-- side (s₂ var-free) is a direct removeField_equiv_of. The other side needs its
-- variables to be l-free under θ — which the counting forces: hrec pins the
-- l-count of (ofSpine t₁)θ to that of the var-free (ofSpine t₂)θ, and
-- sFieldCount l s₁ = sFieldCount l s₂ then makes θ introduce zero l-fields
-- across s₁'s variables (allVars_lfree_of); likewise they stay var-free
-- (allVars_varFree_of). This is the one move whose soundness is genuinely
-- non-local — it reads the residual solution back through the counting.
-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem groundMatch_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨hs₂vars, l, hcount, _, hr₁, hr₂⟩ := groundMatch_inv hg
  have hs₂equiv : RowEquiv ((ofSpine s₂).applySubst θ)
      (.cat (.sing l (τ'.applySubst θ)) ((ofSpine t₂).applySubst θ)) :=
    removeField_equiv_of l s₂ hr₂ (fun β hβ => by simp [hs₂vars] at hβ)
  have ht₂vars : sVarSeq t₂ = [] := by rw [removeField_sVarSeq l s₂ hr₂]; exact hs₂vars
  have ht₂vf : (ofSpine t₂).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact ht₂vars)
  have hv₁nil : sVarSeq ((ofSpine t₁).applySubst θ).toSpine = [] := by
    rw [hrec.char.1]; exact varSeq_applySubst_nil θ t₂ ht₂vars
  have hfc : sFieldCount l ((ofSpine t₁).applySubst θ).toSpine = sFieldCount l t₁ := by
    have e1 := rowEquiv_fieldCount_eq l hrec
    have e2 : sFieldCount l ((ofSpine t₂).applySubst θ).toSpine = sFieldCount l t₂ := by
      rw [sFieldCount_applySubst_varFree θ l ht₂vf, ofSpine_toSpine]
    have c1 := removeField_sFieldCount l s₁ hr₁
    have c2 := removeField_sFieldCount l s₂ hr₂
    omega
  have hvarfree := allVars_varFree_of t₁ hv₁nil
  have hlfree := allVars_lfree_of l t₁ hfc
  have hvars₁ : sVarSeq t₁ = sVarSeq s₁ := removeField_sVarSeq l s₁ hr₁
  have hs₁equiv : RowEquiv ((ofSpine s₁).applySubst θ)
      (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) :=
    removeField_equiv_of l s₁ hr₁
      (fun β hβ => ⟨hvarfree β (by rw [hvars₁]; exact hβ),
                    hlfree β (by rw [hvars₁]; exact hβ)⟩)
  exact hs₁equiv.trans ((RowEquiv.cat (.sing heq) hrec).trans hs₂equiv.symm)

-- U-ground FORWARD: a unifier of the original ground-match pins the field types
-- and unifies the residual. The var-free + l-free side conditions on s₁ (which
-- the backward lemma reads off the residual) are here DERIVED from hu itself:
-- the ground side s₂ fixes count_l and the var sequence under θ, hu transports
-- both to s₁, and hcount forces θ to add no l-fields across s₁'s vars.
-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem groundMatch_reflect_fwd {B : Type} {θ : TySubst B}
    {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨hs₂vars, l, hcount, _, hr₁, hr₂⟩ := groundMatch_inv hg
  have hs₂equiv : RowEquiv ((ofSpine s₂).applySubst θ)
      (.cat (.sing l (τ'.applySubst θ)) ((ofSpine t₂).applySubst θ)) :=
    removeField_equiv_of l s₂ hr₂ (fun β hβ => by simp [hs₂vars] at hβ)
  have hs₂vf : (ofSpine s₂).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact hs₂vars)
  have hv₁nil : sVarSeq ((ofSpine s₁).applySubst θ).toSpine = [] := by
    rw [hu.char.1]; exact varSeq_applySubst_nil θ s₂ hs₂vars
  have hfc : sFieldCount l ((ofSpine s₁).applySubst θ).toSpine = sFieldCount l s₁ := by
    have e1 := rowEquiv_fieldCount_eq l hu
    have e2 : sFieldCount l ((ofSpine s₂).applySubst θ).toSpine = sFieldCount l s₂ := by
      rw [sFieldCount_applySubst_varFree θ l hs₂vf, ofSpine_toSpine]
    omega
  have hvarfree := allVars_varFree_of s₁ hv₁nil
  have hlfree := allVars_lfree_of l s₁ hfc
  have hs₁equiv : RowEquiv ((ofSpine s₁).applySubst θ)
      (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) :=
    removeField_equiv_of l s₁ hr₁ (fun β hβ => ⟨hvarfree β hβ, hlfree β hβ⟩)
  exact (hs₁equiv.symm.trans (hu.trans hs₂equiv)).field_cancel_left

-- ## Eq-emitting moves reflect no-mgu (the augmented-witness case of the lift)
-- Each of matchL/matchR/groundMatch replaces the row problem by "residual row
-- problem PLUS the emitted type equation τ ≐ τ'". reflect_fwd (forward) and
-- reflect (backward) together say a θ unifies the original IFF it unifies the
-- residual AND satisfies τ ≐ τ'. That is a pointwise predicate equivalence, so
-- `hasMguP_congr` transports mgu-status across the move. Thus a stuck residual
-- (whose eq-constrained problem has no mgu) forces the original to have no mgu —
-- the eq-emitting analogue of stripL/stripR_hasMgu_iff.
-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--        HasMguP (λθ. θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂))
theorem matchL_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔
    HasMguP (fun θ => TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
                      Unifies θ (ofSpine t₁) (ofSpine t₂)) :=
  hasMguP_congr (fun _ =>
    ⟨fun hu => matchL_reflect_fwd hmatch hu,
     fun ⟨heq, hrec⟩ => matchL_reflect hmatch heq hrec⟩)

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--        HasMguP (λθ. θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂))
theorem matchR_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔
    HasMguP (fun θ => TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
                      Unifies θ (ofSpine t₁) (ofSpine t₂)) :=
  hasMguP_congr (fun _ =>
    ⟨fun hu => matchR_reflect_fwd hmatch hu,
     fun ⟨heq, hrec⟩ => matchR_reflect hmatch heq hrec⟩)

-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--        HasMguP (λθ. θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂))
theorem groundMatch_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔
    HasMguP (fun θ => TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
                      Unifies θ (ofSpine t₁) (ofSpine t₂)) :=
  hasMguP_congr (fun _ =>
    ⟨fun hu => groundMatch_reflect_fwd hg hu,
     fun ⟨heq, hrec⟩ => groundMatch_reflect hg heq hrec⟩)

-- End-to-end demo that the eq-emitting arm composes (the match analogue of
-- wand_under_strip_no_mgu): prepend a shared ground field l:𝓪 to both sides of
-- the Wand core (β|α)≐ᵣ(l:𝓫). matchL fires on the leading l-fields, emitting the
-- (trivially satisfiable) eq 𝓪≐𝓪, and the residual is exactly Wand. matchL_hasMgu_iff
-- transports mgu-status to HasMguP of that eq-constrained residual; the eq being
-- 𝓪≐𝓪 the constraint is vacuous, so it collapses to HasMgu Wand — with no mgu.
-- (Here the accumulated eq is trivial; the genuine augmented-witness case is when
-- it constrains a shared field variable — same skeleton, witnesses extended to it.)
-- ⊢  ¬ HasMgu (l:𝓪 | β | α) (l:𝓪 | l:𝓫)
theorem wand_under_match_no_mgu {B : Type} (a b : B) :
    ¬ HasMgu (ofSpine [Atom.field "l" (.base a), .var "β", .var "α"])
             (ofSpine [Atom.field "l" (.base a), .field "l" (.base b)]) := by
  intro hmgu
  have hmatch : matchL ([Atom.field "l" (.base a), .var "β", .var "α"] : List (Atom B))
                       [Atom.field "l" (.base a), .field "l" (.base b)]
      = some (.base a, .base a, [.var "β", .var "α"], [.field "l" (.base b)]) := rfl
  rw [matchL_hasMgu_iff hmatch] at hmgu
  -- drop the vacuous eq conjunct: HasMguP (𝓪≐𝓪 ∧ ·) ⟹ HasMgu of the residual
  have hw : HasMgu (ofSpine [Atom.var "β", .var "α"]) (ofSpine [Atom.field "l" (.base b)]) := by
    obtain ⟨θ, ⟨_, hu⟩, hmax⟩ := hmgu
    exact ⟨θ, hu, fun θ' hu' => hmax θ' ⟨TyEquiv.refl _, hu'⟩⟩
  refine absurd hw ?_
  simp only [ofSpine]
  rw [hasMgu_rowEquiv (ρ₁' := .cat (.var "β") (.var "α")) (ρ₂' := .sing "l" (.base b))
        (RowEquiv.cat (RowEquiv.refl _) RowEquiv.unitR) RowEquiv.unitR]
  exact wand_no_mgu_count b "l"

-- ## Assembly: success ⟹ unifies (induction on unifySpineF's fuel)
-- Base cases: one side empty ⟹ allVarsEmpty forces the other's vars to ε.
-- ⊢  unifySpineF fuel [] s₂ = success σ eqs,  θ ⊨ σ
--        ⟹   θ(ofSpine []) ≈ᵣ θ(ofSpine s₂)
theorem unifySpineF_nil_left {B : Type} {θ : TySubst B} (fuel : Nat) (s₂ : List (Atom B))
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel [] s₂ = .success σ eqs) (hσ : SolSat θ σ) :
    RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst]
      exact (allVarsEmpty_sound s₂ hae hσ).symm

-- ⊢  unifySpineF fuel (a::s₁) [] = success σ eqs,  θ ⊨ σ
--        ⟹   θ(ofSpine (a::s₁)) ≈ᵣ θ(ofSpine [])
theorem unifySpineF_cons_nil {B : Type} {θ : TySubst B} (fuel : Nat)
    (a : Atom B) (s₁ : List (Atom B))
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel (a :: s₁) [] = .success σ eqs) (hσ : SolSat θ σ) :
    RowEquiv ((ofSpine (a :: s₁)).applySubst θ) ((ofSpine ([] : List (Atom B))).applySubst θ) := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst]
      exact allVarsEmpty_sound (a :: s₁) hae hσ

-- ⊢  unifySpineF fuel s₁ s₂ = success σ eqs,  θ ⊨ σ,  θ ⊨ eqs
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem unifySpineF_success_sound {B : Type} {θ : TySubst B} (fuel : Nat) :
    ∀ (s₁ s₂ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)},
      unifySpineF fuel s₁ s₂ = .success σ eqs → SolSat θ σ → EqsSat θ eqs →
      RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  induction fuel with
  | zero =>
      intro s₁ s₂ σ eqs h hσ _
      cases s₁ with
      | nil => exact unifySpineF_nil_left 0 s₂ h hσ
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil 0 a s₁ h hσ
        | cons b s₂ => simp [unifySpineF] at h
  | succ fuel ih =>
      intro s₁ s₂ σ eqs h hσ heqs
      cases s₁ with
      | nil => exact unifySpineF_nil_left (fuel + 1) s₂ h hσ
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil (fuel + 1) a s₁ h hσ
        | cons b s₂ =>
          unfold unifySpineF at h
          cases hsl : stripL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
            exact stripL_reflect hsl (ih t₁ t₂ h hσ heqs)
          | none =>
          cases hsr : stripR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
            exact stripR_reflect hsr (ih t₁ t₂ h hσ heqs)
          | none =>
          cases hv1 : solveVar (a :: s₁) (b :: s₂) with
          | some r =>
            simp only [hsl, hsr, hv1] at h
            exact solveVar_reflect (hv1.trans (congrArg some h)) hσ
          | none =>
          cases hv2 : solveVar (b :: s₂) (a :: s₁) with
          | some r =>
            simp only [hsl, hsr, hv1, hv2] at h
            exact (solveVar_reflect (hv2.trans (congrArg some h)) hσ).symm
          | none =>
          cases hml : matchL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact matchL_reflect hml (heqs (τ0, τ0') (by simp))
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp])))
          | none =>
          cases hml2 : matchL (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact (matchL_reflect hml2 (heqs (τ0, τ0') (by simp)).symm
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp]))).symm).symm
          | none =>
          cases hmr : matchR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact matchR_reflect hmr (heqs (τ0, τ0') (by simp))
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp])))
          | none =>
          cases hmr2 : matchR (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact (matchR_reflect hmr2 (heqs (τ0, τ0') (by simp)).symm
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp]))).symm).symm
          | none =>
          cases hg : groundMatch (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact groundMatch_reflect hg (heqs (τ0, τ0') (by simp))
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp])))
          | none =>
          cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact (groundMatch_reflect hg2 (heqs (τ0, τ0') (by simp)).symm
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp]))).symm).symm
          | none =>
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            split at h <;> simp at h

-- The ≐ᵣ success case is SOUND: any θ that meets the emitted row-var solution σ
-- and residual type equations eqs unifies the two rows.
-- ⊢  unifyRow ρ₁ ρ₂ = success σ eqs,  θ ⊨ σ,  θ ⊨ eqs   ⟹   θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRow_success_sound {B : Type} {θ : TySubst B} {ρ₁ ρ₂ : Row B}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifyRow ρ₁ ρ₂ = .success σ eqs) (hσ : SolSat θ σ) (heqs : EqsSat θ eqs) :
    Unifies θ ρ₁ ρ₂ := by
  unfold unifyRow unifySpine at h
  have key := unifySpineF_success_sound _ ρ₁.toSpine ρ₂.toSpine h hσ heqs
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.trans (key.trans e₂.symm)

------------------------- ≐ᵣ CLASH SOUNDNESS (algorithm level) --------------
-- Lifting the two local clash cores through the whole control flow, using the
-- FORWARD reflection: at every move a unifier of the original also unifies the
-- residual, so no-unifier propagates backwards. solveVar never yields clash, and
-- addEq preserves clash, so a clash comes only from a base case (allVarsEmpty) or
-- the final projClash — both already refuted locally. This is the ≐ᵣ CLASH leg of
-- the trichotomy, now at the algorithm level (not just the local conditions).

-- solveVar answers success or occurs, never clash.
theorem solveVar_ne_clash {B : Type} {s₁ s₂ : List (Atom B)} :
    solveVar s₁ s₂ ≠ some .clash := by
  intro h
  cases s₁ with
  | nil => simp [solveVar] at h
  | cons a r =>
    cases a with
    | field _ _ => simp [solveVar] at h
    | var α =>
      cases r with
      | cons _ _ => simp [solveVar] at h
      | nil => simp only [solveVar] at h; split at h <;> simp at h

-- addEq only rewrites a success; a clash result must come from a clash residual.
theorem addEq_clash_inv {B : Type} {τ τ' : Ty B} {u : URes B} :
    u.addEq τ τ' = .clash → u = .clash := by
  cases u <;> simp [URes.addEq]

-- solveVar answers success or occurs, never stuck (the stuck-leg analogue of
-- solveVar_ne_clash).
theorem solveVar_ne_stuck {B : Type} {s₁ s₂ : List (Atom B)} :
    solveVar s₁ s₂ ≠ some .stuck := by
  intro h
  cases s₁ with
  | nil => simp [solveVar] at h
  | cons a r =>
    cases a with
    | field _ _ => simp [solveVar] at h
    | var α =>
      cases r with
      | cons _ _ => simp [solveVar] at h
      | nil => simp only [solveVar] at h; split at h <;> simp at h

-- addEq only rewrites a success; a stuck result must come from a stuck residual.
theorem addEq_stuck_inv {B : Type} {τ τ' : Ty B} {u : URes B} :
    u.addEq τ τ' = .stuck → u = .stuck := by
  cases u <;> simp [URes.addEq]

-- ⊢  unifySpineF fuel s₁ s₂ = clash   ⟹   ¬ ∃ θ. θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
theorem unifySpineF_clash_no_unifier {B : Type} :
    ∀ (fuel : Nat) (s₁ s₂ : List (Atom B)),
      unifySpineF fuel s₁ s₂ = .clash →
      ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s₁) (ofSpine s₂) := by
  intro fuel
  induction fuel with
  | zero =>
      intro s₁ s₂ h
      cases s₁ with
      | nil =>
          simp only [unifySpineF] at h
          cases hae : allVarsEmpty s₂ with
          | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
          | some => simp [hae] at h
      | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
              | some => simp [hae] at h
          | cons b s₂ => simp [unifySpineF] at h
  | succ fuel ih =>
      intro s₁ s₂ h
      cases s₁ with
      | nil =>
          simp only [unifySpineF] at h
          cases hae : allVarsEmpty s₂ with
          | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
          | some => simp [hae] at h
      | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
              | some => simp [hae] at h
          | cons b s₂ =>
              rintro ⟨θ, hu⟩
              unfold unifySpineF at h
              cases hsl : stripL (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
                exact ih t₁ t₂ h ⟨θ, stripL_reflect_fwd hsl hu⟩
              | none =>
              cases hsr : stripR (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
                exact ih t₁ t₂ h ⟨θ, stripR_reflect_fwd hsr hu⟩
              | none =>
              cases hv1 : solveVar (a :: s₁) (b :: s₂) with
              | some r =>
                simp only [hsl, hsr, hv1] at h; exact solveVar_ne_clash (hv1.trans (congrArg some h))
              | none =>
              cases hv2 : solveVar (b :: s₂) (a :: s₁) with
              | some r =>
                simp only [hsl, hsr, hv1, hv2] at h
                exact solveVar_ne_clash (hv2.trans (congrArg some h))
              | none =>
              cases hml : matchL (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchL_reflect_fwd hml hu).2⟩
              | none =>
              cases hml2 : matchL (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchL_reflect_fwd hml2 hu.symm).2.symm⟩
              | none =>
              cases hmr : matchR (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchR_reflect_fwd hmr hu).2⟩
              | none =>
              cases hmr2 : matchR (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchR_reflect_fwd hmr2 hu.symm).2.symm⟩
              | none =>
              cases hg : groundMatch (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (groundMatch_reflect_fwd hg hu).2⟩
              | none =>
              cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (groundMatch_reflect_fwd hg2 hu.symm).2.symm⟩
              | none =>
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
                split at h
                · rename_i hpc; exact projClash_no_unifier hpc ⟨θ, hu⟩
                · simp at h

-- ≐ᵣ CLASH is SOUND: a clash verdict means the two rows have no unifier.
-- ⊢  unifyRow ρ₁ ρ₂ = clash   ⟹   ¬ ∃ θ. θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRow_clash_no_unifier {B : Type} {ρ₁ ρ₂ : Row B}
    (h : unifyRow ρ₁ ρ₂ = .clash) : ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ := by
  rintro ⟨θ, hu⟩
  unfold unifyRow unifySpine at h
  refine unifySpineF_clash_no_unifier _ ρ₁.toSpine ρ₂.toSpine h ⟨θ, ?_⟩
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.symm.trans (hu.trans e₂)

------------------------- ≐ᵣ SUCCESS COMPLETENESS (mgu) ---------------------
-- The emitted σ (row-var bindings) and eqs (deferred type equations) are
-- NECESSARY: EVERY unifier satisfies them. With unifyRow_success_sound (they are
-- SUFFICIENT) this makes unifyRow's output characterize the unifier set exactly —
-- i.e. ≐ᵣ computes a most general unifier, presented as row bindings + residual
-- type equations. The FORWARD reflection layer is the engine: it pushes any
-- unifier through each move to the residual and reads off the emitted type eq.

-- solveVar's success binds α ≔ ofSpine s₂; a unifier of the two rows is exactly
-- a θ meeting that binding (θα ≈ (ofSpine s₂)θ).
theorem solveVar_complete {B : Type} {θ : TySubst B} {s₁ s₂ : List (Atom B)}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (hsolve : solveVar s₁ s₂ = some (.success σ eqs))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  cases s₁ with
  | nil => simp [solveVar] at hsolve
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVar] at hsolve
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVar] at hsolve
      | nil =>
        simp only [solveVar] at hsolve
        split at hsolve
        · simp at hsolve
        · simp only [Option.some.injEq, URes.success.injEq] at hsolve
          obtain ⟨rfl, rfl⟩ := hsolve
          refine ⟨fun p hp => ?_, fun p hp => by simp at hp⟩
          simp only [List.mem_singleton] at hp
          subst hp
          simp only [ofSpine, Row.applySubst] at hu
          exact RowEquiv.unitR.symm.trans hu

theorem unifySpineF_nil_left_complete {B : Type} {θ : TySubst B} (fuel : Nat)
    (s₂ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel [] s₂ = .success σ eqs)
    (hu : RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact ⟨allVarsEmpty_complete s₂ hae hu.symm, fun p hp => by simp at hp⟩

theorem unifySpineF_cons_nil_complete {B : Type} {θ : TySubst B} (fuel : Nat)
    (a : Atom B) (s₁ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel (a :: s₁) [] = .success σ eqs)
    (hu : RowEquiv ((ofSpine (a :: s₁)).applySubst θ)
                   ((ofSpine ([] : List (Atom B))).applySubst θ)) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact ⟨allVarsEmpty_complete (a :: s₁) hae hu, fun p hp => by simp at hp⟩

-- ⊢  unifySpineF fuel s₁ s₂ = success σ eqs,  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   SolSat θ σ  ∧  EqsSat θ eqs
theorem unifySpineF_success_complete {B : Type} {θ : TySubst B} (fuel : Nat) :
    ∀ (s₁ s₂ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)},
      unifySpineF fuel s₁ s₂ = .success σ eqs →
      RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) →
      SolSat θ σ ∧ EqsSat θ eqs := by
  induction fuel with
  | zero =>
      intro s₁ s₂ σ eqs h hu
      cases s₁ with
      | nil => exact unifySpineF_nil_left_complete 0 s₂ h hu
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil_complete 0 a s₁ h hu
        | cons b s₂ => simp [unifySpineF] at h
  | succ fuel ih =>
      intro s₁ s₂ σ eqs h hu
      cases s₁ with
      | nil => exact unifySpineF_nil_left_complete (fuel + 1) s₂ h hu
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil_complete (fuel + 1) a s₁ h hu
        | cons b s₂ =>
          unfold unifySpineF at h
          cases hsl : stripL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
            exact ih t₁ t₂ h (stripL_reflect_fwd hsl hu)
          | none =>
          cases hsr : stripR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
            exact ih t₁ t₂ h (stripR_reflect_fwd hsr hu)
          | none =>
          cases hv1 : solveVar (a :: s₁) (b :: s₂) with
          | some r =>
            simp only [hsl, hsr, hv1] at h
            exact solveVar_complete (hv1.trans (congrArg some h)) hu
          | none =>
          cases hv2 : solveVar (b :: s₂) (a :: s₁) with
          | some r =>
            simp only [hsl, hsr, hv1, hv2] at h
            exact solveVar_complete (hv2.trans (congrArg some h)) hu.symm
          | none =>
          cases hml : matchL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml hu
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru
            exact ⟨hsol, EqsSat.cons hty heqs⟩
          | none =>
          cases hml2 : matchL (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml2 hu.symm
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru.symm
            exact ⟨hsol, EqsSat.cons hty.symm heqs⟩
          | none =>
          cases hmr : matchR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr hu
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru
            exact ⟨hsol, EqsSat.cons hty heqs⟩
          | none =>
          cases hmr2 : matchR (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr2 hu.symm
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru.symm
            exact ⟨hsol, EqsSat.cons hty.symm heqs⟩
          | none =>
          cases hg : groundMatch (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg hu
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru
            exact ⟨hsol, EqsSat.cons hty heqs⟩
          | none =>
          cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg2 hu.symm
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru.symm
            exact ⟨hsol, EqsSat.cons hty.symm heqs⟩
          | none =>
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            split at h <;> simp at h

-- ≐ᵣ SUCCESS COMPLETENESS: any unifier of ρ₁,ρ₂ satisfies the emitted σ and eqs.
-- Together with unifyRow_success_sound: {unifiers of ρ₁ ≐ᵣ ρ₂} = {θ : SolSat θ σ ∧
-- EqsSat θ eqs} — the algorithm's output is a most general unifier.
-- ⊢  unifyRow ρ₁ ρ₂ = success σ eqs,  θ ⊨ ρ₁ ≐ᵣ ρ₂   ⟹   SolSat θ σ ∧ EqsSat θ eqs
theorem unifyRow_success_complete {B : Type} {θ : TySubst B} {ρ₁ ρ₂ : Row B}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifyRow ρ₁ ρ₂ = .success σ eqs) (hu : Unifies θ ρ₁ ρ₂) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  unfold unifyRow unifySpine at h
  unfold Unifies at hu
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact unifySpineF_success_complete _ ρ₁.toSpine ρ₂.toSpine h (e₁.symm.trans (hu.trans e₂))

------------------------- ≐ᵣ FUEL SUFFICIENCY -------------------------------
-- Every recursive move of unifySpineF removes exactly ONE atom from each side
-- (a shared end-var for strip; a matched field + its window/counterpart for
-- match/ground), so the total spine length drops by 2 per step. Hence the
-- starting fuel |s₁| + |s₂| never runs out: unifySpineF is INVARIANT to fuel
-- above that threshold, and the fuel-0 `.stuck` branch is unreachable for
-- unifySpine. A `.stuck` result is therefore a genuine ambiguity (Wand class),
-- not an out-of-fuel artifact — the precondition that makes the trichotomy's
-- stuck/occurs legs well-posed.

-- Each move's length bookkeeping: |t| + 1 = |s| for the field extractors, and
-- |t₁| + |t₂| + 2 = |s₁| + |s₂| for every two-sided move.
theorem windowExtract_len {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') → s'.length + 1 = s.length
  | [], _, _, h => by simp [windowExtract] at h
  | .var _ :: _, _, _, h => by simp [windowExtract] at h
  | .field l' _ :: s, _, _, h => by
      simp only [windowExtract] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := windowExtract_len l s hwe
          simp only [List.length_cons]; omega
        · simp at h

theorem removeField_len {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    removeField l s = some (τ, s') → s'.length + 1 = s.length
  | [], _, _, h => by simp [removeField] at h
  | .var _ :: s, _, _, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have ih := removeField_len l s hwe
        simp only [List.length_cons]; omega
      · simp at h
  | .field l' _ :: s, _, _, h => by
      simp only [removeField] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := removeField_len l s hwe
          simp only [List.length_cons]; omega
        · simp at h

theorem stripL_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripL s₁ s₂ = some (t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv h; simp only [List.length_cons]; omega

theorem stripR_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripR s₁ s₂ = some (t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv h
  simp only [List.length_append, List.length_cons, List.length_nil]; omega

theorem matchL_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (h : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨l, rfl, hwe⟩ := matchL_inv h
  have := windowExtract_len l s₂ hwe
  simp only [List.length_cons]; omega

theorem matchR_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (h : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  unfold matchR at h
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at h; simp at h
  | some p =>
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    rw [hml] at h
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl, rfl⟩ := h
    have := matchL_len hml
    simp only [List.length_reverse] at this ⊢
    omega

theorem groundMatch_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (h : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨_, l, _, _, hr₁, hr₂⟩ := groundMatch_inv h
  have := removeField_len l s₁ hr₁
  have := removeField_len l s₂ hr₂
  omega

-- Fuel invariance: any two fuels ≥ |s₁|+|s₂| give the same result. Induction on
-- a length bound N; each recursive arm drops the bound by 2 (the *_len lemmas)
-- and applies the IH. The control-flow cascade mirrors unifySpineF_success_sound.
theorem unifySpineF_fuel_irrel {B : Type} (N : Nat) :
    ∀ (s₁ s₂ : List (Atom B)) (fuel fuel' : Nat),
      s₁.length + s₂.length ≤ N →
      s₁.length + s₂.length ≤ fuel → s₁.length + s₂.length ≤ fuel' →
      unifySpineF fuel s₁ s₂ = unifySpineF fuel' s₁ s₂ := by
  induction N with
  | zero =>
      intro s₁ s₂ fuel fuel' hN _ _
      cases s₁ with
      | nil => simp only [unifySpineF]
      | cons a s₁ =>
        cases s₂ with
        | nil => simp only [unifySpineF]
        | cons b s₂ => simp only [List.length_cons] at hN; omega
  | succ N IH =>
      intro s₁ s₂ fuel fuel' hN hf hf'
      cases s₁ with
      | nil => simp only [unifySpineF]
      | cons a s₁ =>
        cases s₂ with
        | nil => simp only [unifySpineF]
        | cons b s₂ =>
          have hpos : 2 ≤ (a :: s₁).length + (b :: s₂).length := by
            simp only [List.length_cons]; omega
          obtain ⟨f, rfl⟩ := Nat.exists_eq_succ_of_ne_zero (show fuel ≠ 0 by omega)
          obtain ⟨f', rfl⟩ := Nat.exists_eq_succ_of_ne_zero (show fuel' ≠ 0 by omega)
          simp only [unifySpineF]
          cases hsl : stripL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p
            have hlen := stripL_len hsl
            exact IH t₁ t₂ f f' (by omega) (by omega) (by omega)
          | none =>
          cases hsr : stripR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p
            have hlen := stripR_len hsr
            exact IH t₁ t₂ f f' (by omega) (by omega) (by omega)
          | none =>
          cases hv1 : solveVar (a :: s₁) (b :: s₂) with
          | some r => rfl
          | none =>
          cases hv2 : solveVar (b :: s₂) (a :: s₁) with
          | some r => rfl
          | none =>
          cases hml : matchL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; dsimp only
            have hlen := matchL_len hml
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hml2 : matchL (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; dsimp only
            have hlen := matchL_len hml2
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hmr : matchR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; dsimp only
            have hlen := matchR_len hmr
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hmr2 : matchR (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; dsimp only
            have hlen := matchR_len hmr2
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hg : groundMatch (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; dsimp only
            have hlen := groundMatch_len hg
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; dsimp only
            have hlen := groundMatch_len hg2
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none => rfl

-- unifySpine's own fuel (|s₁|+|s₂|) is enough: any larger fuel agrees with it.
theorem unifySpineF_fuel_stable {B : Type} (s₁ s₂ : List (Atom B)) {fuel : Nat}
    (h : s₁.length + s₂.length ≤ fuel) :
    unifySpineF fuel s₁ s₂ = unifySpine s₁ s₂ :=
  unifySpineF_fuel_irrel (s₁.length + s₂.length) s₁ s₂ fuel _ (Nat.le_refl _) h (Nat.le_refl _)

------------------- ≐ᵣ TERMINAL STUCK-SHAPE STRUCTURE -----------------------
-- Structural facts about the terminal stuck config (every move dead, no
-- projClash). These pin down which shapes the base arm must handle: chiefly,
-- BOTH sides cannot be var-free — a genuinely stuck config always has a live
-- row-var somewhere, so it sits in the setting of the three base-witness
-- techniques (count-shrink / rigidity / non-commutativity). This is the
-- de-risking characterization the base arm dispatches on.

-- removeField finds an l-field whenever the l-count is positive.
theorem removeField_isSome_of_pos {B : Type} (l : Label) :
    (s : List (Atom B)) → 0 < sFieldCount l s → (removeField l s).isSome = true
  | [], h => by simp [sFieldCount] at h
  | .var β :: s, h => by
      simp only [sFieldCount] at h
      simp only [removeField]
      have ih := removeField_isSome_of_pos l s h
      cases hr : removeField l s with
      | none => rw [hr] at ih; simp at ih
      | some p => simp
  | .field l' τ :: s, h => by
      simp only [removeField]
      by_cases hl : l' = l
      · subst hl; simp
      · rw [if_neg hl]
        simp only [sFieldCount, if_neg hl, Nat.zero_add] at h
        have ih := removeField_isSome_of_pos l s h
        cases hr : removeField l s with
        | none => rw [hr] at ih; simp at ih
        | some p => simp

-- groundMatchAux none ⟹ no scanned label has equal positive counts on both sides
-- (equal positive counts would let removeField fire on both, yielding `some`).
theorem groundMatchAux_none_of_mem {B : Type} {s₁ s₂ : List (Atom B)} :
    (ls : List Label) → groundMatchAux s₁ s₂ ls = none →
    ∀ l ∈ ls, ¬ (sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁)
  | [], _, l, hmem, _ => by simp at hmem
  | c :: ls, hnone, l, hmem, hcond' => by
      simp only [groundMatchAux] at hnone
      by_cases hcond : sFieldCount c s₁ = sFieldCount c s₂ ∧ 0 < sFieldCount c s₁
      · rw [if_pos hcond] at hnone
        have h1 := removeField_isSome_of_pos c s₁ hcond.2
        have h2 := removeField_isSome_of_pos c s₂ (hcond.1 ▸ hcond.2)
        cases hr1 : removeField c s₁ with
        | none => rw [hr1] at h1; simp at h1
        | some p1 =>
          cases hr2 : removeField c s₂ with
          | none => rw [hr2] at h2; simp at h2
          | some p2 =>
            obtain ⟨t1, r1⟩ := p1; obtain ⟨t2, r2⟩ := p2
            rw [hr1, hr2] at hnone; simp at hnone
      · rw [if_neg hcond] at hnone
        rcases List.mem_cons.mp hmem with rfl | htail
        · exact hcond hcond'
        · exact groundMatchAux_none_of_mem ls hnone l htail hcond'

-- projClash false, both sides var-free ⟹ every scanned label has EQUAL counts.
theorem projClash_false_count_eq {B : Type} {s₁ s₂ : List (Atom B)}
    (hpc : projClash s₁ s₂ = false) (hv₁ : sHasVar s₁ = false) (hv₂ : sHasVar s₂ = false)
    (l : Label) (hmem : l ∈ sLabels s₁ ++ sLabels s₂) :
    sFieldCount l s₁ = sFieldCount l s₂ := by
  have hpc' : (sLabels s₁ ++ sLabels s₂).any (fun l =>
      (decide (sFieldCount l s₂ < sFieldCount l s₁) && !sHasVar s₂) ||
      (decide (sFieldCount l s₁ < sFieldCount l s₂) && !sHasVar s₁)) = false := hpc
  have hkey := List.any_eq_false.mp hpc' l hmem
  rw [hv₁, hv₂] at hkey
  simp only [Bool.not_false, Bool.and_true, Bool.or_eq_true, decide_eq_true_eq, not_or] at hkey
  omega

-- The de-risking fact: a terminal stuck config is NEVER ground on both sides.
-- (If it were, projClash-false pins all counts equal, and the leading field's
-- label then satisfies groundMatch's fire condition — contradicting groundMatch
-- = none.) So the base arm always has a live var to run a witness technique on.
-- ⊢  groundMatch (a::s₁) s₂ = none,  projClash (a::s₁) s₂ = false,
--      sHasVar (a::s₁) = false,  sHasVar s₂ = false   ⟹   False
theorem stuck_not_both_ground {B : Type} {a : Atom B} {s₁ s₂ : List (Atom B)}
    (hg : groundMatch (a :: s₁) s₂ = none)
    (hpc : projClash (a :: s₁) s₂ = false)
    (hv₁ : sHasVar (a :: s₁) = false) (hv₂ : sHasVar s₂ = false) : False := by
  cases a with
  | var α => simp [sHasVar] at hv₁
  | field l₀ τ =>
    have hpos : 0 < sFieldCount l₀ (Atom.field l₀ τ :: s₁) := by simp [sFieldCount]; omega
    have hmem₁ : l₀ ∈ sLabels (Atom.field l₀ τ :: s₁) := by simp [sLabels]
    have hcount : sFieldCount l₀ (Atom.field l₀ τ :: s₁) = sFieldCount l₀ s₂ :=
      projClash_false_count_eq hpc hv₁ hv₂ l₀ (List.mem_append_left _ hmem₁)
    have hgaux : groundMatchAux (Atom.field l₀ τ :: s₁) s₂ (sLabels (Atom.field l₀ τ :: s₁)) = none := by
      rw [groundMatch, hv₂] at hg; simpa using hg
    exact groundMatchAux_none_of_mem (sLabels (Atom.field l₀ τ :: s₁)) hgaux l₀ hmem₁ ⟨hcount, hpos⟩

-- Leading-atom characterization of a terminal stuck config. With stripL and
-- both matchL directions dead, the two leading atoms can only take one of four
-- shapes — none of which a forced move can act on. This is step 1 of the
-- base-arm DISPATCH the trichotomy still owes: each shape routes to a
-- base-witness technique (distinct leading vars → non-commutativity /
-- allvar_swap; a leading field facing a var, either way → count-shrink /
-- rigidity; two distinct leading fields each absent from the other's window →
-- count-shrink on the mismatched label). The last shape carries the two
-- windowExtract-failures the dispatch needs to locate the offending field.
-- ⊢  stripL/matchL(both) dead   ⟹   the leading atoms are one of the four shapes
theorem stuck_leading_shape {B : Type} {a b : Atom B} {s₁ s₂ : List (Atom B)}
    (hsl : stripL (a :: s₁) (b :: s₂) = none)
    (hml : matchL (a :: s₁) (b :: s₂) = none)
    (hml2 : matchL (b :: s₂) (a :: s₁) = none) :
    (∃ α β, a = .var α ∧ b = .var β ∧ α ≠ β) ∨
    (∃ α l' τ', a = .var α ∧ b = .field l' τ') ∨
    (∃ l τ β, a = .field l τ ∧ b = .var β) ∨
    (∃ l τ l' τ', a = .field l τ ∧ b = .field l' τ' ∧ l ≠ l' ∧
      windowExtract l (b :: s₂) = none ∧ windowExtract l' (a :: s₁) = none) := by
  cases a with
  | var α =>
    cases b with
    | var β =>
      -- stripL would fire on equal leading vars, so α ≠ β.
      refine Or.inl ⟨α, β, rfl, rfl, ?_⟩
      intro h; subst h; simp [stripL] at hsl
    | field l' τ' => exact Or.inr (Or.inl ⟨α, l', τ', rfl, rfl⟩)
  | field l τ =>
    cases b with
    | var β => exact Or.inr (Or.inr (Or.inl ⟨l, τ, β, rfl, rfl⟩))
    | field l' τ' =>
      refine Or.inr (Or.inr (Or.inr ⟨l, τ, l', τ', rfl, rfl, ?_, ?_, ?_⟩))
      -- matchL of a leading field fires exactly when the label is in the
      -- other side's window, so both windowExtracts must be none.
      · -- l ≠ l' : equal labels would let windowExtract fire at the head.
        intro h; subst h
        simp [matchL, windowExtract] at hml
      · simp only [matchL] at hml
        cases hwe : windowExtract l (Atom.field l' τ' :: s₂) with
        | none => rfl
        | some p => obtain ⟨t, r⟩ := p; rw [hwe] at hml; simp at hml
      · simp only [matchL] at hml2
        cases hwe : windowExtract l' (Atom.field l τ :: s₁) with
        | none => rfl
        | some p => obtain ⟨t, r⟩ := p; rw [hwe] at hml2; simp at hml2

--------------------------- ≐ᵣ STUCK ⟹ NO-MGU ------------------------------
-- The third leg of the trichotomy at the algorithm level — but stated HONESTLY
-- as a REDUCTION, because the naive form is FALSE (unify_eq_rescued_stuck):
-- `unifyRow = stuck` does NOT imply "no mgu", since an emitted type-equation can
-- constrain a stuck row-var and collapse the ambiguity that the single row pass
-- calls stuck. So the mgu-status of the whole problem is decided ENTIRELY at the
-- terminal stuck config TOGETHER WITH the accumulated type-equations Q.
--
-- unifySpineF_stuck_no_mgu is exactly that reduction: it threads no-mgu status
-- BACKWARD from the terminal config to the original, carrying Q (the accumulated
-- eqs) as an arbitrary unifier predicate. Structure mirrors
-- unifySpineF_clash_no_unifier, but where clash pushes an EXISTING unifier
-- forward to a local no-unifier fact, stuck pulls NO-MGU status backward through
-- each move via the *_hasMgu transport (hasMguP_congr on the per-θ
-- reflect/reflect_fwd iffs). The strip arms keep Q; the eq-emitting arms
-- (matchL/R, groundMatch) augment Q with the move's type equation.
--
-- The base hypothesis `hbase` (terminal config + Q has no mgu) is therefore NOT
-- universally true: unify_eq_rescued_stuck exhibits a terminal Wand config whose
-- Q = ⟨{β}≐{l:𝓫}⟩ makes it have a unique mgu. hbase HOLDS exactly when Q does not
-- re-constrain the stuck row-vars (e.g. Q between var-free field types) — that
-- side condition, plus the three base-witness techniques, is the remaining
-- (paper-level) content. The threading below is the reusable, ORDER-agnostic core.

-- No-mgu of a redescribed unifier predicate transfers along a pointwise iff.
private theorem hasMguP_not_of_iff {B : Type} {P P' : TySubst B → Prop}
    (hiff : ∀ θ, P θ ↔ P' θ) (h : ¬ HasMguP P') : ¬ HasMguP P :=
  fun hmgu => h ((hasMguP_congr hiff).mp hmgu)

theorem unifySpineF_stuck_no_mgu {B : Type}
    (hbase : ∀ (a : Atom B) (s₁ : List (Atom B)) (b : Atom B) (s₂ : List (Atom B))
              (Q : TySubst B → Prop),
      stripL (a :: s₁) (b :: s₂) = none → stripR (a :: s₁) (b :: s₂) = none →
      solveVar (a :: s₁) (b :: s₂) = none → solveVar (b :: s₂) (a :: s₁) = none →
      matchL (a :: s₁) (b :: s₂) = none → matchL (b :: s₂) (a :: s₁) = none →
      matchR (a :: s₁) (b :: s₂) = none → matchR (b :: s₂) (a :: s₁) = none →
      groundMatch (a :: s₁) (b :: s₂) = none → groundMatch (b :: s₂) (a :: s₁) = none →
      projClash (a :: s₁) (b :: s₂) = false →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine (a :: s₁)) (ofSpine (b :: s₂)) ∧ Q θ)) :
    ∀ (fuel : Nat) (s₁ s₂ : List (Atom B)) (Q : TySubst B → Prop),
      s₁.length + s₂.length ≤ fuel →
      unifySpineF fuel s₁ s₂ = .stuck →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine s₁) (ofSpine s₂) ∧ Q θ) := by
  intro fuel
  induction fuel with
  | zero =>
      intro s₁ s₂ Q hguard h
      cases s₁ with
      | nil =>
          simp only [unifySpineF] at h
          cases hae : allVarsEmpty s₂ with
          | none => rw [hae] at h; simp at h
          | some σ => rw [hae] at h; simp at h
      | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rw [hae] at h; simp at h
              | some σ => rw [hae] at h; simp at h
          | cons b s₂ => simp only [List.length_cons] at hguard; omega
  | succ fuel ih =>
      intro s₁ s₂ Q hguard h
      cases s₁ with
      | nil =>
          -- [] side: allVarsEmpty answers success or clash, never stuck.
          simp only [unifySpineF] at h
          cases hae : allVarsEmpty s₂ with
          | none => rw [hae] at h; simp at h
          | some σ => rw [hae] at h; simp at h
      | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rw [hae] at h; simp at h
              | some σ => rw [hae] at h; simp at h
          | cons b s₂ =>
              unfold unifySpineF at h
              cases hsl : stripL (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
                have hlen := stripL_len hsl
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ) ?_ (ih t₁ t₂ Q (by omega) h)
                exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨stripL_reflect_fwd hsl hu, hq⟩,
                                fun ⟨hu, hq⟩ => ⟨stripL_reflect hsl hu, hq⟩⟩
              | none =>
              cases hsr : stripR (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
                have hlen := stripR_len hsr
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧ Q θ) ?_ (ih t₁ t₂ Q (by omega) h)
                exact fun θ => ⟨fun ⟨hu, hq⟩ => ⟨stripR_reflect_fwd hsr hu, hq⟩,
                                fun ⟨hu, hq⟩ => ⟨stripR_reflect hsr hu, hq⟩⟩
              | none =>
              cases hv1 : solveVar (a :: s₁) (b :: s₂) with
              | some r => simp only [hsl, hsr, hv1] at h; exact absurd (h ▸ hv1) solveVar_ne_stuck
              | none =>
              cases hv2 : solveVar (b :: s₂) (a :: s₁) with
              | some r => simp only [hsl, hsr, hv1, hv2] at h; exact absurd (h ▸ hv2) solveVar_ne_stuck
              | none =>
              cases hml : matchL (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
                have hlen := matchL_len hml
                have h' := addEq_stuck_inv h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                      (TyEquiv (τ0.applySubst θ) (τ0'.applySubst θ) ∧ Q θ)) ?_
                    (ih t₁ t₂ _ (by omega) h')
                exact fun θ => ⟨fun ⟨hu, hq⟩ => let ⟨he, hr⟩ := matchL_reflect_fwd hml hu; ⟨hr, he, hq⟩,
                                fun ⟨hr, he, hq⟩ => ⟨matchL_reflect hml he hr, hq⟩⟩
              | none =>
              cases hml2 : matchL (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
                have hlen := matchL_len hml2
                have h' := addEq_stuck_inv h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                      (TyEquiv (τ0.applySubst θ) (τ0'.applySubst θ) ∧ Q θ)) ?_
                    (ih t₁ t₂ _ (by omega) h')
                exact fun θ => ⟨fun ⟨hu, hq⟩ => let ⟨he, hr⟩ := matchL_reflect_fwd hml2 hu.symm; ⟨hr.symm, he.symm, hq⟩,
                                fun ⟨hr, he, hq⟩ => ⟨(matchL_reflect hml2 he.symm hr.symm).symm, hq⟩⟩
              | none =>
              cases hmr : matchR (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
                have hlen := matchR_len hmr
                have h' := addEq_stuck_inv h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                      (TyEquiv (τ0.applySubst θ) (τ0'.applySubst θ) ∧ Q θ)) ?_
                    (ih t₁ t₂ _ (by omega) h')
                exact fun θ => ⟨fun ⟨hu, hq⟩ => let ⟨he, hr⟩ := matchR_reflect_fwd hmr hu; ⟨hr, he, hq⟩,
                                fun ⟨hr, he, hq⟩ => ⟨matchR_reflect hmr he hr, hq⟩⟩
              | none =>
              cases hmr2 : matchR (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
                have hlen := matchR_len hmr2
                have h' := addEq_stuck_inv h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                      (TyEquiv (τ0.applySubst θ) (τ0'.applySubst θ) ∧ Q θ)) ?_
                    (ih t₁ t₂ _ (by omega) h')
                exact fun θ => ⟨fun ⟨hu, hq⟩ => let ⟨he, hr⟩ := matchR_reflect_fwd hmr2 hu.symm; ⟨hr.symm, he.symm, hq⟩,
                                fun ⟨hr, he, hq⟩ => ⟨(matchR_reflect hmr2 he.symm hr.symm).symm, hq⟩⟩
              | none =>
              cases hg : groundMatch (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
                have hlen := groundMatch_len hg
                have h' := addEq_stuck_inv h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                      (TyEquiv (τ0.applySubst θ) (τ0'.applySubst θ) ∧ Q θ)) ?_
                    (ih t₁ t₂ _ (by omega) h')
                exact fun θ => ⟨fun ⟨hu, hq⟩ => let ⟨he, hr⟩ := groundMatch_reflect_fwd hg hu; ⟨hr, he, hq⟩,
                                fun ⟨hr, he, hq⟩ => ⟨groundMatch_reflect hg he hr, hq⟩⟩
              | none =>
              cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
                have hlen := groundMatch_len hg2
                have h' := addEq_stuck_inv h
                refine hasMguP_not_of_iff (P' := fun θ =>
                    Unifies θ (ofSpine t₁) (ofSpine t₂) ∧
                      (TyEquiv (τ0.applySubst θ) (τ0'.applySubst θ) ∧ Q θ)) ?_
                    (ih t₁ t₂ _ (by omega) h')
                exact fun θ => ⟨fun ⟨hu, hq⟩ => let ⟨he, hr⟩ := groundMatch_reflect_fwd hg2 hu.symm; ⟨hr.symm, he.symm, hq⟩,
                                fun ⟨hr, he, hq⟩ => ⟨(groundMatch_reflect hg2 he.symm hr.symm).symm, hq⟩⟩
              | none =>
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
                split at h
                · simp at h
                · rename_i hpc
                  exact hbase a s₁ b s₂ Q hsl hsr hv1 hv2 hml hml2 hmr hmr2 hg hg2
                    (by simpa using hpc)

-- Row-level reduction: given the base arm, a stuck verdict rules out an mgu.
-- (`Row.toSpine_equiv` transports HasMgu across the normalization, exactly as in
-- unifyRow_clash_no_unifier; the accumulated predicate starts trivial.)
-- ⊢  (hbase)  →  unifyRow ρ₁ ρ₂ = stuck  →  ¬ HasMgu ρ₁ ρ₂
theorem unifyRow_stuck_no_mgu {B : Type} {ρ₁ ρ₂ : Row B}
    (hbase : ∀ (a : Atom B) (s₁ : List (Atom B)) (b : Atom B) (s₂ : List (Atom B))
              (Q : TySubst B → Prop),
      stripL (a :: s₁) (b :: s₂) = none → stripR (a :: s₁) (b :: s₂) = none →
      solveVar (a :: s₁) (b :: s₂) = none → solveVar (b :: s₂) (a :: s₁) = none →
      matchL (a :: s₁) (b :: s₂) = none → matchL (b :: s₂) (a :: s₁) = none →
      matchR (a :: s₁) (b :: s₂) = none → matchR (b :: s₂) (a :: s₁) = none →
      groundMatch (a :: s₁) (b :: s₂) = none → groundMatch (b :: s₂) (a :: s₁) = none →
      projClash (a :: s₁) (b :: s₂) = false →
      ¬ HasMguP (fun θ => Unifies θ (ofSpine (a :: s₁)) (ofSpine (b :: s₂)) ∧ Q θ))
    (h : unifyRow ρ₁ ρ₂ = .stuck) : ¬ HasMgu ρ₁ ρ₂ := by
  intro hmgu
  unfold unifyRow unifySpine at h
  refine unifySpineF_stuck_no_mgu hbase _ ρ₁.toSpine ρ₂.toSpine (fun _ => True)
    (Nat.le_refl _) h ?_
  rw [hasMgu_eq_hasMguP] at hmgu
  refine (hasMguP_congr (fun θ => ?_)).mp hmgu
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact ⟨fun hu => ⟨e₁.symm.trans (hu.trans e₂), trivial⟩,
         fun ⟨hu, _⟩ => e₁.trans (hu.trans e₂.symm)⟩

------------------------------------ NEXT ------------------------------------
-- Milestones that build on this file (algorithmic.typ, Open questions):
--  * STRICTNESS of the QTyped extension: prove ¬Typed for the two-use
--    program at its precise type (lifts no_plain_principal_scheme through
--    let-inversion) — makes "L2 is strictly more precise" a theorem.
--  * Type safety for QTyped itself (progress/preservation) — the L2 system
--    is the real declarative system of the thesis; minimal.lean's proofs
--    are the template, discharge determinism/monotonicity the new inputs.
--  * ≐ᵣ SUCCESS SOUNDNESS — DONE. unifyRow_success_sound: if unifyRow ρ₁ ρ₂ =
--    success σ eqs and θ meets σ (SolSat) and eqs (EqsSat), then Unifies θ ρ₁ ρ₂.
--    Axiom-clean (propext / Quot.sound, no sorry). Assembled from:
--      - move-reflection lemmas ("θ unifies the residual ⟹ θ unified the
--        original"): stripL/stripR_reflect, solveVar_reflect, matchL_reflect
--        (via windowExtract_equiv), matchR_reflect (via revRow /
--        windowExtract_reverse_equiv), groundMatch_reflect, allVarsEmpty_sound;
--      - the U-ground core: field_comm_lfree (a field ≈-commutes past a
--        var-free, l-free row) + removeField_equiv_of, with the COUNTING
--        (allVars_varFree_of / allVars_lfree_of) discharging the "skipped vars
--        are l-free under θ" side condition from hrec + the ground side being
--        var-free — the one genuinely non-local step, now closed;
--      - fuel induction (unifySpineF_success_sound) discharging each match arm.
--    FUEL SUFFICIENCY — DONE (section "≐ᵣ FUEL SUFFICIENCY"):
--    unifySpineF_fuel_irrel / unifySpineF_fuel_stable — each move eats exactly 2
--    atoms (the *_len lemmas), so |s₁|+|s₂| fuel never runs out and a .stuck
--    result is genuine, not out-of-fuel.
--    OCCURS — CHARACTERIZED: occurs is CONSERVATIVE. occurs_allVar_unifiable
--    shows unifyRow α (β|α|γ) = occurs is UNIFIABLE (β,γ ↦ ε), so the check is
--    incomplete; the genuine no-unifier case needs a field —
--    occurs_field_no_unifier (α ∈ vars s₂ ∧ 0 < count_l s₂ ⟹ no unifier), the
--    occurs analogue of projClash_no_unifier. Both after projClash_no_unifier.
--    CLASH — ALGORITHM-LEVEL, DONE: unifyRow_clash_no_unifier (clash ⟹ no
--    unifier), via unifySpineF_clash_no_unifier (fuel induction) using the two
--    local halves (projClash_no_unifier interior + allVarsEmpty_none_no_unifier
--    base) plus the FORWARD-REFLECTION layer + solveVar_ne_clash/addEq_clash_inv.
--    FORWARD REFLECTION — DONE (completeness direction of every move):
--    strip/match/groundMatch_reflect_fwd via field_cancel_left/right — a unifier
--    of the original unifies the residual (+ emitted type eq).
--    MGU-ON-SUCCESS — DONE: unifyRow_success_complete — every unifier satisfies
--    the emitted σ and eqs, so with unifyRow_success_sound the unifier set is
--    EXACTLY {θ : SolSat θ σ ∧ EqsSat θ eqs}: ≐ᵣ computes a most general unifier.
--    Via unifySpineF_success_complete (fuel induction, forward) + solveVar_complete
--    + allVarsEmpty_complete (RowEquiv.cat_empty_split) + EqsSat.cons.
--    STUCK ⟹ NO-MGU — both canonical base shapes DONE: (1) instanceOf_fieldCount_mono
--    (an mgu is pointwise count-minimal, as subst never deletes a field) + the general
--    no_mgu_of_witness_shrinks; wand_no_mgu_count re-proves Wand (field-vs-vars) via
--    counting, GENERALIZED to n vars by vars_vs_field_no_mgu ((v₁|…|vₙ)≐ᵣ(l:𝓫), nodup,
--    ≥2). (2) instanceOf_fieldCount_eq_of_varFree (a var-free component of an mgu is
--    RIGID) + two_sided_no_mgu for (α|l:𝓫)≐ᵣ(l:𝓫|β) — counting can't shrink the ε,ε
--    unifier there, so rigidity does the kill. (3) allvar_swap_no_mgu for (α|β)≐ᵣ(β|α) —
--    NON-COMMUTATIVITY: witnesses force θα,θβ field-free, hu.char gives A++B=B++A, the
--    combinatorial append_comm_subset (first-occurrence idxOf) forces vars(A)⊆vars(B), so
--    emptying B's vars collapses θα. All three base techniques now proven. LIFT INFRA:
--    HasMgu + hasMgu_congr/hasMgu_rowEquiv (no-mgu depends only on the unifier SET, is a
--    ≈-invariant) + stripL/stripR_hasMgu_iff (strip moves preserve the unifier set)
--    discharge the STRIP arms (demo: wand_under_strip_no_mgu). MATCH/GROUND ARMS —
--    congruence DONE: HasMguP (mgu-status of an arbitrary unifier PREDICATE, since
--    InstanceOf never mentions the rows) + hasMguP_congr, with matchL/matchR/
--    groundMatch_hasMgu_iff transporting HasMgu (ofSpine s₁)(ofSpine s₂) to
--    HasMguP (λθ. θτ≈ₜθτ' ∧ θ ⊨ ofSpine t₁≐ᵣofSpine t₂) — the eq-constrained residual.
--    (All axiom-clean: propext/Quot.sound; hasMguP_congr axiom-free.) Composed
--    end-to-end in wand_under_match_no_mgu (¬HasMgu (l:𝓪|β|α)(l:𝓪|l:𝓫): matchL peels the
--    shared leading field emitting the vacuous eq 𝓪≐𝓪, residual = Wand). CAVEAT: unlike
--    the strip arms these SHRINK the unifier set (intersect with the eq-satisfiers), so
--    they transport mgu-STATUS but do not by themselves let a stuck residual kill the
--    original — that needs the base-technique WITNESSES to also satisfy the accumulated
--    eq (the genuine augmented-witness content, now cleanly expressible via HasMguP).
--    Remaining for the full unifySpineF=stuck⟹no-mgu: (a) assemble the general BASE arm
--    from the 3 techniques (+ all-var beyond the swap); (b) re-run those base witnesses
--    under the accumulated equations (carry them as the HasMguP predicate through the
--    fuel induction).
--    NOTE occurs does NOT lift to a no-unifier theorem — it is conservative
--    (occurs_allVar_unifiable); only occurs_field_no_unifier genuine.
--  * The type-level driver ≐: solve the emitted (τ, τ') equations, mutually
--    recursing into rows; occurs/rank discipline across both sorts.
--  * Non-vacuity of qualified schemes: needs lookup_total (RowWF) plus a
--    freshness discipline for the result variables δ.
--  * The covering order ⊴ on qualified schemes (needed to STATE "the
--    principal type improves under reduction").
--  * Solver state S = (θ, Δ, W), stump wake-up, and the confluence argument
--    that the final state is independent of wake-up scheduling
--    (lookup_det + Discharge.mono_of_definite are the two pillars).

end MinimalCalculus
