-- Unification ≐ / ≐ᵣ: the executable MUTUAL algorithm (unifyTyF /
-- unifySpineMF, entry points unifyTyM / unifyRowM), the field-count invariant,
-- and the trichotomy legs — success soundness & completeness (mgu), clash
-- soundness, fuel monotonicity, terminal-stuck structure, and stuck ⟹ no-mgu
-- (still a reduction; see NEXT at the end). Builds on RowEquiv.
--
-- The detectors and their ≈-metatheory come first, then the base no-mgu
-- techniques, then the scaffolding the driver is stated in (P1–P3), then the
-- driver itself (P4) and its legs (P5–P6). proof-plan.md is the live plan.

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
--   * unique-host variable EXPANSION, Rémy-style, when exactly one variable on
--     the other side can carry the label and that side has none  (U-expand)
--
-- No LUtail: field demands never flow through ≐ᵣ (they park as stumps), so
-- the algorithm never guesses a field into a var. Type equations are SOLVED on
-- the spot by the type pass and applied to the residual — that mutual recursion
-- is what makes a `.stuck` verdict mean something (proof-plan.md §0).
--
-- Presentation uses fuel (structural recursion ⟹ the algorithm computes by
-- rfl; the regressions are kernel-checked executions). Fuel is EXPLICIT at the
-- entry points and exhaustion is its own verdict, `outOfFuel`; unifyM_fuel_mono
-- says a verdict that was reached never changes when the budget grows.

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

-- ## U-expand: unique-host variable expansion (proof-plan.md §1.4)
-- The DETECTORS only; the metatheory (host_forced, expand_shift, the two
-- reflection lemmas) is in the P3 section at the end of the file. They sit here
-- because the dispatch cascade below needs them.
--
-- The fresh names are drawn from a supply derived LOCALLY from the problem
-- (localSupply) and THREADED through the driver: deriving it per call from the
-- current problem is non-monotone, since a move that drops a field drops its
-- type's variables and the bound can fall below a name still in scope
-- (proof-plan.md §4-P3b(1)).


structure Supply where
  next : Nat

/-- Hand out a name and advance. -/
def Supply.fresh (S : Supply) : TyVar × Supply := (natName S.next, ⟨S.next + 1⟩)

def sFtv {B : Type} : List (Atom B) → List TyVar
  | [] => []
  | .field _ τ :: s => τ.ftv ++ sFtv s
  | .var α :: s     => α :: sFtv s

-- The names this problem may invent: strictly longer than everything in it.
def localSupply {B : Type} (s₁ s₂ : List (Atom B)) : Supply :=
  ⟨lenBound (sFtv s₁ ++ sFtv s₂) + 1⟩


def renameVar {B : Type} (β β' : TyVar) : List (Atom B) → List (Atom B)
  | [] => []
  | .var γ :: s => (if γ = β then Atom.var β' else Atom.var γ) :: renameVar β β' s
  | .field l τ :: s => .field l τ :: renameVar β β' s

def uniqueHost {B : Type} (l : Label) (s : List (Atom B)) : Option TyVar :=
  match sVarSeq s with
  | [β] => if sFieldCount l s = 0 then some β else none
  | _   => none

def expandL {B : Type} (S : Supply) :
    List (Atom B) → List (Atom B) →
    Option (TyVar × Label × Ty B × List (Atom B) × List (Atom B))
  | .field l τ :: t₁, s₂ =>
      match uniqueHost l s₂ with
      | some β => some (β, l, τ, t₁, renameVar β S.fresh.2.fresh.1 s₂)
      | none => none
  | _, _ => none

-- … and at the right end (the expansion is then β ≔ (β′ | l:δ)).
def expandR {B : Type} (S : Supply) (s₁ s₂ : List (Atom B)) :
    Option (TyVar × Label × Ty B × List (Atom B) × List (Atom B)) :=
  match expandL S s₁.reverse s₂.reverse with
  | some (β, l, τ, t₁, t₂) => some (β, l, τ, t₁.reverse, t₂.reverse)
  | none => none

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

-- No-mgu of a redescribed unifier predicate transfers along a pointwise iff.
private theorem hasMguP_not_of_iff {B : Type} {P P' : TySubst B → Prop}
    (hiff : ∀ θ, P θ ↔ P' θ) (h : ¬ HasMguP P') : ¬ HasMguP P :=
  fun hmgu => h ((hasMguP_congr hiff).mp hmgu)

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
-- (the algorithm's verdict on it is `occurs_allVar_reported`, with the driver)
-- ⊢  ∃ θ. θ ⊨ α ≐ᵣ (β|α|γ)
theorem occurs_allVar_unifiable {B : Type} :
    ∃ θ : TySubst B,
        Unifies θ (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c"))) :=
  ⟨⟨(.var ·), fun x => if x = "b" then .empty else if x = "c" then .empty else .var x⟩,
   by unfold Unifies
      simp only [Row.applySubst]
      exact (RowEquiv.unitL.trans RowEquiv.unitR).symm⟩

-- INCOMPLETENESS OF THE OCCURS GUARD, SHARPLY. occurs_allVar_unifiable shows the
-- reported-occurs problem α ≐ᵣ (β | α | γ) HAS a unifier; this strengthens that
-- to: it has an MGU (β,γ ↦ ε, identity elsewhere). So the guard rejects a config
-- the trichotomy files under (a) — occurs is not merely incomplete for
-- unifiability, it discards a PRINCIPAL solution. The unifier is FORCED by the
-- ≈-characterization: counting (rowEquiv_fieldCount_eq) kills every field in θβ,
-- θγ, and the var-sequence equation A = B ++ A ++ C forces |B| = |C| = 0 by
-- length; field-free + var-free is ε, so every unifier already agrees with the
-- candidate on β and γ and factors through it via σ ≔ θ itself.
-- ⊢  HasMgu α (β | α | γ)      (yet ≐ᵣ reports .occurs — occurs_allVar_reported)
theorem occurs_allVar_hasMgu {B : Type} :
    HasMgu (.var "a" : Row B) (.cat (.var "b") (.cat (.var "a") (.var "c"))) := by
  refine ⟨⟨(.var ·), fun x => if x = "b" then .empty
                              else if x = "c" then .empty else .var x⟩, ?_, ?_⟩
  · -- ε | α | ε ≈ α
    unfold Unifies
    show RowEquiv (Row.var "a" : Row B)
                  (Row.cat Row.empty (Row.cat (Row.var "a") Row.empty))
    exact (RowEquiv.unitL.trans RowEquiv.unitR).symm
  · intro θ hu
    unfold Unifies at hu
    simp only [Row.applySubst] at hu
    -- counting: count_l A = count_l B + count_l A + count_l C ⟹ B, C field-free
    have hcount : ∀ l, sFieldCount l (θ.row "b").toSpine = 0 ∧
                       sFieldCount l (θ.row "c").toSpine = 0 := fun l => by
      have h := rowEquiv_fieldCount_eq l hu
      simp only [show (Row.cat (θ.row "b") (Row.cat (θ.row "a") (θ.row "c"))).toSpine
            = (θ.row "b").toSpine ++ ((θ.row "a").toSpine ++ (θ.row "c").toSpine) from rfl,
        sFieldCount_append] at h
      omega
    -- var sequences: A = B ++ A ++ C ⟹ |B| = |C| = 0 ⟹ B, C var-free
    have hvar : sVarSeq (θ.row "b").toSpine = [] ∧ sVarSeq (θ.row "c").toSpine = [] := by
      obtain ⟨hv, -⟩ := hu.char
      simp only [show (Row.cat (θ.row "b") (Row.cat (θ.row "a") (θ.row "c"))).toSpine
            = (θ.row "b").toSpine ++ ((θ.row "a").toSpine ++ (θ.row "c").toSpine) from rfl,
        sVarSeq_append] at hv
      have hlen := congrArg List.length hv
      simp only [List.length_append] at hlen
      exact ⟨List.eq_nil_of_length_eq_zero (by omega),
             List.eq_nil_of_length_eq_zero (by omega)⟩
    -- field-free + var-free is ε
    have hε : ∀ x : TyVar, sVarSeq (θ.row x).toSpine = [] →
        (∀ l, sFieldCount l (θ.row x).toSpine = 0) → RowEquiv (θ.row x) (Row.empty : Row B) :=
      fun x hv hf => RowEquiv.ofChar ⟨by rw [hv]; rfl,
        fun l => by rw [sProj_nil_of_fieldCount_zero (hf l)]; exact .nil⟩
    -- σ ≔ θ: the candidate is the identity away from β, γ, and θ already sends both to ε
    refine ⟨θ, fun x => ?_, fun x => ?_⟩
    · by_cases hb : x = "b"
      · subst hb
        exact (hε "b" hvar.1 (fun l => (hcount l).1)).trans
          (RowEquiv.refl (Row.empty : Row B))
      · by_cases hc : x = "c"
        · subst hc
          exact (hε "c" hvar.2 (fun l => (hcount l).2)).trans
            (RowEquiv.refl (Row.empty : Row B))
        · show RowEquiv (θ.row x) ((if x = "b" then Row.empty
                              else if x = "c" then Row.empty else Row.var x).applySubst θ)
          rw [if_neg hb, if_neg hc]
          exact RowEquiv.refl _
    · exact TyEquiv.refl _

-- ## The stuck verdict that WAS wrong — now fixed by U-expand
-- (l:𝓫 | α) ≐ᵣ (m:𝓫 | β), l ≠ m, used to be reported STUCK, yet it is not
-- merely unifiable (so is Wand): its unifier is FORCED, i.e. the algorithm was
-- INCOMPLETE here, not just non-principal. Reading the ≈-characterization on a
-- unifier θ (writing A = θα, B = θβ):
--   proj_m A = (0,𝓫) :: proj_m B      (the left has no m outside A…)
--   proj_l B = (0,𝓫) :: proj_l A      (…and the right none outside B)
--   proj_k A = proj_k B  (k ∉ {l,m}),  vars A = vars B
-- Segment index 0 means "before the first var", so A ≈ (m:𝓫 | R) and, feeding
-- that back, B ≈ (l:𝓫 | R) for the SAME R — every unifier factors through
--   α ↦ (m:𝓫 | X),  β ↦ (l:𝓫 | X)      (X fresh)
-- which is therefore an mgu. host_forced mechanizes the "segment index 0 ⟹
-- the field sits at the front of the host" step (crossfield_host_forced).
-- U-expand is exactly this move, and the algorithm now takes it: β is the unique
-- variable of the right side and the right side has no l-field at all, so β is
-- FORCED to host, and the emitted binding is the mgu above with X = β′.
-- The computed verdict is `crossfield_success`, with the driver below.

-- … and the candidate mgu is still a unifier (it is the emitted one, with the
-- type equation 𝓫 ≐ δ solved and β′ instantiated to X).
-- ⊢  θ ⊨ (l:𝓫 | α) ≐ᵣ (m:𝓫 | β)   for  α ↦ (m:𝓫 | X),  β ↦ (l:𝓫 | X)
theorem crossfield_unifiable {B : Type} (b : B) :
    Unifies
        (⟨(.var ·), fun x =>
            if x = "a" then .cat (.sing "m" (.base b)) (.var "X")
            else if x = "b" then .cat (.sing "l" (.base b)) (.var "X")
            else .var x⟩ : TySubst B)
        (.cat (.sing "l" (.base b)) (.var "a"))
        (.cat (.sing "m" (.base b)) (.var "b")) := by
  unfold Unifies
  show RowEquiv
      (Row.cat (Row.sing "l" (.base b)) (Row.cat (Row.sing "m" (.base b)) (Row.var "X")))
      (Row.cat (Row.sing "m" (.base b)) (Row.cat (Row.sing "l" (.base b)) (Row.var "X")))
  exact RowEquiv.assoc.symm.trans
    ((RowEquiv.cat (RowEquiv.comm (by decide)) (RowEquiv.refl _)).trans RowEquiv.assoc)

-- ## Base-case clash: the OTHER place the algorithm answers clash
-- unifySpineMF returns clash when one side is exhausted but the other still
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

------------------------- ≐ᵣ SUCCESS SOUNDNESS  -----------------------
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

-- ## SolSat, cons-wise
theorem SolSat.cons {B : Type} {θ : TySubst B} {α : TyVar} {ρ : Row B}
    {σ : List (TyVar × Row B)}
    (hrow : RowEquiv (θ.row α) (ρ.applySubst θ)) (h : SolSat θ σ) :
    SolSat θ ((α, ρ) :: σ) := by
  intro p hp
  rcases List.mem_cons.mp hp with rfl | hp'
  · exact hrow
  · exact h p hp'

theorem SolSat.head {B : Type} {θ : TySubst B} {α : TyVar} {ρ : Row B}
    {σ : List (TyVar × Row B)} (h : SolSat θ ((α, ρ) :: σ)) :
    RowEquiv (θ.row α) (ρ.applySubst θ) := h (α, ρ) List.mem_cons_self

theorem SolSat.tail {B : Type} {θ : TySubst B} {α : TyVar} {ρ : Row B}
    {σ : List (TyVar × Row B)} (h : SolSat θ ((α, ρ) :: σ)) : SolSat θ σ :=
  fun p hp => h p (List.mem_cons_of_mem _ hp)

---------------- P1: SCAFFOLDING FOR THE MUTUAL ≐ / ≐ᵣ DRIVER -----------------
-- proof-plan.md §1.1 + §2 ("New, small"). The vocabulary the mutual driver is
-- stated in: solutions at both sorts, its result type, and the apply-then-unify
-- bridge every eq-emitting arm consumes.
--
-- The one genuinely new piece of theory is ≗ and its congruence: applySubst
-- respects POINTWISE ≈-EQUALITY of substitutions. minimal.lean only has the
-- equality version (Ty/Row.applySubst_congr, :1697/:1714), which is too rigid
-- here — a solution is only ever met UP TO ≈ (SolSat, :1030), so "θ agrees with
-- θ ∘ s.toSubst" can only ever be a ≈-statement. Everything else in this
-- section is bookkeeping on top of it.

-- ## ≗ : pointwise ≈-equality of substitutions
def SubstEquiv {B : Type} (θ₁ θ₂ : TySubst B) : Prop :=
  (∀ α, TyEquiv (θ₁.ty α) (θ₂.ty α)) ∧ (∀ α, RowEquiv (θ₁.row α) (θ₂.row α))

infix:50 " ≗ " => SubstEquiv

theorem SubstEquiv.refl {B : Type} (θ : TySubst B) : θ ≗ θ :=
  ⟨fun _ => .refl _, fun _ => .refl _⟩

theorem SubstEquiv.symm {B : Type} {θ₁ θ₂ : TySubst B} (h : θ₁ ≗ θ₂) : θ₂ ≗ θ₁ :=
  ⟨fun α => (h.1 α).symm, fun α => (h.2 α).symm⟩

theorem SubstEquiv.trans {B : Type} {θ₁ θ₂ θ₃ : TySubst B}
    (h₁ : θ₁ ≗ θ₂) (h₂ : θ₂ ≗ θ₃) : θ₁ ≗ θ₃ :=
  ⟨fun α => (h₁.1 α).trans (h₂.1 α), fun α => (h₁.2 α).trans (h₂.2 α)⟩

-- THE CONGRUENCE. Structural, at both sorts at once (types contain rows).
-- ⊢  θ₁ ≗ θ₂   ⟹   τ.applySubst θ₁ ≈ₜ τ.applySubst θ₂
-- ⊢  θ₁ ≗ θ₂   ⟹   ρ.applySubst θ₁ ≈ᵣ ρ.applySubst θ₂
mutual
  theorem Ty.applySubst_substEquiv {B : Type} {θ₁ θ₂ : TySubst B} (h : θ₁ ≗ θ₂) :
      (τ : Ty B) → TyEquiv (τ.applySubst θ₁) (τ.applySubst θ₂)
    | .var α    => h.1 α
    | .base _   => .refl _
    | .unk      => .refl _
    | .fn τ₁ τ₂ =>
        .fn (Ty.applySubst_substEquiv h τ₁) (Ty.applySubst_substEquiv h τ₂)
    | .rcd ρ    => .rcd (Row.applySubst_substEquiv h ρ)

  theorem Row.applySubst_substEquiv {B : Type} {θ₁ θ₂ : TySubst B} (h : θ₁ ≗ θ₂) :
      (ρ : Row B) → RowEquiv (ρ.applySubst θ₁) (ρ.applySubst θ₂)
    | .empty     => .refl _
    | .var α     => h.2 α
    | .sing _ τ  => .sing (Ty.applySubst_substEquiv h τ)
    | .cat ρ₁ ρ₂ =>
        .cat (Row.applySubst_substEquiv h ρ₁) (Row.applySubst_substEquiv h ρ₂)
end

-- ## Solutions at both sorts
-- Sol replaces the pair (σ : List (TyVar × Row B), eqs) of URes.success: the
-- type equations are no longer parked but SOLVED, and their solutions land in
-- the .ty component. One shared TyVar namespace (minimal.lean:649), so a
-- variable bound by the type pass is readable by the row pass.
structure Sol (B : Type) where
  ty  : List (TyVar × Ty B)
  row : List (TyVar × Row B)

def Sol.nil {B : Type} : Sol B := ⟨[], []⟩

-- The old row-only solution, embedded.
def Sol.ofRow {B : Type} (σ : List (TyVar × Row B)) : Sol B := ⟨[], σ⟩

-- Association lists, resolved with `if β = α` rather than List.lookup so the
-- membership spec below is a two-line structural induction.
def tyLookup {B : Type} (α : TyVar) : List (TyVar × Ty B) → Ty B
  | [] => .var α
  | (β, τ) :: t => if β = α then τ else tyLookup α t

def rowLookup {B : Type} (α : TyVar) : List (TyVar × Row B) → Row B
  | [] => .var α
  | (β, ρ) :: t => if β = α then ρ else rowLookup α t

-- ⊢  a lookup either leaves the variable free, or is one of the bindings
theorem tyLookup_spec {B : Type} (α : TyVar) :
    (l : List (TyVar × Ty B)) → tyLookup α l = .var α ∨ (α, tyLookup α l) ∈ l
  | [] => .inl rfl
  | (β, τ) :: t => by
      by_cases h : β = α
      · subst h; exact .inr (by simp only [tyLookup, if_pos]; exact List.mem_cons_self)
      · simp only [tyLookup, if_neg h]
        exact (tyLookup_spec α t).imp id (List.mem_cons_of_mem _)

theorem rowLookup_spec {B : Type} (α : TyVar) :
    (l : List (TyVar × Row B)) → rowLookup α l = .var α ∨ (α, rowLookup α l) ∈ l
  | [] => .inl rfl
  | (β, ρ) :: t => by
      by_cases h : β = α
      · subst h; exact .inr (by simp only [rowLookup, if_pos]; exact List.mem_cons_self)
      · simp only [rowLookup, if_neg h]
        exact (rowLookup_spec α t).imp id (List.mem_cons_of_mem _)

-- The substitution a solution denotes: bound variables go to their binding,
-- every other variable stays free.
def Sol.toSubst {B : Type} (s : Sol B) : TySubst B :=
  ⟨fun α => tyLookup α s.ty, fun α => rowLookup α s.row⟩

-- SolSat (:1030) at both sorts.
def Sol.Sat {B : Type} (θ : TySubst B) (s : Sol B) : Prop :=
  (∀ p ∈ s.ty, TyEquiv (θ.ty p.1) (p.2.applySubst θ)) ∧
  (∀ p ∈ s.row, RowEquiv (θ.row p.1) (p.2.applySubst θ))

-- ⊢  Sol.Sat θ (Sol.ofRow σ)  ↔  SolSat θ σ      (the embedding is faithful)
theorem Sol.Sat_ofRow {B : Type} {θ : TySubst B} {σ : List (TyVar × Row B)} :
    Sol.Sat θ (Sol.ofRow σ) ↔ SolSat θ σ :=
  ⟨fun h => h.2, fun h => ⟨fun _ hp => (nomatch hp), h⟩⟩

-- THE POINT OF Sat: meeting a solution means being ≈-unchanged by it. This is
-- what turns "solve, then apply to the residual" into "same problem".
-- ⊢  Sol.Sat θ s   ⟹   θ ≗ θ ∘ s.toSubst
theorem Sol.Sat.substEquiv {B : Type} {θ : TySubst B} {s : Sol B}
    (h : Sol.Sat θ s) : θ ≗ θ.comp s.toSubst := by
  constructor
  · intro α
    show TyEquiv (θ.ty α) ((tyLookup α s.ty).applySubst θ)
    rcases tyLookup_spec α s.ty with he | hm
    · rw [he]; exact .refl _
    · exact h.1 _ hm
  · intro α
    show RowEquiv (θ.row α) ((rowLookup α s.row).applySubst θ)
    rcases rowLookup_spec α s.row with he | hm
    · rw [he]; exact .refl _
    · exact h.2 _ hm

-- ## Composing solutions
-- s₂.comp s₁ — first s₁, then s₂ — the Sol-level image of TySubst.comp
-- (minimal.lean:1669): push s₂ through s₁'s bindings, then keep s₂'s own.
def Sol.comp {B : Type} (s₂ s₁ : Sol B) : Sol B :=
  ⟨s₁.ty.map  (fun p => (p.1, p.2.applySubst s₂.toSubst)) ++ s₂.ty,
   s₁.row.map (fun p => (p.1, p.2.applySubst s₂.toSubst)) ++ s₂.row⟩

-- THE COMPOSE LEMMA the P5 arms need (proof-plan.md §2, table row 1): meeting a
-- composite means meeting both halves. The s₂ half is immediate (it sits in the
-- append verbatim); the s₁ half then follows from ≗-congruence, since θ is
-- ≈-unchanged by s₂.
-- ⊢  Sol.Sat θ (s₂.comp s₁)   ⟹   Sol.Sat θ s₁  ∧  Sol.Sat θ s₂
theorem Sol.Sat.comp_inv {B : Type} {θ : TySubst B} {s₁ s₂ : Sol B}
    (h : Sol.Sat θ (s₂.comp s₁)) : Sol.Sat θ s₁ ∧ Sol.Sat θ s₂ := by
  have h₂ : Sol.Sat θ s₂ :=
    ⟨fun p hp => h.1 p (List.mem_append_right _ hp),
     fun p hp => h.2 p (List.mem_append_right _ hp)⟩
  refine ⟨⟨fun p hp => ?_, fun p hp => ?_⟩, h₂⟩
  · have hm : (p.1, p.2.applySubst s₂.toSubst) ∈ (s₂.comp s₁).ty :=
      List.mem_append_left _ (List.mem_map_of_mem hp)
    have := h.1 _ hm
    refine this.trans ?_
    rw [Ty.applySubst_applySubst]
    exact (Ty.applySubst_substEquiv h₂.substEquiv p.2).symm
  · have hm : (p.1, p.2.applySubst s₂.toSubst) ∈ (s₂.comp s₁).row :=
      List.mem_append_left _ (List.mem_map_of_mem hp)
    have := h.2 _ hm
    refine this.trans ?_
    rw [Row.applySubst_applySubst]
    exact (Row.applySubst_substEquiv h₂.substEquiv p.2).symm

-- ## The result type of the mutual driver
-- Two changes from URes (proof-plan.md §1.1, and the P4 deviation recorded in
-- §4): no `eqs` component — everything a success discovers is SOLVED — and a
-- success carries the SUPPLY it stopped at, so the fresh names invented by one
-- sub-call are not handed out again by the next (a type equation solved inside
-- a field may expand a row variable, and the invented tail then travels into
-- the residual). `outOfFuel` is the fifth verdict: it separates "the algorithm
-- ran out of budget" from "every move is dead", which is what makes the fuel
-- lemma a structural induction rather than a termination measure.
inductive UResM (B : Type) : Type where
  | success   : Sol B → Supply → UResM B
  | clash     : UResM B
  | occurs    : UResM B
  | stuck     : UResM B
  | outOfFuel : UResM B

-- Sequencing, as used by every eq-emitting arm in §1.2: run the second stage
-- under the first stage's solution AND its supply, then compose. A non-success
-- in either stage is the verdict of the whole.
def UResM.seq {B : Type} : UResM B → (TySubst B → Supply → UResM B) → UResM B
  | .success s S, k =>
      match k s.toSubst S with
      | .success s' S' => .success (s'.comp s) S'
      | r => r
  | r, _ => r

-- ⊢  seq inverts: a success came from two successes whose composite it is
theorem UResM.seq_success {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    {s : Sol B} {S : Supply} (h : r.seq k = .success s S) :
    ∃ s₁ S₁ s₂, r = .success s₁ S₁ ∧ k s₁.toSubst S₁ = .success s₂ S ∧ s = s₂.comp s₁ := by
  cases r with
  | success s₁ S₁ =>
      simp only [UResM.seq] at h
      revert h; cases hk : k s₁.toSubst S₁ with
      | success s₂ S₂ => intro h; cases h; exact ⟨s₁, S₁, s₂, rfl, hk, rfl⟩
      | clash  => intro h; cases h
      | occurs => intro h; cases h
      | stuck  => intro h; cases h
      | outOfFuel => intro h; cases h
  | clash  => cases h
  | occurs => cases h
  | stuck  => cases h
  | outOfFuel => cases h

-- ## Unification at the type sort
-- The ≐ counterpart of Unifies (RowEquiv.lean:543); EqsSat is exactly a list of
-- these, so the parked-equation vocabulary survives as the proof-internal
-- device proof-plan.md §1.1 predicts.
def TyUnifies {B : Type} (θ : TySubst B) (τ τ' : Ty B) : Prop :=
  TyEquiv (τ.applySubst θ) (τ'.applySubst θ)

theorem eqsSat_iff_tyUnifies {B : Type} {θ : TySubst B} {eqs : List (Ty B × Ty B)} :
    EqsSat θ eqs ↔ ∀ p ∈ eqs, TyUnifies θ p.1 p.2 := Iff.rfl

-- ## Substitution on spines
-- A var atom expands to a whole spine, so this is not a map: it is the spine
-- image of Row.applySubst. Written by structural recursion (not via
-- ofSpine/toSpine) so the regressions keep reducing by rfl.
def sApplySubst {B : Type} (θ : TySubst B) : List (Atom B) → List (Atom B)
  | [] => []
  | .field l τ :: s => .field l (τ.applySubst θ) :: sApplySubst θ s
  | .var α :: s     => (θ.row α).toSpine ++ sApplySubst θ s

-- ⊢  ofSpine (sApplySubst θ s)  ≈ᵣ  (ofSpine s).applySubst θ
theorem sApplySubst_equiv {B : Type} (θ : TySubst B) :
    (s : List (Atom B)) →
    RowEquiv (ofSpine (sApplySubst θ s)) ((ofSpine s).applySubst θ)
  | [] => .refl _
  | .field _ _ :: s => .cat (.refl _) (sApplySubst_equiv θ s)
  | .var α :: s =>
      (ofSpine_append _ _).trans
        (.cat (Row.toSpine_equiv (θ.row α)).symm (sApplySubst_equiv θ s))

-- ## THE BRIDGE (proof-plan.md §2, "New, small")
-- Unifying the σ-substituted problem = unifying the original under θ ∘ σ.
-- ⊢  θ ⊨ ρ₁σ ≐ᵣ ρ₂σ   ↔   (θ ∘ σ) ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifies_applySubst_iff {B : Type} (θ σ : TySubst B) (ρ₁ ρ₂ : Row B) :
    Unifies θ (ρ₁.applySubst σ) (ρ₂.applySubst σ) ↔ Unifies (θ.comp σ) ρ₁ ρ₂ := by
  unfold Unifies
  rw [Row.applySubst_applySubst, Row.applySubst_applySubst]

-- ⊢  θ ⊨ τσ ≐ τ'σ   ↔   (θ ∘ σ) ⊨ τ ≐ τ'
theorem tyUnifies_applySubst_iff {B : Type} (θ σ : TySubst B) (τ τ' : Ty B) :
    TyUnifies θ (τ.applySubst σ) (τ'.applySubst σ) ↔ TyUnifies (θ.comp σ) τ τ' := by
  unfold TyUnifies
  rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]

-- … and on spines, the shape the eq-emitting arms actually produce.
-- ⊢  θ ⊨ ofSpine (sApplySubst σ s₁) ≐ᵣ ofSpine (sApplySubst σ s₂)
--        ↔   (θ ∘ σ) ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
theorem unifies_sApplySubst_iff {B : Type} (θ σ : TySubst B) (s₁ s₂ : List (Atom B)) :
    Unifies θ (ofSpine (sApplySubst σ s₁)) (ofSpine (sApplySubst σ s₂)) ↔
    Unifies (θ.comp σ) (ofSpine s₁) (ofSpine s₂) := by
  have e₁ := RowEquiv.applySubst θ (sApplySubst_equiv σ s₁)
  have e₂ := RowEquiv.applySubst θ (sApplySubst_equiv σ s₂)
  rw [← unifies_applySubst_iff]
  exact ⟨fun h => e₁.symm.trans (h.trans e₂), fun h => e₁.trans (h.trans e₂.symm)⟩

-- APPLY-THEN-UNIFY. The form used in the arms: once θ MEETS the sub-solution,
-- applying it to the residual changes nothing, so the recursive call is on the
-- same set of unifiers. This is what makes the eager driver conservative.
-- ⊢  Sol.Sat θ s   ⟹   ( θ ⊨ ρ₁s ≐ᵣ ρ₂s  ↔  θ ⊨ ρ₁ ≐ᵣ ρ₂ )
theorem unifies_applySubst_of_sat {B : Type} {θ : TySubst B} {s : Sol B}
    (h : Sol.Sat θ s) (ρ₁ ρ₂ : Row B) :
    Unifies θ (ρ₁.applySubst s.toSubst) (ρ₂.applySubst s.toSubst) ↔ Unifies θ ρ₁ ρ₂ := by
  rw [unifies_applySubst_iff]
  have e₁ := Row.applySubst_substEquiv h.substEquiv ρ₁
  have e₂ := Row.applySubst_substEquiv h.substEquiv ρ₂
  exact ⟨fun hu => e₁.trans (hu.trans e₂.symm), fun hu => e₁.symm.trans (hu.trans e₂)⟩

-- ⊢  Sol.Sat θ s   ⟹   ( θ ⊨ τs ≐ τ's  ↔  θ ⊨ τ ≐ τ' )
theorem tyUnifies_applySubst_of_sat {B : Type} {θ : TySubst B} {s : Sol B}
    (h : Sol.Sat θ s) (τ τ' : Ty B) :
    TyUnifies θ (τ.applySubst s.toSubst) (τ'.applySubst s.toSubst) ↔ TyUnifies θ τ τ' := by
  rw [tyUnifies_applySubst_iff]
  have e₁ := Ty.applySubst_substEquiv h.substEquiv τ
  have e₂ := Ty.applySubst_substEquiv h.substEquiv τ'
  exact ⟨fun hu => e₁.trans (hu.trans e₂.symm), fun hu => e₁.symm.trans (hu.trans e₂)⟩

-- ⊢  Sol.Sat θ s   ⟹
--       ( θ ⊨ ofSpine (sApplySubst s.toSubst s₁) ≐ᵣ ofSpine (sApplySubst s.toSubst s₂)
--         ↔  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂ )
theorem unifies_sApplySubst_of_sat {B : Type} {θ : TySubst B} {s : Sol B}
    (h : Sol.Sat θ s) (s₁ s₂ : List (Atom B)) :
    Unifies θ (ofSpine (sApplySubst s.toSubst s₁)) (ofSpine (sApplySubst s.toSubst s₂)) ↔
    Unifies θ (ofSpine s₁) (ofSpine s₂) := by
  have e₁ := RowEquiv.applySubst θ (sApplySubst_equiv s.toSubst s₁)
  have e₂ := RowEquiv.applySubst θ (sApplySubst_equiv s.toSubst s₂)
  rw [show Unifies θ (ofSpine (sApplySubst s.toSubst s₁))
             (ofSpine (sApplySubst s.toSubst s₂))
        ↔ Unifies θ ((ofSpine s₁).applySubst s.toSubst)
             ((ofSpine s₂).applySubst s.toSubst) from
      ⟨fun hu => e₁.symm.trans (hu.trans e₂), fun hu => e₁.trans (hu.trans e₂.symm)⟩]
  exact unifies_applySubst_of_sat h _ _


------------------- P2: A FRESH-VARIABLE SUPPLY FOR ≐ / ≐ᵣ -------------------
-- proof-plan.md §1.4 / §4-P2. expandVar (P3) invents a field type δ and a tail
-- β′ out of thin air; every soundness proof about it then needs "θ may be
-- perturbed on δ and β′ without changing what it does to the problem". That is
-- the whole content of this section. It is also, independently, the missing
-- input for the qualified-scheme non-vacuity milestone.
--
-- The generator is minimal.lean's (:1752), reused verbatim: natName n has
-- LENGTH n, so a name longer than everything in an avoid-set is fresh and two
-- names of different lengths never collide. Nothing here inspects strings —
-- the supply is a Nat, and the avoid-set is PROOF-ONLY: the algorithm never
-- computes it, so the arms stay reducible.

-- ## The supply
/-- The invariant: every name `S` can still produce is longer than everything
in `avoid`, hence fresh for it. -/
def Supply.Avoids (S : Supply) (avoid : List TyVar) : Prop := lenBound avoid < S.next

-- lenBound is a foldr of max, so consing is definitional.
theorem lenBound_cons (a : TyVar) (l : List TyVar) :
    lenBound (a :: l) = max a.length (lenBound l) := rfl

-- ⊢  every member short enough ⟹ the bound is short enough
theorem lenBound_le {n : Nat} :
    (l : List TyVar) → (∀ s ∈ l, s.length ≤ n) → lenBound l ≤ n
  | [], _ => Nat.zero_le n
  | a :: l, h => by
      rw [lenBound_cons]
      exact Nat.max_le.mpr
        ⟨h a List.mem_cons_self,
         lenBound_le l (fun s hs => h s (List.mem_cons_of_mem _ hs))⟩

-- ⊢  l' ⊆ l   ⟹   lenBound l' ≤ lenBound l
theorem lenBound_mono {l l' : List TyVar} (h : l' ⊆ l) : lenBound l' ≤ lenBound l :=
  lenBound_le l' (fun _ hs => length_le_lenBound (h hs))

-- THE FRESHNESS SPEC.
-- ⊢  S.Avoids avoid   ⟹   S.fresh.1 ∉ avoid
theorem Supply.fresh_not_mem {S : Supply} {avoid : List TyVar}
    (h : S.Avoids avoid) : S.fresh.1 ∉ avoid := fun hm => by
  have hlen : (natName S.next).length ≤ lenBound avoid := length_le_lenBound hm
  rw [natName_length] at hlen
  exact absurd h (Nat.not_lt.mpr hlen)

-- ⊢  advancing keeps the invariant …
theorem Supply.Avoids.advance {S : Supply} {avoid : List TyVar}
    (h : S.Avoids avoid) : S.fresh.2.Avoids avoid := Nat.lt_succ_of_lt h

-- … and the name just handed out may be added to the avoid-set, which is how
-- the invariant survives a move that GROWS the problem (expandVar).
theorem Supply.Avoids.cons_fresh {S : Supply} {avoid : List TyVar}
    (h : S.Avoids avoid) : S.fresh.2.Avoids (S.fresh.1 :: avoid) := by
  show lenBound (natName S.next :: avoid) < S.next + 1
  rw [lenBound_cons, natName_length]
  exact Nat.max_lt.mpr ⟨Nat.lt_succ_self _, Nat.lt_succ_of_lt h⟩

-- ⊢  shrinking the problem never breaks the invariant (every non-expanding move)
theorem Supply.Avoids.mono {S : Supply} {l l' : List TyVar}
    (hsub : l' ⊆ l) (h : S.Avoids l) : S.Avoids l' :=
  Nat.lt_of_le_of_lt (lenBound_mono hsub) h

-- ⊢  two draws are distinct — expandVar needs δ ≠ β′
theorem Supply.fresh_ne (S : Supply) : S.fresh.1 ≠ S.fresh.2.fresh.1 := fun h => by
  have h' : natName S.next = natName (S.next + 1) := h
  have := natName_inj h'
  omega

-- ## Free variables of a spine
-- The avoid-set of a row problem. Field types count: the namespace is shared
-- (minimal.lean:649), so a fresh row-var must dodge type-var occurrences too.
-- ⊢  sFtv s = (ofSpine s).ftv       (so the spine- and row-level notions agree)
theorem sFtv_ofSpine {B : Type} : (s : List (Atom B)) → sFtv s = (ofSpine s).ftv
  | [] => rfl
  | .field _ τ :: s => by simp only [sFtv, ofSpine, Row.ftv, sFtv_ofSpine s]
  | .var α :: s => by simp only [sFtv, ofSpine, Row.ftv, sFtv_ofSpine s]; rfl

theorem sFtv_append {B : Type} :
    (s t : List (Atom B)) → sFtv (s ++ t) = sFtv s ++ sFtv t
  | [], _ => rfl
  | .field _ τ :: s, t => by
      simp only [List.cons_append, sFtv, sFtv_append s t, List.append_assoc]
  | .var _ :: s, t => by simp only [List.cons_append, sFtv, sFtv_append s t]

theorem sFtv_cons {B : Type} (a : Atom B) (s : List (Atom B)) :
    sFtv (a :: s) = sFtv [a] ++ sFtv s := by
  cases a <;> simp only [sFtv, List.append_nil, List.singleton_append]

-- The right-end moves (stripR/matchR) work on reversed spines, so freshness
-- has to see through List.reverse.
theorem mem_sFtv_reverse {B : Type} {α : TyVar} :
    (s : List (Atom B)) → (α ∈ sFtv s.reverse ↔ α ∈ sFtv s)
  | [] => Iff.rfl
  | a :: s => by
      rw [List.reverse_cons, sFtv_append, sFtv_cons a s]
      simp only [List.mem_append, mem_sFtv_reverse s]
      exact Or.comm

-- ## Perturbing a substitution at a fresh variable
def TySubst.setTy {B : Type} (θ : TySubst B) (α : TyVar) (τ : Ty B) : TySubst B :=
  ⟨fun β => if β = α then τ else θ.ty β, θ.row⟩

def TySubst.setRow {B : Type} (θ : TySubst B) (α : TyVar) (ρ : Row B) : TySubst B :=
  ⟨θ.ty, fun β => if β = α then ρ else θ.row β⟩

-- THE INVISIBILITY LEMMAS: a value chosen at a variable the subject does not
-- mention is not observable. Straight off Ty/Row.applySubst_congr
-- (minimal.lean:1697/:1714) — the EQUALITY congruence is exactly right here,
-- since setTy/setRow change θ pointwise, not up to ≈.
theorem Row.applySubst_setRow_of_not_mem {B : Type} {θ : TySubst B} {α : TyVar}
    {ρ : Row B} (ρ' : Row B) (h : α ∉ ρ'.ftv) :
    ρ'.applySubst (θ.setRow α ρ) = ρ'.applySubst θ :=
  Row.applySubst_congr ρ' fun β hβ =>
    ⟨rfl, by
      have hne : β ≠ α := fun hb => h (by rw [← hb]; exact hβ)
      simp only [TySubst.setRow, if_neg hne]⟩

theorem Ty.applySubst_setRow_of_not_mem {B : Type} {θ : TySubst B} {α : TyVar}
    {ρ : Row B} (τ : Ty B) (h : α ∉ τ.ftv) :
    τ.applySubst (θ.setRow α ρ) = τ.applySubst θ :=
  Ty.applySubst_congr τ fun β hβ =>
    ⟨rfl, by
      have hne : β ≠ α := fun hb => h (by rw [← hb]; exact hβ)
      simp only [TySubst.setRow, if_neg hne]⟩

theorem Row.applySubst_setTy_of_not_mem {B : Type} {θ : TySubst B} {α : TyVar}
    {τ : Ty B} (ρ' : Row B) (h : α ∉ ρ'.ftv) :
    ρ'.applySubst (θ.setTy α τ) = ρ'.applySubst θ :=
  Row.applySubst_congr ρ' fun β hβ =>
    ⟨by
      have hne : β ≠ α := fun hb => h (by rw [← hb]; exact hβ)
      simp only [TySubst.setTy, if_neg hne], rfl⟩

theorem Ty.applySubst_setTy_of_not_mem {B : Type} {θ : TySubst B} {α : TyVar}
    {τ : Ty B} (τ' : Ty B) (h : α ∉ τ'.ftv) :
    τ'.applySubst (θ.setTy α τ) = τ'.applySubst θ :=
  Ty.applySubst_congr τ' fun β hβ =>
    ⟨by
      have hne : β ≠ α := fun hb => h (by rw [← hb]; exact hβ)
      simp only [TySubst.setTy, if_neg hne], rfl⟩

-- … lifted to the two unification predicates.
-- ⊢  α ∉ ftv ρ₁, ftv ρ₂   ⟹   ( θ[α ≔ ρ] ⊨ ρ₁ ≐ᵣ ρ₂  ↔  θ ⊨ ρ₁ ≐ᵣ ρ₂ )
theorem unifies_setRow_of_not_mem {B : Type} {θ : TySubst B} {α : TyVar}
    {ρ ρ₁ ρ₂ : Row B} (h₁ : α ∉ ρ₁.ftv) (h₂ : α ∉ ρ₂.ftv) :
    Unifies (θ.setRow α ρ) ρ₁ ρ₂ ↔ Unifies θ ρ₁ ρ₂ := by
  unfold Unifies
  rw [Row.applySubst_setRow_of_not_mem ρ₁ h₁, Row.applySubst_setRow_of_not_mem ρ₂ h₂]

theorem tyUnifies_setTy_of_not_mem {B : Type} {θ : TySubst B} {α : TyVar}
    {τ : Ty B} {τ₁ τ₂ : Ty B} (h₁ : α ∉ τ₁.ftv) (h₂ : α ∉ τ₂.ftv) :
    TyUnifies (θ.setTy α τ) τ₁ τ₂ ↔ TyUnifies θ τ₁ τ₂ := by
  unfold TyUnifies
  rw [Ty.applySubst_setTy_of_not_mem τ₁ h₁, Ty.applySubst_setTy_of_not_mem τ₂ h₂]

-- THE HEADLINE: a supply that avoids the problem hands out a variable whose
-- value is free — the problem's unifier set does not see it. This is the
-- "fresh variables do not occur in the problem" lemma expandVar consumes.
-- ⊢  S.Avoids (sFtv s₁ ++ sFtv s₂)   ⟹
--       ( θ[S.fresh ≔ ρ] ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂  ↔  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂ )
theorem Supply.unifies_setRow_fresh {B : Type} {S : Supply} {θ : TySubst B}
    {ρ : Row B} {s₁ s₂ : List (Atom B)} (h : S.Avoids (sFtv s₁ ++ sFtv s₂)) :
    Unifies (θ.setRow S.fresh.1 ρ) (ofSpine s₁) (ofSpine s₂) ↔
    Unifies θ (ofSpine s₁) (ofSpine s₂) := by
  have hnm := S.fresh_not_mem h
  rw [List.mem_append] at hnm
  refine unifies_setRow_of_not_mem ?_ ?_
  · rw [← sFtv_ofSpine]; exact fun hm => hnm (.inl hm)
  · rw [← sFtv_ofSpine]; exact fun hm => hnm (.inr hm)

-- ## The invariant survives every existing move
-- Each detector's residual mentions no new variable, so `S.Avoids` transports
-- to the recursive call by Avoids.mono. (The detectors themselves are
-- untouched by the rebuild — proof-plan.md §2, "survives untouched".)
theorem windowExtract_ftv {B : Type} {l : Label} :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') → τ.ftv ⊆ sFtv s ∧ sFtv s' ⊆ sFtv s
  | .field l' τ' :: t, τ, s', h => by
      simp only [windowExtract] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h
        cases h
        exact ⟨List.subset_append_left _ _, List.subset_append_right _ _⟩
      · rw [if_neg hl] at h
        revert h
        cases hw : windowExtract l t with
        | none => intro h; cases h
        | some p =>
            intro h
            obtain ⟨hτ, hs⟩ := windowExtract_ftv t hw
            cases h
            refine ⟨fun x hx => List.mem_append_right _ (hτ hx), fun x hx => ?_⟩
            simp only [sFtv, List.mem_append] at hx ⊢
            exact hx.imp id (fun hh => hs hh)

theorem removeField_ftv {B : Type} {l : Label} :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    removeField l s = some (τ, s') → τ.ftv ⊆ sFtv s ∧ sFtv s' ⊆ sFtv s
  | .var β :: t, τ, s', h => by
      simp only [removeField] at h
      revert h
      cases hw : removeField l t with
      | none => intro h; cases h
      | some p =>
          intro h
          obtain ⟨hτ, hs⟩ := removeField_ftv t hw
          cases h
          refine ⟨fun x hx => List.mem_cons_of_mem _ (hτ hx), fun x hx => ?_⟩
          simp only [sFtv, List.mem_cons] at hx ⊢
          exact hx.imp id (fun hh => hs hh)
  | .field l' τ' :: t, τ, s', h => by
      simp only [removeField] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h
        cases h
        exact ⟨List.subset_append_left _ _, List.subset_append_right _ _⟩
      · rw [if_neg hl] at h
        revert h
        cases hw : removeField l t with
        | none => intro h; cases h
        | some p =>
            intro h
            obtain ⟨hτ, hs⟩ := removeField_ftv t hw
            cases h
            refine ⟨fun x hx => List.mem_append_right _ (hτ hx), fun x hx => ?_⟩
            simp only [sFtv, List.mem_append] at hx ⊢
            exact hx.imp id (fun hh => hs hh)

-- ⊢  stripL cancels a shared head var: both residuals shrink
theorem stripL_ftv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripL s₁ s₂ = some (t₁, t₂)) : sFtv t₁ ⊆ sFtv s₁ ∧ sFtv t₂ ⊆ sFtv s₂ := by
  match s₁, s₂ with
  | .var α :: u₁, .var β :: u₂ =>
      simp only [stripL] at h
      by_cases hab : α = β
      · rw [if_pos hab] at h; cases h
        exact ⟨fun _ hx => List.mem_cons_of_mem _ hx,
               fun _ hx => List.mem_cons_of_mem _ hx⟩
      · rw [if_neg hab] at h; cases h

-- ⊢  matchL pairs a leading field with a window occurrence: the emitted types
--    and both residuals live inside the original problem
theorem matchL_ftv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    τ.ftv ⊆ sFtv s₁ ∧ sFtv t₁ ⊆ sFtv s₁ ∧ τ'.ftv ⊆ sFtv s₂ ∧ sFtv t₂ ⊆ sFtv s₂ := by
  match s₁ with
  | .field l σ :: u₁ =>
      simp only [matchL] at h
      revert h
      cases hw : windowExtract l s₂ with
      | none => intro h; cases h
      | some p =>
          intro h
          cases h
          obtain ⟨hτ, hs⟩ := windowExtract_ftv s₂ hw
          exact ⟨List.subset_append_left _ _, List.subset_append_right _ _, hτ, hs⟩

theorem groundMatchAux_ftv {B : Type} {s₁ s₂ : List (Atom B)} :
    (ls : List Label) → {τ τ' : Ty B} → {t₁ t₂ : List (Atom B)} →
    groundMatchAux s₁ s₂ ls = some (τ, τ', t₁, t₂) →
    τ.ftv ⊆ sFtv s₁ ∧ sFtv t₁ ⊆ sFtv s₁ ∧ τ'.ftv ⊆ sFtv s₂ ∧ sFtv t₂ ⊆ sFtv s₂
  | l :: ls, τ, τ', t₁, t₂, h => by
      simp only [groundMatchAux] at h
      by_cases hc : sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁
      · rw [if_pos hc] at h
        revert h
        cases h₁ : removeField l s₁ with
        | none => intro h; exact groundMatchAux_ftv ls (by simpa [h₁] using h)
        | some p₁ =>
            cases h₂ : removeField l s₂ with
            | none => intro h; exact groundMatchAux_ftv ls (by simpa [h₁, h₂] using h)
            | some p₂ =>
                intro h
                cases h
                obtain ⟨ha, hb⟩ := removeField_ftv s₁ h₁
                obtain ⟨hc', hd⟩ := removeField_ftv s₂ h₂
                exact ⟨ha, hb, hc', hd⟩
      · rw [if_neg hc] at h
        exact groundMatchAux_ftv ls h

theorem groundMatch_ftv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    τ.ftv ⊆ sFtv s₁ ∧ sFtv t₁ ⊆ sFtv s₁ ∧ τ'.ftv ⊆ sFtv s₂ ∧ sFtv t₂ ⊆ sFtv s₂ := by
  simp only [groundMatch] at h
  by_cases hv : sHasVar s₂
  · rw [if_pos hv] at h; cases h
  · rw [if_neg hv] at h; exact groundMatchAux_ftv _ h

-- The initial supply of a row problem, and the invariant it establishes.
def initSupply {B : Type} (ρ₁ ρ₂ : Row B) : Supply := ⟨lenBound (ρ₁.ftv ++ ρ₂.ftv) + 1⟩

-- The supply unifySpineM actually starts from does avoid its problem.
theorem localSupply_avoids {B : Type} (s₁ s₂ : List (Atom B)) :
    (localSupply s₁ s₂).Avoids (sFtv s₁ ++ sFtv s₂) := Nat.lt_succ_self _

theorem initSupply_avoids {B : Type} (ρ₁ ρ₂ : Row B) :
    (initSupply ρ₁ ρ₂).Avoids (ρ₁.ftv ++ ρ₂.ftv) := Nat.lt_succ_self _


------------------ P3: UNIQUE-HOST VARIABLE EXPANSION (§1.4) ------------------
-- The missing rule (proof-state.md 26-09-02, proof-plan.md §1.4): when a
-- leading field's label is absent from the OTHER side ENTIRELY and that side
-- has EXACTLY ONE variable, that variable is FORCED to host the field —
-- β ≔ (l:δ | β′), δ and β′ fresh. With two candidate hosts the rule must NOT
-- fire (that is Wand, where vars_vs_field_no_mgu proves there is genuinely no
-- mgu); refusing when the host is UNIQUE costs completeness for free, which is
-- exactly crossfield_stuck_unifiable (:952).
--
-- This section is the METATHEORY of the move: the host is forced (host_forced),
-- the algebraic shift the move performs (expand_shift), and the two reflection
-- lemmas. The dispatch cascade above is still the old one — wiring is next.

-- ## Projections of a var-free, l-free side vanish
-- ⊢  count_l(s) = 0   ⟹   proj_l(s) = []
theorem sProj_nil_of_count_zero {B : Type} (l : Label) :
    (s : List (Atom B)) → sFieldCount l s = 0 → sProj l s = []
  | [], _ => rfl
  | .var _ :: s, h => by
      simp only [sFieldCount] at h
      simp only [sProj, sProj_nil_of_count_zero l s h, List.map_nil]
  | .field l' _ :: s, h => by
      simp only [sFieldCount] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h; omega
      · rw [if_neg hl] at h
        simp only [sProj, if_neg hl]
        exact sProj_nil_of_count_zero l s (by omega)

theorem spineVarFree_applySubst {B : Type} (θ : TySubst B) :
    {ρ : Row B} → ρ.SpineVarFree → (ρ.applySubst θ).SpineVarFree
  | .empty, _ => .empty
  | .var _, h => nomatch h
  | .sing _ _, _ => .sing
  | .cat _ _, .cat h₁ h₂ =>
      .cat (spineVarFree_applySubst θ h₁) (spineVarFree_applySubst θ h₂)

-- ⊢  s var-free and l-free   ⟹   proj_l(θ(ofSpine s)) = []   and it adds no vars
theorem sProj_applySubst_nil {B : Type} {θ : TySubst B} (l : Label)
    (s : List (Atom B)) (hv : sVarSeq s = []) (hc : sFieldCount l s = 0) :
    sProj l ((ofSpine s).applySubst θ).toSpine = [] := by
  have hvf : (ofSpine s).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).2 (by rw [ofSpine_toSpine]; exact hv)
  refine sProj_nil_of_count_zero l _ ?_
  rw [sFieldCount_applySubst_varFree θ l hvf, ofSpine_toSpine]
  exact hc

theorem sVarSeq_applySubst_nil {B : Type} {θ : TySubst B}
    (s : List (Atom B)) (hv : sVarSeq s = []) :
    sVarSeq ((ofSpine s).applySubst θ).toSpine = [] :=
  (spineVarFree_iff_varSeq_nil _).1
    (spineVarFree_applySubst θ ((spineVarFree_iff_varSeq_nil _).2
      (by rw [ofSpine_toSpine]; exact hv)))

-- ## A one-variable spine splits around its variable
theorem sVarSeq_singleton_split {B : Type} {β : TyVar} :
    (s : List (Atom B)) → sVarSeq s = [β] →
    ∃ w v, s = w ++ .var β :: v ∧ sVarSeq w = [] ∧ sVarSeq v = []
  | .field l τ :: s, h => by
      simp only [sVarSeq] at h
      obtain ⟨w, v, rfl, hw, hv⟩ := sVarSeq_singleton_split s h
      exact ⟨.field l τ :: w, v, rfl, by simpa only [sVarSeq] using hw, hv⟩
  | .var γ :: s, h => by
      simp only [sVarSeq] at h
      injection h with hγ hs
      exact ⟨[], s, by rw [hγ]; rfl, rfl, hs⟩

-- ## THE HOST IS FORCED
-- All of the l-projection of the host side comes from the host variable: the
-- fields around it carry other labels, and there is no second variable to hide
-- an l-field behind.
-- ⊢  vars(s) = [β],  count_l(s) = 0   ⟹   proj_l(θ(ofSpine s)) ≈ₚ proj_l(θβ)
theorem host_proj {B : Type} {θ : TySubst B} {β : TyVar} {l : Label} :
    (s : List (Atom B)) → sVarSeq s = [β] → sFieldCount l s = 0 →
    ProjEquiv (sProj l ((ofSpine s).applySubst θ).toSpine)
              (sProj l (θ.row β).toSpine) := by
  intro s hv hc
  obtain ⟨w, v, rfl, hw, hvv⟩ := sVarSeq_singleton_split s hv
  rw [sFieldCount_append] at hc
  simp only [sFieldCount] at hc
  have hcw : sFieldCount l w = 0 := by omega
  have hcv : sFieldCount l v = 0 := by omega
  have hstep : RowEquiv ((ofSpine (w ++ .var β :: v)).applySubst θ)
      (.cat ((ofSpine w).applySubst θ) ((ofSpine (.var β :: v)).applySubst θ)) :=
    RowEquiv.applySubst θ (ofSpine_append w (.var β :: v))
  refine (hstep.char.2 l).trans (ProjEquiv.of_eq ?_)
  show sProj l (((ofSpine w).applySubst θ).toSpine ++
                ((ofSpine (.var β :: v)).applySubst θ).toSpine) = _
  rw [sProj_append, sProj_applySubst_nil l w hw hcw,
      sVarSeq_applySubst_nil w hw]
  show ([] : List (Nat × Ty B)) ++
      (sProj l ((Row.cat (θ.row β) ((ofSpine v).applySubst θ)).toSpine)).map
        (fun p => (p.1 + 0, p.2)) = _
  rw [show (Row.cat (θ.row β) ((ofSpine v).applySubst θ)).toSpine =
        (θ.row β).toSpine ++ ((ofSpine v).applySubst θ).toSpine from rfl,
      sProj_append, sProj_applySubst_nil l v hvv hcv]
  simp

-- ⊢  vars(s₂) = [β],  count_l(s₂) = 0,  θ ⊨ (l:τ | ofSpine t₁) ≐ᵣ ofSpine s₂
--        ⟹   ∃ σ ρ'.  θτ ≈ₜ σ  ∧  θβ ≈ᵣ (l:σ | ρ')
-- The host must expand with an l-field IN FRONT: index 0 says nothing precedes
-- it but fields, and those all carry other labels, so ≈-comm bubbles it out
-- (spine_extract, RowEquiv.lean:243). This is the maximality argument that
-- proof-state.md carries by hand for crossfield.
-- ⊢  proj_l ps ≈ₚ qs with ps starting at index 0   ⟹   so does qs
theorem ProjEquiv.head_zero {B : Type} {τ : Ty B} {ps qs : List (Nat × Ty B)}
    (h : ProjEquiv ((0, τ) :: ps) qs) :
    ∃ σ rest, qs = (0, σ) :: rest ∧ TyEquiv τ σ := by
  cases h with
  | cons hn hty _ => exact ⟨_, _, by rw [← hn], hty⟩

-- ⊢  vars(s₂) = [β],  count_l(s₂) = 0,  θ ⊨ (l:τ | ofSpine t₁) ≐ᵣ ofSpine s₂
--        ⟹   ∃ σ ρ'.  θτ ≈ₜ σ  ∧  θβ ≈ᵣ (l:σ | ρ')
-- The host must expand with an l-field IN FRONT: index 0 says nothing precedes
-- it but fields, and those all carry other labels, so ≈-comm bubbles it out
-- (spine_extract, RowEquiv.lean:243). This is the maximality argument that
-- proof-state.md currently carries BY HAND for crossfield.
theorem host_forced {B : Type} {θ : TySubst B} {β : TyVar} {l : Label} {τ : Ty B}
    {t₁ s₂ : List (Atom B)} (hv : sVarSeq s₂ = [β]) (hc : sFieldCount l s₂ = 0)
    (hu : Unifies θ (ofSpine (.field l τ :: t₁)) (ofSpine s₂)) :
    ∃ (σ : Ty B) (ρ' : Row B),
      TyEquiv (τ.applySubst θ) σ ∧ RowEquiv (θ.row β) (.cat (.sing l σ) ρ') := by
  have hL : sProj l ((ofSpine (.field l τ :: t₁)).applySubst θ).toSpine =
      (0, τ.applySubst θ) :: sProj l ((ofSpine t₁).applySubst θ).toSpine := by
    show sProj l (((Row.sing l τ).applySubst θ).toSpine ++
                  ((ofSpine t₁).applySubst θ).toSpine) = _
    rw [sProj_append]
    simp [Row.applySubst, Row.toSpine, sProj, sVarSeq]
  have hp := ((RowEquiv.char hu).2 l).trans (host_proj s₂ hv hc)
  rw [hL] at hp
  obtain ⟨σ, rest, hq, hty⟩ := ProjEquiv.head_zero hp
  obtain ⟨t, hequiv, -, -, -⟩ := spine_extract (θ.row β).toSpine l hq
  exact ⟨σ, ofSpine t, hty, (Row.toSpine_equiv _).trans hequiv⟩

-- ## The move, algebraically
-- Renaming the host to its fresh tail — the only edit the move makes to the
-- host side; the invented l-field is consumed at once by the pairing, which is
-- why the move costs exactly ONE atom (expandL_len) and the existing fuel
-- bound |s₁|+|s₂| survives.
theorem renameVar_length {B : Type} (β β' : TyVar) :
    (s : List (Atom B)) → (renameVar β β' s).length = s.length
  | [] => rfl
  | .var _ :: s => congrArg (· + 1) (renameVar_length β β' s)
  | .field _ _ :: s => congrArg (· + 1) (renameVar_length β β' s)

theorem renameVar_varFree {B : Type} (β β' : TyVar) :
    (s : List (Atom B)) → sVarSeq s = [] → renameVar β β' s = s
  | [], _ => rfl
  | .var _ :: _, h => nomatch h
  | .field _ _ :: s, h =>
      congrArg _ (renameVar_varFree β β' s (by simpa only [sVarSeq] using h))

-- THE SHIFT. Under a host expansion the whole side factors as "the invented
-- field, then the side with the host renamed": the fields around the host all
-- carry other labels, so ≈-comm walks the field out to the front.
-- ⊢  vars(s)=[β], count_l(s)=0, θβ ≈ (l:σ | θβ′)
--        ⟹  θ(ofSpine s) ≈ᵣ (l:σ | θ(ofSpine s[β↦β′]))
theorem expand_shift {B : Type} {θ : TySubst B} {l : Label} {σ : Ty B}
    {β β' : TyVar} (hβ : RowEquiv (θ.row β) (.cat (.sing l σ) (θ.row β'))) :
    (s : List (Atom B)) → sVarSeq s = [β] → sFieldCount l s = 0 →
    RowEquiv ((ofSpine s).applySubst θ)
             (.cat (.sing l σ) ((ofSpine (renameVar β β' s)).applySubst θ))
  | .var γ :: s, hv, hc => by
      simp only [sVarSeq] at hv
      injection hv with hγ hs
      rw [← hγ] at hβ
      show RowEquiv (.cat (θ.row γ) ((ofSpine s).applySubst θ)) _
      rw [renameVar, if_pos hγ, renameVar_varFree β β' s hs]
      exact (RowEquiv.cat hβ (.refl _)).trans RowEquiv.assoc
  | .field l' τ :: s, hv, hc => by
      simp only [sVarSeq] at hv
      simp only [sFieldCount] at hc
      have hl : ¬ l' = l := by intro hh; rw [if_pos hh] at hc; omega
      rw [if_neg hl] at hc
      have ih := expand_shift hβ s hv (by omega)
      show RowEquiv (.cat (.sing l' (τ.applySubst θ)) ((ofSpine s).applySubst θ)) _
      rw [renameVar]
      exact ((RowEquiv.cat (.refl _) ih).trans RowEquiv.assoc.symm).trans
        ((RowEquiv.cat (RowEquiv.comm hl) (.refl _)).trans RowEquiv.assoc)

-- ## Reflection, both directions
-- BACKWARD (soundness): a θ that meets the emitted binding and equation and
-- unifies the residual unified the original.
theorem expand_reflect {B : Type} {θ : TySubst B} {l : Label} {τ δ : Ty B}
    {β β' : TyVar} {t₁ s₂ : List (Atom B)}
    (hv : sVarSeq s₂ = [β]) (hc : sFieldCount l s₂ = 0)
    (hβ : RowEquiv (θ.row β) (.cat (.sing l δ) (θ.row β')))
    (hty : TyEquiv (τ.applySubst θ) δ)
    (hrec : Unifies θ (ofSpine t₁) (ofSpine (renameVar β β' s₂))) :
    Unifies θ (ofSpine (.field l τ :: t₁)) (ofSpine s₂) := by
  show RowEquiv (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) _
  exact (RowEquiv.cat (RowEquiv.sing hty) hrec).trans (expand_shift hβ s₂ hv hc).symm

theorem mem_sFtv_of_mem_sVarSeq {B : Type} {α : TyVar} :
    (s : List (Atom B)) → α ∈ sVarSeq s → α ∈ sFtv s
  | .var γ :: s, h => by
      simp only [sVarSeq, List.mem_cons] at h
      rcases h with rfl | h
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (mem_sFtv_of_mem_sVarSeq s h)
  | .field _ _ :: s, h => by
      simp only [sVarSeq] at h
      exact List.mem_append_right _ (mem_sFtv_of_mem_sVarSeq s h)

-- FORWARD (completeness): a unifier of the original EXTENDS to one that meets
-- the binding and the emitted equation and unifies the residual. The extension
-- only touches δ and β′, which are FRESH — that is where P2 is consumed, and it
-- is why the move does not shrink the unifier set (unlike matchL/groundMatch,
-- proof-plan.md §2 CAVEAT).
theorem expand_reflect_fwd {B : Type} {θ : TySubst B} {l : Label} {τ : Ty B}
    {β dv β' : TyVar} {t₁ s₂ : List (Atom B)}
    (hv : sVarSeq s₂ = [β]) (hc : sFieldCount l s₂ = 0)
    (hd₁ : dv ∉ sFtv (Atom.field l τ :: t₁)) (hd₂ : dv ∉ sFtv s₂)
    (hb₁ : β' ∉ sFtv (Atom.field l τ :: t₁)) (hb₂ : β' ∉ sFtv s₂)
    (hu : Unifies θ (ofSpine (.field l τ :: t₁)) (ofSpine s₂)) :
    ∃ θ' : TySubst B,
      RowEquiv (θ'.row β) (.cat (.sing l (θ'.ty dv)) (θ'.row β')) ∧
      TyEquiv (τ.applySubst θ') (θ'.ty dv) ∧
      Unifies θ' (ofSpine t₁) (ofSpine (renameVar β β' s₂)) ∧
      Unifies θ' (ofSpine (.field l τ :: t₁)) (ofSpine s₂) ∧
      (∀ γ, γ ≠ dv → γ ≠ β' → θ.ty γ = θ'.ty γ ∧ θ.row γ = θ'.row γ) := by
  obtain ⟨σ, ρ', hty, hβ⟩ := host_forced hv hc hu
  have hββ' : β ≠ β' := fun h =>
    hb₂ (h ▸ mem_sFtv_of_mem_sVarSeq s₂ (by rw [hv]; exact List.mem_cons_self))
  have hdv : ((θ.setTy dv σ).setRow β' ρ').ty dv = σ := by
    show (if dv = dv then σ else θ.ty dv) = σ
    rw [if_pos rfl]
  have hβ'' : RowEquiv (((θ.setTy dv σ).setRow β' ρ').row β)
      (.cat (.sing l σ) (((θ.setTy dv σ).setRow β' ρ').row β')) := by
    show RowEquiv (if β = β' then ρ' else θ.row β)
      (.cat (.sing l σ) (if β' = β' then ρ' else θ.row β'))
    rw [if_neg hββ', if_pos rfl]
    exact hβ
  have hu' : Unifies ((θ.setTy dv σ).setRow β' ρ')
      (ofSpine (.field l τ :: t₁)) (ofSpine s₂) := by
    unfold Unifies
    rw [Row.applySubst_setRow_of_not_mem _ (by rw [← sFtv_ofSpine]; exact hb₁),
        Row.applySubst_setTy_of_not_mem _ (by rw [← sFtv_ofSpine]; exact hd₁),
        Row.applySubst_setRow_of_not_mem _ (by rw [← sFtv_ofSpine]; exact hb₂),
        Row.applySubst_setTy_of_not_mem _ (by rw [← sFtv_ofSpine]; exact hd₂)]
    exact hu
  refine ⟨(θ.setTy dv σ).setRow β' ρ', by rw [hdv]; exact hβ'', ?_, ?_, hu', ?_⟩
  · have hd : dv ∉ τ.ftv := fun h => hd₁ (List.mem_append_left _ h)
    have hb : β' ∉ τ.ftv := fun h => hb₁ (List.mem_append_left _ h)
    rw [hdv, Ty.applySubst_setRow_of_not_mem τ hb,
        Ty.applySubst_setTy_of_not_mem τ hd]
    exact hty
  · have key : RowEquiv
        (.cat (.sing l (τ.applySubst ((θ.setTy dv σ).setRow β' ρ')))
              ((ofSpine t₁).applySubst ((θ.setTy dv σ).setRow β' ρ')))
        (.cat (.sing l σ)
              ((ofSpine (renameVar β β' s₂)).applySubst ((θ.setTy dv σ).setRow β' ρ'))) :=
      hu'.trans (expand_shift hβ'' s₂ hv hc)
    exact key.field_cancel_left.2
  · intro γ hγd hγb
    exact ⟨by simp only [TySubst.setRow, TySubst.setTy, if_neg hγd],
           by simp only [TySubst.setRow, TySubst.setTy, if_neg hγb]⟩

-- ## The detectors
-- U-expand fires only when the host is UNIQUE: exactly one variable on the
-- other side, and no l-field anywhere on it (an l-field further right could
-- host the pairing instead — (l:𝓪 | α) ≐ᵣ (β | l:𝓫) is unifiable with β ≔ ε).
theorem uniqueHost_spec {B : Type} {l : Label} {s : List (Atom B)} {β : TyVar}
    (h : uniqueHost l s = some β) : sVarSeq s = [β] ∧ sFieldCount l s = 0 := by
  unfold uniqueHost at h
  cases hvs : sVarSeq s with
  | nil => rw [hvs] at h; cases h
  | cons γ t =>
      cases t with
      | cons _ _ => rw [hvs] at h; cases h
      | nil =>
          rw [hvs] at h
          replace h : (if sFieldCount l s = 0 then some γ else none) = some β := h
          by_cases hcc : sFieldCount l s = 0
          · rw [if_pos hcc] at h
            injection h with hg
            subst hg
            exact ⟨rfl, hcc⟩
          · rw [if_neg hcc] at h; cases h

-- Left end: the leading field of s₁ against the unique host of s₂. δ and β′ are
-- drawn from the supply; the host side keeps its length (the invented field is
-- consumed at once), so the move costs exactly ONE atom.
-- … and at the right end (the expansion is then β ≔ (β′ | l:δ)).
-- ## The payoff on crossfield
-- proof-state.md carries the maximality half of crossfield's mgu BY HAND
-- ("proj_m A = (0,𝓫)::proj_m B … ⟹ A ≈ (m:𝓫|R), B ≈ (l:𝓫|R)"). host_forced
-- mechanizes it: every unifier of (l:𝓫 | α) ≐ᵣ (m:𝓫 | β) puts an l-field at the
-- FRONT of β — which is exactly the binding expandL emits.
-- ⊢  l ≠ m,  θ ⊨ (l:𝓫 | α) ≐ᵣ (m:𝓫 | β)   ⟹   ∃ ρ'. θβ ≈ᵣ (l:𝓫 | ρ')
theorem crossfield_host_forced {B : Type} (b : B) {l m : Label} (hne : l ≠ m)
    {θ : TySubst B} {α β : TyVar}
    (hu : Unifies θ (.cat (.sing l (.base b)) (.var α))
                    (.cat (.sing m (.base b)) (.var β))) :
    ∃ ρ', RowEquiv (θ.row β) (.cat (.sing l (.base b)) ρ') := by
  have hu' : Unifies θ (ofSpine [.field l (.base b), .var α])
                       (ofSpine [.field m (.base b), .var β]) := by
    unfold Unifies at hu ⊢
    exact (RowEquiv.applySubst θ (Row.toSpine_equiv _)).symm.trans
      (hu.trans (RowEquiv.applySubst θ (Row.toSpine_equiv _)))
  have hc : sFieldCount l [Atom.field m (.base b), .var β] = 0 := by
    simp only [sFieldCount, if_neg (fun h : m = l => hne h.symm)]
  obtain ⟨σ, ρ', hty, hβ⟩ := host_forced (t₁ := [Atom.var α]) rfl hc hu'
  exact ⟨ρ', hβ.trans (RowEquiv.cat (RowEquiv.sing hty.symm) (.refl _))⟩

-- THE FUEL OBSERVATION (contra proof-plan.md §1.3, which expects the bound to
-- die): fusing the expansion with the pairing it enables costs ONE atom, so the
-- existing bound |s₁| + |s₂| still works. Only P4's solve-and-apply grows a
-- spine, so the lexicographic measure is needed there, not here.
-- ⊢  expandL S s₁ s₂ = some (β,l,τ,t₁,t₂)  ⟹  |t₁|+|t₂| + 1 = |s₁|+|s₂|
theorem expandL_len {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (h : expandL S s₁ s₂ = some (β, l, τ, t₁, t₂)) :
    t₁.length + t₂.length + 1 = s₁.length + s₂.length := by
  match s₁ with
  | .field l' τ' :: u₁ =>
      simp only [expandL] at h
      revert h
      cases hh : uniqueHost l' s₂ with
      | none => intro h; cases h
      | some γ =>
          intro h
          cases h
          simp only [List.length_cons, renameVar_length]
          omega

-- ⊢  the detector's side conditions are exactly the reflection lemmas'
theorem expandL_spec {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (h : expandL S s₁ s₂ = some (β, l, τ, t₁, t₂)) :
    s₁ = .field l τ :: t₁ ∧ sVarSeq s₂ = [β] ∧ sFieldCount l s₂ = 0 ∧
    t₂ = renameVar β S.fresh.2.fresh.1 s₂ := by
  match s₁ with
  | .field l' τ' :: u₁ =>
      simp only [expandL] at h
      revert h
      cases hh : uniqueHost l' s₂ with
      | none => intro h; cases h
      | some γ =>
          intro h
          cases h
          obtain ⟨hv, hc⟩ := uniqueHost_spec hh
          exact ⟨rfl, hv, hc, rfl⟩

-- ## The freshness INVARIANT along the recursion
-- The three FORWARD legs (clash, completeness, stuck) extend a unifier at δ and
-- β′, so they need those names to be fresh for the problem — i.e. the invariant
-- `S.Avoids (sFtv s₁ ++ sFtv s₂)` at every recursive call. (The backward leg,
-- success soundness, needs none of this: it only reads a binding off SolSat.)
theorem Supply.Avoids.swap {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    (h : S.Avoids (sFtv s₁ ++ sFtv s₂)) : S.Avoids (sFtv s₂ ++ sFtv s₁) :=
  h.mono (fun {x} hx => by
    rcases List.mem_append.mp hx with hh | hh
    · exact List.mem_append_right _ hh
    · exact List.mem_append_left _ hh)

-- Every non-expanding move shrinks the problem, so the invariant just descends.
theorem Supply.Avoids.residual {B : Type} {S : Supply} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hS : S.Avoids (sFtv s₁ ++ sFtv s₂))
    (h₁ : sFtv t₁ ⊆ sFtv s₁) (h₂ : sFtv t₂ ⊆ sFtv s₂) :
    S.Avoids (sFtv t₁ ++ sFtv t₂) :=
  hS.mono (fun {x} hx => by
    rcases List.mem_append.mp hx with hh | hh
    · exact List.mem_append_left _ (h₁ hh)
    · exact List.mem_append_right _ (h₂ hh))

-- The right-end moves, via reverse.
theorem stripR_ftv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripR s₁ s₂ = some (t₁, t₂)) : sFtv t₁ ⊆ sFtv s₁ ∧ sFtv t₂ ⊆ sFtv s₂ := by
  unfold stripR at h
  revert h
  cases hl : stripL s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      obtain ⟨g₁, g₂⟩ := stripL_ftv hl
      exact ⟨fun _ hx => (mem_sFtv_reverse s₁).mp (g₁ ((mem_sFtv_reverse u₁).mp hx)),
             fun _ hx => (mem_sFtv_reverse s₂).mp (g₂ ((mem_sFtv_reverse u₂).mp hx))⟩

theorem matchR_ftv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    τ.ftv ⊆ sFtv s₁ ∧ sFtv t₁ ⊆ sFtv s₁ ∧ τ'.ftv ⊆ sFtv s₂ ∧ sFtv t₂ ⊆ sFtv s₂ := by
  unfold matchR at h
  revert h
  cases hl : matchL s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨σ0, σ0', u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl, rfl⟩ := h
      obtain ⟨g₀, g₁, g₀', g₂⟩ := matchL_ftv hl
      exact ⟨fun _ hx => (mem_sFtv_reverse s₁).mp (g₀ hx),
             fun _ hx => (mem_sFtv_reverse s₁).mp (g₁ ((mem_sFtv_reverse u₁).mp hx)),
             fun _ hx => (mem_sFtv_reverse s₂).mp (g₀' hx),
             fun _ hx => (mem_sFtv_reverse s₂).mp (g₂ ((mem_sFtv_reverse u₂).mp hx))⟩

-- Renaming the host introduces exactly one new name: the fresh tail.
theorem sFtv_renameVar {B : Type} (β β' : TyVar) :
    (s : List (Atom B)) → ∀ x, x ∈ sFtv (renameVar β β' s) → x ∈ β' :: sFtv s
  | [], _, hx => nomatch hx
  | .var γ :: s, x, hx => by
      simp only [renameVar] at hx
      by_cases hg : γ = β
      · rw [if_pos hg] at hx
        simp only [sFtv, List.mem_cons] at hx ⊢
        rcases hx with rfl | hx
        · exact .inl rfl
        · rcases List.mem_cons.mp (sFtv_renameVar β β' s x hx) with h' | h'
          · exact .inl h'
          · exact .inr (.inr h')
      · rw [if_neg hg] at hx
        simp only [sFtv, List.mem_cons] at hx ⊢
        rcases hx with rfl | hx
        · exact .inr (.inl rfl)
        · rcases List.mem_cons.mp (sFtv_renameVar β β' s x hx) with h' | h'
          · exact .inl h'
          · exact .inr (.inr h')
  | .field _ τ :: s, x, hx => by
      simp only [renameVar, sFtv, List.mem_append] at hx
      simp only [sFtv, List.mem_cons, List.mem_append]
      rcases hx with hx | hx
      · exact .inr (.inl hx)
      · rcases List.mem_cons.mp (sFtv_renameVar β β' s x hx) with h' | h'
        · exact .inl h'
        · exact .inr (.inr h')

-- … so the advanced supply still avoids the residual: β′ is shorter than every
-- name it can still hand out, and everything else came from the problem.
theorem expandL_avoids {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (hS : S.Avoids (sFtv s₁ ++ sFtv s₂))
    (h : expandL S s₁ s₂ = some (β, l, τ, t₁, t₂)) :
    S.fresh.2.fresh.2.Avoids (sFtv t₁ ++ sFtv t₂) := by
  obtain ⟨hs1, -, -, hren⟩ := expandL_spec h
  have hsub : sFtv t₁ ++ sFtv t₂ ⊆ S.fresh.2.fresh.1 :: (sFtv s₁ ++ sFtv s₂) := by
    intro x hx
    rcases List.mem_append.mp hx with hh | hh
    · refine List.mem_cons_of_mem _ (List.mem_append_left _ ?_)
      rw [hs1]
      exact List.mem_append_right _ hh
    · rw [hren] at hh
      rcases List.mem_cons.mp (sFtv_renameVar _ _ s₂ x hh) with rfl | hh'
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (List.mem_append_right _ hh')
  refine Supply.Avoids.mono hsub ?_
  exact hS.advance.cons_fresh

-- FORWARD REFLECTION for the arm: a unifier of the original yields one of the
-- residual (a different substitution — it fixes δ and β′, which the original
-- problem does not mention).
theorem expandL_reflect_fwd {B : Type} {S : Supply} {θ : TySubst B}
    {s₁ s₂ : List (Atom B)} {β : TyVar} {l : Label} {τ : Ty B}
    {t₁ t₂ : List (Atom B)}
    (hS : S.Avoids (sFtv s₁ ++ sFtv s₂))
    (h : expandL S s₁ s₂ = some (β, l, τ, t₁, t₂))
    (hu : Unifies θ (ofSpine s₁) (ofSpine s₂)) :
    ∃ θ' : TySubst B, Unifies θ' (ofSpine t₁) (ofSpine t₂) := by
  obtain ⟨hs1, hv, hc, hren⟩ := expandL_spec h
  have hd := Supply.fresh_not_mem hS
  have hb := Supply.fresh_not_mem hS.advance
  rw [List.mem_append] at hd hb
  rw [hs1] at hu
  obtain ⟨θ', -, -, hrec, -, -⟩ :=
    expand_reflect_fwd hv hc
      (fun hm => hd (.inl (hs1 ▸ hm))) (fun hm => hd (.inr hm))
      (fun hm => hb (.inl (hs1 ▸ hm))) (fun hm => hb (.inr hm)) hu
  exact ⟨θ', by rw [hren]; exact hrec⟩

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

------------------------- AGREEMENT: mgu MODULO FRESH NAMES -----------------
-- The vocabulary ≐ᵣ completeness needs once the algorithm can INVENT variables
-- (proof-plan.md §4-P3b(2)); consumed by P5's completeness and clash legs and by
-- the boundedness invariant they rest on.

-- ## Agreement: the completeness statement must allow EXTENSION
-- U-expand invents δ and β′, so a unifier of the problem cannot literally meet
-- the emitted solution — it says nothing about names the problem never had. The
-- honest mgu statement is "every unifier EXTENDS to one that meets σ and eqs,
-- without moving on the problem's own variables". `V` is any variable set the
-- problem lives inside; it is what the extension promises to leave alone.
def AgreeOn {B : Type} (θ θ' : TySubst B) (V : List TyVar) : Prop :=
  ∀ α ∈ V, θ.ty α = θ'.ty α ∧ θ.row α = θ'.row α

theorem AgreeOn.refl {B : Type} (θ : TySubst B) (V : List TyVar) : AgreeOn θ θ V :=
  fun _ _ => ⟨rfl, rfl⟩

theorem AgreeOn.trans' {B : Type} {θ₁ θ₂ θ₃ : TySubst B} {V W : List TyVar}
    (h₁ : AgreeOn θ₁ θ₂ V) (h₂ : AgreeOn θ₂ θ₃ W) (hVW : V ⊆ W) : AgreeOn θ₁ θ₃ V :=
  fun α hα => ⟨(h₁ α hα).1.trans (h₂ α (hVW hα)).1,
               (h₁ α hα).2.trans (h₂ α (hVW hα)).2⟩

theorem AgreeOn.tyEq {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {τ : Ty B} (hsub : τ.ftv ⊆ V) :
    τ.applySubst θ = τ.applySubst θ' :=
  Ty.applySubst_congr τ (fun α hα => h α (hsub hα))

-- The problem's variables sit inside V; every residual's do too.
theorem sFtv_sub_left {B : Type} {s₁ s₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) : sFtv s₁ ⊆ V :=
  fun _ hx => hV (List.mem_append_left _ hx)

theorem sFtv_sub_right {B : Type} {s₁ s₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) : sFtv s₂ ⊆ V :=
  fun _ hx => hV (List.mem_append_right _ hx)

theorem sFtv_sub_residual {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) (h₁ : sFtv t₁ ⊆ sFtv s₁) (h₂ : sFtv t₂ ⊆ sFtv s₂) :
    (sFtv t₁ ++ sFtv t₂) ⊆ V := fun _ hx => by
  rcases List.mem_append.mp hx with hh | hh
  · exact sFtv_sub_left hV (h₁ hh)
  · exact sFtv_sub_right hV (h₂ hh)

theorem sFtv_sub_swap {B : Type} {s₁ s₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) : (sFtv s₂ ++ sFtv s₁) ⊆ V := fun _ hx => by
  rcases List.mem_append.mp hx with hh | hh
  · exact sFtv_sub_right hV hh
  · exact sFtv_sub_left hV hh

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

--------------------- P4: THE MUTUAL ≐ / ≐ᵣ DRIVER ---------------------------
-- proof-plan.md §1.2 / §4-P4. The row pass no longer PARKS the type equations
-- it discovers: it solves them, on the spot, by calling the type pass, and
-- applies the solution to the residual before recursing. That is the whole of
-- P4, and it is what makes the stuck verdict mean something (§3): an equation
-- is discharged, or fatal, or itself stuck — never merely deferred.
--
-- Three deviations from §1.2, all recorded in §4-P4:
--  * a success carries its SUPPLY, because a type equation solved inside a
--    field may expand a row variable, and the invented tail travels into the
--    residual; without threading, the residual call would re-draw that name;
--  * `outOfFuel` is a separate verdict, so the fuel lemma is a structural
--    induction ("more fuel never changes a verdict that was reached") rather
--    than the termination measure §1.3 asked for — see the note on unifyBound;
--  * `[DecidableEq B]`: ≐ must decide 𝓫 = 𝓫′. The row pass never needed it.
--
-- This is now THE algorithm: the single-pass URes driver and its four legs were
-- deleted once P5/P6 had ported them (proof-plan.md §4-P5/P6).

-- ## Binding a type variable, occurs-checked
-- α ≐ α is vacuous; otherwise α ≔ τ, guarded. ftv spans BOTH sorts
-- (minimal.lean:691), so `α ≐ {… α …}` is rejected even when the inner α is a
-- row variable and the problem is in fact solvable — the same conservatism the
-- row occurs guard has (occurs_allVar_hasMgu), and §1.3 keeps it deliberately:
-- the guard is what makes a binding eliminate its variable.
def tyIsVar {B : Type} : Ty B → Option TyVar
  | .var β => some β
  | _      => none

theorem tyIsVar_eq {B : Type} : {τ : Ty B} → {α : TyVar} → tyIsVar τ = some α → τ = .var α
  | .var _, _, h => by simp only [tyIsVar, Option.some.injEq] at h; rw [h]
  | .base _, _, h => by simp [tyIsVar] at h
  | .unk, _, h => by simp [tyIsVar] at h
  | .fn _ _, _, h => by simp [tyIsVar] at h
  | .rcd _, _, h => by simp [tyIsVar] at h

-- Written as a chain of `if`s rather than a match on τ, so it has ONE
-- unconditional equation and its soundness/completeness proofs never case-split
-- on the shape of τ.
def bindTy {B : Type} (S : Supply) (α : TyVar) (τ : Ty B) : UResM B :=
  if tyIsVar τ = some α then .success .nil S
  else if τ.ftv.contains α then .occurs
  else .success ⟨[(α, τ)], []⟩ S

-- ## U-var-solve, at the mutual driver's result type
-- Same detector as solveVar (:97); solveVar's successes always carry eqs = [],
-- so nothing is lost by dropping that component here.
def solveVarM {B : Type} (S : Supply) : List (Atom B) → List (Atom B) → Option (UResM B)
  | [.var α], s₂ =>
      some (if (sVarSeq s₂).contains α then .occurs
            else .success (Sol.ofRow [(α, ofSpine s₂)]) S)
  | _, _ => none

-- ## U-expand, at the mutual driver's result type
-- expandRes (:217) parked the equation τ ≐ δ. Here δ is FRESH, so that
-- equation has the one solution δ ≔ τ — solving it eagerly is exactly what the
-- driver does everywhere else, and it keeps P3's metatheory (which invents δ)
-- applicable verbatim. The recursive solution is composed ON TOP, so the
-- expansion's own binding sees whatever the residual did to β′.
def expandResM {B : Type} (S : Supply) (β : TyVar) (l : Label) (τ : Ty B) :
    UResM B → UResM B
  | .success s S' =>
      .success (s.comp ⟨[(S.fresh.1, τ)],
                        [(β, .cat (.sing l (.var S.fresh.1)) (.var S.fresh.2.fresh.1))]⟩) S'
  | r => r

-- ## The driver
-- unifyTyF is ≐; unifySpineMF is ≐ᵣ. Both consume one unit of fuel per
-- cross-call, so the block is STRUCTURALLY recursive on fuel — which is what
-- keeps the regressions kernel-checked `rfl` executions (§1.3).
mutual

def unifyTyF {B : Type} [DecidableEq B] (S : Supply) (fuel : Nat) : Ty B → Ty B → UResM B
  -- The fuel is consumed in the two RECURSIVE arms only, so the match on it
  -- sits inside: every other verdict is reached at any fuel, and the fuel
  -- lemma below then has exactly two interesting cases.
  | .var α, τ₂ => bindTy S α τ₂
  | τ₁, .var α => bindTy S α τ₁
  -- ★ is RIGID (§5): it unifies with itself and clashes with everything else
  | .unk, .unk => .success .nil S
  | .base b, .base b' => if b = b' then .success .nil S else .clash
  | .fn a₁ b₁, .fn a₂ b₂ =>
      match fuel with
      | 0 => .outOfFuel
      | f+1 =>
          (unifyTyF S f a₁ a₂).seq fun θ S' =>
            unifyTyF S' f (b₁.applySubst θ) (b₂.applySubst θ)
  | .rcd ρ₁, .rcd ρ₂ =>
      match fuel with
      | 0 => .outOfFuel
      | f+1 => unifySpineMF S f ρ₁.toSpine ρ₂.toSpine
  | _, _ => .clash

def unifySpineMF {B : Type} [DecidableEq B] :
    Supply → Nat → List (Atom B) → List (Atom B) → UResM B
  | S, _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => .success (Sol.ofRow σ) S
      | none   => .clash
  | S, _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => .success (Sol.ofRow σ) S
      | none   => .clash
  | _, 0, _, _ => .outOfFuel
  | S, fuel+1, s₁, s₂ =>
      match stripL s₁ s₂ with
      | some (t₁, t₂) => unifySpineMF S fuel t₁ t₂
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => unifySpineMF S fuel t₁ t₂
      | none =>
      match solveVarM S s₁ s₂ with
      | some r => r
      | none =>
      match solveVarM S s₂ s₁ with
      | some r => r
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match expandL S s₁ s₂ with
      | some (β, l, τ, t₁, t₂) =>
          expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂)
      | none =>
      match expandL S s₂ s₁ with
      | some (β, l, τ, t₁, t₂) =>
          expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂)
      | none =>
      if projClash s₁ s₂ then .clash else .stuck

end

-- ## Entry points
-- Fuel stays EXPLICIT at the top level. §1.3 wanted a closed-form bound
-- realizing a lexicographic measure; solve-and-apply defeats that (see the
-- note below unifyM_fuel_mono), and it is not needed: `outOfFuel` makes every
-- reached verdict fuel-independent, and only the stuck leg (P6) has to know
-- that enough fuel exists.
def unifySpineM {B : Type} [DecidableEq B] (fuel : Nat) (s₁ s₂ : List (Atom B)) : UResM B :=
  unifySpineMF (localSupply s₁ s₂) fuel s₁ s₂

def unifyRowM {B : Type} [DecidableEq B] (fuel : Nat) (ρ₁ ρ₂ : Row B) : UResM B :=
  unifySpineM fuel ρ₁.toSpine ρ₂.toSpine

def unifyTyM {B : Type} [DecidableEq B] (fuel : Nat) (τ τ' : Ty B) : UResM B :=
  unifyTyF ⟨lenBound (τ.ftv ++ τ'.ftv) + 1⟩ fuel τ τ'



-- ## Two worked verdicts, kernel-checked
-- The computational halves of the two incompleteness discussions above; they
-- live here rather than in Regressions.lean because the prose that reads them
-- is here.

-- The occurs guard is CONSERVATIVE: it rejects α ≐ᵣ (β | α | γ), which
-- occurs_allVar_unifiable (:935) shows is unifiable and occurs_allVar_hasMgu
-- (:956) shows has an MGU. Deliberate, and the price §1.3 accepts.
-- ⊢  unifyRowM α (β | α | γ)  =  occurs
theorem occurs_allVar_reported {B : Type} [DecidableEq B] :
    unifyRowM (B := B) 20 (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c")))
      = .occurs := rfl

-- U-EXPAND'S PAYOFF, computed. The verdict crossfield used to get was `.stuck`,
-- and it was WRONG (the prose at :1006 derives the mgu by hand). The driver now
-- finds exactly that mgu, with the equation 𝓫 ≐ δ SOLVED rather than parked.
-- ⊢  unifyRowM (l:𝓫 | α) (m:𝓫 | β)
--      =  success [δ ≔ 𝓫] [β ≔ (l:δ | β′), α ≔ (m:𝓫 | β′ | ε)]
theorem crossfield_success {B : Type} [DecidableEq B] (b : B) :
    unifyRowM (B := B) 20 (.cat (.sing "l" (.base b)) (.var "a"))
                          (.cat (.sing "m" (.base b)) (.var "b")) =
      .success ⟨[(natName 2, .base b)],
                [("b", .cat (.sing "l" (.var (natName 2))) (.var (natName 3))),
                 ("a", .cat (.sing "m" (.base b)) (.cat (.var (natName 3)) .empty))]⟩
               ⟨4⟩ := rfl

-- ## Fuel monotonicity
-- §1.3 asked for a closed-form BOUND realizing a lexicographic measure. That
-- is not what P4 delivers, and the reason is structural: solve-and-apply grows
-- the spine (a variable expands to a whole row), while the number of variables
-- can grow too, since a type equation solved inside a field may itself expand a
-- row variable and hand the invented tail to the residual. Neither component of
-- the §1.3 measure decreases, so no such bound is available yet — the missing
-- ingredient is a Rémy-style argument on the finitely many labels of the
-- problem. See §4-P4.
--
-- `outOfFuel` makes that a separable question. The lemma below says a verdict
-- that was REACHED never changes when the budget grows, which is all any leg
-- except the stuck one needs; only P6 has to know that a sufficient budget
-- exists at all.

/-- `Mono r r'`: `r` is what the algorithm answered on some budget and `r'` on a
larger one — either the smaller run ran out, or the two agree. -/
def UResM.Mono {B : Type} (r r' : UResM B) : Prop := r = .outOfFuel ∨ r' = r

theorem UResM.Mono.rfl' {B : Type} (r : UResM B) : UResM.Mono r r := .inr rfl

-- ⊢  Mono is compatible with sequencing, ARM-WISE: no side condition survives
theorem UResM.Mono.seq {B : Type} {r r' : UResM B}
    {k k' : TySubst B → Supply → UResM B}
    (hr : UResM.Mono r r') (hk : ∀ θ S, UResM.Mono (k θ S) (k' θ S)) :
    UResM.Mono (r.seq k) (r'.seq k') := by
  rcases hr with h | h
  · subst h; exact .inl rfl
  · subst h
    cases r' with
    | success s S =>
        rcases hk s.toSubst S with hh | hh
        · exact .inl (by simp only [UResM.seq, hh])
        · exact .inr (by simp only [UResM.seq, hh])
    | clash     => exact .inr rfl
    | occurs    => exact .inr rfl
    | stuck     => exact .inr rfl
    | outOfFuel => exact .inl rfl

-- ⊢  … and with the U-expand wrapper
theorem UResM.Mono.expandRes {B : Type} (S : Supply) (β : TyVar) (l : Label) (τ : Ty B)
    {r r' : UResM B} (h : UResM.Mono r r') :
    UResM.Mono (expandResM S β l τ r) (expandResM S β l τ r') := by
  rcases h with h | h
  · subst h; exact .inl rfl
  · subst h; exact .inr rfl

-- THE FUEL LEMMA, for both sorts at once (the mutual induction is on the
-- budget, exactly as unifySpineF_fuel_irrel's is — but with no measure
-- hypothesis, because `outOfFuel` absorbs the shortfall).
-- The fuel-0 base, for both sorts: at zero budget everything except the arms
-- that need no recursion at all (a variable binding, ★, a base clash, an
-- exhausted side) is `outOfFuel`, and those arms do not look at the budget.
private theorem unifyM_fuel_mono_zero {B : Type} [DecidableEq B] (fuel' : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B),
        UResM.Mono (unifyTyF S 0 τ τ') (unifyTyF S fuel' τ τ')) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)),
        UResM.Mono (unifySpineMF S 0 s₁ s₂) (unifySpineMF S fuel' s₁ s₂)) := by
  cases fuel' with
  | zero => exact ⟨fun _ _ _ => .inr rfl, fun _ _ _ => .inr rfl⟩
  | succ g =>
      refine ⟨fun S τ τ' => ?_, fun S s₁ s₂ => ?_⟩
      · cases τ <;> cases τ' <;> first | exact .inr rfl | exact .inl rfl
      · cases s₁ with
        | nil => exact .inr rfl
        | cons a s₁ =>
          cases s₂ with
          | nil => exact .inr rfl
          | cons b s₂ => exact .inl rfl

theorem unifyM_fuel_mono {B : Type} [DecidableEq B] (N : Nat) :
    ∀ fuel, fuel ≤ N → ∀ fuel', fuel ≤ fuel' →
      (∀ (S : Supply) (τ τ' : Ty B),
          UResM.Mono (unifyTyF S fuel τ τ') (unifyTyF S fuel' τ τ')) ∧
      (∀ (S : Supply) (s₁ s₂ : List (Atom B)),
          UResM.Mono (unifySpineMF S fuel s₁ s₂) (unifySpineMF S fuel' s₁ s₂)) := by
  induction N with
  | zero =>
      intro fuel hfN fuel' _
      have h0 : fuel = 0 := Nat.le_zero.mp hfN
      subst h0
      exact unifyM_fuel_mono_zero fuel'
  | succ N IH =>
      intro fuel hfN fuel' hff
      cases fuel with
      | zero => exact unifyM_fuel_mono_zero fuel'
      | succ f =>
          obtain ⟨f', rfl⟩ : ∃ g, fuel' = g + 1 := ⟨fuel' - 1, by omega⟩
          have IH' := IH f (by omega) f' (by omega)
          refine ⟨fun S τ τ' => ?_, fun S s₁ s₂ => ?_⟩
          · cases τ <;> cases τ' <;>
              first
                | exact .inr rfl
                | exact UResM.Mono.seq (IH'.1 S _ _) (fun θ S' => IH'.1 S' _ _)
                | exact IH'.2 S _ _
          · cases s₁ with
            | nil => exact .inr rfl
            | cons a s₁ =>
              cases s₂ with
              | nil => exact .inr rfl
              | cons b s₂ =>
                simp only [unifySpineMF]
                cases hsl : stripL (a :: s₁) (b :: s₂) with
                | some p => obtain ⟨t₁, t₂⟩ := p; exact IH'.2 S t₁ t₂
                | none =>
                cases hsr : stripR (a :: s₁) (b :: s₂) with
                | some p => obtain ⟨t₁, t₂⟩ := p; exact IH'.2 S t₁ t₂
                | none =>
                cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
                | some r => exact .inr rfl
                | none =>
                cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
                | some r => exact .inr rfl
                | none =>
                cases hml : matchL (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hml2 : matchL (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hmr : matchR (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hmr2 : matchR (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hg : groundMatch (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases he1 : expandL S (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
                    exact UResM.Mono.expandRes S β0 l0 τ0
                      (IH'.2 S.fresh.2.fresh.2 t₁ t₂)
                | none =>
                cases he2 : expandL S (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
                    exact UResM.Mono.expandRes S β0 l0 τ0
                      (IH'.2 S.fresh.2.fresh.2 t₁ t₂)
                | none => exact .inr rfl

-- ⊢  a REACHED row verdict is fuel-independent
theorem unifySpineMF_fuel_mono {B : Type} [DecidableEq B] {S : Supply}
    {fuel fuel' : Nat} {s₁ s₂ : List (Atom B)} (h : fuel ≤ fuel')
    (hne : unifySpineMF S fuel s₁ s₂ ≠ .outOfFuel) :
    unifySpineMF S fuel' s₁ s₂ = unifySpineMF S fuel s₁ s₂ :=
  ((unifyM_fuel_mono fuel fuel (Nat.le_refl _) fuel' h).2 S s₁ s₂).resolve_left hne

-- ⊢  … and a reached type verdict
theorem unifyTyF_fuel_mono {B : Type} [DecidableEq B] {S : Supply}
    {fuel fuel' : Nat} {τ τ' : Ty B} (h : fuel ≤ fuel')
    (hne : unifyTyF S fuel τ τ' ≠ .outOfFuel) :
    unifyTyF S fuel' τ τ' = unifyTyF S fuel τ τ' :=
  ((unifyM_fuel_mono fuel fuel (Nat.le_refl _) fuel' h).1 S τ τ').resolve_left hne

-- ⊢  … lifted to the entry points
theorem unifyRowM_fuel_mono {B : Type} [DecidableEq B] {fuel fuel' : Nat}
    {ρ₁ ρ₂ : Row B} (h : fuel ≤ fuel') (hne : unifyRowM fuel ρ₁ ρ₂ ≠ .outOfFuel) :
    unifyRowM fuel' ρ₁ ρ₂ = unifyRowM fuel ρ₁ ρ₂ :=
  unifySpineMF_fuel_mono h hne



--------------------- P5: SUCCESS SOUNDNESS, MUTUALLY -----------------------
-- proof-plan.md §2 (table row 1) / §4-P5. The port of unifySpineF_success_sound
-- (:3109) onto the mutual driver, plus its ≐ counterpart — which is new, since
-- there was no type pass to be sound about before.
--
-- What changed in the argument: an eq-emitting arm no longer hands its equation
-- to the caller, so `EqsSat θ eqs` disappears from the statement. In its place
-- the arm's success is a COMPOSITE, and Sol.Sat.comp_inv splits it into the
-- type solution and the residual solution; the type IH turns the first into the
-- `heq` those reflection lemmas want, and unifies_sApplySubst_of_sat (P1's
-- apply-then-unify bridge) undoes the substitution the arm applied to the
-- residual. That is the whole delta — every move-reflection lemma is reused
-- verbatim.
--
-- Fuel is arbitrary: a success means the same thing whatever the budget was, so
-- unifyM_fuel_mono is not needed here.

-- ⊢  α ≔ τ, once met, unifies α with τ
theorem bindTy_sound {B : Type} {θ : TySubst B} {S : Supply} {α : TyVar} {τ : Ty B}
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

-- ⊢  U-var-solve, at the mutual result type (solveVar_reflect, :2321)
theorem solveVarM_reflect {B : Type} {θ : TySubst B} {S : Supply}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hsolve : solveVarM S s₁ s₂ = some (.success s S')) (hsat : Sol.Sat θ s) :
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

-- Base cases: one side exhausted ⟹ allVarsEmpty forces the other's vars to ε.
theorem unifySpineMF_nil_left_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (s₂ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel [] s₂ = .success s S') (hsat : Sol.Sat θ s) :
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
    (h : unifySpineMF S fuel (a :: s₁) [] = .success s S') (hsat : Sol.Sat θ s) :
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
    (h : unifyTyF S fuel (.base b) (.base b') = .success s S') :
    TyUnifies θ (.base b) (.base b') := by
  by_cases hb : b = b'
  · subst hb; exact TyEquiv.refl _
  · simp [unifyTyF, hb] at h

-- THE SOUNDNESS LEG, both sorts at once.
-- ⊢  unifyTyF S fuel τ τ' = success s _,  θ ⊨ s   ⟹   θ ⊨ τ ≐ τ'
-- ⊢  unifySpineMF S fuel s₁ s₂ = success s _,  θ ⊨ s
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem unifyM_success_sound {B : Type} [DecidableEq B] {θ : TySubst B} (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
        unifyTyF S fuel τ τ' = .success s S' → Sol.Sat θ s → TyUnifies θ τ τ') ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply},
        unifySpineMF S fuel s₁ s₂ = .success s S' → Sol.Sat θ s →
        RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) := by
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' s S' h hsat => ?_, fun S s₁ s₂ s S' h hsat => ?_⟩
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
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B))
          {s : Sol B} {S' : Supply},
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' → Sol.Sat θ s →
          TyUnifies θ τ τ' ∧
            RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
        intro S τ τ' t₁ t₂ s S' h hsat
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        obtain ⟨h₁, h₂⟩ := hsat.comp_inv
        exact ⟨ih.1 S τ τ' hty h₁,
               (unifies_sApplySubst_of_sat h₁ t₁ t₂).mp (ih.2 S₁ _ _ hrow h₂)⟩
      refine ⟨fun S τ τ' s S' h hsat => ?_, fun S s₁ s₂ s S' h hsat => ?_⟩
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
                replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .success s S' := h
                obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
                obtain ⟨h₁, h₂⟩ := hsat.comp_inv
                exact TyEquiv.fn (ih.1 S a₁ a₂ hty h₁)
                  ((tyUnifies_applySubst_of_sat h₁ b₁ b₂).mp (ih.1 S₁ _ _ hrow h₂))
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact (bindTy_sound h hsat).symm
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
                exact TyEquiv.rcd
                  (((RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)).trans
                      (ih.2 S _ _ h hsat)).trans
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
              exact stripL_reflect hsl (ih.2 S t₁ t₂ h hsat)
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact stripR_reflect hsr (ih.2 S t₁ t₂ h hsat)
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_reflect (hv1.trans (congrArg some h)) hsat
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact (solveVarM_reflect (hv2.trans (congrArg some h)) hsat).symm
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              obtain ⟨he, hr⟩ := arm S τ0 τ0' t₁ t₂ h hsat
              exact matchL_reflect hml he hr
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              obtain ⟨he, hr⟩ := arm S τ0 τ0' t₁ t₂ h hsat
              exact (matchL_reflect hml2 he.symm hr.symm).symm
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              obtain ⟨he, hr⟩ := arm S τ0 τ0' t₁ t₂ h hsat
              exact matchR_reflect hmr he hr
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              obtain ⟨he, hr⟩ := arm S τ0 τ0' t₁ t₂ h hsat
              exact (matchR_reflect hmr2 he.symm hr.symm).symm
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              obtain ⟨he, hr⟩ := arm S τ0 τ0' t₁ t₂ h hsat
              exact groundMatch_reflect hg he hr
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              obtain ⟨he, hr⟩ := arm S τ0 τ0' t₁ t₂ h hsat
              exact (groundMatch_reflect hg2 he.symm hr.symm).symm
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              obtain ⟨s', hrec, rfl⟩ := expandResM_success h
              obtain ⟨h₀, h'⟩ := hsat.comp_inv
              obtain ⟨hs1, hvv, hcc, hren⟩ := expandL_spec he1
              rw [hs1]
              refine expand_reflect hvv hcc
                (h₀.2 _ List.mem_cons_self) (h₀.1 _ List.mem_cons_self).symm ?_
              rw [← hren]
              exact ih.2 S.fresh.2.fresh.2 t₁ t₂ hrec h'
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              obtain ⟨s', hrec, rfl⟩ := expandResM_success h
              obtain ⟨h₀, h'⟩ := hsat.comp_inv
              obtain ⟨hs2, hvv, hcc, hren⟩ := expandL_spec he2
              rw [hs2]
              refine (expand_reflect hvv hcc
                (h₀.2 _ List.mem_cons_self) (h₀.1 _ List.mem_cons_self).symm ?_).symm
              rw [← hren]
              exact ih.2 S.fresh.2.fresh.2 t₁ t₂ hrec h'
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h <;> cases h

-- The ≐ᵣ success case is SOUND under the mutual driver, with NO residual
-- equations: the solution is the whole story.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = success s _,  θ ⊨ s   ⟹   θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRowM_success_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    {fuel : Nat} {ρ₁ ρ₂ : Row B} {s : Sol B} {S' : Supply}
    (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') (hsat : Sol.Sat θ s) :
    Unifies θ ρ₁ ρ₂ := by
  unfold unifyRowM unifySpineM at h
  have key := (unifyM_success_sound fuel).2 _ ρ₁.toSpine ρ₂.toSpine h hsat
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.trans (key.trans e₂.symm)

-- … and so is ≐ itself.
-- ⊢  unifyTyM fuel τ τ' = success s _,  θ ⊨ s   ⟹   θ ⊨ τ ≐ τ'
theorem unifyTyM_success_sound {B : Type} [DecidableEq B] {θ : TySubst B}
    {fuel : Nat} {τ τ' : Ty B} {s : Sol B} {S' : Supply}
    (h : unifyTyM fuel τ τ' = .success s S') (hsat : Sol.Sat θ s) :
    TyUnifies θ τ τ' :=
  (unifyM_success_sound fuel).1 _ τ τ' h hsat



------------------ P5: WHERE A SOLUTION'S VARIABLES LIVE --------------------
-- New machinery, not anticipated by the plan (§4-P5 records why). Under the
-- mutual driver an eq-emitting arm recurses on the SUBSTITUTED residual, so the
-- freshness invariant `S.Avoids …` no longer transports by "the residual is a
-- sub-problem": the substitution can put names into the residual that the
-- original problem never had — precisely the ones a nested U-expand invented.
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

-- ## What a solution mentions
-- Keys and ranges, at both sorts. Kept as a PREDICATE rather than a list: it is
-- only ever used inside a `⊆ W`, and a predicate needs no membership algebra.
def SolMentions {B : Type} (s : Sol B) (γ : TyVar) : Prop :=
  (∃ p ∈ s.ty, γ = p.1 ∨ γ ∈ p.2.ftv) ∨ (∃ p ∈ s.row, γ = p.1 ∨ γ ∈ p.2.ftv)

/-- `SolBelow s W`: every name the solution mentions is already in `W`. -/
def SolBelow {B : Type} (s : Sol B) (W : List TyVar) : Prop :=
  ∀ γ, SolMentions s γ → γ ∈ W

theorem SolBelow.mono {B : Type} {s : Sol B} {W W' : List TyVar}
    (h : SolBelow s W) (hW : W ⊆ W') : SolBelow s W' := fun γ hγ => hW (h γ hγ)

theorem SolBelow.nil {B : Type} (W : List TyVar) : SolBelow (Sol.nil (B := B)) W := by
  rintro γ (⟨p, hp, -⟩ | ⟨p, hp, -⟩) <;> cases hp

-- ⊢  a solution's substitution only ever produces names it mentions (or the
--    variable it was asked about, which it left alone)
theorem SolMentions.toSubst {B : Type} (s : Sol B) (α γ : TyVar)
    (h : γ ∈ (s.toSubst.ty α).ftv ∨ γ ∈ (s.toSubst.row α).ftv) :
    γ = α ∨ SolMentions s γ := by
  rcases h with h | h
  · rcases tyLookup_spec (B := B) α s.ty with he | hm
    · left; rw [show (s.toSubst.ty α) = tyLookup α s.ty from rfl, he] at h
      simpa [Ty.ftv] using h
    · exact .inr (.inl ⟨(α, tyLookup α s.ty), hm, .inr h⟩)
  · rcases rowLookup_spec (B := B) α s.row with he | hm
    · left; rw [show (s.toSubst.row α) = rowLookup α s.row from rfl, he] at h
      simpa [Row.ftv] using h
    · exact .inr (.inr ⟨(α, rowLookup α s.row), hm, .inr h⟩)

-- ⊢  applying a W-bounded solution to a W-bounded spine stays inside W
theorem sFtv_sApplySubst_sub {B : Type} {s : Sol B} {t : List (Atom B)} {W : List TyVar}
    (ht : sFtv t ⊆ W) (hs : SolBelow s W) : sFtv (sApplySubst s.toSubst t) ⊆ W := by
  intro γ hγ
  obtain ⟨α, hα, hg⟩ := sFtv_sApplySubst s.toSubst t γ hγ
  rcases SolMentions.toSubst s α γ hg with rfl | hm
  · exact ht hα
  · exact hs γ hm

-- ⊢  … and applying it to a W-bounded TYPE
theorem Ty_ftv_applySubst_sub {B : Type} {s : Sol B} {τ : Ty B} {W : List TyVar}
    (ht : τ.ftv ⊆ W) (hs : SolBelow s W) : (τ.applySubst s.toSubst).ftv ⊆ W := by
  intro γ hγ
  obtain ⟨α, hα, hg⟩ := Ty.ftv_applySubst s.toSubst τ γ hγ
  rcases SolMentions.toSubst s α γ hg with rfl | hm
  · exact ht hα
  · exact hs γ hm

theorem Row_ftv_applySubst_sub {B : Type} {s : Sol B} {ρ : Row B} {W : List TyVar}
    (ht : ρ.ftv ⊆ W) (hs : SolBelow s W) : (ρ.applySubst s.toSubst).ftv ⊆ W := by
  intro γ hγ
  obtain ⟨α, hα, hg⟩ := Row.ftv_applySubst s.toSubst ρ γ hγ
  rcases SolMentions.toSubst s α γ hg with rfl | hm
  · exact ht hα
  · exact hs γ hm

-- ⊢  composing two W-bounded solutions stays W-bounded
theorem SolBelow.comp {B : Type} {s₁ s₂ : Sol B} {W : List TyVar}
    (h₁ : SolBelow s₁ W) (h₂ : SolBelow s₂ W) : SolBelow (s₂.comp s₁) W := by
  rintro γ (⟨p, hp, hγ⟩ | ⟨p, hp, hγ⟩)
  · rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      rcases hγ with rfl | hγ
      · exact h₁ _ (.inl ⟨q, hq, .inl rfl⟩)
      · exact Ty_ftv_applySubst_sub
          (fun _ hx => h₁ _ (.inl ⟨q, hq, .inr hx⟩)) h₂ hγ
    · exact h₂ _ (.inl ⟨p, hp, hγ⟩)
  · rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      rcases hγ with rfl | hγ
      · exact h₁ _ (.inr ⟨q, hq, .inl rfl⟩)
      · exact Row_ftv_applySubst_sub
          (fun _ hx => h₁ _ (.inr ⟨q, hq, .inr hx⟩)) h₂ hγ
    · exact h₂ _ (.inr ⟨p, hp, hγ⟩)



------------------ P5: THE FRESHNESS INVARIANT, TRANSPORTED -----------------
-- The obligation the section above exists for: a successful run only ever
-- mentions names below the supply it RETURNS. `W` is the enlarged avoid-set —
-- the original `V` plus whatever the run invented — and the three conclusions
-- say it is an enlargement, that the returned supply is fresh for it, and that
-- the solution lives inside it. That is exactly what an eq-emitting arm needs to
-- hand the invariant on to the substituted residual.

-- ⊢  U-ε-var binds exactly the side's variables, each to ε
theorem allVarsEmpty_mem {B : Type} : (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → ∀ p ∈ σ, p.1 ∈ sFtv s ∧ p.2 = Row.empty
  | [], σ, h, p, hp => by simp only [allVarsEmpty, Option.some.injEq] at h; cases h; cases hp
  | .field _ _ :: _, σ, h, p, hp => by simp [allVarsEmpty] at h
  | .var α :: s, σ, h, p, hp => by
      simp only [allVarsEmpty, Option.map_eq_some_iff] at h
      obtain ⟨σ', hs, rfl⟩ := h
      rcases List.mem_cons.mp hp with rfl | hp
      · exact ⟨List.mem_cons_self, rfl⟩
      · obtain ⟨h₁, h₂⟩ := allVarsEmpty_mem s hs p hp
        exact ⟨List.mem_cons_of_mem _ h₁, h₂⟩

theorem SolBelow_ofRow {B : Type} {σ : List (TyVar × Row B)} {W : List TyVar}
    (h : ∀ p ∈ σ, p.1 ∈ W ∧ p.2.ftv ⊆ W) : SolBelow (Sol.ofRow σ) W := by
  rintro γ (⟨p, hp, -⟩ | ⟨p, hp, hγ⟩)
  · cases hp
  · rcases hγ with rfl | hγ
    · exact (h p hp).1
    · exact (h p hp).2 hγ


-- ⊢  U-var-solve stays inside the problem's variables
theorem solveVarM_bounded {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    {s : Sol B} {S' : Supply} {V : List TyVar}
    (h : solveVarM S s₁ s₂ = some (.success s S'))
    (hS : S.Avoids V) (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) :
    ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
  cases s₁ with
  | nil => simp [solveVarM] at h
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVarM] at h
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVarM] at h
      | nil =>
        simp only [solveVarM] at h
        split at h
        · simp at h
        · simp only [Option.some.injEq, UResM.success.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          refine ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp => ?_)⟩
          obtain rfl := List.mem_singleton.mp hp
          exact ⟨hV (List.mem_append_left _ List.mem_cons_self),
                 fun x hx => hV (List.mem_append_right _ (by rw [sFtv_ofSpine]; exact hx))⟩

-- ⊢  U-expand's own solution mentions only the problem plus the two names it
--    just invented, and both are inside the enlarged avoid-set
theorem expand_bounded {B : Type} [DecidableEq B] {fuel : Nat}
    (ih : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) (V : List TyVar) {s : Sol B} {S' : Supply},
      S.Avoids V → (sFtv u₁ ++ sFtv u₂) ⊆ V →
      unifySpineMF S fuel u₁ u₂ = .success s S' →
      ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W)
    {S : Supply} {u₁ u₂ : List (Atom B)} {V : List TyVar}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hS : S.Avoids V) (hV : (sFtv u₁ ++ sFtv u₂) ⊆ V)
    (he : expandL S u₁ u₂ = some (β, l, τ, t₁, t₂))
    (h : expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂) = .success s S') :
    ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
  obtain ⟨s', hrec, rfl⟩ := expandResM_success h
  obtain ⟨hs1, hvv, hcc, hren⟩ := expandL_spec he
  have hτV : τ.ftv ⊆ V := fun _ hx =>
    sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_left _ hx)
  have hβV : β ∈ V :=
    sFtv_sub_right hV (mem_sFtv_of_mem_sVarSeq u₂ (by rw [hvv]; exact List.mem_cons_self))
  have hS' : S.fresh.2.fresh.2.Avoids (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    hS.cons_fresh.cons_fresh
  have hV' : (sFtv t₁ ++ sFtv t₂) ⊆ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) := by
    intro x hx
    rcases List.mem_append.mp hx with hh | hh
    · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _
        (sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_right _ hh)))
    · rw [hren] at hh
      rcases List.mem_cons.mp (sFtv_renameVar _ _ u₂ x hh) with rfl | hh'
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (sFtv_sub_right hV hh'))
  obtain ⟨W, hVW, hSW, hbW⟩ := ih S.fresh.2.fresh.2 t₁ t₂ _ hS' hV' hrec
  have hVsub : V ⊆ W := fun _ hx => hVW (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ hx))
  have hdW : S.fresh.1 ∈ W := hVW (List.mem_cons_of_mem _ List.mem_cons_self)
  have hbW' : S.fresh.2.fresh.1 ∈ W := hVW List.mem_cons_self
  refine ⟨W, hVsub, hSW, SolBelow.comp ?_ hbW⟩
  rintro γ (⟨p, hp, hγ⟩ | ⟨p, hp, hγ⟩)
  · obtain rfl := List.mem_singleton.mp hp
    rcases hγ with rfl | hγ
    · exact hdW
    · exact hVsub (hτV hγ)
  · obtain rfl := List.mem_singleton.mp hp
    rcases hγ with rfl | hγ
    · exact hVsub hβV
    · simp only [Row.ftv, Ty.ftv, List.mem_append, List.mem_singleton] at hγ
      rcases hγ with rfl | rfl
      · exact hdW
      · exact hbW'

-- THE BOUNDEDNESS INVARIANT, both sorts at once.
theorem unifyM_bounded {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) (V : List TyVar) {s : Sol B} {S' : Supply},
        S.Avoids V → (τ.ftv ++ τ'.ftv) ⊆ V →
        unifyTyF S fuel τ τ' = .success s S' →
        ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) (V : List TyVar) {s : Sol B} {S' : Supply},
        S.Avoids V → (sFtv s₁ ++ sFtv s₂) ⊆ V →
        unifySpineMF S fuel s₁ s₂ = .success s S' →
        ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W) := by
  -- the shape shared by bindTy's two orientations
  have hbind : ∀ (S : Supply) (α : TyVar) (τ : Ty B) (V : List TyVar) {s : Sol B}
      {S' : Supply}, S.Avoids V → α ∈ V → τ.ftv ⊆ V → bindTy S α τ = .success s S' →
      ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
    intro S α τ V s S' hS hα hτ h
    unfold bindTy at h
    split at h
    · simp only [UResM.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
    · split at h
      · cases h
      · simp only [UResM.success.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        refine ⟨V, fun _ hx => hx, hS, ?_⟩
        rintro γ (⟨p, hp, hγ⟩ | ⟨p, hp, -⟩)
        · obtain rfl := List.mem_singleton.mp hp
          rcases hγ with rfl | hγ
          · exact hα
          · exact hτ hγ
        · cases hp
  -- the shape shared by the six eq-emitting arms
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' V s S' hS hV h => ?_, fun S s₁ s₂ V s S' hS hV h => ?_⟩
      · cases τ with
        | var α =>
            exact hbind S α τ' V hS (hV (List.mem_append_left _ List.mem_cons_self))
              (fun _ hx => hV (List.mem_append_right _ hx)) h
        | base b =>
            cases τ' with
            | var α =>
                exact hbind S α (.base b) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base b' =>
                by_cases hb : b = b'
                · subst hb
                  have hred : unifyTyF S 0 (Ty.base b) (Ty.base b)
                      = .success (Sol.nil (B := B)) S := by simp [unifyTyF]
                  rw [hred] at h
                  simp only [UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
                · simp [unifyTyF, hb] at h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α =>
                exact hbind S α .unk V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk =>
                simp only [unifyTyF, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.fn a₁ b₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.rcd ρ₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => simp [hae] at h
            | some σ' =>
                simp only [hae, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                  ⟨hV (List.mem_append_right _ (allVarsEmpty_mem s₂ hae p hp).1),
                   by rw [(allVarsEmpty_mem s₂ hae p hp).2]; exact fun _ hx => by cases hx⟩)⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => simp [hae] at h
              | some σ' =>
                  simp only [hae, UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                    ⟨hV (List.mem_append_left _ (allVarsEmpty_mem (a :: s₁) hae p hp).1),
                     by rw [(allVarsEmpty_mem (a :: s₁) hae p hp).2]
                        exact fun _ hx => by cases hx⟩)⟩
          | cons b s₂ => cases h
  | succ fuel ih =>
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          {s : Sol B} {S' : Supply}, S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' →
          ∃ W : List TyVar, V ⊆ W ∧ S'.Avoids W ∧ SolBelow s W := by
        intro S τ τ' t₁ t₂ V s S' hS hVt hVr h
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := ih.1 S τ τ' V hS hVt hty
        have hres : (sFtv (sApplySubst s₁.toSubst t₁) ++
                     sFtv (sApplySubst s₁.toSubst t₂)) ⊆ W₁ := fun x hx => by
          rcases List.mem_append.mp hx with hh | hh
          · exact sFtv_sApplySubst_sub
              (fun _ hy => hVW₁ (hVr (List.mem_append_left _ hy))) hb₁ hh
          · exact sFtv_sApplySubst_sub
              (fun _ hy => hVW₁ (hVr (List.mem_append_right _ hy))) hb₁ hh
        obtain ⟨W₂, hW₁W₂, hS₂, hb₂⟩ := ih.2 S₁ _ _ W₁ hS₁ hres hrow
        exact ⟨W₂, fun _ hx => hW₁W₂ (hVW₁ hx), hS₂, (hb₁.mono hW₁W₂).comp hb₂⟩
      refine ⟨fun S τ τ' V s S' hS hV h => ?_, fun S s₁ s₂ V s S' hS hV h => ?_⟩
      · cases τ with
        | var α =>
            exact hbind S α τ' V hS (hV (List.mem_append_left _ List.mem_cons_self))
              (fun _ hx => hV (List.mem_append_right _ hx)) h
        | base b =>
            cases τ' with
            | var α =>
                exact hbind S α (.base b) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base b' =>
                by_cases hb : b = b'
                · subst hb
                  have hred : unifyTyF S (fuel + 1) (Ty.base b) (Ty.base b)
                      = .success (Sol.nil (B := B)) S := by simp [unifyTyF]
                  rw [hred] at h
                  simp only [UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
                · simp [unifyTyF, hb] at h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α =>
                exact hbind S α .unk V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk =>
                simp only [unifyTyF, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow.nil V⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.fn a₁ b₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn a₂ b₂ =>
                replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .success s S' := h
                obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
                obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := ih.1 S a₁ a₂ V hS
                  (fun x hx => by
                    rcases List.mem_append.mp hx with hh | hh
                    · exact hV (List.mem_append_left _ (List.mem_append_left _ hh))
                    · exact hV (List.mem_append_right _ (List.mem_append_left _ hh))) hty
                have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                             (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁
                      (hV (List.mem_append_left _ (List.mem_append_right _ hy)))) hb₁ hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁
                      (hV (List.mem_append_right _ (List.mem_append_right _ hy)))) hb₁ hh
                obtain ⟨W₂, hW₁W₂, hS₂, hb₂⟩ := ih.1 S₁ _ _ W₁ hS₁ hres hrow
                exact ⟨W₂, fun _ hx => hW₁W₂ (hVW₁ hx), hS₂, (hb₁.mono hW₁W₂).comp hb₂⟩
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α =>
                exact hbind S α (.rcd ρ₁) V hS
                  (hV (List.mem_append_right _ List.mem_cons_self))
                  (fun _ hx => hV (List.mem_append_left _ hx)) h
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
                refine ih.2 S _ _ V hS (fun x hx => ?_) h
                rcases List.mem_append.mp hx with hh | hh
                · exact hV (List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh))
                · exact hV (List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh))
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => simp [hae] at h
            | some σ' =>
                simp only [hae, UResM.success.injEq] at h
                obtain ⟨rfl, rfl⟩ := h
                exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                  ⟨hV (List.mem_append_right _ (allVarsEmpty_mem s₂ hae p hp).1),
                   by rw [(allVarsEmpty_mem s₂ hae p hp).2]; exact fun _ hx => by cases hx⟩)⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => simp [hae] at h
              | some σ' =>
                  simp only [hae, UResM.success.injEq] at h
                  obtain ⟨rfl, rfl⟩ := h
                  exact ⟨V, fun _ hx => hx, hS, SolBelow_ofRow (fun p hp =>
                    ⟨hV (List.mem_append_left _ (allVarsEmpty_mem (a :: s₁) hae p hp).1),
                     by rw [(allVarsEmpty_mem (a :: s₁) hae p hp).2]
                        exact fun _ hx => by cases hx⟩)⟩
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_bounded (hv1.trans (congrArg some h)) hS hV
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact solveVarM_bounded (hv2.trans (congrArg some h)) hS (sFtv_sub_swap hV)
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchL_ftv hml).1 hh)
                  · exact sFtv_sub_right hV ((matchL_ftv hml).2.2.1 hh))
                (sFtv_sub_residual hV (matchL_ftv hml).2.1 (matchL_ftv hml).2.2.2) h
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchL_ftv hml2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((matchL_ftv hml2).1 hh))
                (sFtv_sub_residual hV (matchL_ftv hml2).2.2.2 (matchL_ftv hml2).2.1) h
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchR_ftv hmr).1 hh)
                  · exact sFtv_sub_right hV ((matchR_ftv hmr).2.2.1 hh))
                (sFtv_sub_residual hV (matchR_ftv hmr).2.1 (matchR_ftv hmr).2.2.2) h
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((matchR_ftv hmr2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((matchR_ftv hmr2).1 hh))
                (sFtv_sub_residual hV (matchR_ftv hmr2).2.2.2 (matchR_ftv hmr2).2.1) h
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((groundMatch_ftv hg).1 hh)
                  · exact sFtv_sub_right hV ((groundMatch_ftv hg).2.2.1 hh))
                (sFtv_sub_residual hV (groundMatch_ftv hg).2.1 (groundMatch_ftv hg).2.2.2) h
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              exact arm S τ0 τ0' t₁ t₂ V hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact sFtv_sub_left hV ((groundMatch_ftv hg2).2.2.1 hh)
                  · exact sFtv_sub_right hV ((groundMatch_ftv hg2).1 hh))
                (sFtv_sub_residual hV (groundMatch_ftv hg2).2.2.2 (groundMatch_ftv hg2).2.1) h
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact expand_bounded ih.2 hS hV he1 h
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              exact expand_bounded ih.2 hS (sFtv_sub_swap hV) he2 h
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h <;> cases h



------------------ P5: SUCCESS COMPLETENESS (mgu), MUTUALLY -----------------
-- The port of unifySpineF_success_complete onto the mutual driver, plus its ≐
-- counterpart. `eqs` is gone: what a unifier must satisfy is the SOLUTION, and
-- nothing else — which is the sharp form of "≐ᵣ computes an mgu" the parked
-- equations always blurred. Together with unifyM_success_sound the unifier set
-- of the problem is EXACTLY {θ : Sol.Sat θ s}, up to the fresh names U-expand
-- invents (the ∃θ'/AgreeOn form, cf. §4-P3b(2)).
--
-- The new work is in the eq-emitting arms: the recursive call is on the
-- SUBSTITUTED residual, so a unifier has to be pushed through the solution
-- (unifies_sApplySubst_of_sat) and the extension the sub-call returns has to be
-- carried back over the solution the arm already had (Sol.Sat.congrAgree, and
-- the boundedness invariant to know it may be).

-- ⊢  the empty solution is met by everything
theorem Sol.Sat_nil {B : Type} {θ : TySubst B} : Sol.Sat θ (Sol.nil (B := B)) :=
  ⟨fun _ hp => (nomatch hp), fun _ hp => (nomatch hp)⟩

-- ⊢  the base and ★ arms bind nothing, so any unifier meets them vacuously
theorem base_arm_complete {B : Type} [DecidableEq B] {θ : TySubst B} {b b' : B}
    {S : Supply} {fuel : Nat} {s : Sol B} {S' : Supply}
    (h : unifyTyF S fuel (.base b) (.base b') = .success s S') : Sol.Sat θ s := by
  by_cases hb : b = b'
  · subst hb
    have hred : unifyTyF S fuel (Ty.base b) (Ty.base b)
        = .success (Sol.nil (B := B)) S := by simp [unifyTyF]
    rw [hred] at h
    simp only [UResM.success.injEq] at h
    obtain ⟨rfl, -⟩ := h
    exact Sol.Sat_nil
  · simp [unifyTyF, hb] at h

theorem unk_arm_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    {S : Supply} {fuel : Nat} {s : Sol B} {S' : Supply}
    (h : unifyTyF S fuel (.unk : Ty B) .unk = .success s S') : Sol.Sat θ s := by
  simp only [unifyTyF, UResM.success.injEq] at h
  obtain ⟨rfl, -⟩ := h
  exact Sol.Sat_nil

theorem AgreeOn.rowEq {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {ρ : Row B} (hsub : ρ.ftv ⊆ V) :
    ρ.applySubst θ = ρ.applySubst θ' :=
  Row.applySubst_congr ρ (fun α hα => h α (hsub hα))

-- ⊢  a unifier transports along agreement, at both sorts
theorem AgreeOn.unifiesSpine {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {t₁ t₂ : List (Atom B)}
    (h₁ : sFtv t₁ ⊆ V) (h₂ : sFtv t₂ ⊆ V)
    (hu : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ') ((ofSpine t₂).applySubst θ') := by
  rw [← h.rowEq (by rw [← sFtv_ofSpine]; exact h₁),
      ← h.rowEq (by rw [← sFtv_ofSpine]; exact h₂)]
  exact hu

theorem AgreeOn.tyUnifies {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {τ τ' : Ty B}
    (h₁ : τ.ftv ⊆ V) (h₂ : τ'.ftv ⊆ V) (hu : TyUnifies θ τ τ') : TyUnifies θ' τ τ' := by
  unfold TyUnifies; rw [← h.tyEq h₁, ← h.tyEq h₂]; exact hu

-- ⊢  meeting a solution transports along agreement, provided the agreement
--    covers every name the solution mentions
theorem Sol.Sat.congrAgree {B : Type} {θ θ' : TySubst B} {s : Sol B} {W : List TyVar}
    (h : Sol.Sat θ s) (hag : AgreeOn θ θ' W) (hb : SolBelow s W) : Sol.Sat θ' s := by
  constructor
  · intro p hp
    rw [← (hag p.1 (hb p.1 (.inl ⟨p, hp, .inl rfl⟩))).1,
        ← hag.tyEq (fun x hx => hb x (.inl ⟨p, hp, .inr hx⟩))]
    exact h.1 p hp
  · intro p hp
    rw [← (hag p.1 (hb p.1 (.inr ⟨p, hp, .inl rfl⟩))).2,
        ← hag.rowEq (fun x hx => hb x (.inr ⟨p, hp, .inr hx⟩))]
    exact h.2 p hp

-- ⊢  meeting both halves of a composite means meeting the composite
--    (the converse of Sol.Sat.comp_inv, and what the arms actually build)
theorem Sol.Sat.comp {B : Type} {θ : TySubst B} {s₁ s₂ : Sol B}
    (h₁ : Sol.Sat θ s₁) (h₂ : Sol.Sat θ s₂) : Sol.Sat θ (s₂.comp s₁) := by
  constructor
  · intro p hp
    rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      show TyEquiv (θ.ty q.1) ((q.2.applySubst s₂.toSubst).applySubst θ)
      rw [Ty.applySubst_applySubst]
      exact (h₁.1 q hq).trans (Ty.applySubst_substEquiv h₂.substEquiv q.2)
    · exact h₂.1 p hp
  · intro p hp
    rcases List.mem_append.mp hp with hp | hp
    · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
      show RowEquiv (θ.row q.1) ((q.2.applySubst s₂.toSubst).applySubst θ)
      rw [Row.applySubst_applySubst]
      exact (h₁.2 q hq).trans (Row.applySubst_substEquiv h₂.substEquiv q.2)
    · exact h₂.2 p hp

-- ⊢  a unifier of α and τ meets the binding α ≔ τ
theorem bindTy_complete {B : Type} {θ : TySubst B} {S : Supply} {α : TyVar} {τ : Ty B}
    {s : Sol B} {S' : Supply}
    (h : bindTy S α τ = .success s S') (hu : TyEquiv (θ.ty α) (τ.applySubst θ)) :
    Sol.Sat θ s := by
  unfold bindTy at h
  split at h
  · simp only [UResM.success.injEq] at h
    obtain ⟨rfl, -⟩ := h
    exact Sol.Sat_nil
  · split at h
    · cases h
    · simp only [UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      refine ⟨fun p hp => ?_, fun _ hp => (nomatch hp)⟩
      obtain rfl := List.mem_singleton.mp hp
      exact hu

theorem solveVarM_complete {B : Type} {θ : TySubst B} {S : Supply}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hsolve : solveVarM S s₁ s₂ = some (.success s S'))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    Sol.Sat θ s := by
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
        · simp at hsolve
        · simp only [Option.some.injEq, UResM.success.injEq] at hsolve
          obtain ⟨rfl, -⟩ := hsolve
          refine Sol.Sat_ofRow.mpr (fun p hp => ?_)
          obtain rfl := List.mem_singleton.mp hp
          simp only [ofSpine, Row.applySubst] at hu
          exact RowEquiv.unitR.symm.trans hu

theorem unifySpineMF_nil_left_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (s₂ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel [] s₂ = .success s S')
    (hu : RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ)
                   ((ofSpine s₂).applySubst θ)) : Sol.Sat θ s := by
  simp only [unifySpineMF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact Sol.Sat_ofRow.mpr (allVarsEmpty_complete s₂ hae hu.symm)

theorem unifySpineMF_cons_nil_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    (S : Supply) (fuel : Nat) (a : Atom B) (s₁ : List (Atom B)) {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel (a :: s₁) [] = .success s S')
    (hu : RowEquiv ((ofSpine (a :: s₁)).applySubst θ)
                   ((ofSpine ([] : List (Atom B))).applySubst θ)) : Sol.Sat θ s := by
  simp only [unifySpineMF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, UResM.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact Sol.Sat_ofRow.mpr (allVarsEmpty_complete (a :: s₁) hae hu)

-- THE U-EXPAND ARM, at both orientations (cf. expand_complete, :3556).
theorem expand_completeM {B : Type} [DecidableEq B] {fuel : Nat}
    (ih : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) (V : List TyVar) {s : Sol B}
      {S' : Supply} {θ : TySubst B},
      S.Avoids V → (sFtv u₁ ++ sFtv u₂) ⊆ V →
      unifySpineMF S fuel u₁ u₂ = .success s S' →
      RowEquiv ((ofSpine u₁).applySubst θ) ((ofSpine u₂).applySubst θ) →
      ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s)
    {S : Supply} {u₁ u₂ : List (Atom B)} {V : List TyVar} {θ : TySubst B}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (hS : S.Avoids V) (hV : (sFtv u₁ ++ sFtv u₂) ⊆ V)
    (he : expandL S u₁ u₂ = some (β, l, τ, t₁, t₂))
    (h : expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂) = .success s S')
    (hu : RowEquiv ((ofSpine u₁).applySubst θ) ((ofSpine u₂).applySubst θ)) :
    ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s := by
  obtain ⟨s', hrec, rfl⟩ := expandResM_success h
  obtain ⟨hs1, hvv, hcc, hren⟩ := expandL_spec he
  have hdV := Supply.fresh_not_mem hS
  have hbV := Supply.fresh_not_mem hS.advance
  have hd₁ : S.fresh.1 ∉ sFtv u₁ := fun hm => hdV (sFtv_sub_left hV hm)
  have hd₂ : S.fresh.1 ∉ sFtv u₂ := fun hm => hdV (sFtv_sub_right hV hm)
  have hb₁ : S.fresh.2.fresh.1 ∉ sFtv u₁ := fun hm => hbV (sFtv_sub_left hV hm)
  have hb₂ : S.fresh.2.fresh.1 ∉ sFtv u₂ := fun hm => hbV (sFtv_sub_right hV hm)
  have hτV : τ.ftv ⊆ V := fun _ hx =>
    sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_left _ hx)
  have hβV : β ∈ V :=
    sFtv_sub_right hV (mem_sFtv_of_mem_sVarSeq u₂ (by rw [hvv]; exact List.mem_cons_self))
  rw [hs1] at hu hd₁ hb₁
  obtain ⟨θ₀, hβ0, hty0, hrec0, -, hag0⟩ :=
    expand_reflect_fwd hvv hcc hd₁ hd₂ hb₁ hb₂ hu
  have hS' : S.fresh.2.fresh.2.Avoids (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    hS.cons_fresh.cons_fresh
  have hV' : (sFtv t₁ ++ sFtv t₂) ⊆ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) := by
    intro x hx
    rcases List.mem_append.mp hx with hh | hh
    · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _
        (sFtv_sub_left hV (by rw [hs1]; exact List.mem_append_right _ hh)))
    · rw [hren] at hh
      rcases List.mem_cons.mp (sFtv_renameVar _ _ u₂ x hh) with rfl | hh'
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (sFtv_sub_right hV hh'))
  have hrec' : RowEquiv ((ofSpine t₁).applySubst θ₀) ((ofSpine t₂).applySubst θ₀) := by
    rw [hren]; exact hrec0
  obtain ⟨θ', hag', hsat'⟩ := ih S.fresh.2.fresh.2 t₁ t₂ _ hS' hV' hrec hrec'
  have hVsub : V ⊆ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    fun _ hx => List.mem_cons_of_mem _ (List.mem_cons_of_mem _ hx)
  have hdvV' : S.fresh.1 ∈ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    List.mem_cons_of_mem _ List.mem_cons_self
  have hb'V' : S.fresh.2.fresh.1 ∈ (S.fresh.2.fresh.1 :: S.fresh.1 :: V) :=
    List.mem_cons_self
  have hagθ : AgreeOn θ θ₀ V := fun α hα =>
    hag0 α (fun hh => hdV (by rw [← hh]; exact hα)) (fun hh => hbV (by rw [← hh]; exact hα))
  refine ⟨θ', hagθ.trans' hag' hVsub, Sol.Sat.comp ⟨fun p hp => ?_, fun p hp => ?_⟩ hsat'⟩
  · obtain rfl := List.mem_singleton.mp hp
    show TyEquiv (θ'.ty S.fresh.1) (τ.applySubst θ')
    rw [← hag'.tyEq (fun _ hx => hVsub (hτV hx)), ← (hag' _ hdvV').1]
    exact hty0.symm
  · obtain rfl := List.mem_singleton.mp hp
    show RowEquiv (θ'.row β)
      (.cat (.sing l (θ'.ty S.fresh.1)) (θ'.row S.fresh.2.fresh.1))
    rw [← (hag' β (hVsub hβV)).2, ← (hag' _ hdvV').1, ← (hag' _ hb'V').2]
    exact hβ0

-- THE COMPLETENESS LEG, both sorts at once.
theorem unifyM_success_complete {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) (V : List TyVar) {s : Sol B} {S' : Supply}
        {θ : TySubst B},
        S.Avoids V → (τ.ftv ++ τ'.ftv) ⊆ V →
        unifyTyF S fuel τ τ' = .success s S' → TyUnifies θ τ τ' →
        ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) (V : List TyVar) {s : Sol B} {S' : Supply}
        {θ : TySubst B},
        S.Avoids V → (sFtv s₁ ++ sFtv s₂) ⊆ V →
        unifySpineMF S fuel s₁ s₂ = .success s S' →
        RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) →
        ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s) := by
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' V s S' θ hS hV h hu => ?_, fun S s₁ s₂ V s S' θ hS hV h hu => ?_⟩
      · cases τ with
        | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu⟩
        | base b =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base b' => exact ⟨θ, AgreeOn.refl θ V, base_arm_complete h⟩
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => exact ⟨θ, AgreeOn.refl θ V, unk_arm_complete h⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
      · cases s₁ with
        | nil => exact ⟨θ, AgreeOn.refl θ V, unifySpineMF_nil_left_complete S 0 s₂ h hu⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              exact ⟨θ, AgreeOn.refl θ V, unifySpineMF_cons_nil_complete S 0 a s₁ h hu⟩
          | cons b s₂ => cases h
  | succ fuel ih =>
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          {s : Sol B} {S' : Supply} {θ : TySubst B}, S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' →
          TyUnifies θ τ τ' →
          RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) →
          ∃ θ' : TySubst B, AgreeOn θ θ' V ∧ Sol.Sat θ' s := by
        intro S τ τ' t₁ t₂ V s S' θ hS hVt hVr h hty hru
        obtain ⟨s₁, S₁, s₂, hsty, hsrow, rfl⟩ := UResM.seq_success h
        obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := (unifyM_bounded fuel).1 S τ τ' V hS hVt hsty
        obtain ⟨θ₁, hag₁, hsat₁⟩ := ih.1 S τ τ' V hS hVt hsty hty
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
        obtain ⟨θ₂, hag₂, hsat₂⟩ := ih.2 S₁ _ _ W₁ hS₁ hres hsrow hru₁
        exact ⟨θ₂, hag₁.trans' hag₂ hVW₁,
               Sol.Sat.comp (hsat₁.congrAgree hag₂ hb₁) hsat₂⟩
      refine ⟨fun S τ τ' V s S' θ hS hV h hu => ?_, fun S s₁ s₂ V s S' θ hS hV h hu => ?_⟩
      · cases τ with
        | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu⟩
        | base b =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base b' => exact ⟨θ, AgreeOn.refl θ V, base_arm_complete h⟩
            | unk => cases h
            | fn _ _ => cases h
            | rcd _ => cases h
        | unk =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => exact ⟨θ, AgreeOn.refl θ V, unk_arm_complete h⟩
            | fn _ _ => cases h
            | rcd _ => cases h
        | fn a₁ b₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn a₂ b₂ =>
                replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
                    unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
                  = .success s S' := h
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
                obtain ⟨s₁, S₁, s₂, hsty, hsrow, rfl⟩ := UResM.seq_success h
                obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ :=
                  (unifyM_bounded fuel).1 S a₁ a₂ V hS hVa hsty
                obtain ⟨θ₁, hag₁, hsat₁⟩ := ih.1 S a₁ a₂ V hS hVa hsty hA
                have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                             (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₁ hy)) hb₁ hh
                  · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₂ hy)) hb₁ hh
                have hB₁ : TyUnifies θ₁ (b₁.applySubst s₁.toSubst)
                    (b₂.applySubst s₁.toSubst) :=
                  (tyUnifies_applySubst_of_sat hsat₁ b₁ b₂).mpr
                    (hag₁.tyUnifies hVb₁ hVb₂ hB)
                obtain ⟨θ₂, hag₂, hsat₂⟩ := ih.1 S₁ _ _ W₁ hS₁ hres hsrow hB₁
                exact ⟨θ₂, hag₁.trans' hag₂ hVW₁,
                       Sol.Sat.comp (hsat₁.congrAgree hag₂ hb₁) hsat₂⟩
            | rcd _ => cases h
        | rcd ρ₁ =>
            cases τ' with
            | var α => exact ⟨θ, AgreeOn.refl θ V, bindTy_complete h hu.symm⟩
            | base _ => cases h
            | unk => cases h
            | fn _ _ => cases h
            | rcd ρ₂ =>
                replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
                obtain ⟨ρ', heq, hR⟩ :=
                  TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ))
                    (Ty.rcd (ρ₂.applySubst θ)) from hu)
                simp only [Ty.rcd.injEq] at heq
                obtain rfl := heq
                have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
                have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
                refine ih.2 S _ _ V hS (fun x hx => ?_) h (e₁.symm.trans (hR.trans e₂))
                rcases List.mem_append.mp hx with hh | hh
                · exact hV (List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh))
                · exact hV (List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh))
      · cases s₁ with
        | nil =>
            exact ⟨θ, AgreeOn.refl θ V,
              unifySpineMF_nil_left_complete S (fuel + 1) s₂ h hu⟩
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              exact ⟨θ, AgreeOn.refl θ V,
                unifySpineMF_cons_nil_complete S (fuel + 1) a s₁ h hu⟩
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
                (stripL_reflect_fwd hsl hu)
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ V hS
                (sFtv_sub_residual hV (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
                (stripR_reflect_fwd hsr hu)
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact ⟨θ, AgreeOn.refl θ V,
                solveVarM_complete (hv1.trans (congrArg some h)) hu⟩
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact ⟨θ, AgreeOn.refl θ V,
                solveVarM_complete (hv2.trans (congrArg some h)) hu.symm⟩
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
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact expand_completeM ih.2 hS hV he1 h hu
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              exact expand_completeM ih.2 hS (sFtv_sub_swap hV) he2 h hu.symm
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h <;> cases h

-- ≐ᵣ SUCCESS COMPLETENESS under the mutual driver. With unifyRowM_success_sound
-- the unifier set of ρ₁ ≐ᵣ ρ₂ is EXACTLY {θ : Sol.Sat θ s}, modulo the fresh
-- names the run invented: the algorithm computes a most general unifier, and
-- there are no leftover equations to qualify that any more.
theorem unifyRowM_success_complete {B : Type} [DecidableEq B] {θ : TySubst B}
    {fuel : Nat} {ρ₁ ρ₂ : Row B} {s : Sol B} {S' : Supply}
    (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') (hu : Unifies θ ρ₁ ρ₂) :
    ∃ θ' : TySubst B,
      AgreeOn θ θ' (sFtv ρ₁.toSpine ++ sFtv ρ₂.toSpine) ∧ Sol.Sat θ' s := by
  unfold unifyRowM unifySpineM at h
  unfold Unifies at hu
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact (unifyM_success_complete fuel).2 _ ρ₁.toSpine ρ₂.toSpine _
    (localSupply_avoids _ _) (fun _ hx => hx) h (e₁.symm.trans (hu.trans e₂))



------------------ P5: CLASH SOUNDNESS, MUTUALLY ----------------------------
-- The port of unifySpineF_clash_no_unifier onto the mutual driver, plus its ≐
-- counterpart. The genuinely new case is the one proof-plan.md §2 predicted:
-- **a clash inside a field type is a clash of the whole**. It splits in two —
-- the sub-unification clashed (the type IH kills it outright), or it SUCCEEDED
-- and the substituted residual clashed. The second is why the plan orders
-- completeness before clash: to hand a unifier to the residual one must first
-- EXTEND it to meet the solution the arm just applied, which is exactly
-- unifyM_success_complete plus unifyM_bounded.

theorem UResM.seq_clash {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (h : r.seq k = .clash) :
    r = .clash ∨ ∃ s S, r = .success s S ∧ k s.toSubst S = .clash := by
  cases r with
  | success s S =>
      refine .inr ⟨s, S, rfl, ?_⟩
      simp only [UResM.seq] at h
      revert h; cases hk : k s.toSubst S with
      | success s' S' => intro h; cases h
      | clash => intro _; rfl
      | occurs => intro h; cases h
      | stuck => intro h; cases h
      | outOfFuel => intro h; cases h
  | clash => exact .inl rfl
  | occurs => cases h
  | stuck => cases h
  | outOfFuel => cases h

theorem bindTy_ne_clash {B : Type} {S : Supply} {α : TyVar} {τ : Ty B} :
    bindTy S α τ ≠ .clash := by
  intro h; unfold bindTy at h
  split at h
  · cases h
  · split at h <;> cases h

theorem solveVarM_ne_clash {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)} :
    solveVarM S s₁ s₂ ≠ some .clash := by
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

theorem expandResM_clash {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} (h : expandResM S β l τ r = .clash) : r = .clash := by
  cases r with
  | success _ _ => cases h
  | clash => rfl
  | occurs => cases h
  | stuck => cases h
  | outOfFuel => cases h

-- ≐ dispatches on the head constructors: every MISMATCH is refuted here once
-- and for all (≈ₜ never changes a head, minimal.lean:163ff), and only the two
-- recursive pairs are handed back to the caller.
theorem tyClash_dispatch {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {θ : TySubst B}
    (h : unifyTyF S fuel τ τ' = .clash) (hu : TyUnifies θ τ τ')
    (hfn : ∀ a₁ b₁ a₂ b₂ : Ty B, τ = .fn a₁ b₁ → τ' = .fn a₂ b₂ → False)
    (hrcd : ∀ ρ₁ ρ₂ : Row B, τ = .rcd ρ₁ → τ' = .rcd ρ₂ → False) : False := by
  cases τ with
  | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := τ') h
  | base b =>
      have hb := TyEquiv.base_inv (show TyEquiv (Ty.base b) _ from hu)
      cases τ' with
      | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := Ty.base b) h
      | base b' =>
          simp only [Ty.applySubst, Ty.base.injEq] at hb
          subst hb; cases fuel <;> simp [unifyTyF] at h
      | unk => simp only [Ty.applySubst] at hb; cases hb
      | fn _ _ => simp only [Ty.applySubst] at hb; cases hb
      | rcd _ => simp only [Ty.applySubst] at hb; cases hb
  | unk =>
      have hb := TyEquiv.unk_inv (show TyEquiv (Ty.unk : Ty B) _ from hu)
      cases τ' with
      | var α =>
          cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := (Ty.unk : Ty B)) h
      | base _ => simp only [Ty.applySubst] at hb; cases hb
      | unk => cases fuel <;> simp [unifyTyF] at h
      | fn _ _ => simp only [Ty.applySubst] at hb; cases hb
      | rcd _ => simp only [Ty.applySubst] at hb; cases hb
  | fn a₁ b₁ =>
      have hb := TyEquiv.fn_inv
        (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from hu)
      cases τ' with
      | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := Ty.fn a₁ b₁) h
      | base _ => obtain ⟨_, _, he, -, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | unk => obtain ⟨_, _, he, -, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | fn a₂ b₂ => exact hfn a₁ b₁ a₂ b₂ rfl rfl
      | rcd _ => obtain ⟨_, _, he, -, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
  | rcd ρ₁ =>
      have hb := TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from hu)
      cases τ' with
      | var α => cases fuel <;> exact bindTy_ne_clash (S := S) (α := α) (τ := Ty.rcd ρ₁) h
      | base _ => obtain ⟨_, he, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | unk => obtain ⟨_, he, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | fn _ _ => obtain ⟨_, he, -⟩ := hb; simp only [Ty.applySubst] at he; cases he
      | rcd ρ₂ => exact hrcd ρ₁ ρ₂ rfl rfl

-- THE CLASH LEG, both sorts at once. `V` is the problem's own variables at every
-- call, so the statement stays free of a set parameter; the arms below hand the
-- enlarged set W to the sub-calls internally.
theorem unifyM_clash_no_unifier {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B),
        S.Avoids (τ.ftv ++ τ'.ftv) → unifyTyF S fuel τ τ' = .clash →
        ¬ ∃ θ : TySubst B, TyUnifies θ τ τ') ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)),
        S.Avoids (sFtv s₁ ++ sFtv s₂) → unifySpineMF S fuel s₁ s₂ = .clash →
        ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s₁) (ofSpine s₂)) := by
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' _ h => ?_, fun S s₁ s₂ _ h => ?_⟩
      · rintro ⟨θ, hu⟩
        exact tyClash_dispatch h hu
          (fun _ _ _ _ e₁ e₂ => by subst e₁; subst e₂; cases h)
          (fun _ _ e₁ e₂ => by subst e₁; subst e₂; cases h)
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
            | some => simp [hae] at h
        | cons a s₁ =>
            cases s₂ with
            | nil =>
                simp only [unifySpineMF] at h
                cases hae : allVarsEmpty (a :: s₁) with
                | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
                | some => simp [hae] at h
            | cons b s₂ => cases h
  | succ fuel ih =>
      -- the shape every eq-emitting arm produces
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) (V : List TyVar)
          (θ : TySubst B), S.Avoids V →
          (τ.ftv ++ τ'.ftv) ⊆ V → (sFtv t₁ ++ sFtv t₂) ⊆ V →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂)) = .clash →
          TyUnifies θ τ τ' →
          RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) → False := by
        intro S τ τ' t₁ t₂ V θ hS hVt hVr h hty hru
        rcases UResM.seq_clash h with hc | ⟨s₁, S₁, hs, hk⟩
        · exact ih.1 S τ τ' (hS.mono hVt) hc ⟨θ, hty⟩
        · obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ := (unifyM_bounded fuel).1 S τ τ' V hS hVt hs
          obtain ⟨θ₁, hag₁, hsat₁⟩ :=
            (unifyM_success_complete fuel).1 S τ τ' V hS hVt hs hty
          have hres : (sFtv (sApplySubst s₁.toSubst t₁) ++
                       sFtv (sApplySubst s₁.toSubst t₂)) ⊆ W₁ := fun x hx => by
            rcases List.mem_append.mp hx with hh | hh
            · exact sFtv_sApplySubst_sub
                (fun _ hy => hVW₁ (hVr (List.mem_append_left _ hy))) hb₁ hh
            · exact sFtv_sApplySubst_sub
                (fun _ hy => hVW₁ (hVr (List.mem_append_right _ hy))) hb₁ hh
          exact ih.2 S₁ _ _ (hS₁.mono hres) hk
            ⟨θ₁, (unifies_sApplySubst_of_sat hsat₁ t₁ t₂).mpr
              (hag₁.unifiesSpine (sFtv_sub_left hVr) (sFtv_sub_right hVr) hru)⟩
      refine ⟨fun S τ τ' hS h => ?_, fun S s₁ s₂ hS h => ?_⟩
      · rintro ⟨θ, hu⟩
        refine tyClash_dispatch h hu (fun a₁ b₁ a₂ b₂ e₁ e₂ => ?_)
          (fun ρ₁ ρ₂ e₁ e₂ => ?_)
        · subst e₁; subst e₂
          replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
              unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ')) = .clash := h
          obtain ⟨σ₁, σ₂, heq, hA, hB⟩ := TyEquiv.fn_inv
            (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from hu)
          simp only [Ty.applySubst, Ty.fn.injEq] at heq
          obtain ⟨rfl, rfl⟩ := heq
          have hVa : (a₁.ftv ++ a₂.ftv) ⊆ ((Ty.fn a₁ b₁).ftv ++ (Ty.fn a₂ b₂).ftv) :=
            fun x hx => by
              rcases List.mem_append.mp hx with hh | hh
              · exact List.mem_append_left _ (List.mem_append_left _ hh)
              · exact List.mem_append_right _ (List.mem_append_left _ hh)
          have hVb₁ : b₁.ftv ⊆ ((Ty.fn a₁ b₁).ftv ++ (Ty.fn a₂ b₂).ftv) :=
            fun _ hy => List.mem_append_left _ (List.mem_append_right _ hy)
          have hVb₂ : b₂.ftv ⊆ ((Ty.fn a₁ b₁).ftv ++ (Ty.fn a₂ b₂).ftv) :=
            fun _ hy => List.mem_append_right _ (List.mem_append_right _ hy)
          rcases UResM.seq_clash h with hc | ⟨s₁, S₁, hs, hk⟩
          · exact ih.1 S a₁ a₂ (hS.mono hVa) hc ⟨θ, hA⟩
          · obtain ⟨W₁, hVW₁, hS₁, hb₁⟩ :=
              (unifyM_bounded fuel).1 S a₁ a₂ _ hS hVa hs
            obtain ⟨θ₁, hag₁, hsat₁⟩ :=
              (unifyM_success_complete fuel).1 S a₁ a₂ _ hS hVa hs hA
            have hres : ((b₁.applySubst s₁.toSubst).ftv ++
                         (b₂.applySubst s₁.toSubst).ftv) ⊆ W₁ := fun x hx => by
              rcases List.mem_append.mp hx with hh | hh
              · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₁ hy)) hb₁ hh
              · exact Ty_ftv_applySubst_sub (fun _ hy => hVW₁ (hVb₂ hy)) hb₁ hh
            exact ih.1 S₁ _ _ (hS₁.mono hres) hk
              ⟨θ₁, (tyUnifies_applySubst_of_sat hsat₁ b₁ b₂).mpr
                (hag₁.tyUnifies hVb₁ hVb₂ hB)⟩
        · subst e₁; subst e₂
          replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .clash := h
          obtain ⟨ρ', heq, hR⟩ := TyEquiv.rcd_inv
            (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from hu)
          simp only [Ty.applySubst, Ty.rcd.injEq] at heq
          obtain rfl := heq
          have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
          have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
          refine ih.2 S _ _ (hS.mono (fun x hx => ?_)) h
            ⟨θ, e₁.symm.trans (hR.trans e₂)⟩
          rcases List.mem_append.mp hx with hh | hh
          · exact List.mem_append_left _ ((mem_sFtv_toSpine ρ₁ x).mp hh)
          · exact List.mem_append_right _ ((mem_sFtv_toSpine ρ₂ x).mp hh)
      · cases s₁ with
        | nil =>
            simp only [unifySpineMF] at h
            cases hae : allVarsEmpty s₂ with
            | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
            | some => simp [hae] at h
        | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineMF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
              | some => simp [hae] at h
          | cons b s₂ =>
            rintro ⟨θ, hu⟩
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact ih.2 S t₁ t₂ (hS.residual (stripL_ftv hsl).1 (stripL_ftv hsl).2) h
                ⟨θ, stripL_reflect_fwd hsl hu⟩
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact ih.2 S t₁ t₂ (hS.residual (stripR_ftv hsr).1 (stripR_ftv hsr).2) h
                ⟨θ, stripR_reflect_fwd hsr hu⟩
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact solveVarM_ne_clash (hv1.trans (congrArg some h))
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact solveVarM_ne_clash (hv2.trans (congrArg some h))
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml hu
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml).1 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml).2.2.1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml).2.1 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml).2.2.2 hh))
                h hty hru
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml2).2.2.1 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml2).1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchL_ftv hml2).2.2.2 hh)
                  · exact List.mem_append_right _ ((matchL_ftv hml2).2.1 hh))
                h hty.symm hru.symm
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr hu
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr).1 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr).2.2.1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr).2.1 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr).2.2.2 hh))
                h hty hru
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr2).2.2.1 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr2).1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((matchR_ftv hmr2).2.2.2 hh)
                  · exact List.mem_append_right _ ((matchR_ftv hmr2).2.1 hh))
                h hty.symm hru.symm
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg hu
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg).1 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg).2.2.1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg).2.1 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg).2.2.2 hh))
                h hty hru
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg2 hu.symm
              exact arm S τ0 τ0' t₁ t₂ _ θ hS
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg2).2.2.1 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg2).1 hh))
                (fun x hx => by
                  rcases List.mem_append.mp hx with hh | hh
                  · exact List.mem_append_left _ ((groundMatch_ftv hg2).2.2.2 hh)
                  · exact List.mem_append_right _ ((groundMatch_ftv hg2).2.1 hh))
                h hty.symm hru.symm
            | none =>
            cases he1 : expandL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1] at h
              exact ih.2 S.fresh.2.fresh.2 t₁ t₂ (expandL_avoids hS he1)
                (expandResM_clash h) (expandL_reflect_fwd hS he1 hu)
            | none =>
            cases he2 : expandL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              exact ih.2 S.fresh.2.fresh.2 t₁ t₂ (expandL_avoids hS.swap he2)
                (expandResM_clash h) (expandL_reflect_fwd hS.swap he2 hu.symm)
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, he1, he2] at h
              split at h
              · rename_i hpc; exact projClash_no_unifier hpc ⟨θ, hu⟩
              · cases h

-- ≐ᵣ CLASH is SOUND under the mutual driver: a clash verdict means the two rows
-- have no unifier — including when the clash was found inside a field type.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = clash   ⟹   ¬ ∃ θ. θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRowM_clash_no_unifier {B : Type} [DecidableEq B] {fuel : Nat}
    {ρ₁ ρ₂ : Row B} (h : unifyRowM fuel ρ₁ ρ₂ = .clash) :
    ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ := by
  rintro ⟨θ, hu⟩
  unfold unifyRowM unifySpineM at h
  refine (unifyM_clash_no_unifier fuel).2 _ ρ₁.toSpine ρ₂.toSpine
    (localSupply_avoids _ _) h ⟨θ, ?_⟩
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.symm.trans (hu.trans e₂)



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
