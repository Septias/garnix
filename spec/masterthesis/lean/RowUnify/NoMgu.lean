-- The field-count invariant and the base no-mgu techniques (counting, rigidity, Wand).
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Detectors

namespace MinimalCalculus

--------------------- ≐ᵣ METATHEORY: FIELD-COUNT INVARIANT -------------------
-- The l-field count is a ≈-invariant: ≈ preserves projection-list lengths.
-- Substitution can only ADD l-fields (vars expand to ≥ 0 new fields); for
-- var-free rows the count is fixed. These three facts together prove the
-- U-clash direction of the trichotomy (projClash_no_unifier).

-- Spine roundtrip: toSpine . ofSpine = id.
-- ⊢  spine(ofSpine s) = s
theorem ofSpine_toSpine {B : Type} : (s : List (Atom B)) → (ofSpine s).toSpine = s
  | [] => rfl
  | .field l τ :: s => by
      simp only [ofSpine, Row.toSpine, List.singleton_append]
      exact congrArg (.field l τ :: ·) (ofSpine_toSpine s)
  | .var α :: s => by
      simp only [ofSpine, Row.toSpine, List.singleton_append]
      exact congrArg (.var α :: ·) (ofSpine_toSpine s)

-- l-field count distributes over spine append.
-- ⊢  count_l(s₁ ++ s₂) = count_l(s₁) + count_l(s₂)
theorem sFieldCount_append {B : Type} (l : Label) : (s₁ s₂ : List (Atom B)) →
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
theorem sHasVar_false_iff {B : Type} : (s : List (Atom B)) →
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
theorem hasMguP_not_of_iff {B : Type} {P P' : TySubst B → Prop}
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


end MinimalCalculus
