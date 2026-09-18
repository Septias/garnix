-- THE UNIFICATION TYPE of row equivalence under asymmetric concatenation.
--
-- Siekmann's classification asks, for an equational theory E, how big a
-- COMPLETE SET of unifiers a solvable E-problem can need:
--   unitary    every solvable problem has a single mgu          (free terms, Rémy rows)
--   finitary   a finite complete set always suffices            (AC, ACI)
--   infinitary some problem needs an infinite one, but a
--              MINIMAL complete set still exists                (word equations)
--   nullary    some solvable problem has no minimal complete set at all
--
-- For ≈ we already knew the first line fails: `wand_no_mgu` (NoMgu.lean) shows
-- (β | α) ≐ᵣ (l:𝓫) is solvable with two incomparable maximal unifiers. That is
-- a FINITARY obstruction — a complete set of size two exists — so it leaves the
-- theory anywhere in {finitary, infinitary, nullary}.
--
-- This file kills finitary as well, with the SHIFT PROBLEM
--
--       (α | l: 𝓫)   ≐ᵣ   (l: 𝓫 | α)
--
-- "can a variable commute past a field it duplicates". In a theory where
-- distinct labels commute and equal labels do not, this is the smallest
-- equation whose solutions are counted rather than constructed:
--
--   * it is SOLVABLE          — α ≔ ε works                     (shift_unifiable)
--   * EVERY unifier makes α SPINE-VAR-FREE                      (shift_unifier_varFree)
--     — the equation admits no symbolic answer at all; the segment index of the
--       trailing field is |vars α|, and the projection forces it to 0.
--   * the unifiers α ≔ (l:𝓫)^k, k ∈ ℕ, are pairwise incomparable (shiftSub_antichain)
--     — a var-free row has a FIXED l-field count under substitution, so nothing
--       covers two different k.
--   * hence no mgu (shift_no_mgu) and NO FINITE COMPLETE SET
--     (shift_no_finite_complete_set).
--
-- So ≈-unification under asymmetric concatenation is AT LEAST INFINITARY:
-- not unitary (already by Wand), and now not finitary either.
--
-- WHAT IS *NOT* SETTLED: infinitary vs. nullary. That asks whether every
-- solvable problem has a MINIMAL complete set, and the (l:𝓫)^k family does not
-- decide it — the family is an antichain but is NOT itself complete:
-- α ≔ (m:𝓫) with m ≠ l is a unifier too (offSub_unifies), covered by no member
-- (shift_antichain_not_complete), because a label OTHER than l is unconstrained
-- and commutes freely past l. The full unifier set is "every var-free row whose
-- l-fields are all 𝓫, with anything at all at the other labels", so the natural
-- candidate for a minimal complete set is indexed by TRACES (label sequences mod
-- commutation of distinct labels) with fresh variables at the non-l payloads.
-- Showing that set complete is what would pin infinitary rather than nullary,
-- and it is NOT done here. Do not cite this file for that half.
--
-- THE POINT FOR THE THESIS. This is a lower bound on the ALGORITHM, not an
-- accident of it: any ≐ᵣ that returns one solution, or finitely many, is
-- incomplete on some solvable problem, no matter how many driver arms are
-- added. The incompleteness of the stuck leg is therefore not a gap to be
-- closed — and the shift problem is exactly where the driver goes stuck today
-- (Regressions.unify_shift_stuck).

import RowUnify.NoMgu

namespace MinimalCalculus

------------------------------ THE SHIFT PROBLEM -------------------------------

-- (α | l: 𝓫)
def shiftL {B : Type} (α : TyVar) (l : Label) (b : B) : Row B :=
  .cat (.var α) (.sing l (.base b))

-- (l: 𝓫 | α)
def shiftR {B : Type} (α : TyVar) (l : Label) (b : B) : Row B :=
  .cat (.sing l (.base b)) (.var α)

-- (l: 𝓫)^k — the k-fold repetition of one field, the shape every unifier has.
def fieldPow {B : Type} (l : Label) (b : B) : Nat → Row B
  | 0     => .empty
  | k + 1 => .cat (.sing l (.base b)) (fieldPow l b k)

-- The candidate unifier α ≔ (l: 𝓫)^k, identity elsewhere.
def shiftSub {B : Type} (α : TyVar) (l : Label) (b : B) (k : Nat) : TySubst B where
  ty  := (.var ·)
  row := fun x => if x = α then fieldPow l b k else .var x

-- ⊢  (shiftSub α l b k).row α  =  (l:𝓫)^k
theorem shiftSub_row_self {B : Type} (α : TyVar) (l : Label) (b : B) (k : Nat) :
    (shiftSub (B := B) α l b k).row α = fieldPow l b k := by
  simp [shiftSub]

---------------------------- (l:𝓫)^k IS A GROUND ROW ---------------------------

-- ⊢  spine((l:𝓫)^k) = replicate k (l:𝓫)
theorem fieldPow_toSpine {B : Type} (l : Label) (b : B) :
    (k : Nat) → (fieldPow l b k).toSpine = List.replicate k (Atom.field l (.base b))
  | 0     => rfl
  | k + 1 => by
      show [Atom.field l (.base b)] ++ (fieldPow l b k).toSpine = _
      rw [fieldPow_toSpine l b k, List.replicate_succ, List.singleton_append]

-- ⊢  (l:𝓫)^k is spine-var-free
theorem fieldPow_varFree {B : Type} (l : Label) (b : B) :
    (k : Nat) → (fieldPow l b k).SpineVarFree
  | 0     => .empty
  | _ + 1 => .cat .sing (fieldPow_varFree l b _)

-- ⊢  count_l(spine (l:𝓫)^k) = k
theorem fieldPow_fieldCount {B : Type} (l : Label) (b : B) :
    (k : Nat) → sFieldCount l (fieldPow (B := B) l b k).toSpine = k
  | 0     => rfl
  | k + 1 => by
      show sFieldCount l ([Atom.field l (.base b)] ++ (fieldPow l b k).toSpine) = _
      rw [sFieldCount_append, fieldPow_fieldCount l b k]
      simp [sFieldCount]
      omega

-- replicate k x ++ [x] = x :: replicate k x — the two sides of the shift
-- problem, once α is instantiated to (l:𝓫)^k, are the SAME spine.
-- ⊢  replicate k x ++ [x]  =  x :: replicate k x
private theorem replicate_append_self {A : Type} (x : A) :
    (k : Nat) → List.replicate k x ++ [x] = x :: List.replicate k x
  | 0     => rfl
  | k + 1 => by
      rw [List.replicate_succ, List.cons_append, replicate_append_self x k]

----------------------------- SOLVABILITY: α ≔ (l:𝓫)^k -------------------------

-- Every member of the family really does solve the problem.
-- ⊢  shiftSub α l b k ⊨ (α | l:𝓫) ≐ᵣ (l:𝓫 | α)
theorem shiftSub_unifies {B : Type} (α : TyVar) (l : Label) (b : B) (k : Nat) :
    Unifies (shiftSub α l b k) (shiftL α l b) (shiftR α l b) := by
  refine RowEquiv.ofChar (Row.Char.of_eq ?_)
  show ((shiftSub (B := B) α l b k).row α).toSpine ++ [Atom.field l (Ty.base b)]
      = [Atom.field l (Ty.base b)] ++ ((shiftSub (B := B) α l b k).row α).toSpine
  rw [shiftSub_row_self, fieldPow_toSpine, List.singleton_append]
  exact replicate_append_self _ k

-- ⊢  the shift problem is SOLVABLE (α ≔ ε)
theorem shift_unifiable {B : Type} (α : TyVar) (l : Label) (b : B) :
    ∃ θ : TySubst B, Unifies θ (shiftL α l b) (shiftR α l b) :=
  ⟨shiftSub α l b 0, shiftSub_unifies α l b 0⟩

----------------- EVERY UNIFIER MAKES α SPINE-VAR-FREE (the rigidity) ----------

-- ⊢  |xs| = 0   ⟹   xs = []
private theorem nil_of_len_zero {A : Type} : {xs : List A} → xs.length = 0 → xs = []
  | [],     _ => rfl
  | _ :: _, h => absurd h (Nat.succ_ne_zero _)

-- The projection equation the shift problem imposes, in isolation.
--
-- With P := proj_l(spine θα) and n := |vars(spine θα)|, the two sides project to
--     proj_l(θα ++ [l:𝓫])  =  P ++ [(n, 𝓫)]        (the field sits after θα's vars)
--     proj_l([l:𝓫] ++ θα)  =  (0, 𝓫) :: P          (the field sits in segment 0)
-- Pointwise agreement walks the list: the head forces P₀'s index to 0, each
-- step copies the previous index, and the LAST entry — which is (n, 𝓫) on the
-- left — meets P's last index. So n = 0: θα cannot contain a variable.
-- ⊢  (P ++ [(n,τ)]) ≈ₚ ((0,σ) :: P)   ⟹   n = 0
theorem shift_proj_forces_zero {B : Type} {n : Nat} {τ : Ty B} :
    (P : List (Nat × Ty B)) → (σ : Ty B) →
    ProjEquiv (P ++ [(n, τ)]) ((0, σ) :: P) → n = 0
  | [],          _, h => by cases h with | cons hn _ _ => exact hn
  | (a, u) :: P, _, h => by
      cases h with
      | cons hn _ h' =>
          have ha : a = 0 := hn
          subst ha
          exact shift_proj_forces_zero P u h'

-- ⊢  θ ⊨ (α | l:𝓫) ≐ᵣ (l:𝓫 | α)   ⟹   θα is spine-var-free
theorem shift_unifier_varFree {B : Type} {α : TyVar} {l : Label} {b : B}
    {θ : TySubst B} (h : Unifies θ (shiftL α l b) (shiftR α l b)) :
    (θ.row α).SpineVarFree := by
  obtain ⟨-, hp⟩ := h.char
  have hl := hp l
  have eL : sProj l (Row.toSpine ((shiftL α l b).applySubst θ))
      = sProj l (θ.row α).toSpine
          ++ [((sVarSeq (θ.row α).toSpine).length, Ty.base b)] := by
    show sProj l ((θ.row α).toSpine ++ [Atom.field l (.base b)]) = _
    rw [sProj_append]
    simp [sProj]
  have eR : sProj l (Row.toSpine ((shiftR α l b).applySubst θ))
      = (0, Ty.base b) :: sProj l (θ.row α).toSpine := by
    show sProj l (Atom.field l (.base b) :: (θ.row α).toSpine) = _
    simp [sProj]
  rw [eL, eR] at hl
  have hn := shift_proj_forces_zero _ _ hl
  exact (spineVarFree_iff_varSeq_nil _).mpr (nil_of_len_zero hn)

------------------- COVERING PINS THE l-COUNT (the antichain) ------------------

-- `instanceOfOn_fieldCount_mono` (NoMgu.lean) only BOUNDS the count, because
-- substitution can add fields under a variable. `instanceOfOn_fieldCount_eq_of_varFree`
-- (also NoMgu.lean, the rigidity half of the same pair) turns the bound into an
-- EQUALITY once the covered image has nothing left to expand. The shift problem
-- makes that hypothesis free: shift_unifier_varFree says EVERY unifier supplies it.
--
-- A unifier of the shift problem covers AT MOST ONE member of the family, and
-- which one is read off its own l-count. This is the whole obstruction.
-- ⊢  θ ⊨ shift,  (α ≔ (l:𝓫)^k) ⊑_{α} θ   ⟹   k = count_l(θα)
theorem shift_cover_forces_count {B : Type} {α : TyVar} {l : Label} {b : B}
    {θ : TySubst B} {k : Nat} (hθ : Unifies θ (shiftL α l b) (shiftR α l b))
    (hcov : InstanceOfOn [α] (shiftSub α l b k) θ) :
    k = sFieldCount l (θ.row α).toSpine := by
  have h := instanceOfOn_fieldCount_eq_of_varFree hcov (List.mem_cons.mpr (.inl rfl))
              (shift_unifier_varFree hθ) l
  rwa [shiftSub_row_self, fieldPow_fieldCount] at h

-- ⊢  j ≠ k   ⟹   (α ≔ (l:𝓫)^j) does NOT factor through (α ≔ (l:𝓫)^k)
theorem shiftSub_antichain {B : Type} (α : TyVar) (l : Label) (b : B) {j k : Nat}
    (hne : j ≠ k) : ¬ InstanceOfOn [α] (shiftSub (B := B) α l b j) (shiftSub α l b k) := by
  intro hcov
  have h := shift_cover_forces_count (shiftSub_unifies α l b k) hcov
  rw [shiftSub_row_self, fieldPow_fieldCount] at h
  exact hne h

-------------------- THE ANTICHAIN IS NOT ITSELF COMPLETE ----------------------
-- A field at a label OTHER than l commutes freely past the l-field, so it is
-- unconstrained by the equation. Hence the (l:𝓫)^k family, though an infinite
-- antichain, does not exhaust the unifiers up to covering — which is exactly
-- why this file bounds the theory from below (not finitary) without deciding
-- infinitary vs. nullary.

-- α ≔ (m: 𝓫), identity elsewhere.
def offSub {B : Type} (α : TyVar) (m : Label) (b : B) : TySubst B where
  ty  := (.var ·)
  row := fun x => if x = α then .sing m (.base b) else .var x

-- ⊢  (offSub α m b).row α  =  (m:𝓫)
theorem offSub_row_self {B : Type} (α : TyVar) (m : Label) (b : B) :
    (offSub (B := B) α m b).row α = .sing m (.base b) := by
  simp [offSub]

-- ⊢  m ≠ l   ⟹   α ≔ (m:𝓫) also solves the shift problem
theorem offSub_unifies {B : Type} (α : TyVar) (l m : Label) (b : B) (hne : m ≠ l) :
    Unifies (offSub α m b) (shiftL α l b) (shiftR α l b) := by
  show RowEquiv (.cat ((offSub (B := B) α m b).row α) (.sing l (.base b)))
                (.cat (.sing l (.base b)) ((offSub (B := B) α m b).row α))
  rw [offSub_row_self]
  exact RowEquiv.comm hne

-- ⊢  m ≠ l   ⟹   count_m(spine (l:𝓫)^k) = 0
theorem fieldPow_fieldCount_off {B : Type} (l m : Label) (b : B) (hne : m ≠ l) :
    (k : Nat) → sFieldCount m (fieldPow (B := B) l b k).toSpine = 0
  | 0     => rfl
  | k + 1 => by
      show sFieldCount m ([Atom.field l (Ty.base b)] ++ (fieldPow l b k).toSpine) = 0
      rw [sFieldCount_append, fieldPow_fieldCount_off l m b hne k]
      have : ¬ (l = m) := fun h => hne h.symm
      simp [sFieldCount, this]

-- ⊢  m ≠ l   ⟹   α ≔ (m:𝓫) factors through NO member of the (l:𝓫)^k family
theorem shift_antichain_not_complete {B : Type} (α : TyVar) (l m : Label) (b : B)
    (hne : m ≠ l) (k : Nat) :
    ¬ InstanceOfOn [α] (offSub (B := B) α m b) (shiftSub α l b k) := by
  intro hcov
  have h := instanceOfOn_fieldCount_eq_of_varFree hcov (List.mem_cons.mpr (.inl rfl))
              (by rw [shiftSub_row_self]; exact fieldPow_varFree l b k) m
  rw [offSub_row_self, shiftSub_row_self, fieldPow_fieldCount_off l m b hne] at h
  simp [Row.toSpine, sFieldCount] at h

------------------------------ NOT UNITARY ------------------------------------

-- ⊢  the shift problem has no mgu, even relativized to its own variable
theorem shift_no_mgu_on {B : Type} (α : TyVar) (l : Label) (b : B) :
    ¬ HasMguOn [α] (fun θ : TySubst B => Unifies θ (shiftL α l b) (shiftR α l b)) := by
  rintro ⟨θ, hθ, hmax⟩
  have h0 := shift_cover_forces_count hθ (hmax _ (shiftSub_unifies α l b 0))
  have h1 := shift_cover_forces_count hθ (hmax _ (shiftSub_unifies α l b 1))
  omega

-- ⊢  … and hence no mgu in the strict sense (the thesis-facing statement)
theorem shift_no_mgu {B : Type} (α : TyVar) (l : Label) (b : B) :
    ¬ HasMgu (shiftL (B := B) α l b) (shiftR α l b) :=
  not_hasMgu_of_not_hasMguOn (shift_no_mgu_on α l b)

------------------------------ NOT FINITARY -----------------------------------

-- ⊢  x ∈ xs   ⟹   x ≤ Σ xs      (to pick a k outside a finite set of counts)
private theorem le_foldr_add : (xs : List Nat) → ∀ x ∈ xs, x ≤ xs.foldr (· + ·) 0
  | [],      _, h => nomatch h
  | y :: ys, x, h => by
      cases List.mem_cons.mp h with
      | inl he => subst he; simp
      | inr hm => have := le_foldr_add ys x hm; simp; omega

-- A complete set of unifiers, relativized to the problem's own variables: every
-- unifier factors through a member.
def CompleteOn {B : Type} (V : List TyVar) (P : TySubst B → Prop)
    (S : List (TySubst B)) : Prop :=
  (∀ θ ∈ S, P θ) ∧ (∀ θ', P θ' → ∃ θ ∈ S, InstanceOfOn V θ' θ)

-- THE RESULT. No FINITE set of unifiers is complete for the shift problem:
-- each member pins one l-count (shift_cover_forces_count), so a list of them
-- pins finitely many, and the family has one unifier per natural number.
-- ⊢  ¬ ∃ finite complete set for (α | l:𝓫) ≐ᵣ (l:𝓫 | α)
theorem shift_no_finite_complete_set {B : Type} (α : TyVar) (l : Label) (b : B)
    (S : List (TySubst B)) :
    ¬ CompleteOn [α] (fun θ => Unifies θ (shiftL α l b) (shiftR α l b)) S := by
  rintro ⟨hsound, hcomplete⟩
  -- a count strictly above every count S can pin
  let counts := S.map (fun θ => sFieldCount l (θ.row α).toSpine)
  let k := counts.foldr (· + ·) 0 + 1
  obtain ⟨θ, hmem, hcov⟩ := hcomplete _ (shiftSub_unifies α l b k)
  have hk : k = sFieldCount l (θ.row α).toSpine :=
    shift_cover_forces_count (hsound θ hmem) hcov
  have hle : sFieldCount l (θ.row α).toSpine ≤ counts.foldr (· + ·) 0 :=
    le_foldr_add counts _ (List.mem_map.mpr ⟨θ, hmem, rfl⟩)
  omega

-- ⊢  … so ≈-unification under asymmetric concatenation is AT LEAST INFINITARY:
--    a solvable problem exists whose every complete set of unifiers is infinite.
theorem rowUnification_not_finitary {B : Type} (b : B) :
    ∃ ρ₁ ρ₂ : Row B,
      (∃ θ, Unifies θ ρ₁ ρ₂) ∧
      ∀ S : List (TySubst B), ¬ CompleteOn ["a"] (fun θ => Unifies θ ρ₁ ρ₂) S :=
  ⟨shiftL "a" "l" b, shiftR "a" "l" b,
   shift_unifiable "a" "l" b, shift_no_finite_complete_set "a" "l" b⟩

end MinimalCalculus
