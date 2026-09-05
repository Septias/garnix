-- Solution algebra (Sol, SolSat, the bridge), the fresh-name supply, and the U-expand metatheory.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.NoMgu

namespace MinimalCalculus

------------------------- ≐ᵣ SUCCESS SOUNDNESS  -----------------------
-- The success case emits a row-var solution list σ and residual type
-- equations eqs. A substitution θ "extends σ" when it agrees with every
-- binding (α ≔ ρ) up to ≈ under θ, and "satisfies eqs" when it makes every
-- emitted pair ≈-equal. Soundness (below/next): under both, θ unifies the
-- original rows. The individual MOVE-REFLECTION lemmas here are the reusable
-- content — each says "if θ unifies the residual, it unified the original".


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
-- equality version (Ty/Row.applySubst_congr), which is too rigid here — a
-- solution is only ever met UP TO ≈ (SolSat), so "θ agrees with
-- θ ∘ s.toSubst" can only ever be a ≈-statement. Everything else in this
-- section is bookkeeping on top of it.


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


theorem eqsSat_iff_tyUnifies {B : Type} {θ : TySubst B} {eqs : List (Ty B × Ty B)} :
    EqsSat θ eqs ↔ ∀ p ∈ eqs, TyUnifies θ p.1 p.2 := Iff.rfl


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
-- The generator is minimal.lean's `natName`, reused verbatim: natName n has
-- LENGTH n, so a name longer than everything in an avoid-set is fresh and two
-- names of different lengths never collide. Nothing here inspects strings —
-- the supply is a Nat, and the avoid-set is PROOF-ONLY: the algorithm never
-- computes it, so the arms stay reducible.


-- ## The supply  (`Supply`, `Supply.Avoids`, `localSupply`, Defs.lean)
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
-- (minimal.lean) — the EQUALITY congruence is exactly right here,
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
-- exactly crossfield_unifiable (NoMgu.lean).
--
-- This section is the METATHEORY of the move: the host is forced (host_forced),
-- the algebraic shift the move performs (expand_shift), and the two reflection
-- lemmas.

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


end MinimalCalculus
