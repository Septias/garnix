-- THE DRIVER RETURNS AN APPLIED SOLUTION — `UnifyWF` and `UnifyAcyclic`, proved.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- ## Why this is provable now
-- With U-expand gone (plans/drop-expand.md) every arm SOLVES AND APPLIES: a
-- binding is emitted only for a variable of the current problem, and the
-- residual the next stage sees has that binding applied. Nothing is invented.
-- So a solution's keys and everything its bindings mention are variables of
-- the ORIGINAL problem, and no binding mentions a key. The fuzzer saw exactly
-- that ("0 successes whose solution has any edge"); `Sol.Good` below is the
-- statement, and `unifyM_good` the induction.
--
-- ## Why the variables are TAGGED
-- The namespace is shared across sorts, and `a ≐ᵣ (l: a)` succeeds with
-- `a ≔ (l: a | ε)` — the row variable `a` bound, the TYPE variable `a` in its
-- payload. An untagged "no binding mentions a key" is false there; tagged by
-- sort (`Ty.sortedFtv`, `Row.sortedFtv`, `Sol.domS`, State.lean) it holds,
-- because `Sol.toSubst` never moves a variable at the other sort.
--
-- ## What falls out
--  * `Sol.Good.applied` — the solution is idempotent, so ⟦S⟧ = `toSubst`
--    (`Sol.closes_toSubst_of_applied`) with no closure construction;
--  * `Sol.Good.wf` — `Acyclic` and `Ranked` at rank ≡ 0;
--  * `Sol.Good.sat` — `toSubst s` satisfies `s`: NON-VACUITY of success. The
--    success legs were vacuous on an unsatisfiable `s`; this closes that;
--  * key-consistency — two bindings of the same key agree, so the
--    first-match-wins reader `toSubst` and the all-pairs reader `Sat` cannot
--    disagree. That was the duplicate-key "keystone" of the gap analysis; its
--    only witness went through U-expand's host-only rename.

import RowUnify.State

namespace MinimalCalculus

------------------------- TAGGED VARIABLES OF A SPINE --------------------------

-- The sort-tagged variables of a spine: `Row.sortedFtv ∘ ofSpine`, written
-- directly on atoms so the detector lemmas below are atom-membership facts.
def sSorted {B : Type} : List (Atom B) → List (Bool × TyVar)
  | [] => []
  | .field _ τ :: s => Ty.sortedFtv τ ++ sSorted s
  | .var α :: s     => (true, α) :: sSorted s

def Atom.sorted {B : Type} : Atom B → List (Bool × TyVar)
  | .field _ τ => Ty.sortedFtv τ
  | .var α     => [(true, α)]

theorem mem_sSorted {B : Type} {x : Bool × TyVar} :
    (s : List (Atom B)) → (x ∈ sSorted s ↔ ∃ a ∈ s, x ∈ a.sorted)
  | [] => by simp [sSorted]
  | .field l τ :: s => by
      simp only [sSorted, List.mem_append, mem_sSorted s, List.mem_cons,
        exists_eq_or_imp, Atom.sorted]
  | .var α :: s => by
      simp only [sSorted, List.mem_cons, mem_sSorted s, exists_eq_or_imp,
        Atom.sorted, List.not_mem_nil, or_false]

-- ⊢  a spine built from atoms of another has no variable the other lacks
theorem sSorted_sub_of_atoms {B : Type} {s t : List (Atom B)}
    (h : ∀ a ∈ t, a ∈ s) : sSorted t ⊆ sSorted s := fun x hx => by
  obtain ⟨a, ha, hx⟩ := (mem_sSorted t).mp hx
  exact (mem_sSorted s).mpr ⟨a, h a ha, hx⟩

theorem sSorted_of_field {B : Type} {s : List (Atom B)} {l : Label} {τ : Ty B}
    (h : .field l τ ∈ s) : Ty.sortedFtv τ ⊆ sSorted s := fun x hx =>
  (mem_sSorted s).mpr ⟨.field l τ, h, hx⟩

theorem sSorted_append {B : Type} :
    (s t : List (Atom B)) → sSorted (s ++ t) = sSorted s ++ sSorted t
  | [], _ => rfl
  | .field _ τ :: s, t => by
      simp only [List.cons_append, sSorted, sSorted_append s t, List.append_assoc]
  | .var _ :: s, t => by simp only [List.cons_append, sSorted, sSorted_append s t]

theorem sSorted_toSpine {B : Type} : (ρ : Row B) → sSorted ρ.toSpine = Row.sortedFtv ρ
  | .empty => rfl
  | .var _ => rfl
  | .sing _ τ => by simp [Row.toSpine, sSorted, Row.sortedFtv]
  | .cat ρ₁ ρ₂ => by
      simp only [Row.toSpine, sSorted_append, sSorted_toSpine ρ₁, sSorted_toSpine ρ₂,
        Row.sortedFtv]

theorem sortedFtv_ofSpine {B : Type} : (s : List (Atom B)) →
    Row.sortedFtv (ofSpine s) = sSorted s
  | [] => rfl
  | .field _ τ :: s => by simp only [ofSpine, Row.sortedFtv, sortedFtv_ofSpine s, sSorted]
  | .var _ :: s => by simp only [ofSpine, Row.sortedFtv, sortedFtv_ofSpine s, sSorted]; rfl

theorem sVarSeq_mem_sSorted {B : Type} {γ : TyVar} :
    (s : List (Atom B)) → γ ∈ sVarSeq s → (true, γ) ∈ sSorted s
  | [], h => by simp [sVarSeq] at h
  | .field _ _ :: s, h => by
      simp only [sVarSeq] at h
      exact List.mem_append_right _ (sVarSeq_mem_sSorted s h)
  | .var δ :: s, h => by
      simp only [sVarSeq, List.mem_cons] at h
      rcases h with rfl | h
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (sVarSeq_mem_sSorted s h)

-- ⊢  every occurrence in a substituted spine comes from one of the original's
theorem mem_sSorted_sApplySubst {B : Type} {θ : TySubst B} {x : Bool × TyVar} :
    (t : List (Atom B)) → x ∈ sSorted (sApplySubst θ t) →
    ∃ y ∈ sSorted t, x ∈ θ.ftvAt y
  | [], h => by simp [sApplySubst, sSorted] at h
  | .field l τ :: t, h => by
      simp only [sApplySubst, sSorted, List.mem_append] at h
      rcases h with h | h
      · obtain ⟨y, hy, hx⟩ := Ty.mem_sortedFtv_applySubst τ h
        exact ⟨y, List.mem_append_left _ hy, hx⟩
      · obtain ⟨y, hy, hx⟩ := mem_sSorted_sApplySubst t h
        exact ⟨y, List.mem_append_right _ hy, hx⟩
  | .var α :: t, h => by
      simp only [sApplySubst, sSorted_append, List.mem_append] at h
      rcases h with h | h
      · rw [sSorted_toSpine] at h
        exact ⟨(true, α), List.mem_cons_self, h⟩
      · obtain ⟨y, hy, hx⟩ := mem_sSorted_sApplySubst t h
        exact ⟨y, List.mem_cons_of_mem _ hy, hx⟩

------------------------- THE DETECTORS TAKE ATOMS APART -----------------------
-- Every non-solving move returns sub-spines of its input and payloads of its
-- input's fields. Stated on ATOMS, which makes them sort-agnostic for free.

theorem stripL_atoms {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripL s₁ s₂ = some (t₁, t₂)) : (∀ a ∈ t₁, a ∈ s₁) ∧ (∀ a ∈ t₂, a ∈ s₂) := by
  match s₁, s₂ with
  | .var α :: u₁, .var β :: u₂ =>
      simp only [stripL] at h
      by_cases hab : α = β
      · rw [if_pos hab] at h; cases h
        exact ⟨fun _ ha => List.mem_cons_of_mem _ ha, fun _ ha => List.mem_cons_of_mem _ ha⟩
      · rw [if_neg hab] at h; cases h

theorem stripR_atoms {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripR s₁ s₂ = some (t₁, t₂)) : (∀ a ∈ t₁, a ∈ s₁) ∧ (∀ a ∈ t₂, a ∈ s₂) := by
  unfold stripR at h
  revert h
  cases hl : stripL s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      obtain ⟨g₁, g₂⟩ := stripL_atoms hl
      exact ⟨fun a ha => List.mem_reverse.mp (g₁ a (List.mem_reverse.mp ha)),
             fun a ha => List.mem_reverse.mp (g₂ a (List.mem_reverse.mp ha))⟩

theorem windowExtract_atoms {B : Type} {l : Label} :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') → (∃ l', .field l' τ ∈ s) ∧ ∀ a ∈ s', a ∈ s
  | .field l' τ' :: t, τ, s', h => by
      simp only [windowExtract] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h
        cases h
        exact ⟨⟨l', List.mem_cons_self⟩, fun _ ha => List.mem_cons_of_mem _ ha⟩
      · rw [if_neg hl] at h
        revert h
        cases hw : windowExtract l t with
        | none => intro h; cases h
        | some p =>
            intro h
            obtain ⟨⟨l'', hτ⟩, hs⟩ := windowExtract_atoms t hw
            cases h
            refine ⟨⟨l'', List.mem_cons_of_mem _ hτ⟩, fun a ha => ?_⟩
            rcases List.mem_cons.mp ha with rfl | ha
            · exact List.mem_cons_self
            · exact List.mem_cons_of_mem _ (hs a ha)

theorem removeField_atoms {B : Type} {l : Label} :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    removeField l s = some (τ, s') → (∃ l', .field l' τ ∈ s) ∧ ∀ a ∈ s', a ∈ s
  | .var β :: t, τ, s', h => by
      simp only [removeField] at h
      revert h
      cases hw : removeField l t with
      | none => intro h; cases h
      | some p =>
          intro h
          obtain ⟨⟨l'', hτ⟩, hs⟩ := removeField_atoms t hw
          cases h
          refine ⟨⟨l'', List.mem_cons_of_mem _ hτ⟩, fun a ha => ?_⟩
          rcases List.mem_cons.mp ha with rfl | ha
          · exact List.mem_cons_self
          · exact List.mem_cons_of_mem _ (hs a ha)
  | .field l' τ' :: t, τ, s', h => by
      simp only [removeField] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h
        cases h
        exact ⟨⟨l', List.mem_cons_self⟩, fun _ ha => List.mem_cons_of_mem _ ha⟩
      · rw [if_neg hl] at h
        revert h
        cases hw : removeField l t with
        | none => intro h; cases h
        | some p =>
            intro h
            obtain ⟨⟨l'', hτ⟩, hs⟩ := removeField_atoms t hw
            cases h
            refine ⟨⟨l'', List.mem_cons_of_mem _ hτ⟩, fun a ha => ?_⟩
            rcases List.mem_cons.mp ha with rfl | ha
            · exact List.mem_cons_self
            · exact List.mem_cons_of_mem _ (hs a ha)

/-- What an eq-emitting detector returns: a payload of each side and a residual
of each side, all made of the input's atoms. -/
def EqEmit {B : Type} (s₁ s₂ : List (Atom B)) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) :
    Prop :=
  (∃ l, .field l τ ∈ s₁) ∧ (∀ a ∈ t₁, a ∈ s₁) ∧
  (∃ l, .field l τ' ∈ s₂) ∧ (∀ a ∈ t₂, a ∈ s₂)

theorem matchL_atoms {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    EqEmit s₁ s₂ τ τ' t₁ t₂ := by
  match s₁ with
  | .field l σ :: u₁ =>
      simp only [matchL] at h
      revert h
      cases hw : windowExtract l s₂ with
      | none => intro h; cases h
      | some p =>
          intro h
          cases h
          obtain ⟨hτ, hs⟩ := windowExtract_atoms s₂ hw
          exact ⟨⟨l, List.mem_cons_self⟩, fun _ ha => List.mem_cons_of_mem _ ha, hτ, hs⟩

theorem matchR_atoms {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    EqEmit s₁ s₂ τ τ' t₁ t₂ := by
  unfold matchR at h
  revert h
  cases hl : matchL s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨σ0, σ0', u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl, rfl⟩ := h
      obtain ⟨⟨l, g₀⟩, g₁, ⟨l', g₀'⟩, g₂⟩ := matchL_atoms hl
      exact ⟨⟨l, List.mem_reverse.mp g₀⟩,
             fun a ha => List.mem_reverse.mp (g₁ a (List.mem_reverse.mp ha)),
             ⟨l', List.mem_reverse.mp g₀'⟩,
             fun a ha => List.mem_reverse.mp (g₂ a (List.mem_reverse.mp ha))⟩

theorem groundMatchAux_atoms {B : Type} {s₁ s₂ : List (Atom B)} :
    (ls : List Label) → {τ τ' : Ty B} → {t₁ t₂ : List (Atom B)} →
    groundMatchAux s₁ s₂ ls = some (τ, τ', t₁, t₂) → EqEmit s₁ s₂ τ τ' t₁ t₂
  | l :: ls, τ, τ', t₁, t₂, h => by
      simp only [groundMatchAux] at h
      by_cases hc : sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁
      · rw [if_pos hc] at h
        revert h
        cases h₁ : removeField l s₁ with
        | none => intro h; exact groundMatchAux_atoms ls (by simpa [h₁] using h)
        | some p₁ =>
            cases h₂ : removeField l s₂ with
            | none => intro h; exact groundMatchAux_atoms ls (by simpa [h₁, h₂] using h)
            | some p₂ =>
                intro h
                cases h
                obtain ⟨ha, hb⟩ := removeField_atoms s₁ h₁
                obtain ⟨hc', hd⟩ := removeField_atoms s₂ h₂
                exact ⟨ha, hb, hc', hd⟩
      · rw [if_neg hc] at h
        exact groundMatchAux_atoms ls h

theorem groundMatch_atoms {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    EqEmit s₁ s₂ τ τ' t₁ t₂ := by
  simp only [groundMatch] at h
  by_cases hv : sHasVar s₂
  · rw [if_pos hv] at h; cases h
  · rw [if_neg hv] at h; exact groundMatchAux_atoms _ h

theorem EqEmit.swap {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : EqEmit s₁ s₂ τ τ' t₁ t₂) : EqEmit s₂ s₁ τ' τ t₂ t₁ :=
  ⟨h.2.2.1, h.2.2.2, h.1, h.2.1⟩

-- ⊢  …so both the emitted equation and the residual live inside the problem
theorem EqEmit.ty_sub {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : EqEmit s₁ s₂ τ τ' t₁ t₂) :
    Ty.sortedFtv τ ++ Ty.sortedFtv τ' ⊆ sSorted s₁ ++ sSorted s₂ := fun x hx => by
  obtain ⟨⟨l, h₁⟩, -, ⟨l', h₂⟩, -⟩ := h
  rcases List.mem_append.mp hx with hx | hx
  · exact List.mem_append_left _ (sSorted_of_field h₁ hx)
  · exact List.mem_append_right _ (sSorted_of_field h₂ hx)

theorem EqEmit.res_sub {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} (h : EqEmit s₁ s₂ τ τ' t₁ t₂) :
    sSorted t₁ ++ sSorted t₂ ⊆ sSorted s₁ ++ sSorted s₂ := fun x hx => by
  obtain ⟨-, h₁, -, h₂⟩ := h
  rcases List.mem_append.mp hx with hx | hx
  · exact List.mem_append_left _ (sSorted_sub_of_atoms h₁ hx)
  · exact List.mem_append_right _ (sSorted_sub_of_atoms h₂ hx)

theorem sSorted_sub_pair {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h₁ : ∀ a ∈ t₁, a ∈ s₁) (h₂ : ∀ a ∈ t₂, a ∈ s₂) :
    sSorted t₁ ++ sSorted t₂ ⊆ sSorted s₁ ++ sSorted s₂ := fun x hx => by
  rcases List.mem_append.mp hx with hx | hx
  · exact List.mem_append_left _ (sSorted_sub_of_atoms h₁ hx)
  · exact List.mem_append_right _ (sSorted_sub_of_atoms h₂ hx)

------------------------- THE INVARIANT ----------------------------------------

/-- `x` occurs, sort-tagged, in some binding of `s`. -/
def Sol.BVar {B : Type} (s : Sol B) (x : Bool × TyVar) : Prop :=
  (∃ p ∈ s.ty, x ∈ Ty.sortedFtv p.2) ∨ (∃ p ∈ s.row, x ∈ Row.sortedFtv p.2)

/-- `s` is a GOOD solution over the tagged variable set `V`: its keys and
everything its bindings mention lie in `V`, no binding mentions a key, and two
bindings of one key agree. -/
structure Sol.Good {B : Type} (V : List (Bool × TyVar)) (s : Sol B) : Prop where
  dom  : ∀ x ∈ s.domS, x ∈ V
  rng  : ∀ x, s.BVar x → x ∈ V ∧ x ∉ s.domS
  fty  : ∀ p ∈ s.ty,  ∀ q ∈ s.ty,  p.1 = q.1 → p.2 = q.2
  frow : ∀ p ∈ s.row, ∀ q ∈ s.row, p.1 = q.1 → p.2 = q.2

theorem Sol.Good.mono {B : Type} {V W : List (Bool × TyVar)} {s : Sol B}
    (h : s.Good V) (hW : V ⊆ W) : s.Good W :=
  ⟨fun x hx => hW (h.dom x hx), fun x hx => ⟨hW (h.rng x hx).1, (h.rng x hx).2⟩,
   h.fty, h.frow⟩

theorem Sol.good_nil {B : Type} (V : List (Bool × TyVar)) : (Sol.nil : Sol B).Good V :=
  ⟨fun x hx => by simp [Sol.domS, Sol.nil] at hx,
   fun x hx => by rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, -⟩ <;> simp [Sol.nil] at hp,
   fun p hp => by simp [Sol.nil] at hp, fun p hp => by simp [Sol.nil] at hp⟩

-- ⊢  what `toSubst s` puts at a tagged variable: the variable itself (if it is
--    not a key), or something a binding mentions (if it is)
theorem Sol.ftvAt_toSubst {B : Type} {s : Sol B} {x y : Bool × TyVar}
    (h : x ∈ s.toSubst.ftvAt y) : (x = y ∧ y ∉ s.domS) ∨ (s.BVar x ∧ y ∈ s.domS) := by
  obtain ⟨b, α⟩ := y
  cases b with
  | false =>
      change x ∈ Ty.sortedFtv (tyLookup α s.ty) at h
      rcases tyLookup_cases s.ty α with ⟨hnm, he⟩ | ⟨p, hp, h1, h2⟩
      · rw [he] at h
        simp only [Ty.sortedFtv, List.mem_singleton] at h
        exact .inl ⟨h, fun hd => hnm (Sol.domS_ty_of_mem hd)⟩
      · rw [h2] at h
        exact .inr ⟨.inl ⟨p, hp, h⟩, Sol.mem_domS_ty (List.mem_map.2 ⟨p, hp, h1⟩)⟩
  | true =>
      change x ∈ Row.sortedFtv (rowLookup α s.row) at h
      rcases rowLookup_cases s.row α with ⟨hnm, he⟩ | ⟨p, hp, h1, h2⟩
      · rw [he] at h
        simp only [Row.sortedFtv, List.mem_singleton] at h
        exact .inl ⟨h, fun hd => hnm (Sol.domS_row_of_mem hd)⟩
      · rw [h2] at h
        exact .inr ⟨.inr ⟨p, hp, h⟩, Sol.mem_domS_row (List.mem_map.2 ⟨p, hp, h1⟩)⟩

-- ⊢  applying a good solution to something inside `V` stays inside `V` and
--    clears every key — the RESIDUAL a later stage sees
theorem Sol.Good.clears {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (hg : s.Good V) {y x : Bool × TyVar} (hy : y ∈ V)
    (hx : x ∈ s.toSubst.ftvAt y) : x ∈ V ∧ x ∉ s.domS := by
  rcases Sol.ftvAt_toSubst hx with ⟨rfl, hnd⟩ | ⟨hb, -⟩
  · exact ⟨hy, hnd⟩
  · exact hg.rng x hb

theorem Sol.Good.clears_ty {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (hg : s.Good V) {τ : Ty B} (hτ : Ty.sortedFtv τ ⊆ V) {x : Bool × TyVar}
    (hx : x ∈ Ty.sortedFtv (τ.applySubst s.toSubst)) : x ∈ V ∧ x ∉ s.domS := by
  obtain ⟨y, hy, hxy⟩ := Ty.mem_sortedFtv_applySubst τ hx
  exact hg.clears (hτ hy) hxy

theorem Sol.Good.clears_spine {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (hg : s.Good V) {t : List (Atom B)} (ht : sSorted t ⊆ V) {x : Bool × TyVar}
    (hx : x ∈ sSorted (sApplySubst s.toSubst t)) : x ∈ V ∧ x ∉ s.domS := by
  obtain ⟨y, hy, hxy⟩ := mem_sSorted_sApplySubst t hx
  exact hg.clears (ht hy) hxy

theorem Sol.mem_domS_comp {B : Type} {s₁ s₂ : Sol B} {x : Bool × TyVar}
    (h : x ∈ (s₂.comp s₁).domS) : x ∈ s₁.domS ∨ x ∈ s₂.domS := by
  obtain ⟨b, α⟩ := x
  cases b with
  | false =>
      have := Sol.domS_ty_of_mem h
      simp only [Sol.comp, List.map_append, List.map_map, List.mem_append] at this
      rcases this with hh | hh
      · exact .inl (Sol.mem_domS_ty (by simpa using hh))
      · exact .inr (Sol.mem_domS_ty hh)
  | true =>
      have := Sol.domS_row_of_mem h
      simp only [Sol.comp, List.map_append, List.map_map, List.mem_append] at this
      rcases this with hh | hh
      · exact .inl (Sol.mem_domS_row (by simpa using hh))
      · exact .inr (Sol.mem_domS_row hh)

-- ⊢  COMPOSITION. If the later stage's keys and mentions avoid the earlier
--    stage's keys — which is what solve-and-apply guarantees, by `clears` —
--    the composite is good.
theorem Sol.Good.comp {B : Type} {V : List (Bool × TyVar)} {s₁ s₂ : Sol B}
    (g₁ : s₁.Good V) (g₂ : s₂.Good V)
    (hdom : ∀ x ∈ s₂.domS, x ∉ s₁.domS) (hrng : ∀ x, s₂.BVar x → x ∉ s₁.domS) :
    (s₂.comp s₁).Good V := by
  -- a mention of a pushed-through earlier binding
  have pushed : ∀ y x, s₁.BVar y → x ∈ s₂.toSubst.ftvAt y →
      x ∈ V ∧ x ∉ (s₂.comp s₁).domS := by
    intro y x hy hx
    obtain ⟨hyV, hyd⟩ := g₁.rng y hy
    rcases Sol.ftvAt_toSubst hx with ⟨rfl, hnd⟩ | ⟨hb, -⟩
    · refine ⟨hyV, fun hc => ?_⟩
      rcases Sol.mem_domS_comp hc with hc | hc
      · exact hyd hc
      · exact hnd hc
    · obtain ⟨hxV, hxd⟩ := g₂.rng x hb
      refine ⟨hxV, fun hc => ?_⟩
      rcases Sol.mem_domS_comp hc with hc | hc
      · exact hrng x hb hc
      · exact hxd hc
  -- a mention of a later binding
  have later : ∀ x, s₂.BVar x → x ∈ V ∧ x ∉ (s₂.comp s₁).domS := by
    intro x hb
    obtain ⟨hxV, hxd⟩ := g₂.rng x hb
    refine ⟨hxV, fun hc => ?_⟩
    rcases Sol.mem_domS_comp hc with hc | hc
    · exact hrng x hb hc
    · exact hxd hc
  refine ⟨fun x hx => ?_, fun x hx => ?_, fun p hp q hq he => ?_, fun p hp q hq he => ?_⟩
  · rcases Sol.mem_domS_comp hx with hx | hx
    · exact g₁.dom x hx
    · exact g₂.dom x hx
  · rcases hx with ⟨p, hp, hx⟩ | ⟨p, hp, hx⟩
    · simp only [Sol.comp, List.mem_append] at hp
      rcases hp with hp | hp
      · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
        obtain ⟨y, hy, hxy⟩ := Ty.mem_sortedFtv_applySubst q.2 hx
        exact pushed y x (.inl ⟨q, hq, hy⟩) hxy
      · exact later x (.inl ⟨p, hp, hx⟩)
    · simp only [Sol.comp, List.mem_append] at hp
      rcases hp with hp | hp
      · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
        obtain ⟨y, hy, hxy⟩ := Row.mem_sortedFtv_applySubst q.2 hx
        exact pushed y x (.inr ⟨q, hq, hy⟩) hxy
      · exact later x (.inr ⟨p, hp, hx⟩)
  · simp only [Sol.comp, List.mem_append] at hp hq
    rcases hp with hp | hp <;> rcases hq with hq | hq
    · obtain ⟨p', hp', rfl⟩ := List.mem_map.mp hp
      obtain ⟨q', hq', rfl⟩ := List.mem_map.mp hq
      simp only at he ⊢
      rw [g₁.fty p' hp' q' hq' he]
    · obtain ⟨p', hp', rfl⟩ := List.mem_map.mp hp
      exact absurd (Sol.mem_domS_ty (List.mem_map.2 ⟨p', hp', rfl⟩))
        (by simp only at he; rw [he]
            exact hdom _ (Sol.mem_domS_ty (List.mem_map.2 ⟨q, hq, rfl⟩)))
    · obtain ⟨q', hq', rfl⟩ := List.mem_map.mp hq
      exact absurd (Sol.mem_domS_ty (List.mem_map.2 ⟨q', hq', rfl⟩))
        (by simp only at he; rw [← he]
            exact hdom _ (Sol.mem_domS_ty (List.mem_map.2 ⟨p, hp, rfl⟩)))
    · exact g₂.fty p hp q hq he
  · simp only [Sol.comp, List.mem_append] at hp hq
    rcases hp with hp | hp <;> rcases hq with hq | hq
    · obtain ⟨p', hp', rfl⟩ := List.mem_map.mp hp
      obtain ⟨q', hq', rfl⟩ := List.mem_map.mp hq
      simp only at he ⊢
      rw [g₁.frow p' hp' q' hq' he]
    · obtain ⟨p', hp', rfl⟩ := List.mem_map.mp hp
      exact absurd (Sol.mem_domS_row (List.mem_map.2 ⟨p', hp', rfl⟩))
        (by simp only at he; rw [he]
            exact hdom _ (Sol.mem_domS_row (List.mem_map.2 ⟨q, hq, rfl⟩)))
    · obtain ⟨q', hq', rfl⟩ := List.mem_map.mp hq
      exact absurd (Sol.mem_domS_row (List.mem_map.2 ⟨q', hq', rfl⟩))
        (by simp only at he; rw [← he]
            exact hdom _ (Sol.mem_domS_row (List.mem_map.2 ⟨p, hp, rfl⟩)))
    · exact g₂.frow p hp q hq he

------------------------- WHAT A GOOD SOLUTION IS ------------------------------

-- ⊢  a good solution is FULLY APPLIED — idempotent, so ⟦S⟧ is `toSubst`
theorem Sol.Good.applied {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (hg : s.Good V) : s.Applied := by
  have fix_ty : ∀ α, (false, α) ∉ s.domS → s.toSubst.ty α = .var α :=
    fun α h => tyLookup_not_mem _ (fun hm => h (Sol.mem_domS_ty hm))
  have fix_row : ∀ α, (true, α) ∉ s.domS → s.toSubst.row α = .var α :=
    fun α h => rowLookup_not_mem _ (fun hm => h (Sol.mem_domS_row hm))
  exact ⟨fun p hp => Ty.applySubst_fixed_sorted p.2
          (fun α hα => fix_ty α (hg.rng _ (.inl ⟨p, hp, hα⟩)).2)
          (fun α hα => fix_row α (hg.rng _ (.inl ⟨p, hp, hα⟩)).2),
         fun p hp => Row.applySubst_fixed_sorted p.2
          (fun α hα => fix_ty α (hg.rng _ (.inr ⟨p, hp, hα⟩)).2)
          (fun α hα => fix_row α (hg.rng _ (.inr ⟨p, hp, hα⟩)).2)⟩

-- ⊢  …WELL-FORMED, at rank ≡ 0: no binding mentions a key, so the descent
--    condition of `Ranked` is vacuous
theorem Sol.Good.wf {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (hg : s.Good V) : s.WF where
  acyclic := by
    intro p hp β hβ hmem
    have hs : (true, β) ∈ Row.sortedFtv p.2 := by
      rw [← sSorted_toSpine]; exact sVarSeq_mem_sSorted _ hβ
    exact (hg.rng _ (.inr ⟨p, hp, hs⟩)).2 (Sol.mem_domS_row hmem)
  ranked := by
    refine ⟨fun _ => 0, fun x hx => ?_, fun p hp x hx hxd => ?_, fun p hp x hx hxd => ?_⟩
    · cases hlen : s.domS with
      | nil => rw [hlen] at hx; cases hx
      | cons _ _ => simp
    · exact absurd hxd (hg.rng x (.inl ⟨p, hp, hx⟩)).2
    · exact absurd hxd (hg.rng x (.inr ⟨p, hp, hx⟩)).2

-- ⊢  …and SATISFIABLE, by its own substitution. The success legs of the
--    trichotomy are vacuous on an unsatisfiable solution; this rules that out.
theorem Sol.Good.sat {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (hg : s.Good V) : Sol.Sat s.toSubst s := by
  have ha := hg.applied
  refine ⟨fun p hp => ?_, fun p hp => ?_⟩
  · rw [ha.1 p hp]
    change TyEquiv (tyLookup p.1 s.ty) p.2
    rcases tyLookup_cases s.ty p.1 with ⟨hnm, -⟩ | ⟨q, hq, h1, h2⟩
    · exact absurd (List.mem_map.2 ⟨p, hp, rfl⟩) hnm
    · rw [h2, hg.fty q hq p hp h1]; exact TyEquiv.refl _
  · rw [ha.2 p hp]
    change RowEquiv (rowLookup p.1 s.row) p.2
    rcases rowLookup_cases s.row p.1 with ⟨hnm, -⟩ | ⟨q, hq, h1, h2⟩
    · exact absurd (List.mem_map.2 ⟨p, hp, rfl⟩) hnm
    · rw [h2, hg.frow q hq p hp h1]; exact RowEquiv.refl _

------------------------- THE BASE ARMS ----------------------------------------

-- ⊢  U-var at the type sort: α ≔ τ with α ∉ τ at the TYPE sort
theorem Sol.good_bindTy {B : Type} {S : Supply} {α : TyVar} {τ : Ty B}
    {V : List (Bool × TyVar)} {s : Sol B} {S' : Supply}
    (h : bindTy S α τ = .success s S') (hα : (false, α) ∈ V)
    (hτ : Ty.sortedFtv τ ⊆ V) : s.Good V := by
  unfold bindTy at h
  split at h
  · simp only [UResM.success.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    exact Sol.good_nil V
  · split at h
    · cases h
    · next hocc =>
      simp only [UResM.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      have hdom : ∀ x, x ∈ Sol.domS (⟨[(α, τ)], []⟩ : Sol B) → x = (false, α) := by
        intro x hx; simpa [Sol.domS] using hx
      refine ⟨fun x hx => by rw [hdom x hx]; exact hα, fun x hx => ?_,
              fun p hp q hq _ => by
                rw [List.mem_singleton.mp hp, List.mem_singleton.mp hq],
              fun p hp => by simp at hp⟩
      rcases hx with ⟨p, hp, hx⟩ | ⟨p, hp, -⟩
      · obtain rfl := List.mem_singleton.mp hp
        refine ⟨hτ hx, fun hd => hocc ?_⟩
        rw [hdom x hd] at hx
        exact List.elem_eq_true_of_mem ((Ty.mem_tyFtv_iff_sortedFtv τ).mp hx)
      · simp at hp

-- ⊢  bindings of spine variables to ε
theorem Sol.good_ofRow_eps {B : Type} {σ : List (TyVar × Row B)}
    {V : List (Bool × TyVar)} (h : ∀ p ∈ σ, (true, p.1) ∈ V ∧ p.2 = .empty) :
    (Sol.ofRow σ).Good V := by
  refine ⟨fun x hx => ?_, fun x hx => ?_, fun p hp => by simp [Sol.ofRow] at hp,
          fun p hp q hq _ => by rw [(h p hp).2, (h q hq).2]⟩
  · simp only [Sol.domS, Sol.ofRow, List.map_nil, List.nil_append, List.mem_map] at hx
    obtain ⟨p, hp, rfl⟩ := hx
    exact (h p hp).1
  · rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, hx⟩
    · simp [Sol.ofRow] at hp
    · rw [(h p hp).2] at hx; simp [Row.sortedFtv] at hx

theorem allVarsEmpty_sorted {B : Type} : (s : List (Atom B)) →
    {σ : List (TyVar × Row B)} → allVarsEmpty s = some σ →
    ∀ p ∈ σ, (true, p.1) ∈ sSorted s ∧ p.2 = Row.empty
  | [], σ, h, p, hp => by simp only [allVarsEmpty, Option.some.injEq] at h; cases h; cases hp
  | .field _ _ :: _, σ, h, p, hp => by simp [allVarsEmpty] at h
  | .var α :: s, σ, h, p, hp => by
      simp only [allVarsEmpty, Option.map_eq_some_iff] at h
      obtain ⟨σ', hs, rfl⟩ := h
      rcases List.mem_cons.mp hp with rfl | hp
      · exact ⟨List.mem_cons_self, rfl⟩
      · obtain ⟨h₁, h₂⟩ := allVarsEmpty_sorted s hs p hp
        exact ⟨List.mem_cons_of_mem _ h₁, h₂⟩

-- ⊢  U-var-solve (and the ε-collapse it tries first)
theorem Sol.good_solveVarM {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)}
    {s : Sol B} {S' : Supply} (h : solveVarM S s₁ s₂ = some (.success s S')) :
    s.Good (sSorted s₁ ++ sSorted s₂) := by
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
        · next σ hc =>
            simp only [Option.some.injEq, UResM.success.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            obtain ⟨-, -, he⟩ := collapseSol_spec hc
            refine Sol.good_ofRow_eps (fun p hp => ?_)
            obtain ⟨h₁, -, h₃⟩ := epsCollapse_mem s₂ he p hp
            exact ⟨List.mem_append_right _ (sVarSeq_mem_sSorted s₂ h₁), h₃⟩
        · split at h
          · simp at h
          · next hocc =>
            simp only [Option.some.injEq, UResM.success.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            have hdom : ∀ x, x ∈ (Sol.ofRow [(α, ofSpine s₂)] : Sol B).domS →
                x = (true, α) := by
              intro x hx; simpa [Sol.domS, Sol.ofRow] using hx
            refine ⟨fun x hx => by
                      rw [hdom x hx]; exact List.mem_append_left _ List.mem_cons_self,
                    fun x hx => ?_, fun p hp => by simp [Sol.ofRow] at hp,
                    fun p hp q hq _ => by
                      simp only [Sol.ofRow, List.mem_singleton] at hp hq
                      rw [hp, hq]⟩
            rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, hx⟩
            · simp [Sol.ofRow] at hp
            · simp only [Sol.ofRow, List.mem_singleton] at hp
              subst hp
              simp only [sortedFtv_ofSpine] at hx
              refine ⟨List.mem_append_right _ hx, fun hd => hocc ?_⟩
              rw [hdom x hd, ← sortedFtv_ofSpine] at hx
              exact List.elem_eq_true_of_mem
                ((Row.mem_allRowVars_iff_sortedFtv (ofSpine s₂)).mp hx)

------------------------- THE INDUCTION ----------------------------------------

-- The two type-sort shapes on which `unifyTyF` recurses, as a Bool, so the
-- induction can split on them without classical case analysis.
def tyRec {B : Type} : Ty B → Ty B → Bool
  | .fn _ _, .fn _ _ => true
  | .rcd _, .rcd _   => true
  | _, _             => false

theorem tyRec_true {B : Type} {τ τ' : Ty B} (h : tyRec τ τ' = true) :
    (∃ a₁ b₁ a₂ b₂, τ = .fn a₁ b₁ ∧ τ' = .fn a₂ b₂) ∨
    (∃ ρ₁ ρ₂, τ = .rcd ρ₁ ∧ τ' = .rcd ρ₂) := by
  cases τ <;> cases τ' <;> simp [tyRec] at h
  · exact .inl ⟨_, _, _, _, rfl, rfl⟩
  · exact .inr ⟨_, _, rfl, rfl⟩

-- ⊢  EVERY SUCCESS IS GOOD, over the problem's own tagged variables, at both
--    sorts. The shape is `unifyM_bounded`'s (Completeness.lean); what is new is
--    the arm, where `clears` shows the residual avoids the first stage's keys,
--    which is exactly the side condition `Sol.Good.comp` needs.
theorem unifyM_good {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
        unifyTyF S fuel τ τ' = .success s S' →
        s.Good (Ty.sortedFtv τ ++ Ty.sortedFtv τ')) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply},
        unifySpineMF S fuel s₁ s₂ = .success s S' →
        s.Good (sSorted s₁ ++ sSorted s₂)) := by
  have hbL : ∀ (S : Supply) (α : TyVar) (τ : Ty B) {s : Sol B} {S' : Supply},
      bindTy S α τ = .success s S' → s.Good (Ty.sortedFtv (Ty.var (B := B) α) ++ Ty.sortedFtv τ) :=
    fun S α τ s S' h => Sol.good_bindTy h
      (List.mem_append_left _ (by simp [Ty.sortedFtv])) (fun _ hx => List.mem_append_right _ hx)
  have hbR : ∀ (S : Supply) (α : TyVar) (τ : Ty B) {s : Sol B} {S' : Supply},
      bindTy S α τ = .success s S' → s.Good (Ty.sortedFtv τ ++ Ty.sortedFtv (Ty.var (B := B) α)) :=
    fun S α τ s S' h => Sol.good_bindTy h
      (List.mem_append_right _ (by simp [Ty.sortedFtv])) (fun _ hx => List.mem_append_left _ hx)
  have hnilL : ∀ (S : Supply) (s₂ : List (Atom B)) (fuel : Nat) {s : Sol B} {S' : Supply},
      unifySpineMF S fuel [] s₂ = .success s S' → s.Good (sSorted (B := B) [] ++ sSorted s₂) := by
    intro S s₂ fuel s S' h
    simp only [unifySpineMF] at h
    cases hae : allVarsEmpty s₂ with
    | none => simp [hae] at h
    | some σ' =>
        simp only [hae, UResM.success.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact Sol.good_ofRow_eps (fun p hp =>
          ⟨List.mem_append_right _ (allVarsEmpty_sorted s₂ hae p hp).1,
           (allVarsEmpty_sorted s₂ hae p hp).2⟩)
  have hnilR : ∀ (S : Supply) (a : Atom B) (s₁ : List (Atom B)) (fuel : Nat) {s : Sol B}
      {S' : Supply}, unifySpineMF S fuel (a :: s₁) [] = .success s S' →
      s.Good (sSorted (a :: s₁) ++ sSorted (B := B) []) := by
    intro S a s₁ fuel s S' h
    simp only [unifySpineMF] at h
    cases hae : allVarsEmpty (a :: s₁) with
    | none => simp [hae] at h
    | some σ' =>
        simp only [hae, UResM.success.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact Sol.good_ofRow_eps (fun p hp =>
          ⟨List.mem_append_left _ (allVarsEmpty_sorted (a :: s₁) hae p hp).1,
           (allVarsEmpty_sorted (a :: s₁) hae p hp).2⟩)
  -- the type-sort arms that do not recurse, at any fuel
  have hflat : ∀ (fuel : Nat) (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
      tyRec τ τ' = false →
      unifyTyF S fuel τ τ' = .success s S' →
      s.Good (Ty.sortedFtv τ ++ Ty.sortedFtv τ') := by
    intro fuel S τ τ' s S' hrec h
    cases τ with
    | var α => cases fuel <;> exact hbL S α τ' h
    | base b =>
        cases τ' with
        | var α => cases fuel <;> exact hbR S α _ h
        | base b' =>
            by_cases hb : b = b'
            · subst hb
              have hred : unifyTyF S fuel (Ty.base b) (Ty.base b)
                  = .success (Sol.nil (B := B)) S := by
                cases fuel <;> simp [unifyTyF]
              rw [hred] at h
              simp only [UResM.success.injEq] at h
              obtain ⟨rfl, rfl⟩ := h
              exact Sol.good_nil _
            · cases fuel <;> simp [unifyTyF, hb] at h
        | unk => cases fuel <;> cases h
        | fn _ _ => cases fuel <;> cases h
        | rcd _ => cases fuel <;> cases h
    | unk =>
        cases τ' with
        | var α => cases fuel <;> exact hbR S α _ h
        | base _ => cases fuel <;> cases h
        | unk =>
            have hred : unifyTyF S fuel (Ty.unk : Ty B) Ty.unk
                = .success (Sol.nil (B := B)) S := by cases fuel <;> simp [unifyTyF]
            rw [hred] at h
            simp only [UResM.success.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            exact Sol.good_nil _
        | fn _ _ => cases fuel <;> cases h
        | rcd _ => cases fuel <;> cases h
    | fn a₁ b₁ =>
        cases τ' with
        | var α => cases fuel <;> exact hbR S α _ h
        | base _ => cases fuel <;> cases h
        | unk => cases fuel <;> cases h
        | fn a₂ b₂ => simp [tyRec] at hrec
        | rcd _ => cases fuel <;> cases h
    | rcd ρ₁ =>
        cases τ' with
        | var α => cases fuel <;> exact hbR S α _ h
        | base _ => cases fuel <;> cases h
        | unk => cases fuel <;> cases h
        | fn _ _ => cases fuel <;> cases h
        | rcd ρ₂ => simp [tyRec] at hrec
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' s S' h => ?_, fun S s₁ s₂ s S' h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => exact hflat 0 S τ τ' hrec h
        | true =>
          rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
          · cases h
          · cases h
      · cases s₁ with
        | nil => exact hnilL S s₂ 0 h
        | cons a s₁ =>
          cases s₂ with
          | nil => exact hnilR S a s₁ 0 h
          | cons b s₂ => cases h
  | succ fuel ih =>
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B))
          (Q : List (Bool × TyVar)) {s : Sol B} {S' : Supply},
          Ty.sortedFtv τ ++ Ty.sortedFtv τ' ⊆ Q → sSorted t₁ ++ sSorted t₂ ⊆ Q →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' → s.Good Q := by
        intro S τ τ' t₁ t₂ Q s S' hQt hQr h
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        have g₁ := (ih.1 S τ τ' hty).mono hQt
        have hR : ∀ x ∈ sSorted (sApplySubst s₁.toSubst t₁) ++
            sSorted (sApplySubst s₁.toSubst t₂), x ∈ Q ∧ x ∉ s₁.domS := by
          intro x hx
          rcases List.mem_append.mp hx with hx | hx
          · exact g₁.clears_spine (fun _ hy => hQr (List.mem_append_left _ hy)) hx
          · exact g₁.clears_spine (fun _ hy => hQr (List.mem_append_right _ hy)) hx
        have g₂ := ih.2 S₁ _ _ hrow
        exact g₁.comp (g₂.mono fun x hx => (hR x hx).1)
          (fun x hx => (hR x (g₂.dom x hx)).2) (fun x hx => (hR x (g₂.rng x hx).1).2)
      refine ⟨fun S τ τ' s S' h => ?_, fun S s₁ s₂ s S' h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => exact hflat (fuel + 1) S τ τ' hrec h
        | true =>
        rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
        · replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
              unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
            = .success s S' := h
          obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
          have hQ : Ty.sortedFtv a₁ ++ Ty.sortedFtv a₂ ⊆
              Ty.sortedFtv (.fn a₁ b₁) ++ Ty.sortedFtv (.fn a₂ b₂) := fun x hx => by
            simp only [Ty.sortedFtv, List.mem_append] at hx ⊢
            rcases hx with hx | hx
            · exact .inl (.inl hx)
            · exact .inr (.inl hx)
          have g₁ := (ih.1 S a₁ a₂ hty).mono hQ
          have hR : ∀ x ∈ Ty.sortedFtv (b₁.applySubst s₁.toSubst) ++
              Ty.sortedFtv (b₂.applySubst s₁.toSubst),
              x ∈ Ty.sortedFtv (.fn a₁ b₁) ++ Ty.sortedFtv (.fn a₂ b₂) ∧
              x ∉ s₁.domS := by
            intro x hx
            rcases List.mem_append.mp hx with hx | hx
            · exact g₁.clears_ty (fun y hy => by
                simp only [Ty.sortedFtv, List.mem_append]; exact .inl (.inr hy)) hx
            · exact g₁.clears_ty (fun y hy => by
                simp only [Ty.sortedFtv, List.mem_append]; exact .inr (.inr hy)) hx
          have g₂ := ih.1 S₁ _ _ hrow
          exact g₁.comp (g₂.mono fun x hx => (hR x hx).1)
            (fun x hx => (hR x (g₂.dom x hx)).2) (fun x hx => (hR x (g₂.rng x hx).1).2)
        · replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
          have g := ih.2 S _ _ h
          rw [sSorted_toSpine, sSorted_toSpine] at g
          exact g
      · cases s₁ with
        | nil => exact hnilL S s₂ _ h
        | cons a s₁ =>
          cases s₂ with
          | nil => exact hnilR S a s₁ _ h
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              exact (ih.2 S t₁ t₂ h).mono
                (sSorted_sub_pair (stripL_atoms hsl).1 (stripL_atoms hsl).2)
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              exact (ih.2 S t₁ t₂ h).mono
                (sSorted_sub_pair (stripR_atoms hsr).1 (stripR_atoms hsr).2)
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact Sol.good_solveVarM (hv1.trans (congrArg some h))
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact (Sol.good_solveVarM (hv2.trans (congrArg some h))).mono
                (fun x hx => by
                  rcases List.mem_append.mp hx with hx | hx
                  · exact List.mem_append_right _ hx
                  · exact List.mem_append_left _ hx)
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchL_atoms hml).ty_sub
                (matchL_atoms hml).res_sub h
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchL_atoms hml2).swap.ty_sub
                (matchL_atoms hml2).swap.res_sub h
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchR_atoms hmr).ty_sub
                (matchR_atoms hmr).res_sub h
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchR_atoms hmr2).swap.ty_sub
                (matchR_atoms hmr2).swap.res_sub h
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              exact arm S τ0 τ0' t₁ t₂ _ (groundMatch_atoms hg).ty_sub
                (groundMatch_atoms hg).res_sub h
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (groundMatch_atoms hg2).swap.ty_sub
                (groundMatch_atoms hg2).swap.res_sub h
            | none =>
            cases hpc : projClash (a :: s₁) (b :: s₂) with
            | true =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc] at h
              cases h
            | false =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc] at h
              cases h

------------------------- AT THE ENTRY POINTS ----------------------------------

theorem unifyRowM_good {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    s.Good (Row.sortedFtv ρ₁ ++ Row.sortedFtv ρ₂) := by
  unfold unifyRowM unifySpineM at h
  have g := (unifyM_good fuel).2 _ _ _ h
  rw [sSorted_toSpine, sSorted_toSpine] at g
  exact g

-- ⊢  `UnifyWF` and `UnifyAcyclic` (State.lean), no longer open
theorem unifyWF (B : Type) [DecidableEq B] : UnifyWF B :=
  fun _ _ _ _ _ h => (unifyRowM_good h).wf

theorem unifyAcyclic (B : Type) [DecidableEq B] : UnifyAcyclic B :=
  fun _ _ _ _ _ h => (unifyRowM_good h).wf.acyclic

-- ⊢  the returned solution is fully applied, at both entry points the
--    inference layer uses (`SolveTy` / `SolveRow` call `unifyTyF` /
--    `unifySpineMF` on the state's supply)
theorem unifyTyF_success_applied {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {s : Sol B} {S' : Supply} (h : unifyTyF S fuel τ τ' = .success s S') :
    s.Applied := ((unifyM_good fuel).1 _ _ _ h).applied

theorem unifySpineMF_success_applied {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel s₁ s₂ = .success s S') : s.Applied :=
  ((unifyM_good fuel).2 _ _ _ h).applied

theorem unifyTyF_success_wf {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {s : Sol B} {S' : Supply} (h : unifyTyF S fuel τ τ' = .success s S') :
    s.WF := ((unifyM_good fuel).1 _ _ _ h).wf

theorem unifySpineMF_success_wf {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel s₁ s₂ = .success s S') : s.WF :=
  ((unifyM_good fuel).2 _ _ _ h).wf

-- ⊢  NON-VACUITY: a success is never an unsatisfiable solution. Its own
--    substitution satisfies it — and therefore UNIFIES the problem, by
--    success soundness. Before this, both mgu legs were vacuously true of a
--    success on an unsolvable input.
theorem unifyRowM_success_sat {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    Sol.Sat s.toSubst s := (unifyRowM_good h).sat

theorem unifyRowM_success_unifies {B : Type} [DecidableEq B] {fuel : Nat}
    {ρ₁ ρ₂ : Row B} {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    Unifies s.toSubst ρ₁ ρ₂ := unifyRowM_success_sound h (unifyRowM_success_sat h)

theorem unifyTyF_success_unifies {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {s : Sol B} {S' : Supply} (h : unifyTyF S fuel τ τ' = .success s S') :
    TyUnifies s.toSubst τ τ' :=
  (unifyM_success_sound fuel).1 _ _ _ h ((unifyM_good fuel).1 _ _ _ h).sat

-- ⊢  …so a success MEANS the problem is solvable, and `s.toSubst` is an mgu of
--    it: a unifier, and every unifier extends one that meets `s`.
theorem unifyRowM_success_mgu {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    Unifies s.toSubst ρ₁ ρ₂ ∧
    (∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ → ∃ θ' : TySubst B,
        AgreeOn θ θ' (sFtv ρ₁.toSpine ++ sFtv ρ₂.toSpine) ∧ Sol.Sat θ' s) :=
  ⟨unifyRowM_success_unifies h, (unifyRowM_success_iff h).2⟩

------------------------- THE SOLVER STATE: `Sol.Clean` ------------------------
-- The inference layer composes solutions exactly as `.seq` does —
-- `SolverState.extend S s = s.comp S.sol` (Infer.lean) — and solves each new
-- equation on the SUBSTITUTED problem. So the same argument keeps the state's
-- solution idempotent. There is no fixed problem to bound it by, so this is
-- `Good` without its `V`: no binding mentions a key, and keys are consistent.

def Sol.Clean {B : Type} (s : Sol B) : Prop :=
  (∀ x, s.BVar x → x ∉ s.domS) ∧
  (∀ p ∈ s.ty,  ∀ q ∈ s.ty,  p.1 = q.1 → p.2 = q.2) ∧
  (∀ p ∈ s.row, ∀ q ∈ s.row, p.1 = q.1 → p.2 = q.2)

/-- Every tagged variable `s` mentions, keys included — the `V` a clean
solution is good over. -/
def Sol.allS {B : Type} (s : Sol B) : List (Bool × TyVar) :=
  s.domS ++ s.ty.flatMap (fun p => Ty.sortedFtv p.2) ++
    s.row.flatMap (fun p => Row.sortedFtv p.2)

theorem Sol.Good.clean {B : Type} {V : List (Bool × TyVar)} {s : Sol B}
    (h : s.Good V) : s.Clean :=
  ⟨fun x hx => (h.rng x hx).2, h.fty, h.frow⟩

theorem Sol.Clean.good {B : Type} {s : Sol B} (h : s.Clean) : s.Good s.allS := by
  refine ⟨fun x hx => List.mem_append_left _ (List.mem_append_left _ hx),
          fun x hx => ⟨?_, h.1 x hx⟩, h.2.1, h.2.2⟩
  rcases hx with ⟨p, hp, hx⟩ | ⟨p, hp, hx⟩
  · exact List.mem_append_left _ (List.mem_append_right _ (List.mem_flatMap.2 ⟨p, hp, hx⟩))
  · exact List.mem_append_right _ (List.mem_flatMap.2 ⟨p, hp, hx⟩)

theorem Sol.clean_nil {B : Type} : (Sol.nil : Sol B).Clean := (Sol.good_nil []).clean

theorem Sol.Clean.applied {B : Type} {s : Sol B} (h : s.Clean) : s.Applied := h.good.applied

theorem Sol.Clean.wf {B : Type} {s : Sol B} (h : s.Clean) : s.WF := h.good.wf

theorem Sol.Clean.sat {B : Type} {s : Sol B} (h : s.Clean) : Sol.Sat s.toSubst s :=
  h.good.sat

-- ⊢  applying a clean solution clears its keys, at both sorts
theorem Sol.Clean.clears_ty {B : Type} {s : Sol B} (h : s.Clean) {τ : Ty B}
    {x : Bool × TyVar} (hx : x ∈ Ty.sortedFtv (τ.applySubst s.toSubst)) : x ∉ s.domS :=
  ((h.good.mono (V := s.allS) (W := s.allS ++ Ty.sortedFtv τ)
      (fun _ hy => List.mem_append_left _ hy)).clears_ty
    (fun _ hy => List.mem_append_right _ hy) hx).2

theorem Sol.Clean.clears_row {B : Type} {s : Sol B} (h : s.Clean) {ρ : Row B}
    {x : Bool × TyVar} (hx : x ∈ Row.sortedFtv (ρ.applySubst s.toSubst)) : x ∉ s.domS := by
  obtain ⟨y, hy, hxy⟩ := Row.mem_sortedFtv_applySubst ρ hx
  exact ((h.good.mono (W := s.allS ++ Row.sortedFtv ρ)
      (fun _ hy => List.mem_append_left _ hy)).clears
    (List.mem_append_right _ hy) hxy).2

-- ⊢  COMPOSITION, V-free
theorem Sol.Clean.comp {B : Type} {s₁ s₂ : Sol B} (h₁ : s₁.Clean) (h₂ : s₂.Clean)
    (hdom : ∀ x ∈ s₂.domS, x ∉ s₁.domS) (hrng : ∀ x, s₂.BVar x → x ∉ s₁.domS) :
    (s₂.comp s₁).Clean :=
  (Sol.Good.comp (h₁.good.mono (fun _ hx => List.mem_append_left _ hx))
    (h₂.good.mono (fun _ hx => List.mem_append_right (s₁.allS) hx)) hdom hrng).clean

-- ⊢  A NEW SOLUTION FOR AN ALREADY-SUBSTITUTED PROBLEM keeps the state clean:
--    whatever `s` binds or mentions is a variable of the substituted problem,
--    and those avoid the state's keys.
theorem Sol.Clean.extend {B : Type} {s₀ s : Sol B} {P : List (Bool × TyVar)}
    (h₀ : s₀.Clean) (hg : s.Good P) (hP : ∀ x ∈ P, x ∉ s₀.domS) : (s.comp s₀).Clean :=
  h₀.comp hg.clean (fun x hx => hP x (hg.dom x hx)) (fun x hx => hP x (hg.rng x hx).1)

end MinimalCalculus
