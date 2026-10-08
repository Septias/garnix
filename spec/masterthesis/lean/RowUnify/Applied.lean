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
def sSorted {B : Type} : List (Atom B) → List (Srt × TyVar)
  | [] => []
  | .field _ τ :: s => Ty.sortedFtv τ ++ sSorted s
  | .var α :: s     => (.row, α) :: sSorted s
  | .dfield o τ :: s => [(.lab, o)] ++ Ty.sortedFtv τ ++ sSorted s

def Atom.sorted {B : Type} : Atom B → List (Srt × TyVar)
  | .field _ τ => Ty.sortedFtv τ
  | .var α     => [(.row, α)]
  | .dfield o τ => [(.lab, o)] ++ Ty.sortedFtv τ

theorem mem_sSorted {B : Type} {x : Srt × TyVar} :
    (s : List (Atom B)) → (x ∈ sSorted s ↔ ∃ a ∈ s, x ∈ a.sorted)
  | [] => by simp [sSorted]
  | .field l τ :: s => by
      simp only [sSorted, List.mem_append, mem_sSorted s, List.mem_cons,
        exists_eq_or_imp, Atom.sorted]
  | .var α :: s => by
      simp only [sSorted, List.mem_cons, mem_sSorted s, exists_eq_or_imp,
        Atom.sorted, List.not_mem_nil, or_false]
  | .dfield o τ :: s => by
      simp only [sSorted, List.mem_append, mem_sSorted s, List.mem_cons,
        exists_eq_or_imp, Atom.sorted]

-- ⊢  a spine built from atoms of another has no variable the other lacks
theorem sSorted_sub_of_atoms {B : Type} {s t : List (Atom B)}
    (h : ∀ a ∈ t, a ∈ s) : sSorted t ⊆ sSorted s := fun x hx => by
  obtain ⟨a, ha, hx⟩ := (mem_sSorted t).mp hx
  exact (mem_sSorted s).mpr ⟨a, h a ha, hx⟩

theorem sSorted_of_field {B : Type} {s : List (Atom B)} {l : Label} {τ : Ty B}
    (h : .field l τ ∈ s) : Ty.sortedFtv τ ⊆ sSorted s := fun x hx =>
  (mem_sSorted s).mpr ⟨.field l τ, h, hx⟩

theorem sSorted_of_dfield {B : Type} {s : List (Atom B)} {α : TyVar} {τ : Ty B}
    (h : .dfield α τ ∈ s) : Ty.sortedFtv τ ⊆ sSorted s := fun x hx =>
  (mem_sSorted s).mpr ⟨.dfield α τ, h, by simp [Atom.sorted, hx]⟩

/-- `τ` is the payload of some field of `s`, literal or keyed. -/
def PayIn {B : Type} (τ : Ty B) (s : List (Atom B)) : Prop :=
  (∃ l, .field l τ ∈ s) ∨ (∃ α, .dfield α τ ∈ s)

theorem PayIn.sorted_sub {B : Type} {τ : Ty B} {s : List (Atom B)} (h : PayIn τ s) :
    Ty.sortedFtv τ ⊆ sSorted s := by
  rcases h with ⟨_, h⟩ | ⟨_, h⟩
  · exact sSorted_of_field h
  · exact sSorted_of_dfield h

theorem PayIn.reverse {B : Type} {τ : Ty B} {s : List (Atom B)} (h : PayIn τ s.reverse) :
    PayIn τ s := by
  rcases h with ⟨l, h⟩ | ⟨α, h⟩
  · exact .inl ⟨l, List.mem_reverse.mp h⟩
  · exact .inr ⟨α, List.mem_reverse.mp h⟩

theorem sSorted_append {B : Type} :
    (s t : List (Atom B)) → sSorted (s ++ t) = sSorted s ++ sSorted t
  | [], _ => rfl
  | .field _ τ :: s, t => by
      simp only [List.cons_append, sSorted, sSorted_append s t, List.append_assoc]
  | .var _ :: s, t => by simp only [List.cons_append, sSorted, sSorted_append s t]
  | .dfield _ τ :: s, t => by
      simp only [List.cons_append, sSorted, sSorted_append s t, List.append_assoc]

-- ⊢  a spine's tagged variables are its row's (a junk key's are forgotten)
theorem sSorted_toSpine {B : Type} : (ρ : Row B) → ∀ x, x ∈ sSorted ρ.toSpine →
    x ∈ Row.sortedFtv ρ
  | .empty => fun _ h => h
  | .var _ => fun _ h => by simpa [Row.toSpine, sSorted, Row.sortedFtv] using h
  | .sing _ τ => fun _ h => by simpa [Row.toSpine, sSorted, Row.sortedFtv] using h
  | .cat ρ₁ ρ₂ => fun x h => by
      simp only [Row.toSpine, sSorted_append, List.mem_append] at h
      simp only [Row.sortedFtv, List.mem_append]
      rcases h with h | h
      · exact .inl (sSorted_toSpine ρ₁ x h)
      · exact .inr (sSorted_toSpine ρ₂ x h)
  | .dsing q τ => fun x h => by
      simp only [Row.toSpine] at h
      simp only [Row.sortedFtv, List.mem_append]
      cases q <;> simp only [Atom.ofKey, sSorted, Key.sortedFtv,
        List.append_nil, List.nil_append, List.mem_append] at h ⊢ <;>
        first | exact .inr h | exact h

theorem sortedFtv_ofSpine {B : Type} : (s : List (Atom B)) →
    Row.sortedFtv (ofSpine s) = sSorted s
  | [] => rfl
  | .field _ τ :: s => by simp only [ofSpine, Row.sortedFtv, sortedFtv_ofSpine s, sSorted]
  | .var _ :: s => by simp only [ofSpine, Row.sortedFtv, sortedFtv_ofSpine s, sSorted]; rfl
  | .dfield _ τ :: s => by
      simp only [ofSpine, Row.sortedFtv, sortedFtv_ofSpine s, sSorted, List.append_assoc]
      rfl

theorem sVarSeq_mem_sSorted {B : Type} {γ : TyVar} :
    (s : List (Atom B)) → γ ∈ sVarSeq s → (.row, γ) ∈ sSorted s
  | [], h => by simp [sVarSeq] at h
  | .field _ _ :: s, h => by
      simp only [sVarSeq] at h
      exact List.mem_append_right _ (sVarSeq_mem_sSorted s h)
  | .dfield _ _ :: s, h => by
      simp only [sVarSeq] at h
      exact List.mem_append_right _ (sVarSeq_mem_sSorted s h)
  | .var δ :: s, h => by
      simp only [sVarSeq, List.mem_cons] at h
      rcases h with rfl | h
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (sVarSeq_mem_sSorted s h)

-- ⊢  every occurrence in a substituted spine comes from one of the original's
theorem mem_sSorted_sApplySubst {B : Type} {θ : TySubst B} {x : Srt × TyVar} :
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
      · exact ⟨(.row, α), List.mem_cons_self, sSorted_toSpine _ _ h⟩
      · obtain ⟨y, hy, hx⟩ := mem_sSorted_sApplySubst t h
        exact ⟨y, List.mem_cons_of_mem _ hy, hx⟩
  | .dfield o τ :: t, h => by
      simp only [sApplySubst] at h
      rw [show sSorted (Atom.ofKey (θ.lab o) (τ.applySubst θ) ::
            sApplySubst θ t) = sSorted [Atom.ofKey (θ.lab o)
              (τ.applySubst θ)] ++ sSorted (sApplySubst θ t) from by
          rw [← sSorted_append]; rfl, List.mem_append] at h
      rcases h with h | h
      · have h' : x ∈ Row.sortedFtv (.dsing (θ.lab o) (τ.applySubst θ)) :=
          sSorted_toSpine _ _ h
        simp only [Row.sortedFtv, List.mem_append] at h'
        rcases h' with h' | h'
        · obtain ⟨y, hy, hx⟩ := Key.mem_sortedFtv_applySubst (θ := θ) (.var o) h'
          have hy' : y = (.lab, o) := by simpa [Key.sortedFtv] using hy
          subst hy'
          exact ⟨_, by simp only [sSorted, List.mem_append]; exact .inl (.inl (by simp)), hx⟩
        · obtain ⟨y, hy, hx⟩ := Ty.mem_sortedFtv_applySubst τ h'
          exact ⟨y, by simp only [sSorted, List.mem_append]; exact .inl (.inr hy), hx⟩
      · obtain ⟨y, hy, hx⟩ := mem_sSorted_sApplySubst t h
        exact ⟨y, by simp only [sSorted, List.mem_append]; exact .inr hy, hx⟩

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
  | .dfield o σ :: t, τ, s', h => by
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
  PayIn τ s₁ ∧ (∀ a ∈ t₁, a ∈ s₁) ∧ PayIn τ' s₂ ∧ (∀ a ∈ t₂, a ∈ s₂)

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
          exact ⟨.inl ⟨l, List.mem_cons_self⟩, fun _ ha => List.mem_cons_of_mem _ ha, .inl hτ, hs⟩
  | .dfield α σ :: u₁ =>
      simp only [matchL] at h
      revert h
      cases hw : keyExtract α s₂ with
      | none => intro h; cases h
      | some p =>
          intro h
          cases h
          rw [keyExtract_inv hw]
          exact ⟨.inr ⟨α, List.mem_cons_self⟩, fun _ ha => List.mem_cons_of_mem _ ha,
                 .inr ⟨α, List.mem_cons_self⟩, fun _ ha => List.mem_cons_of_mem _ ha⟩

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
      obtain ⟨g₀, g₁, g₀', g₂⟩ := matchL_atoms hl
      exact ⟨g₀.reverse,
             fun a ha => List.mem_reverse.mp (g₁ a (List.mem_reverse.mp ha)),
             g₀'.reverse,
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
                exact ⟨.inl ha, hb, .inl hc', hd⟩
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
  obtain ⟨h₁, -, h₂, -⟩ := h
  rcases List.mem_append.mp hx with hx | hx
  · exact List.mem_append_left _ (h₁.sorted_sub hx)
  · exact List.mem_append_right _ (h₂.sorted_sub hx)

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
def Sol.BVar {B : Type} (s : Sol B) (x : Srt × TyVar) : Prop :=
  (∃ p ∈ s.ty, x ∈ Ty.sortedFtv p.2) ∨ (∃ p ∈ s.row, x ∈ Row.sortedFtv p.2) ∨
    (∃ p ∈ s.lab, x ∈ Key.sortedFtv p.2)

/-- `s` is a GOOD solution over the tagged variable set `V`: its keys and
everything its bindings mention lie in `V`, no binding mentions a key, and two
bindings of one key agree. -/
structure Sol.Good {B : Type} (V : List (Srt × TyVar)) (s : Sol B) : Prop where
  dom  : ∀ x ∈ s.domS, x ∈ V
  rng  : ∀ x, s.BVar x → x ∈ V ∧ x ∉ s.domS
  fty  : ∀ p ∈ s.ty,  ∀ q ∈ s.ty,  p.1 = q.1 → p.2 = q.2
  frow : ∀ p ∈ s.row, ∀ q ∈ s.row, p.1 = q.1 → p.2 = q.2
  flab : ∀ p ∈ s.lab, ∀ q ∈ s.lab, p.1 = q.1 → p.2 = q.2

theorem Sol.Good.mono {B : Type} {V W : List (Srt × TyVar)} {s : Sol B}
    (h : s.Good V) (hW : V ⊆ W) : s.Good W :=
  ⟨fun x hx => hW (h.dom x hx), fun x hx => ⟨hW (h.rng x hx).1, (h.rng x hx).2⟩,
   h.fty, h.frow, h.flab⟩

theorem Sol.good_nil {B : Type} (V : List (Srt × TyVar)) : (Sol.nil : Sol B).Good V :=
  ⟨fun x hx => by simp [Sol.domS, Sol.nil] at hx,
   fun x hx => by rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, -⟩ | ⟨p, hp, -⟩ <;> simp [Sol.nil] at hp,
   fun p hp => by simp [Sol.nil] at hp, fun p hp => by simp [Sol.nil] at hp,
   fun p hp => by simp [Sol.nil] at hp⟩

-- ⊢  what `toSubst s` puts at a tagged variable: the variable itself (if it is
--    not a key), or something a binding mentions (if it is)
theorem Sol.ftvAt_toSubst {B : Type} {s : Sol B} {x y : Srt × TyVar}
    (h : x ∈ s.toSubst.ftvAt y) : (x = y ∧ y ∉ s.domS) ∨ (s.BVar x ∧ y ∈ s.domS) := by
  obtain ⟨b, α⟩ := y
  cases b with
  | ty =>
      change x ∈ Ty.sortedFtv (tyLookup α s.ty) at h
      rcases tyLookup_cases s.ty α with ⟨hnm, he⟩ | ⟨p, hp, h1, h2⟩
      · rw [he] at h
        simp only [Ty.sortedFtv, List.mem_singleton] at h
        exact .inl ⟨h, fun hd => hnm (Sol.domS_ty_of_mem hd)⟩
      · rw [h2] at h
        exact .inr ⟨.inl ⟨p, hp, h⟩, Sol.mem_domS_ty (List.mem_map.2 ⟨p, hp, h1⟩)⟩
  | row =>
      change x ∈ Row.sortedFtv (rowLookup α s.row) at h
      rcases rowLookup_cases s.row α with ⟨hnm, he⟩ | ⟨p, hp, h1, h2⟩
      · rw [he] at h
        simp only [Row.sortedFtv, List.mem_singleton] at h
        exact .inl ⟨h, fun hd => hnm (Sol.domS_row_of_mem hd)⟩
      · rw [h2] at h
        exact .inr ⟨.inr (.inl ⟨p, hp, h⟩), Sol.mem_domS_row (List.mem_map.2 ⟨p, hp, h1⟩)⟩
  | lab =>
      change x ∈ Key.sortedFtv (labLookup α s.lab) at h
      rcases labLookup_cases s.lab α with ⟨hnm, he⟩ | ⟨p, hp, h1, h2⟩
      · rw [he] at h
        simp only [Key.sortedFtv, List.mem_singleton] at h
        exact .inl ⟨h, fun hd => hnm (Sol.domS_lab_of_mem hd)⟩
      · rw [h2] at h
        exact .inr ⟨.inr (.inr ⟨p, hp, h⟩), Sol.mem_domS_lab (List.mem_map.2 ⟨p, hp, h1⟩)⟩

-- ⊢  applying a good solution to something inside `V` stays inside `V` and
--    clears every key — the RESIDUAL a later stage sees
theorem Sol.Good.clears {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (hg : s.Good V) {y x : Srt × TyVar} (hy : y ∈ V)
    (hx : x ∈ s.toSubst.ftvAt y) : x ∈ V ∧ x ∉ s.domS := by
  rcases Sol.ftvAt_toSubst hx with ⟨rfl, hnd⟩ | ⟨hb, -⟩
  · exact ⟨hy, hnd⟩
  · exact hg.rng x hb

theorem Sol.Good.clears_ty {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (hg : s.Good V) {τ : Ty B} (hτ : Ty.sortedFtv τ ⊆ V) {x : Srt × TyVar}
    (hx : x ∈ Ty.sortedFtv (τ.applySubst s.toSubst)) : x ∈ V ∧ x ∉ s.domS := by
  obtain ⟨y, hy, hxy⟩ := Ty.mem_sortedFtv_applySubst τ hx
  exact hg.clears (hτ hy) hxy

theorem Sol.Good.clears_spine {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (hg : s.Good V) {t : List (Atom B)} (ht : sSorted t ⊆ V) {x : Srt × TyVar}
    (hx : x ∈ sSorted (sApplySubst s.toSubst t)) : x ∈ V ∧ x ∉ s.domS := by
  obtain ⟨y, hy, hxy⟩ := mem_sSorted_sApplySubst t hx
  exact hg.clears (ht hy) hxy

theorem Sol.mem_domS_comp {B : Type} {s₁ s₂ : Sol B} {x : Srt × TyVar}
    (h : x ∈ (s₂.comp s₁).domS) : x ∈ s₁.domS ∨ x ∈ s₂.domS := by
  obtain ⟨b, α⟩ := x
  cases b with
  | ty =>
      have := Sol.domS_ty_of_mem h
      simp only [Sol.comp, List.map_append, List.map_map, List.mem_append] at this
      rcases this with hh | hh
      · exact .inl (Sol.mem_domS_ty (by simpa using hh))
      · exact .inr (Sol.mem_domS_ty hh)
  | row =>
      have := Sol.domS_row_of_mem h
      simp only [Sol.comp, List.map_append, List.map_map, List.mem_append] at this
      rcases this with hh | hh
      · exact .inl (Sol.mem_domS_row (by simpa using hh))
      · exact .inr (Sol.mem_domS_row hh)
  | lab =>
      have := Sol.domS_lab_of_mem h
      simp only [Sol.comp, List.map_append, List.map_map, List.mem_append] at this
      rcases this with hh | hh
      · exact .inl (Sol.mem_domS_lab (by simpa using hh))
      · exact .inr (Sol.mem_domS_lab hh)

-- ⊢  COMPOSITION. If the later stage's keys and mentions avoid the earlier
--    stage's keys — which is what solve-and-apply guarantees, by `clears` —
--    the composite is good.
theorem Sol.Good.comp {B : Type} {V : List (Srt × TyVar)} {s₁ s₂ : Sol B}
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
  refine ⟨fun x hx => ?_, fun x hx => ?_, fun p hp q hq he => ?_, fun p hp q hq he => ?_,
    fun p hp q hq he => ?_⟩
  · rcases Sol.mem_domS_comp hx with hx | hx
    · exact g₁.dom x hx
    · exact g₂.dom x hx
  · rcases hx with ⟨p, hp, hx⟩ | ⟨p, hp, hx⟩ | ⟨p, hp, hx⟩
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
        exact pushed y x (.inr (.inl ⟨q, hq, hy⟩)) hxy
      · exact later x (.inr (.inl ⟨p, hp, hx⟩))
    · simp only [Sol.comp, List.mem_append] at hp
      rcases hp with hp | hp
      · obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
        obtain ⟨y, hy, hxy⟩ := Key.mem_sortedFtv_applySubst q.2 hx
        exact pushed y x (.inr (.inr ⟨q, hq, hy⟩)) hxy
      · exact later x (.inr (.inr ⟨p, hp, hx⟩))
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
  · simp only [Sol.comp, List.mem_append] at hp hq
    rcases hp with hp | hp <;> rcases hq with hq | hq
    · obtain ⟨p', hp', rfl⟩ := List.mem_map.mp hp
      obtain ⟨q', hq', rfl⟩ := List.mem_map.mp hq
      simp only at he ⊢
      rw [g₁.flab p' hp' q' hq' he]
    · obtain ⟨p', hp', rfl⟩ := List.mem_map.mp hp
      exact absurd (Sol.mem_domS_lab (List.mem_map.2 ⟨p', hp', rfl⟩))
        (by simp only at he; rw [he]
            exact hdom _ (Sol.mem_domS_lab (List.mem_map.2 ⟨q, hq, rfl⟩)))
    · obtain ⟨q', hq', rfl⟩ := List.mem_map.mp hq
      exact absurd (Sol.mem_domS_lab (List.mem_map.2 ⟨q', hq', rfl⟩))
        (by simp only at he; rw [← he]
            exact hdom _ (Sol.mem_domS_lab (List.mem_map.2 ⟨p, hp, rfl⟩)))
    · exact g₂.flab p hp q hq he

------------------------- WHAT A GOOD SOLUTION IS ------------------------------

-- ⊢  a good solution is FULLY APPLIED — idempotent, so ⟦S⟧ is `toSubst`
theorem Sol.Good.applied {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (hg : s.Good V) : s.Applied := by
  have fix_ty : ∀ α, (.ty, α) ∉ s.domS → s.toSubst.ty α = .var α :=
    fun α h => tyLookup_not_mem _ (fun hm => h (Sol.mem_domS_ty hm))
  have fix_row : ∀ α, (.row, α) ∉ s.domS → s.toSubst.row α = .var α :=
    fun α h => rowLookup_not_mem _ (fun hm => h (Sol.mem_domS_row hm))
  have fix_lab : ∀ α, (.lab, α) ∉ s.domS → s.toSubst.lab α = .var α :=
    fun α h => labLookup_not_mem _ (fun hm => h (Sol.mem_domS_lab hm))
  exact ⟨fun p hp => Ty.applySubst_fixed_sorted p.2
          (fun α hα => fix_ty α (hg.rng _ (.inl ⟨p, hp, hα⟩)).2)
          (fun α hα => fix_row α (hg.rng _ (.inl ⟨p, hp, hα⟩)).2)
          (fun α hα => fix_lab α (hg.rng _ (.inl ⟨p, hp, hα⟩)).2),
         fun p hp => Row.applySubst_fixed_sorted p.2
          (fun α hα => fix_ty α (hg.rng _ (.inr (.inl ⟨p, hp, hα⟩))).2)
          (fun α hα => fix_row α (hg.rng _ (.inr (.inl ⟨p, hp, hα⟩))).2)
          (fun α hα => fix_lab α (hg.rng _ (.inr (.inl ⟨p, hp, hα⟩))).2),
         fun p hp => Key.applySubst_fixed_sorted p.2
          (fun α hα => fix_lab α (hg.rng _ (.inr (.inr ⟨p, hp, hα⟩))).2)⟩

-- ⊢  …WELL-FORMED, at rank ≡ 0: no binding mentions a key, so the descent
--    condition of `Ranked` is vacuous
theorem Sol.Good.wf {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (hg : s.Good V) : s.WF where
  acyclic := by
    intro p hp β hβ hmem
    have hs : (.row, β) ∈ Row.sortedFtv p.2 := by
      exact sSorted_toSpine _ _ (sVarSeq_mem_sSorted _ hβ)
    exact (hg.rng _ (.inr (.inl ⟨p, hp, hs⟩))).2 (Sol.mem_domS_row hmem)
  ranked := by
    refine ⟨fun _ => 0, fun x hx => ?_, fun p hp x hx hxd => ?_, fun p hp x hx hxd => ?_,
      fun p hp x hx hxd => ?_⟩
    · cases hlen : s.domS with
      | nil => rw [hlen] at hx; cases hx
      | cons _ _ => simp
    · exact absurd hxd (hg.rng x (.inl ⟨p, hp, hx⟩)).2
    · exact absurd hxd (hg.rng x (.inr (.inl ⟨p, hp, hx⟩))).2
    · exact absurd hxd (hg.rng x (.inr (.inr ⟨p, hp, hx⟩))).2

-- ⊢  …and SATISFIABLE, by its own substitution. The success legs of the
--    trichotomy are vacuous on an unsatisfiable solution; this rules that out.
theorem Sol.Good.sat {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (hg : s.Good V) : Sol.Sat s.toSubst s := by
  have ha := hg.applied
  refine ⟨fun p hp => ?_, fun p hp => ?_, fun p hp => ?_⟩
  · rw [ha.1 p hp]
    change TyEquiv (tyLookup p.1 s.ty) p.2
    rcases tyLookup_cases s.ty p.1 with ⟨hnm, -⟩ | ⟨q, hq, h1, h2⟩
    · exact absurd (List.mem_map.2 ⟨p, hp, rfl⟩) hnm
    · rw [h2, hg.fty q hq p hp h1]; exact TyEquiv.refl _
  · rw [ha.2.1 p hp]
    change RowEquiv (rowLookup p.1 s.row) p.2
    rcases rowLookup_cases s.row p.1 with ⟨hnm, -⟩ | ⟨q, hq, h1, h2⟩
    · exact absurd (List.mem_map.2 ⟨p, hp, rfl⟩) hnm
    · rw [h2, hg.frow q hq p hp h1]; exact RowEquiv.refl _
  · rw [ha.2.2 p hp]
    change labLookup p.1 s.lab = p.2
    rcases labLookup_cases s.lab p.1 with ⟨hnm, -⟩ | ⟨q, hq, h1, h2⟩
    · exact absurd (List.mem_map.2 ⟨p, hp, rfl⟩) hnm
    · rw [h2, hg.flab q hq p hp h1]

------------------------- THE BASE ARMS ----------------------------------------

-- ⊢  U-var at the type sort: α ≔ τ with α ∉ τ at the TYPE sort
theorem Sol.good_bindTy {B : Type} {S : Supply} {α : TyVar} {τ : Ty B}
    {V : List (Srt × TyVar)} {s : Sol B} {S' : Supply}
    (h : bindTy S α τ = .success s S') (hα : (.ty, α) ∈ V)
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
      have hdom : ∀ x, x ∈ Sol.domS (⟨[(α, τ)], [], []⟩ : Sol B) → x = (.ty, α) := by
        intro x hx; simpa [Sol.domS] using hx
      refine ⟨fun x hx => by rw [hdom x hx]; exact hα, fun x hx => ?_,
              fun p hp q hq _ => by
                rw [List.mem_singleton.mp hp, List.mem_singleton.mp hq],
              fun p hp => by simp at hp, fun p hp => by simp at hp⟩
      rcases hx with ⟨p, hp, hx⟩ | ⟨p, hp, -⟩ | ⟨p, hp, -⟩
      · obtain rfl := List.mem_singleton.mp hp
        refine ⟨hτ hx, fun hd => ?_⟩
        rw [hdom x hd] at hx
        exact hocc (List.elem_eq_true_of_mem (Ty.mem_tyFtv_of_sortedFtv τ hx))
      · simp at hp
      · simp at hp

-- ⊢  bindings of spine variables to ε
theorem Sol.good_ofRow_eps {B : Type} {σ : List (TyVar × Row B)}
    {V : List (Srt × TyVar)} (h : ∀ p ∈ σ, (.row, p.1) ∈ V ∧ p.2 = .empty) :
    (Sol.ofRow σ).Good V := by
  refine ⟨fun x hx => ?_, fun x hx => ?_, fun p hp => by simp [Sol.ofRow] at hp,
          fun p hp q hq _ => by rw [(h p hp).2, (h q hq).2],
          fun p hp => by simp [Sol.ofRow] at hp⟩
  · simp only [Sol.domS, Sol.ofRow, List.map_nil, List.nil_append, List.append_nil,
      List.mem_map] at hx
    obtain ⟨p, hp, rfl⟩ := hx
    exact (h p hp).1
  · rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, hx⟩ | ⟨p, hp, -⟩
    · simp [Sol.ofRow] at hp
    · rw [(h p hp).2] at hx; simp [Row.sortedFtv] at hx
    · simp [Sol.ofRow] at hp

theorem allVarsEmpty_sorted {B : Type} : (s : List (Atom B)) →
    {σ : List (TyVar × Row B)} → allVarsEmpty s = some σ →
    ∀ p ∈ σ, (.row, p.1) ∈ sSorted s ∧ p.2 = Row.empty
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
    | dfield _ _ => simp [solveVarM] at h
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVarM] at h
      | nil =>
        simp only [solveVarM] at h
        split at h
        · next σ hc =>
            simp only [Option.some.injEq, UResM.success.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            obtain ⟨-, -, -, he⟩ := collapseSol_spec hc
            refine Sol.good_ofRow_eps (fun p hp => ?_)
            obtain ⟨h₁, -, h₃⟩ := epsCollapse_mem s₂ he p hp
            exact ⟨List.mem_append_right _ (sVarSeq_mem_sSorted s₂ h₁), h₃⟩
        · split at h
          · split at h <;> (try split at h) <;> simp at h
          · next hocc =>
            simp only [Option.some.injEq, UResM.success.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            have hdom : ∀ x, x ∈ (Sol.ofRow [(α, ofSpine s₂)] : Sol B).domS →
                x = (.row, α) := by
              intro x hx; simpa [Sol.domS, Sol.ofRow] using hx
            refine ⟨fun x hx => by
                      rw [hdom x hx]; exact List.mem_append_left _ List.mem_cons_self,
                    fun x hx => ?_, fun p hp => by simp [Sol.ofRow] at hp,
                    fun p hp q hq _ => by
                      simp only [Sol.ofRow, List.mem_singleton] at hp hq
                      rw [hp, hq],
                    fun p hp => by simp [Sol.ofRow] at hp⟩
            rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, hx⟩ | ⟨p, hp, -⟩
            · simp [Sol.ofRow] at hp
            · simp only [Sol.ofRow, List.mem_singleton] at hp
              subst hp
              simp only [sortedFtv_ofSpine] at hx
              refine ⟨List.mem_append_right _ hx, fun hd => ?_⟩
              rw [hdom x hd, ← sortedFtv_ofSpine] at hx
              exact hocc (List.elem_eq_true_of_mem (Row.mem_allRowVars_of_sortedFtv (ofSpine s₂) hx))
            · simp [Sol.ofRow] at hp

-- ⊢  the key arm: a label variable bound to a key it is not
theorem Sol.good_unifyKey {B : Type} {S : Supply} {k₁ k₂ : Key} {s : Sol B}
    {S' : Supply} (h : unifyKey S k₁ k₂ = .success s S') :
    s.Good (Key.sortedFtv k₁ ++ Key.sortedFtv k₂) := by
  have hb : ∀ {α k} {V : List (Srt × TyVar)}, (.lab, α) ∈ V → Key.sortedFtv k ⊆ V →
      bindLab (B := B) S α k = .success s S' → s.Good V := by
    intro α k V hα hk h
    unfold bindLab at h
    split at h
    · simp only [UResM.success.injEq] at h; obtain ⟨rfl, rfl⟩ := h; exact Sol.good_nil V
    · next hne =>
      simp only [UResM.success.injEq] at h; obtain ⟨rfl, rfl⟩ := h
      have hdom : ∀ x, x ∈ Sol.domS (⟨[], [], [(α, k)]⟩ : Sol B) → x = (.lab, α) := by
        intro x hx; simpa [Sol.domS] using hx
      refine ⟨fun x hx => by rw [hdom x hx]; exact hα, fun x hx => ?_,
              fun p hp => by simp at hp, fun p hp => by simp at hp,
              fun p hp q hq _ => by rw [List.mem_singleton.mp hp, List.mem_singleton.mp hq]⟩
      rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, -⟩ | ⟨p, hp, hx⟩
      · simp at hp
      · simp at hp
      · obtain rfl := List.mem_singleton.mp hp
        refine ⟨hk hx, fun hd => ?_⟩
        rw [hdom x hd] at hx
        cases k with
        | lit _ => simp [Key.sortedFtv] at hx
        | var β =>
            simp [Key.sortedFtv] at hx
            exact hne (by rw [hx])
  cases k₁ with
  | var α =>
      exact hb (List.mem_append_left _ (by simp [Key.sortedFtv]))
        (fun _ hx => List.mem_append_right _ hx) h
  | lit l =>
      cases k₂ with
      | var α =>
          exact hb (List.mem_append_right _ (by simp [Key.sortedFtv]))
            (by simp [Key.sortedFtv]) h
      | lit l' =>
          simp only [unifyKey] at h
          split at h
          · simp only [UResM.success.injEq] at h; obtain ⟨rfl, rfl⟩ := h
            exact Sol.good_nil _
          · cases h

------------------------- FRESH NAMES ------------------------------------------
-- U-host is the one arm that invents a name: β′, the supply's next. So a
-- success is good over the problem's variables PLUS a list `R` of names the run
-- drew. All the disjointness this needs is read off name LENGTH: the supply
-- hands out `natName k`, of length k, so "every problem variable is shorter
-- than the supply" (`Below`) and "drawn between S and S′" (`FreshIn`) separate
-- the problem from the drawn names, and two runs' draws from each other.

/-- Every variable of `V` is shorter than the supply's next name. -/
def Below (S : Supply) (V : List (Srt × TyVar)) : Prop := ∀ x ∈ V, x.2.length < S.next

private theorem length_le_utf8ByteSize_ofList :
    (l : List Char) → (String.ofList l).length ≤ (String.ofList l).utf8ByteSize
  | [] => by simp
  | c :: l => by
      have h := length_le_utf8ByteSize_ofList l
      have e : String.ofList (c :: l) = String.singleton c ++ String.ofList l := by
        apply String.toList_inj.mp; simp
      rw [e, String.length_append, String.utf8ByteSize_append, String.length_singleton,
        String.utf8ByteSize_singleton]
      have := Char.utf8Size_pos c
      omega

theorem String.length_le_utf8ByteSize (s : String) : s.length ≤ s.utf8ByteSize := by
  have := length_le_utf8ByteSize_ofList s.toList
  rwa [String.ofList_toList] at this

theorem le_byteBound {x : Srt × TyVar} : {V : List (Srt × TyVar)} → x ∈ V →
    x.2.utf8ByteSize ≤ byteBound V
  | _ :: V, h => by
      simp only [byteBound, List.foldr_cons] at *
      rcases List.mem_cons.mp h with rfl | h
      · exact Nat.le_max_left _ _
      · exact Nat.le_trans (le_byteBound h) (Nat.le_max_right _ _)

theorem below_above (S : Supply) (V : List (Srt × TyVar)) : Below (S.above V) V :=
  fun x hx => Nat.lt_of_lt_of_le (Nat.lt_succ_of_le
    (Nat.le_trans (String.length_le_utf8ByteSize _) (le_byteBound hx))) (Nat.le_max_right _ _)

/-- Every name of `R` was drawn between `S` and `S′`: a row variable named by
the supply, so `R` has at most as many distinct names as there were draws. -/
def FreshIn (S S' : Supply) (R : List (Srt × TyVar)) : Prop :=
  ∀ x ∈ R, S.next ≤ x.2.length ∧ x.2.length < S'.next ∧ x = (.row, natName x.2.length)

theorem FreshIn.nil (S S' : Supply) : FreshIn S S' [] := fun _ hx => nomatch hx

-- ⊢  a good solution with no drawn names
theorem Sol.Good.noFresh {B : Type} {V : List (Srt × TyVar)} {s : Sol B} (S S' : Supply)
    (g : s.Good V) : ∃ R, FreshIn S S' R ∧ s.Good (V ++ R) :=
  ⟨[], FreshIn.nil S S', g.mono (fun _ hx => List.mem_append_left _ hx)⟩

-- ⊢  …moved to a larger problem
theorem Sol.Good.freshMono {B : Type} {V W R : List (Srt × TyVar)} {s : Sol B}
    (g : s.Good (V ++ R)) (h : V ⊆ W) : s.Good (W ++ R) :=
  g.mono (fun x hx => by
    rcases List.mem_append.mp hx with hx | hx
    · exact List.mem_append_left _ (h hx)
    · exact List.mem_append_right _ hx)

-- ⊢  STAGED COMPOSITION with drawn names. The first stage is good over the
--    problem plus `R₁`; the residual lives inside that and avoids the first
--    stage's keys (`hR`, from `clears` or from sole occurrence); the second
--    stage is good over the residual plus `R₂`, drawn LATER, so `R₂` is longer
--    than everything the first stage could have bound.
theorem Sol.good_comp_fresh {B : Type} {S S₁ S' : Supply} {Q Res R₁ R₂ : List (Srt × TyVar)}
    {s₁ s₂ : Sol B}
    (hB : Below S Q) (hS₁ : S.next ≤ S₁.next) (hS' : S₁.next ≤ S'.next)
    (hf₁ : FreshIn S S₁ R₁) (hf₂ : FreshIn S₁ S' R₂)
    (g₁ : s₁.Good (Q ++ R₁)) (g₂ : s₂.Good (Res ++ R₂))
    (hR : ∀ x ∈ Res, x ∈ Q ++ R₁ ∧ x ∉ s₁.domS) :
    FreshIn S S' (R₁ ++ R₂) ∧ (s₂.comp s₁).Good (Q ++ (R₁ ++ R₂)) := by
  have lt₁ : ∀ x ∈ Q ++ R₁, x.2.length < S₁.next := fun x hx => by
    rcases List.mem_append.mp hx with hx | hx
    · exact Nat.lt_of_lt_of_le (hB x hx) hS₁
    · exact (hf₁ x hx).2.1
  have hsep : ∀ x ∈ Res ++ R₂, x ∉ s₁.domS := fun x hx hd => by
    rcases List.mem_append.mp hx with hx | hx
    · exact (hR x hx).2 hd
    · exact absurd (lt₁ x (g₁.dom x hd)) (Nat.not_lt.mpr (hf₂ x hx).1)
  refine ⟨fun x hx => ?_, ?_⟩
  · rcases List.mem_append.mp hx with hx | hx
    · exact ⟨(hf₁ x hx).1, Nat.lt_of_lt_of_le (hf₁ x hx).2.1 hS', (hf₁ x hx).2.2⟩
    · exact ⟨Nat.le_trans hS₁ (hf₂ x hx).1, (hf₂ x hx).2⟩
  · refine (g₁.mono fun x hx => ?_).comp (g₂.mono fun x hx => ?_)
      (fun x hx => hsep x (g₂.dom x hx)) (fun x hx => hsep x (g₂.rng x hx).1)
    · rcases List.mem_append.mp hx with hx | hx
      · exact List.mem_append_left _ hx
      · exact List.mem_append_right _ (List.mem_append_left _ hx)
    · rcases List.mem_append.mp hx with hx | hx
      · rcases List.mem_append.mp (hR x hx).1 with hq | hq
        · exact List.mem_append_left _ hq
        · exact List.mem_append_right _ (List.mem_append_left _ hq)
      · exact List.mem_append_right _ (List.mem_append_right _ hx)

-- ⊢  the residual is again below the advanced supply
theorem Below.residual {S S₁ : Supply} {Q Res R₁ : List (Srt × TyVar)}
    (hB : Below S Q) (hS₁ : S.next ≤ S₁.next) (hf₁ : FreshIn S S₁ R₁)
    (hR : ∀ x ∈ Res, x ∈ Q ++ R₁) : Below S₁ Res := fun x hx => by
  rcases List.mem_append.mp (hR x hx) with h | h
  · exact Nat.lt_of_lt_of_le (hB x h) hS₁
  · exact (hf₁ x h).2.1

------------------------- U-HOST, TAGGED ---------------------------------------

-- ⊢  renaming the host introduces exactly β′
theorem sSorted_renameVar {B : Type} (β β' : TyVar) {x : Srt × TyVar} :
    (s : List (Atom B)) → x ∈ sSorted (renameVar β β' s) → x = (.row, β') ∨ x ∈ sSorted s
  | [], h => nomatch h
  | .var γ :: s, h => by
      simp only [renameVar] at h
      by_cases hg : γ = β
      · rw [if_pos hg] at h
        simp only [sSorted, List.mem_cons] at h ⊢
        rcases h with rfl | h
        · exact .inl rfl
        · rcases sSorted_renameVar β β' s h with h' | h'
          · exact .inl h'
          · exact .inr (.inr h')
      · rw [if_neg hg] at h
        simp only [sSorted, List.mem_cons] at h ⊢
        rcases h with rfl | h
        · exact .inr (.inl rfl)
        · rcases sSorted_renameVar β β' s h with h' | h'
          · exact .inl h'
          · exact .inr (.inr h')
  | .field _ τ :: s, h => by
      simp only [renameVar, sSorted, List.mem_append] at h ⊢
      rcases h with h | h
      · exact .inr (.inl h)
      · rcases sSorted_renameVar β β' s h with h' | h'
        · exact .inl h'
        · exact .inr (.inr h')
  | .dfield o τ :: s, h => by
      simp only [renameVar, sSorted, List.mem_append] at h ⊢
      rcases h with h | h
      · exact .inr (.inl h)
      · rcases sSorted_renameVar β β' s h with h' | h'
        · exact .inl h'
        · exact .inr (.inr h')

theorem mem_allRowVars_of_sSorted {B : Type} {α : TyVar} {s : List (Atom B)}
    (h : (.row, α) ∈ sSorted s) : α ∈ Row.allRowVars (ofSpine s) :=
  Row.mem_allRowVars_of_sortedFtv _ (by rw [sortedFtv_ofSpine]; exact h)

-- ⊢  SOLE OCCURRENCE, tagged: once β is renamed on the spine, it is gone
theorem renameVar_sole {B : Type} {β β' : TyVar} (hne : β ≠ β') :
    (s : List (Atom B)) → β ∈ sVarSeq s → (Row.allRowVars (ofSpine s)).count β = 1 →
    (.row, β) ∉ sSorted (renameVar β β' s)
  | [], hm, _, _ => nomatch hm
  | .var γ :: s, hm, hc, h => by
      simp only [ofSpine, Row.allRowVars, List.count_append] at hc
      by_cases hg : γ = β
      · subst hg
        have h0 : (Row.allRowVars (ofSpine s)).count γ = 0 := by
          simp only [List.count_singleton_self] at hc; omega
        have h1 : (Atom.var β' :: renameVar γ β' s : List (Atom B)) = renameVar γ β' (.var γ :: s) := by
          simp [renameVar]
        rw [← h1] at h
        rcases List.mem_cons.mp h with h | h
        · exact hne (by simpa using h)
        · rcases sSorted_renameVar γ β' s h with h' | h'
          · exact hne (by simpa using h')
          · exact (List.count_eq_zero.mp h0) (mem_allRowVars_of_sSorted h')
      · simp only [renameVar, if_neg hg, sSorted, List.mem_cons] at h
        have hm' : β ∈ sVarSeq s := by
          simp only [sVarSeq, List.mem_cons] at hm
          rcases hm with hm | hm
          · exact absurd hm.symm hg
          · exact hm
        have hc' : (Row.allRowVars (ofSpine s)).count β = 1 := by
          simp only [List.count_singleton, beq_iff_eq, hg, if_false] at hc
          have : ¬ (γ == β) = true := by simpa using hg
          simp [this] at hc; omega
        rcases h with h | h
        · exact hg (Prod.mk.inj h).2.symm
        · exact renameVar_sole hne s hm' hc' h
  | .field _ τ :: s, hm, hc, h => by
      simp only [ofSpine, Row.allRowVars, List.count_append] at hc
      simp only [sVarSeq] at hm
      have hpos : 0 < (Row.allRowVars (ofSpine s)).count β :=
        List.count_pos_iff.mpr (mem_allRowVars_of_sSorted (sVarSeq_mem_sSorted s hm))
      simp only [renameVar, sSorted, List.mem_append] at h
      rcases h with h | h
      · have : 0 < (Ty.allRowVars τ).count β :=
          List.count_pos_iff.mpr (Ty.mem_allRowVars_of_sortedFtv τ h)
        omega
      · exact renameVar_sole hne s hm (by omega) h
  | .dfield _ τ :: s, hm, hc, h => by
      simp only [ofSpine, Row.allRowVars, List.count_append] at hc
      simp only [sVarSeq] at hm
      have hpos : 0 < (Row.allRowVars (ofSpine s)).count β :=
        List.count_pos_iff.mpr (mem_allRowVars_of_sSorted (sVarSeq_mem_sSorted s hm))
      simp only [renameVar, sSorted, List.mem_append, List.mem_cons, List.mem_nil_iff,
        or_false] at h
      rcases h with (h | h) | h
      · cases h
      · have : 0 < (Ty.allRowVars τ).count β :=
          List.count_pos_iff.mpr (Ty.mem_allRowVars_of_sortedFtv τ h)
        omega
      · exact renameVar_sole hne s hm (by omega) h

-- ⊢  the host's own binding β ≔ (l:τ | β′) is good over the problem plus β′
theorem Sol.good_hostBind {B : Type} {V : List (Srt × TyVar)} {β β' : TyVar} {l : Label}
    {τ : Ty B} (hβ : (.row, β) ∈ V) (hτ : Ty.sortedFtv τ ⊆ V)
    (hnot : β ∉ Ty.allRowVars τ) (hne : β ≠ β') :
    (⟨[], [(β, .cat (.sing l τ) (.var β'))], []⟩ : Sol B).Good (V ++ [(.row, β')]) := by
  have hdom : ∀ x, x ∈ Sol.domS (⟨[], [(β, .cat (.sing l τ) (.var β'))], []⟩ : Sol B) →
      x = (.row, β) := by
    intro x hx; simpa [Sol.domS] using hx
  refine ⟨fun x hx => by rw [hdom x hx]; exact List.mem_append_left _ hβ,
          fun x hx => ?_, fun p hp => by simp at hp,
          fun p hp q hq _ => by rw [List.mem_singleton.mp hp, List.mem_singleton.mp hq],
          fun p hp => by simp at hp⟩
  rcases hx with ⟨p, hp, -⟩ | ⟨p, hp, hx⟩ | ⟨p, hp, -⟩
  · simp at hp
  · obtain rfl := List.mem_singleton.mp hp
    simp only [Row.sortedFtv, List.mem_append, List.mem_singleton] at hx
    rcases hx with hx | rfl
    · refine ⟨List.mem_append_left _ (hτ hx), fun hd => ?_⟩
      rw [hdom x hd] at hx
      exact hnot (Ty.mem_allRowVars_of_sortedFtv τ hx)
    · refine ⟨List.mem_append_right _ List.mem_cons_self, fun hd => ?_⟩
      exact hne (Prod.mk.inj (hdom _ hd)).2.symm
  · simp at hp

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

-- ⊢  U-HOST'S RESIDUAL: inside the problem plus β′, and clear of β — `t₁`
--    never mentions it, and on the host side SOLE OCCURRENCE leaves nothing for
--    the rename to miss. β itself is a problem variable, so not the draw β′.
theorem host_residual {B : Type} {S : Supply} {u₁ u₂ : List (Atom B)} {β : TyVar}
    {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (hB : Below S (sSorted u₁ ++ sSorted u₂))
    (he : hostL S u₁ u₂ = some (β, l, τ, t₁, t₂)) :
    (.row, β) ∈ sSorted u₁ ++ sSorted u₂ ∧ β ≠ S.fresh.1 ∧
    ∀ z ∈ sSorted t₁ ++ sSorted t₂,
      z ∈ (sSorted u₁ ++ sSorted u₂) ++ [(.row, S.fresh.1)] ∧ z ≠ (.row, β) := by
  obtain ⟨hs1, ⟨⟨rest, hvv, -⟩, -⟩, ht₁, hcnt, hren⟩ := hostL_spec he
  have hβm : β ∈ sVarSeq u₂ := by rw [hvv]; exact List.mem_cons_self
  have hβV : (.row, β) ∈ sSorted u₁ ++ sSorted u₂ :=
    List.mem_append_right _ (sVarSeq_mem_sSorted u₂ hβm)
  have hne : β ≠ S.fresh.1 := fun hh => by
    have := hB _ hβV
    simp only [hh, Supply.fresh, natName_length] at this
    exact Nat.lt_irrefl _ this
  refine ⟨hβV, hne, fun z hz => ?_⟩
  rcases List.mem_append.mp hz with hz | hz
  · refine ⟨List.mem_append_left _ (List.mem_append_left _
      (by rw [hs1]; exact List.mem_append_right _ hz)), fun hd => ?_⟩
    rw [hd] at hz
    exact ht₁ (mem_allRowVars_of_sSorted hz)
  · rw [hren] at hz
    refine ⟨?_, fun hd => ?_⟩
    · rcases sSorted_renameVar β S.fresh.1 u₂ hz with rfl | hz'
      · exact List.mem_append_right _ List.mem_cons_self
      · exact List.mem_append_left _ (List.mem_append_right _ hz')
    · rw [hd] at hz
      exact renameVar_sole hne u₂ hβm hcnt hz

-- ⊢  U-HOST'S CASE, either orientation (the residual call is on (x, y)).
--    The host's binding is good over the problem plus β′; the residual avoids
--    β (`host_residual`), so `good_comp_fresh` composes.
theorem host_good {B : Type} [DecidableEq B] {fuel : Nat}
    (ih : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) {s : Sol B} {S' : Supply},
      Below S (sSorted u₁ ++ sSorted u₂) → unifySpineMF S fuel u₁ u₂ = .success s S' →
      ∃ R, FreshIn S S' R ∧ s.Good (sSorted u₁ ++ sSorted u₂ ++ R))
    {S : Supply} {u₁ u₂ : List (Atom B)} {β : TyVar} {l : Label} {τ : Ty B}
    {t₁ t₂ x y : List (Atom B)} {s : Sol B} {S' : Supply}
    (hB : Below S (sSorted u₁ ++ sSorted u₂))
    (he : hostL S u₁ u₂ = some (β, l, τ, t₁, t₂))
    (hxy : (x = t₁ ∧ y = t₂) ∨ (x = t₂ ∧ y = t₁))
    (h : hostResM S β l τ (unifySpineMF S.fresh.2 fuel x y) = .success s S') :
    ∃ R, FreshIn S S' R ∧ s.Good (sSorted u₁ ++ sSorted u₂ ++ R) := by
  obtain ⟨s₀, hr, rfl⟩ := hostResM_success h
  obtain ⟨hs1, ⟨-, -, hnot, -⟩, -⟩ := hostL_spec he
  obtain ⟨hβV, hne, hres⟩ := host_residual hB he
  have hτV : Ty.sortedFtv τ ⊆ sSorted u₁ ++ sSorted u₂ := fun z hz =>
    List.mem_append_left _ (by rw [hs1]; exact List.mem_append_left _ hz)
  have hdom : ∀ z, z ∈ (⟨[], [(β, .cat (.sing l τ) (.var S.fresh.1))], []⟩ : Sol B).domS →
      z = (.row, β) := by
    intro z hz; simpa [Sol.domS] using hz
  have hR' : ∀ z ∈ sSorted x ++ sSorted y,
      z ∈ (sSorted u₁ ++ sSorted u₂) ++ [(.row, S.fresh.1)] ∧
      z ∉ (⟨[], [(β, .cat (.sing l τ) (.var S.fresh.1))], []⟩ : Sol B).domS := by
    have hR : ∀ z ∈ sSorted t₁ ++ sSorted t₂,
        z ∈ (sSorted u₁ ++ sSorted u₂) ++ [(.row, S.fresh.1)] ∧
        z ∉ (⟨[], [(β, .cat (.sing l τ) (.var S.fresh.1))], []⟩ : Sol B).domS :=
      fun z hz => ⟨(hres z hz).1, fun hd => (hres z hz).2 (hdom z hd)⟩
    rcases hxy with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
    · exact hR
    · exact fun z hz => hR z (append_sub_swap (fun _ h => h) hz)
  have hS₁ : S.next ≤ S.fresh.2.next := Nat.le_succ _
  have hS' : S.fresh.2.next ≤ S'.next := (unifyM_supply_mono fuel).2 _ _ _ hr
  have hf₁ : FreshIn S S.fresh.2 [(.row, S.fresh.1)] := by
    intro z hz
    obtain rfl := List.mem_singleton.mp hz
    simp only [Supply.fresh, natName_length]
    exact ⟨Nat.le_refl _, Nat.lt_succ_self _, trivial⟩
  obtain ⟨R₀, hf₀, g₀⟩ :=
    ih S.fresh.2 x y (Below.residual hB hS₁ hf₁ (fun z hz => (hR' z hz).1)) hr
  obtain ⟨hf, g⟩ := Sol.good_comp_fresh hB hS₁ hS' hf₁ hf₀
    (Sol.good_hostBind hβV hτV hnot hne) g₀ hR'
  exact ⟨_, hf, g⟩

-- ⊢  the type-sort arms that do not recurse, at any fuel and any supply:
--    good over the problem alone, with no drawn names
theorem unifyTyF_flat_good {B : Type} [DecidableEq B] (fuel : Nat) (S : Supply)
    (τ τ' : Ty B) {s : Sol B} {S' : Supply} (hrec : tyRec τ τ' = false)
    (h : unifyTyF S fuel τ τ' = .success s S') : s.Good (Ty.sortedFtv τ ++ Ty.sortedFtv τ') := by
  have hbL : ∀ (S : Supply) (α : TyVar) (τ : Ty B) {s : Sol B} {S' : Supply},
      bindTy S α τ = .success s S' → s.Good (Ty.sortedFtv (Ty.var (B := B) α) ++ Ty.sortedFtv τ) :=
    fun S α τ s S' h => Sol.good_bindTy h
      (List.mem_append_left _ (by simp [Ty.sortedFtv])) (fun _ hx => List.mem_append_right _ hx)
  have hbR : ∀ (S : Supply) (α : TyVar) (τ : Ty B) {s : Sol B} {S' : Supply},
      bindTy S α τ = .success s S' → s.Good (Ty.sortedFtv τ ++ Ty.sortedFtv (Ty.var (B := B) α)) :=
    fun S α τ s S' h => Sol.good_bindTy h
      (List.mem_append_right _ (by simp [Ty.sortedFtv])) (fun _ hx => List.mem_append_left _ hx)
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
      | lab _ => cases fuel <;> cases h
      | unk => cases fuel <;> cases h
      | fn _ _ => cases fuel <;> cases h
      | rcd _ => cases fuel <;> cases h
  | lab b =>
      cases τ' with
      | var α => cases fuel <;> exact hbR S α _ h
      | base _ => cases fuel <;> cases h
      | lab b' =>
          have h' : unifyKey S b b' = .success s S' := by cases fuel <;> exact h
          exact Sol.good_unifyKey h'
      | unk => cases fuel <;> cases h
      | fn _ _ => cases fuel <;> cases h
      | rcd _ => cases fuel <;> cases h
  | unk =>
      cases τ' with
      | var α => cases fuel <;> exact hbR S α _ h
      | base _ => cases fuel <;> cases h
      | lab _ => cases fuel <;> cases h
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
      | lab _ => cases fuel <;> cases h
      | unk => cases fuel <;> cases h
      | fn a₂ b₂ => simp [tyRec] at hrec
      | rcd _ => cases fuel <;> cases h
  | rcd ρ₁ =>
      cases τ' with
      | var α => cases fuel <;> exact hbR S α _ h
      | base _ => cases fuel <;> cases h
      | lab _ => cases fuel <;> cases h
      | unk => cases fuel <;> cases h
      | fn _ _ => cases fuel <;> cases h
      | rcd ρ₂ => simp [tyRec] at hrec

-- ⊢  EVERY SUCCESS IS GOOD, over the problem's own tagged variables plus the
--    names the run drew, at both sorts — PROVIDED the supply starts above the
--    problem (`Below`). The proviso is not bookkeeping: a problem variable named
--    like the next draw could be bound by an early stage and then mentioned by
--    U-host's β′, and the solution would not be applied.
theorem unifyM_good {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
        Below S (Ty.sortedFtv τ ++ Ty.sortedFtv τ') →
        unifyTyF S fuel τ τ' = .success s S' →
        ∃ R, FreshIn S S' R ∧ s.Good (Ty.sortedFtv τ ++ Ty.sortedFtv τ' ++ R)) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply},
        Below S (sSorted s₁ ++ sSorted s₂) →
        unifySpineMF S fuel s₁ s₂ = .success s S' →
        ∃ R, FreshIn S S' R ∧ s.Good (sSorted s₁ ++ sSorted s₂ ++ R)) := by
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
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' s S' _ h => ?_, fun S s₁ s₂ s S' _ h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => exact (unifyTyF_flat_good 0 S τ τ' hrec h).noFresh S S'
        | true =>
          rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
          · cases h
          · cases h
      · cases s₁ with
        | nil => exact (hnilL S s₂ 0 h).noFresh S S'
        | cons a s₁ =>
          cases s₂ with
          | nil => exact (hnilR S a s₁ 0 h).noFresh S S'
          | cons b s₂ => cases h
  | succ fuel ih =>
      -- a STAGED composite: the first stage under `Q`, the residual under the
      -- first stage's solution. Shared by the eq-emitting arms and `fn`.
      have stage : ∀ (S S₁ S' : Supply) (Q Q₁ Res : List (Srt × TyVar)) (s₁ s₂ : Sol B),
          Below S Q → Q₁ ⊆ Q → S.next ≤ S₁.next → S₁.next ≤ S'.next →
          (∃ R, FreshIn S S₁ R ∧ s₁.Good (Q₁ ++ R)) →
          (∀ R, s₁.Good (Q ++ R) → ∀ x ∈ Res, x ∈ Q ++ R ∧ x ∉ s₁.domS) →
          (∀ R, FreshIn S S₁ R → (∀ x ∈ Res, x ∈ Q ++ R) →
            ∃ R', FreshIn S₁ S' R' ∧ s₂.Good (Res ++ R')) →
          ∃ R, FreshIn S S' R ∧ (s₂.comp s₁).Good (Q ++ R) := by
        intro S S₁ S' Q Q₁ Res s₁ s₂ hB hQ hS₁ hS' ⟨R₁, hf₁, g₁⟩ hclr hrec
        have g₁' := g₁.freshMono hQ
        have hR := hclr R₁ g₁'
        obtain ⟨R₂, hf₂, g₂⟩ := hrec R₁ hf₁ (fun x hx => (hR x hx).1)
        obtain ⟨hf, g⟩ := Sol.good_comp_fresh hB hS₁ hS' hf₁ hf₂ g₁' g₂ hR
        exact ⟨_, hf, g⟩
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B))
          (Q : List (Srt × TyVar)) {s : Sol B} {S' : Supply},
          Ty.sortedFtv τ ++ Ty.sortedFtv τ' ⊆ Q → sSorted t₁ ++ sSorted t₂ ⊆ Q →
          Below S Q →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' → ∃ R, FreshIn S S' R ∧ s.Good (Q ++ R) := by
        intro S τ τ' t₁ t₂ Q s S' hQt hQr hB h
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        exact stage S S₁ S' Q _ _ s₁ s₂ hB hQt
          ((unifyM_supply_mono fuel).1 _ _ _ hty) ((unifyM_supply_mono fuel).2 _ _ _ hrow)
          (ih.1 S τ τ' (fun x hx => hB x (hQt hx)) hty)
          (fun R g₁ x hx => by
            rcases List.mem_append.mp hx with hx | hx
            · exact g₁.clears_spine (fun _ hy => List.mem_append_left _
                (hQr (List.mem_append_left _ hy))) hx
            · exact g₁.clears_spine (fun _ hy => List.mem_append_left _
                (hQr (List.mem_append_right _ hy))) hx)
          (fun R hf₁ hR => ih.2 S₁ _ _
            (Below.residual hB ((unifyM_supply_mono fuel).1 _ _ _ hty) hf₁ hR) hrow)
      refine ⟨fun S τ τ' s S' hB h => ?_, fun S s₁ s₂ s S' hB h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => exact (unifyTyF_flat_good (fuel + 1) S τ τ' hrec h).noFresh S S'
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
          exact stage S S₁ S' _ _ _ s₁ s₂ hB hQ
            ((unifyM_supply_mono fuel).1 _ _ _ hty) ((unifyM_supply_mono fuel).1 _ _ _ hrow)
            (ih.1 S a₁ a₂ (fun x hx => hB x (hQ hx)) hty)
            (fun R g₁ x hx => by
              rcases List.mem_append.mp hx with hx | hx
              · exact g₁.clears_ty (fun y hy => List.mem_append_left _ (by
                  simp only [Ty.sortedFtv, List.mem_append]; exact .inl (.inr hy))) hx
              · exact g₁.clears_ty (fun y hy => List.mem_append_left _ (by
                  simp only [Ty.sortedFtv, List.mem_append]; exact .inr (.inr hy))) hx)
            (fun R hf₁ hR => ih.1 S₁ _ _
              (Below.residual hB ((unifyM_supply_mono fuel).1 _ _ _ hty) hf₁ hR) hrow)
        · replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
          have hsub : sSorted ρ₁.toSpine ++ sSorted ρ₂.toSpine ⊆
              Ty.sortedFtv (.rcd ρ₁) ++ Ty.sortedFtv (.rcd ρ₂) := fun x hx => by
            rcases List.mem_append.mp hx with hx | hx
            · exact List.mem_append_left _ (sSorted_toSpine _ _ hx)
            · exact List.mem_append_right _ (sSorted_toSpine _ _ hx)
          obtain ⟨R, hf, g⟩ := ih.2 S _ _ (fun x hx => hB x (hsub hx)) h
          exact ⟨R, hf, g.freshMono hsub⟩
      · cases s₁ with
        | nil => exact (hnilL S s₂ _ h).noFresh S S'
        | cons a s₁ =>
          cases s₂ with
          | nil => exact (hnilR S a s₁ _ h).noFresh S S'
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              have hsub := sSorted_sub_pair (stripL_atoms hsl).1 (stripL_atoms hsl).2
              obtain ⟨R, hf, g⟩ := ih.2 S t₁ t₂ (fun x hx => hB x (hsub hx)) h
              exact ⟨R, hf, g.freshMono hsub⟩
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              have hsub := sSorted_sub_pair (stripR_atoms hsr).1 (stripR_atoms hsr).2
              obtain ⟨R, hf, g⟩ := ih.2 S t₁ t₂ (fun x hx => hB x (hsub hx)) h
              exact ⟨R, hf, g.freshMono hsub⟩
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              exact (Sol.good_solveVarM (hv1.trans (congrArg some h))).noFresh S S'
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              exact ((Sol.good_solveVarM (hv2.trans (congrArg some h))).mono
                (fun x hx => by
                  rcases List.mem_append.mp hx with hx | hx
                  · exact List.mem_append_right _ hx
                  · exact List.mem_append_left _ hx)).noFresh S S'
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchL_atoms hml).ty_sub
                (matchL_atoms hml).res_sub hB h
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchL_atoms hml2).swap.ty_sub
                (matchL_atoms hml2).swap.res_sub hB h
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchR_atoms hmr).ty_sub
                (matchR_atoms hmr).res_sub hB h
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchR_atoms hmr2).swap.ty_sub
                (matchR_atoms hmr2).swap.res_sub hB h
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              exact arm S τ0 τ0' t₁ t₂ _ (groundMatch_atoms hg).ty_sub
                (groundMatch_atoms hg).res_sub hB h
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (groundMatch_atoms hg2).swap.ty_sub
                (groundMatch_atoms hg2).swap.res_sub hB h
            | none =>
            cases hpc : projClash (a :: s₁) (b :: s₂) with
            | true =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc] at h
              cases h
            | false =>
            cases hh1 : hostL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1,
                Bool.false_eq_true, ite_false] at h
              exact host_good ih.2 hB hh1 (.inl ⟨rfl, rfl⟩) h
            | none =>
            cases hh2 : hostL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1, hh2,
                Bool.false_eq_true, ite_false] at h
              obtain ⟨R, hf, g⟩ := host_good ih.2
                (fun x hx => hB x (append_sub_swap (fun _ h => h) hx)) hh2 (.inr ⟨rfl, rfl⟩) h
              exact ⟨R, hf, g.freshMono (append_sub_swap (fun _ h => h))⟩
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1, hh2,
                Bool.false_eq_true, ite_false] at h
              cases h

------------------------- AT THE ENTRY POINTS ----------------------------------

-- ⊢  a problem's own variables are below its local supply
theorem mem_sFtv_of_sSorted {B : Type} {x : Srt × TyVar} {s : List (Atom B)}
    (h : x ∈ sSorted s) : x.2 ∈ sFtv s := by
  rw [sFtv_ofSpine]
  exact Row.mem_ftv_of_mem_sortedFtv _ (by rw [sortedFtv_ofSpine]; exact h)

theorem below_localSupply {B : Type} (s₁ s₂ : List (Atom B)) :
    Below (localSupply s₁ s₂) (sSorted s₁ ++ sSorted s₂) := fun x hx => by
  show x.2.length < lenBound (sFtv s₁ ++ sFtv s₂) + 1
  refine Nat.lt_succ_of_le (length_le_lenBound ?_)
  rcases List.mem_append.mp hx with hx | hx
  · exact List.mem_append_left _ (mem_sFtv_of_sSorted hx)
  · exact List.mem_append_right _ (mem_sFtv_of_sSorted hx)

theorem unifyRowM_good {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    ∃ R, FreshIn (localSupply ρ₁.toSpine ρ₂.toSpine) S' R ∧
      s.Good (Row.sortedFtv ρ₁ ++ Row.sortedFtv ρ₂ ++ R) := by
  unfold unifyRowM unifySpineM at h
  obtain ⟨R, hf, g⟩ := (unifyM_good fuel).2 _ _ _ (below_localSupply _ _) h
  exact ⟨R, hf, g.freshMono (fun x hx => by
    rcases List.mem_append.mp hx with hx | hx
    · exact List.mem_append_left _ (sSorted_toSpine _ _ hx)
    · exact List.mem_append_right _ (sSorted_toSpine _ _ hx))⟩

-- ⊢  `UnifyWF` and `UnifyAcyclic` (State.lean), no longer open
theorem unifyWF (B : Type) [DecidableEq B] : UnifyWF B :=
  fun _ _ _ _ _ h => (unifyRowM_good h).elim fun _ ⟨_, g⟩ => g.wf

theorem unifyAcyclic (B : Type) [DecidableEq B] : UnifyAcyclic B :=
  fun _ _ _ _ _ h => (unifyRowM_good h).elim fun _ ⟨_, g⟩ => g.wf.acyclic

-- ⊢  the returned solution is fully applied, at both entry points the
--    inference layer uses (`SolveTy` / `SolveRow` call `unifyTyF` /
--    `unifySpineMF` on a supply above the problem)
theorem unifyTyF_success_applied {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {s : Sol B} {S' : Supply} (hB : Below S (Ty.sortedFtv τ ++ Ty.sortedFtv τ'))
    (h : unifyTyF S fuel τ τ' = .success s S') : s.Applied :=
  ((unifyM_good fuel).1 _ _ _ hB h).elim fun _ ⟨_, g⟩ => g.applied

theorem unifySpineMF_success_applied {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply} (hB : Below S (sSorted s₁ ++ sSorted s₂))
    (h : unifySpineMF S fuel s₁ s₂ = .success s S') : s.Applied :=
  ((unifyM_good fuel).2 _ _ _ hB h).elim fun _ ⟨_, g⟩ => g.applied

theorem unifyTyF_success_wf {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {s : Sol B} {S' : Supply} (hB : Below S (Ty.sortedFtv τ ++ Ty.sortedFtv τ'))
    (h : unifyTyF S fuel τ τ' = .success s S') : s.WF :=
  ((unifyM_good fuel).1 _ _ _ hB h).elim fun _ ⟨_, g⟩ => g.wf

theorem unifySpineMF_success_wf {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {s₁ s₂ : List (Atom B)} {s : Sol B} {S' : Supply} (hB : Below S (sSorted s₁ ++ sSorted s₂))
    (h : unifySpineMF S fuel s₁ s₂ = .success s S') : s.WF :=
  ((unifyM_good fuel).2 _ _ _ hB h).elim fun _ ⟨_, g⟩ => g.wf

-- ⊢  NON-VACUITY: a success is never an unsatisfiable solution. Its own
--    substitution satisfies it — and therefore UNIFIES the problem, by
--    success soundness. Before this, both mgu legs were vacuously true of a
--    success on an unsolvable input.
theorem unifyRowM_success_sat {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    Sol.Sat s.toSubst s := (unifyRowM_good h).elim fun _ ⟨_, g⟩ => g.sat

theorem unifyRowM_success_unifies {B : Type} [DecidableEq B] {fuel : Nat}
    {ρ₁ ρ₂ : Row B} {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    Unifies s.toSubst ρ₁ ρ₂ := unifyRowM_success_sound h (unifyRowM_success_sat h)

theorem unifyTyF_success_unifies {B : Type} [DecidableEq B] {S : Supply} {fuel : Nat}
    {τ τ' : Ty B} {s : Sol B} {S' : Supply} (hB : Below S (Ty.sortedFtv τ ++ Ty.sortedFtv τ'))
    (h : unifyTyF S fuel τ τ' = .success s S') : TyUnifies s.toSubst τ τ' :=
  (unifyM_success_sound fuel).1 _ _ _ h
    (((unifyM_good fuel).1 _ _ _ hB h).elim fun _ ⟨_, g⟩ => g.sat)

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
  (∀ p ∈ s.row, ∀ q ∈ s.row, p.1 = q.1 → p.2 = q.2) ∧
  (∀ p ∈ s.lab, ∀ q ∈ s.lab, p.1 = q.1 → p.2 = q.2)

/-- Every tagged variable `s` mentions, keys included — the `V` a clean
solution is good over. -/
def Sol.allS {B : Type} (s : Sol B) : List (Srt × TyVar) :=
  s.domS ++ s.ty.flatMap (fun p => Ty.sortedFtv p.2) ++
    s.row.flatMap (fun p => Row.sortedFtv p.2) ++ s.lab.flatMap (fun p => Key.sortedFtv p.2)

theorem Sol.Good.clean {B : Type} {V : List (Srt × TyVar)} {s : Sol B}
    (h : s.Good V) : s.Clean :=
  ⟨fun x hx => (h.rng x hx).2, h.fty, h.frow, h.flab⟩

theorem Sol.Clean.good {B : Type} {s : Sol B} (h : s.Clean) : s.Good s.allS := by
  refine ⟨fun x hx => List.mem_append_left _ (List.mem_append_left _
            (List.mem_append_left _ hx)),
          fun x hx => ⟨?_, h.1 x hx⟩, h.2.1, h.2.2.1, h.2.2.2⟩
  rcases hx with ⟨p, hp, hx⟩ | ⟨p, hp, hx⟩ | ⟨p, hp, hx⟩
  · exact List.mem_append_left _ (List.mem_append_left _
      (List.mem_append_right _ (List.mem_flatMap.2 ⟨p, hp, hx⟩)))
  · exact List.mem_append_left _ (List.mem_append_right _ (List.mem_flatMap.2 ⟨p, hp, hx⟩))
  · exact List.mem_append_right _ (List.mem_flatMap.2 ⟨p, hp, hx⟩)

theorem Sol.clean_nil {B : Type} : (Sol.nil : Sol B).Clean := (Sol.good_nil []).clean

theorem Sol.Clean.applied {B : Type} {s : Sol B} (h : s.Clean) : s.Applied := h.good.applied

theorem Sol.Clean.wf {B : Type} {s : Sol B} (h : s.Clean) : s.WF := h.good.wf

theorem Sol.Clean.sat {B : Type} {s : Sol B} (h : s.Clean) : Sol.Sat s.toSubst s :=
  h.good.sat

-- ⊢  applying a clean solution clears its keys, at both sorts
theorem Sol.Clean.clears_ty {B : Type} {s : Sol B} (h : s.Clean) {τ : Ty B}
    {x : Srt × TyVar} (hx : x ∈ Ty.sortedFtv (τ.applySubst s.toSubst)) : x ∉ s.domS :=
  ((h.good.mono (V := s.allS) (W := s.allS ++ Ty.sortedFtv τ)
      (fun _ hy => List.mem_append_left _ hy)).clears_ty
    (fun _ hy => List.mem_append_right _ hy) hx).2

theorem Sol.Clean.clears_row {B : Type} {s : Sol B} (h : s.Clean) {ρ : Row B}
    {x : Srt × TyVar} (hx : x ∈ Row.sortedFtv (ρ.applySubst s.toSubst)) : x ∉ s.domS := by
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
theorem Sol.Clean.extend {B : Type} {s₀ s : Sol B} {P : List (Srt × TyVar)}
    (h₀ : s₀.Clean) (hg : s.Good P) (hP : ∀ x ∈ P, x ∉ s₀.domS) : (s.comp s₀).Clean :=
  h₀.comp hg.clean (fun x hx => hP x (hg.dom x hx)) (fun x hx => hP x (hg.rng x hx).1)

end MinimalCalculus
