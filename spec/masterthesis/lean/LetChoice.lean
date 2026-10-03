-- A-let'S CHOICE OF ᾱ IS CANONICAL: THERE IS A GREATEST ADMISSIBLE ᾱ.
--
-- `Infer.letE` REQUIRES an admissible ᾱ; it does not say how to find one. A function
-- has to choose ᾱ, and the open question was whether a
-- choice exists that every other admissible choice sits below — otherwise the
-- algorithm would have to guess, and lose principality at every let.
--
-- It does. Admissibility is a property of ᾱ's MEMBERSHIP, the empty ᾱ is
-- admissible, and admissible sets are closed under union (`LetAdmissible.union`).
-- The premise that looked like it could break union — a spent result's "no
-- spent blocker inside" — is saved by Δγ's own premise: a stump that is
-- generalized under ᾱ₁ but not under ᾱ₂ sits in Δγ₂, and Δγ₂ may not mention
-- anything ᾱ₂ generalizes.
--
-- Candidates are ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁) (`letCand`); "greatest" is among them.
-- The greatest ᾱ is COMPUTED, not searched for: `letPrune` deletes every
-- variable that no admissible subset can contain (`LetBad`) until nothing is
-- deleted. Each deletion is forced, so every admissible ᾱ survives it; at the
-- fixpoint nothing is bad, which is admissibility.

import Infer

namespace MinimalCalculus

variable {B : Type}

--------------------- ADMISSIBILITY -------------------------------------------
-- `LetAdmissible`, `letQ` and `letG` live in Infer.lean, next to the rule.

/-- ⊢  nothing generalized is always admissible. -/
theorem LetAdmissible.nil {Γ : QCtx B} {S S₁ : SolverState B} :
    LetAdmissible Γ S S₁ [] :=
  ⟨fun _ h => absurd h List.not_mem_nil,
   fun _ hp => absurd (mem_letQ.mp hp).2 List.not_mem_nil,
   fun _ hp => absurd (mem_letQ.mp hp).2 List.not_mem_nil,
   fun _ hp => absurd (mem_letQ.mp hp).2 List.not_mem_nil⟩

/-- the spent clause, stated over Δ₁ and membership in ᾱ -/
theorem LetAdmissible.spent' {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar}
    (h : LetAdmissible Γ S S₁ ᾱ) :
    ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → (p.stump.res.applySubst S₁.subst).isVar = false →
      (p.fillable S₁) = true ∧
      (∀ q ∈ S₁.parked, q.blocker = p.blocker →
        (q.fillable S₁) = true ∧
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst →
          q.spentAlike S₁ p)) ∧
      (∀ q ∈ S₁.parked, q.blocker ∈ ᾱ → (q.stump.res.applySubst S₁.subst).isVar = false →
        q.blocker ∉ (p.stump.res.applySubst S₁.subst).ftv) := by
  intro p hp hb hsp
  obtain ⟨a, b, c⟩ := h.spent p (mem_letQ.mpr ⟨hp, hb⟩) hsp
  exact ⟨a, fun q hq hqb => b q (mem_letQ.mpr ⟨hq, hqb ▸ hb⟩) hqb,
    fun q hq hqb => c q (mem_letQ.mpr ⟨hq, hqb⟩)⟩

/-- ⊢  admissibility from its conditions, stated over Δ₁ and membership in ᾱ -/
theorem LetAdmissible.of_parts {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar}
    (gfresh : ∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv ∧
      α ∉ (S₁.subst.lab β).ftv)
    (own : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ∀ q ∈ S.parked, p.stump ≠ q.stump)
    (res : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ →
      ∀ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∈ ᾱ)
    (spent : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → (p.stump.res.applySubst S₁.subst).isVar = false →
      (p.fillable S₁) = true ∧
      (∀ q ∈ S₁.parked, q.blocker = p.blocker →
        (q.fillable S₁) = true ∧
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst →
          q.spentAlike S₁ p)) ∧
      (∀ q ∈ S₁.parked, q.blocker ∈ ᾱ → (q.stump.res.applySubst S₁.subst).isVar = false →
        q.blocker ∉ (p.stump.res.applySubst S₁.subst).ftv))
    (dis : ∀ α ∈ ᾱ, ∀ p ∈ S₁.parked, p.blocker ∉ ᾱ →
      α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧ α ∉ (p.stump.res.applySubst S₁.subst).ftv ∧
      α ∉ (p.stump.label.applySubst S₁.subst).ftv) :
    LetAdmissible Γ S S₁ ᾱ := by
  refine ⟨LetAdmissible.fresh_of gfresh
      (fun α hα p hp => dis α hα p (mem_letG.mp hp).1 (mem_letG.mp hp).2),
    fun p hp => own p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2,
    fun p hp => res p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2,
    fun p hp hsp => ?_⟩
  obtain ⟨a, b, c⟩ := spent p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2 hsp
  exact ⟨a, fun q hq => b q (mem_letQ.mp hq).1,
    fun q hq => c q (mem_letQ.mp hq).1 (mem_letQ.mp hq).2⟩

/-- ⊢  **admissible choices are closed under union.** -/
theorem LetAdmissible.union {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ₁ ᾱ₂ : List TyVar}
    (h₁ : LetAdmissible Γ S S₁ ᾱ₁) (h₂ : LetAdmissible Γ S S₁ ᾱ₂) :
    LetAdmissible Γ S S₁ (ᾱ₁ ++ ᾱ₂) := by
  -- a spent stump's conditions, from ITS side: whatever shares its blocker is on
  -- that side too, and a blocker from the other side is no variable of its result
  have sideSpent : ∀ {ᾱ ᾱ' : List TyVar}, LetAdmissible Γ S S₁ ᾱ →
      ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → (p.stump.res.applySubst S₁.subst).isVar = false →
      (p.fillable S₁) = true ∧
      (∀ q ∈ S₁.parked, q.blocker = p.blocker →
        (q.fillable S₁) = true ∧
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst →
          q.spentAlike S₁ p)) ∧
      (∀ q ∈ S₁.parked, q.blocker ∈ ᾱ' → (q.stump.res.applySubst S₁.subst).isVar = false →
        q.blocker ∉ (p.stump.res.applySubst S₁.subst).ftv) := by
    intro ᾱ ᾱ' hj p hp hpb hsp
    obtain ⟨hl, hsame, hbl⟩ := hj.spent' p hp hpb hsp
    refine ⟨hl, hsame, fun q hq _ hqs hm => ?_⟩
    by_cases hqb : q.blocker ∈ ᾱ
    · exact hbl q hq hqb hqs hm
    · exact hqb (hj.res p (mem_letQ.mpr ⟨hp, hpb⟩) _ hm)
  refine LetAdmissible.of_parts ?_ ?_ ?_ ?_ ?_
  · intro α hα
    rcases List.mem_append.mp hα with h | h
    · exact h₁.gfresh α h
    · exact h₂.gfresh α h
  · intro p hp hb
    rcases List.mem_append.mp hb with h | h
    · exact h₁.own p (mem_letQ.mpr ⟨hp, h⟩)
    · exact h₂.own p (mem_letQ.mpr ⟨hp, h⟩)
  · intro p hp hb
    rcases List.mem_append.mp hb with h | h
    · exact fun δ hδ => List.mem_append_left _ (h₁.res p (mem_letQ.mpr ⟨hp, h⟩) δ hδ)
    · exact fun δ hδ => List.mem_append_right _ (h₂.res p (mem_letQ.mpr ⟨hp, h⟩) δ hδ)
  · intro p hp hb hsp
    rcases List.mem_append.mp hb with h | h
    · exact sideSpent (ᾱ' := ᾱ₁ ++ ᾱ₂) h₁ p hp h hsp
    · exact sideSpent (ᾱ' := ᾱ₁ ++ ᾱ₂) h₂ p hp h hsp
  · intro α hα p hp hb
    have hb₁ : p.blocker ∉ ᾱ₁ := fun h => hb (List.mem_append_left _ h)
    have hb₂ : p.blocker ∉ ᾱ₂ := fun h => hb (List.mem_append_right _ h)
    rcases List.mem_append.mp hα with h | h
    · exact h₁.dis α h p (mem_letG.mpr ⟨hp, hb₁⟩)
    · exact h₂.dis α h p (mem_letG.mpr ⟨hp, hb₂⟩)

/-- ⊢  admissibility only reads ᾱ's membership. -/
theorem LetAdmissible.congr {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ ᾱ' : List TyVar}
    (he : ∀ α, α ∈ ᾱ ↔ α ∈ ᾱ') (h : LetAdmissible Γ S S₁ ᾱ) :
    LetAdmissible Γ S S₁ ᾱ' := by
  refine LetAdmissible.of_parts (fun α hα => h.gfresh α ((he α).mpr hα))
    (fun p hp hb => h.own p (mem_letQ.mpr ⟨hp, (he _).mpr hb⟩))
    (fun p hp hb δ hδ => (he δ).mp (h.res p (mem_letQ.mpr ⟨hp, (he _).mpr hb⟩) δ hδ))
    (fun p hp hb hsp => ?_)
    (fun α hα p hp hb => h.dis α ((he α).mpr hα) p
      (mem_letG.mpr ⟨hp, fun h' => hb ((he _).mp h')⟩))
  obtain ⟨a, b, c⟩ := h.spent' p hp ((he _).mpr hb) hsp
  exact ⟨a, b, fun q hq hqb => c q hq ((he _).mpr hqb)⟩

--------------------- THE GREATEST ᾱ, COMPUTED --------------------------------

/-- α is FORCED OUT of every admissible subset of ᾱ. One disjunct per way a
single variable can make admissibility fail. -/
def LetBad (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) (α : TyVar) : Prop :=
  (∃ β ∈ Γ.ftv, α ∈ (S₁.subst.ty β).ftv ∨ α ∈ (S₁.subst.row β).ftv ∨
    α ∈ (S₁.subst.lab β).ftv) ∨
  -- a stump blocked on α answers outside ᾱ, or was parked before the let
  (∃ p ∈ S₁.parked, p.blocker = α ∧
     ((∃ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∉ ᾱ) ∨
      ∃ q ∈ S.parked, p.stump = q.stump)) ∨
  -- a stump that must stay in Δ_Γ mentions α
  (∃ p ∈ S₁.parked, p.blocker ∉ ᾱ ∧
     (α ∈ (p.stump.row.applySubst S₁.subst).ftv ∨ α ∈ (p.stump.res.applySubst S₁.subst).ftv ∨
      α ∈ (p.stump.label.applySubst S₁.subst).ftv)) ∨
  -- a spent stump blocked on α cannot be met by extending α
  (∃ p ∈ S₁.parked, p.blocker = α ∧ (p.stump.res.applySubst S₁.subst).isVar = false ∧
     ((p.fillable S₁) = false ∨
      ∃ q ∈ S₁.parked, q.blocker = α ∧ ((q.fillable S₁) = false ∨
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst ∧
         ¬ q.spentAlike S₁ p)))) ∨
  -- a spent stump blocked on α has α inside a spent result
  (∃ q ∈ S₁.parked, q.blocker = α ∧ (q.stump.res.applySubst S₁.subst).isVar = false ∧
     ∃ p ∈ S₁.parked, (p.stump.res.applySubst S₁.subst).isVar = false ∧
       α ∈ (p.stump.res.applySubst S₁.subst).ftv)

set_option synthInstance.maxSize 512 in
set_option synthInstance.maxHeartbeats 200000 in
instance [DecidableEq B] {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar} {α : TyVar} :
    Decidable (LetBad Γ S S₁ ᾱ α) := by
  unfold LetBad; infer_instance

/-- ⊢  a bad variable is in no admissible subset. -/
theorem LetBad.excluded {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ ᾱ' : List TyVar}
    {α : TyVar} (hbad : LetBad Γ S S₁ ᾱ α) (hsub : ∀ β ∈ ᾱ', β ∈ ᾱ)
    (hA : LetAdmissible Γ S S₁ ᾱ') : α ∉ ᾱ' := by
  intro hα
  rcases hbad with ⟨β, hβ, h⟩ | ⟨p, hp, rfl, h⟩ | ⟨p, hp, hb, h⟩ |
    ⟨p, hp, rfl, hsp, h⟩ | ⟨q, hq, rfl, hqs, p, hp, hps, hm⟩
  · rcases h with h | h | h
    · exact (hA.gfresh _ hα β hβ).1 h
    · exact (hA.gfresh _ hα β hβ).2.1 h
    · exact (hA.gfresh _ hα β hβ).2.2 h
  · rcases h with ⟨δ, hδ, hn⟩ | ⟨q, hq, he⟩
    · exact hn (hsub _ (hA.res p (mem_letQ.mpr ⟨hp, hα⟩) δ hδ))
    · exact hA.own p (mem_letQ.mpr ⟨hp, hα⟩) q hq he
  · have hb' : p.blocker ∉ ᾱ' := fun h' => hb (hsub _ h')
    have hg := hA.dis _ hα p (mem_letG.mpr ⟨hp, hb'⟩)
    rcases h with h | h | h
    · exact hg.1 h
    · exact hg.2.1 h
    · exact hg.2.2 h
  · obtain ⟨hl, hsame, -⟩ := hA.spent' p hp hα hsp
    rcases h with h | ⟨q, hq, hqb, h | ⟨he, hne⟩⟩
    · rw [hl] at h; exact Bool.noConfusion h
    · rw [(hsame q hq hqb).1] at h; exact Bool.noConfusion h
    · exact hne ((hsame q hq hqb).2 he)
  · by_cases hpb : p.blocker ∈ ᾱ'
    · exact (hA.spent' p hp hpb hps).2.2 q hq hα hqs hm
    · exact (hA.dis _ hα p (mem_letG.mpr ⟨hp, hpb⟩)).2.1 hm

/-- ⊢  a set with no bad member is admissible. -/
theorem LetAdmissible.of_no_bad {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar}
    (h : ∀ α ∈ ᾱ, ¬ LetBad Γ S S₁ ᾱ α) : LetAdmissible Γ S S₁ ᾱ := by
  -- the disjuncts, one name each
  have b2 : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ¬ ((∃ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∉ ᾱ) ∨
      ∃ q ∈ S.parked, p.stump = q.stump) :=
    fun p hp hb hn => h _ hb (.inr (.inl ⟨p, hp, rfl, hn⟩))
  refine LetAdmissible.of_parts (fun α hα β hβ => ?_) (fun p hp hb q hq he => ?_)
    (fun p hp hb => ?_) (fun p hp hb hsp => ?_) (fun α hα p hp hb => ?_)
  · refine ⟨fun hm => ?_, fun hm => ?_, fun hm => ?_⟩
    · exact h α hα (.inl ⟨β, hβ, .inl hm⟩)
    · exact h α hα (.inl ⟨β, hβ, .inr (.inl hm)⟩)
    · exact h α hα (.inl ⟨β, hβ, .inr (.inr hm)⟩)
  · exact b2 p hp hb (.inr ⟨q, hq, he⟩)
  · exact fun δ hδ => Classical.byContradiction fun hn =>
        b2 p hp hb (.inl ⟨δ, hδ, hn⟩)
  · have b4 : ¬ ((p.fillable S₁) = false ∨
        ∃ q ∈ S₁.parked, q.blocker = p.blocker ∧
          ((q.fillable S₁) = false ∨
           (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst ∧
            ¬ q.spentAlike S₁ p))) :=
      fun hn => h _ hb (.inr (.inr (.inr (.inl ⟨p, hp, rfl, hsp, hn⟩))))
    refine ⟨?_, fun q hq hqb => ⟨?_, fun he => ?_⟩, fun q hq hqb hqs hm => ?_⟩
    · cases hc : (p.fillable S₁)
      · exact absurd (.inl hc) b4
      · rfl
    · cases hc : (q.fillable S₁)
      · exact absurd (.inr ⟨q, hq, hqb, .inl hc⟩) b4
      · rfl
    · exact Classical.byContradiction fun hn => b4 (.inr ⟨q, hq, hqb, .inr ⟨he, hn⟩⟩)
    · exact h _ hqb (.inr (.inr (.inr (.inr ⟨q, hq, rfl, hqs, p, hp, hsp, hm⟩))))
  · refine ⟨fun hm => ?_, fun hm => ?_, fun hm => ?_⟩
    · exact h α hα (.inr (.inr (.inl ⟨p, hp, hb, .inl hm⟩)))
    · exact h α hα (.inr (.inr (.inl ⟨p, hp, hb, .inr (.inl hm)⟩)))
    · exact h α hα (.inr (.inr (.inl ⟨p, hp, hb, .inr (.inr hm)⟩)))

/-- one round: delete every variable that is bad for the current ᾱ -/
def letPruneStep [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) :
    List TyVar :=
  ᾱ.filter (fun α => !decide (LetBad Γ S S₁ ᾱ α))

/-- delete bad variables until none is left -/
def letPrune [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) :
    List TyVar :=
  if _h : (letPruneStep Γ S S₁ ᾱ).length < ᾱ.length then
    letPrune Γ S S₁ (letPruneStep Γ S S₁ ᾱ)
  else ᾱ
termination_by ᾱ.length
decreasing_by exact _h

/-- ⊢  pruning keeps every admissible subset, and ends admissible. -/
theorem letPrune_spec [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) :
    LetAdmissible Γ S S₁ (letPrune Γ S S₁ ᾱ) ∧
    (∀ α ∈ letPrune Γ S S₁ ᾱ, α ∈ ᾱ) ∧
    (∀ ᾱ', (∀ β ∈ ᾱ', β ∈ ᾱ) → LetAdmissible Γ S S₁ ᾱ' →
      ∀ β ∈ ᾱ', β ∈ letPrune Γ S S₁ ᾱ) := by
  induction ᾱ using letPrune.induct Γ S S₁ with
  | case1 ᾱ hlt ih =>
      rw [letPrune, dif_pos hlt]
      obtain ⟨hA, hsub, hgr⟩ := ih
      refine ⟨hA, fun α hα => (List.mem_filter.mp (hsub α hα)).1, fun ᾱ'' hs hA'' β hβ => ?_⟩
      refine hgr ᾱ'' (fun γ hγ => List.mem_filter.mpr ⟨hs γ hγ, ?_⟩) hA'' β hβ
      simpa using fun hbad => hbad.excluded hs hA'' hγ
  | case2 ᾱ hge =>
      rw [letPrune, dif_neg hge]
      refine ⟨LetAdmissible.of_no_bad fun α hα hbad => ?_, fun _ h => h,
        fun _ hs _ β hβ => hs β hβ⟩
      have hall := List.length_filter_eq_length_iff.mp
        (Nat.le_antisymm (List.length_filter_le _ _) (Nat.not_lt.mp hge))
      simpa [hbad] using hall α hα

/-- ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁): what A-let chooses ᾱ from. A variable outside it
would be generalized vacuously. -/
def letCand (S₁ : SolverState B) (τ₁ : Ty B) : List TyVar :=
  ((τ₁.applySubst S₁.subst).ftv ++ S₁.parked.flatMap (fun p =>
    (p.stump.row.applySubst S₁.subst).ftv ++ (p.stump.res.applySubst S₁.subst).ftv ++
      (p.stump.label.applySubst S₁.subst).ftv)).eraseDups

/-- the ᾱ a function should choose at A-let: gen_{Γ,S}(S₁, τ₁) -/
def greatestAlpha [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) (τ₁ : Ty B) :
    List TyVar :=
  letPrune Γ S S₁ (letCand S₁ τ₁)

/-- ⊢  **the greatest admissible ᾱ ⊆ ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁) exists, and
`greatestAlpha` computes it.** -/
theorem greatestAlpha_spec [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) (τ₁ : Ty B) :
    LetAdmissible Γ S S₁ (greatestAlpha Γ S S₁ τ₁) ∧
    (∀ α ∈ greatestAlpha Γ S S₁ τ₁, α ∈ letCand S₁ τ₁) ∧
    ∀ ᾱ, (∀ α ∈ ᾱ, α ∈ letCand S₁ τ₁) → LetAdmissible Γ S S₁ ᾱ →
      ∀ α ∈ ᾱ, α ∈ greatestAlpha Γ S S₁ τ₁ :=
  letPrune_spec Γ S S₁ (letCand S₁ τ₁)

end MinimalCalculus
