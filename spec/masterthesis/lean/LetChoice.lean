-- A-let'S CHOICE OF ᾱ IS CANONICAL: THERE IS A GREATEST ADMISSIBLE ᾱ.
--
-- `Infer.letE` REQUIRES its split; it does not say how to find one. A function
-- has to choose ᾱ, and the open question was whether a
-- choice exists that every other admissible choice sits below — otherwise the
-- algorithm would have to guess, and lose principality at every let.
--
-- It does. Admissibility is a property of ᾱ's MEMBERSHIP, the empty ᾱ is
-- admissible, and admissible sets are closed under union (`LetAdmissible.union`).
-- The premises that looked like they could break union — disjoint results and
-- a spent result's "no spent blocker inside" — are saved by Δγ's own premise: a stump that is
-- generalized under ᾱ₁ but not under ᾱ₂ sits in Δγ₂, and Δγ₂ may not mention
-- anything ᾱ₂ generalizes.
--
-- The greatest ᾱ is COMPUTED, not searched for: `letPrune` deletes every
-- variable that no admissible subset can contain (`LetBad`) until nothing is
-- deleted. Each deletion is forced, so every admissible ᾱ survives it; at the
-- fixpoint nothing is bad, which is admissibility.

import Infer

namespace MinimalCalculus

variable {B : Type}

--------------------- ADMISSIBILITY -------------------------------------------

/-- Δ_q for a choice ᾱ: the stumps blocked on a generalized variable. -/
def letQ (S₁ : SolverState B) (ᾱ : List TyVar) : List (Parked B) :=
  S₁.parked.filter (fun p => decide (p.blocker ∈ ᾱ))

/-- Δ_Γ for a choice ᾱ: the rest. -/
def letG (S₁ : SolverState B) (ᾱ : List TyVar) : List (Parked B) :=
  S₁.parked.filter (fun p => !decide (p.blocker ∈ ᾱ))

theorem mem_letQ {S₁ : SolverState B} {ᾱ : List TyVar} {p : Parked B} :
    p ∈ letQ S₁ ᾱ ↔ p ∈ S₁.parked ∧ p.blocker ∈ ᾱ := by
  simp [letQ]

theorem mem_letG {S₁ : SolverState B} {ᾱ : List TyVar} {p : Parked B} :
    p ∈ letG S₁ ᾱ ↔ p ∈ S₁.parked ∧ p.blocker ∉ ᾱ := by
  simp [letG]

/-- `Infer.letE`'s premises on ᾱ, with Δ_q/Δ_Γ determined by ᾱ and stated
through membership only. `kinded` is `Assigns` read as a set condition. Results,
rows and keys are read at S₁. -/
structure LetAdmissible (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) : Prop where
  kinded   : ∀ α ∈ ᾱ, α ∈ KEnv.dom S₁.kinds
  gfresh   : ∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv ∧
               α ∉ (S₁.subst.lab β).ftv
  own      : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ∀ q ∈ S.parked, p.stump ≠ q.stump
  res      : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ →
               ∀ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∈ ᾱ
  disj     : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ∀ q ∈ S₁.parked, q.blocker ∈ ᾱ →
               ∀ δ ∈ (p.stump.res.applySubst S₁.subst).ftv,
               δ ∈ (q.stump.res.applySubst S₁.subst).ftv → p.stump = q.stump
  spent    : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → (p.stump.res.applySubst S₁.subst).isVar = false →
               (p.fillable S₁) = true ∧
               (∀ q ∈ S₁.parked, q.blocker = p.blocker →
                 (q.fillable S₁) = true ∧
                 (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst →
                   q.stump = p.stump)) ∧
               (∀ q ∈ S₁.parked, q.blocker ∈ ᾱ → (q.stump.res.applySubst S₁.subst).isVar = false →
                 q.blocker ∉ (p.stump.res.applySubst S₁.subst).ftv)
  dis      : ∀ α ∈ ᾱ, ∀ p ∈ S₁.parked, p.blocker ∉ ᾱ →
               α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧ α ∉ (p.stump.res.applySubst S₁.subst).ftv ∧
               α ∉ (p.stump.label.applySubst S₁.subst).ftv
  unsolved : ∀ α ∈ ᾱ, α ∉ S₁.sol.dom

/-- ⊢  nothing generalized is always admissible. -/
theorem LetAdmissible.nil {Γ : QCtx B} {S S₁ : SolverState B} :
    LetAdmissible Γ S S₁ [] :=
  ⟨fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil,
   fun _ _ h => absurd h List.not_mem_nil, fun _ _ h => absurd h List.not_mem_nil,
   fun _ _ h => absurd h List.not_mem_nil, fun _ _ h => absurd h List.not_mem_nil,
   fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil⟩

/-- ⊢  **admissible choices are closed under union.** -/
theorem LetAdmissible.union {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ₁ ᾱ₂ : List TyVar}
    (h₁ : LetAdmissible Γ S S₁ ᾱ₁) (h₂ : LetAdmissible Γ S S₁ ᾱ₂) :
    LetAdmissible Γ S S₁ (ᾱ₁ ++ ᾱ₂) := by
  -- no shared result variable, from ONE side: if p is not generalized there, it
  -- sits in that side's Δγ, which may not mention q's generalized result
  have sideDisj : ∀ {ᾱ : List TyVar}, LetAdmissible Γ S S₁ ᾱ →
      ∀ p ∈ S₁.parked, ∀ q ∈ S₁.parked, q.blocker ∈ ᾱ →
      ∀ δ ∈ (p.stump.res.applySubst S₁.subst).ftv,
      δ ∈ (q.stump.res.applySubst S₁.subst).ftv → p.stump = q.stump := by
    intro ᾱ hj p hp q hq hqb δ hδp hδq
    by_cases hpb : p.blocker ∈ ᾱ
    · exact hj.disj p hp hpb q hq hqb δ hδp hδq
    · exact absurd hδp (hj.dis δ (hj.res q hq hqb δ hδq) p hp hpb).2.1
  -- a spent stump's conditions, from ITS side: whatever shares its blocker is on
  -- that side too, and a blocker from the other side is no variable of its result
  have sideSpent : ∀ {ᾱ ᾱ' : List TyVar}, LetAdmissible Γ S S₁ ᾱ →
      ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → (p.stump.res.applySubst S₁.subst).isVar = false →
      (p.fillable S₁) = true ∧
      (∀ q ∈ S₁.parked, q.blocker = p.blocker →
        (q.fillable S₁) = true ∧
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst →
          q.stump = p.stump)) ∧
      (∀ q ∈ S₁.parked, q.blocker ∈ ᾱ' → (q.stump.res.applySubst S₁.subst).isVar = false →
        q.blocker ∉ (p.stump.res.applySubst S₁.subst).ftv) := by
    intro ᾱ ᾱ' hj p hp hpb hsp
    obtain ⟨hl, hsame, hbl⟩ := hj.spent p hp hpb hsp
    refine ⟨hl, hsame, fun q hq _ hqs hm => ?_⟩
    by_cases hqb : q.blocker ∈ ᾱ
    · exact hbl q hq hqb hqs hm
    · exact hqb (hj.res p hp hpb _ hm)
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro α hα
    rcases List.mem_append.mp hα with h | h
    · exact h₁.kinded α h
    · exact h₂.kinded α h
  · intro α hα
    rcases List.mem_append.mp hα with h | h
    · exact h₁.gfresh α h
    · exact h₂.gfresh α h
  · intro p hp hb
    rcases List.mem_append.mp hb with h | h
    · exact h₁.own p hp h
    · exact h₂.own p hp h
  · intro p hp hb
    rcases List.mem_append.mp hb with h | h
    · exact fun δ hδ => List.mem_append_left _ (h₁.res p hp h δ hδ)
    · exact fun δ hδ => List.mem_append_right _ (h₂.res p hp h δ hδ)
  · intro p hp _ q hq hqb δ hδp hδq
    rcases List.mem_append.mp hqb with h | h
    · exact sideDisj h₁ p hp q hq h δ hδp hδq
    · exact sideDisj h₂ p hp q hq h δ hδp hδq
  · intro p hp hb hsp
    rcases List.mem_append.mp hb with h | h
    · obtain ⟨a, b, c⟩ := sideSpent (ᾱ' := ᾱ₁ ++ ᾱ₂) h₁ p hp h hsp
      exact ⟨a, b, c⟩
    · obtain ⟨a, b, c⟩ := sideSpent (ᾱ' := ᾱ₁ ++ ᾱ₂) h₂ p hp h hsp
      exact ⟨a, b, c⟩
  · intro α hα p hp hb
    have hb₁ : p.blocker ∉ ᾱ₁ := fun h => hb (List.mem_append_left _ h)
    have hb₂ : p.blocker ∉ ᾱ₂ := fun h => hb (List.mem_append_right _ h)
    rcases List.mem_append.mp hα with h | h
    · exact h₁.dis α h p hp hb₁
    · exact h₂.dis α h p hp hb₂
  · intro α hα
    rcases List.mem_append.mp hα with h | h
    · exact h₁.unsolved α h
    · exact h₂.unsolved α h

/-- ⊢  admissibility only reads ᾱ's membership. -/
theorem LetAdmissible.congr {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ ᾱ' : List TyVar}
    (he : ∀ α, α ∈ ᾱ ↔ α ∈ ᾱ') (h : LetAdmissible Γ S S₁ ᾱ) :
    LetAdmissible Γ S S₁ ᾱ' := by
  refine ⟨fun α hα => h.kinded α ((he α).mpr hα), fun α hα => h.gfresh α ((he α).mpr hα),
    fun p hp hb => h.own p hp ((he _).mpr hb),
    fun p hp hb => ?_,
    fun p hp hb q hq hqb => h.disj p hp ((he _).mpr hb) q hq ((he _).mpr hqb),
    fun p hp hb hsp => ?_,
    fun α hα p hp hb => h.dis α ((he α).mpr hα) p hp (fun h' => hb ((he _).mp h')),
    fun α hα => h.unsolved α ((he α).mpr hα)⟩
  · exact fun δ hδ => (he δ).mp (h.res p hp ((he _).mpr hb) δ hδ)
  · obtain ⟨a, b, c⟩ := h.spent p hp ((he _).mpr hb) hsp
    exact ⟨a, b, fun q hq hqb => c q hq ((he _).mpr hqb)⟩

--------------------- THE BRIDGE TO `Infer.letE` ------------------------------

theorem KEnv.lookup_isSome_of_mem_dom {K : KEnv} {α : TyVar} (h : α ∈ KEnv.dom K) :
    ∃ κ, KEnv.lookup K α = some κ := by
  induction K with
  | nil => exact absurd h List.not_mem_nil
  | cons a K ih =>
      by_cases ha : a.1 = α
      · exact ⟨a.2, by simp [KEnv.lookup, List.find?, ha]⟩
      · have h' : α ∈ KEnv.dom K := by
          simp only [KEnv.dom, List.map_cons, List.mem_cons] at h
          rcases h with h | h
          · exact absurd h.symm ha
          · exact h
        obtain ⟨κ, hκ⟩ := ih h'
        refine ⟨κ, ?_⟩
        simp only [KEnv.lookup, List.find?] at hκ ⊢
        have : (a.1 == α) = false := by simpa using ha
        rw [this]; exact hκ

/-- the kinds an admissible ᾱ is generalized at -/
def letKinds (S₁ : SolverState B) (ᾱ : List TyVar) : List Kind :=
  ᾱ.map (fun α => (S₁.kinds.lookup α).getD .ty)

theorem letKinds_assigns {S₁ : SolverState B} {ᾱ : List TyVar}
    (h : ∀ α ∈ ᾱ, α ∈ KEnv.dom S₁.kinds) : S₁.kinds.Assigns ᾱ (letKinds S₁ ᾱ) := by
  unfold KEnv.Assigns letKinds
  rw [List.map_map]
  refine List.map_congr_left (fun α hα => ?_)
  obtain ⟨κ, hκ⟩ := KEnv.lookup_isSome_of_mem_dom (h α hα)
  simp [hκ]

/-- ⊢  **an admissible ᾱ is a legal A-let step**, with Δ_q/Δ_Γ the filters. -/
theorem LetAdmissible.letE [DecidableEq B] {C : Type} {constTy : C → B} {Γ : QCtx B}
    {S S₁ S₂ : SolverState B} {x : Var} {e₁ e₂ : Expr C} {τ₁ τ₂ : Ty B}
    {ᾱ : List TyVar} (hA : LetAdmissible Γ S S₁ ᾱ)
    (h₁ : Infer constTy Γ S e₁ τ₁ S₁)
    (h₂ : Infer constTy (Γ.bindScheme x (letScheme S₁ ᾱ (letQ S₁ ᾱ) τ₁))
      { S₁ with parked := letG S₁ ᾱ } e₂ τ₂ S₂) :
    Infer constTy Γ S (.letE x e₁ e₂) τ₂ S₂ := by
  refine Infer.letE (Δq := letQ S₁ ᾱ) (Δγ := letG S₁ ᾱ) (κs := letKinds S₁ ᾱ) h₁
    (letKinds_assigns hA.kinded) (List.filter_append_perm _ _).symm
    (fun p hp => (mem_letQ.mp hp).2) (fun p hp => (mem_letG.mp hp).2)
    hA.gfresh
    (fun p hp => hA.own p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2)
    ⟨fun p hp => hA.res p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2,
     fun p hp q hq => hA.disj p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2
       q (mem_letQ.mp hq).1 (mem_letQ.mp hq).2,
     fun p hp hsp => by
       obtain ⟨a, b, c⟩ := hA.spent p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2 hsp
       exact ⟨a, fun q hq => b q (mem_letQ.mp hq).1,
         fun q hq => c q (mem_letQ.mp hq).1 (mem_letQ.mp hq).2⟩⟩
    (fun α hα p hp => hA.dis α hα p (mem_letG.mp hp).1 (mem_letG.mp hp).2)
    hA.unsolved
    h₂

--------------------- THE GREATEST ᾱ, COMPUTED --------------------------------

/-- α is FORCED OUT of every admissible subset of ᾱ. One disjunct per way a
single variable can make admissibility fail. -/
def LetBad (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) (α : TyVar) : Prop :=
  α ∉ KEnv.dom S₁.kinds ∨
  (∃ β ∈ Γ.ftv, α ∈ (S₁.subst.ty β).ftv ∨ α ∈ (S₁.subst.row β).ftv ∨
    α ∈ (S₁.subst.lab β).ftv) ∨
  α ∈ S₁.sol.dom ∨
  -- a stump blocked on α answers outside ᾱ, or was parked before the let
  (∃ p ∈ S₁.parked, p.blocker = α ∧
     ((∃ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∉ ᾱ) ∨
      ∃ q ∈ S.parked, p.stump = q.stump)) ∨
  -- a stump that must stay in Δ_Γ mentions α
  (∃ p ∈ S₁.parked, p.blocker ∉ ᾱ ∧
     (α ∈ (p.stump.row.applySubst S₁.subst).ftv ∨ α ∈ (p.stump.res.applySubst S₁.subst).ftv ∨
      α ∈ (p.stump.label.applySubst S₁.subst).ftv)) ∨
  -- a stump blocked on α shares a result variable with another one
  (∃ q ∈ S₁.parked, q.blocker = α ∧
     ∃ p ∈ S₁.parked, p.stump ≠ q.stump ∧ ∃ δ ∈ (p.stump.res.applySubst S₁.subst).ftv,
       δ ∈ (q.stump.res.applySubst S₁.subst).ftv) ∨
  -- a spent stump blocked on α cannot be met by extending α
  (∃ p ∈ S₁.parked, p.blocker = α ∧ (p.stump.res.applySubst S₁.subst).isVar = false ∧
     ((p.fillable S₁) = false ∨
      ∃ q ∈ S₁.parked, q.blocker = α ∧ ((q.fillable S₁) = false ∨
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst ∧
         q.stump ≠ p.stump)))) ∨
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
  rcases hbad with h | ⟨β, hβ, h⟩ | h | ⟨p, hp, rfl, h⟩ | ⟨p, hp, hb, h⟩ |
    ⟨q, hq, rfl, p, hp, hne, δ, hδp, hδq⟩ |
    ⟨p, hp, rfl, hsp, h⟩ | ⟨q, hq, rfl, hqs, p, hp, hps, hm⟩
  · exact h (hA.kinded _ hα)
  · rcases h with h | h | h
    · exact (hA.gfresh _ hα β hβ).1 h
    · exact (hA.gfresh _ hα β hβ).2.1 h
    · exact (hA.gfresh _ hα β hβ).2.2 h
  · exact hA.unsolved _ hα h
  · rcases h with ⟨δ, hδ, hn⟩ | ⟨q, hq, he⟩
    · exact hn (hsub _ (hA.res p hp hα δ hδ))
    · exact hA.own p hp hα q hq he
  · have hb' : p.blocker ∉ ᾱ' := fun h' => hb (hsub _ h')
    rcases h with h | h | h
    · exact (hA.dis _ hα p hp hb').1 h
    · exact (hA.dis _ hα p hp hb').2.1 h
    · exact (hA.dis _ hα p hp hb').2.2 h
  · by_cases hpb : p.blocker ∈ ᾱ'
    · exact hne (hA.disj p hp hpb q hq hα δ hδp hδq)
    · exact (hA.dis δ (hA.res q hq hα δ hδq) p hp hpb).2.1 hδp
  · obtain ⟨hl, hsame, -⟩ := hA.spent p hp hα hsp
    rcases h with h | ⟨q, hq, hqb, h | ⟨he, hne⟩⟩
    · rw [hl] at h; exact Bool.noConfusion h
    · rw [(hsame q hq hqb).1] at h; exact Bool.noConfusion h
    · exact hne ((hsame q hq hqb).2 he)
  · by_cases hpb : p.blocker ∈ ᾱ'
    · exact (hA.spent p hp hpb hps).2.2 q hq hα hqs hm
    · exact (hA.dis _ hα p hp hpb).2.1 hm

/-- ⊢  a set with no bad member is admissible. -/
theorem LetAdmissible.of_no_bad {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar}
    (h : ∀ α ∈ ᾱ, ¬ LetBad Γ S S₁ ᾱ α) : LetAdmissible Γ S S₁ ᾱ := by
  -- the disjuncts, one name each
  have b4 : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ¬ ((∃ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∉ ᾱ) ∨
      ∃ q ∈ S.parked, p.stump = q.stump) :=
    fun p hp hb hn => h _ hb (.inr (.inr (.inr (.inl ⟨p, hp, rfl, hn⟩))))
  refine ⟨fun α hα => ?_, fun α hα β hβ => ?_, fun p hp hb q hq he => ?_,
    fun p hp hb => ?_, fun p hp hb q hq hqb δ hδp hδq => ?_, fun p hp hb hsp => ?_,
    fun α hα p hp hb => ?_, fun α hα hd => ?_⟩
  · exact Classical.byContradiction fun hn => h α hα (.inl hn)
  · refine ⟨fun hm => ?_, fun hm => ?_, fun hm => ?_⟩
    · exact h α hα (.inr (.inl ⟨β, hβ, .inl hm⟩))
    · exact h α hα (.inr (.inl ⟨β, hβ, .inr (.inl hm)⟩))
    · exact h α hα (.inr (.inl ⟨β, hβ, .inr (.inr hm)⟩))
  · exact b4 p hp hb (.inr ⟨q, hq, he⟩)
  · exact fun δ hδ => Classical.byContradiction fun hn =>
        b4 p hp hb (.inl ⟨δ, hδ, hn⟩)
  · exact Classical.byContradiction fun hn => h _ hqb
      (.inr (.inr (.inr (.inr (.inr (.inl ⟨q, hq, rfl, p, hp, hn, δ, hδp, hδq⟩))))))
  · have b8 : ¬ ((p.fillable S₁) = false ∨
        ∃ q ∈ S₁.parked, q.blocker = p.blocker ∧
          ((q.fillable S₁) = false ∨
           (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst ∧
            q.stump ≠ p.stump))) :=
      fun hn => h _ hb (.inr (.inr (.inr (.inr (.inr (.inr (.inl ⟨p, hp, rfl, hsp, hn⟩)))))))
    refine ⟨?_, fun q hq hqb => ⟨?_, fun he => ?_⟩, fun q hq hqb hqs hm => ?_⟩
    · cases hc : (p.fillable S₁)
      · exact absurd (.inl hc) b8
      · rfl
    · cases hc : (q.fillable S₁)
      · exact absurd (.inr ⟨q, hq, hqb, .inl hc⟩) b8
      · rfl
    · exact Classical.byContradiction fun hn => b8 (.inr ⟨q, hq, hqb, .inr ⟨he, hn⟩⟩)
    · exact h _ hqb (.inr (.inr (.inr (.inr (.inr (.inr (.inr
        ⟨q, hq, rfl, hqs, p, hp, hsp, hm⟩)))))))
  · refine ⟨fun hm => ?_, fun hm => ?_, fun hm => ?_⟩
    · exact h α hα (.inr (.inr (.inr (.inr (.inl ⟨p, hp, hb, .inl hm⟩)))))
    · exact h α hα (.inr (.inr (.inr (.inr (.inl ⟨p, hp, hb, .inr (.inl hm)⟩)))))
    · exact h α hα (.inr (.inr (.inr (.inr (.inl ⟨p, hp, hb, .inr (.inr hm)⟩)))))
  · exact h α hα (.inr (.inr (.inl hd)))

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

/-- the ᾱ a function should choose at A-let -/
def greatestAlpha [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) : List TyVar :=
  letPrune Γ S S₁ (KEnv.dom S₁.kinds)

/-- ⊢  **the greatest admissible ᾱ exists, and `greatestAlpha` computes it.** -/
theorem greatestAlpha_spec [DecidableEq B] (Γ : QCtx B) (S S₁ : SolverState B) :
    LetAdmissible Γ S S₁ (greatestAlpha Γ S S₁) ∧
    ∀ ᾱ, LetAdmissible Γ S S₁ ᾱ → ∀ α ∈ ᾱ, α ∈ greatestAlpha Γ S S₁ := by
  obtain ⟨hA, -, hgr⟩ := letPrune_spec Γ S S₁ (KEnv.dom S₁.kinds)
  exact ⟨hA, fun ᾱ h => hgr ᾱ h.kinded h⟩

end MinimalCalculus
