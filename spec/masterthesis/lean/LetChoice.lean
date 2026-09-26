-- A-let'S CHOICE OF ᾱ IS CANONICAL: THERE IS A GREATEST ADMISSIBLE ᾱ.
--
-- `Infer.letE` REQUIRES its split; it does not say how to find one. A function
-- has to choose ᾱ, and the question `OpenEnds.lean` §2 left open was whether a
-- choice exists that every other admissible choice sits below — otherwise the
-- algorithm would have to guess, and lose principality at every let.
--
-- It does. Admissibility is a property of ᾱ's MEMBERSHIP, the empty ᾱ is
-- admissible, and admissible sets are closed under union (`LetAdmissible.union`).
-- The one premise that looked like it could break union — correctability, "no
-- generalized stump's row mentions another's result" — is saved by Δγ's own
-- premise: a stump that is generalized under ᾱ₁ but not under ᾱ₂ sits in Δγ₂,
-- and Δγ₂ may not mention anything ᾱ₂ generalizes.
--
-- The greatest ᾱ is COMPUTED, not searched for: `letPrune` deletes every
-- variable that no admissible subset can contain (`LetBad`) until nothing is
-- deleted. Each deletion is forced, so every admissible ᾱ survives it; at the
-- fixpoint nothing is bad, which is admissibility.

import Infer

namespace MinimalCalculus

deriving instance DecidableEq for Ty, Row
deriving instance DecidableEq for Stump

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
through membership only. `kinded` is `Assigns` read as a set condition. -/
structure LetAdmissible (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) : Prop where
  kinded   : ∀ α ∈ ᾱ, α ∈ KEnv.dom S₁.kinds
  gfresh   : ∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv
  own      : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ∀ q ∈ S.parked, p.stump ≠ q.stump
  res      : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ →
               S₁.subst.ty p.stump.res = .var (S₁.resVar p.stump.res) ∧ S₁.resVar p.stump.res ∈ ᾱ
  inj      : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ∀ q ∈ S₁.parked, q.blocker ∈ ᾱ →
               S₁.resVar p.stump.res = S₁.resVar q.stump.res → p.stump = q.stump
  dis      : ∀ α ∈ ᾱ, ∀ p ∈ S₁.parked, p.blocker ∉ ᾱ →
               α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧ α ∉ (S₁.subst.ty p.stump.res).ftv
  unsolved : ∀ α ∈ ᾱ, α ∉ S₁.sol.dom
  indep    : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ → ∀ q ∈ S₁.parked, q.blocker ∈ ᾱ →
               S₁.resVar q.stump.res ∉ (p.stump.row.applySubst S₁.subst).ftv

/-- ⊢  nothing generalized is always admissible. -/
theorem LetAdmissible.nil {Γ : QCtx B} {S S₁ : SolverState B} :
    LetAdmissible Γ S S₁ [] :=
  ⟨fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil,
   fun _ _ h => absurd h List.not_mem_nil, fun _ _ h => absurd h List.not_mem_nil,
   fun _ _ h => absurd h List.not_mem_nil,
   fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil,
   fun _ _ h => absurd h List.not_mem_nil⟩

/-- ⊢  **admissible choices are closed under union.** -/
theorem LetAdmissible.union {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ₁ ᾱ₂ : List TyVar}
    (h₁ : LetAdmissible Γ S S₁ ᾱ₁) (h₂ : LetAdmissible Γ S S₁ ᾱ₂) :
    LetAdmissible Γ S S₁ (ᾱ₁ ++ ᾱ₂) := by
  -- correctability, from ONE side: q is generalized under hj; p either is too
  -- (hj's own correctability) or sits in hj's Δγ (hj's Δγ premise)
  have side : ∀ {ᾱ : List TyVar}, LetAdmissible Γ S S₁ ᾱ →
      ∀ p ∈ S₁.parked, ∀ q ∈ S₁.parked, q.blocker ∈ ᾱ →
      S₁.resVar q.stump.res ∉ (p.stump.row.applySubst S₁.subst).ftv := by
    intro ᾱ hj p hp q hq hqb
    by_cases hpb : p.blocker ∈ ᾱ
    · exact hj.indep p hp hpb q hq hqb
    · exact (hj.dis _ (hj.res q hq hqb).2 p hp hpb).1
  -- one result per stump, from ONE side: if p is not generalized there, it sits
  -- in that side's Δγ, which may not mention the result q is generalized at
  have sideInj : ∀ {ᾱ : List TyVar}, LetAdmissible Γ S S₁ ᾱ →
      ∀ p ∈ S₁.parked, ∀ q ∈ S₁.parked, q.blocker ∈ ᾱ →
      S₁.subst.ty p.stump.res = .var (S₁.resVar p.stump.res) →
      S₁.resVar p.stump.res = S₁.resVar q.stump.res → p.stump = q.stump := by
    intro ᾱ hj p hp q hq hqb hpv he
    by_cases hpb : p.blocker ∈ ᾱ
    · exact hj.inj p hp hpb q hq hqb he
    · refine absurd ?_ (hj.dis _ (hj.res q hq hqb).2 p hp hpb).2
      rw [hpv, ← he]; simp [Ty.ftv]
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
    · exact ⟨(h₁.res p hp h).1, List.mem_append_left _ (h₁.res p hp h).2⟩
    · exact ⟨(h₂.res p hp h).1, List.mem_append_right _ (h₂.res p hp h).2⟩
  · intro p hp hb q hq hqb he
    have hpv : S₁.subst.ty p.stump.res = .var (S₁.resVar p.stump.res) := by
      rcases List.mem_append.mp hb with h | h
      · exact (h₁.res p hp h).1
      · exact (h₂.res p hp h).1
    rcases List.mem_append.mp hqb with h | h
    · exact sideInj h₁ p hp q hq h hpv he
    · exact sideInj h₂ p hp q hq h hpv he
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
  · intro p hp _ q hq hqb
    rcases List.mem_append.mp hqb with h | h
    · exact side h₁ p hp q hq h
    · exact side h₂ p hp q hq h

/-- ⊢  admissibility only reads ᾱ's membership. -/
theorem LetAdmissible.congr {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ ᾱ' : List TyVar}
    (he : ∀ α, α ∈ ᾱ ↔ α ∈ ᾱ') (h : LetAdmissible Γ S S₁ ᾱ) :
    LetAdmissible Γ S S₁ ᾱ' := by
  refine ⟨fun α hα => h.kinded α ((he α).mpr hα), fun α hα => h.gfresh α ((he α).mpr hα),
    fun p hp hb => h.own p hp ((he _).mpr hb),
    fun p hp hb => ⟨(h.res p hp ((he _).mpr hb)).1, (he _).mp (h.res p hp ((he _).mpr hb)).2⟩,
    fun p hp hb q hq hqb => h.inj p hp ((he _).mpr hb) q hq ((he _).mpr hqb),
    fun α hα p hp hb => h.dis α ((he α).mpr hα) p hp (fun h' => hb ((he _).mp h')),
    fun α hα => h.unsolved α ((he α).mpr hα),
    fun p hp hb q hq hqb => h.indep p hp ((he _).mpr hb) q hq ((he _).mpr hqb)⟩

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
     fun p hp q hq => hA.inj p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2
       q (mem_letQ.mp hq).1 (mem_letQ.mp hq).2⟩
    (fun α hα p hp => hA.dis α hα p (mem_letG.mp hp).1 (mem_letG.mp hp).2)
    hA.unsolved
    (fun p hp q hq => hA.indep p (mem_letQ.mp hp).1 (mem_letQ.mp hp).2
      q (mem_letQ.mp hq).1 (mem_letQ.mp hq).2)
    h₂

--------------------- THE GREATEST ᾱ, COMPUTED --------------------------------

/-- α is FORCED OUT of every admissible subset of ᾱ. One disjunct per way a
single variable can make admissibility fail. -/
def LetBad (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) (α : TyVar) : Prop :=
  α ∉ KEnv.dom S₁.kinds ∨
  (∃ β ∈ Γ.ftv, α ∈ (S₁.subst.ty β).ftv ∨ α ∈ (S₁.subst.row β).ftv) ∨
  α ∈ S₁.sol.dom ∨
  -- a stump blocked on α answers with a non-variable or outside ᾱ, or was
  -- parked before the let
  (∃ p ∈ S₁.parked, p.blocker = α ∧
     (S₁.subst.ty p.stump.res ≠ .var (S₁.resVar p.stump.res) ∨ S₁.resVar p.stump.res ∉ ᾱ ∨
      ∃ q ∈ S.parked, p.stump = q.stump)) ∨
  -- a stump that must stay in Δ_Γ mentions α
  (∃ p ∈ S₁.parked, p.blocker ∉ ᾱ ∧
     (α ∈ (p.stump.row.applySubst S₁.subst).ftv ∨ α ∈ (S₁.subst.ty p.stump.res).ftv)) ∨
  -- generalizing a stump blocked on α would generalize its result, which some
  -- parked row mentions
  (∃ q ∈ S₁.parked, q.blocker = α ∧
     ∃ p ∈ S₁.parked, S₁.resVar q.stump.res ∈ (p.stump.row.applySubst S₁.subst).ftv) ∨
  -- a stump blocked on α reads its result as the same variable as another one
  (∃ q ∈ S₁.parked, q.blocker = α ∧
     ∃ p ∈ S₁.parked, p.stump ≠ q.stump ∧ S₁.resVar p.stump.res = S₁.resVar q.stump.res ∧
       S₁.subst.ty p.stump.res = .var (S₁.resVar p.stump.res))

instance [DecidableEq B] {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar} {α : TyVar} :
    Decidable (LetBad Γ S S₁ ᾱ α) := by
  unfold LetBad; infer_instance

/-- ⊢  a bad variable is in no admissible subset. -/
theorem LetBad.excluded {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ ᾱ' : List TyVar}
    {α : TyVar} (hbad : LetBad Γ S S₁ ᾱ α) (hsub : ∀ β ∈ ᾱ', β ∈ ᾱ)
    (hA : LetAdmissible Γ S S₁ ᾱ') : α ∉ ᾱ' := by
  intro hα
  rcases hbad with h | ⟨β, hβ, h⟩ | h | ⟨p, hp, rfl, h⟩ | ⟨p, hp, hb, h⟩ | ⟨q, hq, rfl, p, hp, h⟩ |
    ⟨q, hq, rfl, p, hp, hne, he, hpv⟩
  · exact h (hA.kinded _ hα)
  · rcases h with h | h
    · exact (hA.gfresh _ hα β hβ).1 h
    · exact (hA.gfresh _ hα β hβ).2 h
  · exact hA.unsolved _ hα h
  · rcases h with h | h | ⟨q, hq, he⟩
    · exact h (hA.res p hp hα).1
    · exact h (hsub _ (hA.res p hp hα).2)
    · exact hA.own p hp hα q hq he
  · have hb' : p.blocker ∉ ᾱ' := fun h' => hb (hsub _ h')
    rcases h with h | h
    · exact (hA.dis _ hα p hp hb').1 h
    · exact (hA.dis _ hα p hp hb').2 h
  · by_cases hpb : p.blocker ∈ ᾱ'
    · exact hA.indep p hp hpb q hq hα h
    · exact (hA.dis _ (hA.res q hq hα).2 p hp hpb).1 h
  · by_cases hpb : p.blocker ∈ ᾱ'
    · exact hne (hA.inj p hp hpb q hq hα he)
    · refine (hA.dis _ (hA.res q hq hα).2 p hp hpb).2 ?_
      rw [hpv, he]; simp [Ty.ftv]

/-- ⊢  a set with no bad member is admissible. -/
theorem LetAdmissible.of_no_bad {Γ : QCtx B} {S S₁ : SolverState B} {ᾱ : List TyVar}
    (h : ∀ α ∈ ᾱ, ¬ LetBad Γ S S₁ ᾱ α) : LetAdmissible Γ S S₁ ᾱ := by
  have hres : ∀ p ∈ S₁.parked, p.blocker ∈ ᾱ →
      S₁.subst.ty p.stump.res = .var (S₁.resVar p.stump.res) ∧ S₁.resVar p.stump.res ∈ ᾱ :=
    fun p hp hb => ⟨Classical.byContradiction fun hn =>
        h _ hb (.inr (.inr (.inr (.inl ⟨p, hp, rfl, .inl hn⟩)))),
      Classical.byContradiction fun hn =>
        h _ hb (.inr (.inr (.inr (.inl ⟨p, hp, rfl, .inr (.inl hn)⟩))))⟩
  refine ⟨fun α hα => ?_, fun α hα β hβ => ?_, fun p hp hb q hq he => ?_,
    hres, fun p hp hb q hq hqb he => ?_, fun α hα p hp hb => ?_, fun α hα hd => ?_,
    fun p hp _ q hq hqb hm => ?_⟩
  · exact Classical.byContradiction fun hn => h α hα (.inl hn)
  · refine ⟨fun hm => ?_, fun hm => ?_⟩
    · exact h α hα (.inr (.inl ⟨β, hβ, .inl hm⟩))
    · exact h α hα (.inr (.inl ⟨β, hβ, .inr hm⟩))
  · exact h _ hb (.inr (.inr (.inr (.inl ⟨p, hp, rfl, .inr (.inr ⟨q, hq, he⟩)⟩))))
  · exact Classical.byContradiction fun hn => h _ hqb
      (.inr (.inr (.inr (.inr (.inr (.inr ⟨q, hq, rfl, p, hp, hn, he, (hres p hp hb).1⟩))))))
  · refine ⟨fun hm => ?_, fun hm => ?_⟩
    · exact h α hα (.inr (.inr (.inr (.inr (.inl ⟨p, hp, hb, .inl hm⟩)))))
    · exact h α hα (.inr (.inr (.inr (.inr (.inl ⟨p, hp, hb, .inr hm⟩)))))
  · exact h α hα (.inr (.inr (.inl hd)))
  · exact h _ hqb (.inr (.inr (.inr (.inr (.inr (.inl ⟨q, hq, rfl, p, hp, hm⟩))))))

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
