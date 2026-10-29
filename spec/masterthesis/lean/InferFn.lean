-- INFERENCE AS A FUNCTION.
--
-- `Infer` is a relation: every choice a function would have to make is one of
-- its premises. This module makes every choice, in the pattern `unifyTyF` set
-- (RowUnify/Defs.lean): an executable function with FUEL, whose answers are
-- `ok`, a `fail` (a verdict: clash, occurs, stuck, a spent promise, …) or
-- `oof` (out of fuel), together with
--
--   * SOUNDNESS for `ok`: every answer is a derivation of the relation
--     (`inferF_sound`, `runF_sound`), so `runSound` types it declaratively;
--   * TERMINATION: some fuel always gives a verdict (`inferF_terminates`).
--
-- The choices, rule by rule:
--   * equations — `unifyTyF` at the given fuel;
--   * lookups — `lookupQF`, structural: no fuel, since nothing is chased;
--   * saturation — `saturateF` wakes the FIRST stale stump, until none is;
--   * A-var — the renaming is the next |ᾱ| names of the supply, in binder
--     order; `FreshRenaming` is CHECKED, as is that each binder has a kind;
--   * A-let — `greatestAlpha` (LetChoice.lean), the greatest admissible ᾱ.

import LetChoice
import Finalization

namespace MinimalCalculus

deriving instance DecidableEq for Parked
deriving instance DecidableEq for Atom

variable {B : Type} [DecidableEq B]

--------------------- THE RESULT TYPE -----------------------------------------

/-- what a fuelled run answers -/
inductive IRes (α : Type) where
  | ok   : α → IRes α
  | fail : String → IRes α
  | oof  : IRes α
  deriving Repr

instance : Monad IRes where
  pure := .ok
  bind x f := match x with
    | .ok a   => f a
    | .fail m => .fail m
    | .oof    => .oof

@[simp] theorem IRes.bind_eq_ok {α β : Type} {x : IRes α} {f : α → IRes β} {b : β} :
    (x >>= f) = .ok b ↔ ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x <;> simp [bind]

/-- replace a failure's message -/
def IRes.withMsg {α : Type} (x : IRes α) (msg : String) : IRes α :=
  match x with
  | .ok a   => .ok a
  | .fail _ => .fail msg
  | .oof    => .oof

@[simp] theorem IRes.withMsg_eq_ok {α : Type} {x : IRes α} {msg : String} {a : α} :
    x.withMsg msg = .ok a ↔ x = .ok a := by
  cases x <;> simp [IRes.withMsg]

@[simp] theorem IRes.pure_eq_ok {α : Type} {a b : α} :
    (pure a : IRes α) = .ok b ↔ a = b := by
  simp [pure]

/-- a failure becomes the fallback `d`; running out of fuel stays -/
def IRes.orElse {α : Type} (x : IRes α) (d : α) : IRes α :=
  match x with
  | .ok a   => .ok a
  | .fail _ => .ok d
  | .oof    => .oof

theorem IRes.orElse_eq_ok {α : Type} {x : IRes α} {d a : α} (h : x.orElse d = .ok a) :
    x = .ok a ∨ a = d := by
  cases x <;> simp_all [IRes.orElse]

/-- try `x`; on a failure, `y` -/
def IRes.orTry {α : Type} (x y : IRes α) : IRes α :=
  match x with
  | .ok a   => .ok a
  | .fail _ => y
  | .oof    => .oof

theorem IRes.orTry_eq_ok {α : Type} {x y : IRes α} {a : α} (h : x.orTry y = .ok a) :
    x = .ok a ∨ y = .ok a := by
  cases x <;> simp_all [IRes.orTry]

--------------------- LOOKUP ---------------------------------------------------
-- One keyed lookup, structural: with L-α gone there is no solution to chase, so
-- no fuel and no way to fail — the `IRes` is only there so lookups compose with
-- the fuelled steps in `do`-blocks. A label key is the key `⌊l⌋`
-- (`LookupQ.lab_iff`).

/-- what `lookupQF` answers: `LookupQ`'s three results, with `?` carrying its blocker -/
inductive LOut (B : Type) where
  | found   : Ty B → LOut B
  | absent  : LOut B
  | blocked : TyVar → LOut B

/-- what an answer asserts of a keyed lookup -/
def LOut.HoldsQ (ρ : Row B) (q : Key) : LOut B → Prop
  | .found τ   => LookupQ ρ q (.found τ)
  | .absent    => LookupQ ρ q .absent
  | .blocked α => LookupBlockedQ ρ q α

/-- the blocker of an undecided comparison of lookup key k with field key q:
the lookup key when it is a label variable (`dunkQ`, `sunk`), the field key
otherwise (`dunkF`). Two literals always compare, so the last arm is never
reached. -/
def keyBlocker (k q : Key) : TyVar :=
  match k, q with
  | .var α, _ => α
  | .lit _, .var γ => γ
  | .lit _, .lit _ => ""

/-- `ρ.k ↓ r`, computed -/
def lookupQF : Row B → Key → IRes (LOut B)
  | .empty, _ => .ok .absent
  | .var β, _ => .ok (.blocked β)
  | .sing l τ, k =>
      match Key.cmp k (.lit l) with
      | .eq    => .ok (.found τ)
      | .apart => .ok .absent
      | .undec => .ok (.blocked (keyBlocker k (.lit l)))
  | .dsing q τ, k =>
      match Key.cmp k q with
      | .eq    => .ok (.found τ)
      | .apart => .ok .absent
      | .undec => .ok (.blocked (keyBlocker k q))
  | .cat ρ₁ ρ₂, k =>
      match lookupQF ρ₁ k with
      | .ok (.found τ)   => .ok (.found τ)
      | .ok .absent      => lookupQF ρ₂ k
      | .ok (.blocked α) => .ok (.blocked α)
      | .fail m          => .fail m
      | .oof             => .oof

omit [DecidableEq B] in
theorem lookupQF_sound :
    ∀ {ρ : Row B} {k : Key} {r : LOut B}, lookupQF ρ k = .ok r → r.HoldsQ ρ k
  | .empty, _, r, h => by
      simp only [lookupQF, IRes.ok.injEq] at h; subst h; exact .emp
  | .var β, _, r, h => by
      simp only [lookupQF, IRes.ok.injEq] at h; subst h; exact .varFree
  | .sing l τ, k, r, h => by
      simp only [lookupQF] at h
      split at h
      · rename_i he; cases h; exact .hit he
      · rename_i he; cases h; exact .miss he
      · rename_i he; cases h
        obtain ⟨α, rfl⟩ := Key.cmp_undec_lit he
        exact .sunk
  | .dsing q τ, k, r, h => by
      simp only [lookupQF] at h
      split at h
      · rename_i he; cases h; exact .dhit he
      · rename_i he; cases h; exact .dmiss he
      · rename_i he; cases h
        cases k with
        | var α => exact .dunkQ he
        | lit l =>
            obtain ⟨γ, rfl⟩ := Key.cmp_lit_undec he
            exact .dunkF
  | .cat ρ₁ ρ₂, k, r, h => by
      simp only [lookupQF] at h
      split at h
      · rename_i τ h₁; cases h; exact .catHit (lookupQF_sound h₁)
      · rename_i h₁
        have ih₂ := lookupQF_sound h
        cases r with
        | found τ => exact .catSkip (lookupQF_sound h₁) ih₂
        | absent => exact .catSkip (lookupQF_sound h₁) ih₂
        | blocked β => exact .catSkip (lookupQF_sound h₁) ih₂
      · rename_i α h₁; cases h; exact .catUnk (lookupQF_sound h₁)
      · cases h
      · cases h

omit [DecidableEq B] in
/-- ⊢  an answer other than `blocked β` rules out being blocked on β -/
theorem LOut.not_blockedQ {ρ : Row B} {q : Key} {r : LOut B} {β : TyVar}
    (h : r.HoldsQ ρ q) (hne : r ≠ .blocked β) : ¬ LookupBlockedQ ρ q β := by
  intro hb
  cases r with
  | found τ => exact absurd (h.det hb.toLookupQ) (by simp)
  | absent => exact absurd (h.det hb.toLookupQ) (by simp)
  | blocked α => exact hne (by rw [LookupBlockedQ.det h hb])

--------------------- EQUATIONS ------------------------------------------------

/-- `S ⊢ τ ≐ τ′ ⇝ S′`, computed -/
def solveTyF (n : Nat) (S : SolverState B) (τ τ' : Ty B) : IRes (SolverState B) :=
  match unifyTyF (S.supplyTy (τ.applySubst S.subst) (τ'.applySubst S.subst)) n
      (τ.applySubst S.subst) (τ'.applySubst S.subst) with
  | .success s Sup => .ok (S.extend s Sup)
  | .clash         => .fail "clash"
  | .occurs        => .fail "occurs"
  | .stuck         => .fail "stuck"
  | .outOfFuel     => .oof

theorem solveTyF_sound {n : Nat} {S S' : SolverState B} {τ τ' : Ty B}
    (h : solveTyF n S τ τ' = .ok S') : SolveTy S τ τ' S' := by
  unfold solveTyF at h
  split at h
  · rename_i s Sup hu; cases h; exact ⟨n, s, Sup, hu, rfl⟩
  all_goals cases h

--------------------- WAKE-UP AND SATURATION ----------------------------------

/-- one wake-up step on `p`: K-hit, K-⊥ or K-repark, as the lookup decides -/
def wakeF (n : Nat) (S : SolverState B) (p : Parked B) : IRes (SolverState B) := do
  match ← lookupQF (p.stump.row.applySubst S.subst)
      (p.stump.label.applySubst S.subst) with
  | .found τ =>
      let S' ← solveTyF n S p.stump.res τ
      pure { S' with parked := S'.parked.filter (·.stump != p.stump) }
  | .absent =>
      let S' ← solveTyF n S p.stump.res .unk
      pure ({ S' with parked := S'.parked.filter (·.stump != p.stump) }.flag
        (p.stump.label.applySubst S.subst).keyName)
  | .blocked α' =>
      pure (({ S with parked := S.parked.filter (·.stump != p.stump) }).park
        ⟨α', p.stump⟩)

theorem wakeF_sound {n : Nat} {S S' : SolverState B} {p : Parked B}
    (h : wakeF n S p = .ok S') : Wake S p S' := by
  simp only [wakeF, IRes.bind_eq_ok] at h
  obtain ⟨r, hr, h⟩ := h
  have hH := lookupQF_sound hr
  cases r with
  | found τ =>
      simp only [IRes.bind_eq_ok, IRes.pure_eq_ok] at h
      obtain ⟨S₁, hs, rfl⟩ := h
      exact .hit hH (solveTyF_sound hs)
  | absent =>
      simp only [IRes.bind_eq_ok, IRes.pure_eq_ok] at h
      obtain ⟨S₁, hs, rfl⟩ := h
      exact .abs hH (solveTyF_sound hs)
  | blocked α =>
      simp only [IRes.pure_eq_ok] at h
      subst h; exact .repark hH

/-- the first parked stump whose recorded blocker no longer blocks it -/
def staleF (n : Nat) (S : SolverState B) : List (Parked B) → IRes (Option (Parked B))
  | [] => .ok none
  | p :: ps => do
      match ← lookupQF (p.stump.row.applySubst S.subst)
          (p.stump.label.applySubst S.subst) with
      | .blocked α => if α = p.blocker then staleF n S ps else pure (some p)
      | _ => pure (some p)

theorem staleF_none {n : Nat} {S : SolverState B} :
    ∀ {ps : List (Parked B)}, staleF n S ps = .ok none →
      ∀ p ∈ ps, LookupBlockedQ (p.stump.row.applySubst S.subst)
        (p.stump.label.applySubst S.subst) p.blocker
  | [], _, _, hp => absurd hp List.not_mem_nil
  | q :: qs, h, p, hp => by
      simp only [staleF, IRes.bind_eq_ok] at h
      obtain ⟨r, hr, h⟩ := h
      cases r with
      | blocked α =>
          simp only at h
          split at h
          · rename_i he
            rcases List.mem_cons.mp hp with rfl | hp
            · exact he ▸ lookupQF_sound hr
            · exact staleF_none h p hp
          · simp at h
      | found τ => simp at h
      | absent => simp at h

theorem staleF_some {n : Nat} {S : SolverState B} {p : Parked B} :
    ∀ {ps : List (Parked B)}, staleF n S ps = .ok (some p) →
      p ∈ ps ∧ ¬ LookupBlockedQ (p.stump.row.applySubst S.subst)
        (p.stump.label.applySubst S.subst) p.blocker
  | [], h => by simp [staleF] at h
  | q :: qs, h => by
      simp only [staleF, IRes.bind_eq_ok] at h
      obtain ⟨r, hr, h⟩ := h
      have hH := lookupQF_sound hr
      cases r with
      | blocked α =>
          simp only at h
          split at h
          · obtain ⟨hm, hb⟩ := staleF_some h
            exact ⟨List.mem_cons_of_mem _ hm, hb⟩
          · rename_i hne
            simp only [IRes.pure_eq_ok, Option.some.injEq] at h; subst h
            exact ⟨List.mem_cons_self, LOut.not_blockedQ (r := .blocked α) hH (by simpa using hne)⟩
      | found τ =>
          simp only [IRes.pure_eq_ok, Option.some.injEq] at h; subst h
          exact ⟨List.mem_cons_self, LOut.not_blockedQ (r := .found τ) hH (by simp)⟩
      | absent =>
          simp only [IRes.pure_eq_ok, Option.some.injEq] at h; subst h
          exact ⟨List.mem_cons_self, LOut.not_blockedQ (r := .absent) hH (by simp)⟩

/-- `S ⊢ ↝! S′`, computed: wake the first stale stump until none is -/
def saturateF : Nat → SolverState B → IRes (SolverState B)
  | 0, _ => .oof
  | n + 1, S => do
      match ← staleF n S S.parked with
      | none => pure S
      | some p =>
          let S₁ ← wakeF n S p
          saturateF n S₁

theorem saturateF_sound : ∀ {n : Nat} {S S' : SolverState B},
    saturateF n S = .ok S' → Saturate S S'
  | 0, _, _, h => by simp [saturateF] at h
  | n + 1, S, S', h => by
      simp only [saturateF, IRes.bind_eq_ok] at h
      obtain ⟨o, ho, h⟩ := h
      cases o with
      | none =>
          simp only [IRes.pure_eq_ok] at h; subst h
          exact .done (staleF_none ho)
      | some p =>
          simp only [IRes.bind_eq_ok] at h
          obtain ⟨S₁, hw, hs⟩ := h
          obtain ⟨hm, hnb⟩ := staleF_some ho
          exact .step hm hnb (wakeF_sound hw) (saturateF_sound hs)

/-- `S ⊢ τ ≐ τ′ ⇝! S′`, computed -/
def solveTySatF (n : Nat) (S : SolverState B) (τ τ' : Ty B) : IRes (SolverState B) := do
  let S₁ ← solveTyF n S τ τ'
  saturateF n S₁

theorem solveTySatF_sound {n : Nat} {S S' : SolverState B} {τ τ' : Ty B}
    (h : solveTySatF n S τ τ' = .ok S') : SolveTySat S τ τ' S' := by
  simp only [solveTySatF, IRes.bind_eq_ok] at h
  obtain ⟨S₁, h₁, h₂⟩ := h
  exact ⟨S₁, solveTyF_sound h₁, saturateF_sound h₂⟩

/-- `S ⊢ Q ↝* S′` for A-var: each instantiated constraint is either parked on
the blocker its lookup reports (K-park) or woken at once. Returns the parked
images — the blockers are what the lookups said. -/
def wakesF (n : Nat) : SolverState B → List (Stump B) → IRes (List (Parked B) × SolverState B)
  | S, [] => .ok ([], S)
  | S, st :: sts => do
      match ← lookupQF (st.row.applySubst S.subst) (st.label.applySubst S.subst) with
      | .blocked α =>
          let r ← wakesF n (S.park ⟨α, st⟩) sts
          pure (⟨α, st⟩ :: r.1, r.2)
      | _ =>
          -- it resolves at once, so it never waits and its blocker is never read
          let S₁ ← wakeF n S ⟨"", st⟩
          let r ← wakesF n S₁ sts
          pure (⟨"", st⟩ :: r.1, r.2)

theorem wakesF_sound {n : Nat} : ∀ {S S' : SolverState B} {sts : List (Stump B)}
    {ps : List (Parked B)}, wakesF n S sts = .ok (ps, S') →
      ps.map Parked.stump = sts ∧ Wakes S ps S'
  | S, S', [], ps, h => by
      simp only [wakesF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact ⟨rfl, .nil⟩
  | S, S', st :: sts, ps, h => by
      simp only [wakesF, IRes.bind_eq_ok] at h
      obtain ⟨r, hr, h⟩ := h
      have hH := lookupQF_sound hr
      cases r with
      | blocked α =>
          simp only [IRes.bind_eq_ok, IRes.pure_eq_ok] at h
          obtain ⟨⟨ps', S''⟩, hrec, he⟩ := h
          simp only [Prod.mk.injEq] at he
          obtain ⟨rfl, rfl⟩ := he
          obtain ⟨hm, hw⟩ := wakesF_sound hrec
          exact ⟨by simp [hm], .park hH hw⟩
      | found τ =>
          simp only [IRes.bind_eq_ok, IRes.pure_eq_ok] at h
          obtain ⟨S₁, hw₁, ⟨ps', S''⟩, hrec, he⟩ := h
          simp only [Prod.mk.injEq] at he
          obtain ⟨rfl, rfl⟩ := he
          obtain ⟨hm, hw⟩ := wakesF_sound hrec
          exact ⟨by simp [hm], .cons (wakeF_sound hw₁) hw⟩
      | absent =>
          simp only [IRes.bind_eq_ok, IRes.pure_eq_ok] at h
          obtain ⟨S₁, hw₁, ⟨ps', S''⟩, hrec, he⟩ := h
          simp only [Prod.mk.injEq] at he
          obtain ⟨rfl, rfl⟩ := he
          obtain ⟨hm, hw⟩ := wakesF_sound hrec
          exact ⟨by simp [hm], .cons (wakeF_sound hw₁) hw⟩

--------------------- THE FORCED MERGE -----------------------------------------

/-- p and q ask one lookup at S (`Merge`'s premises) and their results still
differ there -/
def Parked.mergeable (S : SolverState B) (p q : Parked B) : Bool :=
  decide ((p.stump.row.applySubst S.subst).toSpine = (q.stump.row.applySubst S.subst).toSpine) &&
  decide (p.stump.label.applySubst S.subst = q.stump.label.applySubst S.subst) &&
  !decide (p.stump.res.applySubst S.subst = q.stump.res.applySubst S.subst)

/-- p and q wait on one blocker for one key, and their results still differ:
the witness merge (`Guess`), not forced -/
def Parked.alike (S : SolverState B) (p q : Parked B) : Bool :=
  decide (p.blocker = q.blocker) &&
  decide (p.stump.label.applySubst S.subst = q.stump.label.applySubst S.subst) &&
  !decide (p.stump.res.applySubst S.subst = q.stump.res.applySubst S.subst)

/-- unify the results of one pair of stumps if the test `t` passes, computed. A
stump is found by its STUMP, not its blocker: an earlier step's saturation may
have re-parked it. A failed equation skips the pair. -/
def pairF (t : SolverState B → Parked B → Parked B → Bool) (n : Nat) (S : SolverState B)
    (st₁ st₂ : Stump B) : IRes (SolverState B) :=
  match S.parked.find? (·.stump == st₁), S.parked.find? (·.stump == st₂) with
  | some p, some q =>
      if t S p q then (solveTySatF n S p.stump.res q.stump.res).orElse S
      else pure S
  | _, _ => pure S

theorem pairF_spec {t : SolverState B → Parked B → Parked B → Bool} {n : Nat}
    {S S' : SolverState B} {st₁ st₂ : Stump B} (h : pairF t n S st₁ st₂ = .ok S') :
    S' = S ∨ ∃ p ∈ S.parked, ∃ q ∈ S.parked, t S p q = true ∧
      SolveTySat S p.stump.res q.stump.res S' := by
  unfold pairF at h
  split at h
  · next p q hp hq =>
    split at h
    · next hm =>
      rcases IRes.orElse_eq_ok h with h | rfl
      · exact .inr ⟨p, List.mem_of_find?_eq_some hp, q, List.mem_of_find?_eq_some hq, hm,
          solveTySatF_sound h⟩
      · exact .inl rfl
    · simp only [IRes.pure_eq_ok] at h; exact .inl h.symm
  · simp only [IRes.pure_eq_ok] at h; exact .inl h.symm

/-- the pairs of a list, each once, in order -/
def pairsOf {α : Type} : List α → List (α × α)
  | [] => []
  | a :: as => as.map (a, ·) ++ pairsOf as

def pairsF (t : SolverState B → Parked B → Parked B → Bool) (n : Nat) :
    SolverState B → List (Stump B × Stump B) → IRes (SolverState B)
  | S, [] => .ok S
  | S, (st₁, st₂) :: ps => do
      let S₁ ← pairF t n S st₁ st₂
      pairsF t n S₁ ps

theorem Merges.trans {S₁ S₂ S₃ : SolverState B} :
    Merges S₁ S₂ → Merges S₂ S₃ → Merges S₁ S₃
  | .refl _, h => h
  | .step hm hms, h => .step hm (Merges.trans hms h)

/-- ⊢  with the test `mergeable`, every step is an M-merge. -/
theorem pairsF_merges {n : Nat} : ∀ {S S' : SolverState B} {ps : List (Stump B × Stump B)},
    pairsF Parked.mergeable n S ps = .ok S' → Merges S S'
  | S, S', [], h => by simp only [pairsF, IRes.ok.injEq] at h; subst h; exact .refl _
  | S, S', (st₁, st₂) :: ps, h => by
      simp only [pairsF, IRes.bind_eq_ok] at h
      obtain ⟨S₁, h₁, h₂⟩ := h
      refine Merges.trans ?_ (pairsF_merges h₂)
      rcases pairF_spec h₁ with rfl | ⟨p, hp, q, hq, hm, hs⟩
      · exact .refl _
      · simp only [Parked.mergeable, Bool.and_eq_true, decide_eq_true_eq] at hm
        exact .step (.merge hp hq hm.1.1 hm.1.2 hs) (.refl _)

/-- ⊢  with any test, every step is a step of the witness search. -/
theorem pairsF_guesses {t : SolverState B → Parked B → Parked B → Bool} {n : Nat} :
    ∀ {S S' : SolverState B} {ps : List (Stump B × Stump B)},
      pairsF t n S ps = .ok S' → Guesses S S'
  | S, S', [], h => by simp only [pairsF, IRes.ok.injEq] at h; subst h; exact .refl _
  | S, S', (st₁, st₂) :: ps, h => by
      simp only [pairsF, IRes.bind_eq_ok] at h
      obtain ⟨S₁, h₁, h₂⟩ := h
      refine Guesses.trans ?_ (pairsF_guesses h₂)
      rcases pairF_spec h₁ with rfl | ⟨_, _, _, _, _, hs⟩
      · exact .refl _
      · exact .step (.solve hs) (.refl _)

/-- the merge phase: every pair of stumps parked at S, once -/
def mergeAllF (n : Nat) (S : SolverState B) : IRes (SolverState B) :=
  pairsF Parked.mergeable n S (pairsOf (S.parked.map (·.stump)))

theorem mergeAllF_sound {n : Nat} {S S' : SolverState B} (h : mergeAllF n S = .ok S') :
    Merges S S' :=
  pairsF_merges h

--------------------- THE WITNESS SEARCH ---------------------------------------

/-- K-guess for one key variable, computed: the first candidate label whose
equation goes through; none, if none does. -/
def keyGuessF (n : Nat) (S : SolverState B) (κ : TyVar) : List Label → IRes (SolverState B)
  | [] => pure S
  | l :: ls => (solveTySatF n S (.lab (.var κ)) (.lab (.lit l))).orTry (keyGuessF n S κ ls)

theorem keyGuessF_sound {n : Nat} {S : SolverState B} {κ : TyVar} :
    ∀ {ls : List Label} {S' : SolverState B}, keyGuessF n S κ ls = .ok S' → Guesses S S'
  | [], S', h => by simp only [keyGuessF, IRes.pure_eq_ok] at h; subst h; exact .refl _
  | l :: ls, S', h => by
      simp only [keyGuessF] at h
      rcases IRes.orTry_eq_ok h with h | h
      · exact .step (.solve (solveTySatF_sound h)) (.refl _)
      · exact keyGuessF_sound h

/-- the key variables the witness guesses, each with its candidates: the
variable key of a stump on a spent stump's blocker (the spent stump itself
included). First a label no program writes, then the literal labels of the
stump's row, for a lookup that has to hit. -/
def guessTargets (S : SolverState B) : List (TyVar × List Label) :=
  let spent := S.parked.filter fun p => !(p.stump.res.applySubst S.subst).isVar
  S.parked.filterMap fun p =>
    match p.stump.label.applySubst S.subst with
    | .var κ =>
        if spent.any (·.blocker == p.blocker) then
          some (κ, Key.keyName (.var κ) :: (p.stump.row.applySubst S.subst).toSpine.filterMap
            (fun | .field l _ => some l | _ => none))
        else none
    | .lit _ => none

def keyGuessesF (n : Nat) : SolverState B → List (TyVar × List Label) → IRes (SolverState B)
  | S, [] => .ok S
  | S, (κ, ls) :: ts => do
      let S₁ ← keyGuessF n S κ ls
      keyGuessesF n S₁ ts

theorem keyGuessesF_sound {n : Nat} :
    ∀ {S S' : SolverState B} {ts : List (TyVar × List Label)},
      keyGuessesF n S ts = .ok S' → Guesses S S'
  | S, S', [], h => by simp only [keyGuessesF, IRes.ok.injEq] at h; subst h; exact .refl _
  | S, S', (κ, ls) :: ts, h => by
      simp only [keyGuessesF, IRes.bind_eq_ok] at h
      obtain ⟨S₁, h₁, h₂⟩ := h
      exact (keyGuessF_sound h₁).trans (keyGuessesF_sound h₂)

/-- the witness search: guess the keys, then the witness merge on every pair -/
def witnessF (n : Nat) (S : SolverState B) : IRes (SolverState B) := do
  let S₁ ← keyGuessesF n S (guessTargets S)
  pairsF Parked.alike n S₁ (pairsOf (S₁.parked.map (·.stump)))

theorem witnessF_sound {n : Nat} {S S' : SolverState B} (h : witnessF n S = .ok S') :
    Guesses S S' := by
  simp only [witnessF, IRes.bind_eq_ok] at h
  obtain ⟨S₁, h₁, h₂⟩ := h
  exact (keyGuessesF_sound h₁).trans (pairsF_guesses h₂)

--------------------- A-var ----------------------------------------------------

/-- the renaming A-var draws: binder number i goes to the supply's i-th name -/
def varRen (next : Nat) (vs : List TyVar) (α : TyVar) : TyVar := natName (next + vs.idxOf α)

/-- the substitution that renames `vs` by `f` and fixes everything else -/
def renSubst (vs : List TyVar) (f : TyVar → TyVar) : TySubst B :=
  ⟨fun α => if α ∈ vs then .var (f α) else .var α,
   fun α => if α ∈ vs then .var (f α) else .var α,
   fun α => if α ∈ vs then .var (f α) else .var α⟩

omit [DecidableEq B] in
theorem renSubst_isRenaming (vs : List TyVar) (f : TyVar → TyVar) :
    IsRenaming (renSubst (B := B) vs f) vs f :=
  ⟨⟨fun α h => by simp [renSubst, h], fun α h => by simp [renSubst, h],
    fun α h => by simp [renSubst, h]⟩,
   fun α h => by simp [renSubst, h]⟩

/-- the constraints of an instance, before their blockers are known -/
def instStumps (θ : TySubst B) (f : TyVar → TyVar) (Q : List (Stump B)) : List (Stump B) :=
  Q.map (fun st => ⟨st.row.applySubst θ, st.label.applySubst θ, st.res.applySubst θ⟩)

instance {f : TyVar → TyVar} {vs : List TyVar} {Γ : QCtx B} {S : SolverState B} :
    Decidable (FreshRenaming f vs Γ S) := by
  unfold FreshRenaming; infer_instance

/-- A-var, computed: draw the renaming, wake the instantiated constraints,
saturate. Freshness and kinds are CHECKED, so a violated supply invariant is a
`fail`, never an unsound answer. -/
def varF (n : Nat) (Γ : QCtx B) (S : SolverState B) (σ : QScheme B) :
    IRes (Ty B × SolverState B) :=
  let f := varRen S.supply.next σ.vars
  if FreshRenaming f σ.vars Γ S then
    if ∀ α ∈ σ.vars, (S.kinds.lookup α).isSome then do
      let κs := σ.vars.map (fun α => (S.kinds.lookup α).getD .ty)
      let θ : TySubst B := renSubst σ.vars f
      let r ← wakesF n { S with supply := ⟨S.supply.next + σ.vars.length⟩,
                                kinds := (σ.vars.map f).zip κs ++ S.kinds }
        (instStumps θ f σ.constraints)
      let S' ← saturateF n r.2
      pure (σ.body.applySubst θ, S')
    else .fail "A-var: a binder has no recorded kind"
  else .fail "A-var: renaming not fresh"

theorem varF_sound {C : Type} {constTy : Sig B C} {n : Nat} {Γ : QCtx B}
    {S S' : SolverState B} {x : Var} {σ : QScheme B} {τ : Ty B}
    (hl : Γ.lookup x = some σ) (h : varF n Γ S σ = .ok (τ, S')) :
    Infer constTy Γ S (.var x) τ S' := by
  unfold varF at h
  dsimp only at h
  by_cases hfr : FreshRenaming (varRen S.supply.next σ.vars) σ.vars Γ S
  · by_cases hk : ∀ α ∈ σ.vars, (S.kinds.lookup α).isSome
    · rw [if_pos hfr, if_pos hk] at h
      simp only [IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨ps, S₁⟩, hw, S₂, hs, rfl, rfl⟩ := h
      obtain ⟨hm, hws⟩ := wakesF_sound hw
      refine Infer.var (Sup := ⟨S.supply.next + σ.vars.length⟩) hl
        (renSubst_isRenaming _ _) hfr
        (fun α hα => ⟨S.supply.next + σ.vars.idxOf α, Nat.le_add_right _ _,
          Nat.add_lt_add_left (List.idxOf_lt_length_of_mem hα) _, rfl⟩)
        (Nat.le_add_right _ _) ?_ hm ⟨S₁, hws, saturateF_sound hs⟩
      unfold KEnv.Assigns
      rw [List.map_map]
      refine List.map_congr_left (fun α hα => ?_)
      obtain ⟨κ, hκ⟩ := Option.isSome_iff_exists.mp (hk α hα)
      simp [hκ]
    · rw [if_pos hfr, if_neg hk] at h; cases h
  · rw [if_neg hfr] at h; cases h

--------------------- INFERENCE ------------------------------------------------

variable {C : Type}

/-- the sort a tag names -/
def Srt.toKind : Srt → Kind
  | .ty => .ty | .row => .row | .lab => .lab

/-- A-prim's instance: every variable of the builtin's type renamed to a fresh
name, drawn at the sort it occurs at. The state is the one the relation asks
for: supply advanced past the block, the kinds recorded. -/
def primInst (S : SolverState B) (τp : Ty B) : TySubst B × SolverState B :=
  let vs := ((Ty.sortedFtv τp).map Prod.snd).eraseDups
  let f := varRen S.supply.next vs
  let κ : TyVar → Kind := fun α =>
    (((Ty.sortedFtv τp).find? (·.2 == α)).map (·.1.toKind)).getD .ty
  (renSubst vs f,
   { S with supply := ⟨S.supply.next + vs.length⟩,
            kinds := vs.map (fun α => (f α, κ α)) ++ S.kinds })

mutual

/-- `Γ; S ⊢ e ⇒ τ; S′`, computed -/
def inferF (constTy : Sig B C) (n : Nat) :
    QCtx B → SolverState B → Expr C → IRes (Ty B × SolverState B)
  | _, S, .con c => .ok (.base (constTy c), S)
  | Γ, S, .var x =>
      match Γ.lookup x with
      | none   => .fail "unbound variable"
      | some σ => varF n Γ S σ
  | Γ, S, .lam x e => do
      let r ← inferF constTy n (Γ.bindTy x (.var (S.draw .ty).1)) (S.draw .ty).2 e
      pure (.fn (.var (S.draw .ty).1) r.1, r.2)
  | Γ, S, .app e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let r₂ ← inferF constTy n Γ r₁.2 e₂
      let S₃ ← solveTySatF n (r₂.2.draw .ty).2 r₁.1 (.fn r₂.1 (.var (r₂.2.draw .ty).1))
      pure (.var (r₂.2.draw .ty).1, S₃)
  | Γ, S, .rcdDyn e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let r₂ ← inferF constTy n Γ r₁.2 e₂
      match (r₁.1.applySubst r₂.2.subst).asBase with
      | some _ => pure (.unk, r₂.2.flag Key.baseName)
      | none => do
        let S₃ ← solveTySatF n (r₂.2.draw .lab).2 r₁.1 (.lab (.var (r₂.2.draw .lab).1))
        pure (.rcd (.dsing (.var (r₂.2.draw .lab).1) r₂.1), S₃)
  | Γ, S, .cat e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let r₂ ← inferF constTy n Γ r₁.2 e₂
      let Sa := (r₂.2.draw .row).2
      let S₃ ← solveTySatF n (Sa.draw .row).2 r₁.1 (.rcd (.var (r₂.2.draw .row).1))
      let S₄ ← solveTySatF n S₃ r₂.1 (.rcd (.var (Sa.draw .row).1))
      pure (.rcd (.cat (.var (Sa.draw .row).1) (.var (r₂.2.draw .row).1)), S₄)
  | Γ, S, .sel e l => do
      let r₁ ← inferF constTy n Γ S e
      let rv := (r₁.2.draw .row).1
      let S₂ ← solveTySatF n (r₁.2.draw .row).2 r₁.1 (.rcd (.var rv))
      match ← lookupQF ((Row.var rv).applySubst S₂.subst) (.lit l) with
      | .found τ' => pure (τ', S₂)
      | .absent   => pure (.unk, S₂.flag l)
      | .blocked α =>
          pure (.var (S₂.draw .ty).1,
            (S₂.draw .ty).2.park ⟨α, ⟨.var rv, .lit l, .var (S₂.draw .ty).1⟩⟩)
  | _, S, .lab l => .ok (.lab (.lit l), S)
  | Γ, S, .selDyn e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let rv := (r₁.2.draw .row).1
      let S₂ ← solveTySatF n (r₁.2.draw .row).2 r₁.1 (.rcd (.var rv))
      let r₂ ← inferF constTy n Γ S₂ e₂
      match (r₂.1.applySubst r₂.2.subst).asBase with
      | some _ => pure (.unk, r₂.2.flag Key.baseName)
      | none => do
        let κ := (r₂.2.draw .lab).1
        let S₄ ← solveTySatF n (r₂.2.draw .lab).2 r₂.1 (.lab (.var κ))
        match ← lookupQF ((Row.var rv).applySubst S₄.subst) ((Key.var κ).applySubst S₄.subst) with
        | .found τ' => pure (τ', S₄)
        | .absent   => pure (.unk, S₄.flag ((Key.var κ).applySubst S₄.subst).keyName)
        | .blocked α =>
            pure (.var (S₄.draw .ty).1,
              (S₄.draw .ty).2.park ⟨α, ⟨.var rv, .var κ, .var (S₄.draw .ty).1⟩⟩)
  | Γ, S, .rcd ξ => do
      let r ← inferRecF constTy n Γ S ξ
      pure (.rcd r.1, r.2)
  | Γ, S, .letE x e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let S₁ ← mergeAllF n r₁.2
      let ᾱ := greatestAlpha Γ S S₁ r₁.1
      inferF constTy n (Γ.bindScheme x (letScheme S₁ ᾱ (letQ S₁ ᾱ) r₁.1))
        { S₁ with parked := letG S₁ ᾱ } e₂
  -- MacroNix
  | Γ, S, .prim p es =>
      match constTy.prim p with
      | none => .fail "A-prim: unknown builtin"
      | some τp => do
          let ra ← inferArgsF constTy n Γ (primInst S τp).2 es
          let S₂ ← solveTySatF n (ra.2.draw .ty).2 (τp.applySubst (primInst S τp).1)
            (Ty.arrows ra.1 (.var (ra.2.draw .ty).1))
          pure (.var (ra.2.draw .ty).1, S₂)
  | _, S, .bool _ =>
      match constTy.boolB with
      | none => .fail "no booleans"
      | some bb => .ok (.base bb, S)
  | Γ, S, .ite e e₁ e₂ =>
      match constTy.boolB with
      | none => .fail "no booleans"
      | some bb => do
          let r ← inferF constTy n Γ S e
          let S₂ ← solveTySatF n r.2 r.1 (.base bb)
          let r₁ ← inferF constTy n Γ S₂ e₁
          let r₂ ← inferF constTy n Γ r₁.2 e₂
          let S₅ ← solveTySatF n r₂.2 r₁.1 r₂.1
          pure (r₁.1, S₅)
  | Γ, S, .asrt e e₂ =>
      match constTy.boolB with
      | none => .fail "no booleans"
      | some bb => do
          let r ← inferF constTy n Γ S e
          let S₂ ← solveTySatF n r.2 r.1 (.base bb)
          inferF constTy n Γ S₂ e₂
  | _, S, .nil => .ok (.list (.var (S.draw .ty).1), (S.draw .ty).2)
  | Γ, S, .cons e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let r₂ ← inferF constTy n Γ r₁.2 e₂
      let S₃ ← solveTySatF n r₂.2 r₂.1 (.list r₁.1)
      pure (.list r₁.1, S₃)
  | Γ, S, .has e _ =>
      match constTy.boolB with
      | none => .fail "no booleans"
      | some bb => do
          let r ← inferF constTy n Γ S e
          let S₂ ← solveTySatF n (r.2.draw .row).2 r.1 (.rcd (.var (r.2.draw .row).1))
          pure (.base bb, S₂)

def inferRecF (constTy : Sig B C) (n : Nat) :
    QCtx B → SolverState B → RecBody (Expr C) → IRes (Row B × SolverState B)
  | _, S, .empty => .ok (.empty, S)
  | Γ, S, .field l e => do
      let r ← inferF constTy n Γ S e
      pure (.sing l r.1, r.2)
  | Γ, S, .cat ξ₁ ξ₂ => do
      let r₁ ← inferRecF constTy n Γ S ξ₁
      let r₂ ← inferRecF constTy n Γ r₁.2 ξ₂
      pure (.cat r₁.1 r₂.1, r₂.2)

def inferArgsF (constTy : Sig B C) (n : Nat) :
    QCtx B → SolverState B → List (Expr C) → IRes (List (Ty B) × SolverState B)
  | _, S, [] => .ok ([], S)
  | Γ, S, e :: es => do
      let r ← inferF constTy n Γ S e
      let rs ← inferArgsF constTy n Γ r.2 es
      pure (r.1 :: rs.1, rs.2)

end

mutual

/-- ⊢  **every answer of `inferF` is a derivation of `Infer`.** -/
theorem inferF_sound {constTy : Sig B C} {n : Nat} :
    ∀ {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B},
      inferF constTy n Γ S e = .ok (τ, S') → Infer constTy Γ S e τ S'
  | _, S, S', .con c, τ, h => by
      simp only [inferF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact .con
  | Γ, S, S', .var x, τ, h => by
      simp only [inferF] at h
      split at h
      · cases h
      · rename_i σ hl; exact varF_sound hl h
  | Γ, S, S', .lam x e, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ', S''⟩, hb, rfl, rfl⟩ := h
      exact .lam rfl (inferF_sound hb)
  | Γ, S, S', .app e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, ⟨τ₂, S₂⟩, h₂, S₃, h₃, rfl, rfl⟩ := h
      exact .app (inferF_sound h₁) (inferF_sound h₂) rfl (solveTySatF_sound h₃)
  | Γ, S, S', .rcdDyn e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, ⟨τ₂, S₂⟩, h₂, h⟩ := h
      split at h
      · rename_i b hb
        simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact .rcdDynBase (inferF_sound h₁) (inferF_sound h₂) (Ty.asBase_eq_some hb)
      · simp only [IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
        obtain ⟨S₃, h₃, rfl, rfl⟩ := h
        exact .rcdDyn (inferF_sound h₁) (inferF_sound h₂) rfl (solveTySatF_sound h₃)
  | Γ, S, S', .cat e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, ⟨τ₂, S₂⟩, h₂, S₃, h₃, S₄, h₄, rfl, rfl⟩ := h
      exact .conc (inferF_sound h₁) (inferF_sound h₂) rfl rfl
        (solveTySatF_sound h₃) (solveTySatF_sound h₄)
  | Γ, S, S', .sel e l, τ, h => by
      simp only [inferF, IRes.bind_eq_ok] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, S₂, h₂, o, ho, h⟩ := h
      have hH := lookupQF_sound ho
      cases o with
      | found τ' =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .sel (inferF_sound h₁) rfl (solveTySatF_sound h₂) (LookupQ.lab_iff.mp hH)
      | absent =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .selAbs (inferF_sound h₁) rfl (solveTySatF_sound h₂) (LookupQ.lab_iff.mp hH)
      | blocked α =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .selUnk (inferF_sound h₁) rfl (solveTySatF_sound h₂)
            (LookupBlockedQ.toLit hH) rfl
  | _, S, S', .lab l, τ, h => by
      simp only [inferF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact .lab
  | Γ, S, S', .selDyn e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, S₂, h₂, ⟨τ₂, S₃⟩, h₃, h⟩ := h
      split at h
      · rename_i b hb
        simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact .selDynBase (inferF_sound h₁) rfl (solveTySatF_sound h₂) (inferF_sound h₃)
          (Ty.asBase_eq_some hb)
      simp only [IRes.bind_eq_ok] at h
      obtain ⟨S₄, h₄, o, ho, h⟩ := h
      have hH := lookupQF_sound ho
      cases o with
      | found τ' =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .selDyn (inferF_sound h₁) rfl (solveTySatF_sound h₂) (inferF_sound h₃) rfl
            (solveTySatF_sound h₄) hH
      | absent =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .selDynAbs (inferF_sound h₁) rfl (solveTySatF_sound h₂) (inferF_sound h₃) rfl
            (solveTySatF_sound h₄) hH
      | blocked α =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .selDynUnk (inferF_sound h₁) rfl (solveTySatF_sound h₂)
            (inferF_sound h₃) rfl (solveTySatF_sound h₄) hH rfl
  | Γ, S, S', .rcd ξ, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨ρ, S''⟩, hb, rfl, rfl⟩ := h
      exact .rcd (inferRecF_sound hb)
  | Γ, S, S', .letE x e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, S₁', hm, h₂⟩ := h
      exact .letE (inferF_sound h₁) (mergeAllF_sound hm) (greatestAlpha_spec Γ S S₁' τ₁).1
        (inferF_sound h₂)
  | Γ, S, S', .prim p es, τ, h => by
      simp only [inferF] at h
      split at h
      · cases h
      · rename_i τp hp
        simp only [IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
        obtain ⟨⟨τs, S₁⟩, ha, S₂, hs, rfl, rfl⟩ := h
        exact .prim hp (Nat.le_add_right _ _) (inferArgsF_sound ha) rfl (solveTySatF_sound hs)
  | _, S, S', .bool b, τ, h => by
      simp only [inferF] at h
      split at h
      · cases h
      · rename_i bb hb
        simp only [IRes.ok.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; exact .bool hb
  | Γ, S, S', .ite e e₁ e₂, τ, h => by
      simp only [inferF] at h
      split at h
      · cases h
      · rename_i bb hb
        simp only [IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
        obtain ⟨⟨τ₀, S₁⟩, h₀, S₂, hs₁, ⟨τ₁, S₃⟩, h₁, ⟨τ₂, S₄⟩, h₂, S₅, hs₂, rfl, rfl⟩ := h
        exact .ite hb (inferF_sound h₀) (solveTySatF_sound hs₁) (inferF_sound h₁)
          (inferF_sound h₂) (solveTySatF_sound hs₂)
  | Γ, S, S', .asrt e e₂, τ, h => by
      simp only [inferF] at h
      split at h
      · cases h
      · rename_i bb hb
        simp only [IRes.bind_eq_ok] at h
        obtain ⟨⟨τ₀, S₁⟩, h₀, S₂, hs, h₂⟩ := h
        exact .asrt hb (inferF_sound h₀) (solveTySatF_sound hs) (inferF_sound h₂)
  | _, S, S', .nil, τ, h => by
      simp only [inferF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact .nil rfl
  | Γ, S, S', .cons e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, ⟨τ₂, S₂⟩, h₂, S₃, hs, rfl, rfl⟩ := h
      exact .cons (inferF_sound h₁) (inferF_sound h₂) (solveTySatF_sound hs)
  | Γ, S, S', .has e l, τ, h => by
      simp only [inferF] at h
      split at h
      · cases h
      · rename_i bb hb
        simp only [IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
        obtain ⟨⟨τ₀, S₁⟩, h₀, S₂, hs, rfl, rfl⟩ := h
        exact .has hb (inferF_sound h₀) rfl (solveTySatF_sound hs)

theorem inferRecF_sound {constTy : Sig B C} {n : Nat} :
    ∀ {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B},
      inferRecF constTy n Γ S ξ = .ok (ρ, S') → InferRec constTy Γ S ξ ρ S'
  | _, S, S', .empty, ρ, h => by
      simp only [inferRecF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact .empty
  | Γ, S, S', .field l e, ρ, h => by
      simp only [inferRecF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ, S''⟩, hb, rfl, rfl⟩ := h
      exact .field (inferF_sound hb)
  | Γ, S, S', .cat ξ₁ ξ₂, ρ, h => by
      simp only [inferRecF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨ρ₁, S₁⟩, h₁, ⟨ρ₂, S₂⟩, h₂, rfl, rfl⟩ := h
      exact .cat (inferRecF_sound h₁) (inferRecF_sound h₂)

theorem inferArgsF_sound {constTy : Sig B C} {n : Nat} :
    ∀ {Γ : QCtx B} {S S' : SolverState B} {es : List (Expr C)} {τs : List (Ty B)},
      inferArgsF constTy n Γ S es = .ok (τs, S') → InferArgs constTy Γ S es τs S'
  | _, S, S', [], τs, h => by
      simp only [inferArgsF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact .nil
  | Γ, S, S', e :: es, τs, h => by
      simp only [inferArgsF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ, S₁⟩, h₁, ⟨τs', S₂⟩, h₂, rfl, rfl⟩ := h
      exact .cons (inferF_sound h₁) (inferArgsF_sound h₂)

end

--------------------- A RUN ----------------------------------------------------

/-- the state every run starts in -/
def runStart : SolverState B := ⟨Sol.nil, [], [], ⟨1⟩, []⟩

/-- a run that answers a SCHEME (plans/top-level-schemes.md): infer from
nothing, merge what asks one lookup, then generalize like A-let with Γ = ∅.
Nothing is decided: a spent promise stays a constraint, and the run fails only
when `LetSpent` finds no instance of the scheme -/
def runF (constTy : Sig B C) (n : Nat) (e : Expr C) : IRes (QScheme B × SolverState B) := do
  let r ← inferF constTy n QCtx.empty runStart e
  let S₂ ← mergeAllF n r.2
  let S₃ ← witnessF n S₂
  if LetSpent S₃ S₃.parked then
    pure (letScheme S₂ (letCand S₂ r.1) S₂.parked r.1, S₂)
  else .fail "spent promise without witness"

/-- ⊢  every answer of `runF` is a `Run`. -/
theorem runF_sound {constTy : Sig B C} {n : Nat} {e : Expr C} {σ : QScheme B}
    {S : SolverState B} (h : runF constTy n e = .ok (σ, S)) : Run constTy e σ := by
  simp only [runF, IRes.bind_eq_ok] at h
  obtain ⟨⟨τ, S₁⟩, h₁, S₂, h₂, S₃, h₃, h⟩ := h
  split at h
  · next hs =>
    simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
    exact ⟨τ, S₁, S₂, S₃, inferF_sound h₁, mergeAllF_sound h₂, h.1.symm, witnessF_sound h₃, hs⟩
  · cases h

/-- ⊢  **what `runF` answers is sound**: every instance types the program, and
there is one. -/
theorem runF_typed {constTy : Sig B C} {n : Nat} {e : Expr C} {σ : QScheme B}
    {S : SolverState B} (h : runF constTy n e = .ok (σ, S)) :
    (∀ τ, σ.Inst τ → QTyped constTy QCtx.empty e τ) ∧ ∃ τ, σ.Inst τ :=
  runSound e σ (runF_sound h)

end MinimalCalculus
