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
--   * lookups — `lookupF`, fuel spent on each solved variable it chases;
--   * saturation — `saturateF` wakes the FIRST stale stump, until none is;
--   * A-var — the renaming is the next |ᾱ| names of the supply, in binder
--     order; `FreshRenaming` is CHECKED, as is that each binder has a kind;
--   * A-let — `greatestAlpha` (LetChoice.lean), the greatest admissible ᾱ.

import LetChoice
import Finalization

namespace MinimalCalculus

deriving instance DecidableEq for Parked

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

--------------------- LOOKUP ---------------------------------------------------

/-- what `lookupF` answers: `Lookup`'s three results, with `?` carrying its blocker -/
inductive LOut (B : Type) where
  | found   : Ty B → LOut B
  | absent  : LOut B
  | blocked : TyVar → LOut B

/-- what an answer asserts of the row -/
def LOut.Holds (Γ : Ctx B) (ρ : Row B) (l : Label) : LOut B → Prop
  | .found τ   => Lookup Γ ρ l (.found τ)
  | .absent    => Lookup Γ ρ l .absent
  | .blocked α => LookupBlocked Γ ρ l α

/-- `Γ ⊢ ρ.l ↓ r`, computed. Fuel is spent on each solved variable chased. -/
def lookupF (Γ : Ctx B) : Nat → Row B → Label → IRes (LOut B)
  | _, .empty, _ => .ok .absent
  | _, .sing l' τ, l => if l' = l then .ok (.found τ) else .ok .absent
  | n, .cat ρ₁ ρ₂, l =>
      match lookupF Γ n ρ₁ l with
      | .ok (.found τ)   => .ok (.found τ)
      | .ok .absent      => lookupF Γ n ρ₂ l
      | .ok (.blocked α) => .ok (.blocked α)
      | .fail m          => .fail m
      | .oof             => .oof
  | 0, .var _, _ => .oof
  | n + 1, .var α, l =>
      match Γ.lookupRow α with
      | none   => .ok (.blocked α)
      | some ρ => lookupF Γ n ρ l

omit [DecidableEq B] in
theorem lookupF_sound {Γ : Ctx B} :
    ∀ {n : Nat} {ρ : Row B} {l : Label} {r : LOut B},
      lookupF Γ n ρ l = .ok r → r.Holds Γ ρ l
  | _, .empty, _, r, h => by
      simp only [lookupF, IRes.ok.injEq] at h; subst h; exact .emp
  | _, .sing l' τ, l, r, h => by
      simp only [lookupF] at h
      split at h
      · rename_i he; subst he; cases h; exact .hit
      · rename_i hne; cases h; exact .miss hne
  | n, .cat ρ₁ ρ₂, l, r, h => by
      simp only [lookupF] at h
      split at h
      · rename_i τ h₁; cases h; exact .catHit (lookupF_sound h₁)
      · rename_i h₁
        have ih₂ := lookupF_sound h
        cases r with
        | found τ => exact .catSkip (lookupF_sound h₁) ih₂
        | absent => exact .catSkip (lookupF_sound h₁) ih₂
        | blocked β => exact .catSkip (lookupF_sound h₁) ih₂
      · rename_i α h₁; cases h; exact .catUnk (lookupF_sound h₁)
      · cases h
      · cases h
  | 0, .var _, _, _, h => by simp [lookupF] at h
  | n + 1, .var α, l, r, h => by
      simp only [lookupF] at h
      split at h
      · rename_i hα; cases h; exact .varFree hα
      · rename_i ρ hα
        have ih := lookupF_sound h
        cases r with
        | found τ => exact .var hα ih
        | absent => exact .var hα ih
        | blocked β => exact .var hα ih

omit [DecidableEq B] in
/-- ⊢  an answer other than `blocked β` rules out being blocked on β -/
theorem LOut.not_blocked {Γ : Ctx B} {ρ : Row B} {l : Label} {r : LOut B} {β : TyVar}
    (h : r.Holds Γ ρ l) (hne : r ≠ .blocked β) : ¬ LookupBlocked Γ ρ l β := by
  intro hb
  cases r with
  | found τ => exact absurd (lookup_det h hb.toLookup) (by simp)
  | absent => exact absurd (lookup_det h hb.toLookup) (by simp)
  | blocked α => exact hne (by rw [LookupBlocked.det h hb])

--------------------- EQUATIONS ------------------------------------------------

/-- `S ⊢ τ ≐ τ′ ⇝ S′`, computed -/
def solveTyF (n : Nat) (S : SolverState B) (τ τ' : Ty B) : IRes (SolverState B) :=
  match unifyTyF S.supply n (τ.applySubst S.subst) (τ'.applySubst S.subst) with
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
  match ← lookupF S.ctx n (p.stump.row.applySubst S.subst) p.stump.label with
  | .found τ =>
      let S' ← solveTyF n S (.var p.stump.res) τ
      pure { S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }
  | .absent =>
      let S' ← solveTyF n S (.var p.stump.res) .unk
      pure ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
        p.stump.label)
  | .blocked α' =>
      pure (({ S with parked := S.parked.filter (·.stump.res != p.stump.res) }).park
        ⟨α', p.stump⟩)

theorem wakeF_sound {n : Nat} {S S' : SolverState B} {p : Parked B}
    (h : wakeF n S p = .ok S') : Wake S p S' := by
  simp only [wakeF, IRes.bind_eq_ok] at h
  obtain ⟨r, hr, h⟩ := h
  have hH := lookupF_sound hr
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
      match ← lookupF S.ctx n (p.stump.row.applySubst S.subst) p.stump.label with
      | .blocked α => if α = p.blocker then staleF n S ps else pure (some p)
      | _ => pure (some p)

theorem staleF_none {n : Nat} {S : SolverState B} :
    ∀ {ps : List (Parked B)}, staleF n S ps = .ok none →
      ∀ p ∈ ps, LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label p.blocker
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
            · exact he ▸ lookupF_sound hr
            · exact staleF_none h p hp
          · simp at h
      | found τ => simp at h
      | absent => simp at h

theorem staleF_some {n : Nat} {S : SolverState B} {p : Parked B} :
    ∀ {ps : List (Parked B)}, staleF n S ps = .ok (some p) →
      p ∈ ps ∧ ¬ LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label p.blocker
  | [], h => by simp [staleF] at h
  | q :: qs, h => by
      simp only [staleF, IRes.bind_eq_ok] at h
      obtain ⟨r, hr, h⟩ := h
      have hH := lookupF_sound hr
      cases r with
      | blocked α =>
          simp only at h
          split at h
          · obtain ⟨hm, hb⟩ := staleF_some h
            exact ⟨List.mem_cons_of_mem _ hm, hb⟩
          · rename_i hne
            simp only [IRes.pure_eq_ok, Option.some.injEq] at h; subst h
            exact ⟨List.mem_cons_self, LOut.not_blocked (r := .blocked α) hH (by simpa using hne)⟩
      | found τ =>
          simp only [IRes.pure_eq_ok, Option.some.injEq] at h; subst h
          exact ⟨List.mem_cons_self, LOut.not_blocked (r := .found τ) hH (by simp)⟩
      | absent =>
          simp only [IRes.pure_eq_ok, Option.some.injEq] at h; subst h
          exact ⟨List.mem_cons_self, LOut.not_blocked (r := .absent) hH (by simp)⟩

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
      match ← lookupF S.ctx n (st.row.applySubst S.subst) st.label with
      | .blocked α =>
          let r ← wakesF n (S.park ⟨α, st⟩) sts
          pure (⟨α, st⟩ :: r.1, r.2)
      | _ =>
          let S₁ ← wakeF n S ⟨st.res, st⟩
          let r ← wakesF n S₁ sts
          pure (⟨st.res, st⟩ :: r.1, r.2)

theorem wakesF_sound {n : Nat} : ∀ {S S' : SolverState B} {sts : List (Stump B)}
    {ps : List (Parked B)}, wakesF n S sts = .ok (ps, S') →
      ps.map Parked.stump = sts ∧ Wakes S ps S'
  | S, S', [], ps, h => by
      simp only [wakesF, IRes.ok.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h; exact ⟨rfl, .nil⟩
  | S, S', st :: sts, ps, h => by
      simp only [wakesF, IRes.bind_eq_ok] at h
      obtain ⟨r, hr, h⟩ := h
      have hH := lookupF_sound hr
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

--------------------- FINALIZATION ---------------------------------------------

/-- F-★ on one stump, computed. A spent promise fails here. -/
def finalizeF (n : Nat) (S : SolverState B) (p : Parked B) : IRes (SolverState B) := do
  if p ∉ S.parked then .fail "finalize: stump no longer parked" else
  match ← lookupF S.ctx n (p.stump.row.applySubst S.subst) p.stump.label with
  | .blocked α =>
      if α = p.blocker then do
        let S' ← (solveTyF n S (.var p.stump.res) .unk).withMsg
          "spent promise: a stump's result is no longer a variable"
        pure ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
          p.stump.label)
      else .fail "finalize: stale blocker"
  | _ => .fail "finalize: lookup resolved"

theorem finalizeF_sound {n : Nat} {S S' : SolverState B} {p : Parked B}
    (h : finalizeF n S p = .ok S') : Finalize S p S' := by
  simp only [finalizeF] at h
  split at h
  · cases h
  · rename_i hm
    simp only [IRes.bind_eq_ok] at h
    obtain ⟨r, hr, h⟩ := h
    cases r with
    | blocked α =>
        simp only at h
        split at h
        · rename_i he
          simp only [IRes.bind_eq_ok, IRes.withMsg_eq_ok, IRes.pure_eq_ok] at h
          obtain ⟨S₁, hs, rfl⟩ := h
          exact .star (Decidable.of_not_not hm) (he ▸ lookupF_sound hr) (solveTyF_sound hs)
        · cases h
    | found τ => cases h
    | absent => cases h

def finalizesF (n : Nat) : SolverState B → List (Parked B) → IRes (SolverState B)
  | S, [] => .ok S
  | S, p :: ps => do
      let S₁ ← finalizeF n S p
      finalizesF n S₁ ps

theorem finalizesF_sound {n : Nat} : ∀ {S S' : SolverState B} {ps : List (Parked B)},
    finalizesF n S ps = .ok S' → Finalizes S ps S'
  | S, S', [], h => by simp only [finalizesF, IRes.ok.injEq] at h; subst h; exact .nil
  | S, S', p :: ps, h => by
      simp only [finalizesF, IRes.bind_eq_ok] at h
      obtain ⟨S₁, h₁, h₂⟩ := h
      exact .cons (finalizeF_sound h₁) (finalizesF_sound h₂)

--------------------- A-var ----------------------------------------------------

/-- the renaming A-var draws: binder number i goes to the supply's i-th name -/
def varRen (next : Nat) (vs : List TyVar) (α : TyVar) : TyVar := natName (next + vs.idxOf α)

/-- the substitution that renames `vs` by `f` and fixes everything else -/
def renSubst (vs : List TyVar) (f : TyVar → TyVar) : TySubst B :=
  ⟨fun α => if α ∈ vs then .var (f α) else .var α,
   fun α => if α ∈ vs then .var (f α) else .var α⟩

omit [DecidableEq B] in
theorem renSubst_isRenaming (vs : List TyVar) (f : TyVar → TyVar) :
    IsRenaming (renSubst (B := B) vs f) vs f :=
  ⟨⟨fun α h => by simp [renSubst, h], fun α h => by simp [renSubst, h]⟩,
   fun α h => by simp [renSubst, h]⟩

/-- the constraints of an instance, before their blockers are known -/
def instStumps (θ : TySubst B) (f : TyVar → TyVar) (Q : List (Stump B)) : List (Stump B) :=
  Q.map (fun st => ⟨st.row.applySubst θ, st.label, f st.res⟩)

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

theorem varF_sound {C : Type} {constTy : C → B} {n : Nat} {Γ : QCtx B}
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

mutual

/-- `Γ; S ⊢ e ⇒ τ; S′`, computed -/
def inferF (constTy : C → B) (n : Nat) :
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
      match ← lookupF S₂.ctx n (.var rv) l with
      | .found τ' => pure (τ', S₂)
      | .absent   => pure (.unk, S₂.flag l)
      | .blocked _ =>
          match ← lookupF S₂.ctx n ((Row.var rv).applySubst S₂.subst) l with
          | .blocked α =>
              pure (.var (S₂.draw .ty).1,
                (S₂.draw .ty).2.park ⟨α, ⟨.var rv, l, (S₂.draw .ty).1⟩⟩)
          | _ => .fail "A-sel: the two lookup forms disagree"
  | Γ, S, .rcd ξ => do
      let r ← inferRecF constTy n Γ S ξ
      pure (.rcd r.1, r.2)
  | Γ, S, .letE x e₁ e₂ => do
      let r₁ ← inferF constTy n Γ S e₁
      let ᾱ := greatestAlpha Γ S r₁.2
      inferF constTy n (Γ.bindScheme x (letScheme r₁.2 ᾱ (letQ r₁.2 ᾱ) r₁.1))
        { r₁.2 with parked := letG r₁.2 ᾱ } e₂

def inferRecF (constTy : C → B) (n : Nat) :
    QCtx B → SolverState B → RecBody (Expr C) → IRes (Row B × SolverState B)
  | _, S, .empty => .ok (.empty, S)
  | Γ, S, .field l e => do
      let r ← inferF constTy n Γ S e
      pure (.sing l r.1, r.2)
  | Γ, S, .cat ξ₁ ξ₂ => do
      let r₁ ← inferRecF constTy n Γ S ξ₁
      let r₂ ← inferRecF constTy n Γ r₁.2 ξ₂
      pure (.cat r₁.1 r₂.1, r₂.2)

end

mutual

/-- ⊢  **every answer of `inferF` is a derivation of `Infer`.** -/
theorem inferF_sound {constTy : C → B} {n : Nat} :
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
  | Γ, S, S', .cat e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, ⟨τ₂, S₂⟩, h₂, S₃, h₃, S₄, h₄, rfl, rfl⟩ := h
      exact .conc (inferF_sound h₁) (inferF_sound h₂) rfl rfl
        (solveTySatF_sound h₃) (solveTySatF_sound h₄)
  | Γ, S, S', .sel e l, τ, h => by
      simp only [inferF, IRes.bind_eq_ok] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, S₂, h₂, o, ho, h⟩ := h
      have hH := lookupF_sound ho
      cases o with
      | found τ' =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .sel (inferF_sound h₁) rfl (solveTySatF_sound h₂) hH
      | absent =>
          simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact .selAbs (inferF_sound h₁) rfl (solveTySatF_sound h₂) hH
      | blocked β =>
          simp only [IRes.bind_eq_ok] at h
          obtain ⟨o', ho', h⟩ := h
          cases o' with
          | blocked α =>
              simp only [IRes.pure_eq_ok, Prod.mk.injEq] at h
              obtain ⟨rfl, rfl⟩ := h
              exact .selUnk (inferF_sound h₁) rfl (solveTySatF_sound h₂)
                (lookupF_sound ho') rfl
          | found _ => cases h
          | absent => cases h
  | Γ, S, S', .rcd ξ, τ, h => by
      simp only [inferF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨⟨ρ, S''⟩, hb, rfl, rfl⟩ := h
      exact .rcd (inferRecF_sound hb)
  | Γ, S, S', .letE x e₁ e₂, τ, h => by
      simp only [inferF, IRes.bind_eq_ok] at h
      obtain ⟨⟨τ₁, S₁⟩, h₁, h₂⟩ := h
      exact ((greatestAlpha_spec Γ S S₁).1).letE (inferF_sound h₁) (inferF_sound h₂)

theorem inferRecF_sound {constTy : C → B} {n : Nat} :
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

end

--------------------- A RUN ----------------------------------------------------

/-- the state every run starts in -/
def runStart : SolverState B := ⟨Sol.nil, [], [], ⟨1⟩, []⟩

/-- a run, computed: infer from nothing, then finalize every parked stump -/
def runF (constTy : C → B) (n : Nat) (e : Expr C) : IRes (Ty B × SolverState B) := do
  let r ← inferF constTy n ⟨[], []⟩ runStart e
  let S' ← finalizesF n r.2 r.2.parked
  pure (r.1, S')

/-- ⊢  every answer of `runF` is a `Run`. -/
theorem runF_sound {constTy : C → B} {n : Nat} {e : Expr C} {τ : Ty B}
    {S' : SolverState B} (h : runF constTy n e = .ok (τ, S')) : Run constTy e τ S' := by
  simp only [runF, IRes.bind_eq_ok, IRes.pure_eq_ok, Prod.mk.injEq] at h
  obtain ⟨⟨τ₁, S₁⟩, h₁, S₂, h₂, rfl, rfl⟩ := h
  exact ⟨S₁, inferF_sound h₁, finalizesF_sound h₂⟩

/-- ⊢  **what `runF` answers is declaratively typed**, at the type it reports
read under its own final substitution. -/
theorem runF_typed {constTy : C → B} {n : Nat} {e : Expr C} {τ : Ty B}
    {S' : SolverState B} (h : runF constTy n e = .ok (τ, S')) :
    QTyped constTy ⟨[], []⟩ e (τ.applySubst S'.subst) :=
  runSound e τ S' (runF_sound h)

end MinimalCalculus
