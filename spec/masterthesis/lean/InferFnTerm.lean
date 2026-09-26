-- INFERENCE TERMINATES.
--
-- `inferF` (InferFn.lean) is structurally recursive in the term, so as a Lean
-- function it is total by construction; what is left to say is that its FUEL is
-- never the reason it stops. Two facts, for every fuelled piece:
--
--   * STABLE — once a budget suffices, every larger one gives the same verdict
--     (`Stable`, the image of `unifyM_fuel_mono` for the whole algorithm);
--   * SETTLES — some budget suffices (`Settles`), from a clean start. The
--     ingredients are the three termination results already proved: the
--     unifier's (`unifyTyF_terminates`), the lookup's (a clean state's row
--     solutions are acyclic, `Sol.rowWF_toCtx`), and saturation's (`satStep_wf`).
--
-- Together: `inferF_terminates` / `runF_terminates` — inference always reaches a
-- verdict, `ok` or a `fail` — and `infer`/`run`, the total functions they make.

import InferFn
import RowUnify.Termination
import OpenEnds

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- STABLE AND SETTLES ---------------------------------------

/-- more fuel never changes a verdict that was reached -/
def Stable {α : Type} (f : Nat → IRes α) : Prop :=
  ∀ m m', m ≤ m' → f m ≠ .oof → f m' = f m

/-- some fuel reaches a verdict -/
def Settles {α : Type} (f : Nat → IRes α) : Prop :=
  ∃ n, f n ≠ .oof

theorem Stable.const {α : Type} (x : IRes α) : Stable (fun _ => x) :=
  fun _ _ _ _ => rfl

theorem Stable.bind {α β : Type} {x : Nat → IRes α} {g : Nat → α → IRes β}
    (hx : Stable x) (hg : ∀ a, Stable (fun m => g m a)) :
    Stable (fun m => x m >>= g m) := by
  intro m m' hle h
  simp only at h ⊢
  cases hxm : x m with
  | oof => rw [hxm] at h; exact absurd rfl h
  | fail msg =>
      rw [hx m m' hle (by rw [hxm]; simp)]; rw [hxm]; rfl
  | ok a =>
      rw [hx m m' hle (by rw [hxm]; simp), hxm]
      rw [hxm] at h
      exact hg a m m' hle h

theorem Settles.bind {α β : Type} {x : Nat → IRes α} {g : Nat → α → IRes β}
    (hxs : Stable x) (hx : Settles x) (hgs : ∀ a, Stable (fun m => g m a))
    (hg : ∀ a n, x n = .ok a → Settles (fun m => g m a)) :
    Settles (fun m => x m >>= g m) := by
  obtain ⟨n₁, h₁⟩ := hx
  cases hxn : x n₁ with
  | oof => exact absurd hxn h₁
  | fail msg => exact ⟨n₁, by simp only; rw [hxn]; exact fun h => nomatch h⟩
  | ok a =>
      obtain ⟨n₂, h₂⟩ := hg a n₁ hxn
      refine ⟨max n₁ n₂, ?_⟩
      simp only
      rw [hxs n₁ _ (Nat.le_max_left _ _) (by rw [hxn]; simp), hxn]
      have h₂' : g n₂ a ≠ .oof := h₂
      have hst : g (max n₁ n₂) a = g n₂ a := hgs a n₂ _ (Nat.le_max_right _ _) h₂'
      show g (max n₁ n₂) a ≠ .oof
      rw [hst]; exact h₂'

theorem Settles.pure {α : Type} (a : α) : Settles (fun _ => (pure a : IRes α)) :=
  ⟨0, fun h => nomatch h⟩

theorem Settles.fail {α : Type} (msg : String) : Settles (fun _ => (IRes.fail msg : IRes α)) :=
  ⟨0, fun h => nomatch h⟩

theorem Stable.withMsg {α : Type} {x : Nat → IRes α} (hx : Stable x) (msg : String) :
    Stable (fun m => (x m).withMsg msg) := by
  intro m m' hle h
  simp only at h ⊢
  cases hxm : x m with
  | oof => rw [hxm] at h; exact absurd rfl h
  | fail _ => rw [hx m m' hle (by rw [hxm]; simp), hxm]
  | ok a => rw [hx m m' hle (by rw [hxm]; simp), hxm]

theorem Settles.withMsg {α : Type} {x : Nat → IRes α} (hx : Settles x) (msg : String) :
    Settles (fun m => (x m).withMsg msg) := by
  obtain ⟨n, h⟩ := hx
  refine ⟨n, ?_⟩
  cases hxn : x n with
  | oof => exact absurd hxn h
  | fail _ => simp [hxn, IRes.withMsg]
  | ok a => simp [hxn, IRes.withMsg]

/-- a stable family that settles at n answers the same at every larger budget -/
theorem Stable.settled {α : Type} {f : Nat → IRes α} (hs : Stable f) {n m : Nat}
    (hle : n ≤ m) (h : f n ≠ .oof) : f m ≠ .oof := by
  rw [hs n m hle h]; exact h

--------------------- LOOKUP ---------------------------------------------------

omit [DecidableEq B] in
private theorem lookupF_mono {Γ : Ctx B} {l : Label} :
    ∀ (m : Nat) (ρ : Row B) (m' : Nat), m ≤ m' →
      lookupF Γ m ρ l ≠ .oof → lookupF Γ m' ρ l = lookupF Γ m ρ l
  | _, .empty, _, _, _ => by simp only [lookupF]
  | _, .sing _ _, _, _, _ => by simp only [lookupF]
  | m, .cat ρ₁ ρ₂, m', hle, h => by
      simp only [lookupF] at h ⊢
      cases h₁ : lookupF Γ m ρ₁ l with
      | oof => rw [h₁] at h; exact absurd rfl h
      | fail msg => rw [lookupF_mono m ρ₁ m' hle (by rw [h₁]; simp), h₁]
      | ok r =>
          rw [lookupF_mono m ρ₁ m' hle (by rw [h₁]; simp), h₁]
          rw [h₁] at h
          cases r with
          | found τ => rfl
          | blocked α => rfl
          | absent => exact lookupF_mono m ρ₂ m' hle h
  | 0, .var _, _, _, h => by simp [lookupF] at h
  | k + 1, .var α, m', hle, h => by
      obtain ⟨k', rfl⟩ : ∃ k', m' = k' + 1 := ⟨m' - 1, by omega⟩
      simp only [lookupF] at h ⊢
      cases hα : Γ.lookupRow α with
      | none => rfl
      | some ρ =>
          rw [hα] at h
          exact lookupF_mono k ρ k' (by omega) h
termination_by m ρ => (m, sizeOf ρ)

omit [DecidableEq B] in
theorem lookupF_stable {Γ : Ctx B} {l : Label} (ρ : Row B) :
    Stable (fun m => lookupF Γ m ρ l) :=
  fun m m' hle h => lookupF_mono m ρ m' hle h

omit [DecidableEq B] in
/-- ⊢  on an acyclic row environment, fuel past the rank suffices -/
theorem lookupF_settles_of_rank {Γ : Ctx B} {l : Label} {rank : TyVar → Nat}
    (hrank : ∀ α ρ, Γ.lookupRow α = some ρ → ρ.rankUnder rank < rank α) :
    ∀ (m : Nat) (ρ : Row B), ρ.rankUnder rank < m → lookupF Γ m ρ l ≠ .oof
  | 0, _, h => absurd h (Nat.not_lt_zero _)
  | _, .empty, _ => by simp [lookupF]
  | _, .sing l' τ, _ => by simp only [lookupF]; split <;> simp
  | k + 1, .var α, hr => by
      simp only [lookupF]
      cases hα : Γ.lookupRow α with
      | none => simp
      | some ρ =>
          have := hrank α ρ hα
          simp only [Row.rankUnder] at hr
          exact lookupF_settles_of_rank (l := l) hrank k ρ (by omega)
  | k + 1, .cat ρ₁ ρ₂, hr => by
      simp only [Row.rankUnder] at hr
      simp only [lookupF]
      have h₁ := lookupF_settles_of_rank (l := l) hrank (k + 1) ρ₁ (by omega)
      cases hl : lookupF Γ (k + 1) ρ₁ l with
      | oof => exact absurd hl h₁
      | fail msg => simp
      | ok r =>
          cases r with
          | found τ => simp
          | blocked α => simp
          | absent => exact lookupF_settles_of_rank (l := l) hrank (k + 1) ρ₂ (by omega)
termination_by m ρ => (m, sizeOf ρ)

omit [DecidableEq B] in
theorem lookupF_settles {Γ : Ctx B} {l : Label} (hwf : Γ.RowWF) (ρ : Row B) :
    Settles (fun m => lookupF Γ m ρ l) :=
  match hwf with
  | ⟨_, hrank⟩ => ⟨_, lookupF_settles_of_rank hrank _ ρ (Nat.lt_succ_self _)⟩

/-- a clean state's lookups settle -/
theorem SolverState.rowWF_of_clean {S : SolverState B} (hc : S.sol.Clean) : S.ctx.RowWF :=
  Sol.rowWF_toCtx hc.wf.acyclic

--------------------- EQUATIONS, WAKE-UP, SATURATION ---------------------------

theorem solveTyF_stable (S : SolverState B) (τ τ' : Ty B) :
    Stable (fun m => solveTyF m S τ τ') := by
  intro m m' hle h
  simp only [solveTyF] at h ⊢
  have hne : unifyTyF S.supply m (τ.applySubst S.subst) (τ'.applySubst S.subst) ≠ .outOfFuel := by
    intro he; rw [he] at h; exact h rfl
  rw [mono_ty_eq hle hne]

theorem solveTyF_settles (S : SolverState B) (τ τ' : Ty B) :
    Settles (fun m => solveTyF m S τ τ') := by
  obtain ⟨n, hn⟩ := unifyTyF_terminates S.supply (τ.applySubst S.subst) (τ'.applySubst S.subst)
  refine ⟨n, ?_⟩
  simp only [solveTyF]
  cases hu : unifyTyF S.supply n (τ.applySubst S.subst) (τ'.applySubst S.subst) with
  | outOfFuel => exact absurd hu hn
  | _ => simp

theorem wakeF_stable (S : SolverState B) (p : Parked B) : Stable (fun m => wakeF m S p) := by
  unfold wakeF
  refine Stable.bind (lookupF_stable _) (fun r => ?_)
  cases r with
  | found τ => exact Stable.bind (solveTyF_stable _ _ _) (fun _ => Stable.const _)
  | absent => exact Stable.bind (solveTyF_stable _ _ _) (fun _ => Stable.const _)
  | blocked α => exact Stable.const _

theorem wakeF_settles {S : SolverState B} (hc : S.sol.Clean) (p : Parked B) :
    Settles (fun m => wakeF m S p) := by
  unfold wakeF
  refine Settles.bind (lookupF_stable _) (lookupF_settles (SolverState.rowWF_of_clean hc) _)
    (fun r => ?_) (fun r _ _ => ?_)
  · cases r with
    | found τ => exact Stable.bind (solveTyF_stable _ _ _) (fun _ => Stable.const _)
    | absent => exact Stable.bind (solveTyF_stable _ _ _) (fun _ => Stable.const _)
    | blocked α => exact Stable.const _
  · cases r with
    | found τ =>
        exact Settles.bind (solveTyF_stable _ _ _) (solveTyF_settles _ _ _)
            (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
    | absent =>
        exact Settles.bind (solveTyF_stable _ _ _) (solveTyF_settles _ _ _)
            (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
    | blocked α => exact Settles.pure _

theorem staleF_stable (S : SolverState B) :
    ∀ ps : List (Parked B), Stable (fun m => staleF m S ps)
  | [] => by simp only [staleF]; exact Stable.const _
  | p :: ps => by
      simp only [staleF]
      refine Stable.bind (lookupF_stable _) (fun r => ?_)
      cases r with
      | blocked α =>
          by_cases he : α = p.blocker
          · simp only [he, if_true]; exact staleF_stable S ps
          · simp only [he, if_false]; exact Stable.const _
      | found τ => exact Stable.const _
      | absent => exact Stable.const _

theorem staleF_settles {S : SolverState B} (hc : S.sol.Clean) :
    ∀ ps : List (Parked B), Settles (fun m => staleF m S ps)
  | [] => by simp only [staleF]; exact ⟨0, fun h => nomatch h⟩
  | p :: ps => by
      simp only [staleF]
      refine Settles.bind (lookupF_stable _) (lookupF_settles (SolverState.rowWF_of_clean hc) _)
        (fun r => ?_) (fun r _ _ => ?_)
      · cases r with
        | blocked α =>
            by_cases he : α = p.blocker
            · simp only [he, if_true]; exact staleF_stable S ps
            · simp only [he, if_false]; exact Stable.const _
        | found τ => exact Stable.const _
        | absent => exact Stable.const _
      · cases r with
        | blocked α =>
            by_cases he : α = p.blocker
            · simp only [he, if_true]; exact staleF_settles hc ps
            · simp only [he, if_false]; exact Settles.pure _
        | found τ => exact Settles.pure _
        | absent => exact Settles.pure _

private theorem staleF_eq {S : SolverState B} {ps : List (Parked B)} {k k' : Nat}
    (hle : k ≤ k') (h : staleF k S ps ≠ .oof) : staleF k' S ps = staleF k S ps :=
  staleF_stable S ps k k' hle h

private theorem wakeF_eq {S : SolverState B} {p : Parked B} {k k' : Nat}
    (hle : k ≤ k') (h : wakeF k S p ≠ .oof) : wakeF k' S p = wakeF k S p :=
  wakeF_stable S p k k' hle h

theorem saturateF_stable : ∀ (m : Nat) (S : SolverState B) (m' : Nat), m ≤ m' →
    saturateF m S ≠ .oof → saturateF m' S = saturateF m S
  | 0, S, _, _, h => by simp [saturateF] at h
  | k + 1, S, m', hle, h => by
      obtain ⟨k', rfl⟩ : ∃ k', m' = k' + 1 := ⟨m' - 1, by omega⟩
      simp only [saturateF] at h ⊢
      cases h₁ : staleF k S S.parked with
      | oof => rw [h₁] at h; exact absurd rfl h
      | fail msg => rw [staleF_eq (k := k) (k' := k') (by omega) (by rw [h₁]; simp), h₁]; rfl
      | ok o =>
          rw [staleF_eq (k := k) (k' := k') (by omega) (by rw [h₁]; simp), h₁]
          rw [h₁] at h
          cases o with
          | none => rfl
          | some p =>
              show wakeF k' S p >>= saturateF k' = wakeF k S p >>= saturateF k
              change wakeF k S p >>= saturateF k ≠ .oof at h
              cases h₂ : wakeF k S p with
              | oof => rw [h₂] at h; exact absurd rfl h
              | fail msg => rw [wakeF_eq (k := k) (k' := k') (by omega) (by rw [h₂]; simp), h₂]; rfl
              | ok S₁ =>
                  rw [wakeF_eq (k := k) (k' := k') (by omega) (by rw [h₂]; simp), h₂]
                  rw [h₂] at h
                  exact saturateF_stable k S₁ k' (by omega) h

theorem saturateF_stable' (S : SolverState B) : Stable (fun m => saturateF m S) :=
  fun m m' hle h => saturateF_stable m S m' hle h

/-- ⊢  **saturation always reaches a verdict** from a clean state: the fuel it
needs is found by well-founded induction on `SatStep` (`satStep_wf`). -/
theorem saturateF_settles : ∀ (S : SolverState B), S.sol.Clean →
    Settles (fun m => saturateF m S) := by
  intro S
  induction S using (satStep_wf (B := B)).induction with
  | _ S ih =>
    intro hc
    obtain ⟨n₁, h₁⟩ := staleF_settles hc S.parked
    cases hs : staleF n₁ S S.parked with
    | oof => exact absurd hs h₁
    | fail msg =>
        refine ⟨n₁ + 1, ?_⟩
        simp only [saturateF, hs]; exact fun h => nomatch h
    | ok o =>
        cases o with
        | none =>
            refine ⟨n₁ + 1, ?_⟩
            simp only [saturateF, hs]; exact fun h => nomatch h
        | some p =>
            obtain ⟨hm, hnb⟩ := staleF_some hs
            obtain ⟨n₂, h₂⟩ := wakeF_settles hc p
            cases hw : wakeF n₂ S p with
            | oof => exact absurd hw h₂
            | fail msg =>
                refine ⟨max n₁ n₂ + 1, ?_⟩
                simp only [saturateF]
                rw [staleF_eq (Nat.le_max_left _ _) (by rw [hs]; simp), hs]
                show wakeF (max n₁ n₂) S p >>= saturateF (max n₁ n₂) ≠ .oof
                rw [wakeF_eq (Nat.le_max_right _ _) (by rw [hw]; simp), hw]
                exact fun h => nomatch h
            | ok S₁ =>
                have hW := wakeF_sound hw
                have hstep : SatStep S₁ S := ⟨p, hm, hnb, hW⟩
                obtain ⟨n₃, h₃⟩ := ih S₁ hstep (hW.clean hc)
                refine ⟨max (max n₁ n₂) n₃ + 1, ?_⟩
                simp only [saturateF]
                rw [staleF_eq (k := n₁) (k' := max (max n₁ n₂) n₃) (by omega) (by rw [hs]; simp), hs]
                show wakeF _ S p >>= saturateF _ ≠ .oof
                rw [wakeF_eq (k := n₂) (k' := max (max n₁ n₂) n₃) (by omega) (by rw [hw]; simp), hw]
                show saturateF _ S₁ ≠ .oof
                rw [saturateF_stable n₃ S₁ _ (by omega) h₃]; exact h₃

theorem solveTySatF_stable (S : SolverState B) (τ τ' : Ty B) :
    Stable (fun m => solveTySatF m S τ τ') := by
  unfold solveTySatF
  exact Stable.bind (solveTyF_stable _ _ _) (fun S₁ => saturateF_stable' S₁)

theorem solveTySatF_settles {S : SolverState B} (hc : S.sol.Clean) (τ τ' : Ty B) :
    Settles (fun m => solveTySatF m S τ τ') := by
  unfold solveTySatF
  exact Settles.bind (solveTyF_stable _ _ _) (solveTyF_settles _ _ _)
    (fun S₁ => saturateF_stable' S₁)
    (fun S₁ _ h => saturateF_settles S₁ ((solveTyF_sound h).clean hc))

theorem wakesF_stable : ∀ (sts : List (Stump B)) (S : SolverState B),
    Stable (fun m => wakesF m S sts)
  | [], S => by simp only [wakesF]; exact Stable.const _
  | st :: sts, S => by
      simp only [wakesF]
      refine Stable.bind (lookupF_stable _) (fun r => ?_)
      cases r with
      | blocked α => exact Stable.bind (wakesF_stable sts _) (fun _ => Stable.const _)
      | found τ =>
          exact Stable.bind (wakeF_stable _ _)
              (fun S₁ => Stable.bind (wakesF_stable sts S₁) (fun _ => Stable.const _))
      | absent =>
          exact Stable.bind (wakeF_stable _ _)
              (fun S₁ => Stable.bind (wakesF_stable sts S₁) (fun _ => Stable.const _))

theorem wakesF_settles : ∀ (sts : List (Stump B)) {S : SolverState B}, S.sol.Clean →
    Settles (fun m => wakesF m S sts)
  | [], S, _ => by simp only [wakesF]; exact ⟨0, fun h => nomatch h⟩
  | st :: sts, S, hc => by
      simp only [wakesF]
      refine Settles.bind (lookupF_stable _) (lookupF_settles (SolverState.rowWF_of_clean hc) _)
        (fun r => ?_) (fun r _ _ => ?_)
      · cases r with
        | blocked α => exact Stable.bind (wakesF_stable sts _) (fun _ => Stable.const _)
        | found τ =>
            exact Stable.bind (wakeF_stable _ _)
                (fun S₁ => Stable.bind (wakesF_stable sts S₁) (fun _ => Stable.const _))
        | absent =>
            exact Stable.bind (wakeF_stable _ _)
                (fun S₁ => Stable.bind (wakesF_stable sts S₁) (fun _ => Stable.const _))
      · cases r with
        | blocked α =>
            exact Settles.bind (wakesF_stable sts _) (wakesF_settles sts hc)
              (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
        | found τ =>
            exact Settles.bind (wakeF_stable _ _) (wakeF_settles hc _)
              (fun S₁ => Stable.bind (wakesF_stable sts S₁) (fun _ => Stable.const _))
              (fun S₁ _ h => Settles.bind (wakesF_stable sts S₁)
                (wakesF_settles sts ((wakeF_sound h).clean hc))
                (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _))
        | absent =>
            exact Settles.bind (wakeF_stable _ _) (wakeF_settles hc _)
              (fun S₁ => Stable.bind (wakesF_stable sts S₁) (fun _ => Stable.const _))
              (fun S₁ _ h => Settles.bind (wakesF_stable sts S₁)
                (wakesF_settles sts ((wakeF_sound h).clean hc))
                (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _))

--------------------- FINALIZATION ---------------------------------------------

theorem finalizeF_stable (S : SolverState B) (p : Parked B) :
    Stable (fun m => finalizeF m S p) := by
  unfold finalizeF
  by_cases hm : p ∉ S.parked
  · simp only [if_pos hm]; exact Stable.const _
  · simp only [if_neg hm]
    refine Stable.bind (lookupF_stable _) (fun r => ?_)
    cases r with
    | blocked α =>
        by_cases he : α = p.blocker
        · simp only [he, if_true]
          exact Stable.bind (Stable.withMsg (solveTyF_stable _ _ _) _) (fun _ => Stable.const _)
        · simp only [he, if_false]; exact Stable.const _
    | found τ => exact Stable.const _
    | absent => exact Stable.const _

theorem finalizeF_settles {S : SolverState B} (hc : S.sol.Clean) (p : Parked B) :
    Settles (fun m => finalizeF m S p) := by
  unfold finalizeF
  by_cases hm : p ∉ S.parked
  · simp only [if_pos hm]; exact Settles.fail _
  · simp only [if_neg hm]
    refine Settles.bind (lookupF_stable _) (lookupF_settles (SolverState.rowWF_of_clean hc) _)
      (fun r => ?_) (fun r _ _ => ?_)
    · cases r with
      | blocked α =>
          by_cases he : α = p.blocker
          · simp only [he, if_true]
            exact Stable.bind (Stable.withMsg (solveTyF_stable _ _ _) _) (fun _ => Stable.const _)
          · simp only [he, if_false]; exact Stable.const _
      | found τ => exact Stable.const _
      | absent => exact Stable.const _
    · cases r with
      | blocked α =>
          by_cases he : α = p.blocker
          · simp only [he, if_true]
            exact Settles.bind (Stable.withMsg (solveTyF_stable _ _ _) _)
              (Settles.withMsg (solveTyF_settles _ _ _) _) (fun _ => Stable.const _)
              (fun _ _ _ => Settles.pure _)
          · simp only [he, if_false]; exact Settles.fail _
      | found τ => exact Settles.fail _
      | absent => exact Settles.fail _

theorem finalizesF_stable : ∀ (ps : List (Parked B)) (S : SolverState B),
    Stable (fun m => finalizesF m S ps)
  | [], S => by simp only [finalizesF]; exact Stable.const _
  | p :: ps, S => by
      simp only [finalizesF]
      exact Stable.bind (finalizeF_stable _ _) (fun S₁ => finalizesF_stable ps S₁)

theorem finalizesF_settles : ∀ (ps : List (Parked B)) {S : SolverState B}, S.sol.Clean →
    Settles (fun m => finalizesF m S ps)
  | [], S, _ => by simp only [finalizesF]; exact ⟨0, fun h => nomatch h⟩
  | p :: ps, S, hc => by
      simp only [finalizesF]
      exact Settles.bind (finalizeF_stable _ _) (finalizeF_settles hc _)
        (fun S₁ => finalizesF_stable ps S₁)
        (fun S₁ _ h => finalizesF_settles ps ((finalizeF_sound h).clean hc))

--------------------- A-var ----------------------------------------------------

theorem varF_stable (Γ : QCtx B) (S : SolverState B) (σ : QScheme B) :
    Stable (fun m => varF m Γ S σ) := by
  unfold varF
  dsimp only
  by_cases hfr : FreshRenaming (varRen S.supply.next σ.vars) σ.vars Γ S
  · by_cases hk : ∀ α ∈ σ.vars, (S.kinds.lookup α).isSome
    · simp only [if_pos hfr, if_pos hk]
      exact Stable.bind (wakesF_stable _ _)
        (fun r => Stable.bind (saturateF_stable' _) (fun _ => Stable.const _))
    · simp only [if_pos hfr, if_neg hk]; exact Stable.const _
  · simp only [if_neg hfr]; exact Stable.const _

theorem varF_settles {Γ : QCtx B} {S : SolverState B} (hc : S.sol.Clean) (σ : QScheme B) :
    Settles (fun m => varF m Γ S σ) := by
  unfold varF
  dsimp only
  by_cases hfr : FreshRenaming (varRen S.supply.next σ.vars) σ.vars Γ S
  · by_cases hk : ∀ α ∈ σ.vars, (S.kinds.lookup α).isSome
    · simp only [if_pos hfr, if_pos hk]
      exact Settles.bind (wakesF_stable _ _) (wakesF_settles _ hc)
        (fun r => Stable.bind (saturateF_stable' _) (fun _ => Stable.const _))
        (fun r _ h => Settles.bind (saturateF_stable' _)
          (saturateF_settles _ ((wakesF_sound h).2.clean hc))
          (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _))
    · simp only [if_pos hfr, if_neg hk]; exact Settles.fail _
  · simp only [if_neg hfr]; exact Settles.fail _

--------------------- INFERENCE ------------------------------------------------

variable {C : Type}

mutual

theorem inferF_stable (constTy : C → B) :
    ∀ (Γ : QCtx B) (S : SolverState B) (e : Expr C), Stable (fun m => inferF constTy m Γ S e)
  | _, S, .con c => by simp only [inferF]; exact Stable.const _
  | Γ, S, .var x => by
      simp only [inferF]
      cases hl : Γ.lookup x with
      | none => exact Stable.const _
      | some σ => exact varF_stable Γ S σ
  | Γ, S, .lam x e => by
      simp only [inferF]
      exact Stable.bind (inferF_stable constTy _ _ e) (fun _ => Stable.const _)
  | Γ, S, .app e₁ e₂ => by
      simp only [inferF]
      exact Stable.bind (inferF_stable constTy _ _ e₁) (fun r₁ =>
        Stable.bind (inferF_stable constTy _ _ e₂) (fun r₂ =>
          Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _)))
  | Γ, S, .cat e₁ e₂ => by
      simp only [inferF]
      exact Stable.bind (inferF_stable constTy _ _ e₁) (fun r₁ =>
        Stable.bind (inferF_stable constTy _ _ e₂) (fun r₂ =>
          Stable.bind (solveTySatF_stable _ _ _) (fun _ =>
            Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _))))
  | Γ, S, .sel e l => by
      simp only [inferF]
      refine Stable.bind (inferF_stable constTy _ _ e) (fun r₁ =>
        Stable.bind (solveTySatF_stable _ _ _) (fun S₂ =>
          Stable.bind (lookupF_stable _) (fun o => ?_)))
      cases o with
      | found τ => exact Stable.const _
      | absent => exact Stable.const _
      | blocked β =>
          refine Stable.bind (lookupF_stable _) (fun o' => ?_)
          cases o' <;> exact Stable.const _
  | Γ, S, .rcd ξ => by
      simp only [inferF]
      exact Stable.bind (inferRecF_stable constTy _ _ ξ) (fun _ => Stable.const _)
  | Γ, S, .letE x e₁ e₂ => by
      simp only [inferF]
      exact Stable.bind (inferF_stable constTy _ _ e₁) (fun r₁ => inferF_stable constTy _ _ e₂)

theorem inferRecF_stable (constTy : C → B) :
    ∀ (Γ : QCtx B) (S : SolverState B) (ξ : RecBody (Expr C)),
      Stable (fun m => inferRecF constTy m Γ S ξ)
  | _, S, .empty => by simp only [inferRecF]; exact Stable.const _
  | Γ, S, .field l e => by
      simp only [inferRecF]
      exact Stable.bind (inferF_stable constTy _ _ e) (fun _ => Stable.const _)
  | Γ, S, .cat ξ₁ ξ₂ => by
      simp only [inferRecF]
      exact Stable.bind (inferRecF_stable constTy _ _ ξ₁) (fun r₁ =>
        Stable.bind (inferRecF_stable constTy _ _ ξ₂) (fun _ => Stable.const _))

end

mutual

theorem inferF_settles (constTy : C → B) :
    ∀ (Γ : QCtx B) (S : SolverState B) (e : Expr C), S.sol.Clean →
      Settles (fun m => inferF constTy m Γ S e)
  | _, S, .con c, _ => by simp only [inferF]; exact ⟨0, fun h => nomatch h⟩
  | Γ, S, .var x, hc => by
      simp only [inferF]
      cases hl : Γ.lookup x with
      | none => exact Settles.fail _
      | some σ => exact varF_settles hc σ
  | Γ, S, .lam x e, hc => by
      simp only [inferF]
      exact Settles.bind (inferF_stable constTy _ _ e) (inferF_settles constTy _ _ e hc)
        (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
  | Γ, S, .app e₁ e₂, hc => by
      simp only [inferF]
      refine Settles.bind (inferF_stable constTy _ _ e₁) (inferF_settles constTy _ _ e₁ hc)
        (fun r₁ => Stable.bind (inferF_stable constTy _ _ e₂) (fun r₂ =>
          Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _)))
        (fun r₁ _ h₁ => ?_)
      have c₁ : r₁.2.sol.Clean := Infer.clean (inferF_sound h₁) hc
      refine Settles.bind (inferF_stable constTy _ _ e₂) (inferF_settles constTy _ _ e₂ c₁)
        (fun r₂ => Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _))
        (fun r₂ _ h₂ => ?_)
      have c₂ : r₂.2.sol.Clean := Infer.clean (inferF_sound h₂) c₁
      exact Settles.bind (solveTySatF_stable _ _ _)
        (solveTySatF_settles (S := (r₂.2.draw .ty).2) c₂ _ _)
        (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
  | Γ, S, .cat e₁ e₂, hc => by
      simp only [inferF]
      refine Settles.bind (inferF_stable constTy _ _ e₁) (inferF_settles constTy _ _ e₁ hc)
        (fun r₁ => Stable.bind (inferF_stable constTy _ _ e₂) (fun r₂ =>
          Stable.bind (solveTySatF_stable _ _ _) (fun _ =>
            Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _))))
        (fun r₁ _ h₁ => ?_)
      have c₁ : r₁.2.sol.Clean := Infer.clean (inferF_sound h₁) hc
      refine Settles.bind (inferF_stable constTy _ _ e₂) (inferF_settles constTy _ _ e₂ c₁)
        (fun r₂ => Stable.bind (solveTySatF_stable _ _ _) (fun _ =>
            Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _)))
        (fun r₂ _ h₂ => ?_)
      have c₂ : r₂.2.sol.Clean := Infer.clean (inferF_sound h₂) c₁
      refine Settles.bind (solveTySatF_stable _ _ _)
        (solveTySatF_settles (S := ((r₂.2.draw .row).2.draw .row).2) c₂ _ _)
        (fun _ => Stable.bind (solveTySatF_stable _ _ _) (fun _ => Stable.const _))
        (fun S₃ _ h₃ => ?_)
      exact Settles.bind (solveTySatF_stable _ _ _)
        (solveTySatF_settles ((solveTySatF_sound h₃).clean c₂) _ _)
        (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
  | Γ, S, .sel e l, hc => by
      simp only [inferF]
      refine Settles.bind (inferF_stable constTy _ _ e) (inferF_settles constTy _ _ e hc)
        (fun r₁ => Stable.bind (solveTySatF_stable _ _ _) (fun S₂ =>
          Stable.bind (lookupF_stable _) (fun o => ?_)))
        (fun r₁ _ h₁ => ?_)
      · cases o with
        | found τ => exact Stable.const _
        | absent => exact Stable.const _
        | blocked β =>
            refine Stable.bind (lookupF_stable _) (fun o' => ?_)
            cases o' <;> exact Stable.const _
      have c₁ : r₁.2.sol.Clean := Infer.clean (inferF_sound h₁) hc
      refine Settles.bind (solveTySatF_stable _ _ _)
        (solveTySatF_settles (S := (r₁.2.draw .row).2) c₁ _ _)
        (fun S₂ => Stable.bind (lookupF_stable _) (fun o => ?_)) (fun S₂ _ h₂ => ?_)
      · cases o with
        | found τ => exact Stable.const _
        | absent => exact Stable.const _
        | blocked β =>
            refine Stable.bind (lookupF_stable _) (fun o' => ?_)
            cases o' <;> exact Stable.const _
      have c₂ : S₂.sol.Clean := (solveTySatF_sound h₂).clean c₁
      refine Settles.bind (lookupF_stable _) (lookupF_settles (SolverState.rowWF_of_clean c₂) _)
        (fun o => ?_) (fun o _ _ => ?_)
      · cases o with
        | found τ => exact Stable.const _
        | absent => exact Stable.const _
        | blocked β =>
            refine Stable.bind (lookupF_stable _) (fun o' => ?_)
            cases o' <;> exact Stable.const _
      · cases o with
        | found τ => exact Settles.pure _
        | absent => exact Settles.pure _
        | blocked β =>
            refine Settles.bind (lookupF_stable _)
              (lookupF_settles (SolverState.rowWF_of_clean c₂) _)
              (fun o' => ?_) (fun o' _ _ => ?_)
            · cases o' <;> exact Stable.const _
            · cases o' with
              | blocked α => exact Settles.pure _
              | found _ => exact Settles.fail _
              | absent => exact Settles.fail _
  | Γ, S, .rcd ξ, hc => by
      simp only [inferF]
      exact Settles.bind (inferRecF_stable constTy _ _ ξ) (inferRecF_settles constTy _ _ ξ hc)
        (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
  | Γ, S, .letE x e₁ e₂, hc => by
      simp only [inferF]
      exact Settles.bind (inferF_stable constTy _ _ e₁) (inferF_settles constTy _ _ e₁ hc)
        (fun r₁ => inferF_stable constTy _ _ e₂)
        (fun r₁ _ h₁ => by
          have c₁ : r₁.2.sol.Clean := Infer.clean (inferF_sound h₁) hc
          exact inferF_settles constTy _ _ e₂ c₁)

theorem inferRecF_settles (constTy : C → B) :
    ∀ (Γ : QCtx B) (S : SolverState B) (ξ : RecBody (Expr C)), S.sol.Clean →
      Settles (fun m => inferRecF constTy m Γ S ξ)
  | _, S, .empty, _ => by simp only [inferRecF]; exact ⟨0, fun h => nomatch h⟩
  | Γ, S, .field l e, hc => by
      simp only [inferRecF]
      exact Settles.bind (inferF_stable constTy _ _ e) (inferF_settles constTy _ _ e hc)
        (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _)
  | Γ, S, .cat ξ₁ ξ₂, hc => by
      simp only [inferRecF]
      exact Settles.bind (inferRecF_stable constTy _ _ ξ₁) (inferRecF_settles constTy _ _ ξ₁ hc)
        (fun r₁ => Stable.bind (inferRecF_stable constTy _ _ ξ₂) (fun _ => Stable.const _))
        (fun r₁ _ h₁ => Settles.bind (inferRecF_stable constTy _ _ ξ₂)
          (inferRecF_settles constTy _ _ ξ₂ (InferRec.clean (inferRecF_sound h₁) hc))
          (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _))

end

/-- ⊢  **INFERENCE TERMINATES**: from a clean state, some fuel gives a verdict. -/
theorem inferF_terminates {constTy : C → B} {Γ : QCtx B} {S : SolverState B} (e : Expr C)
    (hc : S.sol.Clean) : ∃ n, inferF constTy n Γ S e ≠ .oof :=
  inferF_settles constTy Γ S e hc

theorem runF_stable (constTy : C → B) (e : Expr C) : Stable (fun m => runF constTy m e) := by
  unfold runF
  exact Stable.bind (inferF_stable constTy _ _ e) (fun r =>
    Stable.bind (finalizesF_stable _ _) (fun _ => Stable.const _))

/-- ⊢  **A RUN TERMINATES**: every program gets a verdict at some fuel. -/
theorem runF_terminates (constTy : C → B) (e : Expr C) : ∃ n, runF constTy n e ≠ .oof := by
  unfold runF
  exact Settles.bind (inferF_stable constTy _ _ e) (inferF_settles constTy _ _ e Sol.clean_nil)
    (fun r => Stable.bind (finalizesF_stable _ _) (fun _ => Stable.const _))
    (fun r _ h => Settles.bind (finalizesF_stable _ _)
      (finalizesF_settles _ (Infer.clean (inferF_sound h) Sol.clean_nil))
      (fun _ => Stable.const _) (fun _ _ _ => Settles.pure _))

--------------------- THE TOTAL FUNCTION ---------------------------------------

/-- **inference as a total function**: the verdict at any fuel that suffices.
As with `unifyRow`, the fuel is chosen, not computed. -/
noncomputable def run (constTy : C → B) (e : Expr C) : IRes (Ty B × SolverState B) :=
  runF constTy (Classical.choose (runF_terminates constTy e)) e

theorem run_ne_oof (constTy : C → B) (e : Expr C) : run constTy e ≠ .oof :=
  Classical.choose_spec (runF_terminates constTy e)

/-- ⊢  every fuelled run that answers, answers `run` -/
theorem runF_eq_run {constTy : C → B} {e : Expr C} {n : Nat} (h : runF constTy n e ≠ .oof) :
    runF constTy n e = run constTy e := by
  unfold run
  have hc := Classical.choose_spec (runF_terminates constTy e)
  rcases Nat.le_total n (Classical.choose (runF_terminates constTy e)) with hle | hle
  · exact (runF_stable constTy e _ _ hle h).symm
  · exact runF_stable constTy e _ _ hle hc

/-- ⊢  **`run` is sound**: what it answers types declaratively. -/
theorem run_typed {constTy : C → B} {e : Expr C} {τ : Ty B} {S' : SolverState B}
    (h : run constTy e = .ok (τ, S')) :
    QTyped constTy ⟨[], []⟩ e (τ.applySubst S'.subst) :=
  runF_typed h

end MinimalCalculus
