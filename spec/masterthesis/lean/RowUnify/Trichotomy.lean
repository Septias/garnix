-- P6: the mgu statement and the stuck leg, plus the NEXT roadmap.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Clash

namespace MinimalCalculus

-- ## The mgu statement, in one place
-- Soundness and completeness together: a success DESCRIBES the unifier set.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = success s _   ⟹
--      every θ meeting s unifies ρ₁, ρ₂,  and every unifier of ρ₁, ρ₂ extends
--      (without moving on the problem's own variables) to one meeting s
theorem unifyRowM_success_iff {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    (∀ θ : TySubst B, Sol.Sat θ s → Unifies θ ρ₁ ρ₂) ∧
    (∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ → ∃ θ' : TySubst B,
        AgreeOn θ θ' (sFtv ρ₁.toSpine ++ sFtv ρ₂.toSpine) ∧ Sol.Sat θ' s) :=
  ⟨fun _ hsat => unifyRowM_success_sound h hsat,
   fun _ hu => unifyRowM_success_complete h hu⟩


------------------ P6: THE BASE-ARM DISPATCH, STEP 2 ------------------------
-- THIS SECTION IS GONE (Stage 1b, plans/drop-expand.md). It held
-- `uniqueHost_none`, `expandL_none_field`, `expandR_none_field`,
-- `stuck_leading_shape_expand`, `stuck_field_vs_var` and the two list helpers
-- they used.
--
-- Step 2 refined the four leading shapes of `stuck_leading_shape` (Reflection)
-- by what U-EXPAND'S REFUSAL added: a leading field facing the other side meant
-- either ≥ 2 candidate hosts (Wand's shape) or the label already present behind
-- a variable. That reading was only ever available because the driver had tried
-- to host the field and declined. With no expansion arm the driver never asks
-- the question, so there is nothing to read off, and `NoHost` — the predicate
-- the whole section was phrased in — no longer exists.
--
-- Nothing is lost that was load-bearing: the fourth leg is not a converse (see
-- THE FOURTH LEG below), so step 2 was feeding a reduction that is itself
-- refuted. What survives is step 1, `stuck_leading_shape`, which is a fact about
-- the SHAPES the surviving arms leave behind, and the specific no-mgu theorems
-- in NoMgu.lean, which never went through the dispatch.




------------------ P6: THE FOURTH LEG ---------------------------------------
-- WHAT THIS LEG IS NOT. There used to be a `unifyM_stuck_no_mgu` here: an
-- induction on fuel reducing `.stuck ⟹ ¬HasMgu` to four hypotheses (hbase,
-- hexp, hsolve, hsolveTy), each threading an accumulated predicate `Q`. It is
-- deleted, because the theorem it was reducing is FALSE:
--
--   * `Refutations.stuck_masks_mgu` — the driver answers `.stuck` on
--     (k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫), which has the unique mgu
--     β ≔ (l:𝓫), α ≔ ε. `UResM.seq` propagates the ambiguous sub-equation
--     {β|α} ≐ {l:𝓫} before the residual β ≐ᵣ (l:𝓫), which pins β, is looked
--     at. So `.stuck` is a CONSERVATIVE verdict, exactly like `.occurs`.
--   * `Refutations.hbase_shape_false` / `hbase_stableQ_false` — the four
--     hypotheses were also false in their own right, an unconstrained `Q`
--     conjunct being able to shrink a unifier set down to one with an mgu.
--     That is the shadow of the first point: the `Q`-threading assumed a stuck
--     conjunct makes the conjunction ambiguous.
--
-- NOR IS IT the retreat to TERMINAL configurations. `TerminalNoMgu` (Defs.lean)
-- is REFUTED, on (l:{w}) ≐ᵣ (w | v): one of the two candidate placements is
-- ruled out by an occurs violation the guards cannot see, so the unifier is
-- unique — hence most general — while every move is dead. The refutation was
-- briefly suspended while U-expand had a right-end arm that solved that
-- configuration; with the arms gone it is terminal again and
-- `Refutations.terminalNoMgu_false` states it. Both candidate converses are
-- therefore settled NEGATIVE: neither `.stuck` nor terminality implies no-mgu.
--
-- WHAT THE LEG ACTUALLY IS, then: the SPECIFIC no-mgu theorems (NoMgu.lean —
-- vars_vs_field_no_mgu for Wand, two_sided_no_mgu, allvar_swap_no_mgu, each
-- also at the `On` level), together with the conservativity examples. There is
-- no general converse to prove. The dispatch below stays useful as the case
-- analysis a side-condition-guarded version would still go through.
--
-- The lemmas kept below (which arms can answer `.stuck` at all, and ≐'s
-- congruence iffs) are facts about the driver in their own right, and are what
-- a future algorithm-level statement would still be built from.

-- ## ≐'s congruence arms are pointwise iffs (so they need no hypothesis)
theorem tyUnifies_fn_iff {B : Type} (θ : TySubst B) (a₁ b₁ a₂ b₂ : Ty B) :
    TyUnifies θ (.fn a₁ b₁) (.fn a₂ b₂) ↔ (TyUnifies θ a₁ a₂ ∧ TyUnifies θ b₁ b₂) := by
  constructor
  · intro h
    obtain ⟨σ₁, σ₂, heq, hA, hB⟩ := TyEquiv.fn_inv
      (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from h)
    simp only [Ty.applySubst, Ty.fn.injEq] at heq
    obtain ⟨rfl, rfl⟩ := heq
    exact ⟨hA, hB⟩
  · exact fun ⟨hA, hB⟩ => TyEquiv.fn hA hB

theorem tyUnifies_rcd_iff {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) :
    TyUnifies θ (.rcd ρ₁) (.rcd ρ₂) ↔ Unifies θ ρ₁ ρ₂ := by
  constructor
  · intro h
    obtain ⟨ρ', heq, hR⟩ :=
      TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from h)
    simp only [Ty.applySubst, Ty.rcd.injEq] at heq
    obtain rfl := heq
    exact hR
  · exact fun h => TyEquiv.rcd h

theorem unifies_toSpine_iff {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) :
    Unifies θ (ofSpine ρ₁.toSpine) (ofSpine ρ₂.toSpine) ↔ Unifies θ ρ₁ ρ₂ := by
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact ⟨fun h => e₁.trans (h.trans e₂.symm), fun h => e₁.symm.trans (h.trans e₂)⟩

-- ## Which arms can answer `stuck` at all
theorem bindTy_ne_stuck {B : Type} {S : Supply} {α : TyVar} {τ : Ty B} :
    bindTy S α τ ≠ .stuck := by
  intro h; unfold bindTy at h
  split at h
  · cases h
  · split at h <;> cases h

theorem solveVarM_ne_stuck {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)} :
    solveVarM S s₁ s₂ ≠ some .stuck := by
  intro h
  cases s₁ with
  | nil => simp [solveVarM] at h
  | cons a r =>
    cases a with
    | field _ _ => simp [solveVarM] at h
    | var α =>
      cases r with
      | cons _ _ => simp [solveVarM] at h
      | nil =>
        simp only [solveVarM] at h
        split at h
        · simp at h
        · split at h <;> simp at h

-- (`expandResM_stuck` LIVED HERE — "the expansion wrapper is stuck only if its
-- residual was". Its wrapper went with the arms.)

theorem UResM.seq_stuck {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (h : r.seq k = .stuck) :
    r = .stuck ∨ ∃ s S, r = .success s S ∧ k s.toSubst S = .stuck := by
  cases r with
  | success s S =>
      refine .inr ⟨s, S, rfl, ?_⟩
      simp only [UResM.seq] at h
      revert h; cases hk : k s.toSubst S with
      | success s' S' => intro h; cases h
      | clash => intro h; cases h
      | occurs => intro h; cases h
      | stuck => intro _; rfl
      | outOfFuel => intro h; cases h
  | clash => cases h
  | occurs => cases h
  | stuck => exact .inl rfl
  | outOfFuel => cases h

-- ⊢  the dispatch, packaged from a `Terminal` record
--
-- RESTATED at Stage 1b. It used to carry a `NoHost` refinement on each
-- field-bearing shape, read off U-expand's refusal; with no expansion arm the
-- four leading shapes are all a terminal configuration says, and the lemma is
-- exactly `stuck_leading_shape` fed from the record. `Terminal` is a weaker
-- hypothesis than it was (four fewer fields) and this is a weaker conclusion,
-- but the pair still lines up: only the LEADING-end fields ever fed it.
--
-- It is no longer "the entry point Phase B proves `TerminalNoMgu` through" —
-- `TerminalNoMgu` is refuted (see above). What it remains is the case analysis
-- any side-condition-guarded statement about terminal configurations starts
-- from, and the place the specific no-mgu witnesses get dispatched.
theorem terminal_leading_shape {B : Type} {S : Supply} {a b : Atom B}
    {s₁ s₂ : List (Atom B)} (ht : Terminal S (a :: s₁) (b :: s₂)) :
    (∃ α β, a = .var α ∧ b = .var β ∧ α ≠ β) ∨
    (∃ α l' τ', a = .var α ∧ b = .field l' τ') ∨
    (∃ l τ β, a = .field l τ ∧ b = .var β) ∨
    (∃ l τ l' τ', a = .field l τ ∧ b = .field l' τ' ∧ l ≠ l' ∧
      windowExtract l (b :: s₂) = none ∧ windowExtract l' (a :: s₁) = none) :=
  stuck_leading_shape ht.hstripL ht.hmatchL₁ ht.hmatchL₂




------------------------------------ NEXT ------------------------------------
-- WHERE ≐ / ≐ᵣ STANDS (typesystems/proof-state.md is the live ledger;
-- plans/drop-expand.md is this branch's plan).
--
-- The algorithm is `unifyTyF` / `unifySpineMF` — one mutual block, structurally
-- recursive on fuel, so every worked example is a kernel-checked `rfl`
-- (Regressions.lean). Five verdicts: success / clash / occurs / stuck /
-- outOfFuel. Since Stage 1b every arm SOLVES AND APPLIES: no arm invents a
-- variable, no solver state accumulates, and there is no `DepGraph`.
--
-- Three of the four legs are THEOREMS, at any fuel: unifyM_success_sound,
-- unifyM_success_complete (∃θ′/AgreeOn form; with soundness, the solution
-- DESCRIBES the unifier set — unifyRowM_success_iff), and
-- unifyM_clash_no_unifier. Supporting invariants: unifyM_fuel_mono and
-- unifyM_bounded.
--
-- The fourth leg is NOT a converse. Both candidates are refuted:
-- `.stuck ⟹ ¬HasMgu` by `Refutations.stuck_masks_mgu`, and the retreat to
-- terminal configurations by `Refutations.terminalNoMgu_false`. What stands in
-- its place is the specific no-mgu theorems of NoMgu.lean together with the
-- conservativity witnesses that show why no more is available.
--
-- OPEN, in dependency order:
--  * `unifyRowM … = .occurs ⟹ ¬∃θ`, the driver-level lift. Stage 1b UNBLOCKED
--    this: the occurs guard used to read `depReach Θ`, so it could fire on an α
--    merely reachable from s₂ through the accumulated expansions — a fact about
--    the solver state, not the problem. The guard is local again and
--    `solveVarM_occurs_no_unifier` is unconditional, so what is left is the
--    induction over the driver, not a missing side condition.
--  * `Sol.Applied` as a driver invariant. Every arm applies its solution, so no
--    solution mentions a variable in its own domain; the fuzz sweep reads 0
--    failures in all three universes. With it, `Sol.Ranked`, `Sol.WF` and the
--    whole refuted rank search are unnecessary — `Applied` is strictly stronger
--    and `Sol.closes_toSubst_of_applied` hands over ⟦S⟧ = toSubst directly.
--  * `HasMguOn V` / `InstanceOfOn V` — mgu RELATIVIZED to a variable set. Less
--    urgent than it was: the mismatch it repaired was the algorithm inventing
--    variables, which no arm now does.
--  * TERMINATION: a fuel that provably suffices. The Rémy-measure obstruction
--    went with the arm (nothing re-expands a variable any more), but the
--    lexicographic `(|unsolved vars|, |spine|)` candidate is unproved and
--    `sApplySubst` can still grow a spine — Stage 0 does not evidence it.
--
-- MILESTONES ELSEWHERE THAT BUILD ON THIS FILE (algorithmic.typ, Open questions):
--  * Non-vacuity of qualified schemes: needs lookup_total (RowWF) plus a
--    freshness discipline for the result variables δ — P2/P5 now supply that
--    discipline (Supply/Avoids, SolBelow, AgreeOn, the substitution-ftv toolkit).
--  * STRICTNESS of the QTyped extension, and type safety for QTyped itself.
--  * The covering order ⊴ on qualified schemes (needed to STATE "the principal
--    type improves under reduction").
--  * Solver state S = (θ, Δ, W), stump wake-up, and the confluence argument that
--    the final state is independent of wake-up scheduling
--    (lookup_det + Discharge.mono_of_definite are the two pillars).


end MinimalCalculus
