-- WHAT IS LEFT AFTER `RunSound`: TERMINATION AND PRINCIPALITY.
--
-- Soundness is closed (`runSound`, Finalization.lean). This module holds the
-- two remaining headline properties, as far as they go:
--
--   1. SATURATION TERMINATES — PROVED. The `↝*` closure has no infinite run
--      from any state (`satStep_wf`): the measure is (|Δ|, #unblocked stumps),
--      lexicographically. K-hit and K-⊥ retire a stump; K-repark keeps |Δ| but
--      trades an unblocked stump for a blocked one.
--
--   2. INFERENCE TERMINATES — PROVED, about a real function: `inferF`/`runF`
--      (InferFn.lean) make every choice §2 lists, every answer is a derivation
--      (`inferF_sound`, `runF_typed`), and some fuel always gives a verdict that
--      more fuel does not change (`runF_terminates`, `runF_eq_run`,
--      InferFnTerm.lean). §2 keeps the account of what had to be decided.
--
--   3. GENERAL PRINCIPALITY — STATED (`GeneralPrincipality`), not proved.

import Finalization

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- 1. SATURATION TERMINATES ---------------------------------

/-- a parked stump is still blocked on the blocker it records -/
def Parked.BlockedAt (S : SolverState B) (p : Parked B) : Prop :=
  LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label p.blocker

/-- one `Saturate.step`: wake a stump whose blocker was solved. Written
`SatStep S' S` — the successor first — so that well-foundedness reads as "no
infinite run". -/
def SatStep (S' S : SolverState B) : Prop :=
  ∃ p ∈ S.parked, ¬ p.BlockedAt S ∧ Wake S p S'

open Classical in
/-- the second component of the measure -/
noncomputable def SolverState.unblocked (S : SolverState B) : Nat :=
  S.parked.countP (fun p => ¬ p.BlockedAt S)

private theorem filter_res_lt {p : Parked B} {l : List (Parked B)} (hp : p ∈ l) :
    (l.filter (·.stump.res != p.stump.res)).length < l.length :=
  List.length_filter_lt_length_iff_exists.mpr ⟨p, hp, by simp⟩

private theorem countP_filter_lt {p : Parked B} {l : List (Parked B)}
    (q : Parked B → Bool) (hp : p ∈ l) (hq : q p = true) :
    (l.filter (·.stump.res != p.stump.res)).countP q < l.countP q := by
  induction l with
  | nil => exact absurd hp List.not_mem_nil
  | cons a l ih =>
      have hle : (l.filter (·.stump.res != p.stump.res)).countP q
          ≤ l.countP q := by
        rw [List.countP_filter]
        exact List.countP_mono_left (fun x _ h => by simp at h ⊢; exact h.1)
      rcases List.mem_cons.mp hp with rfl | hp'
      · simp [List.filter_cons, List.countP_cons, hq]; omega
      · have := ih hp'
        by_cases ha : a.stump.res = p.stump.res
        · simp only [List.filter_cons, ha, bne_self_eq_false, Bool.false_eq_true,
            if_false, List.countP_cons]
          omega
        · simp only [List.filter_cons, bne_iff_ne, ne_eq, ha, not_false_eq_true,
            if_true, List.countP_cons]
          omega

/-- ⊢  **one step decreases (|Δ|, #unblocked), lexicographically.** -/
theorem SatStep.decreases {S S' : SolverState B} (h : SatStep S' S) :
    S'.parked.length < S.parked.length ∨
      (S'.parked.length ≤ S.parked.length ∧ S'.unblocked < S.unblocked) := by
  obtain ⟨p, hp, hnb, hw⟩ := h
  cases hw with
  | hit _ hs =>
      obtain ⟨_, _, _, _, rfl⟩ := hs
      exact .inl (filter_res_lt hp)
  | abs _ hs =>
      obtain ⟨_, _, _, _, rfl⟩ := hs
      exact .inl (filter_res_lt hp)
  | repark hb =>
      rename_i α'
      refine .inr ⟨filter_res_lt hp, ?_⟩
      -- the new entry is blocked at the SAME state reading: nothing was solved
      have hnew : Parked.BlockedAt
          (({ S with parked := S.parked.filter (·.stump.res != p.stump.res) }).park
            ⟨α', p.stump⟩) ⟨α', p.stump⟩ := hb
      classical
      simp only [SolverState.unblocked]
      show List.countP _ ((⟨α', p.stump⟩ : Parked B) :: _) < _
      rw [List.countP_cons]
      simp only [hnew, not_true_eq_false, decide_false, Bool.false_eq_true, if_false,
        Nat.add_zero]
      have heq : (fun x : Parked B => decide (¬ x.BlockedAt
          (({ S with parked := S.parked.filter (·.stump.res != p.stump.res) }).park
            ⟨α', p.stump⟩)))
          = (fun x : Parked B => decide (¬ x.BlockedAt S)) :=
        funext fun x => decide_eq_decide.mpr Iff.rfl
      rw [heq]
      exact countP_filter_lt _ hp (by simpa using hnb)

/-- ⊢  **SATURATION TERMINATES**: there is no infinite `↝*` run, from any state
at all — no cleanliness, quiescence or well-formedness needed. Together with
`Saturate`'s own rules this says a saturation run either reaches a quiescent
state or gets stuck on an equation that does not solve (a clash), in finitely
many steps. -/
theorem satStep_wf : WellFounded (SatStep (B := B)) := by
  have hlex : WellFounded (fun (a b : Nat × Nat) =>
      a.1 < b.1 ∨ (a.1 ≤ b.1 ∧ a.2 < b.2)) := by
    refine Subrelation.wf (r := (Prod.lex Nat.lt_wfRel Nat.lt_wfRel).rel) ?_
      (Prod.lex Nat.lt_wfRel Nat.lt_wfRel).wf
    intro a b h
    rcases a with ⟨a₁, a₂⟩; rcases b with ⟨b₁, b₂⟩
    rcases h with h | ⟨h₁, h₂⟩
    · exact Prod.Lex.left _ _ h
    · rcases Nat.lt_or_eq_of_le h₁ with h₁ | rfl
      · exact Prod.Lex.left _ _ h₁
      · exact Prod.Lex.right _ h₂
  exact Subrelation.wf (fun h => h.decreases)
    (InvImage.wf (fun S : SolverState B => (S.parked.length, S.unblocked)) hlex)

--------------------- 2. INFERENCE TERMINATION: WHAT A FUNCTION HAD TO DECIDE --
-- `Infer` is a relation; every choice a function would have to make is a
-- premise. "Inference terminates" is therefore a statement about a FUNCTION,
-- plus `infer = some (τ, S′) → Infer … τ S′` — anything of the form "∃ f, …" is
-- classically trivial. That function is now `inferF` (InferFn.lean). What it
-- had to decide, rule by rule:
--
--   * the equations — `unifyTyF` at enough fuel: TERMINATES
--     (`unifyTyM_terminates`, RowUnify/Termination.lean);
--   * the `↝*` closure — `Saturate`: TERMINATES (`satStep_wf` above), and each
--     step's premise is a lookup, which is total (`lookup_total`);
--   * A-var's fresh names — `Sup` is ANY supply past the renaming; a function
--     takes the least one. No search;
--   * A-let's generalization — ᾱ and the split Δ₁ ~ Δq ++ Δγ. SETTLED
--     (`LetChoice.lean`): admissible ᾱ are closed under union
--     (`LetAdmissible.union`), so a GREATEST one exists, and `greatestAlpha`
--     computes it by deleting forced-out variables (`greatestAlpha_spec`). The
--     worry that correctability breaks union was unfounded: a stump generalized
--     under ᾱ₁ but not ᾱ₂ sits in Δγ₂, which may not mention ᾱ₂. So A-let costs
--     the algorithm no principality in its CHOICE; its premises still cost
--     completeness against the calculus.
--
-- FINDING, while writing this down: `Infer.letE` asked `S₁.parked = Δq ++ Δγ` —
-- a PREFIX split of the parked list, not a partition. FIXED: the premise is now
-- `S₁.parked.Perm (Δq ++ Δγ)`; every soundness lemma read Δ₁ through membership
-- only and absorbed it unchanged.

--------------------- 3. GENERAL PRINCIPALITY ----------------------------------

/-- **General principality** — every typeable program has a principal qualified
scheme, in the order `selQ_principal` establishes for `λx.x.l`
(`QScheme.Principal`: sound, inhabited, and covering every typing up to ≼ₜ).

Named, not proved. What it needs:
  * a CANDIDATE for every program — the natural one is the run's own output,
    generalized: ∀ ftv(τ). Δ ⇒ τ read at ⟦S′⟧ BEFORE finalization, since F-★
    commits choices a principal scheme must leave open;
  * soundness of the candidate — essentially `inferSound`, which is proved;
  * COVERING, which is algorithmic COMPLETENESS, and that is known false in
    three ways: the spent promise (`spentEx_declarative` types a program no run
    finalizes), the stuck verdict U-expand's removal left without a rule, and
    A-let's premises (§2 and proof-state.md). So this can only hold for a
    fragment, or for a calculus whose L2 rules are narrowed to match. Which
    fragment is the thesis's decision, not the proof's. -/
def GeneralPrincipality (B C : Type) (constTy : C → B) : Prop :=
  ∀ (Γ : QCtx B) (e : Expr C), (∃ τ, QTyped constTy Γ e τ) →
    ∃ σ : QScheme B, QScheme.Principal constTy Γ e σ

end MinimalCalculus
