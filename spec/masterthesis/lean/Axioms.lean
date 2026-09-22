-- AXIOM GUARD. `#guard_msgs` pins the exact axiom dependencies of the headline
-- theorems: if a `sorry` (sorryAx) or any unexpected axiom ever creeps into a
-- proof these theorems rest on, the printed axiom list changes and this file
-- FAILS to build. Row-unification results are propext/Quot.sound only; the L2
-- qualified-scheme results additionally use Classical.choice (from minimal).
-- Update an expected message here only when the change is understood and intended.

import Qualified
import RowUnify
import Refutations
import QSubst
import Infer
import InferSound

namespace MinimalCalculus

-- ## ≈-characterization (RowEquiv)
/-- info: 'MinimalCalculus.rowEquiv_iff_char' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms rowEquiv_iff_char

-- ## ≐ᵣ / ≐ trichotomy (RowUnify)
-- The four legs are guarded in the P5/P6 blocks below. What stays here are the
-- two local no-unifier cores the clash leg rests on.
/-- info: 'MinimalCalculus.projClash_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms projClash_no_unifier

/-- info: 'MinimalCalculus.stuck_not_both_ground' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms stuck_not_both_ground

-- The all-variable occurrence, which the occurs guard used to reject: not a
-- give-up any more but the CORRECTNESS PROOF of the ε-collapse rule. The
-- three-atom witness (Classical.choice matches its sibling no-mgu/mgu theorems,
-- allvar_swap) and the general rule it became.
/--
info: 'MinimalCalculus.occurs_allVar_hasMgu' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms occurs_allVar_hasMgu

/-- info: 'MinimalCalculus.allvar_occurs_mgu' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms allvar_occurs_mgu

/-- info: 'MinimalCalculus.collapseSol_reflect' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms collapseSol_reflect

/-- info: 'MinimalCalculus.collapseSol_complete' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms collapseSol_complete

-- THE OCCURS VERDICT, SOUND WHERE IT IS LOCAL. The converse-shaped direction —
-- the only one available anywhere in this development. Both sources: the type
-- sort (constructor depth) and the row sort (where the case analysis closes).
/-- info: 'MinimalCalculus.ty_occurs_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms ty_occurs_no_unifier

/-- info: 'MinimalCalculus.bindTy_occurs_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms bindTy_occurs_no_unifier

/--
info: 'MinimalCalculus.solveVarM_occurs_no_unifier' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms solveVarM_occurs_no_unifier

/--
info: 'MinimalCalculus.solveVarM_occurs_no_unifier_nil' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms solveVarM_occurs_no_unifier_nil

-- ## P1: mutual ≐/≐ᵣ scaffolding
-- The ≗-congruence is the new load-bearing theory; it is axiom-FREE, and the
-- bridge lemmas built on it stay propext-only.
/-- info: 'MinimalCalculus.Row.applySubst_substEquiv' does not depend on any axioms -/
#guard_msgs in #print axioms Row.applySubst_substEquiv

/-- info: 'MinimalCalculus.unifies_applySubst_iff' depends on axioms: [propext] -/
#guard_msgs in #print axioms unifies_applySubst_iff

/-- info: 'MinimalCalculus.unifies_applySubst_of_sat' depends on axioms: [propext] -/
#guard_msgs in #print axioms unifies_applySubst_of_sat

/-- info: 'MinimalCalculus.unifies_sApplySubst_of_sat' depends on axioms: [propext] -/
#guard_msgs in #print axioms unifies_sApplySubst_of_sat

/-- info: 'MinimalCalculus.Sol.Sat.comp_inv' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Sol.Sat.comp_inv

-- ## P2: the fresh-variable supply
-- Classical.choice reaches these through minimal.lean's natName/lenBound
-- toolkit; the freshness content itself adds nothing.
/--
info: 'MinimalCalculus.Supply.fresh_not_mem' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Supply.fresh_not_mem

/--
info: 'MinimalCalculus.Supply.unifies_setRow_fresh' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Supply.unifies_setRow_fresh

/-- info: 'MinimalCalculus.unifies_setRow_of_not_mem' depends on axioms: [propext] -/
#guard_msgs in #print axioms unifies_setRow_of_not_mem

/-- info: 'MinimalCalculus.groundMatch_ftv' depends on axioms: [propext] -/
#guard_msgs in #print axioms groundMatch_ftv

-- ## P3: the unique-host ARGUMENT (the arm itself is gone)
-- host_forced mechanizes the maximality argument proof-state.md carries by hand
-- for crossfield. These are facts about the CALCULUS — what any unifier of a
-- crossfield problem must look like — and they stay true now that no arm
-- exploits them. `expand_reflect` / `expand_reflect_fwd`, which were the
-- soundness and completeness directions of the MOVE, went with it
-- (plans/drop-expand.md).
/-- info: 'MinimalCalculus.host_forced' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms host_forced

/-- info: 'MinimalCalculus.crossfield_host_forced' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms crossfield_host_forced

-- ## P4: the mutual ≐ / ≐ᵣ driver
-- The fuel lemma replaces a termination measure: `outOfFuel` is its own
-- verdict, so "more budget never changes a verdict that was reached" is a plain
-- structural induction and stays propext/Quot.sound. unifyRowM_fuel_mono picks
-- up Classical.choice only through localSupply's lenBound, like its ≐ᵣ siblings.
/-- info: 'MinimalCalculus.UResM.Mono.seq' depends on axioms: [propext] -/
#guard_msgs in #print axioms UResM.Mono.seq

/-- info: 'MinimalCalculus.unifyM_fuel_mono' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyM_fuel_mono

/-- info: 'MinimalCalculus.unifySpineMF_fuel_mono' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifySpineMF_fuel_mono

/-- info: 'MinimalCalculus.unifyTyF_fuel_mono' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyTyF_fuel_mono

/--
info: 'MinimalCalculus.unifyRowM_fuel_mono' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyRowM_fuel_mono

-- ## P5: the three forward legs, on the MUTUAL driver
-- Soundness needs no freshness, so it stays propext/Quot.sound; the other two
-- reach Classical.choice through the Supply's lenBound, as the ≐ᵣ originals do.
/-- info: 'MinimalCalculus.unifyM_success_sound' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyM_success_sound

/--
info: 'MinimalCalculus.unifyRowM_success_sound' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyRowM_success_sound

/--
info: 'MinimalCalculus.unifyTyM_success_sound' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyTyM_success_sound

-- The freshness invariant solve-and-apply forced: a run only mentions
-- names below the supply it returns.
/--
info: 'MinimalCalculus.unifyM_bounded' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyM_bounded

/-- info: 'MinimalCalculus.Ty.ftv_applySubst' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Ty.ftv_applySubst

/-- info: 'MinimalCalculus.Sol.Sat.comp' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Sol.Sat.comp

/--
info: 'MinimalCalculus.unifyRowM_success_complete' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyRowM_success_complete

/--
info: 'MinimalCalculus.unifyRowM_clash_no_unifier' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyRowM_clash_no_unifier

-- ## P6: step 2 of the base-arm dispatch
-- What U-expand's REFUSAL contributes to the terminal stuck configuration: ≥ 2
-- candidate hosts, or the label already present behind a variable. Pure
-- structure, so no Classical.choice.
/-- info: 'MinimalCalculus.uniqueHost_none' depends on axioms: [propext] -/
#guard_msgs in #print axioms uniqueHost_none

/-- info: 'MinimalCalculus.stuck_leading_shape_expand' depends on axioms: [propext] -/
#guard_msgs in #print axioms stuck_leading_shape_expand

/-- info: 'MinimalCalculus.stuck_field_vs_var' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms stuck_field_vs_var

/--
info: 'MinimalCalculus.unifyRowM_success_iff' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyRowM_success_iff

-- The fourth leg is NOT guarded here: there is nothing to guard yet. The old
-- reduction `unifyM_stuck_no_mgu` was deleted (its hypotheses are false —
-- see the Refutations block below), and its replacement `TerminalNoMgu` is a
-- STATEMENT, not yet a theorem. What is guarded is the dispatch it will be
-- proved through.
/-- info: 'MinimalCalculus.terminal_leading_shape' depends on axioms: [propext] -/
#guard_msgs in #print axioms terminal_leading_shape

-- ## P6 / Phase A: mgu RELATIVIZED to a variable set
-- `¬ HasMguOn V` is the stronger statement (factoring on V only is an easier
-- demand than factoring everywhere), so it hands back the thesis-facing
-- `¬ HasMgu` for free — and unlike the strict form it is insensitive to the
-- variables the algorithm invents. The relativization itself is axiom-free.
/-- info: 'MinimalCalculus.not_hasMgu_of_not_hasMguOn' does not depend on any axioms -/
#guard_msgs in #print axioms not_hasMgu_of_not_hasMguOn

/-- info: 'MinimalCalculus.hasMguOn_congr' does not depend on any axioms -/
#guard_msgs in #print axioms hasMguOn_congr

-- The two count bounds an mgu obeys, relativized: an mgu is pointwise minimal
-- in every label count (mono) and rigid where its image is var-free.
/-- info: 'MinimalCalculus.instanceOfOn_fieldCount_mono' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms instanceOfOn_fieldCount_mono

/--
info: 'MinimalCalculus.instanceOfOn_fieldCount_eq_of_varFree' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in #print axioms instanceOfOn_fieldCount_eq_of_varFree

-- The three base no-mgu techniques at the `On` level: count-shrink
-- (field vs ≥2 variable hosts), rigidity (two-sided), non-commutativity
-- (all-variable). These are what Phase B has to run at the general shape.
/--
info: 'MinimalCalculus.vars_vs_field_no_mgu_on' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in #print axioms vars_vs_field_no_mgu_on

/--
info: 'MinimalCalculus.two_sided_no_mgu_on' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms two_sided_no_mgu_on

/--
info: 'MinimalCalculus.allvar_swap_no_mgu_on' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms allvar_swap_no_mgu_on

-- ## P6: why that reduction cannot be discharged as stated (Refutations)
-- The parked hypotheses quantify over an unconstrained `Q`, and conjoining a
-- predicate can SHRINK a unifier set to one that has an mgu. Both refutations
-- run on the Wand configuration, whose thirteen terminal-move premises hold by
-- `rfl`. If either of these ever stops holding, the stuck leg has been
-- restated — which is the point of Phase A in proof-plan.md.
/--
info: 'MinimalCalculus.hbase_shape_false' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms hbase_shape_false

-- …and restricting `Q` to the substitution-stable shape an emitted type
-- equation really has does not save it either.
/--
info: 'MinimalCalculus.hbase_stableQ_false' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms hbase_stableQ_false

-- THE SHARP ONE. `.stuck` is a CONSERVATIVE verdict, like `.occurs`: the driver
-- propagates an ambiguous sub-equation before it ever looks at the residual
-- that would disambiguate the problem. So "stuck ⟹ no mgu" is false outright,
-- not merely misstated, and the honest fourth leg is about TERMINAL
-- configurations. The pair below is the counterexample: the algorithm RUNS to
-- `.stuck` (a kernel-checked rfl), and the problem has an mgu.
/--
info: 'MinimalCalculus.stuck_masks_mgu_reported' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms stuck_masks_mgu_reported

/--
info: 'MinimalCalculus.stuck_masks_mgu' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms stuck_masks_mgu

-- AND THE SHARPEST ONE, WHICH NO LONGER HOLDS. Retreating from the `.stuck`
-- VERDICT to TERMINAL CONFIGURATIONS used to fail too: (l:{w}) ≐ᵣ (w | v) was
-- terminal and has a UNIQUE unifier, because hosting the field in w would force
-- θw ≈ (l:{θw}). Field counts are blind to that — the recursion passes under a
-- record constructor — so the guards missed it; `rcdDepth` is the ≈-invariant
-- that sees it.
--
-- The configuration was terminal only because U-expand was ONE-ENDED. With the
-- right-end arm it is not terminal any more and the driver solves it, so
-- `terminalNoMgu_false` is GONE and `TerminalNoMgu` is open — unrefuted and
-- unproven. What is guarded now is that pair of facts.
/-- info: 'MinimalCalculus.terminal_masks_mgu_not_terminal' depends on axioms: [propext] -/
#guard_msgs in #print axioms terminal_masks_mgu_not_terminal

-- … the driver's verdict on it, back to `.stuck` now that the arm is gone
/--
info: 'MinimalCalculus.terminal_masks_mgu_stuck' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms terminal_masks_mgu_stuck

-- … and the hand-built mgu, which is a CONSERVATIVITY witness again
/--
info: 'MinimalCalculus.terminal_masks_mgu' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms terminal_masks_mgu

-- The tool behind it: record NESTING is a ≈-invariant, so an occurs violation
-- that hides inside a field payload is still visible. Axiom-light on purpose.
/-- info: 'MinimalCalculus.RowEquiv.rcdDepth_eq' depends on axioms: [propext] -/
#guard_msgs in #print axioms RowEquiv.rcdDepth_eq

/-- info: 'MinimalCalculus.no_rcd_self_reference' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms no_rcd_self_reference

-- ## L2 qualified schemes (Qualified) — Classical.choice is expected here
/--
info: 'MinimalCalculus.qtyped_two_use' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms qtyped_two_use

/--
info: 'MinimalCalculus.selQ_instance_closed' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selQ_instance_closed

-- Type safety for the QUALIFIED system — the real declarative system of the
-- thesis. minimal.lean's progress/preservation are the L1 template.
/--
info: 'MinimalCalculus.qProgress' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms qProgress

/--
info: 'MinimalCalculus.qPreservation' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms qPreservation

-- ## The covering order ⊴ / ⊴⊑ (Qualified) — the vocabulary of principality
-- ⊑-transitivity is what ⊴⊑ composes with; axiom-free, like the ≗-congruence.
/-- info: 'MinimalCalculus.TyPrec.trans' does not depend on any axioms -/
#guard_msgs in #print axioms TyPrec.trans

/-- info: 'MinimalCalculus.QScheme.covered_toQ' does not depend on any axioms -/
#guard_msgs in #print axioms QScheme.covered_toQ

-- The obstruction that forces the up-to-precision order: a TYPING of λx.x.l
-- that is not an INSTANCE of selQ, because ★ is rigid under substitution.
/-- info: 'MinimalCalculus.selQ_no_blurred_inst' depends on axioms: [propext] -/
#guard_msgs in #print axioms selQ_no_blurred_inst

/--
info: 'MinimalCalculus.selEx_blurred_typing' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selEx_blurred_typing

-- ⊴ acting on contexts: scheme weakening for the L2 typing relation.
/-- info: 'MinimalCalculus.qtyped_bind_cov' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms qtyped_bind_cov

-- The certificate form of ⊴ — what a solver would emit.
/-- info: 'MinimalCalculus.QScheme.covered_of_witness' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms QScheme.covered_of_witness

-- ⊴[Γ] does NOT survive Γ ⊑ Γ': the ?-arm of discharge moves under a solution,
-- which is why the uniform ⊴ quantifies over every context.
/--
info: 'MinimalCalculus.covered_not_rowExt_stable' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms covered_not_rowExt_stable

-- The second closure property of the typing set: ⊑ alone cannot absorb T-eq,
-- so ⊑-only principality is refuted for selQ and ≼ replaces it.
/--
info: 'MinimalCalculus.selQ_not_principalStrict' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selQ_not_principalStrict

/--
info: 'MinimalCalculus.selQ_needs_equiv' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selQ_needs_equiv

-- ⊑ and ≈ commute — the lemma that makes ≼ transitive and ⊴≼ a preorder.
-- Axiom-free, like the rest of the precision theory.
/-- info: 'MinimalCalculus.TyPrec.comm_equiv' does not depend on any axioms -/
#guard_msgs in #print axioms TyPrec.comm_equiv

/-- info: 'MinimalCalculus.TyBelow.trans' does not depend on any axioms -/
#guard_msgs in #print axioms TyBelow.trans

-- THE POSITIVE BOOKEND: selQ is principal for λx.x.l in the ⊴≼ order —
-- instance-closed, inhabited, and covering EVERY typing (not just every lookup
-- verdict). Pair with no_plain_principal_scheme for the "forced" claim.
/--
info: 'MinimalCalculus.selQ_principal' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selQ_principal

/--
info: 'MinimalCalculus.selQ_greatest' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selQ_greatest

/-- info: 'MinimalCalculus.qsel_var_inv' depends on axioms: [propext] -/
#guard_msgs in #print axioms qsel_var_inv

-- L1 ⊊ L2, mechanized: the two-use program types in L2 and NOT in L1. Closes
-- the last claim of the metatheory that was only a comment.
/--
info: 'MinimalCalculus.l1_strictly_weaker' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms l1_strictly_weaker

/--
info: 'MinimalCalculus.l1_rejects_two_use' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms l1_rejects_two_use

-- ## ⟦S⟧ as a context (RowUnify.State) — the θ ↦ rowEnv bridge
-- Without these two, no inference rule that performs a lookup under a partial
-- solution is even a proposition. rowWF_toCtx is what lets A-sel's premise
-- HAVE a derivation (it feeds lookup_total); lookup_toCtx is the bridge itself.
/--
info: 'MinimalCalculus.Sol.rowWF_toCtx' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Sol.rowWF_toCtx

/--
info: 'MinimalCalculus.Sol.lookup_toCtx' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Sol.lookup_toCtx

/--
info: 'MinimalCalculus.Sol.lookup_toCtx_iff' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Sol.lookup_toCtx_iff

-- ## The general depth-aware occurs theorem (RowUnify.NoMgu)
-- The repair for BOTH spine-level blind spots: `sVarSeq` misses a variable that
-- sits inside a field payload, and so does `sFieldCount`. Record nesting sees
-- it at any depth, so α ≐ᵣ ρ with α under a `.rcd` in ρ has no unifier.
/-- info: 'MinimalCalculus.deep_occurs_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms deep_occurs_no_unifier

-- (1) what U-var-solve's occurs check should have caught
/-- info: 'MinimalCalculus.cyclic_binding_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms cyclic_binding_no_unifier

-- (2) the candidate host U-expand should have been able to eliminate
/-- info: 'MinimalCalculus.self_hosting_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms self_hosting_no_unifier

-- ## The repaired row-level occurs check (Refutations)
-- It used to be spine-only, so `a ≐ᵣ (l:{a})` succeeded with the cyclic binding
-- a ≔ (l:{a} | ε) on a problem with no unifier at all. It now tests
-- `Row.allRowVars (ofSpine s₂)` and answers occurs. These two pin the verdict
-- and the fact that the verdict is correct.
/--
info: 'MinimalCalculus.cyclic_occurs_reported' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms cyclic_occurs_reported

/-- info: 'MinimalCalculus.cyclic_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms cyclic_no_unifier

-- The syntactic sufficient condition for a legal solver state. NOT an invariant
-- of the driver — `a ≐ᵣ (l:a)` returns a solution that fails it, because ftv is
-- sort-blind — which is why Sol.WF is stated semantically. See State.lean.
/--
info: 'MinimalCalculus.Sol.wf_of_noCapture' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in #print axioms Sol.wf_of_noCapture

-- ## U-expand's self-reference filter (Stage 3)
-- The semantic content of the filter: a candidate host occurring in the payload
-- cannot host, and when NO candidate survives the problem has no unifier.
/-- info: 'MinimalCalculus.selfref_no_l_field' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms selfref_no_l_field

/-- info: 'MinimalCalculus.selfref_host_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms selfref_host_no_unifier

-- The multi-candidate generalization of host_proj that makes the filter usable:
-- the head of the projection sits at index 0, so SOME variable of the side
-- hosts it at the front.
/-- info: 'MinimalCalculus.proj_head_zero_var' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms proj_head_zero_var

-- Why the surviving candidate must also LEAD: `‖` shadows on a shared label, so
-- the invented field cannot commute past a variable that might carry one.
/-- info: 'MinimalCalculus.shadow_order_matters' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms shadow_order_matters

/--
info: 'MinimalCalculus.unrestricted_filter_refused' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unrestricted_filter_refused

-- What the filter used to buy, now `.stuck` on both counts.
/--
info: 'MinimalCalculus.selfref_filter_stuck' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selfref_filter_stuck

/--
info: 'MinimalCalculus.selfref_lone_host_reported' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selfref_lone_host_reported

/-- info: 'MinimalCalculus.selfref_lone_host_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms selfref_lone_host_no_unifier

-- ## ⟦S⟧ as a CLOSURE (State.lean)
-- `Sol.Applied` is no longer demanded of the driver: a ranked solution HAS a
-- closure, |dom| rounds of substitution compute it, and the bridge theorems are
-- stated against `Sol.Closes` instead.
/-- info: 'MinimalCalculus.Sol.closes_closure' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Sol.closes_closure

/-- info: 'MinimalCalculus.Sol.closes_toSubst_of_applied' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Sol.closes_toSubst_of_applied

/-- info: 'MinimalCalculus.Sol.closes_of_wf' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Sol.closes_of_wf

-- ## INFERENCE  Γ; S ⊢ e ⇒ τ; S′  (Infer.lean)
-- The judgement is new and the first thing to guard is that it is NOT EMPTY:
-- the motivating program goes through it end to end, landing on A-sel-? with
-- one stump parked — the shape `selQ` describes declaratively.
/-- info: 'MinimalCalculus.selEx_infers' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms selEx_infers

-- ## L2 TYPE SUBSTITUTION (QSubst.lean)
-- A QTyped derivation transports along a solution's closure, into the context
-- read under it. L1 had `typed_applySubst_aux`; QTyped had only TERM
-- substitution. This is the engine InferSound runs on — inference concludes at
-- the FINAL state while its premises type at intermediate ones. All thirteen
-- QTyped constructors, modulo the one named hypothesis `SchemeImage` (the
-- capture-avoiding σ-image of a let-bound scheme; L1's counterpart is
-- `renameScheme`).
/--
info: 'MinimalCalculus.qtyped_applySubst' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms qtyped_applySubst

-- The freshness invariant: a successful unification never hands back a supply
-- behind the one it was given, and inference therefore never re-issues a name.
-- (It lost its `Quot.sound` dependency with U-expand: the only quotient
-- reasoning in this proof was the expansion cases' `expandResM_success`.)
/-- info: 'MinimalCalculus.unifyM_supply_mono' depends on axioms: [propext] -/
#guard_msgs in #print axioms unifyM_supply_mono

/-- info: 'MinimalCalculus.Infer.supply_mono' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Infer.supply_mono

-- "θ is only ever refined" — what lets a premise solved at an INTERMEDIATE
-- state be replayed under the σ the conclusion is stated at.
/-- info: 'MinimalCalculus.Infer.sat_mono' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms Infer.sat_mono

-- ## INFERENCE SOUNDNESS, CASE BY CASE  (InferSound.lean)
-- Eight of the thirteen A-rules, plus A-sel-? modulo `StumpHonest`. The two
-- pieces of new machinery first: replaying a solved equation under any σ that
-- satisfies the state it produced, and transporting a DEFINITE lookup out of
-- ⟦S⟧-as-a-context under mere `Sat` rather than `Closes` — the hypothesis the
-- induction actually has at an intermediate state.
/-- info: 'MinimalCalculus.SolveTy.unifies_sat' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms SolveTy.unifies_sat

/--
info: 'MinimalCalculus.Sol.lookup_toCtx_sat' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Sol.lookup_toCtx_sat

-- A record-typed subject always types a selection at ★: the declarative content
-- of "a selection never gets stuck", and what makes the ★ half of A-sel-? free.
/--
info: 'MinimalCalculus.qtyped_sel_star' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms qtyped_sel_star

-- The rules themselves. A-app and A-conc emit an equation and let T-eq absorb
-- the ≈; A-sel and A-sel-⊥ additionally read a field off the solution.
/-- info: 'MinimalCalculus.infer_sound_app_step' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms infer_sound_app_step

/-- info: 'MinimalCalculus.infer_sound_conc_step' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms infer_sound_conc_step

/--
info: 'MinimalCalculus.infer_sound_sel_step' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms infer_sound_sel_step

/--
info: 'MinimalCalculus.infer_sound_selAbs_step' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms infer_sound_selAbs_step

-- A-sel-?, the case proof-state.md calls a DESIGN question. The answer is that
-- a parked stump is worth a DISCHARGE — the declarative `Stump.Discharge`, read
-- at σ against a discharged row environment, with the hit payload relaxed to ≈
-- because a SOLVED equation is only ever an ≈-fact. Given that, the rule is
-- sound; establishing it is wake-up's job.
/--
info: 'MinimalCalculus.infer_sound_selUnk_step' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in #print axioms infer_sound_selUnk_step

-- ## THE K-/D- CORRESPONDENCE  (InferSound.lean)
-- "K-hit / K-⊥ / K-repark are D-hit / D-⊥ / D-?" as a theorem, at the
-- granularity of one wake-up step: a step either DISCHARGES its constraint or
-- re-parks it with the stump intact. K-repark corresponds to nothing, which is
-- why the conclusion is a disjunction and not an implication.
/--
info: 'MinimalCalculus.Wake.dischargeEquiv' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Wake.dischargeEquiv

-- …and over a whole run. The lift needs exactly one structural fact — every
-- rule that retires a stump filters the parked list on `stump.res`, so an entry
-- survives a step precisely when its result variable differs from the one being
-- woken — plus the distinctness of what was submitted, which is what
-- `FreshRenaming` gives A-var.
/--
info: 'MinimalCalculus.Wakes.dischargeEquiv' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Wakes.dischargeEquiv

/-- info: 'MinimalCalculus.InstStumps.pairwise' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms InstStumps.pairwise

-- A-var, once the discharge obligation is separated out: qVar wants a scheme
-- and an instance, and the instance's own substitution is ours to choose —
-- which is what pays for the ≈ the correspondence leaves behind.
/-- info: 'MinimalCalculus.infer_sound_var_step' does not depend on any axioms -/
#guard_msgs in #print axioms infer_sound_var_step

-- ## WHY F-★ CARRIES ITS PREMISE  (InferSound.lean)
-- A REFUTATION, in the sense Refutations.lean uses the word: F-★ USED TO HAVE no
-- premise about the lookup, so it could commit a stump to ★ where the lookup
-- lands — and then nothing discharges it, not even up to ≈. The witness is a
-- stump on the literal row (l: 𝓫). It is stated about `FinalizeUnguarded`, which
-- IS the old rule kept for the purpose: a fixed rule makes its own
-- counterexample unstateable, so the counterexample has to name what it refutes.
/--
info: 'MinimalCalculus.finalize_star_no_discharge' depends on axioms: [propext]
-/
#guard_msgs in #print axioms finalize_star_no_discharge

-- …and the positive half: at that same state the SHIPPED rule has no derivation,
-- because a `sing` row is never blocked. So the premise is what rules the
-- configuration out, and the inclusion `Finalize ⊆ FinalizeUnguarded` is proper.
/--
info: 'MinimalCalculus.finalize_star_guarded_cannot_fire' depends on axioms: [propext]
-/
#guard_msgs in #print axioms finalize_star_guarded_cannot_fire

/-- info: 'MinimalCalculus.Finalize.toUnguarded' depends on axioms: [propext] -/
#guard_msgs in #print axioms Finalize.toUnguarded

-- ## FINALIZATION, GUARDED  (Infer.lean)
-- The premise costs nothing where it is used: every state a run produces is
-- quiescent, and quiescence IS this premise for every parked stump.
/-- info: 'MinimalCalculus.Finalize.of_quiescent' depends on axioms: [propext] -/
#guard_msgs in #print axioms Finalize.of_quiescent

-- …and it is what repairs the determinism claim: wherever F-★ applies, the only
-- wake-up step available on that stump is a K-repark, which commits nothing.
/-- info: 'MinimalCalculus.Finalize.wake_no_commit' depends on axioms: [propext] -/
#guard_msgs in #print axioms Finalize.wake_no_commit

/--
info: 'MinimalCalculus.SolverState.Quiescent.wake_no_commit' depends on axioms: [propext]
-/
#guard_msgs in #print axioms SolverState.Quiescent.wake_no_commit

-- ## THE TOP-LEVEL ENTRY JUDGEMENT  (Infer.lean)
-- §B's last ✘ row, now a definition — and not an empty one. `λx. x.l` runs to
-- `{β} → ★` with `l` flagged, F-★ supplying the ★: the L1-finalized type, reached
-- by the algorithm. `fStarEx_runs` is the complementary shape, where saturation
-- has already discharged the stump and finalization has nothing to do.
/-- info: 'MinimalCalculus.selEx_runs' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms selEx_runs

/-- info: 'MinimalCalculus.selEx_runs_star' does not depend on any axioms -/
#guard_msgs in #print axioms selEx_runs_star

-- ## THE STATE INVARIANT  (Infer.lean)
-- `SolverState.Quiescent` — every parked stump is genuinely blocked on the
-- blocker it records — is the invariant `plans/inference-gap-analysis.md` §B
-- lists as absent. It is now MAINTAINED: the A-rules take `SolveTySat` /
-- `WakesSat` (solve, then wake what the solution staled) in place of bare
-- `SolveTy` / `Wakes`, and every reachable state satisfies it. Before that
-- change this theorem was false, and `fStarEx_stale_blocker` below is the
-- counterexample.
/-- info: 'MinimalCalculus.Infer.quiescent' depends on axioms: [propext] -/
#guard_msgs in #print axioms Infer.quiescent

/-- info: 'MinimalCalculus.Infer.quiescent_of_nil' depends on axioms: [propext] -/
#guard_msgs in #print axioms Infer.quiescent_of_nil

-- what the invariant is FOR: in a quiescent state no parked blocker is solved,
-- so a stump's annotation is a fact about the state rather than a leftover.
/--
info: 'MinimalCalculus.SolverState.Quiescent.blocker_unsolved' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms SolverState.Quiescent.blocker_unsolved

-- …AND THE CONFIGURATION WAS REACHABLE. The hand-built witness above leaves one
-- objection open: A-sel-? parks `.var r`, never a literal row, so at the moment
-- of parking the lookup IS blocked. These say it does not stay blocked. On the
-- closed program `(λx. {a = x.l}) {l = c}` the run itself solves the stump's
-- blocker — A-app's arrow equation writes `r ≔ (l: 𝓫)`, and `Infer.var` is the
-- only rule that runs wake-up — so the final state carries a stump whose lookup
-- LANDS. Guarding the whole chain: the run, the stale blocker, the
-- K-hit/F-★ disagreement (determinism refuted), the non-discharge at that state,
-- and the type the program loses.
/-- info: 'MinimalCalculus.fStarEx_infers' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms fStarEx_infers

/-- info: 'MinimalCalculus.fStarEx_stale_blocker' does not depend on any axioms -/
#guard_msgs in #print axioms fStarEx_stale_blocker

/-- info: 'MinimalCalculus.fStarEx_not_quiescent' depends on axioms: [propext] -/
#guard_msgs in #print axioms fStarEx_not_quiescent

-- and the regression that says saturation FIXED it: the same closed program now
-- runs to an empty Δ with the refinement intact, `{a: 𝓫}` — the declarative
-- answer, not the `{a: ★}` the stale state finalized to.
/-- info: 'MinimalCalculus.fStarEx_recovers' does not depend on any axioms -/
#guard_msgs in #print axioms fStarEx_recovers

/-- info: 'MinimalCalculus.fStarEx_runs' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms fStarEx_runs

-- both halves of the fix at one state: the unguarded rule fires, the shipped one
-- cannot. Saturation removes the state from any run; the premise removes it from
-- the rule. Neither alone does both.
/-- info: 'MinimalCalculus.fStar_guarded_cannot_fire' depends on axioms: [propext] -/
#guard_msgs in #print axioms fStar_guarded_cannot_fire

/-- info: 'MinimalCalculus.fStar_wake_star_disagree' depends on axioms: [propext] -/
#guard_msgs in #print axioms fStar_wake_star_disagree

/--
info: 'MinimalCalculus.fStar_reachable_no_discharge' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms fStar_reachable_no_discharge

/-- info: 'MinimalCalculus.fStarEx_refinement_lost' depends on axioms: [propext] -/
#guard_msgs in #print axioms fStarEx_refinement_lost

-- `? on α`: the blocker of an unknown lookup, which A-sel-? and K-repark both
-- need and `Lookup` does not record. Sound, complete and deterministic.
/-- info: 'MinimalCalculus.Lookup.unknown_blocked' depends on axioms: [propext] -/
#guard_msgs in #print axioms Lookup.unknown_blocked

/-- info: 'MinimalCalculus.LookupBlocked.det' depends on axioms: [propext] -/
#guard_msgs in #print axioms LookupBlocked.det

-- ## SORTED OCCURRENCES (RowUnify/Defs.lean, RowUnify/State.lean)
-- `Ty.tyFtv` and `Ty.allRowVars` partition the sort-blind `ftv`, and they are
-- the two fibres of `sortedFtv`'s tag. This is what let `bindTy`'s occurs check
-- become sort-aware: it binds at the TYPE sort, so it guards on the type fibre.
/-- info: 'MinimalCalculus.Ty.mem_ftv_iff' depends on axioms: [propext] -/
#guard_msgs in #print axioms Ty.mem_ftv_iff

/-- info: 'MinimalCalculus.Ty.mem_tyFtv_iff_sortedFtv' depends on axioms: [propext] -/
#guard_msgs in #print axioms Ty.mem_tyFtv_iff_sortedFtv

-- ## SORTS OF INVENTED VARIABLES (Infer.lean)
-- Every drawn name now carries the sort it was drawn at, and the record is only
-- ever extended — which is what makes `A-let`'s `κ̄ = Γ(ᾱ)` premise mean that a
-- generalized binder is quantified at the kind it was invented at.
/-- info: 'MinimalCalculus.Infer.kinds_mono' depends on axioms: [propext] -/
#guard_msgs in #print axioms Infer.kinds_mono

-- ## PUSHING σ UNDER A SCHEME'S BINDERS (QSubst.lean)
-- The forward half of `QCovers` for the naive `QScheme.applySubst`, under the
-- capture-avoidance side condition `QScheme.Avoiding`. This is the content the
-- old "wiring renameScheme in here is open" note on `QScheme.applySubst` was
-- standing in for.
/--
info: 'MinimalCalculus.QCovers.forward_of_avoiding' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms QCovers.forward_of_avoiding

-- …and the BACKWARD half refuted for that same witness: `applySubst σ` is not
-- surjective, so an instance set — which is as large as its binders allow —
-- cannot be exactly the σ-image of one. `SchemeImage` therefore cannot be
-- discharged by pushing σ through the scheme, whatever the freshness discipline.
/--
info: 'MinimalCalculus.qcovers_backward_false_for_applySubst' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in #print axioms qcovers_backward_false_for_applySubst

-- ## WHAT A PARKED STUMP MEANS  (InferSound.lean)
-- `QTypedC` — typing under stump ASSUMPTIONS — is the answer to the question
-- proof-state.md called a design question rather than a proof-effort one: an inner
-- A-sel-? types its selection at a variable, and the variable has no declarative
-- reading until the promise is redeemed. As a hypothesis of the judgement it needs
-- none: `inferC_sound_selUnk_step` is the case with NO discharge, where
-- `infer_sound_selUnk_step` needs one.
/-- info: 'MinimalCalculus.QTyped.toC' does not depend on any axioms -/
#guard_msgs in #print axioms QTyped.toC

-- …and with no assumptions the two judgements coincide, so nothing was smuggled
-- in: `stump` is the only new rule and an empty Δ makes it unusable.
/-- info: 'MinimalCalculus.QTypedC.toQTyped' does not depend on any axioms -/
#guard_msgs in #print axioms QTypedC.toQTyped

/--
info: 'MinimalCalculus.inferC_sound_selUnk_step' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in #print axioms inferC_sound_selUnk_step

-- ## WHAT FINALIZATION IS WORTH  (InferSound.lean)
-- Blockedness gives an unknown lookup at a discharged row environment —
-- monotonicity read backwards, and the general form of what F-★'s premise buys.
/--
info: 'MinimalCalculus.lookup_unknown_of_blocked' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms lookup_unknown_of_blocked

-- …so finalization DISCHARGES the stump it finalizes: D-? with the lookup done by
-- the algorithm. On the nose, not up to ≈ — the ?-arm is rigid.
/--
info: 'MinimalCalculus.Finalize.dischargeEquiv' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Finalize.dischargeEquiv

/--
info: 'MinimalCalculus.Finalize.discharge_isUnk' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms Finalize.discharge_isUnk

-- and the join where it is cheap: a run that left nothing parked needs only the
-- induction, no transport.
/--
info: 'MinimalCalculus.runSound_of_inferSoundC_nil' depends on axioms: [propext]
-/
#guard_msgs in #print axioms runSound_of_inferSoundC_nil

-- ## THE SPENT PROMISE  (Infer.lean, InferSound.lean)
-- The TENSION CASE as a verdict: A-sel-? hands back δ so the position stays
-- writable, a USE of the selection writes an arrow into it, and then F-★'s own
-- `δ ≐ ★` clashes — ★ is rigid. No rule applies, which is the same discipline a
-- clash is rejected under.
/-- info: 'MinimalCalculus.no_finalize_of_spent' depends on axioms: [propext] -/
#guard_msgs in #print axioms no_finalize_of_spent

-- …and it is REACHABLE: `λx. λy. (x.l) y` runs to a quiescent state with the
-- stump still blocked, which cannot be finalized.
/-- info: 'MinimalCalculus.spentEx_infers' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms spentEx_infers

/--
info: 'MinimalCalculus.spentEx_cannot_finalize' depends on axioms: [propext]
-/
#guard_msgs in #print axioms spentEx_cannot_finalize

-- …while the program IS declaratively typeable, at {(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫. So this
-- is the algorithm's INCOMPLETENESS, not the declarative system's rejection, and
-- the gap is that `Stump.res` is a TyVar: the constraint the answer needs,
-- ⟨r.l ↓ (α → β)⟩, cannot be written.
/-- info: 'MinimalCalculus.spentEx_declarative' does not depend on any axioms -/
#guard_msgs in #print axioms spentEx_declarative

end MinimalCalculus
