-- AXIOM GUARD. `#guard_msgs` pins the exact axiom dependencies of the headline
-- theorems: if a `sorry` (sorryAx) or any unexpected axiom ever creeps into a
-- proof these theorems rest on, the printed axiom list changes and this file
-- FAILS to build. Row-unification results are propext/Quot.sound only; the L2
-- qualified-scheme results additionally use Classical.choice (from minimal).
-- Update an expected message here only when the change is understood and intended.

import Qualified
import RowUnify
import Refutations

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

-- The occurs guard's incompleteness, sharply: a reported-occurs problem with an
-- MGU (the algorithm's verdict on it is occurs_allVar_reported).
-- Classical.choice matches its sibling no-mgu/mgu theorems (allvar_swap).
/--
info: 'MinimalCalculus.occurs_allVar_hasMgu' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms occurs_allVar_hasMgu

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

-- ## P3: unique-host expansion
-- host_forced mechanizes the maximality argument proof-state.md carries by hand
-- for crossfield; expand_reflect_fwd is the completeness direction of the move.
/-- info: 'MinimalCalculus.host_forced' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms host_forced

/-- info: 'MinimalCalculus.crossfield_host_forced' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms crossfield_host_forced

/-- info: 'MinimalCalculus.expand_reflect' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms expand_reflect

/-- info: 'MinimalCalculus.expand_reflect_fwd' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms expand_reflect_fwd

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

-- AND THE SHARPEST. Retreating from the `.stuck` VERDICT to TERMINAL
-- CONFIGURATIONS does not rescue the leg either: (l:{w}) ≐ᵣ (w | v) is terminal
-- (each of the twelve moves `none` by rfl) and has a UNIQUE unifier, because
-- hosting the field in w would force θw ≈ (l:{θw}). Field counts are blind to
-- that — the recursion passes under a record constructor — so the guards miss
-- it; `rcdDepth` is the ≈-invariant that sees it.
/-- info: 'MinimalCalculus.terminal_masks_mgu_terminal' depends on axioms: [propext] -/
#guard_msgs in #print axioms terminal_masks_mgu_terminal

/--
info: 'MinimalCalculus.terminal_masks_mgu' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms terminal_masks_mgu

/--
info: 'MinimalCalculus.terminalNoMgu_false' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms terminalNoMgu_false

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

-- What the filter buys: one new success, and one vacuous success closed.
/--
info: 'MinimalCalculus.selfref_filter_fires' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selfref_filter_fires

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

end MinimalCalculus
