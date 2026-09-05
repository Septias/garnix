-- AXIOM GUARD. `#guard_msgs` pins the exact axiom dependencies of the headline
-- theorems: if a `sorry` (sorryAx) or any unexpected axiom ever creeps into a
-- proof these theorems rest on, the printed axiom list changes and this file
-- FAILS to build. Row-unification results are propext/Quot.sound only; the L2
-- qualified-scheme results additionally use Classical.choice (from minimal).
-- Update an expected message here only when the change is understood and intended.

import Qualified
import RowUnify

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

-- ## P1: mutual ≐/≐ᵣ scaffolding (RowUnify, proof-plan.md §1.1/§2)
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

-- ## P2: the fresh-variable supply (RowUnify, proof-plan.md §1.4)
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

-- ## P3: unique-host expansion (RowUnify, proof-plan.md §1.4)
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

-- ## P4: the mutual ≐ / ≐ᵣ driver (RowUnify, proof-plan.md §1.2)
-- The fuel lemma replaces §1.3's termination measure: `outOfFuel` is its own
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

-- ## P5: the three forward legs, on the MUTUAL driver (proof-plan.md §4-P5)
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

-- The freshness invariant solve-and-apply forced (§4-P5): a run only mentions
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

-- ## P6: step 2 of the base-arm dispatch (proof-plan.md §4-P6)
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

-- The fourth leg, still a REDUCTION (to hbase / hexp / hsolve / hsolveTy).
/-- info: 'MinimalCalculus.unifyM_stuck_no_mgu' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyM_stuck_no_mgu

/--
info: 'MinimalCalculus.unifyRowM_stuck_no_mgu' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms unifyRowM_stuck_no_mgu

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

end MinimalCalculus
