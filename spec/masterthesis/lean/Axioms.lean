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

-- ## ≐ᵣ trichotomy (RowUnify)
/-- info: 'MinimalCalculus.unifyRow_success_sound' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyRow_success_sound

/-- info: 'MinimalCalculus.unifyRow_success_complete' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyRow_success_complete

/-- info: 'MinimalCalculus.unifyRow_clash_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyRow_clash_no_unifier

/-- info: 'MinimalCalculus.unifyRow_stuck_no_mgu' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifyRow_stuck_no_mgu

/-- info: 'MinimalCalculus.unifySpineF_stuck_no_mgu' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms unifySpineF_stuck_no_mgu

/-- info: 'MinimalCalculus.projClash_no_unifier' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms projClash_no_unifier

/-- info: 'MinimalCalculus.stuck_not_both_ground' depends on axioms: [propext, Quot.sound] -/
#guard_msgs in #print axioms stuck_not_both_ground

-- ## L2 qualified schemes (Qualified) — Classical.choice is expected here
/--
info: 'MinimalCalculus.qtyped_two_use' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms qtyped_two_use

/--
info: 'MinimalCalculus.selQ_instance_closed' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in #print axioms selQ_instance_closed

end MinimalCalculus
