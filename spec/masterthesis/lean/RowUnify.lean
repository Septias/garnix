-- Unification ≐ / ≐ᵣ: the executable MUTUAL algorithm (unifyTyF /
-- unifySpineMF, entry points unifyTyM / unifyRowM), the field-count invariant,
-- and the trichotomy legs — success soundness & completeness (mgu), clash
-- soundness, fuel monotonicity, terminal-stuck structure, and stuck ⟹ no-mgu
-- (still a reduction; see NEXT at the end of Trichotomy). Builds on RowEquiv.
--
-- Defs.lean holds the algorithm and the vocabulary the theorems are stated in,
-- with no proofs — read it first. Then the base no-mgu techniques, the
-- scaffolding the driver is stated in (P1–P3), the driver's metatheory, and
-- the legs (P4–P6). proof-plan.md is the live plan.
--
-- This file is the root: the imports below are the table of contents, listed
-- in dependency order (each module imports its predecessor).

import RowUnify.Defs          -- the algorithm + statement vocabulary, proof-free
import RowUnify.NoMgu         -- field-count invariant; base no-mgu techniques
import RowUnify.Solutions     -- Sol algebra, freshness, U-expand metatheory
import RowUnify.Reflection    -- move reflection, U-ground, agreement
import RowUnify.Driver        -- P4: fuel monotonicity and worked verdicts
import RowUnify.Soundness     -- P5: success soundness
import RowUnify.Completeness  -- P5: boundedness and success completeness
import RowUnify.Clash         -- P5: clash soundness
import RowUnify.Trichotomy    -- P6: the mgu statement and the stuck leg
