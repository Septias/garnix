-- Unification ≐ / ≐ᵣ: the executable MUTUAL algorithm (unifyTyF /
-- unifySpineMF, entry points unifyTyM / unifyRowM) and the trichotomy legs.
-- Stuck ⟹ no-mgu is still a reduction; see NEXT at the end of Trichotomy.
--
-- Read Defs.lean first — it is the algorithm and the vocabulary the theorems
-- are stated in, with no proofs. The imports below are the table of contents,
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
