-- Unification ≐ / ≐ᵣ: the executable mutual algorithm and the trichotomy legs.

import RowUnify.Defs          -- the algorithm + statement vocabulary, proof-free
import RowUnify.NoMgu         -- field-count invariant; base no-mgu techniques
import RowUnify.Solutions     -- Sol algebra, freshness, U-expand metatheory
import RowUnify.Reflection    -- move reflection, U-ground, agreement
import RowUnify.ExpandR       -- U-expand at the RIGHT end (mirror, not transport)
import RowUnify.Driver        -- fuel monotonicity and worked verdicts
import RowUnify.Soundness     -- success soundness
import RowUnify.Completeness  -- boundedness and success completeness
import RowUnify.Clash         -- clash soundness
import RowUnify.Trichotomy    -- the mgu statement and the stuck leg
import RowUnify.State         -- ⟦S⟧ as a context: the θ ↦ rowEnv bridge
