-- Unification ≐ | ≐ᵣ: the executable. Builds on RowEquiv.

import RowUnify.Detectors     -- the algorithm's detectors and supply
import RowUnify.NoMgu         -- field-count invariant; base no-mgu techniques
import RowUnify.Solutions     -- Solutions algebra, freshness, U-expand metatheory
import RowUnify.Reflection    -- move reflection, U-ground, agreement
import RowUnify.Driver        -- the mutual driver and fuel monotonicity
import RowUnify.Soundness     -- success soundness
import RowUnify.Completeness  -- boundedness and success completeness
import RowUnify.Clash         -- clash soundness
import RowUnify.Trichotomy    -- the mgu statement and the stuck leg
