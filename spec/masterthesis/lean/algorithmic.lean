-- Algorithmic system — umbrella re-export. The former monolithic file was split
-- (2026-08-25) along its two independent halves plus the row-algebra foundation:
--   * Qualified.lean — L2 qualified schemes, discharge, principality, QTyped
--   * RowEquiv.lean  — the ≈-characterization (trace-monoid normal form)
--   * RowUnify.lean  — the ≐ᵣ algorithm + trichotomy legs (builds on RowEquiv)
-- `import algorithmic` still brings the whole development into scope.

import Qualified
import RowEquiv
import RowUnify
