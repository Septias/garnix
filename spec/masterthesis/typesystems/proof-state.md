
related:
./feature-state.md
../claude/agent_instructions.md

# Claude Worktrees
- Negative Information (feat/negative-info / ~/coding/garnix-wt/negative-info) — plans/negative-information.md
  - Occurrence Typing afterwards
- Recursive Types ( / )
- Gradual Typing (feat/gradual / ~/coding/garnix-wt/gradual) — plans/gradual.md
- MacroNix (feat/macronix / ~/coding/garnix-wt/macronix) — plans/macronix.md
- Broekhoof Semantics ( / )

# Thesis examples, checked in Lean (2026-10-19)
`lean/ThesisExamples.lean` (in the build globs) replays every worked example of `text/sections/*.typ`: unification examples move by move (`traceRow`, the driver with rule names, each checked to give the driver's verdict), inference examples by `runF`/`inferF`, plus the declarative typings, instances, ≈ facts and evaluations.
Resolved mismatches: §5 gained U-var-collapse, and U-var-solve/-occurs now test rv(s), the row variables at any depth, as the driver does. The §4 Corollary now cites `l1_strictly_weaker`'s own witness `{a = f {l = c}; b = f {}}`. Axioms.lean now guards the cited crossfield_success, finalized_no_blur, lookup_equiv, no_plain_principal_scheme, RowEquiv.cancel_cat_left/right, shift_no_finite_complete_set and unifyM_draws.
Not checked (no Lean counterpart): the P&X comparisons.

# Principality (2026-10-30)
- Stumps on a shared key encode constraints: literal key `⟨(m: A).m ↓ δ⟩, ⟨(m: B).m ↓ δ⟩` gives A ≈ δ ≈ B; a quantified key `⟨(m: A).κ ↓ δ⟩` gives δ ≈ A (κ ↦ m) or δ = ★ (otherwise), i.e. one T-★-intro
- So feature-state's "`g: x: {a = g x; b = g {l=c}}` has no covering scheme" overclaims: `∀α γ R κ. ⟨(m: {l: 𝓫}).κ ↓ α⟩, ⟨(m: γ).κ ↓ α⟩ ⇒ (α → R) → γ → {a: R | b: R}` is WF and has both families as instances (scratch check, not in the repo); principality of it still needs a QTyped inversion for the term
- What is false is answer-principality: `run` gives `({l: 𝓫} → α) → {l: 𝓫} → …` and misses the typing `(★ → R) → 𝓫 → …`
