
related:
./feature-state.md
../claude/agent_instructions.md

# Claude Worktrees
- Negative Information ( / )
- Recursive Types ( / )
- Gradual Typing ( / ) 
- First-class labels (feat/fc-labels / ~/coding/garnix-wt/fc-labels) — plans/fc-labels.md
- Lazy semantics (feat/lazy-semantics / ~/coding/garnix-wt/lazy-semantics) — plans/lazy-semantics.md

# Findings

## Replay at A-var and saturation (2026-11-02)
Status: open. Nothing changed in `lean/` or `text/`; the replay claim is tested, not proved.

### Saturation terminates
- `satStep_wf` (`lean/InferFnTerm.lean:105`, guarded in `lean/Axioms.lean:1019`) holds from any state. Measure: lexicographic (parked stumps, stale stumps).
- K-hit and K-⊥ remove a stump. K-repark writes nothing and lowers the stale count, since the K!-step premise forces α′ ≠ α.
- A failing equation gets stuck, which is an error rather than a loop.
- Text defect: `S ⊢ Δ ↝! S′` (`text/sections/05_inference.typ:85-94`) leaves Δ unconstrained. K!-done accepts any Δ. Lean has no such argument (`Saturate`, `lean/Infer.lean:500`). Proposed: `S ⊢ ↝! S′`.
- Text gap: the termination theorem (05, line 295) only cites Lean; the measure is not in the prose.

### Replay always parks (tested)
- Claim: a qualifier's blocker is generalized, so A-var renames it to a fresh variable. The fields before it were already decided, so the lookup stops at the renamed blocker. K-cons never fires in replay, and the saturation in `↝^(*!)` has nothing to do.
- Evidence: no decided replay in any run.

| Programs | Count | With non-empty replay | Decided at replay |
|---|---|---|---|
| All closed programs up to size 8 | 5,277,345 | 2,747 | 0 |
| `k: w: let f = e in {a = w k; b = w "l"; c = f {}; d = f {l = c}}`, closed e over k, w up to size 6 | 179,513 | 984 | 0 |

- Method: a scratch copy of `inferF` (`lean/InferFn.lean:626`) whose A-var fails with a marker if an instantiated stump's `lookupQF` is not `.blocked`. Run with `lake env lean --run`. The grammar uses the labels l and m only and does not include the MacroNix forms. The script was not added to the repo.
- Not covered: the key `∗`. `text/sections/07_fc-labels.typ:217` lets a key variable bind to `∗`, which makes the stump opaque, but the Lean `Key` printer (`lean/InferRuns.lean`) handles only literal and variable keys. A monomorphic key solved to `∗` before the use could still be decided at replay.
- Contradicted by this result: 05 lines 104 and 194, which justify replay with stumps answered at replay.

### Proposal: drop the replay judgement
- A-var parks the renamed stumps and saturates: `S ∪ Q[β̄/ᾱ] ⊢ ↝! S′`. An opaque stump is stale on arrival, so K-⊥ answers it and chapter 07 needs no replay rules.
- Removes `↝^*`, K-nil, K-cons, K-park and `↝^(*!)` from 05.
- Costs: scheme stumps must keep their blocker (`Δ_q` has one; 05 line 253 and the Lean `Stump` drop it). `wakesF` (`lean/InferFn.lean:336`) recomputes the blocker, so Lean needs the same change or a lemma that its decided branch is unreachable.
- Next step: prove that lemma, then edit A-var, the Δ argument and 05 lines 104 and 194.

### Other notation issues in 05
- `*` means one pass in `↝^*` but "exhaustively" in `⇝_m^*` (line 245).
- `!` is not defined as a convention ("ends in a quiescent state"), and `⇝^!`, `↝^(*!)`, `⇝_m^*` and state-level `⇝` have no rules, only prose.
