# Dropping U-expand

Branch `worktree-drop-expand`, worktree `.claude/worktrees/drop-expand`, based
on `a3a150c`. Stage 0 ran on `main`; everything below runs here.

## Why

U-expand (Rémy-style unique-host expansion, `expandL`/`expandR` × 2 directions)
is the only arm that **invents variables** instead of applying a substitution.
Everything expensive about the current mechanization traces to that:

- it needs `renameVar`, which is spine-only and so leaves stale payloads — both
  pinned vacuity witnesses (`vacuous_success_payload_cycle`,
  `vacuous_success_spine_cycle`) go through it;
- it forced the move away from purely local arms to the solver-state-aware
  `DepGraph` guards (the "B1" change, `506bf95`);
- it is why `Sol` is triangular, which is why `Sol.Ranked` exists, which is what
  the whole refuted rank search was chasing;
- it blocks `unifyRowM … = .occurs ⟹ ¬∃θ` via the `depReach Θ` disjunct in
  `NoHost`.

The algorithm is incomplete whatever we do — `≐ᵣ` under asymmetric concatenation
is at least infinitary, so completeness was never reachable and "success ⟹
sound, stuck ⟹ `★`" is the real contract. The question is only where to draw the
incompleteness line, and U-expand is a defensible place.

## What Stage 0 established (2026-09-22, on `main`)

Full numbers in `typesystems/proof-state.md`. The three that drive this plan,
measured over 771 578 pairs in the `wide`/`deep`/`nest` universes at cap 64 by a
stubbed clone of the driver (`nxSpineMF` in `Fuzz.lean`):

1. **Verdict-monotone: 0 counterexamples.** Wherever both drivers reach a
   verdict, either they agree or the stub is `.stuck`. So every proof obligation
   here is of the form *verdict reached ⟹ property*, and removal deletes cases
   from inductions rather than weakening statements. In particular
   "completeness" is `… = .success s S' → Sol.Sat θ s` — conditional on success,
   never "a unifier exists ⟹ success". **No headline theorem is weakened.**

2. **`Sol.Applied` ⟺ no expansion fired, exactly.** Cross-tabbed, not inferred
   from equal totals: ¬Applied-and-lost 1036 / 38 808 / 348, ¬Applied-but-kept
   **0/0/0**, Applied-but-lost **0/0/0**. All 105 444 stub successes are
   `Applied`. Triangularity is *entirely* U-expand's doing; `Sol.comp` never
   produces a non-idempotent solution by itself.

3. **Cost: 7.0% / 30.5% / 9.1% of successes** degrade to `.stuck`. `clash ⇝
   stuck` is 0 everywhere (`projClash` already catches those); `occurs ⇝ stuck`
   is free, since both degrade to `★` downstream.

## The prize

Not "`UnifyWF` becomes provable" — `Acyclic` and `Ranked` already hold 0/0/0
under *both* drivers. The prize is that **`Sol.Ranked` and the `DepGraph` stop
existing**:

- `Sol.Applied` is strictly stronger than `Sol.Ranked`, and
  `Sol.closes_toSubst_of_applied` then hands over `⟦S⟧ = toSubst` directly. No
  rank, no closure construction, no `Sol.Closes` obligation at each use site.
- `expandDeps` is the `DepGraph`'s only producer, so with no expand arm the
  graph is `[]` at every node and `depReach [] V = V` makes every guard reading
  it inert. The `Fuzz.lean` clone already drops `Θ` entirely and agrees with the
  stubbed driver — the graph is provably dead code, not merely unused.

## Stages

Stage 0 is done and lives on `main` (additive only: a second clone in
`Fuzz.lean`, plus findings in `proof-state.md`).

### Stage 1a — neutralize

Replace the four expansion arms in `unifySpineMF` with `| none => .stuck`,
leaving every signature alone, and fix the fallout. Do **not** touch `Θ` yet:
staging the diff this way means a failure has one cause.

Declarations that go (38 named, plus the bodies they support):

| file | goes | lines |
|---|---|---|
| `RowUnify/ExpandR.lean` | whole file: `HostShapeR`, `hostShapeR_of_hostShape`, `expandR_spec`, `expandR_len`, `host_forced_R`, `expand_shift_R`, `expandR_reflect`, `expandR_reflect_fwd`, `expandR_reflect'`, `expandR_avoids`, `expandR_reflect_fwd'` | 577 |
| `RowUnify/Solutions.lean` | `expand_shift`, `expand_reflect`, `expand_reflect_fwd`, `uniqueHost_spec`, `expandL_len`, `expandL_spec`, `expandL_avoids`, `expandL_reflect_fwd` | ~400 of 1135 |
| `RowUnify/Completeness.lean` | `expand_bounded`, `expandR_bounded`, `expand_completeM`, `expandR_completeM` | ~250 of 1239 |
| `RowUnify/Trichotomy.lean` | `uniqueHost_none`, `expandL_none_field`, `expandR_none_field`, `expandResM_stuck`; `stuck_leading_shape_expand` and `terminal_leading_shape` **restated**, not deleted — their `NoHost` disjuncts collapse | ~120 of 338 |
| `RowUnify/Clash.lean` | `expandResM_clash`, `expandResRM_clash`, two dispatch arms | ~40 |
| `RowUnify/Soundness.lean` | `expandResM_success`, `expandResRM_success` | ~40 |
| `RowUnify/Driver.lean` | `UResM.Mono.expandRes`, `UResM.Mono.expandResR`, four `fuel_mono` cases | ~40 |
| `RowUnify/Defs.lean` | `uniqueHost`, `expandL`, `expandR`, `NoHost`, `expandResM`, `expandResRM`, `expandDeps` | ~120 |

`HostShape` is listed there in error and STAYS: `host_forced` and
`crossfield_host_forced` are stated in it, and they are kept (below).

**Kept deliberately.** These are facts about the *calculus*, not about the arm,
and they stop being the soundness argument for a move and become standalone
results: `host_proj`, `host_forced`, `crossfield_host_forced`,
`selfref_host_no_unifier` (`Solutions.lean`), `self_hosting_no_unifier`
(`NoMgu.lean`), and all of `NoMgu.lean` including `rcdDepth`. `Axioms.lean` P3
keeps its `host_forced` / `crossfield_host_forced` guards and loses the
`expand_reflect` / `expand_reflect_fwd` / `uniqueHost_none` ones.

### Stage 1b — delete `Θ`

Only once 1a is green. `DepGraph`, `depInsert`, `depRound`, `depMark`,
`depReach`, `depReach_mono` (~80 lines of `Defs.lean`), then the mechanical
parameter removal across the remaining signatures and the call sites in
`Infer.lean` (`:229`, `:237`, `:239`, `:246`) plus `InferSound.lean:539`. All of
those already pass `[]`, so **the inference layer and the 8/13 proved A-rules
are untouched** — the blast radius stops at `lean/RowUnify/`.

### Stage 1c — regressions

These are tripwires and are *expected* to fire. Re-derive each expected verdict
by hand; a `rfl` regression edited to match the implementation is worth nothing.

- `Regressions.lean`: `expandL_crossfield`, `expandL_wand_refuses`,
  `expandL_lfield_refuses`, `expandR_crossfield_mirror` — delete with the arm.
- `Regressions.lean:215 unify_terminal_masks_mgu_solved` → becomes `.stuck`.
- `Regressions.lean:245 vacuous_success_spine_cycle` expects `.occurs` reached
  via two expansions → becomes `.stuck`. This is the *good* outcome: the vacuity
  it pins disappears rather than being worked around.
- `Refutations.lean:244/254 terminal_masks_mgu_not_terminal` /
  `terminal_masks_mgu_now_solved` invert; `TerminalNoMgu` is refuted again and
  `terminalNoMgu_false` (deleted by `506bf95`) comes back.
- `Refutations.lean:50/51` `expandL … = none` — delete.
- `examples/mechanized.md:220-250` is *already* stale on this point (it still
  cites `terminalNoMgu_false`); after this it becomes correct again.

**Exit criterion for Stage 1: green build + the sweep re-run.** Not a new
theorem. If Stage 2 stalls, Stage 1 still stands alone — ~1500 fewer lines, the
`DepGraph` gone, and one uniform "solve and apply, never invent" story.

### Stage 2 — collect (open-ended)

1. `Sol.Applied` as a driver invariant. Stage 0 says it holds on 105 444 /
   105 444 stub successes; this is the proof. Then `Sol.Ranked`, `Sol.WF`,
   `UnifyWF` / `UnifyAcyclic` and the rank search are all *unnecessary*.
2. `unifyRowM … = .occurs ⟹ ¬∃θ` — the ~280-line induction the 09-16 daily
   declined to write because of the `depReach Θ` disjunct in `NoHost`.
3. A termination measure, `(|unsolved vars|, |spine|)` lexicographic.
   **Unverified and not evidenced by Stage 0**: the parametric families show no
   profile change (expansion was already linear on them), and `sApplySubst` can
   still grow a spine, so the inner component needs a real argument.

## Prerequisite for merging, not a follow-up

Every lost success routed to `A-app-degrade` → `★`, and `appDeg` has no
declarative counterpart — there is no `T-app-★`. Removal does not create that
gap but makes it load-bearing. Either add the ★-elimination rule or prove
`appDeg` unreachable **before** this branch merges.

**RESOLVED 2026-09-26 by a third option:** ★ stays rigid, and `appDeg`,
`selDeg` and `SolveTyDegrades` are deleted from `Infer.lean`. A stuck or occurs
verdict now has no rule and the program is rejected, just like a clash. That is
sound by construction; the lost successes become type errors instead of ★.

## Progress

### Stage 1a — DONE, build green

The four arms are `.stuck`. Signatures untouched, `Θ` still threaded. Fallout
fixed in six files, and the shape of it is worth recording because every case
was the *same* case:

- `Driver.lean` `unifyM_fuel_mono` — four expansion cases → one `projClash`
  split. `crossfield_success` became **`crossfield_stuck`**: kept deliberately
  as the concrete price of the removal (the problem has a unifier; the driver no
  longer finds it), and as the tripwire that fires if an arm is ever reintroduced.
- `Soundness.lean` — the `expL`/`expR` local shapes in `unifyM_supply_mono` are
  gone: they existed only to advance the supply by two for the invented δ, β′.
  With no arm inventing names, `arm` covers every case. The success-soundness
  induction lost its `expand_reflect` / `expandR_reflect'` leaves — the only
  places soundness had to reason about invented variables at all.
- `Completeness.lean` — both dispatch inductions. The expansion cases were the
  only ones with a non-trivial freshness side-condition (`expand_bounded`,
  `expandR_bounded` arguing the invented names stay above `V`).
- `Clash.lean` — `projClash` now carries the clash leg alone, which Stage 0
  predicted (`clash ⇝ stuck` = 0 in all three universes).
- `Axioms.lean` — **`unifyM_supply_mono` lost its `Quot.sound` dependency**
  (now `[propext]`): the only quotient reasoning in that proof was the
  expansion cases.

### Verification

`lake exe fuzz` — the real driver now agrees with the Stage-0 stub on
**771 578 / 771 578 pairs (100.0%)** in all three universes, with success counts
matching exactly (13 685 / 88 302 / 3 457). So Stage 1a changed the driver into
precisely the thing Stage 0 measured, and nothing else.

The prize landed, and harder than forecast:

- `Applied` fails **0** on the real driver (was 1036 / 38 808 / 348).
- Sections [6]/[7] of the main report now read *"over the **0** successes whose
  solution has any edge"* in all three universes. The solution dependency graph
  is **edgeless**: no solution ever mentions a variable in its own domain. So
  `Sol.Ranked` is not merely provable, the question is vacuous — rank ≡ 0 works
  — and `deep`'s 680 "ranked by NEITHER live candidate" successes are gone.
- `[4] ill-formed solutions (refutes UnifyWF): 0`, as before but now trivially.

### Tripwires, re-derived by hand and re-pinned

Every one computed, not pattern-matched to output:

| was | now | where |
|---|---|---|
| `unify_crossfield_mirror` success | `unify_crossfield_mirror_stuck` | `Regressions.lean` |
| `unify_terminal_masks_mgu_solved` success | `unify_terminal_masks_mgu_stuck` | `Regressions.lean` |
| `vacuous_success_spine_cycle` `.occurs` | `.stuck` | `Regressions.lean` |
| `vacuous_success_payload_cycle` `.stuck` | unchanged | `Regressions.lean` |
| `outOfFuel_is_separate` (crossfield @1) | new witness + `outOfFuel_is_only_the_budget` | `Regressions.lean` |
| `terminal_masks_mgu_now_solved` | `terminal_masks_mgu_stuck` | `Refutations.lean` |
| `selfref_filter_fires` | `selfref_filter_stuck` | `Refutations.lean` |
| `selfref_lone_host_reported` `.occurs` | `.stuck` | `Refutations.lean` |

`outOfFuel_is_separate` needed a **new witness**: its old one was crossfield at
fuel 1, which ran out only because U-expand recursed, and is now `.stuck` at
every budget — it could no longer tell exhaustion from stuckness. Replaced with
a nested-record pair that exhausts fuel in the *type* pass, plus
`outOfFuel_is_only_the_budget` pinning the same problem succeeding at fuel 3.

Two honest losses of precision, recorded rather than papered over:
`vacuous_success_spine_cycle` and `selfref_lone_host_reported` both had **no
unifier** and used to get the sharp `.occurs`; they now get the merely-safe
`.stuck`. `selfref_lone_host_no_unifier` still proves the no-unifier fact, so
soundness is intact and only precision dropped.

### Known incoherence, closes in Stage 1b

`Terminal` (`Defs.lean`) is defined by which *moves* refuse and still has
`hexpandR₁`/`hexpandR₂` fields naming arms the driver no longer consults. So
`terminal_masks_mgu_not_terminal` still holds while the driver is stuck on that
problem. When `Terminal` drops those fields the configuration is terminal again
and `terminalNoMgu_false` (deleted by `506bf95`) should come back, refuting
`TerminalNoMgu` as it did before. Until then `TerminalNoMgu` must not be cited
in either direction.

### Still to do in 1a — DONE

`ExpandR.lean` whole and the `Solutions.lean` / `Completeness.lean` /
`Clash.lean` / `Soundness.lean` / `Driver.lean` blocks went in `e61c65d`
(−1087). `Trichotomy.lean` and `Defs.lean` were deferred into 1b rather than
done here: their declarations are phrased *through* `NoHost` / `uniqueHost` /
`Terminal`, so they could not be restated while `Θ` still existed.

### Stage 1b — DONE, build green, sweep re-run

Merged `main` first (`4e737cf`): the branch was based on `a3a150c` and so
predated the commit that put Stage 0's findings in `proof-state.md`. `Fuzz.lean`
merged clean — the branch had re-applied the same 270 lines under a different
commit, byte-identical.

−1367 / +511 across 12 files. What went:

- **`Defs.lean`** — `DepGraph`, `depInsert`, `depRound`, `depMark`, `depReach`
  and the four monotonicity lemmas; `uniqueHost`, `expandL`, `expandR`,
  `NoHost`; `expandResM`, `expandResRM`, `expandDeps`. `HostShape` stays (see
  above). `Θ` off `solveVarM`, `unifyTyF`, `unifySpineMF`, `Terminal`,
  `TerminalNoMgu`, and the `[]` argument off every entry point.
- **`Terminal`** loses `hexpandL₁/₂` and `hexpandR₁/₂` — eleven fields, all
  naming moves the driver actually consults.
- **`Trichotomy.lean`** — the whole "BASE-ARM DISPATCH, STEP 2" section
  (`uniqueHost_none`, `expandL_none_field`, `expandR_none_field`,
  `stuck_leading_shape_expand`, `stuck_field_vs_var`, two list helpers) and
  `expandResM_stuck`. `terminal_leading_shape` is **restated**: with `NoHost`
  gone it is exactly `stuck_leading_shape` fed from the record.
- **`Fuzz.lean`** — the traced clone (`traceTyF`/`traceSpineMF`), the graph
  utilities, `depVerdict`, report section [7], and the Stage-0 clone
  (`nxTyF`/`nxSpineMF` + `NXStats`). The last of those is the one worth naming:
  after 1b the stub was **character-for-character the real driver**, so its
  A/B comparison could only report 100% by construction. Its job is done and its
  numbers are in `proof-state.md`; `crossfield_stuck` and
  `unify_crossfield_mirror_stuck` are the tripwire now, kernel-checked and one
  line each.

The mechanical parameter removal across the inference layer was 5 call sites
(`Infer.lean`, `InferSound.lean`), all of them dropping a literal `[]`, exactly
as forecast — the 8/13 proved A-rules are untouched.

#### The two prizes, both landed

**`occurs ⟹ ¬∃θ` is unblocked.** `solveVarM`'s guard read
`depReach Θ (allRowVars s₂)`, so `solveVarM_occurs_inv` could only conclude
*reachability* — a fact about the solver state — and
`solveVarM_occurs_no_unifier` had to take the occurrence as a HYPOTHESIS, with
`solveVarM_occurs_no_unifier_nil` as the only unconditional case (Θ = []). The
guard is local again: `_inv` now yields `α ∈ allRowVars (ofSpine s₂)` outright,
`solveVarM_occurs_no_unifier` is unconditional, and `_nil` is deleted as
subsumed. What is left of Stage 2's item 2 is the induction over the driver, not
a missing side condition.

**`TerminalNoMgu` is refuted again**, exactly as forecast.
`terminal_masks_mgu_not_terminal` is replaced by `terminal_masks_mgu_terminal`
(⟨rfl ×11⟩) and `terminalNoMgu_false` comes back in the form it had before
`506bf95`. With `stuck_masks_mgu` that settles **both** candidate converses
negative: neither the `.stuck` verdict nor terminality implies no-mgu. The
fourth leg is the specific no-mgu theorems plus the conservativity witnesses,
and there is no general converse left to look for — which is what
`RowUnify/Trichotomy.lean`'s NEXT block and `Axioms.lean`'s P6 section now say.

#### Verification

`lake build` green (24 jobs), `lake build fuzz` green, no `sorry`. `lake exe
fuzz` reproduces the Stage-1a verdict counts **exactly** — 13 685 / 88 302 /
3 457 successes over 74 529 / 672 400 / 24 649 pairs — with `[4] ill-formed
solutions (refutes UnifyWF): 0` and `[6] … over the **0** successes whose
solution has any edge` in all three universes. Axiom guards re-pinned;
`terminal_masks_mgu_terminal` is `[propext]`.

## Open input

The universes are synthetic and exhaustive, heavily weighted toward exotic
shapes; every lost witness is one shape up to renaming, `(l:{a}) ≐ᵣ (b | a)` — a
field whose payload mentions the other side's tail. Whether Nix-shaped code hits
it needs hand-written `Infer` examples (`//` over partially-known rows, applying
`{l: τ | ρ} → …` to a record with an abstract tail). That is the only evidence
that can make 30.5% look either irrelevant or fatal.
