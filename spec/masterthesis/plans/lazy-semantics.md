# Lazy semantics for MiniNix

Branch `feat/lazy-semantics`, worktree `~/coding/garnix-wt/lazy-semantics`, based on `feat/fc-labels` (ebe4166).
On approval this file is copied to `plans/lazy-semantics.md` on main, as `claude/agent_instructions.md` asks. State reports go to a new section `# Feat: Lazy semantics` in `typesystems/feature-state.md` on main.


# Context
The thesis motivates T-sel-⊥ and G2 by the laziness of Nix, but `Step` in `lean/minimal.lean` is call-by-value for application and `let`. Only record fields and list cells are lazy. So `(x: c) ({}.l)` reaches ↯ and `let x = 3 4 in 5` is stuck, while Nix returns c and 5.

Goal: application and `let` are lazy in Lean, the thesis says so, and `qProgress` / `qPreservation` keep their names and statements. Decided with the user (2026-11-01): T-let's inhabitation premise goes too, because laziness makes it unnecessary for safety.


# Analysis

## What the change needs
Checked by reading the proofs and by running scratch lemmas against the current oleans, on main and on feat/fc-labels (no files written).

| Question | Finding |
|---|---|
| Does substitution need values? | No. `qsubst_preserves_typing` and `qsubst_scheme_preserves_typing` only need a term typed in the empty context. `hval` is never used in the `beta` and `letBeta` proofs. Both lemmas checked on non-value arguments. |
| Does progress get harder? | No, simpler. `.qApp` only forces the function; `.qLet` always steps. No new stuck state. |
| Do inference or unification use `Step`? | No. Only `minimal.lean`, `Qualified.lean`, `MacroConservative.lean`, `Builtins.lean` (statements only) and `ThesisExamples.lean` do. |
| Does anything else force a redesign? | No. Values stay WHNF, and `‖`, `e.l`, `e.${k}`, `if`, `assert` and `?` already force what Nix forces. |

So the semantics change alone is small. The real consequences are for inhabitation.

## Inhabitation
- Under lazy `let`, neither progress nor preservation uses the premise. let-β was checked with instance-closure alone.
- With an uninhabited σ, T-var finds no instance, so e₂ cannot mention x and e₁ is never evaluated. This also answers the open note at `fixes.md:45` ("find an example that also breaks in real nix"): none can exist.
- `let x = 3 4 in 5` becomes typable declaratively (premises checked in Lean). `run` still answers `fail: clash`, because A-let infers e₁ regardless. This is a new, accepted incompleteness.
- `(x: 5) (3 4)` stays untypable, since T-λ-E types its argument. The thesis must state this asymmetry, not hide it.

## Fillability
- **A-let's test can go.** In `letCase`, `hres.2` (the `spent` condition) is only used inside `LetResults.inhabited`, which only serves qLet's inhabitation goal. `letScheme_wf` uses only `hres.1`. The instance-closure and well-formedness proofs do not use it. So removing `LetAdmissible.spent` does not affect soundness, on either branch.
- **The entry test stays.** `Run` keeps `Guesses` and `LetSpent`. Without them `runSound`'s `∃τ, σ.Inst τ` is lost, and "every instance types e" becomes vacuous for a whole program. It also keeps Principal's second conjunct and the `fail: spent promise without witness` guard (overlay form).

## Why base on feat/fc-labels
- Main has no Lean change that fc-labels lacks; its 5 extra commits are thesis text.
- fc-labels heavily changes `Infer`, `InferFn`, `InferSoundA`, `LetSound` and `Qualified`, which M2 and M3 also touch, and adds `qLabBlur`.
- Thesis chapter 07 on main already describes fc-labels.
- Merging stays the user's call. If fc-labels gets its M6 commits first, rebase onto them.


# Decisions
- **Call-by-name by substitution, no heap.** Theorem statements stay as they are, and sharing changes cost, not values, errors or termination, in a pure calculus. Thunks and a store would mean a new `Step` and store typing, for no thesis claim.
- **T-let loses its premise, A-let loses `spent`, the entry keeps its test.**
- **Keep A-let's `Merges`.** It is forced and harmless, and removing it is not needed.

Non-goals:
- Lazy builtins: `primArg` still forces all arguments. The only visible gap is e.g. `map` forcing `f` on `[]`; the parity figure says so.
- Recursive `let` / `rec`.
- Modelling pattern entry checks.


# Milestones
Commit in the worktree after each green milestone; never on main. Report to feature-state after each. At most three attempts per obstacle, then come back to the user.

## M0 Setup
- Create the worktree from `feat/fc-labels`, copy that worktree's `.lake`, confirm a green baseline.
- On main (uncommitted): `plans/lazy-semantics.md`, the feature-state section, and the worktree line in `proof-state.md`.

## M1 Lazy `Step` (premise still in place)
- `lean/minimal.lean`:
  - `Step`: `beta` and `letBeta` lose `Value`; delete `appArg` and `letCong`.
  - `Err`: delete `appArg` and `letBind`.
  - Simplify the `.tApp` and `.tLet` arms of `progress` and `preservation_aux`.
  - Fix comments ("CBV let", "value v", the VALUES header).
- `lean/Qualified.lean`: the same in `qprogress` (`.qLet` becomes `.step .letBeta`) and `qpreservation_aux`.
- `lean/MacroConservative.lean`: arms of `Step.mini`.
- `lean/ThesisExamples.lean`:
  - `.beta .rcd` becomes `.beta`.
  - Replace `stuckLet_stuck` with `Step δ0 stuckLet c`.
  - Reprove "no typing" directly, not via progress. A scratch proof of this already compiles on main (`scratch_let_bound`, `scratch_conApp_false`); it needs a `qLabBlur` arm on fc-labels.
  - New witnesses: `(x: c) ({}.l)` and `let x = {}.l in c` step to c, are typed at 𝓫, are not `Err`, and `run` gives `("𝓫", ["l"])`.
- `lean/Axioms.lean`: guards should stay unchanged. Accept a change only if a footprint shrinks.

## M2 Drop the T-let premise
- Remove the `∃ τ₁` argument of `qLet` in `QTyped`, `QTypedC` (`InferSound.lean`) and `QTypedA` (`InferSoundA.lean`). About 50 mostly mechanical sites, also in `QSubst.lean` (delete the `hinh` bullet), `LetSound.lean`, `LetCase.lean`, `MacroConservative.lean` and `ThesisExamples.lean`.
- `letCase`: remove the inhabitation goal. `LetResults.inhabited` stays for the entry.
- Rewrite the `qLet` comment in `Qualified.lean` and the comment before `qprogress`; both say progress would break.
- `ThesisExamples.lean`: rename `stuckLet` to `deadLet`, prove it typable at 𝓫, keep `run = fail: clash`, add `¬ ∃ τ` for `(x: c) (c c)`.

## M3 Drop `spent` from A-let
- `lean/Infer.lean`: `LetAdmissible` loses `spent`; `LetAdmissible.results` goes.
- `lean/LetChoice.lean`: delete `spent'` and `sideSpent`; `LetBad` loses its last two disjuncts; shrink `of_parts`, `union`, `congr`, `excluded`, `of_no_bad`; update the header.
- `lean/LetCase.lean` (`hres` becomes `hA.res`), `lean/InferSoundA.lean` (`letScheme_wf`), `lean/FreshNames.lean`, `lean/LetSound.lean` (`UnownedLet`): follow.
- Delete `Parked.fillable_mono` and its Axioms guard if nothing uses it. Update the `LetAdmissible.union` guard if its footprint shrinks.
- Keep `LetSpent`, `Parked.fillable`, `spentAlike`, `Guesses`, `top_inhabited`, `inhabited_of_ext`, `Run`, `runF`.
- **Guard flips:** `LetBad` prunes less, so answers in `InferRuns`, `ThesisExamples`, `Regressions` and `NixSuite` may change. Log every flip in feature-state. Expected direction: clash or monomorphic becomes typed. Stop and report any typed program that starts failing.
- `NixSuite.lean` row (5) (`patterns`) may flip to ✓; its note suspects `LetSpent` but says "not confirmed". Update the results table and its encoding note: under lazy `let`, a missing pattern field fails at first use, not at entry.

## M4 Text, on main
Re-read each file right before editing. The user is editing 02 and 05 today (`02:229` and `02:264` changed during this analysis), so anchor on content, not line numbers. Smallest local edits only, per the minimal-edit rule; read `text/write-plan.md` first. No commits on main.

| File | Change |
|---|---|
| `text/sections/02_declarative.typ` | T-let figure: delete the `∃τ₁` premise. Semantics figure: contexts lose `v E` and `let x = E in e`; E-β and E-let take any term. "call-by-value" becomes "call-by-name". The uninhabited-scheme sentence and the "third premise" sentence lose the safety reason. |
| `text/sections/05_inference.typ` | `gen`: delete "Δ_q fillable at S₁". Move the fillability definition from the generalization text to the entry paragraph; "fillability test of A-let" becomes "fillability test". |
| `text/sections/06_incompleteness.typ` | Fillability clause at A-let goes; the `[…](explain this)` block follows the M3 flips. |
| `text/sections/07_fc-labels.typ` | Fillability sentence refers to the entry. |
| `text/sections/09_discussion.typ` | Suite count, if NixSuite flips; G3 reasons, if affected. |
| `text/figures/nix-parity.typ` | Application row; Laziness row (fields, cells, arguments, `let`; no sharing; builtins force their arguments); pattern row (missing field at first use). |
| `meta/glossar.md` | Fillability test: entry point only. |
| `typesystems/gradual.typ`, `typesystems/mini-nix.typ` | "CBV application and let"; A-let side conditions. |
| `fixes.md`, `typesystems/feature-state.md` | Items 8, 9 and the note at :45 resolved; "A-let premises" section updated. |

Offered in chat, not edited, since they add sentences:
- A clause that NixLang shares thunks but sharing changes only the cost, which would need a citation not yet in the bib (Ariola et al., "A Call-By-Need Lambda Calculus", details to verify).
- A 06 entry for `let x = 3 4 in 5` (typed, but inference clashes).
- The T-let / T-λ-E asymmetry.


# Verification
In the worktree, after every milestone:
- `lake build`, then check for `^error` and `sorry`. The `Axioms.lean` guards run as part of the build.

Once M1 is green:
- `grep -nE 'appArg|letCong|letBind'` finds nothing outside selDyn/prim names.

After M2 and M3:
- `lake env lean NixSuite.lean`
- `lake build fuzz && lake exe fuzz` (0 violations)

Pinned in `ThesisExamples.lean` (fail the build if the claims drift):
- `(x: c) ({}.l)` and `let x = {}.l in c` step to c and are typed at 𝓫.
- `deadLet` steps to c and is typed at 𝓫, and `run` answers `fail: clash`.
- `(x: c) (c c)` has no typing.
- The overlay form still answers `fail: spent promise without witness`.

After M4:
- `typst compile text/thesis.typ` to a scratch PDF.
- `grep -rn 'call-by-value\|fillable at'` over `text/` shows only intended hits.


# Outcome (2026-11-01)
M0 to M3 are done; details in `typesystems/feature-state.md`. M4 is not: the thesis edits were reverted at the user's request, and thesis text changes only on their go. One change against this plan: M3 hit its stop rule (6 programs lost), and the user chose to make `Parked.spentAlike` compare `Ty.flat` forms. Result: 1404 generated programs, typed 676 to 748, none lost.


# Risks
- **Guard flips cannot be predicted without running.** A-let generalizes more, so answers can gain stump copies at use sites, and the entry test could fail where a monomorphic stump once passed. This is what the M3 stop rule is for.
- **fc-labels M6 is pending**, so a rebase may be needed. No conflict expected in `Step`.
- **Declarative and inference drift apart by one more case**: dead ill-typed bindings. Accepted in the decision above; the thesis must name it.
