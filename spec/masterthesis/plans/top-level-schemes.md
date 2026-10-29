# Top-Level Schemes

Branch `feat/top-level-schemes`, worktree `~/coding/garnix-wt/top-level-schemes`. Status (2026-10-29): M0-M9 done, M10 spec done, text open.
State reports: `typesystems/feature-state.md` § Idea: Top-level schemes (main).
Spec: `typesystems/mini-nix.typ` (main). No draft file; the thesis passages are listed under Text.


# Motivation
Entry today (05_inference.typ:271, `Run` Infer.lean:1293): infer, then F-hit on every spent stump (`Materializes`), then F-★ on every stump still parked (`Finalizes`). Both phases pick ONE instance of the scheme that A-let would have kept for the same term.

- F-hit is not forced (05:294 says so). It puts the field into the blocker, but a row behind the blocker may supply it
  - `x: (({l = y: y} ‖ x).l) c` runs to `{l: 𝓫 → α | ρ} → α`; `{ε} → 𝓫` also types, and no instance of the answer covers it (feature-state, F-hit guesses)
- F-★ decides too. `x: x.l` runs to `{β} → ★` (`selEx_runs_star`), and `no_plain_principal_scheme` says no plain scheme is principal for it. A-let generalizes the same term to selQ (05:177), which `selQ_principal` says is principal. A binding gets a better answer than the program itself
- Wand's program `a: b: (a ‖ b).l` runs to `{ρ₁} → {ρ₂} → ★`. With a top-level scheme it answers `∀ρ₁ ρ₂ δ. ⟨(ρ₁ ‖ ρ₂).l ↓ δ⟩ ⇒ {ρ₁} → {ρ₂} → δ`, which has the shape of Rose's principal type (10_related-work.typ:33)
- PROF 05:293: every nixpkgs file is a top-level function, so F-★ hits the dominant case. Override (09:24) gets `app : ★ → β`
- Two open problems already name this as their fix: key-blocked spent promise (Fix B) and F-hit guesses (feature-state, Base-System)

Fix: drop both phases. The top level merges stumps that ask the same lookup (a forced move, D5), then generalizes like A-let with Γ = ∅ and answers the scheme. Nothing is decided at the top level.


# Goal
- Entry: `∅; S₀ ⊢ e ⇒ τ; S₁`, `S₁ ⊢ Δ₁ ⇝ₘ* S₂` (merge), `σ = gen_{∅,S₀}(S₂, τ)`, `Δ_Γ = ∅` ⟹ `⊢ e ⇒ σ`
- `Run e σ` with `σ : QScheme`; `runF` returns a scheme
- `runSound : Run e σ → (∀ τ, σ ≥ τ → ∅ ⊢ e : τ) ∧ (∃ τ, σ ≥ τ)`: conjuncts 1 and 2 of `QScheme.Principal`
- `runF_selEx_principal : QScheme.Principal ∅ (x: x.l) σ` for the scheme `runF` answers: the first principal answer of the algorithm
- The F-hit witness: its answer has both `{l: 𝓫 → α | ρ} → α` and `{ε} → 𝓫` as instances
- The old answers are instances: F-★'s by D-?, F-hit's by the fill instance of `LetSpent` (shown on `selEx`, `spentEx`)
- Forced result merge at the entry (D5): `x: {p = (x.l) c; q = x.l}` stays accepted
- Key-blocked spent stumps answered (D6): `r: a: (r.${a}) c` gets a scheme instead of failing
- `Materialize`, `Finalize` and everything built on them are deleted, not left dead
- Text: inference makes no choice between typings; the exception sentence at 05:294 goes


# Non-goals
- Covering (conjunct 3 of `Principal`) in general. Inference stays incomplete (06), and T-★-intro joins give programs with no principal scheme at all (feature-state, "Joining breaks principality without a clash"). Covering only per example
- No change to L2 (`QTyped`, `QScheme.Inst`, `Stump.Discharge`), unification, wake-up. A-let changes only in M8 (merge at A-let) and, if union closure holds, M9 (key guess at A-let)
- No ★-reading of an answer as an algorithm step (Q1)


# Design

## D1 Entry
```
def Run (constTy : Sig B C) (e : Expr C) (σ : QScheme B) : Prop :=
  ∃ τ S₁ S₂ S₃ ᾱ, Infer constTy QCtx.empty runStart e τ S₁ ∧
    Merges S₁ S₁.parked S₂ ∧
    σ = letScheme S₂ (letCand S₂ τ) S₂.parked τ ∧
    -- witness only (D3, D6): guess keys in a copy, merge again, then admissibility
    KeyGuesses S₂ S₂.parked S₃ ∧
    LetAdmissible QCtx.empty runStart S₃ ᾱ ∧ letG S₃ ᾱ = []
```
- `Merges` is D5. It takes the place `Materializes` has in `Run` today, so the proof skeleton of `runSound` carries over
- The answer is read at S₂; S₃ only exists to show it has an instance. `KeyGuesses` includes the second merge
- At the top level `fresh` (Γ half) and `own` are free: Γ = ∅, `runStart` has no stumps. With `ᾱ = letCand S₃ τ` every blocker is generalized, so `letG = []` and `res` hold too. Only `LetSpent S₃ S₃.parked` is left. Lemma `topAdmissible_iff`
- `runF`: `inferF`, `mergesF`, read the scheme at S₂, then `keyGuessesF` and `mergesF` on the copy and `greatestAlpha ∅ runStart S₃ τ`; `letG ≠ []` fails with `"spent promise without witness"`
- `runF_terminates`: `mergesF` and `keyGuessesF` take over the stability/settling proofs of `materializesF` (InferFnTerm.lean:496-560)

## D2 Soundness at every instance
- Instance χ (fixed outside ᾱ, discharges `letQ` read at S₂). Set `σ′ := χ ∘ ⟦S₂⟧`
- `Absorbs σ′ S₁`, `Sat σ′ S₁.sol` from `S₂.sol.Clean` and `Merges.ext`, as `runSound` does today through `Materializes` (Finalization.lean:318)
- Each stump parked at S₁ either is still parked at S₂, and then holds at σ′ because its constraint discharges at χ, or the merge's saturation discharged it (`Merges.keeps`, today `Materializes.keeps`)
- `runSoundA` (LetCase.lean:623) gives `∅ ⊢ e : ⟦S₂⟧τ χ`
- No `SchemeRead` and no renaming: at the top level nothing outside the scheme shares its names

## D3 Inhabitation
- `letCase` proves it inline (LetCase.lean:425-615) from `LetResults` through `readSub σ ᾱ f`
- Factor it out as `LetResults.inhabited`, used by A-let (with f) and by the top level
- `inhabited_of_ext`: if S₃ extends S₂ (`Ext`, `KeepsS`, as `Materializes.keeps` states today), an instance of the scheme read at S₃ gives one of the scheme read at S₂. The instance is `χ₃ ∘ ⟦S₃⟧` restricted to ᾱ. A stump of S₂ still parked at S₃ discharges by χ₃; one the extension's saturation discharged holds at every σ that satisfies S₃. Same argument as D2
- So any extension of the state is a valid witness search. The top level uses it for the key guess (D6); with no key-blocked spent stump, S₃ = S₂
- This is where Materialize survives: as the fill witness inside a proof, never as a reported type. feature-state already says "Guessing is fine inside a proof (inhabitation witness), not in a reported type"

## D4 What the user sees
- Answers print as `∀ᾱ. Q ⇒ τ`; Q = ∅ prints as the plain type (InferRuns, ThesisExamples, NixSuite)
- ★ inside inference comes only from K-⊥ (definite absence) and A-sel-dyn-𝓫; elsewhere only at instances (D-?, D-⊥)
- Warnings (`flags`) come only from K-⊥; F-★'s `flag` goes. Wand's program loses its top-level warning: the ★ appears at a use, not at the definition

## D5 Forced result merge
`LetSpent` (Infer.lean:819) is stricter than Materialize. Its condition 2 asks a stump q on a spent stump's blocker with the same label to be spent as well, and its result to equal p's once type variables are ★ (`Parked.spentAlike`). Checked in Lean (2026-10-29, `lean_run_code` on InferRuns):

| program | today |
|---|---|
| `x: {p = (x.l) c; q = x.l}` | ok through F-hit: `{l: (𝓫 → t4) \| r7 \| ε \| ε} → {p: t4 \| q: (𝓫 → t4)}` |
| `let f = x: {p = (x.l) c; q = x.l} in {a = f {l = z: z}; b = f {l = z: z; m = c}}` | `fail: clash` (f monomorphic) |
| the same with `q = (x.l) c` | `{a: {p: 𝓫 \| q: 𝓫} \| b: {p: 𝓫 \| q: 𝓫}}` |

Row 1 would be rejected under D1 without a fix. The Nix shape is common: `pkgs.lib.foo …` and `pkgs.lib` in one file.

- Rule M-merge: p, q ∈ Δ with `⟦S⟧p.row = ⟦S⟧q.row` and `⟦S⟧p.key = ⟦S⟧q.key`, `S ⊢ p.res ≐ q.res ⇝! S′`. Both stumps stay parked
- Forced (`merge_forced`): at every θ that discharges both, the two lookups are the same lookup, so `lookup_det` gives the same verdict. D-hit gives both results ≈ the found type, and D-⊥ and D-? give both ★. So θ unifies the results up to ≈, and the mgu loses no instance
- Unification fails (clash, occurs, stuck): skip the pair. The merge never makes a run fail, so the entry accepts at least what `LetSpent` accepts without it
- After the merge q's result is p's, so `spentAlike` holds. This also covers two spent results that unify but differ in shape (`α → 𝓫` against `𝓫 → β`)
- Duplicate constraints on one result are fine in a scheme (`icSc_inst`, Finalization.lean)
- Gap that stays: a spent stump sharing its blocker with a key-blocked stump; two stumps on one blocker and label but different rows (`({l = …} ‖ x).l` against `x.l`) whose results unify but are not alike. Old Materialize accepts both when the results unify. M1/M2 measure them

## D6 Key-blocked spent stumps
- `r: a: (r.${a}) c` fails today (`"spent promise: …"`, InferRuns.lean:178): no row to extend. 06:56-58 and 08:250, :256 list it as incompleteness
- Answer: `∀ρ κ β. ⟨ρ.(κ) ↓ 𝓫 → β⟩ ⇒ {ρ} → ⌊κ⌋ → β`. Nothing is guessed in it
- Witness, in a scratch copy S₃ of S₂: for every spent stump blocked on a key variable κ, draw a fresh label ℓ, solve `⌊κ⌋ ≐ ⌊ℓ⌋` and saturate (the stump wakes and re-parks on its row), merge again, then `LetSpent S₃ S₃.parked` as before (fill the row with ℓ)
- This is "Fix A" of feature-state (guess the key), but only inside the witness. The report stays the scheme ("Fix B"), so the made-up label never reaches a type
- Fresh labels come from the supply, one per κ. Soundness needs no freshness proof: `LetSpent S₃` checks whatever the guess woke (other lookups on κ, literal lookups past a `${κ}` field). Freshness only makes the check succeed more often
- Sound by `inhabited_of_ext` (D3)
- Not at A-let before M9: `let f = r: a: r.${a} c` stays monomorphic (06:53)


# Milestones
- M0 worktree from main, green baseline; record current answers of InferRuns, ThesisExamples, NixSuite, Regressions
- M1 `runQF` beside `runF` (nothing deleted, no merge yet), scheme printer; diff old and new on all four corpora; report the gap in feature-state
- M2 merge at the entry: `Merge`, `Merges`, `mergeF(s)`, `Merges.keeps` (from `Materialize.keeps`), `merge_forced`; `runQF` uses it; re-diff, the D5 table's row 1 is accepted as a scheme
- M3 `LetResults.inhabited` factored out of `letCase`, `letCase` uses it, green
- M4 `RunQ`, `runQSound` (D2 + D3), `runQF_sound`, `runQF_terminates`, Axioms guards
- M5 key guess (D6): `KeyGuess`, `KeyGuesses`, `keyGuessF(s)`, `inhabited_of_ext`; `RunQ` checks admissibility at the copy; witness `r: a: (r.${a}) c` (scheme, plus `{foo: 𝓫 → 𝓫} → ⌊foo⌋ → 𝓫` as an instance)
- M6 witnesses: `runF_selEx_principal` (answer ≅ selQ, `Principal` transported along the renaming), F-hit witness (both instances), `spentEx` (fill instance), `{β} → ★` as the D-? instance
- M7 switch `Run := RunQ`; delete `Materialize(s)`, `Finalize(s)`, their F-functions and lemmas; update consumers (Files); green, no sorry
- M8 merge at A-let: a premise of `Infer.letE` on S₁, before ᾱ is chosen (about ten proofs case on `letE`; `letCase` carries the merged state via `Merges.keeps`). The D5 table's row 2 stops clashing
- M9 (done, syntactic: `LetSpent` reads a key variable of ᾱ as the label `${κ}`) only if union closure holds: key guess at A-let. First prove that "a witness exists after the key guess in a copy" is closed under union (`LetAdmissible.union`); if not, stop and report. Then `let f = r: a: r.${a} c` generalizes
- M10 spec `mini-nix.typ` (done: M-merge, G-solve, Entry), then the thesis passages with the user (read `text/write-plan.md` first)


# Files
- `Infer.lean`: `Run`, `RunSound` restated; `Merge`, `Merges`, `KeyGuess`, `KeyGuesses` next to where `Materialize` stood; delete `Finalize`, `Finalizes`, `Finalize.of_quiescent`, `Finalize.wake_no_commit`, `no_finalize_of_spent`, `Materialize`, `Materializes` (:540-695), `Finalize.supply/kinds/satMono` (:1402-1461, :1802-1807); `selEx_runs` restated, `selEx_runs_star` becomes the D-? instance
- `Finalization.lean`: F-★ discharge goes; `Materialize.keeps/clean` become `Merges.keeps/clean` (both are one `SolveTySat`); new `runSound` (D2, D3), `merge_forced`, `inhabited_of_ext`. Keep the file name, the lakefile glob names it
- `Absorb.lean:45-52` `Finalize(s).clean`
- `LetCase.lean`: D3 factoring
- `InferFn.lean:361-470` `finalizeF(s)` go, `materializeF(s)` become `mergeF(s)`; new `keyGuessF(s)`; `runF`, `runF_sound`, `runF_typed` (:872-896)
- `InferFnTerm.lean:496-575, :999-1024` stability/settling: F-★'s go, Materialize's become the merge's and the key guess's; `runF_terminates`
- `InferSound.lean`: F-★ refutations (`FinalizeUnguarded`, `fStar_*`, `spentEx_cannot_finalize`, :461-560, :700-780, :900-910) go; `spentEx_declarative` stays
- `LetSound.lean`: `runSound_false_unguarded_let`, `_let_captures`, `_unowned_let` restated at the identity instance (the empty-parked witnesses carry over)
- `MacroConservative.lean:135` `runF_mini`
- `InferRuns.lean`, `ThesisExamples.lean`, `NixSuite.lean`: expected strings (InferRuns.lean:178, the key-blocked spent promise, becomes a scheme)
- `Axioms.lean`: drop the guards of deleted theorems, add `runSound`, `LetResults.inhabited`, `merge_forced`, `inhabited_of_ext`, M5 and M6 witnesses
- untouched: `Qualified.lean`, `minimal.lean`, `RowUnify/*`, `Fuzz.lean` (unification only)


# Text (M10, minimal edits, with the user)
- 05_inference.typ
  - :14 "every pending stump is typed as ★" → becomes a qualifier of the answer
  - :17 TODO pipeline, solver-state figure :43-44 (drop ⇓ₘ, ⇓)
  - finalization figure :241-281 → Entry only; section "Wake-Up and Finalization" retitled
  - :289-294 rewritten: entry generalizes; "order of the phases is forced" and the non-forced sentence go; PROF :293 resolved
  - ex-repark (answer `∀ρ_r δ. ⟨(m: 𝓫_c | ρ_r).l ↓ δ⟩ ⇒ {ρ_r} → δ`), ex-spent (scheme; the old answer as the fill instance)
  - thm-infer-sound: "every instance of σ types e, and σ has one"
  - :173 "would never be finalized" → "would never be discharged"
- 04_unification.typ:245 "The only place where it does is the materialization…" → it never does
- 05_inference.typ, Entry: M-merge as a premise, and the sentence that it is forced
- 06_incompleteness.typ:56-58 ("Spent Promises Blocked on a Key") goes at the top level; :53 gains "a field selected once spent and once unspent" among what passes (M8), and loses `let f = r: a: r.${a} c` only if M9 lands
- 08_extensions.typ:250 ("A spent promise blocked on its key … remains unanswered"), :256 (the third cost and "absence of top-level qualified types" go)
- 09_discussion.typ:10-30: Overlay ("Finalization materializes"), Override (`app : ★ → β`); rerun every example type
- 11_conclusion.typ:4 "an answer that never arrives is recorded as the unknown type ★"
- 03_metatheory.typ: one sentence after selQ_principal that the algorithm answers selQ
- `meta/glossar.md:28-29`, `typesystems/mini-nix.typ:571, :1022, :1047`


# Risks
- R1 Acceptance gap. Without the merge, D1 rejects a field selected once spent and once unspent, which the old entry accepts (D5 table, checked). M2 closes that case and the shape case with a forced move; the two remaining cases of D5 are measured in M1/M2 and reported
- R2 The inhabitation block is tied to `readSub σ ᾱ f`. If f = id does not fit, the top level draws a fresh renaming like A-let (`fresh_renaming_exists`); costs nothing semantically
- R3 Every changed guard in ThesisExamples is a changed thesis claim (Wand's program :377, `timeEx` :367 is A-sel-dyn-𝓫 and stays). M1 lists them for M10
- R8 A-let and the entry diverge until M8 (merge) and, for key-blocked stumps, for good unless M9 lands. The text may say "the top level is A-let with Γ = ∅" only after M9; with M8 alone it needs the key guess as the one difference
- R9 One fresh label per key is a single guess. If it wakes a stump whose result then clashes, the run fails although another label might work. Rare; M1/M2 diffs show it
- R4 Longer answers in NixSuite; Q = ∅ prints plain
- R5 "finalized" is also an L1 notion (`finalized_no_blur`, 03:39). Do not rename L1 vocabulary
- R6 Other features: negative-information Q2 (`RunN` reports Π at top level) builds on this. gradual.md:80 expects F-★ to turn match constraints into ★-arms; it needs to re-plan that step
- R7 Claim scope. "No choice between typings" is prose, backed per step (mgu, `lookup_det`, K-⊥ = D-⊥). No general principality claim for the algorithm


# Open questions
- Q1 resolved (2026-10-29): F-★ goes too. Keeping it would keep `{β} → ★` for `x: x.l`, which is not principal, and the PROF objection. A ★-reading can be a remark (it is the D-? instance)
- Q2 resolved (2026-10-29): if `LetSpent` still fails after the merge, the run fails (`"spent promise without witness"`). No fallback to F-hit; only the two rare cases of D5 are lost
- Q3 resolved (2026-10-29): key-blocked spent stumps go in (D6, M5)
- Q4 resolved (2026-10-29): merge at A-let after M7 (M8); key guess at A-let only if union closure holds (M9). The top level needs no union, since it generalizes everything or fails
