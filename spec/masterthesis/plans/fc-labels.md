# First-Class Labels: round off the extension

Branch `feat/fc-labels`, worktree `~/coding/garnix-wt/fc-labels` (from main, after the user commits main's working tree, see M0).
Plan file: `plans/fc-labels.md` (main, uncommitted). State reports: `typesystems/feature-state.md` § Feat: First-class labels (main).
Spec: the thesis chapter itself, `text/sections/07_fc-labels.typ` (main). Findings so far: `typesystems/proof-state.md` § First-class labels (2026-10-31).


# Context
Chapter 07 reads as a rule-by-rule patch over chapters 02-05, not as one idea. The analysis (chat, 2026-10-31) found:
- **No spine.** The single new judgement, the three-valued key comparison, is a one-liner. The "barrier" generalization that 07:7 promises is never delivered. Unification has five ad-hoc exceptions (07:183).
- **Duplication.** There are six lookup rules where three would do. Four 𝓫-rules (T-/A-sel-dyn-𝓫, T-/A-rcd-dyn-𝓫) patch over the fact that strings have no key type.
- **G2 gap (verified in Lean).** MacroNix strings (`MConst.str`) are not labels at runtime. So the intro example `{ "1759190400" = 1; }.${toString builtins.currentTime}` *always* reaches ↯, and T-sel-dyn-𝓫 types a definite error.
- **Costs that need not be costs:**
  - `r // { ${n} = v; }` with a computed string n is rejected (fixes.md 26): T-rcd-dyn-𝓫 gives ★.
  - A key used before it is known to be a string clashes (07:252, 09 G3).
  - Strings and labels are separate.
- **Stale is defined wrongly in the prose.** A stump can become decided while its blocker is still unsolved. 05, 07, 09 G4 and fixes.md 24 say "blocker solved"; Lean's `staleF` is right.
- **Occurs exception is a proof artifact.** Occurs next to a keyed field is reported as stuck only because the counting proof ignores keys (`RowUnify/Defs.lean:709`).

Decided with the user (2026-10-31):
- `string := ⌊★⌋`, with an unknown key ★ (`k ::= l | α | ★`).
- One key sort, and **no α ≡ α**: only two literals compare definitely, so any key variable may become ★.
- MacroNix strings are labels.
- A literal join at A-if / A-cons comes last.

Intended outcome: one kind of string, no string-specific typing rules, ↯-key gone, the `//` idiom typed at `{${★}: τ | ρ}`, and the chapter organized around the key comparison.


# Goal
- **Lean (feature worktree):**
  - key comparison without α ≡ α
  - unknown key ★
  - MacroNix strings as labels with `string := ⌊★⌋`
  - 𝓫-rules and ↯-key removed
  - literal join
  - green build, no sorry, Axioms guards
- **Thesis (main):** 07 restructured section by section around the key comparison, plus knock-on fixes in 00, 05, 09, 11, nix-parity and glossar.

# Non-goals
- No identity of unknown keys. `{${a} = v}.${a}` becomes a stump that hits at every literal instance, not an immediate hit. This is documented as a cost.
- No joins beyond two literal keys at A-if / A-cons (`[ {name = "a";} {name = "b";} ]` still clashes: the Q4 gap).
- No finite label sets (`⌊{x86, arm}⌋`), which the `genAttrs systems` idiom would need. Outlook only.
- No `e ? ${k}`, no `let ${e}`, no `attrNames` / `mapAttrs` / `listToAttrs`.


# Design

## D1 Key comparison: only literals decide
- `Key.cmp` (`minimal.lean:399`):
  - `lit l, lit l'` gives `.eq` / `.apart`
  - every other pair gives `.undec`, including `var α, var α`
- Consequences:
  - `LookupQ.applySubst` gets simpler: no `.eq` on variables.
  - L-dhit fires only on literal keys, which ≈-dlab already identifies with literal fields.
  - `Key.cmp_self` becomes "on literals".
- Unchanged: U-key-L/R (they cancel syntactically equal keyed fields, which is type equality via ≈-dsing, not a key comparison) and M-merge (syntactic stump comparison).
- Blockers (`InferFn.lean:110` `keyBlocker`):
  - a variable lookup key blocks on itself
  - a literal key against `${γ}` blocks on γ
- Fillability (InferFn ~494, LetSpent) and the entry-point search instantiate a key variable to a fresh literal. Check that nothing relies on α ≡ α.

## D2 Unknown key ★
- `Key.unk` (`minimal.lean:68`):
  - `Key.ftv .unk = []`
  - substitution leaves it fixed
  - `Key.cmp .unk _ = Key.cmp _ .unk = .undec`
  - `TySubst.lab` may send a variable to ★ (no sort restriction needed after D1)
- **Opaque ?.** A comparison with ★ never decides. The lookup answers ? with no blocker that could ever help.
  - Lean: `LOut.opaque`, with `HoldsQ = LookupQ … .unknown`.
  - Inference treats it like ⊥ for the result:
    - A-sel-dyn answers ★ on the spot.
    - New K-opaque: δ ≐ ★, drop the stump. This matches D-?.
  - An opaque spent stump is not fillable.
- ≈ / trace monoid: `${★}: τ` is a keyed field with a non-literal key, so it is already a barrier. Check that `rowEquiv_iff_char` and cancellativity are generic in non-literal keys (`Row.SpineVarFree` only admits `dsing (.lit l)`).
- `unifyKey` (`RowUnify/Defs.lean:659`):
  - a variable binds to any key, including ★
  - ★ ≐ ★ succeeds
  - ★ ≐ l clashes; this is sound since ★ is a distinct key constant
  - U-key-pin: `(${α}) ≐ (${★})` binds α ≔ ★, and `(l) ≐ (${★})` clashes
- Typing: no new rule. A key of type ⌊★⌋ goes through T-sel-dyn-★/⊥ and T-rcd-dyn (`{${★}: τ}`).
  - Algorithmically, ⌊★⌋ ≐ ⌊β⌋ binds β ≔ ★, so A-sel-dyn-𝓫 and A-rcd-dyn-𝓫 are subsumed.

## D3 Strings are labels (MacroNix)
- Drop `MConst.str`. String literals become `.lab s`.
- `tStr := .lab .unk` (`Builtins.lean:34`). δ for `toString` / `strcat` produces and consumes `.lab`.
- Builtin types: `strcat : ⌊σ₁⌋ → ⌊σ₂⌋ → ⌊★⌋`, `toString : int → ⌊★⌋`. Prim types are instantiated by any θ, so σᵢ accept literals and ★.
- Declarative blur rules, siblings of T-★-intro (`qUnk`). Both apply to whole expressions only, so they induce no subtyping. Inference never uses them.
  - **T-lab-blur** `Γ ⊢ e: ⌊l⌋ ⟹ Γ ⊢ e: ⌊★⌋`. Needed for δ-preservation (`toString 3 → "3"`).
  - **T-field-blur** `Γ ⊢ e: {ρ₁ | l: τ | ρ₂} ⟹ Γ ⊢ e: {ρ₁ | ${★}: τ | ρ₂}`. Needed for E-rcd-dyn (`{${"s"} = e} : {${★}: τ}` steps to `{s = e}`).
  - Sound because no lookup reads through ★.
  - Fallback if the inversion lemmas break: keep dynamic records with a ⌊★⌋ key as values (keyed record bodies). Ask before switching.
- Remove `qSelDynBase` / `qRcdDynBase` (`Qualified.lean:619-622`) and their inference twins (Infer.lean, InferFn.lean, Absorb, ParkedInv, InferSoundA).
- Remove `Err.selDynKey` / `rcdDynKeyBad`. A key typed ⌊k⌋ is a label by canonical forms, and no other key type has a rule. `qProgress` gets stronger. This answers write-plan "Is ↯key okay?" and the 07 comment "why lookup error?".

## D4 MacroNix string ergonomics (last)
- Literal join: A-if and A-cons. If the two types are `⌊l⌋`, `⌊m⌋` with l ≠ m under the current solution, the result is ⌊★⌋ (declaratively: T-lab-blur on both).
  - The result depends on the order in which inference meets the literals; admitted in the text.
- `==` (`Builtins.lean:43`): `α → β → bool`, with `atomEq` returning false across atom groups, as Nix does (today `1 == true` fails). Otherwise `system == "x86_64-darwin"` pins the key of system to one literal.

## D5 Small items (no semantic change)
- Occurs over keyed fields: `Defs.lean:709` returns `.occurs`. Generalize `solveVarM_occurs_no_unifier` (NoMgu.lean) to count all field atoms, whatever their key.
- `RowEquiv` lemma `comm_keys`: `Key.cmp k₁ k₂ = .apart → (${k₁}: τ₁ | ${k₂}: τ₂) ≈ (${k₂}: τ₂ | ${k₁}: τ₁)`. It backs ≈-comm with a # premise.
- Regression guards (InferRuns.lean / ThesisExamples.lean) that pin today's answers before each semantic milestone moves them: staleness probe, flake idiom, `//` probe, `{}.${3}`, `(a: {l = c}.${a}) (toString 1)`.


# Thesis text (main; per section: propose in chat, then edit; keep the user's wording; resolve the inline comments)
T0-T3 and T6 can start once M2 is green. T4, T5 and T7 follow M4.
- **T0 Opening.**
  - The flake idiom `system: pkgs: pkgs.${system}.default` as motivation.
  - The one idea: every label comparison of 02-05 becomes a three-valued key comparison, decided only between literals.
  - Delete or deliver the barrier promise (07:7).
- **T1 Keys and lookup.**
  - `k ::= l | α | ★`, `string := ⌊★⌋`.
  - Comparison: l ≡ l, l # l′, otherwise ≟ ("apart", "undecided").
  - Three lookup rules, reading `l: τ` as `${l}: τ` (Lean: `LookupQ.lab_iff`). Opaque ? for ★.
  - Lemma blocks with Lean names (`LookupQ.det`, `LookupQ.applySubst`). The visual example (07:56 comment).
- **T2 Barriers.**
  - Definition: a keyed field with a non-literal key, or a row-variable.
  - ≈-comm with premise k₁ # k₂ (`comm_keys`). Spines carry over.
  - Hook to negative information.
- **T3 Unification.**
  - "var-free" becomes "barrier-free". U-key-L/R as U-cancel on a keyed barrier. U-key-pin, including α ≔ ★.
  - Key unification is first-order on atoms, which answers the U-key-refl / U-key-bind comments.
  - Occurs is now sound. Answer "explain the figure", "stands alone", "check".
- **T4 Typing and evaluation.**
  - Static selection as sugar for `e.${"l"}`.
  - The two blur rules as siblings of T-★-intro.
  - No 𝓫-rules, no ↯-key. Name T-sel-⊥ as the analogue (07:137 comment).
  - Theorem block (`qProgress`, `qPreservation`) including E-rcd-dyn.
- **T5 Inference.**
  - Blocker sources, opaque ?, K-opaque.
  - Stale = "no longer ? on its recorded blocker", also in 05 and 09 G4.
  - The fillability witness instantiates a key variable to a fresh literal.
  - Move 05's key clauses (05:177, 05:265) here.
- **T6 Small fixes.** "and and" ×2, "mechanism apply". "Label variables go into rows" should say keyed fields.
- **T7 Costs and knock-ons.**
  - Costs: identity of unknown keys; joins beyond literals; keyed barriers (negative info); ★ has no eliminators (`pkgs.${currentSystem}.hello`); finite label sets as outlook.
  - 00: the time example is now truthfully undecided.
  - 09: remove G3's fourth reason.
  - 11: strings as labels, builtins over ⌊★⌋, `==`, literal join.
  - nix-parity rows (strings, dynamic keys, `==`).
  - glossar: key comparison, apart, undecided, barrier, unknown key, opaque.
  - fixes.md 24/26 marked as resolved.


# Milestones (worktree; commit on feat/fc-labels after each green milestone; report in feature-state.md on main)
- **M0**
  - Ask the user to commit main's working tree; `lean/ThesisExamples.lean` has uncommitted changes. I never commit on main.
  - `git worktree add ~/coding/garnix-wt/fc-labels -b feat/fc-labels`, copy main's `lean/.lake`, green `lake build`.
  - On main: `plans/fc-labels.md` (this plan), the feature-state section, the proof-state worktree line.
- **M1** D5: occurs generalization, `comm_keys`, regression guards pinning today's answers, Axioms guards.
- **M2** D1: `Key.cmp` without α ≡ α. Fix the proofs, update the guards that move (the staleness probe becomes a stump; NixSuite `dynBare`), and check fillability and the entry search.
- **M3** D2: `Key.unk`, opaque lookups, K-opaque, `unifyKey` and U-key-pin with ★, the ≈ check. **Gate: report before M4a.**
- **M4a** D3 typing: MacroNix strings as `.lab`, T-lab-blur and T-field-blur, `qPreservation` / `qProgress`, δ-preservation of the string builtins.
- **M4b** D3 cleanup: delete the 𝓫-rules (declarative and algorithmic), shrink `Err`, re-run NixSuite, update the guards (`//` probe, `(a: …) (toString 1)` gives ★, `{}.${3}` is rejected).
- **M5** D4: literal join, `==`. Guards: `[ "a" "b" ]` gives `[⌊★⌋]`, and `system: if system == "x86_64-darwin" then … else pkgs.${system}` stays polymorphic.
- **M6** Merge readiness: `lake build`, NixSuite, fuzz, ThesisExamples in sync with 07. The user merges.


# Files
- **Lean (worktree):**
  - `minimal.lean` (Key, Key.cmp, Sig, Step/Err, Row/RowEquiv)
  - `LabelLookup.lean`, `Qualified.lean` (QTyped blur rules, progress, preservation)
  - `RowEquiv.lean`, `RowUnify/{Defs,NoMgu,OccursLift,Clash,Trichotomy,…}.lean`
  - `InferFn.lean`, `Infer.lean`, `InferSound*.lean`, `Absorb.lean`, `ParkedInv.lean`, `Let*.lean`
  - `Builtins.lean`, `MacroConservative.lean`
  - `InferRuns.lean`, `ThesisExamples.lean`, `NixSuite.lean`, `Axioms.lean`
- **Main:**
  - `text/sections/07_fc-labels.typ`, plus `00`, `05`, `09`, `11`
  - `text/figures/nix-parity.typ`, `meta/glossar.md`, `fixes.md`
  - `plans/fc-labels.md`, `typesystems/feature-state.md`, `typesystems/proof-state.md`
- **Reuse:**
  - `LookupQ.lab_iff` (static = literal key)
  - `staleF` (already re-runs lookups)
  - `qUnk` and its inversion cases as the model for both blur rules
  - K-⊥ as the model for K-opaque
  - `keyPin` legs (Clash.lean, Draws.lean) for the ★ clash legs


# Risks
- **R1 Core change.** The user approved touching the core (2026-10-31). Theorem names stay. Statements only get stronger (Err shrinks) or simpler (applySubst).
- **R2 Dropping α ≡ α.** Answers move (stumps instead of hits). Fillability or the entry search may rely on `(${α}: τ).α ↓ τ` for witnesses. Switch them to fresh-literal witnesses.
- **R3 Blur rules.** Every QTyped induction and inversion lemma gets two cases. The largest proof risk. At most three attempts per leg, then report; the fallback is keyed record values.
- **R4 Opaque ? in inference.** Must keep `runSound`, `merge_forced` and the fillability test. An opaque stump is never parked.
- **R5 Negative info.** Its plan uses `Key.cmpN` and `κ ≠ k` atoms. With ★ and no α ≡ α, they need adapting. Negative-info has no commits yet, so it should start from feat/fc-labels once merged. Note this in both feature-state sections.
- **R6 MacroNix churn.** The NixSuite translator (`ms` → `.lab`), printed guard types (`string` → `⌊★⌋` or precise `⌊s⌋`), the `MacroConservative` statements, and the appendix sentence on `1 == true`.
- **R7 Time.** The write-plan budget is ~4 days for the text. T0-T3 and T6 depend only on M2. If M4 stalls, T7 states the blur rules as the designed fix.


# Verification
- `cd lean && lake build`: no `^error`, no `sorry`, Axioms guards updated (`qProgress`, `qPreservation`, `LookupQ.applySubst`, occurs, `comm_keys`, the blur inversions, `runSound`).
- By hand: `lake env lean NixSuite.lean`, `lake build fuzz && lake exe fuzz`.
- Guards (final answers):
  - flake idiom: `⟨ρ₂.${κ} ↓ {ρ₁}⟩, ⟨ρ₁.default ↓ δ⟩ ⇒ ⌊κ⌋ → {ρ₂} → δ`; applied to `"x86"` and a matching record: 𝓫
  - `r: r ‖ {${toString 1} = c}`: `{${★}: 𝓫 | ρ}`
  - `(a: {l = c}.${a}) (toString 1)`: ★
  - time example: ★ with a warning, plus a `Step` example showing the literal-key version evaluates to the field
  - `{}.${3}`: rejected
  - `[ "a" "b" ]`: `[⌊★⌋]`
  - staleness probe: a stump hitting at literal instances
- Thesis: `typst compile text/thesis.typ` (from the flake's dev shell). Every new claim in 07 has a Lean name or a guard.
