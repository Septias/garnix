# MacroNix

Branch `feat/macronix`, worktree `~/coding/garnix-wt/macronix`.
State reports: `typesystems/feature-state.md` § Feat: Macro-Nix (main).
Spec: `typesystems/macronix.typ` (main). Draft: `drafts/macronix.typ` (main).

# Goal
- Syntax: `if`, `assert`, `e ? l`, list literals, primitive operators/builtins; `e.l or d` derived
- Types: `[τ]`, designated base types `bool`, `int`, `string`, `null`
- L2 (QTyped) rules + inference rules for every new form
- Headliners extended: `qProgress`, `qPreservation`, unification trichotomy, `inferSound`, `runSound`, `run_typed`, `runF_terminates`
- Conservative: MiniNix terms type and infer exactly as before
- NixSuite ported, §10 idioms rerun on real Nix

# Non-goals
- `with` (later, as elaboration), `rec` / `let rec`, `__functor`, `__overrides`, string interpolation as a type
- `&&`/`||` beyond derived forms (`ite`)
- Joining heterogeneous branches / lists (see R2, Gradual)
- L1 (`Typed`): rejects new forms, like FC-labels (`typed_lab_false` pattern)

# Design

## D1 Signature (as built)
- `Sig B C`: `con : C → B`, `boolB : Option B`, `prim : C → Option (Ty B)` (all ftv generic), `delta : C → List (Expr C) → Option (Expr C)`
- builtins are constants (`prim p es` with `p : C`), no extra type parameter
- `CoeFun` keeps `constTy c`; `Sig.plain con` = MiniNix
- Laws in `SigOK` (after typing): no constant inhabits bool; δ preserves types. δ = none is `Err.primFail`
- Core stays parametric; MacroNix is one concrete instance (D7)

## D2 Syntax
- `Expr`: `prim : C → List Expr → Expr` (SATURATED), `bool : Bool → Expr`, `ite`, `asrt`, `nil`, `cons`, `has`
- `Ty.list : Ty B → Ty B`. Row unchanged
- `assert : Expr → Expr → Expr` (`assert c; e`)
- `e.l or d` := `(e ‖ {l = d}).l` (lazy: field is a thunk). No constructor
- Values: `bool b`, `nil`, `cons e₁ e₂` (lazy, like `rcd`); no partial prim applications (eta-expansion)

## D3 Semantics
- `iteC`, `iteT`, `iteF`; `assertC`, `assertT` (→ e); `hasStep`, `hasVal` (→ `tt`/`ff` by `RecBody.lookup`)
- `primδ`: saturated `app … (prim p) v₁ … vₙ` ↦ `δ p [v̄]`
- Err: `assert ff` (`Err.assertFail`), `ite`/`assert` on non-bool, `has` on non-record, prim errors (`head []`, div 0, unsupported `==`), congruences

## D4 Declarative (QTyped)
- T-prim: `prim p = τp`, `τp θ = τ₁ → … → τₙ → τ`, args at τᵢ (any θ: no closedness side condition)
- T-ite: `e : bool`, `e₁ e₂ : τ` ⇒ `τ`
- T-assert: `c : bool`, `e : τ` ⇒ `τ`
- T-nil `[τ]`, T-cons `τ`, `[τ]` ⇒ `[τ]`
- T-has: `e : {ρ}` ⇒ `bool` (no refinement; occurrence typing is negative-info's S3)
- ≈: congruence for `[·]`; ★ stays rigid, so canonical forms hold (no ★ eliminator)

## D5 Unification
- `[τ] ≐ [τ′]` ⇒ `τ ≐ τ′`; `[τ]` vs other head ⇒ clash; occurs through `[·]`
- Every leg (success-mgu, clash, occurs, terminates) + `stuck_masks_mgu` gets the list case. No new row moves

## D6 Inference
- A-prim: instantiate `primTy p` with fresh vars (A-var with Q = ∅)
- A-ite: cond ≐ bool, branches ≐ each other
- A-assert: cond ≐ bool, result = body
- A-nil fresh `[α]`; A-cons elem ≐ α, tail ≐ `[α]`
- A-has: subject ≐ `{ρ}` fresh, result bool

## D7 MacroNix instance (`Builtins.lean`)
- Constants: `int n`, `str s`, `bool b`, `null`
- Prims: `+ - * /`, `< <=`, `==`, `!`, `++`, `head`, `tail`, `elemAt`, `length`, `map`, `filter`, `foldl'`, `toString`, `isAttrs`, `strcat`; `> >= != && || ->` derived; `hasAttr` dropped (`?` covers it)
- `==`: `∀α. α → α → bool`, δ on constants, labels, `nil`; else `Err.prim` (Q3)
- Discharge D1 laws per prim

# Milestones
Status: M0-M7 done, merged into main (692e664); thesis: appendix + note in §8 (Q1: appendix)
- M0 syntax: D2 constructors + `Ty.list` through every match, all existing headliners green, `Axioms.lean` guards
- M1 `Sig`, semantics D3, `qProgress`/`qPreservation` extended
- M2 unification D5, all legs
- M3 inference D6, `inferSound`, `runSound`, `runF_terminates`
- M4 conservativity: `Expr.IsMini e → (QTyped ext ↔ old rules only)`, `run` unchanged on MiniNix
- M5 `Builtins.lean` + laws
- M6 NixSuite port (drop tuple encoding, rerun table, add list/if tests), `Regressions.lean`, Fuzz generator
- M7 main: `typesystems/macronix.typ`, §10 idioms on real Nix, §8 numbers (read `text/write-plan.md` first)


# Files
- touched: `minimal.lean` (Expr, Ty, Value, Step, Err, subst), `Qualified.lean`, `RowEquiv.lean`, `RowUnify/*` (UnifType, Defs, NoMgu, Completeness, …), `Infer.lean`, `InferFn.lean`, `InferFnTerm.lean`, `InferSound*.lean`, `Absorb.lean`, `ParkedInv.lean`, `Fuzz.lean`, `NixSuite.lean`, `Axioms.lean`
- new: `lean/Builtins.lean` (+ lakefile glob)
- main: `typesystems/macronix.typ`, `drafts/macronix.typ`, feature-state.md § Macro-Nix, thesis §8/§10

# Risks
- R1 Size: ~25 files match on Expr/Ty (FC-labels touched the same set). Do M0 alone, green, before any new rule
- R2 Conditional extension `if c then {a=…;} else {}` clashes (rows differ); heterogeneous lists clash. Declaratively typed via T-★-intro: same incompleteness as Unrestricted T-★-intro. §10 idiom 3 may stay untypable. State it, do not fix here
- R3 Merge conflicts: core edits collide with any other worktree touching `minimal.lean`/`Qualified.lean` (Gradual likely). Coordinate before M0
- R4 Thesis states L2 metatheory; after merge the mechanized defs are L2 + MacroNix. M4 conservativity must carry the L2 claims
- R5 Polymorphic `==` with δ undefined on records/functions: progress only up to `Err.prim`
- R6 δ-rules returning terms (`map`) need preservation via substitution lemmas, not just value typing

# Open questions
- Q1 Merge MacroNix into main's core, or keep it on the branch as thesis appendix?
- Q2 `null` and `string` as plain base types, or anything special (e.g. `toString` coercions)?
- Q3 `==`: polymorphic with partial δ (built: atomic values only)
- Q4 `if` with differing record branches: accept clash (proposal), or hand to Gradual?
