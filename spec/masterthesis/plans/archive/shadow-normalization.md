# Shadow Normalization

Branch `feat/shadow-nf`, worktree `~/coding/garnix-wt/shadow-nf` (from main `2fab4f8`, baseline `lake build` green, 43 jobs).
State reports: `typesystems/feature-state.md` § Feat: Shadow Normalization (main).
Draft: `drafts/shadowed-fields.typ` (main): the argument, the cost table, the propositions on paper.
Lean is the source of truth: green `lake build`, no sorry, Axioms guards. Commit after each green milestone.


# Motivation
≈ is a trace monoid: distinct labels commute, equal labels keep their order AND their multiplicity. The multiplicity comes from scoped labels (Leijen, P&X), where restriction `e \ l` exposes the next l-field. MiniNix has no restriction, and Nix's `//` deletes an overwritten attribute, so a shadowed field (an l-field with an earlier l-field to its left, across row-variables too) is never observable: lookup stops before it, substitution and concatenation keep it shadowed, selection takes the leftmost binding.

T-eq retypes up to ≈ and unification decides ≈, so shadowed fields count. `runF sig 1000` (base = `{ name = ""; v = 0; }`, Nix `a // b` = `.cat a b`):

| # | program                                                         | today              | after B | ≅-mgu               |
|---|---                                                              |---                 |---      |---                  |
| 1 | `[ base (base // { name = ""; }) ]`                             | clash              | typed   | yes                 |
| 2 | `(f: { a = f base; b = f (base // { name = ""; }); }) (r: r.v)` | clash              | typed   | yes                 |
| 3 | `r: [ ({ l = 0; } // r) { l = 0; } ]`                           | `{ε} → [{l: int}]` | same    | none                |
| 4 | program 3 applied to `{ l = 5; }`                               | clash              | clash   | none                |
| 5 | `r: [ ({ l = 0; } // r) { l = true; } ]`                        | clash              | clash   | `α ≔ (l: bool)`     |
| 6 | `(b: [ b (b // { name = ""; }) ]) base`                         | clash              | clash   | `β ≔ (name: string \| γ)` |

Unifying modulo the shadowing relation (option C) is excluded: it is not cancellative (`(l:𝓫 | l:𝓫) ≅ (l:𝓫 | ε)` but `(l:𝓫) ≇ ε`), and `(α | l:𝓫) ≐ (l:𝓫)` has no ≅-mgu (unifiers α ≔ ε and α ≔ (l:𝓫)). ≈ is the coarsest equivalence that respects lookup and substitution and cancels a leading atom (maximality, draft), i.e. the coarsest relation in which U-field-L and U-var-refl-L stay forced.


# Goal
Two relations, one bridge:
- ≅ (shadowing relation) is row equality of the TYPE SYSTEM: T-eq and ≼ use it
- ≈ stays the relation UNIFICATION decides; `RowEquiv.lean` and `RowUnify/*` are untouched
- `nf` deletes shadowed fields before unification; the unifier's theorems apply to the normalized problem
- Programs 1 and 2 type; 3 to 6 keep their verdicts (documented as the price, see draft)
- Mechanized: lookup respects ≅, characterization of ≅ by nf, unification after normalization, no ≅-mgu, maximality of ≈

Stage 1 (M1-M3) alone is consistent: inference stays sound for the ≅-system since ≈ ⊆ ≅. Stage 2 (M4) is unsound without stage 1. Order is fixed.


# Non-goals
- No unification modulo ≅ (option C); no change to the cascade
- No deletion of repeated row-variables in nf (`(ρ | α | ρ′ | α)`): ≅ deletes fields only, so it is idempotent on var-free rows only. Revisit after M6 (Q1)
- No normalization in the middle of a run (inside `sApplySubst`): would break `unifyRowM_success_mgu` (Q2)
- No restriction / `removeAttrs`
- D-hit stays on ≈ (T-eq closes instances under ≅ anyway; same argument as "≈ instead of = loses nothing")
- No thesis text edits on this branch; text changes are proposed in chat after M6 and made on main


# Design

## D1 The relation ≅
`TyEquivS` / `RowEquivS` (names provisional): a mutual inductive with all constructors of `TyEquiv` / `RowEquiv` plus

    shadow : l ∈ ρ.fieldLabels → RowEquivS (.cat ρ (.sing l τ)) ρ

`fieldLabels` = literal field labels of the spine; a keyed field `${κ}: τ` is not a label occurrence and never shadows. Basic facts: `≈ ⊆ ≅` (`TyEquiv.toS`), symm/trans/congruence, stable under substitution, preserves the row-variable sequence (`sBarSeq`) and the label set, `lookup_equivS` (results both ⊥, both ?, or ≅ types).

## D2 Normal form
`nf : Row → Row`, `nfTy : Ty → Ty`: one pass over the spine with the set of labels seen; drop `l: τ` if l was seen, else keep it with `nfTy τ`. Variables and keyed fields are kept. `ShadowFree` predicate (deep).
- `nf` idempotent, `ftv (nf ρ) ⊆ ftv ρ`, size does not grow, `ShadowFree (nf ρ)`, `ShadowFree ρ → nf ρ = ρ` (up to spine)
- `ρ ≅ nf ρ`
- characterization `rowEquivS_iff_nf : ρ₁ ≅ ρ₂ ↔ nf ρ₁ ≈ nf ρ₂` (needs `ρ ≈ ρ′ → nf ρ ≈ nf ρ′`, `nf (ρ | l:τ) = nf ρ` for l ∈ ρ, `nf (ρ₁ | ρ₂)` determined by `nf ρ₁`, `nf ρ₂`); fallback route through `Row.Char`

## D3 Declarative switch (stage 1)
- `QTyped.qEq` takes `TyEquivS`
- canonical-form inversions (`Qualified.lean` ~660-910: `unk_inv`, `base_inv`, `fn_inv_both`, `rcd_inv_both`, lab, bool, list): redo for ≅; ≅-shadow only acts on rows, so heads are preserved
- preservation record cases (`Qualified.lean` 951, 1458-1505, 1772): `lookup_equivS`
- `hasSing` invariance (`Qualified.lean` 2526): ≅-shadow keeps a field, so it holds
- ≼ = ≅ then ⊑ (`TyBelow`, `Qualified.lean` ~2315), transitivity (`minimal.lean` ~2777): ⊑ commutes past ≅ (R2)
- principality (`selQ_principal`, `l1_strictly_weaker`) re-checked
- inference soundness glue: `InferSound*`, `LetSound`, `Finalization`, `Absorb`, `QSubst` (qEq uses: Qualified 25, InferSoundA 30, InferSound 12, LetSound 2, QSubst 2, Builtins 1, MacroConservative 1) via `TyEquiv.toS`

## D4 Normalization at the solver entry (stage 2)
- `SolveTy` / `SolveRow` (`Infer.lean` 375/383) unify `nfTy (τ.applySubst S.subst)` etc.; supply from the normalized problem (ftv shrinks, so avoidance holds)
- the executable twins must match: `solveTyF` (`InferFn.lean` ~169) and the row counterpart, termination in `InferFnTerm.lean` ~231-244, `runF_eq_run`
- `SolveTy.unifies` concludes ≅ on the applied types: `θ(nf x) ≈ θ(nf y)` ⇒ `θx ≅ θy`
- `runSound`, `run_typed`, `inferSound` green

## D5 Theorems (headliners, guarded in `Axioms.lean`)
- `lookup_equivS`, `rowEquivS_iff_nf`
- `unify_after_nf`: success ⇒ Θρ₁ ≅ Θρ₂; every ShadowFree-on-nf ≅-unifier satisfies the mgu condition of `unifyRowM_success_mgu`; clash/occurs ⇒ no ShadowFree ≅-unifier
- `shadow_no_mgu`: `(α | l:𝓫) ≐ (l:𝓫)` has no ≅-mgu, its only ≈-unifier is α ≔ ε
- `rowEquiv_maximal`: every equivalence ⊇ ≈ that respects lookup (results up to ≈), is stable under substitution and cancels a leading field or row-variable coincides with ≈ (proof in the draft, induction on the spine of ρ₁)
- `shadowS_not_cancel`: ≅ is not cancellative


# Milestones
- M0 Setup: branch, worktree, `.lake` copy, baseline build green. DONE
- M1 `Shadow.lean`: D1 (≅, ≈ ⊆ ≅, substitution, var sequence, labels, `lookup_equivS`). Add to lakefile globs
- M2 D2: `nf`, `nfTy`, `ShadowFree`, `ρ ≅ nf ρ`, characterization
- M3 Stage 1: D3, whole build green with T-eq on ≅ and inference unchanged
- M4 Stage 2: D4, whole build green with nf at the solver entry
- M5 D5 theorems + Axioms guards
- M6 Regressions: the six programs in `Regressions.lean` (1, 2 typed; 3-6 as in the table); `lake env lean NixSuite.lean` no verdict worse; `lake build fuzz && lake exe fuzz`
- M7 Report: feature-state.md, update the draft (status block), propose thesis edits in chat (02 ≅ + characterization, 04 bridge theorem + "(explain this)" at l.228, 06 witnesses are ≈-statements, 00 contribution 1 wording)


# Files
- new: `lean/Shadow.lean` (D1, D2, D5 parts), possibly `lean/ShadowUnify.lean` (bridge theorem, maximality)
- changed: `lakefile.toml` (globs), `Qualified.lean`, `minimal.lean` (≼ transitivity only if needed), `Infer.lean`, `InferFn.lean`, `InferFnTerm.lean`, `InferSound.lean`, `InferSoundA.lean`, `LetSound.lean`, `Finalization.lean`, `Absorb.lean`, `QSubst.lean`, `Builtins.lean`, `MacroConservative.lean`, `ThesisExamples.lean` (replays ≈ facts and typings of the text), `Regressions.lean`, `Axioms.lean`
- untouched: `RowEquiv.lean`, `RowUnify/*`, `LabelLookup.lean` (lookup), `NoMgu`/`UnifType` results
- main only: this plan, `drafts/shadowed-fields.typ`, feature-state.md section


# Risks
- R1 qEq inversions are used by pattern-matching on `TyEquiv` derivations in many places (InferSoundA 30 uses); each needs the extra `shadow` case or a detour through `TyEquiv.toS`
- R2 ≼ transitivity: needs `⊑ ; ≅ ⊆ ≅ ; ⊑` style commutation; ⊑ acts below ★ inside fields, ≅-shadow deletes fields, interaction must be checked (a ⊑-step inside a shadowed field is invisible after deletion: should be harmless)
- R3 Keyed fields: a keyed field may later become l; nf and ≅-shadow must ignore keyed fields completely. A keyed field before a literal l-field does not make it shadowed
- R4 Invariants over applied types (`Quiescent`, `SatMono`, parked stumps, `supplyTy`) may mention the un-normalized problem; nf must not break `Below`/supply lemmas used by `unifyM_good` / termination
- R5 Principality statements may silently change meaning with ≼ on ≅; re-read their statements, not only the build
- R6 Effort concentrates in M3; if M3 exceeds three attempts on a lemma, stop and report (agent instructions)


# Open questions
- Q1 Delete repeated row-variables too (observational equivalence, makes ≅ a band)? After M6
- Q2 Normalize mid-run? Current answer: no (breaks the mgu theorem)
- Q3 Print normalized types in `run` output and thesis examples?
- Q4 RESOLVED on main (2026-10-27): `NixSuite.lean` `upd`/`orE`/`pat` fixed; `patterns` now clashes (A-let, note (5)). Bring into the worktree before M6
