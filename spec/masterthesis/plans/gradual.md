# Gradual Typing

Branch `feat/gradual`, worktree `~/coding/garnix-wt/gradual`.
State reports: `typesystems/feature-state.md` § Feat: Gradual Typing (main).
Spec: `typesystems/gradual.typ` (main). Starts after Negative information M5.


# Motivation
In L2, ★ is a top type without eliminators: a value typed ★ can be passed around but never applied, selected from or concatenated. Adding ★-eliminators to L2 alone breaks progress and preservation (feature-state § Findings), so the thesis currently has to admit the eliminator problem without a fix. ★ arises often in real Nix: every ⊥- or ?-lookup verdict, every unknown import, every `args` of a module. Reading ★ as the *dynamic* type gives it eliminators while keeping a safety theorem.

Nix already checks the tag of every eliminator operand (`(1).a`, `1 2`, `{} // 1` raise defined errors). Transient gradual typing (Vitousek et al.) therefore applies without inserting a single cast. The checks exist only in the proof.

We expect:
- eliminators for ★ (M-fn-★, M-rcd-★), the missing half of soft typing
- the conditional-extension idiom `{…} // (if c then {a = 1;} else {})` typed via ★ᵣ (feature-state Q4 option c, the only option that recovers it)
- `args // {x = 1}` with `args: ★` typed `{x: 𝓫 | ★ᵣ}`, so `.x` stays precise
- the Unrestricted T-★-intro witness `λg. {a = g {l=c}; b = g {m=c}}` typed without T-★-intro
- in the thesis: the "eliminator problem forces gradual typing" section (write-plan § Sessions) backed by a result rather than an outlook


# Goal
- ★ is the dynamic type, ★ᵣ the dynamic row ("some fields, unknown which")
- Consistent equivalence ≃ (≈ and consistency in one relation), matching ▷ with ★-arms
- Declarative `QTypedG`: eliminators through ▷, application through ≃, T-★-intro dropped
- Transient safety: tag progress, tag preservation, erasure (checks only in the proof)
- Conservative: on ★/★ᵣ-free terms and types it coincides with L2 without T-★-intro/T-sel-★/T-sel-⊥. L2 embeds up to precision
- Static gradual guarantee (Γ is the only source of precision)
- Algorithmic: unification up to ≃, inference sound against `QTypedG`


# Non-goals
- No casts, no blame, no runtime change to Nix programs (checked mode is Later)
- No annotation syntax (lib.types / mkOption is Later)
- No occurrence typing on `isAttrs`/`? l` (builds on Negative S3, Later)
- Wand, Levi and Shift stay stuck: none of them mentions ★
- Dynamic gradual guarantee: not claimed


# Design

## D1 Syntax and runtime
- Row `★ᵣ`, sort rule S-★ᵣ
- `e.l or d` becomes a constructor. The MacroNix desugaring `(e ‖ {l = d}).l` is wrong here: Nix gives `(1).a or 2 = 2`, but `1 // {…}` raises
- Tags: `𝓫_c`, fn, rcd, lab, plus MacroNix's bool and list
- ↯tag: an eliminator meets the wrong tag (X-app, X-sel, X-sel-dyn, X-cat-L/R, plus MacroNix: `if` on non-bool, `head` on non-list, δ on wrong tags)
- `or` and `?` are total on every tag (R-or-tag, R-has-tag)
- Relation to `Err.primFail` (feature-state Problems): wrong-tag δ failures become ↯tag. Only the declared failures (`head []`, div by 0) stay prim errors. Do the primFail fix on main first

## D2 Lookup
- L-★ᵣ: `★ᵣ.k ↓ ★` for every key, a found ★, never `?` or ⊥
- `(★ᵣ | ρ)` and `★ᵣ` are NOT made ≈-equal (keeps `rowEquiv_iff_char`)
- Headliners: `LookupG.det`, `.total`, `.applySubst`, `lookupG_starFree_iff` (agrees with LookupQ on ★ᵣ-free rows)
- With Π: `★ᵣ ⊬ l` is never entailed (★ᵣ may hold l)

## D3 Consistent equivalence and matching
- Row splitting `s ⊲ₗ (τ, s′)` (SP-hit, SP-skip, SP-★ᵣ), ≃ / ≃ᵣ as in gradual.typ
- ▷: M-fn-★, M-rcd-★, plus M-list-★ and M-bool-★ for MacroNix. No ★-arm for keys yet
- Headliners: ≃ reflexive and symmetric (not transitive), `consEquiv_starFree_iff` (= ≈ on ★-free types), decomposition into ≈ ∘ ∼ ∘ ≈ (Sekiyama-Igarashi Thm 4.1, new: several vars per row, mid-row ★ᵣ)

## D4 Declarative typing
- `QTypedG`: T-λ-E via ▷ and ≃, T-conc/T-sel/T-sel-dyn via ▷, T-★-intro dropped, T-sel-★ and T-sel-⊥ kept
- ★-safe observers: T-or-hit, T-or-⊥, T-or-?, T-or-tag, T-has
- Headliners: `qTypedG_conservative` (template Sekiyama-Igarashi Thm 5.3), `l2_embeds_upto_precision`

## D5 Safety
- Elaboration `e ⇝ e⁺` with checks `⦇T⦈ e` at C-arg and C-res; R-chk / X-chk
- ★-arm eliminations get no check: Nix's own ↯tag plays ↯chk
- Headliners: `tagProgress` (never ↯tag at a static elimination), `tagPreservation` (tag-level typing ⊢ₜ), `erasure` (both directions)
- Plain preservation does NOT hold (`c.l` has no type). The thesis must say this

## D6 Precision and gradual guarantee
- ⊑-★ᵣ. Headliners: monotonicity of ↓, ≃, ▷; `staticGradualGuarantee`

## D7 Unification (decision needed, Q1)
- Recommended: ★ consistent and never bound (Siek-Vachharajani), `★ ≐ τ ⇝ ∅`; ★ᵣ absorbs a prefix (CE-★ᵣ-L), U-field-L gets a ★ᵣ case pairing the field with ★
- Mgu statements become "mgu up to ≃ for ★-free solutions"; the clash leg becomes "no unifier up to ≃"

## D8 Inference
- A-app, A-sel, A-conc through ▷: ★ takes the ★-arm, α is bound to the shape
- Spent promises: match constraints `δ ▷ β₁ → β₂` instead of `δ ≔ β₁ → β₂`; F-★ turns them into ★-arms. Check whether F-hit and `LetSpent` disappear
- `or`: stump with default `⟨ρ.l ↓ δ ∥ τ₂⟩`
- A-let premises re-checked against ≃
- Headliners: `inferSoundG`, `runSoundG`

## D9 Principality
- ★ is no longer rigid, so the §11 witness has ⊑-incomparable typings
- Candidate order ⊴⊑. Decide after M7, or state as open


# Milestones
- M0 paper: decide Q1, extend gradual.typ to MacroNix tags/eliminators, `or` constructor
- M1 scaffold `lean/Gradual/`, own `Gradual/Axioms.lean`, syntax, ↯tag runtime, green build
- M2 L-★ᵣ + D2 headliners; ⊲, ≃, ▷ + D3 headliners
- M3 `QTypedG`, conservativity, L2 embedding up to precision
- M4 elaboration, tag progress, tag preservation, erasure
- M5 precision monotonicity, static gradual guarantee
- M6 unification up to ≃ + trichotomy
- M7 inference, `runSoundG`, principality decision
- M8 integration with Negative information (★ᵣ × Π, `propagate` on `α ≔ ★ᵣ`)


# Files
- new: `lean/Gradual/{Syntax,Runtime,Lookup,Consistency,Typing,Safety,Precision,Unify,Infer,Regressions,Axioms}.lean`, `lean/Gradual.lean`
- touched: `lean/lakefile.toml` (globs `Gradual`, `Gradual.+`)
- untouched up to M5: every existing `.lean` file. M6 may need a hook in RowUnify (R3)
- main: `typesystems/gradual.typ`, thesis section on the eliminator problem, feature-state section


# Risks
- R1 ≃ is not transitive: no proof may chain two ≃ steps. Declarative rules use ≃ only at one use site
- R2 Safety is transient, not standard: plain preservation fails. The thesis's safety claim weakens to "every Nix type error is caused by ★" (tag progress + erasure)
- R3 Unification: ★ ≐ τ moves from the clash leg to success. `unifyRowM_clash_no_unifier` changes meaning. RowUnify is ~13k lines; try a wrapper first, touch the core only if forced
- R4 Dropping T-★-intro changes existing L2 typings; conservativity holds only on ★-free terms, embedding only up to precision
- R5 Principality may be lost (D9). Acceptable outcome if stated
- R6 `or` as a constructor diverges from MacroNix's desugaring; MacroNix conservativity must be restated for it
- R7 Interaction with Π (M8): `α ⊬ l` and `α ≔ ★ᵣ`, is this a lacks clash or consistent? Undecided


# Open questions
- Q1 Unification: Siek-Vachharajani (recommended, closest to ≐), Garcia-Cimini gradual variables, or Miyazaki-Sekiyama-Igarashi analysis only?
- Q2 Lean scope: everything through M7, or paper for M6-M7 if time runs short?
- Q3 Checked mode (emit C-arg/C-res as Nix asserts): thesis contribution or Later?
- Q4 Key ★-arm `ρ.(★) ↓ ★` (FC-labels): in M2 or Later?
