# What is missing to STATE strong unification / inference theorems

Scope: `typesystems/algorithmic.typ` (design) vs `lean/RowUnify/*`, `lean/Qualified.lean`
(mechanization). The question is deliberately *statement*-level: which definitions are
absent, so that the theorem cannot even be written down — separate from which proofs
are open.

Legend: ✔ there (mechanized) · ◐ on paper only / partial · ✘ absent

---

## A. Unification ≐ / ≐ᵣ

| Statement | Status | What is missing to state it |
|---|---|---|
| success soundness — `Sat θ s → Unifies θ ρ₁ ρ₂` | ✔ `unifyRowM_success_sound` | — |
| success completeness — every unifier extends to one meeting `s` | ✔ `unifyRowM_success_complete` | — |
| **mgu on success** (the two combined) | ✔ `unifyRowM_success_iff` | — |
| clash soundness — clash ⟹ no unifier | ✔ `unifyM_clash_no_unifier` | — |
| occurs soundness | ◐ genuine case ✔; `occurs_allVar_hasMgu` refutes the general form | a *guarded* statement: "occurs ⟹ no unifier **unless** all of `vars(s)` are ... ". The side condition has never been formulated |
| stuck ⟹ no mgu | ✘ **false** (`stuck_masks_mgu`, `terminalNoMgu_false`) | nothing to fix at the statement level — the converse does not exist. What is stateable is the *disjunctive* form: the three no-mgu techniques + three conservativity witnesses. Already assembled, not yet written up |
| fuel independence | ✔ `unifyM_fuel_mono`, `unifyM_bounded` | — |
| **termination / totality** — `∃ fuel. result ≠ outOfFuel` | ✘ | a well-founded measure. Rémy's does not close (renaming adds no fields → host keeps `count_l = 0`). Without this, `unifyRow` is not a *function* and every inference theorem inherits the fuel parameter |
| **idempotence / acyclicity of the returned solution** | ✘ | `Sol` has no triangular-form or acyclicity invariant. `⟦S⟧` is specified as "applies the substitution *as a closure*" — closure application is only well-defined on an acyclic solution. Needed before `⟦S⟧` is even a total function |
| **unification preserves `RowWF`** | ✘ (only a comment at `minimal.lean:2279`) | the statement `RowWF Γ → unify ⇝ θ → RowWF (Γ ⊕ θ)`. This is the hinge between unification and the lookup relation: `lookup_total` is conditioned on `RowWF`, so without it `A-sel`'s premise `⟦S₂⟧ ⊢ ρ.l ↓ r` may have no derivation at all |
| refinement order on solutions `S ≤ S′` | ✘ | needed to state "inference only ever refines θ", the induction hypothesis of every completeness proof downstream |
| sorted occurs-check | ◐ paper `S-*` rules; Lean has no `Kind` | the mechanization uses an ftv spanning both sorts, so the proven statement is strictly weaker than the one in the thesis. Either mechanize sorts or state the theorem at the unsorted ftv and say so |

## B. Inference `Γ; S ⊢ e ⇒ τ; S′`

| Ingredient | Status | What is missing to state it |
|---|---|---|
| the judgement itself | ◐ `algorithmic.typ` only | no Lean definition at all — `Infer` does not exist |
| **`⟦S⟧` as a context** | ✘ | `⟦S⟧τ` is defined (substitution closure); `⟦S⟧ ⊢ ρ.l ↓ τ′` in `A-sel` uses `⟦S⟧` as a *context* for the lookup judgement, and that coercion is never defined. Declarative `L-α` reads row-solutions `(α = ρ) ∈ Γ`; the algorithm keeps them in θ. **The θ ↦ rowEnv bridge is the single most load-bearing missing definition** — `Γ; S ⊢ e ⇒ τ; S′ ⟹ ⟦S′⟧Γ ⊢ e : ⟦S′⟧τ` is unwriteable without it |
| well-formedness of `S` | ✘ | invariants that every rule must preserve: θ acyclic (⇒ `RowWF`), every stump in Δ genuinely blocked on its recorded blocker under `⟦S⟧`, δ's fresh and distinct, W irrelevant to typing. Nothing yet says what a *legal* solver state is |
| **name supply in the inference rules** | ✘ | `fresh α: Type` is informal prose. `≐` threads a `Supply` and has `Avoids`/`SolBelow`; the A-rules thread nothing. Needed for "inferred variables are fresh for Γ", which every soundness and generalization statement uses |
| sorts of invented variables | ✘ | `A-let` writes `κ̄ = Γ(ᾱ)`, but ᾱ are exactly the variables *not* in Γ. There is no sort assignment carried alongside the supply |
| **rules for non-success verdicts** | ✘ | the failure policy ("clash = hard error; stuck/occurs degrade to ★ + W") is prose only. No `A-app-stuck` / `A-*-degrade` rule exists, so the algorithm is *undefined* on those inputs — i.e. it is not yet a total relation and "inference terminates with a verdict" cannot be stated |
| parking a fresh constraint | ✘ | `A-var` submits `Q[β̄/ᾱ]` to `↝*`, but `↝` (K-hit/K-⊥/K-repark) is defined only on blocker-carrying stumps `⟨α ▷ ρ.l ↓ δ⟩`, while Q holds blocker-less `⟨ρ.l ↓ δ⟩`. A `K-park` rule that computes the initial blocker is missing |
| determinism of inference | ✘ | syntax-directed, so morally deterministic — but stating it needs "unique up to renaming of invented names", i.e. an α-equivalence on (τ, S) outputs |
| **termination of inference** | ✘ | inherits unification termination, *plus* the `A-let` Δ-split least-fixpoint (asserted monotone and bounded by \|Δ₁\|, never stated) *plus* the `↝*` wake-up closure |
| confluence of wake-up | ◐ claimed | the two pillars exist (`lookup_det`, `Discharge.mono_of_definite`); the statement "final (θ,W,τ) is independent of wake-up order" is not written |
| top-level entry point | ✘ | `⇓`/`F-★` finalization exists as a rule, but there is no `Infer(e) = finalize(...)` top-level judgement to state soundness *of the algorithm* about |
| **type substitution for L2** | ✘ | L1 has `typed_applySubst_aux`; `QTyped` has only *term* substitution (`qsubst_preserves_typing`). Inference completeness is proved by transporting a declarative derivation along θ — it needs the L2 version |
| non-vacuity of qualified schemes | ✘ | `T-let`'s inhabitation premise `∃τ₁. σ ≥_Γ τ₁` is claimed to hold "by construction" (a carried stump finalizes at ★). Needs `lookup_total` + the freshness discipline for δ. Until stated, `A-let` is not known to produce schemes `T-let` accepts |

## C. Principality — the vocabulary gap

| Ingredient | Status | What is missing |
|---|---|---|
| instantiation `σ ≥_Γ τ` | ✔ `QScheme.Inst` | — |
| **covering order `⊴` on qualified schemes** | ✘ | the flagged #1 open. Without it *neither* "the inferred scheme is principal" *nor* "the principal type improves under reduction" is a sentence |
| constraint entailment `Q ⊨ q` | ✘ | `⊴` in the qualified-types tradition is defined modulo an entailment relation. Here discharge is defined only relative to (Γ, θ); there is no entailment between *constraint sets*, hence no way to compare two schemes' Q's |
| precision `⊑` on types | ✔ (paper + L1) | — |
| **`⊑` on schemes / on constraint sets / on solver states** | ✘ | the algorithm degrades to ★ on stuck and occurs. So the inferred type is *not* most general in the substitution order, and plain principality is false by construction. The honest statement is "**principal up to precision**": a combined order `⊴ ∘ ⊑`. That order does not exist anywhere yet |
| soundness of degradation | ✘ | "replacing a position by ★ preserves declarative typeability" — needed to justify the failure policy, and to make the combined order above sound |
| ⊑-monotonicity of inference | ✘ | "a more precise input context yields a more precise inferred type" — the algorithmic image of L1's lookup monotonicity |

## D. One design gap that is not a statement gap

`★` is rigid (`U-★`) and has no elimination rule. So `A-app` on a `★`-typed
function emits `★ ≐ (τ₂ → β)` ⟹ **clash** ⟹ hard error, and declaratively
`(e.l) e₂` with `e.l : ★` has no derivation either. The two systems agree, so no
theorem statement is blocked — but the soft-typing claim "every program keeps its
untyped semantics, ★ only marks surrender" is *false* at the checker level: those
programs are rejected outright. Fixing it means a consistency relation (`τ ~ ★`)
next to `≐`, plus matching declarative rules — a real extension, not a gap to fill in.

---

## Critical path

1. `⟦S⟧` as a context + θ↦rowEnv bridge — unblocks every inference statement.
2. `RowWF` preservation by unification — unblocks `A-sel`'s lookup premise.
3. Supply threading + `S` well-formedness — unblocks freshness and generalization.
4. Degradation rules for stuck/occurs — makes the algorithm total.
5. `⊴` + `⊑`-on-schemes — the only route to *any* principality statement.
6. Termination (unification measure, then the `A-let` fixpoint).
