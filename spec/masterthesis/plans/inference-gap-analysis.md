# What is missing to STATE strong unification / inference theorems


Legend: ✔ mechanized (a theorem) · ◐ **stated in Lean, not proved** — or partial ·
✘ absent, cannot be written down

The named-but-unconcluded obligations are exactly `UnifyWF`, `UnifyAcyclic`,
`TerminalNoMgu` (RowUnify/), `InferSound`, `RunSound`, `KindsSound` (Infer.lean),
`InferSoundC`, `QTypedCDischarge`, `QScheme.ResWF` (InferSound.lean) and `SchemeImage`
(QSubst.lean) — each a `def … : Prop` that no theorem concludes.

## A. Unification ≐ / ≐ᵣ

- ✔ **success soundness** — `Sat θ s → Unifies θ ρ₁ ρ₂`
  - `unifyRowM_success_sound`
- ✔ **success completeness** — every unifier extends to one meeting `s`
  - `unifyRowM_success_complete`
- ✔ **mgu on success** — the two combined
  - `unifyRowM_success_iff`
- ✔ **clash soundness** — clash ⟹ no unifier
  - `unifyM_clash_no_unifier`
- ✔ **occurs soundness** — at both sorts, where the guard is LOCAL
  - `solveVarM_occurs_no_unifier`, `bindTy_occurs_no_unifier`
  - the all-variable occurrence is not a failure but the ε-collapse rule
    (`allvar_occurs_mgu`), so what is rejected is deep or field-pinned, and both
    are genuine
  - only the `depReach Θ` stale-binding disjunct is left, and it is a
    solver-state fact, not a problem fact — `solveVarM_occurs_no_unifier_nil` is
    the unconditional Θ = [] case
- ✘ **stuck ⟹ no mgu** — FALSE
  - `stuck_masks_mgu` — and it alone. `terminalNoMgu_false` is DELETED
    (Refutations.lean:327); see the `TerminalNoMgu` entry below
  - nothing to fix at the statement level: the converse does not exist
  - what IS stateable is the *disjunctive* form — the three no-mgu techniques
    plus the three conservativity witnesses.
- ◐ **`TerminalNoMgu`** — a terminal configuration has no mgu — stated,
  `Defs.lean:926`
  - the retreat position for the fourth leg, and the statement step 3 of the
    base-arm dispatch would conclude
  - OPEN, not refuted: `terminalNoMgu_false` is deleted, because U-expand went
    two-ended — `expandR` hosts in `v`, so its counterexample
    `(l:{w}) ≐ᵣ (w | v)` is no longer terminal and the driver solves it
    (`terminal_masks_mgu_not_terminal`, `terminal_masks_mgu_now_solved`)
  - nor is it thereby proved: terminality is a fact about the MOVES, not about
    the problem, and another configuration where the guards miss a forced
    placement is not ruled out. **Cite it as neither** — Refutations.lean:230,
    Axioms.lean:288 and Trichotomy.lean:170 all say so
  - `terminal_masks_mgu` survives with a changed job: no longer a conservativity
    witness but the CORRECTNESS witness for the right-end arm
- ✔ **fuel independence**
  - `unifyM_fuel_mono`, `unifyM_bounded`
- ✘ **termination / totality** — `∃ fuel. result ≠ outOfFuel`
  - missing: a *well-founded measure*. Rémy's does not close — renaming adds no
    fields, so the host keeps `count_l = 0`
  - without this `unifyRow` is not a *function*, and every inference theorem
    inherits the fuel parameter
  - **Stage 0 is not a route out**: with the expansion arms stubbed the fuel
    profile is unchanged (`fuelGain = 0`, every parametric family flat), so
    removing U-expand buys nothing here
- ✘ **no duplicate keys on `Sol`** — and FALSE. *The keystone.*
  - `Sol.ty` / `Sol.row` are bare association lists with no invariant, and two
    readers disagree: `Sol.toSubst` goes through `tyLookup`/`rowLookup`, which
    are FIRST-MATCH-WINS, while `Sol.Sat` quantifies `∀ p ∈ s.row` over every
    pair
  - live two-atom witness:
    `(b | a) ≐ᵣ (l:{a} | b)  ⟹  [aa≔{a} ; b≔l:aa | aaa, b≔aaa | a | ε]` —
    `b` bound twice, to values that disagree
  - cause: `expandL` applies `renameVar β β′` to the HOST side only
    (Defs.lean:414), so the `β` in the field side's residual survives and
    U-var-solve binds it again; `Sol.comp` is a plain append with no dedup
  - fix is either a driver invariant (have `expandL` apply β's binding to the
    residual, or `comp` drop a shadowed key) or restating `Sat`/`Ranked`
    through `toSubst`. Or remove the expand mechanism.
- ◐ **well-formed returned solution** (`UnifyWF` = `Acyclic` ∧ `Ranked`) —
  stated, `State.lean:974`
  - the vocabulary is complete: `Sol.Acyclic`, `Sol.Ranked`, `Sol.Closes`,
    `Sol.WF`, `Sol.closes_of_wf`
  - what is missing is the proof, and the cheap routes are **ruled out by
    measurement**: six rank candidates refuted (list position both directions,
    name length, DepGraph reachability, DepGraph depth, creation order both
    intra-expansion orders), and the `Sol.comp` preservation step is false as
    stated
  - the blocker underneath is the duplicate-key entry above — `Ranked`
    quantifies over ALL bindings, so a dead shadowed binding must also be
    rank-decreasing. **Decide duplicate keys before looking for a rank again**
  - empirically 0/0/0 in all three universes since B1

- ◐ **`UnifyAcyclic`** — the SPINE half alone — stated, `State.lean:970`
  - strictly cheaper, and already buys what inference needs
  - two algebraic steps DONE: `sVarSeq_applySubst`, `Sol.acyclic_comp`
  - remains: no "values avoid the domain" invariant over `Ty.allRowVars`
    survives — three swept, all ✘, and false BY DESIGN, since a triangular
    solution legitimately holds bound variables under record constructors. A
    different induction is needed

- ✘ **non-vacuity of success** — `success s _ → ∃θ. Sat θ s`
  - not `UnifyWF`. Both mgu legs are vacuously true when `s` is unsatisfiable
    AND the problem has no unifier, so the pair does not exclude a success on an
    unsolvable input. Nothing proved is false; the pair is under-specified
  - route: carry `∃θ. Sat θ s` as a THIRD conjunct of the mutual success
    induction
  - fallback: `solRankedB`/`peelDeps` (Fuzz.lean) already DECIDES it — promote
    it into the driver as a final gate
  - sub-obligation is the duplicate-key entry

- ◐ **unification preserves `RowWF`** — reduces to `UnifyAcyclic`
  - no longer a missing statement: `Sol.rowWF_toCtx` derives `RowWF` from
    `Sol.Acyclic`, and `Sol.lookup_total_toCtx` feeds `lookup_total`, so
    `A-sel`'s premise is guaranteed to HAVE a derivation as soon as
    `UnifyAcyclic` lands
  - the old pointer to a comment at `minimal.lean:2279` is dead — that line is
    `RowPrec.cat_inv` now

- ✔ **refinement order on solutions `S ≤ S′`**
  - `SolverState.SatMono`, `Infer.sat_mono`
  - stated SEMANTICALLY ("any σ satisfying the later solution satisfies the
    earlier"), so it is transitive on the nose and needs no associativity of
    `Sol.comp`
  - this is what lets a conclusion sit at the FINAL state while premises were
    solved at intermediate ones

- ✔ **sorted occurs-check** — both halves
  - `Row.allRowVars` guards the row sort, `Ty.tyVars` the type sort; `bindTy` no
    longer reads the sort-blind `τ.ftv`
  - `x ≐ {x}` succeeds; `x ≐ (x→x)` and `x ≐ {l:x}` report occurs

- ✔ **what U-expand is worth** (Stage 0, 2026-09-22) — measured
  - not a gap: a priced design decision
  - a clone with the four expansion arms stubbed to `.stuck` agrees with the
    driver on 98.3 / 92.4 / 97.5% of 771 578 pairs; verdict-monotonicity has
    **0** counterexamples, so removal weakens no theorem — all four legs are
    *verdict reached ⟹ property*
  - `Sol.Applied` ⟺ no expansion fired, **exactly** (0/0/0 both directions), so
    triangularity is entirely an artefact of the arm, and without it
    `Sol.Ranked` is not merely provable but *unnecessary*
  - cost: 7.0 / 30.5 / 9.1% of successes become stuck, and every one of them
    routes to `A-app-degrade` → ★, which has no declarative counterpart (§C)
  - **decide this jointly with the degradation entry, not before it**

## B. Inference `Γ; S ⊢ e ⇒ τ; S′`

- ✔ **the judgement itself**
  - `Infer` / `InferRec` (Infer.lean)
  - a RELATION, not a function: a function would owe unification's termination,
    `A-let`'s fixpoint and the `↝*` closure, none of which exist
  - non-vacuity pinned by `selEx_infers`, `fStarEx_infers`

- ✔ **`⟦S⟧` as a context**
  - `Sol.toCtx`, `Sol.lookup_toCtx_iff`, `SolverState.applyCtx`
  - the bridge is conditioned on `Sol.WF`, so it waits on `UnifyWF`; `⟦S⟧` in
    the A-rules is currently `Sol.toSubst` (one step), which agrees with the
    closure on a well-formed state
  - `Sol.lookup_toCtx_sat` transports DEFINITE lookups under mere `Sat`, which
    is what the induction actually has at an intermediate state

- ◐ **well-formedness of `S`** — one of four invariants proved
  - `SolverState.Quiescent` (every parked stump genuinely blocked on the blocker
    it records, read on the SUBSTITUTED row) is stated AND maintained:
    `Infer.quiescent` / `InferRec.quiescent` show **every reachable state is
    quiescent**, by mutual recursion over the rules
  - still missing: θ acyclic (`UnifyAcyclic`), δ's distinct (`QScheme.ResWF`
    below), W irrelevant to typing

- ✔ **name supply in the inference rules** — threaded and monotone
  - `SolverState.supply`/`draw`, with `SolveTy`/`SolveRow` calling
    `unifyTyF`/`unifySpineMF` on `S.supply` rather than the local one —
    `localSupply` hands back a supply BEHIND the state's and lets a later `draw`
    re-issue a live name
  - `Infer.supply_mono` and `Infer.kinds_mono` are proved
  - still missing: Γ-freshness

- ◐ **sorts of invented variables** — `KEnv`, `KEnv.Assigns`, `KindsSound` stated
  - `A-let` writes `κ̄ = Γ(ᾱ)` where ᾱ are exactly the variables *not* in Γ
  - the kind environment now exists and is threaded (`Infer.kinds_mono`);
    `KindsSound` is named, not proved

- ✔ **rules for non-success verdicts**
  - `Infer.appDeg`, `Infer.selDeg` + `SolveTyDegrades`
  - a clash has NO rule — that IS the hard error, sound by
    `unifyM_clash_no_unifier`
  - their declarative counterpart is the open problem, not their statement (§C)

- ✔ **parking a fresh constraint**
  - `Wakes.park`
  - the blocker is computed by exhibiting a `LookupBlocked` witness, which is
    exactly the missing `K-park`
  - `LookupBlocked` also supplies the paper's `? on α`, which `Lookup` does not
    record — proved sound, complete, deterministic

- ✔ **wake-up run to quiescence**
  - `Saturate`, `SolveTySat`, `WakesSat`
  - a RELATION with quiescence as a PREMISE, for the same reason `Infer` is one:
    a saturation *function* owes a termination measure and K-repark has none.
    "A quiescent state exists" therefore joins the open list
  - the A-rules now take the saturating wrappers, which is what makes
    `Infer.quiescent` true — before that change it was **false**
    (`fStarEx_stale_blocker`)

- ◐ **determinism of inference** — repaired where F-★ broke it
  - `Quiescent.wake_no_commit` and `Finalize.wake_no_commit`: at a quiescent
    state no wake-up step commits anything, so finalization is the only progress
    left at the end of a run. This is what `fStar_wake_star_disagree` had refuted
  - still ✘ for a whole run: stating it needs "unique up to renaming of invented
    names", i.e. an α-equivalence on (τ, S) outputs

- ✘ **termination of inference**
  - inherits unification termination, *plus* the `A-let` Δ-split least-fixpoint
    (asserted monotone and bounded by |Δ₁|, never stated), *plus* the `↝*`
    wake-up closure
  - the weakest leg in the development: the only `termination_by` anywhere is
    `minimal.lean:412`, unrelated

- ◐ **confluence of wake-up** — claimed
  - the two pillars exist (`lookup_det`, `Discharge.mono_of_definite`) and the
    no-commit lemmas above cover the quiescent end
  - "final (θ,W,τ) is independent of wake-up order" is still not written

- ✔ **top-level entry point**
  - `Run`, `Finalizes`, `RunSound` (Infer.lean)
  - infer from the empty state, then finalize what is parked — `algorithmic.typ`
    left the `⇓*` closure implicit
  - `RunSound` is what `InferSound` becomes once something supplies its
    `parked = []` hypothesis: named, not proved
  - non-vacuous on both shapes — `selEx_runs` (F-★ fires) and `fStarEx_runs`
    (saturation left it nothing to do)

- ✔ **type substitution for L2**
  - `qtyped_applySubst` (QSubst.lean): all 13 `QTyped` constructors + the 3 body
    ones, modulo one named hypothesis `SchemeImage`
  - **sharpened, and the news is bad for the obvious route**: the FORWARD half
    of `QCovers` is proved (`QCovers.forward_of_avoiding`, which is what `qVar`
    consumes), but the BACKWARD half is REFUTED for the pushed-through scheme
    (`qcovers_backward_false_for_applySubst`) — `applySubst σ` is not surjective
    while an instance set is as large as its binders allow
  - not capture, so no freshness discipline fixes it: a `SchemeImage` proof must
    produce a DIFFERENT scheme. Do not retry `σ₀.applySubst σ`
  - **cheap probe first**: check whether A-let needs backward at all, or can be
    restated forward-only

- ◐ **`QScheme.ResWF`** — well-formedness of a scheme's constraints — stated,
  `InferSound.lean:438`
  - `QScheme` requires neither pairwise-distinct result variables nor that each
    δ be BOUND
  - a missing invariant, not a live bug — the algorithm only builds schemes from
    stumps drawn fresh (A-sel-?) or renamed apart (A-var) — but the type permits
    a violating one, and then wake-up silently DROPS a constraint, since every
    rule that retires a stump filters on `stump.res`
  - belongs with `UnifyWF` in the "true of the states inference builds,
    unproved" family

- ◐ **non-vacuity of qualified schemes** — rule-level basis now exists, general
  claim **false**
  - `T-let`'s inhabitation premise was claimed to hold "by construction" (a
    carried stump finalizes at ★). F-★ now carries its `LookupBlocked` premise,
    so the claim has a rule to rest on
  - but see the spent-promise entry: it is false in general

- ✘ **the spent promise** — unstateable, and it is an INCOMPLETENESS
  - if δ is already solved to a non-★ type when finalization reaches the stump
    (A-app writes into δ whenever the selection's result is USED), `δ ≐ ★`
    clashes and F-★ has no derivation. Now a stated verdict — `Ty.Spent`,
    `no_finalize_of_spent` — rather than a stuck derivation
  - **the earlier reading of this was wrong**: `λx. λy. (x.l) y` IS typeable, at
    `{(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫` (`spentEx_declarative`), while the run is quiescent,
    otherwise sound, and cannot finalize (`spentEx_infers`,
    `spentEx_cannot_finalize`)
  - so it is the ALGORITHM's incompleteness, not the declarative system's
    rejection, and not §D's ★-elimination gap either — no ★ is ever formed
  - root cause: `Stump.res : TyVar`, so carrying `⟨r.l ↓ (α_y → β)⟩` cannot be
    written
  - three exits: leave it a hard error and write up the limitation (what the
    rules do now); `Stump.res : Ty B` (touches `Stump`, `Discharge`,
    `QScheme.WF`, `selQ` and every principality theorem on them); or the `τ ~ ★`
    consistency relation (§D)
  - **A-let inherits it either way** — `QScheme.WF` wants each `res` among the
    binders, and a spent δ is not one

### B′. Inference soundness, as it now factors

`RunSound = InferSoundC ∘ Finalize.dischargeEquiv ∘ QTypedCDischarge`. The middle
factor is **proved**; the outer two are statements.

- ✔ **per-rule soundness, 12 of the 15 `Infer`/`InferRec` constructors**
  - `infer_sound_{con,lam,rcd,app,conc,sel,selAbs,selUnk,var}_step`
  - `inferRec_sound_{empty,field,cat}_step`

- ✘ **A-let** — blocked on `SchemeImage` + the Δ-split

- ✘ **A-app-degrade, A-sel-degrade** — blocked on a declarative rule that does
  not exist (§C)

- ✔ **the K-/D- correspondence**
  - `Wake.dischargeEquiv`, `Wakes.dischargeEquiv`, `InstStumps.pairwise`

- ✔ **a parked stump as a typing HYPOTHESIS**
  - `QTypedC`, `QTyped.toC`, `QTypedC.toQTyped`, `Parked.toStumpC`

- ✔ **finalization discharges its stump**
  - `Finalize.dischargeEquiv`, `lookup_unknown_of_blocked` — on the nose, not up
    to ≈
  - one named side condition `hfix` (σ may not refine the blocked row), which is
    why finalization runs last

- ◐ **`InferSoundC`** — soundness without the `parked = []` hypothesis — stated,
  `InferSound.lean:1194`

- ◐ **`QTypedCDischarge`** — a constrained typing whose assumptions discharge is
  a plain one — stated, `InferSound.lean:1213`
  - a module's worth of transport along χ; the honest edge

- ✔ **the join, where it is free**
  - `runSound_of_inferSoundC_nil` — a run that left nothing parked needs only
    the induction

## C. Principality

- ✔ **instantiation `σ ≥_Γ τ`**
  - `QScheme.Inst`

- ✔ **covering order `⊴` on qualified schemes**
  - `QScheme.Covered` / `PrecCovered` / `BelowCoveredAt` (Qualified.lean)
  - *was the flagged #1 open; closed*
  - Γ-relative and uniform versions, both PREORDERS — not partial orders, since
    α-renaming gives ⊴-equivalent distinct schemes
  - `⊴[Γ]` is NOT stable under Γ ⊑ Γ′ (`covered_not_rowExt_stable`), which is
    what the uniform ⊴ is for
  - `⊴` is blind to vacuity (`coveredAt_of_uninhabited`), which is why
    `Principal` carries inhabitation as its own conjunct

- ◐ **constraint entailment `Q ⊨ q`** — discharged, not axiomatized
  - `⊴` in the qualified-types tradition is defined modulo an entailment
    relation. Here there is still no entailment between constraint SETS
  - but `QScheme.covered_of_witness` gives the certificate form (one θ + Jones'
    freshness + stump entailment), and `selQ_covers_selQb` discharges the
    stump-carrying case by RUNNING the lookup
  - that is the concrete answer to ROSE's "entailment is a parameter"

- ✔ **precision `⊑` on types** — paper + L1
  - `TyPrec.trans` proved (was only announced admissible)

- ✔ **`⊑` on schemes / the order principality is stated in** — `⊴⊑`, then `⊴≼`
  - the first attempt was **refuted**, which is a result: the typing set has TWO
    closure properties (T-★-intro and T-eq) while ⊑ is pure congruence, so
    `selQ_not_principalStrict` / `selQ_needs_equiv` kill ⊑-only covering
  - `≼ₜ` := ≈ then ⊑ (`TyBelow`) replaces it, transitive by `TyPrec.comm_equiv`
    (⊑ and ≈ COMMUTE, proved in both directions at once, axiom-free), so `⊴≼` is
    a preorder

- ✔ **principality for a single program** — the @contributions claim, complete
  - `no_plain_principal_scheme` (no plain ∀ᾱ.τ scheme is instance-closed while
    having both typings of `λx.x.l`) + `selQ_principal` + `selQ_greatest`
  - plain schemes refuted, the qualified one exhibited AND shown principal AND
    ⊴≼-greatest among all sound schemes
  - needed the L2 inversion `qsel_var_inv`: every typing of a selection on a
    monotype-bound x factors through ONE lookup. Axiom-clean
  - bookended by `l1_strictly_weaker` — **L1 ⊊ L2 is a theorem**, no longer prose

- ✘ **general principality** — `∀e ∃σ. Principal Γ e σ`
  - gated behind TERMINATION, not behind principality work
  - there is no W: `Infer` is a relation and cannot be a function until
    unification terminates, so "the scheme inference produces" has no subject
    and the covering conjunct cannot be stated about it

- ✘ **soundness of degradation**
  - NOT "replacing a position by ★ preserves declarative typeability" — that is
    about a term that already types
  - when `A-app-degrade` fires the application types *nowhere*: `qApp` wants a
    literal arrow, `qEq` only moves along ≈, and ≈ relates ★ to nothing but
    itself (`TyEquiv.unk_inv`), so `qUnk` cannot manufacture one
  - what is missing is a **declarative rule for application at ★** (the
    ★-elimination the failure policy assumes), or a proof that the rule is
    unreachable
  - **`A-sel-degrade` plausibly IS unreachable** — `r` is drawn fresh
    immediately before `τ ≐ {r}`, so that equation can only clash or succeed;
    proving it closes half this entry cheaply
  - `A-app-degrade` is not: its equation descends into an arbitrary row problem

- ✘ **⊑-monotonicity of inference**
  - "a more precise input context yields a more precise inferred type" — the
    algorithmic image of L1's lookup monotonicity
