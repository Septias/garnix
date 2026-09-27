# What is missing to STATE strong unification / inference theorems


Legend: ✔ mechanized (a theorem) · ◐ **stated in Lean, not proved** — or partial ·
✘ absent, cannot be written down

The named-but-unconcluded obligations are exactly `KindsSound` (Infer.lean),
`SchemeImage` (QSubst.lean) and `GeneralPrincipality` (OpenEnds.lean) — each a
`def … : Prop` that no theorem concludes. The named statements that are REFUTED
are kept as the names their refutations are about: `TerminalNoMgu`,
`InferSoundC`, `InstEquivCorrects`. `InferSound` and `RunSound` are PROVED
(`inferSound`, `runSound`, 2026-09-26).

## A. Unification ≐ / ≐ᵣ

- ✔ **success soundness** — `Sat θ s → Unifies θ ρ₁ ρ₂`
  - `unifyRowM_success_sound`
- ✔ **success completeness** — every unifier extends to one meeting `s`
  - `unifyRowM_success_complete`
- ✔ **mgu on success** — the two combined
  - `unifyRowM_success_iff`
- ✔ **clash soundness** — clash ⟹ no unifier
  - `unifyM_clash_no_unifier`
- ✔ **occurs soundness** — at both sorts, and through the WHOLE DRIVER
  - local: `solveVarM_occurs_no_unifier`, `bindTy_occurs_no_unifier`
  - lifted (2026-09-26): `unifyM_occurs_no_unifier`,
    `unifyRowM_occurs_no_unifier`, `unifyTyM_occurs_no_unifier`
    (RowUnify/OccursLift.lean) — forced moves carry a unifier to the residual,
    and success completeness carries it past a solved first stage
  - the all-variable occurrence is not a failure but the ε-collapse rule
    (`allvar_occurs_mgu`), so what is rejected is deep or field-pinned, and both
    are genuine
  - the `depReach Θ` stale-binding disjunct went with U-expand, which is what
    made the lift a plain induction
- ✘ **stuck ⟹ no mgu** — FALSE
  - `stuck_masks_mgu` — and it alone. `terminalNoMgu_false` is DELETED
    (Refutations.lean:327); see the `TerminalNoMgu` entry below
  - nothing to fix at the statement level: the converse does not exist
  - what IS stateable is the *disjunctive* form — the three no-mgu techniques
    plus the three conservativity witnesses.
- ✘ **`TerminalNoMgu`** — a terminal configuration has no mgu — REFUTED
  - since U-expand is removed (2026-09-23), `(l:{w}) ≐ᵣ (w | v)` is terminal
    again (`terminal_masks_mgu_terminal`) while having a unique unifier, and
    `terminalNoMgu_false` is restored
  - with `stuck_masks_mgu`, both candidate converses are negative: the fourth
    leg is the specific no-mgu theorems plus the conservativity witnesses
- ✔ **fuel independence**
  - `unifyM_fuel_mono`, `unifyM_bounded`
- ✔ **termination / totality** — `unifyRowM_terminates`, `unifyTyM_terminates`
  (RowUnify/Termination.lean, 2026-09-26)
  - measure: (problem variables counted inside a fixed universe, problem size),
    lexicographic. Forced moves shrink the size over no new variable; a solved
    stage with a binding loses that key (`Sol.Good.clears`), however much
    `sApplySubst` grows the spine; a keyless stage is the identity
  - `unifyRow` is the total function, and `unifyRow_eq` says every run that
    answers agrees with it. No closed-form fuel bound — the fuel is chosen
- ✔ **consistent keys on `Sol`** — PROVED 2026-09-26 as part of `Sol.Good`
  (`RowUnify/Applied.lean`): two bindings of one key agree, so `toSubst`
  (first match) and `Sat` (all pairs) cannot disagree. The old witness went
  through U-expand's host-only rename and is gone with it

- ✔ **well-formed returned solution** (`UnifyWF`) — `unifyWF`
  - via `Sol.Good`: every success is `Applied`, so `Ranked` holds at rank ≡ 0
    and no rank search is needed. The refuted rank candidates were all
    artefacts of U-expand's triangular solutions
- ✔ **`UnifyAcyclic`** — `unifyAcyclic`, a corollary
- ✔ **non-vacuity of success** — `unifyRowM_success_sat`,
  `unifyRowM_success_unifies`, `unifyRowM_success_mgu`: `s.toSubst` satisfies
  `s` and unifies the problem, so a success means the problem is solvable

- ✔ **unification preserves `RowWF`** — via `unifyAcyclic` and `Sol.rowWF_toCtx`
  - no longer a missing statement: `Sol.rowWF_toCtx` derives `RowWF` from
    `Sol.Acyclic`, and `Sol.lookup_total_toCtx` feeds `lookup_total`, so
    `A-sel`'s premise is guaranteed to HAVE a derivation as soon as
    `UnifyAcyclic` lands — which it now has
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
    used to route to `A-app-degrade` → ★; since the degradation rules are
    deleted they are now rejected
  - **decided 2026-09-23/26**: U-expand removed, degradation rules removed

## B. Inference `Γ; S ⊢ e ⇒ τ; S′`

- ✔ **the judgement itself**
  - `Infer` / `InferRec` (Infer.lean)
  - a RELATION, not a function: a function would owe unification's termination
    (now PROVED, `unifyRowM_terminates`), `A-let`'s fixpoint and the `↝*`
    closure (both still absent)
  - non-vacuity pinned by `selEx_infers`, `fStarEx_infers`

- ✔ **`⟦S⟧` as a context**
  - `Sol.toCtx`, `Sol.lookup_toCtx_iff`, `SolverState.applyCtx`
  - the bridge is conditioned on `Sol.WF`, which `SolveTy.clean` /
    `SolveRow.clean` now maintain across solved equations; `⟦S⟧` in
    the A-rules is currently `Sol.toSubst` (one step), which agrees with the
    closure on a well-formed state
  - `Sol.lookup_toCtx_sat` transports DEFINITE lookups under mere `Sat`, which
    is what the induction actually has at an intermediate state

- ✔ **well-formedness of `S`** — the invariants the soundness proof needs, all proved
  - `SolverState.Quiescent` (every parked stump genuinely blocked on the blocker
    it records, read on the SUBSTITUTED row) is stated AND maintained:
    `Infer.quiescent` / `InferRec.quiescent` show **every reachable state is
    quiescent**, by mutual recursion over the rules
  - θ clean (hence acyclic, applied) over the whole derivation (`Infer.clean`,
    `Finalizes.clean`, Absorb.lean), and every derivation extends its start
    (`Infer.ext`)
  - `PInv` (ParkedInv.lean): parked result variables are issued by the supply
    and there is one stump per result variable (`Infer.pinv_keeps`)
  - W (the flags) plays no part in typing, and nothing needs it to

- ✔ **name supply in the inference rules** — threaded and monotone
  - `SolverState.supply`/`draw`, with `SolveTy`/`SolveRow` calling
    `unifyTyF`/`unifySpineMF` on `S.supply` rather than the local one —
    `localSupply` hands back a supply BEHIND the state's and lets a later `draw`
    re-issue a live name
  - `Infer.supply_mono` and `Infer.kinds_mono` are proved
  - Γ-freshness is no longer missing: A-var draws its renaming from the supply
    (`Infer.var`'s range premise), and `nameReuse_*` (FreshNames.lean) is the
    witness that the old unreserved names broke `PInv`

- ◐ **sorts of invented variables** — `KEnv`, `KEnv.Assigns`, `KindsSound` stated
  - `A-let` writes `κ̄ = Γ(ᾱ)` where ᾱ are exactly the variables *not* in Γ
  - the kind environment now exists and is threaded (`Infer.kinds_mono`);
    `KindsSound` is named, not proved

- ✔ **rules for non-success verdicts** — there are none, by decision
  - clash, stuck and occurs all have NO rule: the program is rejected
  - clash: sound by `unifyM_clash_no_unifier`
  - stuck/occurs: `Infer.appDeg` / `Infer.selDeg` / `SolveTyDegrades` are
    DELETED (2026-09-26). They degraded to ★, and ★ is rigid, so there was no
    declarative rule for them to be sound against. Rejection is sound by
    construction and costs only completeness

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

- ◐ **termination of inference** — both closures terminate; the statement
  awaits a function
  - unification: `unifyRowM_terminates` / `unifyTyM_terminates`
  - the `↝*` wake-up closure: `satStep_wf` (OpenEnds.lean) — no infinite
    saturation run from ANY state, measure (|Δ|, #unblocked) lexicographic
  - what remains is not a proof but a definition: `Infer` is a relation, so
    "terminates" needs `infer : … → Option (Ty × SolverState)`. Its one real
    decision is A-let's ᾱ / Δ-split; whether a greatest admissible ᾱ exists
    (premises closed under union) is open, and the correctability premise is
    the suspect. OpenEnds.lean §2
  - finding: `Infer.letE` asks `S₁.parked = Δq ++ Δγ`, a PREFIX split, not a
    partition — a completeness cost; the fix is `List.Perm`

- ◐ **confluence of wake-up** — claimed
  - the two pillars exist (`lookup_det`, `Discharge.mono_of_definite`) and the
    no-commit lemmas above cover the quiescent end
  - "final (θ,W,τ) is independent of wake-up order" is still not written

- ✔ **top-level entry point**
  - `Run`, `Finalizes`, `RunSound` (Infer.lean)
  - infer from the empty state, then finalize what is parked — `algorithmic.typ`
    left the `⇓*` closure implicit
  - `RunSound` is PROVED (`runSound`, Finalization.lean). `Run` no longer asks
    `S′.parked = []` — the proof never used it — so the theorem got stronger
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

- ✔ **well-formed schemes** — `QScheme.Correctable` (InferSoundA.lean)
  - bound result variables, one constraint per result variable, and no
    constraint row mentioning a result variable; A-let builds only such
    schemes (`letCase`), and they are exactly what the χ-correction needs
    (`QScheme.Correctable.correct`)
  - replaces `QScheme.ResWF` / `InstStumps.pairwise`, deleted 2026-09-26
  - the general correction is FALSE (`instEquivCorrects_false`)

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

### B′. Inference soundness — PROVED

`runSound : RunSound` (Finalization.lean), axioms [propext, Classical.choice,
Quot.sound]. The chain:

- ✔ **the restated statement** — `InferSound` (InferSoundA.lean), proved as
  `inferSound` (LetCase.lean)
  - Γ and τ read under the SAME σ ⊨ S′ (`CtxRead`); the first form read Γ at
    ⟦S′⟧ and τ at σ and is false (`inferSoundC_false`)
  - parked stumps are TYPED assumptions (`QTypedA`), not promises;
    `QTypedA.weaken` drops the ones that hold
- ✔ **A-var** — `varCase`, via `Wakes.fate` + `KeepsS`
- ✔ **A-let** — `letCase`; it needed five new premises on `Infer.letE`, each
  with a witness of what it costs to drop (`runSound_false_unguarded_let`,
  `runSound_false_let_captures`, LetSound.lean)
- ✔ **finalization discharges** — `Finalizes.holds`: F-★ is D-?, with the old
  `hfix` side condition PROVED at ⟦S′⟧ (the blocker stays free)
- ✔ **the χ-correction** — `QScheme.Correctable.correct`; false in general
  (`instEquivCorrects_false`), so D-hit stays exact and A-let builds only
  correctable schemes
- removed as superseded: `QTypedCDischarge`, `runSound_of_inferSoundC_nil`,
  `Finalize.dischargeEquiv`, `Finalize.discharge_isUnk`,
  `lookup_unknown_of_blocked`, `inferC_sound_selUnk_step`, the old `InferSound`

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

- ◐ **general principality** — `GeneralPrincipality` (OpenEnds.lean),
  `∀Γ e. (∃τ. Γ ⊢ e : τ) → ∃σ. Principal Γ e σ` — stated, not proved
  - both closures now terminate (`unifyRowM_terminates`, `satStep_wf`), so the
    gate is no longer termination but the A-let function (see B, termination)
  - the covering conjunct is algorithmic COMPLETENESS, which is known false
    three ways: the spent promise, the stuck verdict, A-let's premises. So it
    can only hold for a fragment; which one is a thesis decision

- ✘ **⊑-monotonicity of inference**
  - "a more precise input context yields a more precise inferred type" — the
    algorithmic image of L1's lookup monotonicity
