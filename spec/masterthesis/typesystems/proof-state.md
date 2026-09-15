
> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative typesystem and extend it to an algorithmic one in a similar fashion.

Our contribution is a _lookup relation_ that tries to solve one motivating example: `a: b: (a || b).l` which is a lookup on a concatenation of two row-variables that can not be typed easily. This wand-example is actually unsolvable, even with our effort. The novelty of our approach is to lookup a type on a _best-effort_ basis and give back an unkown result ★ in the wand-example. Our lookup relation thus returns a result out of (τ | ⊥ | ?) where ⊥ symbolises definite absence of a field and ? means "we don't know" (yet). Our lookup relation `Γ ⊢ ρ.l ↓ r` is able to lookup row-variables in the context that were instantiated on application. This can also be done with normal substitution of type-variables, but already shows the algorithmic implementation.

This mechanism allows to _refine_ types on function application. See the example `x: ({l: τ} || x).l` of type `{β} → ★` since the lookup-relation can not look past the type-variable introduced by x. Only after instantiation, it becomes clear whether the label is _shadowed_ or not. Applying the argument `x = {}` promotes the unknown type ★ to τ because it becomes clear that x does not shadow the label defined in the literal record.

The type-safety proofs have to account for this new lookup-mechanism in two ways: Progress can only be proven for definite types, but ★ forms a boundary where programs can get stuck. The preservation proof has to account for type refinement by allowing types to become more precise during small steps.

Principality forces qualified schemes that use parked stumps during unification to get mgus in many cases. The algorithm outputs three solution: A sucees with MGU, a failure without MGU, stuck for the wand example and finally an outcome »occurs«, that cuts across the other output paths. The occurs class of outputs is a syntactic check for recursive row-variables that naturally occur in nix.


## Related Files
- minimal.typ: provides a semi-formal method of a simpliefed typesystem (L1)
- minimal.lean: provides a fully formal version of minimal.typ
- algorithmic.typ: Algorithmic typesystem with qualified schemes
- algorithmic.lean: Root of the formal algorithmic system with unification
- In the bib/plaintext folder there is the plaintext version of relevant literature

# Progress
- [x] Scoped Records
- [x] Asymmetric Concat
- [x] Row Equivalence ≈
- [x] Refinement ⊑
- [x] Unknown Type Abstraction
- [x] Let-Statements
- [x] Qualified Schemes
- [~] Unification
- [~] Type Inference  (judgement MECHANIZED 2026-09-14, lean/Infer.lean)
- [~] Mathematical Properties
- [ ] FC-Labels
- [ ] Negative type information
- [?] Patterns
- [?] Occurrence Typing
- [?] Recursive Types
- [?] With
- [?] Inherit

# Tisch
> Things that are still open and not refuted, but stalled for now
- Deferral of row unification

# Current Notes
> Notes about the current state


## Unification
- Outcomes
  - [~] success: *sound & complete*
    - VACUOUS successes: none left in any fuzz universe, either half, since the
      guards read the accumulated solution (B1). `UnifyWF` is still UNPROVED —
      empirically clean is not a theorem.
  - [¡] occurs: *incomplete*
    - α ≐ᵣ (β|α|γ)
  - [~] stuck : *incomplete*
    - (k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫)
    - (l:{w}) ≐ᵣ (w | v) — NO LONGER STUCK, expandR solves it
  
- Open
  - [x] the expandR driver arm — DONE (RowUnify/ExpandR.lean, 2026-09-13).
    `expandR` existed since the self-reference filter and was never called; the
    driver solved (l:𝓫|α) ≐ᵣ (m:𝓫|β) and went STUCK on the mirror
    (α|l:𝓫) ≐ᵣ (β|m:𝓫). Two arms added AFTER the projClash test (projClash is a
    sound no-unifier test, so a success reached past it would be vacuous — the
    ordering makes the change monotone on reached verdicts).
    NOT a transport: `revRow` transports the SUBSTITUTION-FREE lemmas (stripR,
    matchR), but `revRow (ρ.applySubst θ) ≠ (revRow ρ).applySubst θ`, so
    `expand_shift_R` and `host_forced_R` are genuine mirror proofs. The mirror of
    `spine_extract` reads the l-field off segment index |vars| ("no variable
    FOLLOWS it") instead of index 0; `ProjEquiv.reverse` turns "last entry" into
    a head so the inductions stay left-to-right.
    All four legs re-proved with the new arms: success soundness, success
    completeness, boundedness, clash soundness, fuel monotonicity.
    PAYOFF: `terminal_masks_mgu`'s configuration (l:{w}) ≐ᵣ (w|v) is no longer
    terminal — expandR hosts in v — and the driver now returns exactly the mgu
    that section builds by hand (Regressions.unify_terminal_masks_mgu_solved).
    So `terminalNoMgu_false` is DELETED and `TerminalNoMgu` is OPEN: unrefuted
    and unproven. Do not cite it either way. `stuck_masks_mgu` is untouched —
    that one is about UResM.seq, so stuck ⟹ ¬mgu stays FALSE.
    SWEEP (all three universes, cap 64, vs. pristine HEAD baseline):
      stuck    2928/258564/4552 → 2664/243356/4352   (≈15k resolved in `deep`)
      success 13493/ 83430/2917 → 13677/ 94022/2989
      clash   unchanged in all three; divergence candidates still 0
    REGRESSION, measured and NOT yet fixed — see the uniqueHost item below.

  - [x] `uniqueHost` / the occurs guards need the SOLVER STATE — DONE
    (2026-09-13, "B1"). The guards were LOCAL: they compared a variable with the
    spine or payload AS WRITTEN. That is enough for every arm that SOLVES AND
    APPLIES (matchL/R, groundMatch, the ≐ congruences) because those push their
    solution into the residual. U-expand is the exception: it RENAMES the host
    (β ↦ β′) instead of applying β ≔ (l:δ | β′), and `renameVar` touches SPINE
    variables only — so a payload mentioning β still reads β after the move
    while β is already bound, and a later guard misses a cycle that exists only
    in the transitive closure.
    THE FIX: `DepGraph` (Defs.lean) — the accumulated EXPANSIONS as an edge
    list, threaded through `unifyTyF`/`unifySpineMF` and read by
      * `uniqueHost`: the HOST condition becomes β ∉ depReach Θ (allRowVars τ).
        The `rest` condition is NOT widened the same way — those variables are
        excused by `selfref_no_l_field`, which needs the genuine occurrence in
        τ, so widening there would be UNSOUND.
      * `solveVarM`: α ≔ ofSpine s₂ is a cycle as soon as α is REACHABLE from
        s₂, not only when it occurs in it. This is the guard the spine-level
        cycle ran through — `uniqueHost` alone does not fix it.
    Only expansions accumulate; every other arm's solution is already in its
    residual. `depReach` is inflationary, so the guards only ever reject MORE:
    `uniqueHost_spec` still yields the same `HostShape`, and success soundness /
    completeness / clash soundness went through unchanged.
    COST, and it is not incidental: the guards run inside kernel-checked `rfl`
    regressions and `TyVar = String`. The obvious frontier-plus-`eraseDups`
    closure made ONE regression take 76s to reduce. `depReach` is a marked set
    kept deduped by construction, with a `[]` fast path for the (common) case
    where no expansion has happened.
    SWEEP — ill-formed solutions, BOTH halves, all three universes:
      baseline (HEAD)   Acyclic 0/  0/ 0    Ranked 0/576/ 8
      + expandR         Acyclic 0/ 32/ 0    Ranked 0/1176/16
      + B1              Acyclic 0/  0/ 0    Ranked 0/  0/ 0     ← BELOW baseline
    clash counts unchanged throughout; divergence candidates still 0.
    So `UnifyWF` (State.lean:810) now has NO counterexample in any universe, in
    either half — the original tripwire finding is closed too, not just the
    regression expandR introduced. Witnesses are pinned in Regressions.lean
    (`vacuous_success_payload_cycle` → stuck, `vacuous_success_spine_cycle` →
    occurs, the latter being the RIGHT answer: that problem has no unifier).
    WHAT IT COST ELSEWHERE: `NoHost`'s third disjunct is widened to
    `β ∈ depReach Θ (allRowVars τ)`, which has two sources of different
    strength — the genuine self-reference (no unifier at all,
    `selfref_host_no_unifier`) and the stale-binding case (a fact about the
    solver state, from which no no-unifier theorem follows). Trichotomy's
    step-2 dispatch is correspondingly WEAKER and anything reading that
    disjunct must re-split. Recorded, not repaired.
  - [~] `UnifyAcyclic` (State.lean) — the SPINE half of `UnifyWF`, split out
    because it is cheaper and already buys what inference needs: `Acyclic` gives
    `rowWF_toCtx` and `lookup_total_toCtx`, i.e. `A-sel`'s premise is guaranteed
    to HAVE a derivation. (The θ ↦ rowEnv bridge needs `Closes` and still waits
    on `Ranked`.) THE TWO ALGEBRAIC STEPS ARE DONE (2026-09-15):
      * `sVarSeq_applySubst` — spine variables transform by FLATMAP under
        substitution. Payloads contribute nothing, so the cross-sort leak that
        refutes `NoCapture` (`a ≐ᵣ (l:a)`: `a` bound at the row sort, the
        payload `a` a TYPE variable) cannot reach this property. A spine
        position never holds a type variable.
      * `Sol.acyclic_comp` — COMPOSITION preserves it, given the later stage
        avoids the earlier stage's domain, which `Sol.AcyclicAvoiding` carries.
        This is precisely the step that defeats `Ranked`, and at spine
        positions it CLOSES: `sApplySubst` strips the earlier domain from the
        residual (by that stage's own acyclicity), and the expansions rename
        their host away instead.
    REMAINS: thread `AcyclicAvoiding V` through the driver (the arm-by-arm plan
    is in the `UnifyAcyclic` docstring). The one missing ingredient is a "spine
    variables of a `Ty`" measure, which the `.fn` arm needs because it recurses
    on `b.applySubst θ`.
  - [ ] PROVE `UnifyWF`. Empirically clean since B1 (0/0/0, both halves, three
    universes) and the gate on ⟦S⟧ being a total function — items 1 and 2 of
    plans/inference-gap-analysis.md's critical path. But empirically clean is
    not a theorem, and the cheap routes are now RULED OUT BY MEASUREMENT rather
    than by intuition. `Sol.Ranked` demands an explicit rank function; three
    natural candidates were swept over all three universes:
      * rank = LIST POSITION, bindings mention only LATER ones
          violations 1036 / 31688 / 324   ✘
      * rank = LIST POSITION, bindings mention only EARLIER ones
          violations    0 /   376 /   0   ✘ (clean in `wide`/`nest`, not `deep`)
      * rank = NAME LENGTH (supply birth order: later-drawn ranks lower)
          violations   40 /  2336 /  24   ✘
    Only "a binding never mentions the variable it binds" survives (0/0/0), and
    that is far too weak to build a rank from.
    WHY POSITION FAILS, concretely. List order IS creation order — `Sol.comp`
    puts the earlier stage first in both call sites (`UResM.seq` and
    `expandResM`). But `comp` also PUSHES the earlier stage's values through the
    later solution: `expandResM` emits `δ ≔ τ.applySubst s.toSubst` where `s` is
    the RECURSIVE result. So a binding created early can acquire a dependency on
    a variable created later, e.g.
        (l:{a} | a | l:𝓫) ≐ᵣ (m:𝓫 | b)
        ⟹ aa ≔ {m:aaaa | aaaaa}   with aa created BEFORE aaaa
    and the DAG order there is aaaa ≺ aa ≺ b — neither list order nor its
    reverse, nor name length (b is shorter than aa but must rank above it).
    So the rank is genuinely TOPOLOGICAL and the proof has to show the sorted
    dependency graph is acyclic from how the driver builds it. The handle is
    `Sol.ftvAt_rank` (State.lean:421), which already says one unfolding step
    drops the rank of every bound occurrence; what is missing is the
    `Sol.comp` preservation step, and that in turn needs to know what the later
    stage may mention of the earlier stage's domain — which the
    `Supply`/`Avoids`/`SolBelow` discipline constrains but does not yet pin
    down. Do not retry the three ranks above.
  - [ ] sorted ftv, second half. `Ty/Row.sortedFtv` (State.lean) and
    `Ty/Row.allRowVars` (Defs.lean) exist; `bindTy` still tests the sort-blind
    `τ.ftv`, which is the over-conservatism the thesis flags.
  - [x] ⊴ covering order on qualified schemes (Qualified.lean, THE COVERING ORDER)
  - [ ] solver state S = (θ, Δ, W), stump wake-up, confluence of the final state

## Termination
- Fuzzing suggests, that the algorithm actually terminates


## Inference  (lean/Infer.lean, 2026-09-14)
`Infer` / `InferRec` now EXIST in Lean — the first row of
plans/inference-gap-analysis.md §B, which every downstream statement was blocked
on. A RELATION, not a function: a function would owe three termination arguments
the development does not have (unification's, `A-let`'s Δ-split fixpoint, the
`↝*` closure). Determinism and totality become theorems ABOUT it instead of
assumptions inside it.

What the mechanization had to supply that the paper leaves as prose:
- *`? on α`*: `Lookup` records `.unknown` but not WHICH variable blocked, and
  both A-sel-? and K-repark need it. `LookupBlocked` refines the three
  unknown-producing rules with the blocker threaded through — proved sound
  (`toLookup`), complete (`Lookup.unknown_blocked`), deterministic
  (`LookupBlocked.det`), and to name an unsolved variable (`.unsolved`).
- *the failure policy*: "clash = hard error, stuck/occurs degrade to ★ + W" had
  no rules, so the algorithm was UNDEFINED there. A clash now has no rule at all
  — that IS the hard error, sound by `unifyM_clash_no_unifier` — and the
  degradations are explicit (`Infer.appDeg`, `Infer.selDeg`).
- *K-park*: `Wakes.park` computes a fresh constraint's initial blocker by
  exhibiting a `LookupBlocked` witness.

NON-VACUITY, which is what a relation is easy to get wrong: `selEx_infers`
derives λx. x.l end to end with a fully concrete final state — result `α → δ`,
one stump parked, blocked on the record's row variable `r`, writing into `δ`.
A-sel-? fires, not A-sel-⊥ and not a degradation, because `α ≐ {r}` binds α at
the TYPE sort and leaves `r` unsolved at the ROW sort. That is exactly the shape
`selQ = ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ` describes declaratively.

FOUND WHILE WRITING THE RULES: `unifyTyM` / `unifyRowM` start from a LOCAL
supply (`lenBound … + 1`, `localSupply`), not the threaded one. Used naively in
an A-rule it hands back a supply BEHIND the state's, and a later `draw`
re-issues a name inference is already using. `SolveTy`/`SolveRow` therefore go
through `unifyTyF`/`unifySpineMF` with `S.supply` threaded in and out.

- [ ] `InferSound` (Infer.lean) — `Γ; S ⊢ e ⇒ τ; S′ ⟹ ⟦S′⟧Γ ⊢ e : ⟦S′⟧τ`.
  STATED, not proved; the statement is the point, since it was unwriteable
  before. Needs: `UnifyWF` (so ⟦S′⟧ is the closure rather than one step), the
  parked stumps discharged (hence the `S′.parked = []` hypothesis, with F-★
  supplying it), capture-avoidance for `QScheme.applySubst`, and an L2
  type-substitution lemma (L1 has `typed_applySubst_aux`; `QTyped` has only
  term substitution).
- [x] supply monotonicity — DONE 2026-09-15. `unifyM_supply_mono`
  (Soundness.lean): a SUCCESSFUL unification never hands back a supply behind
  the one it was given — every arm either returns its supply (bindTy,
  solveVarM, allVarsEmpty, the ★/base arms), sequences two calls, or advances
  by two and recurses (the four expansions). Lifted through `SolveTy`/`SolveRow`
  /`Wake`/`Wakes`/`Finalize` to `Infer.supply_mono` / `InferRec.supply_mono` by
  mutual structural recursion. This is what "inferred variables are fresh"
  rests on; it is also the first real theorem ABOUT the relation, so it
  confirms the mutual induction over Infer/InferRec is workable.
- [ ] sorts. The paper draws `fresh α: κ`; `Supply` has no kinds, so the rules
  draw untyped names and `A-let`'s `κ̄ = Γ(ᾱ)` is still not expressible.

## Principality
- [x] covering order on schemes ⊴ — defined 2026-09-14, Qualified.lean
  - *⊴[Γ]* / *⊴*: Inst_Γ(σ) ⊆ Inst_Γ(σ'), at one Γ / at every Γ. Γ-relative because
    discharge reads the row-solutions; for plain schemes the two collapse (covered_toQ)
  - *⊴⊑*: the same up to precision — σ' answers each instance of σ with one ⊑ₜ-below it.
    THIS is the order principality needs, and the reason is a theorem, not taste:
    typings are blur-closed (T-★-intro) and instance sets are not (★ is rigid), so
    λx.x.l types at {(l: 𝓫)} → ★ while selQ has no such instance
    (selEx_blurred_typing + selQ_no_blurred_inst). "Instances = typings" is unreachable
  - both are PREORDERS, not partial orders: α-renaming gives ⊴-equivalent distinct schemes.
    Needed TyPrec.trans, which was only ever announced as admissible — now proved (minimal.lean)
  - *⊴ is blind to vacuity*: an uninhabited scheme sits below everything
    (coveredAt_of_uninhabited) — qLet's inhabitation premise from the order side, which is
    why Principal carries it as its own conjunct
  - *Principal Γ e σ* := instance-closed ∧ inhabited ∧ covers every typing up to ⊑.
    Sanity: a principal scheme is ⊴⊑-greatest among the sound schemes (Principal.greatest)
  - *⊴ acts on contexts*: swapping a bound scheme for a ⊴-larger one preserves typing
    (qtyped_cov / qtyped_bind_cov, the QCtx.Sub induction with ⊴ in place of equality).
    NB `let` is ANTITONE in the scheme it binds — qLet's premise quantifies over all
    instances — and monotone only in the context it types under
  - *syntactic witness*: one θ instantiating σ' to σ + Jones' freshness + stump entailment
    certifies ⊴ (QScheme.covered_of_witness). Plain schemes give back the textbook
    generic-instance rule (Scheme.covered_of_inst); selQ_covers_selQb is the stump-carrying
    case, where entailment is discharged by RUNNING the lookup — the concrete answer to
    ROSE's "entailment is a parameter"
  - *⊴[Γ] is NOT stable under Γ ⊑ Γ'* (covered_not_rowExt_stable): ∀δ.⟨α.l↓δ⟩⇒δ with α
    unsolved has instance set {★} and is covered by the monotype ★; solving α = (l: 𝓫)
    moves it to {𝓫} and the covering fails — for ⊴ AND ⊴⊑. This is what the uniform
    ⊴ (∀Γ) is for; Γ-relative generality claims are claims about that Γ's solutions
- CONJUNCT 3 WAS WRONG, and the attempt is what found it (2026-09-14):
  - the typing set has TWO closure properties — T-★-intro (blur) and T-eq (≈) — and ⊑ is
    pure congruence on rows, so it can sharpen a payload but never move a field past a unit
  - witness: `λx.x.l : {(l: {ε | m: 𝓫})} → {m: 𝓫}` (selEx_equiv_typing) is a typing with NO
    ⊑-answer among selQ's instances (selQ_no_prec_answer) — the domain forces the payload
    τ_p, the hit pins the result to the same τ_p, and τ_p would have to be ⊑-below a `cat`
    row and a `sing` row at once. Hence ¬PrincipalStrict selQ (selQ_not_principalStrict)
  - fix: *≼ₜ* := ≈ then ⊑ (TyBelow), and *⊴≼* covering. Principal now reads ≼; the same
    instance answers the witness via ≈ on the result (selQ_needs_equiv)
  - [x] ⊑ AND ≈ COMMUTE (TyPrec.comm_equiv / RowPrec.comm_equiv, 2026-09-15, axiom-FREE):
    a ⊑ τ, τ ≈ b ⟹ ∃a'. a ≈ a' ⊑ b. Proved in BOTH directions at once — ≈ has symm/trans
    as constructors, so the one-directional statement cannot survive its own induction.
    The content is that ⊑ never touches a LABEL, never merges and never deletes a field:
    assoc/unitL/unitR re-make the same move on the blurred row (unit needs RowPrec.empty_inv,
    ρ ⊑ᵣ ε ⟹ ρ = ε), and comm survives precisely because its l₁ ≠ l₂ is untouched by blur
  - [x] hence *≼ is transitive* (TyBelow.trans) and *⊴≼ is a PREORDER*
    (BelowCoveredAt.refl + .trans) — the order principality is stated in now composes
  - [ ] OPEN: `Principal selQ (λx.x.l)` in the ≼ form — conjuncts 1+2 done
    (selQ_sound_and_inhabited), 3 still needs the L2 inversion for λx.x.l (analogue of
    minimal.lean's sel_var_unk)


# Problems
> Problems found during mechanized proving and their proposed solutions

- [!] **AND THE NEW TRIPWIRE FINDING** (2026-09-12). With `Ranked` in place of
  `Applied`, ill-formed solutions fall from 860 / 22408 / 268 to
  **0 / 576 / 8** across the three universes, and the spine-cyclic half stays
  0 / 0 / 0. The `wide` universe is now completely clean. What survives in
  `deep`/`nest` is a genuine cycle THROUGH PAYLOADS, e.g. on
      (b | a)  ≐ᵣ  (l:{l:𝓫 | a} | l:{a | b})
  the solution contains `aaaa ≔ {ε | b}` (type sort) together with
  `b ≔ (l:aa | l:aaaa | ε)` (row sort) — so ⟦S⟧ at `aaaa` never terminates, and
  by rcdDepth no θ satisfies both bindings: the success is VACUOUS and the
  problem has no unifier. The self-reference filter cannot see it: the second field is
  hosted in `aaa`, a fresh tail that is part of `b` by an EARLIER binding, and
  `uniqueHost` only compares the payload with the host variable itself — the
  accumulated solution is never threaded back into the spines. Fixing it means
  filtering against the transitive closure of the solution, i.e. giving the
  detector access to the solver state. Recorded, not attempted.
- [!] the four parked hypotheses are also false in their own right.
  Each has the shape `∀ Q. … → ¬HasMguP (Unifies θ ρ₁ ρ₂ ∧ Q θ)`, and
  conjoining an unconstrained Q can SHRINK a unifier set to one that has an
  mgu. Refutations.lean, both axiom-clean, both on the Wand configuration
  (all thirteen terminal-move premises hold by rfl):
  - *hbase_shape_false*: Q := (· = wθ) — a singleton set is its own mgu
  - *hbase_stableQ_false*: even a SUBSTITUTION-STABLE Q (the real shape of an
    emitted eq) fails — under β ≈ (l:𝓫) the Wand set has the mgu
    β ≔ (l:𝓫), α ≔ ε. This is eq_rescued_solved seen at the residual.
  This is the shadow of the sharp result above: the Q-threading exists so
  that an arm whose type sub-call is stuck can still use the IH, carrying
  the residual as Q — which silently assumes a stuck conjunct makes the
  conjunction ambiguous. It does not.

## Symbols
- ↓: Row-lookup relation, three-way result r := (τ | ⊥ | ?)
- ★: Definite uncertainty, no elimination
- ⊑: Precision relation for ★
  - Every other type is below ★
- ≈: Row-equivalence relation
- ≤|≥: Instantiation relation for type-schemes
  - τ ≤ σ: τ is an instance of σ
  - σ ≥ τ: σ instantiates as τ
- ≐: Type unification
- ≐ᵣ: Row unification
- ⊴: "At least as general" (covering order on schemes) — σ ⊴ σ' : σ' covers σ
- ⊴⊑: covering up to precision — σ' answers each σ-instance with a ⊑ₜ-sharper one

## Properties
- ↓: deterministic, monotone, total (under RowWF)
- ⊑: reflexive, transitive (limmited)
- ≈: refl, symm, trans, congruence under |; adjacent distinct labels commute, ε is a unit
- ρ: rows mod ≈ form a trace monoid (partially-commutative, cancellative)

## Proof Overview
Proofs are for _closed_ programs (Γ = ∅). e ↯ marks _lookup-errors_: a selection reached a record literal without the label. ★ makes such programs typeable (now also via T-sel-⊥), so progress only holds up to ↯. 

*Progress*: If Γ = ∅ and Γ ⊢ e: τ, then `Progress e`
  - step: ∃e' with e → e'
  - done: or e ∈ Values
  - err: e ↯

*Preservation*: If ∅ ⊢ e: τ and e → e' then ∅ ⊢ e'
*Soundness*: If ⊨ e: τ then ⊢ e: τ
*Completeness*: If ⊢ e: τ then ⊨ e: τ

## Lemma Overview
### Declarative (L1)
- Progress & Preservation
  - *record inversion*: T-eq and T-★-intro can wrap any derivation and have to be stripped; each inversion gains a `∨ τ = ★` disjunct (harmless for canonical forms since fn/rcd heads ≠ ★).
  - *head rigidity*: ≈ₜ never changes the head constructor, so we can get "back" our shape. Now includes ★-rigidity (★ ≈ σ ⟹ σ = ★) because T-★-intro lives outside ≈.
  - *lookup-equivalence*: Lookup-category (τ | ⊥ | ★) is not changed by row-equivalence.
  - *term/type agreement*: Lookup on types carries over to syntax-lookup
- Progress:
  - *canonical forms*: A value's syntactic shape is determined by its type's head.
  - *scheme non-vacuity*: Every scheme has its own body as instance (θ = id).
- Preservation:
  - *polymorphic substitution*: if x: σ and v types at every instance of σ, then e[x:=v] keeps its type
    - *context conversion*: typing only sees the context through lookups, so contexts that agree on lookups type the same terms. Subsumes weakening, exchange and shadowing.
    - *rowEnv congruence*: lookup only depends on row-solutions, so substitution leaves lookups untouched
  - *spine-var-freeness*: literal rows carry no row-var in their spine, so no ★
- Refinement:
  - *lookup monotonicity in ⊑-vocabulary*: Γ ⊑ Γ' sharpens a lookup — definite results survive on the nose (monotonicity), ? re-resolves via totality (needs Γ'.RowWF).
  - *⊑-rigidity*: below anything but ★ sits only the same head constructor; ★ sits only below itself. 
  - *★-typeability of selections*: a selection on a record-typed term always types at ★
- Standalone Metatheory:
  - *determinism*: lookup is deterministic.
  - *monotonicity*: definite results (τ/⊥) survive extending the row-solutions, only ★ can improve
  - *totality*: under acyclic row-solutions (RowWF) every lookup has a result
  - *substitution stability*: definite lookups survive type substitution
- Type substitution & generalization:
  - *type-substitution lemma*: typing transports along θ into a context whose schemes θ-cover the originals (typed_applySubst_aux); the ?-selection case re-derives through T-sel + T-★-intro / T-sel-⊥ / T-sel-★ per the substituted lookup
  - *scheme renaming*: capture-avoiding renaming of scheme binders against a finite avoid-set (renameScheme) — the only place fresh names are needed
  - *syntactic let*: the standard HM generalization rule (one derivation + ᾱ ∩ ftv(Γ) = ∅) is admissible for instance-closed T-let (tLet_syntactic)
- Principality refutation:
  - *no blur factoring*: no substitution instance of the L1-finalized {β} → ★ sits ⊑-below a found-typing {(l: τ₀)} → τ₀ with τ₀ ≠ ★ (finalized_no_blur)
  - *no plain principal scheme*: no ∀ᾱ.τ scheme is instance-closed while having both the found-typing and the ⊥-typing of λx. x.l as instances (no_plain_principal_scheme) — plain schemes cannot be principal; qualified/stump-carrying schemes (L2) are forced

### Algorithmic (L2) 
- Qualified schemes:
  - *L2 TYPE SAFETY*: qProgress + qPreservation — the qualified system is safe in
    its own right, not via L1. 
  - *plain embedding*: Q = ∅ degenerates ≥\_Γ to the Γ-independent Scheme.Inst
  - *discharge determinism*: Row discharge is deterministic
  - *definite-stability*: a resolved stump never re-checks, wake-up only improves
  - *instance-closedness*: EVERY ≥\_Γ-instance of selQ = ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
    is a declarative typing of λx. x.l, in ANY Γ (selQ_instance_closed) — the three
    discharge cases replay T-sel / T-sel-⊥ / T-sel-★ per instance
  - *L1 ⊆ L2*: Typed.toQ embeds every plain derivation (Q = ∅ instances)
- IS L2 »SOUND & COMPLETE«? — audit 2026-09-13, build green, no sorries
  - *safety*: YES. qProgress/qPreservation are proven over ⊢_Q directly (Step/Value/Err
    reused from L1), preservation ON THE NOSE, axiom-guarded in Axioms.lean
  - *vs. L1*: only the ⊆ direction. The converse is believed false, but NOT mechanized:
    there is no `¬ Typed ∅ two_use ({a: 𝓫_c | b: ★})`. no_plain_principal_scheme is about
    λx.x.l at ONE instance pair, so the Qualified.lean comment "no single plain scheme
    could serve both uses" is prose, not a theorem. ⇒ L1 ⊊ L2 is an OPEN refutation
  - *completeness w.r.t. inference*: NO, and not yet stateable. No W (algorithmic.lean is
    an import root); unifyRowM_success_iff is completeness for ≐ᵣ, not for ⊢_Q; and
    principality for L2 exists only as the single-example bookend qualified_principal_scheme
    (λx.x.l), not as "∀e ∃σ principal". The ⊴ order now exists (see Principality); what is
    still missing is a W and the covering conjunct for a scheme it produces
  - *watch item*: qLet's INHABITATION premise (∃τ₁. σ ≥_Γ τ₁) is non-standard — it exists
    because progress is false without it. Any future completeness proof must show inference
    always discharges it ("stumps always finalize")

- ≈-characterization:
  - *normal form*: rows flatten to spines
  - *the characterization*: ρ₁ ≈ ρ₂ iff same var sequence and all l-projections pointwise equal
  - *end-var cancellativity*: shared leading/trailing vars cancel
  - *full cancellativity*: any shared prefix/suffix row cancels 
  - *ground rows*: SpineVarFree ↔ empty var sequence
  - *Some examples*: Wand ambiguity & Regression

- The unification algorithm ≐:
  - *forced steps*: There is always a forced step we can take during unification that keeps mgus
  - *field-count invariant*: ≈ preserves l-field count; substitution only increases count
  - *projClash soundness*: projClash s₁ s₂ → no unifier (projClash_no_unifier)
  - *SUCCESS SOUNDNESS*:
    - *MOVE-REFLECTION lemmas*: θ unifies the residual ofSpine tᵢ ⟹ θ unified the original ofSpine sᵢ"
    - *U-GROUND*: a field does NOT commute past a var, shadowing

