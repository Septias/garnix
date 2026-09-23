
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
- [~] Type Inference
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
      guards read the accumulated solution (B1).
    - WHAT VACUITY ACTUALLY IS (assessment, 2026-09-14). The two legs are
      `Sat θ s → Unifies θ` and `Unifies θ → ∃θ' ⊨ s`. BOTH are vacuously true
      when `s` is unsatisfiable AND the problem has no unifier, so the pair does
      not exclude a success on an unsolvable input. Contrapositive of
      completeness: if the problem HAS a unifier then `s` is satisfiable — so
      *vacuous success ⟺ success on a problem with no unifier*. Nothing proved
      today is FALSE; the pair is under-specified, not broken. Blast radius is
      exactly inference soundness, which is unproved, so nothing currently rests
      on it — but that is the next item on the critical path.
    - THE MISSING STATEMENT is NOT `UnifyWF`. It is
        `unifyRowM … = .success s _ → ∃θ, Sol.Sat θ s`.
      `UnifyWF` bundles `Acyclic` (⟦S⟧-as-context, the lookup bridge) with
      `Ranked` (closure exists ⟹ satisfiable). Only the RANKED half bears on
      vacuity. Unbundle them so the lookup-bridge half does not block this one.
    - PROPOSED ROUTE: carry `∃θ, Sol.Sat θ s` as a THIRD conjunct of the
      existing mutual success induction, rather than proving a property of the
      returned `s` post hoc. Each arm exhibits its own witness; the only hard
      case is `UResM.seq`/`Sol.comp`, where the two stages' witnesses COMPOSE —
      no rank function is constructed, so the three refuted rank candidates
      above do not apply. What composition needs is domain non-collision, which
      is what `unifyM_bounded`'s Supply/Avoids/SolBelow already delivers.
      SUB-OBLIGATION: `Sol.Sat` quantifies over every pair in the list while
      `Sol.toSubst` reads only the FIRST binding for a variable, so
      `Closes → Sat` silently needs no-duplicate-keys. There is no such
      invariant anywhere. Prove it or restate Sat through `toSubst`.
      FALLBACK: the decidable check already exists — `solRankedB`/`peelDeps`
      (Fuzz.lean) is Kahn's algorithm on the sorted dependency graph. Promote it
      into the driver as a final gate that downgrades an unrankable success.
      Then non-vacuity holds by construction. Costs: an extra pass, a changed
      verdict profile (full re-sweep), and kernel-reduction time — the same
      `TyVar = String` sensitivity that made one regression take 76s under B1.
  - [~] occurs: *sound where it is LOCAL* (2026-09-16)
    - α ≐ᵣ (β|α|γ) is no longer reported: the ε-collapse rule SOLVES the
      all-variable occurrence, at any spine and any multiplicity
      (`allvar_occurs_mgu`). What the guard still rejects is the deep and the
      field-pinned occurrence, and both are genuine no-unifiers — the case
      analysis closes (`solveVarM_occurs_no_unifier`). At the type sort the
      guard is now SORTED and its rejections are genuine too
      (`bindTy_occurs_no_unifier`, a constructor-depth argument).
    - THE REMAINING GAP is not about the problem: the guard reads
      `depReach Θ`, so it can also fire on an α that is merely REACHABLE from s₂
      through the accumulated expansions. No no-unifier theorem follows from
      that disjunct, so the driver-level `unifyRowM … = .occurs ⟹ ¬∃θ` is
      blocked there — and only there. `solveVarM_occurs_no_unifier_nil` is the
      unconditional statement at Θ = [] (`depReach [] V = V`).
  - [~] stuck : *incomplete*
    - (k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫)
  
- Open
  - SWEEP — ill-formed solutions, BOTH halves, all three universes:
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
    disjunct must re-split. *Recorded, not repaired.*
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
    REMAINS, and it is NOT a missing definition. The
    type-pass measure ALREADY EXISTS: `Ty.allRowVars`/`Row.allRowVars`
    (Defs.lean:177) is exactly "row variables at spine positions at any nesting
    depth", and it is sort-aware, so the `NoCapture` leak cannot reach it.
    The obstruction is that no "values avoid the domain" invariant over it
    survives. Three swept over all three universes:
      every binding's allRowVars avoids the row domain   40 / 2336 / 24  ✘
      ROW bindings only                                  40 / 2160 / 24  ✘
      TYPE bindings only (what the `.fn` arm needs)        0 /  176 /  0  ✘
    and the witnesses show it is false BY DESIGN:
      (l:𝓫 | b) ≐ᵣ (m:{a} | a)  ⟹  b ≔ (m:{a} | aaa | ε)
    puts the bound row variable `a` inside a PAYLOAD — which `Acyclic` permits,
    since it reads only top-level spines — and
      (l:𝓫 | b | l:𝓫) ≐ᵣ (m:{a} | a)  ⟹  aaaa ≔ {a}
    does the same at the type sort, `aaaa` being a δ an expansion invented with
    its payload captured before `a` was solved.
    So the `.fn` arm cannot carry "the problem's row variables avoid V": that is
    not preserved by `applySubst`, because a triangular solution legitimately
    holds bound variables under record constructors. What IS true is that such a
    variable never reaches a top-level SPINE — the driver applies θ before
    recursing — but that is a fact about solve-and-apply, not about the syntax
    of the problem, and stating it needs the applied form, which brings the
    closure (and `Ranked`) back. A different induction is needed.
    DO NOT retry the three invariants above. `sVarSeq_applySubst` and
    `Sol.acyclic_comp` stand and are reusable in whatever replaces them.
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
    THE DEPGRAPH ROUTE IS CLOSED (2026-09-19), and so is the whole rank-by-order
    family. Measured with a TRACED CLONE of the driver in Fuzz.lean ([6]/[7]) —
    `Θ` is an input to `unifyTyF`/`unifySpineMF` and never returned, so it had to
    be cloned to be observed; the clone's verdict is compared against the real
    driver on every pair (`nDis` = 0 / 0 / 0, so the numbers below stand).
      * Θ AS THE CARRIER: ✘ twice over.
        - Θ is itself CYCLIC on 316 / 3232 / 88 successes. The cycle is spurious
          and cross-sort: `expandDeps` emits `(δ, τ.ftv)` and `DepGraph` is
          UNTAGGED, so on (l:a | a) ≐ᵣ (a | m:𝓫) it records a→[aa aaa], aa→[a]
          where the first `a` is a ROW variable and the second a TYPE variable.
        - Θ is INCOMPLETE: it records expansions only, so U-var-solve's bindings
          have no edges at all and Θ-reachability MISSES 40 / 2736 / 24 real
          solution edges — first at (l:𝓫 | b) ≐ᵣ (m:{a} | a), which needs ᵣb→ᵣa.
        - consequently Θ-depth as the rank fails 332 / 4700 / 96.
        Tagging Θ and recording U-var-solve in it would make Θ *equal* to the
        solution's dependency graph — at which point "Θ is acyclic" IS
        `Sol.Ranked`, so that repair renames the problem rather than reducing it.
      * CREATION ORDER: ✘, and this kills the family. The clone also records a
        LEDGER — every key bound, tagged, in the order bound — which is what
        `domS` is not (`domS` = ty ++ row scrambles two sorts the driver
        interleaves, so the earlier index candidates were confounded). Both
        directions, and both intra-expansion orders, fail:
          payload δ before host β:  decreasing  16 / 15036 /  16   ✘
          host β before payload δ:  increasing  40 /  2720 /  24   ✘
        because the two edge sources point OPPOSITE WAYS in time. An expansion's
        host binding mentions the variables it just invented (forward); a
        U-var-solve binding mentions variables already there (backward); and
        `comp`'s push makes an old binding acquire a new dependency. All three
        occur in one solution at (b | a) ≐ᵣ (l:{a} | l:{a}):
          [aa≔{ε}, aaaa≔{ε} ; b≔l:aa | l:aaaa | ε, aaa≔l:aaaa | ε, aaaaa≔ε, a≔ε]
          ledger ₜaa ᵣb ₜaaaa ᵣaaa ᵣaaaaa ᵣa — ᵣb→ₜaa points back, ᵣb→ₜaaaa forward.
        So no rank read off any timeline can work. Do not retry one.
      * THE `Sol.comp` PRESERVATION STEP IS FALSE AS STATED. The lemma the entry
        above asks for — the later stage mentions nothing of the earlier stage's
        domain, so the two ranks stack — was measured at every `.seq` and every
        expansion: 388 / 11756 / 132 crossing mentions. And the cause is not
        capture, it is that THE DRIVER RE-BINDS A KEY IT ALREADY BOUND.
    THE ACTUAL OBSTRUCTION, and it is new: SHADOWED BINDINGS THAT DISAGREE.
    Successes whose solution binds one key twice: 348 / 8964 / 108 — and of
    those, the two bindings are not even syntactically equal in 348 / 8852 / 108.
    Minimal witness:
        (b | a) ≐ᵣ (l:{a} | b)   ⟹   [aa≔{a} ; b≔l:aa | aaa, b≔aaa | a | ε]
    `expandL`'s `renameVar β β′` is applied to the HOST side ONLY — verified:
      expandL [] (localSupply s₁ s₂) s₂ s₁
        = host β=b, label=l, residual left [b], residual right [aaa, a]
    so the `b` on the OTHER side survives the expansion and U-var-solve binds it
    a second time, to something unrelated to what the expansion bound it to.
    This is exactly the SUB-OBLIGATION flagged under "success" above — `Sol.Sat`
    quantifies over every pair while `Sol.toSubst` reads only the FIRST — and it
    is not a corner case: it fires on a two-atom problem. Everything downstream
    inherits it. `Sol.Ranked` quantifies over ALL bindings, so the dead second
    binding must ALSO be rank-decreasing; `Sol.comp` has no disjoint-domain
    invariant to appeal to; and ⟦S⟧ and `Sat` are describing different
    substitutions wherever the two values disagree.
    WHAT IS STILL TRUE, and it is the lead. The solution's dependency graph is a
    DAG even UNTAGGED — 0 / 0 / 0 over every success in every universe — so a
    rank on NAMES exists, which is stronger than `Sol.Ranked` needs. And with
    shadowed bindings dropped (first one kept, which is all `toSubst` reads),
    domS-index-decreasing is clean in `wide`/`nest` and fails only 1000 times in
    `deep`. So the order to attack this in is: FIRST decide the duplicate-key
    question — either give the driver a no-duplicate-keys invariant (make
    `expandL` apply β's binding to the residual, or have `comp` drop a shadowed
    key) or restate `Sol.Ranked`/`Sol.Sat` through `toSubst` — and only THEN
    look for a rank. A rank proved against the current `Sol` has to carry the
    dead bindings, and that is what every candidate so far has died on.
    REPRODUCE: `cd lean && lake build fuzz && lake exe fuzz`, sections [6]/[7].
  - [ ] solver state S = (θ, Δ, W), stump wake-up, confluence of the final state

- **2026-09-22 — pricing U-expand (Stage 0).** `Fuzz.lean` now carries a second
  clone, `nxSpineMF`/`nxTyF`, identical to the driver except that the four
  expansion arms are `.stuck`. It is a CLONE, not an edit: the real driver and
  the traced clone are untouched, `nDis` still reads 0, and nothing in
  `RowUnify/` moved. `Θ` is absent from the clone rather than threaded-and-
  ignored — `expandDeps` is the graph's only producer, so with no expansion the
  graph is `[]` at every node and `depReach [] V = V` makes every guard reading
  it inert. **The DepGraph is dead code without U-expand**, ~80 lines of
  `Defs.lean` plus a parameter on ~80 signatures.
  REPRODUCE: `cd lean && lake build fuzz && lake exe fuzz`, section `STAGE 0`.

  Over the three universes (771 578 pairs, cap 64):

  | | wide | deep | nest |
  |---|---|---|---|
  | verdicts agree | 98.3% | 92.4% | 97.5% |
  | success ⇝ stuck (the cost) | 1036 = 7.0% of successes | 38 808 = 30.5% | 348 = 9.1% |
  | clash ⇝ stuck | 0 | 0 | 0 |
  | occurs ⇝ stuck | 168 | 11 912 | 256 |
  | fuel differences either way | 0 | 0 | 0 |

  Three findings, in order of what they settle.

  1. **Verdict-monotonicity, checked rather than argued.** The expansion arms
     are last in the dispatch order, so the stubbed run should be the real run
     with some subtrees replaced by `.stuck`, and `.stuck` propagates through
     `.seq` and `expandResM`. Prediction: at equal fuel, wherever both reach a
     verdict, either they agree or the stub says `.stuck`. Counterexamples
     found: **0 / 771 578**. This is what licenses "removing the arm shortens
     every induction and weakens no theorem" — all four legs (clash soundness,
     occurs soundness, success soundness, success completeness) are of the form
     *verdict reached ⟹ property*, and completeness in particular is
     conditional on success (`… = .success s S' → Sol.Sat θ s`), never
     "a unifier exists ⟹ success".

  2. **`Sol.Applied` ⟺ no expansion fired — exactly.** Cross-tabbed over the
     driver's successes, not inferred from equal totals:

     - ¬Applied **and** lost to the stub: 1036 / 38 808 / 348
     - ¬Applied but **kept**: **0 / 0 / 0**
     - Applied but **lost**: **0 / 0 / 0**

     and every one of the 105 444 stub successes is `Applied`. So the
     triangularity of `Sol` is *entirely* an artefact of U-expand; `Sol.comp`'s
     stage-pushing never produces a non-idempotent solution on its own. This
     refutes the worry recorded above that the duplicate-key/shadowing problem
     is `comp`'s rather than the arm's. Consequence: **without U-expand,
     `Sol.Ranked` is not merely provable, it is unnecessary** — `Applied` is
     strictly stronger and `Sol.closes_toSubst_of_applied` hands over `⟦S⟧` =
     `toSubst` directly. The whole rank search (list position, reverse
     position, name length, DepGraph reachability, DepGraph depth, creation
     order — all refuted) is searching for something only U-expand makes
     necessary.

  3. **`Acyclic` and `Ranked` already hold 0/0/0 under BOTH drivers**, so
     `UnifyWF` is not where removal pays; `Applied` is. And `clash ⇝ stuck` is
     0 everywhere: no clash is ever reached only through an expansion, so
     `projClash` alone already catches what the arms would have.

  Two things Stage 0 does NOT establish.

  - **Termination.** The parametric families show no profile change worth the
    name: `crossfield-n` drops from fuel `n+1` to `1` because it is exactly the
    expansion engine and now gets stuck at once, and every other family is
    unchanged. Expansion is already *linear* on these shapes. So the families
    are not evidence for a post-removal measure; they were designed before the
    measure question and do not probe it. `fuelGain = 0` also says no pair in
    these universes diverges *because of* expansion.
  - **Whether the 7–30% is worth paying.** The universes are exhaustive over
    synthetic spine pairs and heavily weighted toward exotic shapes; the lost
    witnesses are all one shape up to renaming, `(l:{a}) ≐ᵣ (b | a)` — a field
    whose payload mentions the other side's tail. Whether Nix-shaped programs
    hit it is a question for hand-written `Infer` examples, not for this sweep.
    Note also that every lost success routes to `A-app-degrade` → `★`, and
    `appDeg` has no declarative counterpart (no `T-app-★`), so removal makes an
    already-open gap load-bearing.



## Termination
- Fuzzing suggests, that the algorithm actually terminates


## Inference

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

- [x] L2 TYPE SUBSTITUTION 
  A `QTyped` derivation transports along a solution's
  CLOSURE, into the context read under it. All 13 QTyped constructors and the 3
  body constructors, modulo one named hypothesis `SchemeImage`: every scheme has
  a capture-avoiding σ-image (L1's counterpart is `renameScheme`), which only
  `qLet` needs because it introduces a scheme the context map knows nothing of.
  THE NAIVE SHAPE IS WRONG and the `.var` case says so: a context relation
  "every row solution survives with θ applied" cannot work, because
  `(Row.var α).applySubst θ` is `θ.row α` — after substituting there is no
  variable left for `L-α` to chase. The solution has to be DISCHARGED into the
  substitution, which is `Sol.Closes`, and the transport is then
  `Sol.lookup_toCtx` (already proved in State.lean). This is also why
  `InferSound` carries a `Closes` hypothesis: that was the right call.
- [~] `InferSound` — the machinery is built and validated; three cases are the
  real obstruction. DONE 2026-09-15: `Infer.sat_mono` (θ is only ever refined,
  stated SEMANTICALLY as "any σ satisfying the later solution satisfies the
  earlier" — transitive on the nose, needs no associativity of `Sol.comp`, and
  is what lets the conclusion sit at the FINAL state while premises were solved
  at intermediate ones); and `infer_sound_app_step`, the A-app case end to end:
  `Sol.Sat.comp_inv` peels the stage's own solution off the composite, success
  soundness turns it into a `TyUnifies`, `tyUnifies_applySubst_of_sat` strips
  the substitution the arm unified under, and `qEq` absorbs the resulting ≈ —
  which is what T-eq is FOR. So the chain fits.
  DONE 2026-09-16: `lean/InferSound.lean`, EIGHT of the thirteen A-rules, each
  as its own lemma, plus A-sel-? modulo one named condition. Proved: A-cons,
  A-lam, A-rec and the three A-ξ rules (pure congruences); A-app and A-conc
  (emit an equation, T-eq absorbs the ≈); A-sel and A-sel-⊥ (additionally read
  a field off the solution). Three pieces of new machinery carry them:
    * `SolveTy.unifies_sat` — a solved equation holds under ANY σ satisfying the
      state it produced, and of the ORIGINAL types, not the ones the arm saw.
      The three moves `infer_sound_app_step` did by hand, done once.
    * `Sol.lookup_toCtx_sat` — THE FIND. `Sol.lookup_toCtx` asks for
      `Sol.Closes`, and the induction cannot supply it: at an INTERMEDIATE state
      σ is never the closure, because later stages bind variables this state
      left free and `Closes` demands σ fix exactly those. `Sol.Sat` is enough on
      DEFINITE results — a definite derivation never uses `L-α-free`, the one
      rule whose result `Sat` cannot control. The price is that `Sat` is only ≈,
      so the transported lookup lands on an ≈-equivalent row and the conclusion
      carries a `ResEquiv` where `lookup_toCtx` has an equation; `lookup_equiv`
      supplies the step, which is a second calibration check on ≈. So `Closes`
      is needed only at the LAST step, where σ := S′.subst is taken — NOT inside
      the induction, which refines the QSubst.lean entry above.
    * `qtyped_sel_star` — a record-typed subject always types a selection at ★:
      at an empty row environment lookup is total for free, and T-sel / T-sel-⊥
      / T-sel-? cover the three verdicts with T-★-intro blurring the first.
  THE REMAINING FIVE, and what each is actually waiting on (A-var STARTED):
    * `A-var` — STARTED 2026-09-16, and it splits cleanly in two. The typing
      half is `infer_sound_var_step`: `qVar` wants a scheme and an INSTANCE, and
      the instance's own substitution χ is ours to choose, since `QScheme.Inst`
      quantifies it existentially. The constraint half is
      `Wake.dischargeEquiv` — "K-hit / K-⊥ / K-repark are D-hit / D-⊥ / D-?"
      as a theorem, one wake-up step: a step either DISCHARGES its constraint or
      re-parks it with the stump intact. K-repark corresponds to nothing, so the
      conclusion is a disjunction, not an implication.
      TWO THINGS THE INFORMAL STATEMENT HIDES, both forced:
        - K looks up the row the STATE HAS ALREADY SUBSTITUTED (`ρ[⟦S⟧]`), D
          looks up `ρ[θ]`. Under a σ satisfying S the two are ≈-equal
          (`Sol.Sat.substEquiv` is a ≗ and applySubst is a ≗-congruence), so
          they transport — but only up to ≈, hence `lookup_equiv` again.
        - `Stump.Discharge.hit` pins `θδ = τ` ON THE NOSE and the algorithm
          cannot deliver that: K-hit SOLVES `δ ≐ τ`, and a solved equation is
          only ever an ≈-fact. So the correspondence is stated against
          `Stump.DischargeEquiv` (hit relaxed to ≈; ⊥ and ? stay rigid, since ★
          has no ≈-congruence rule). The ≈ is paid for by χ — σ corrected at the
          constraints' result variables to the types the lookups actually found
          — and what is left over is absorbed by T-eq.
      LIFTED TO A WHOLE RUN the same day: `Wakes.dischargeEquiv` — every
      constraint submitted to wake-up is either DISCHARGED or still parked when
      the run ends. The lift needs exactly one structural fact
      (`Wake.parked_preserved`): every rule that retires a stump filters the
      parked list on `stump.res`, so an entry survives a step precisely when its
      result variable differs from the one being woken. Its side condition —
      the submitted constraints are pairwise distinct in `res` — comes from the
      rule's own `FreshRenaming` via `InstStumps.pairwise`.
      STILL OPEN for A-var: `QScheme.ResWF` (below); the σ-image of the scheme
      (`SchemeImage`/`QCovers` plus capture-avoidance); and the `Finalize.star`
      defect below, which is where every still-parked constraint lands.
      A-var is also the case that FIXES THE SHAPE of the context correspondence,
      so it has to be settled before the induction is assembled, not after.
    * `QScheme` CARRIES NO WELL-FORMEDNESS on its constraints — found
      2026-09-16, stated as `QScheme.ResWF` (InferSound.lean), not proved. A
      scheme's constraints ought to have pairwise distinct result variables, and
      every one of them ought to be BOUND; the structure requires neither.
      A MISSING INVARIANT, not a live bug: the algorithm only ever builds
      schemes out of stumps whose result variables were drawn fresh (A-sel-?) or
      renamed apart (A-var), so it never constructs a violating one. But the
      type permits it, and if one existed wake-up would silently DROP a
      constraint — every rule that retires a stump filters on `stump.res`, so
      two stumps sharing one are both retired when either fires, the second's
      lookup never performed and its δ pinned by the first's.
      `Stump.Discharge.det` makes that sound only if the two lookups agree,
      which nothing requires. Belongs with `UnifyWF` in the family of "true of
      the states inference builds, unproved".
    * `Finalize.star` HAS NO LOOKUP PREMISE — found and REFUTED 2026-09-16,
      `finalize_star_no_discharge` (InferSound.lean), guarded in Axioms.lean.
      F-★ sets δ := ★ unconditionally, while `Stump.Discharge` offers ★ only
      when the lookup is `⊥` (D-⊥) or `?` (D-?). So a stump finalized after its
      lookup has already started to land has NO declarative reading at all.
      Every OTHER rule that touches a stump's result variable says what the
      lookup did first: `Wake.hit` carries its `Lookup … (.found τ)`,
      `Wake.abs` its `Lookup … .absent`, `Wake.repark` its `LookupBlocked`.
      THE WITNESS is as small as it gets: a stump on the LITERAL row `(l: 𝓫)`,
      whose lookup lands at every context and under every substitution. F-★
      fires on it anyway — nothing in the rule looks — and the resulting state
      forces σδ = ★ while the lookup says `𝓫`. Not even `DischargeEquiv`, the
      ≈-relaxed version, survives: ★ has no ≈-congruence rule, so `★ ≈ 𝓫` is
      false too.
      THE FIX is the premise its siblings have — `LookupBlocked` on the row it
      is finalizing, which is exactly what A-sel-? already establishes when it
      parks the stump. **APPLIED 2026-09-18**, together with the other half the
      reachable witness forced (saturation in the A-rules); see the F-★ entry
      under `# Problems` for the whole chain, and `Finalize.dischargeEquiv` for
      what the premise then buys. The refutations survive the fix by naming what
      they refute: `FinalizeUnguarded` is the old rule, kept.
    * `A-let` — the generalized scheme's instances, needing `SchemeImage`
      (QSubst.lean) plus the Δ-split. It also depends on the stump condition
      below: `qLet`'s INHABITATION premise is met "by construction" only because
      a parked stump always finalizes.
      SHARPENED 2026-09-16, and the news is bad for the obvious route.
      `QScheme.applySubst` moved to Qualified.lean and now
      carries its side condition explicitly (`QScheme.Avoiding`: σ fixes the
      binders and its image on the scheme's other variables never mentions one,
      plus `QScheme.WF`: the δ's are among the binders). Under it the FORWARD
      half of `QCovers` is PROVED — `QCovers.forward_of_avoiding` — discharge and
      all, the three Discharge cases going through `Sol.lookup_toCtx`. That is
      the whole content of the old "wiring renameScheme in here is open" note on
      `QScheme.applySubst`, and it is what `qVar` consumes.
      The BACKWARD half is REFUTED for that witness:
      `qcovers_backward_false_for_applySubst`. Take s = (b ≔ 𝓫), σ = ⟦s⟧,
      σ₀ = ∀a.a — which IS `Avoiding` σ, and whose image is σ₀ again. `b` is an
      instance of the image, and `b` is not the σ-image of anything, because σ
      sends `b` to 𝓫 and fixes everything else. So the obstruction is NOT
      capture and no freshness discipline fixes it: `applySubst σ` is not
      surjective, while an instance set is always as large as its binders allow.
      A `SchemeImage` proof must therefore produce a DIFFERENT scheme, not the
      pushed-through one. The same example looks fatal for `SchemeImage` itself —
      forward forces the image scheme's body to be one of its own binders (its
      instances must include both 𝓫 and an arrow), and such a scheme has every
      type as an instance unless a stump blocks it, while the σ-image of ∀a.a is
      exactly the types not mentioning `b`. Ruling out every stump configuration
      is what a full refutation still owes; recorded as the shape of the
      obstruction, not as a theorem. Do not retry `σ₀.applySubst σ`.
    * `A-sel-?` — no longer a design question with no answer. The rule returns a
      stump-variable δ and parks `⟨α ▷ ρ.l ↓ δ⟩`; δ is a PROMISE, not yet a
      type. What the promise is worth by the time σ is fixed is exactly a
      DISCHARGE — the declarative `Stump.Discharge`, read at σ against a
      discharged row environment, with the hit payload relaxed to ≈
      (`Stump.DischargeEquiv`, see A-var). That is the SAME condition A-var
      needs of the constraints it submits to wake-up, so the two cases share one
      notion instead of inventing a second. GIVEN IT, the rule is PROVED sound
      (`infer_sound_selUnk_step`), all three discharge cases landing on T-sel /
      T-sel-⊥ / T-sel-★. Establishing it is wake-up's job, and it is now a
      statable obligation rather than an open question about meaning.
      SHARPENED 2026-09-18: the case needs no discharge at ALL once the promise
      is a hypothesis OF THE JUDGEMENT. `QTypedC` (InferSound.lean) is QTyped plus
      a list of stump ASSUMPTIONS threaded unchanged through every rule, with one
      new rule `stump` answering at the stump's own result variable, so a parked
      stump IS a typing hypothesis and δ is a variable standing for its future
      value. `inferC_sound_selUnk_step` is then the record equation plus one rule,
      with `σ.ty δ = .var δ` — "σ has no opinion at δ yet", which is what parked
      MEANS — as its only side condition. The promise is redeemed once, at the
      end (`QTypedCDischarge`), instead of at every selection site. Sanity:
      `QTyped.toC` and `QTypedC.toQTyped` (at Δ = [] the two judgements coincide,
      so `stump` is the only addition). `Parked.toStumpC` builds the assumptions:
      σ applied to the stump's ROW, result variable left RAW — that pair is the
      difference between a stump and a typing.
    * `A-app-degrade` and `A-sel-degrade` — SOUNDNESS-GAPPED BY CONSTRUCTION,
      and not in the way `plans/inference-gap-analysis.md` records. That file
      asks for "replacing a position by ★ preserves declarative typeability",
      which is a statement about a term that ALREADY types. Here it does not:
      when `τ₁ ≐ τ₂ → β` is stuck, `e₁` has a type, `e₂` has a type, and QTyped
      has NO RULE that applies one to the other. `qApp` wants a literal arrow,
      `qEq` only moves along ≈, and ≈ relates ★ to nothing but itself
      (`TyEquiv.unk_inv`), so `qUnk` cannot manufacture the arrow either. What is
      missing is a DECLARATIVE rule — application at ★, the ★-elimination the
      failure policy assumes and the type system does not have. The alternative
      is to prove the degradations unreachable, which A-sel-degrade may well be:
      `r` is drawn fresh immediately before `τ ≐ {r}`, so that equation can only
      clash or succeed. A-app-degrade is NOT unreachable — its equation descends
      into `τ₂ ≐ A`, which is an arbitrary row problem and can genuinely stick.
  ALSO FOUND: `S′.parked = []` is more usable than the earlier note claimed.
  `SolveTy`/`extend` leave `parked` alone, so parked only ever GROWS — except at
  A-let, which drops Δq into the scheme, and at A-var, whose `Wakes` filter only
  ever removes stumps the instantiation itself created (`FreshRenaming` keeps
  their result variables off the pre-existing ones). So the hypothesis does push
  down into sub-derivations everywhere except through A-let, and an inner
  A-sel-? whose stump is never retired is already excluded by it.
- [~] `InferSound` (Infer.lean) — `Γ; S ⊢ e ⇒ τ; S′ ⟹ ⟦S′⟧Γ ⊢ e : ⟦S′⟧τ`.
  The statement is in Infer.lean, the proof in lean/InferSound.lean, case by
  case; the entry above is the ledger. The whole theorem still needs: `UnifyWF`
  (so ⟦S′⟧ is the closure rather than one step — the per-case lemmas are stated
  at an arbitrary σ with `Sol.Sat σ S′.sol` and do NOT need it), the parked
  stumps discharged (`S′.parked = []` plus `StumpHonest`, with F-★ supplying
  both), capture-avoidance for `QScheme.applySubst`, and — for the two
  degradation rules — a declarative rule for application at ★, which does not
  exist.

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
  - [x] `Principal selQ (λx.x.l)` in the ≼ form — DONE 2026-09-18, `selQ_principal`
    (Qualified.lean). Conjuncts 1+2 were selQ_sound_and_inhabited; 3 is
    `selQ_covers_typings`, and the L2 inversion it needed is `qsel_var_inv` — the
    counterpart of minimal.lean's sel_var_unk, but FULL rather than the ★-only
    special case: every typing of a selection on a monotype-bound x factors
    through ONE lookup, and the typing sits ≼-above that lookup's collapse
    (`∃ ρ r. τx ≈ {ρ} ∧ Γ ⊢ ρ.l ↓ r ∧ collapse r ≼ₜ τ`). qEq composes by
    TyBelow.trans, qUnk by TyPrec.unk — which is exactly why the ≼ form goes
    through where the ⊑-only one was refuted. `selQ_inst_of_lookup` then ANSWERS
    each typing with the instance for that same lookup, so conjunct 3 is
    discharged by the lookup the typing already performed, not by a search.
    Needed `qvar_inst_inv` (the L2 `var_inst_inv`) as well. Axiom-clean.
    So λx.x.l HAS a principal qualified scheme in the corrected order — the
    bookend `no_plain_principal_scheme` forced, now proved rather than asserted.
  - [x] hence *selQ is ⊴≼-greatest* among all schemes sound for λx.x.l (selQ_greatest).
    With no_plain_principal_scheme this is the full @contributions claim: plain schemes
    are refuted, the qualified one is exhibited AND shown principal


# Problems
> Problems found during mechanized proving and their proposed solutions

- [!] **F-★ IS DEFECTIVE, AND THE DEFECT IS IN THE RULE SET**.
  `Finalize.star` has one premise, `S ⊢ δ ≐ ★ ⇝ S′`, and says
  NOTHING about the lookup — while `Stump.Discharge` offers ★ only under D-⊥ or
  D-?. So F-★ can commit a stump to ★ where the lookup LANDS, and the state it
  produces discharges under no substitution satisfying it, not even up to ≈.
  `finalize_star_no_discharge` (InferSound.lean) was the hand-built witness; the
  news is that the configuration is REACHABLE, which makes this a defect in the
  rule set and not only in the rule:
    * `fStarEx_infers` runs `(λx. {a = x.l}) {l = c}` — the thesis' own
      refinement example, closed — end to end. Inside the λ it is
      `selEx_infers` verbatim: A-sel-? parks `⟨r ▷ r.l ↓ δ⟩`, blocked on the
      record's row variable.
    * `fStarEx_stale_blocker`: A-app's arrow equation then solves `r ≔ (l: 𝓫)`
      and the stump is STILL PARKED. `Infer.var` is the only rule that runs
      wake-up, so A-app / A-conc / A-sel / A-let can all break the invariant
      "every parked stump is blocked on its recorded blocker" — which
      `plans/inference-gap-analysis.md` §B lists as absent, and which nothing
      enforces.
    * `fStar_wake_star_disagree`: from that one state K-hit and F-★ BOTH fire
      and commit δ to different types (𝓫 and ★). **Determinism is false** —
      algorithmic.typ's "the final θ, W and τ do not depend on the wake-up
      order" does not survive a rule that ignores the lookup.
    * `fStar_reachable_no_discharge`: the non-discharge, at that reachable
      state, for every σ satisfying it and every discharged row environment.
    * `fStarEx_refinement_lost`: the program's declarative type is `{a: 𝓫}`
      (`QTyped`, derived) and the finalized run answers `{a: ★}`, strictly
      blurrier — and by `finalized_no_blur` (minimal.lean) a ★ is never
      sharpened back. The refinement this development exists to deliver is
      discarded by the last rule of the run.
  THE FIX IS TWO-PART, and the reachable witness is what shows the second half
  is needed: F-★ gets the `LookupBlocked` premise its siblings carry (K-hit and
  K-⊥ both state what the lookup did; it is also exactly what A-sel-? already
  establishes when it parks), AND wake-up must re-run wherever a solution is
  written — equivalently, a quiescence side condition on the state finalization
  may run at. The premise ALONE turns a wrong answer into a stuck one: in the
  state above no finalization rule would apply at all.
  **SECOND HALF DONE 2026-09-18** — the invariant is now stated AND maintained:
    * `SolverState.Quiescent` (Infer.lean): every parked stump is genuinely
      blocked on the blocker it records, read on the row the state has
      SUBSTITUTED, which is the form `Wake` / `Wakes.park` check. (That differs
      from the ctx-chasing `Lookup S.ctx (.var r) l` on a non-idempotent
      solution — `UnifyWF`'s business — but coincides for a bare-variable stump
      row, which is all A-sel-? ever parks.) `Quiescent.blocker_unsolved` reads
      it as "no parked blocker is solved", which is §B's invariant.
    * `Saturate` — wake-up run to quiescence, stepping only on stumps that have
      gone STALE. A RELATION, and quiescence a PREMISE, for the same reason
      `Infer` is one: a saturation function owes a termination measure and
      K-repark has none. "A quiescent state exists" therefore joins the open
      list: `Wake`'s arms need `lookup_total` (hence `UnifyAcyclic`), and a step's
      equation can CLASH — the tension case, a hard error, not a gap.
    * `SolveTySat` / `WakesSat` — solve-then-saturate, wake-then-saturate — now
      stand in the A-rules where bare `SolveTy` / `Wakes` did (A-app, A-conc, the
      three A-sel rules, A-var). Packaged as definitions rather than extra
      premises, so every proof by recursion over the rules — `supply_mono`,
      `kinds_mono`, `sat_mono` — carried over with NO changes, given
      `.supply` / `.kinds` / `.satMono` for the three new wrappers.
    * A-sel-?'s `LookupBlocked` premise now reads the SUBSTITUTED row, matching
      the invariant. For its freshly drawn `r` that is the same condition.
    * `Infer.quiescent` / `InferRec.quiescent`: **every reachable state is
      quiescent**, by mutual recursion over the rules — unconditional at the
      rules that write (they end in a saturation), from the IH at the rest.
      `Infer.quiescent_of_nil` drops the hypothesis for a run starting at Δ = [].
    * `fStarEx_infers` is now the REGRESSION for the fix: the same closed program
      runs to an empty Δ with `{a: 𝓫}` — the declarative answer
      (`fStarEx_recovers`). `fStarEx_not_quiescent` names what was wrong with the
      old final state. The rule-level witnesses (`fStar_wake_star_disagree`,
      `fStar_reachable_no_discharge`) stay, at that state: saturation makes it
      unreachable, it does not repair F-★.
    * CONSEQUENCE FOR THE PREMISE: at a quiescent state F-★'s missing premise is
      IMPLIED, so Stage 2 can carry it either on the rule or as a `Quiescent`
      hypothesis on the finalization pass. It still has to be somewhere — the
      soundness lemma needs the fact, and `Finalize` can be applied at a state no
      run produced.
  **FIRST HALF DONE 2026-09-18 too** — F-★ now carries the premise, and the rule
  set has a top:
    * `Finalize.star` takes `p ∈ S.parked` and the `LookupBlocked` premise. Chosen
      on the rule rather than as a `Quiescent` hypothesis on the pass: it is
      self-contained, it mirrors K-hit/K-⊥/K-repark (every sibling states its
      verdict), and nothing stops `Finalize` from being applied at a state no run
      produced. `Finalize.of_quiescent` shows it costs the ALGORITHM nothing —
      quiescence IS the premise for every parked stump, and every reachable state
      is quiescent.
    * `Finalizes` — the `⇓*` closure, which `algorithmic.typ` left implicit.
    * `Run` / `RunSound` (Infer.lean): **the top-level entry judgement**, §B's last
      ✘ row. Infer from the empty state, then finalize what is parked. `RunSound`
      is what `InferSound` becomes once something can supply its `parked = []`
      hypothesis. Named, not proved — and its shape records WHY finalization runs
      last: the ★ it writes is only a discharge at the state's OWN substitution,
      not under every satisfying σ, because a later refinement can solve the
      blocker and make the lookup land (`Sol.lookup_toCtx_sat` excludes `?` for
      exactly this reason).
    * DETERMINISM, repaired: `Quiescent.wake_no_commit` (at a quiescent state no
      wake-up step commits anything — the only available step is a K-repark onto
      the same blocker) and `Finalize.wake_no_commit` (wherever F-★ applies, the
      same holds of that stump). So finalization is the only progress left at the
      end of a run. Order-independence for a whole run is still design.
    * NON-VACUITY, the two shapes the entry point has: `selEx_runs` — λx. x.l runs
      to `{β} → ★` with `l` flagged, F-★ supplying the ★, which is the
      L1-finalized type `finalized_no_blur` says nothing sharpens back; and
      `fStarEx_runs` — the refinement example runs to `{a: 𝓫}` with Δ already
      empty, finalization having nothing to do.
    * THE WITNESSES, rehomed: a fixed rule makes its own counterexample
      unstateable, so `FinalizeUnguarded` (InferSound.lean) IS the old premise-free
      rule, kept so `finalize_star_no_discharge` and `fStar_wake_star_disagree`
      stay checkable. `Finalize.toUnguarded` says the shipped rule is weaker, and
      `finalize_star_guarded_cannot_fire` / `fStar_guarded_cannot_fire` say the
      inclusion is PROPER — at both witness states the guarded rule has no
      derivation. Neither half of the fix suffices alone: saturation leaves
      `Finalize` applicable at hand-built states, and the premise alone would have
      left the run stuck at `fsS` with no finalization step at all.
    * `algorithmic.typ` UPDATED to match: `⇝!` / `↝!` / `↝*!` in the A-rules, the
      quiescence invariant in the state section, K!-done / K!-step, F-★ with its
      premise, F-nil / F-cons, an `== Entry` section, and the "Deterministic"
      bullet rewritten around what is actually proved. Its "everything below is
      DESIGN, not mechanization" header is gone — that layer is mechanized now;
      what is not is termination, determinism up to renaming, and soundness.
  **STAGE 3 (2026-09-18): WHAT FINALIZATION IS WORTH.** With F-★ guarded and `Run`
  in place, algorithm soundness FACTORS, and the middle factor is proved:
      RunSound = InferSoundC ∘ Finalize.dischargeEquiv ∘ QTypedCDischarge
    * `Finalize.dischargeEquiv` / `Finalize.discharge_isUnk` — **a finalization
      step discharges the stump it finalizes**, on the nose and not merely up to ≈
      (the ?-arm is rigid, unlike D-hit). This is the lemma F-★'s premise exists
      for: the premise supplies the `?`, the equation supplies the ★ (via
      `SolveTy.unifies_sat` — any σ satisfying the state sends δ to something ≈ ★,
      and ★ is ≈-rigid). It is `Stump.Discharge.unk` with the lookup performed by
      the algorithm: the K-/D- correspondence's last row, which used to be the one
      finalization could not honour.
    * `lookup_unknown_of_blocked` — the general step under it, worth having alone:
      blockedness anywhere gives an unknown lookup at a DISCHARGED row
      environment. Monotonicity read backwards (a definite answer survives
      extending the row solutions, so a definite answer at the empty environment
      would contradict blockedness), with totality-for-free at the empty
      environment supplying the answer to case on.
    * THE ONE SIDE CONDITION, named not hidden: `hfix` — σ may not refine the row
      the stump is blocked on. This is the ?-arm's single asymmetry against the
      definite arms (`Sol.lookup_toCtx_sat` transports those under mere `Sat` and
      explicitly EXCLUDES `?`), and it is the same fact from the other side as
      "A-sel-? parks instead of committing to ★": a later σ can solve the blocker
      and make the lookup land. It is why finalization runs last. Discharged by
      idempotence (`Sol.Applied`, hence `UnifyWF`) at σ := ⟦S⟧, and by
      `FixedOutside` at any χ that moves only the δ's.
    * `InferSoundC` — inference soundness in the CONSTRAINED form, without the
      `parked = []` hypothesis, its Δ being the σ-images of the stumps the run
      left parked. That hypothesis is what made `InferSound` unprovable at an
      inner A-sel-?; this statement does not carry it, and its cases are the
      existing per-rule lemmas with `QTyped.toC` applied.
    * `QTypedCDischarge` — the remaining obligation, named: a constrained typing
      whose assumptions all discharge is a plain one. The QTypedC counterpart of
      `qtyped_applySubst` and the same kind of work — transport along χ, with χ
      moving only the δ's and the δ's fresh for Γ. A module's worth, not a lemma;
      this is the honest edge of Stage 3.
    * `runSound_of_inferSoundC_nil` — the join where it is free: a run that left
      nothing parked needs only the induction, no transport. `fStarEx_runs` is
      such a run, which is the case saturation created.
  STILL OPEN HERE: `QTypedCDischarge`, `InferSoundC` itself, the spent-δ case
  (below), and `RunSound`.
  Two more things the premise buys, neither of them cosmetic: it is what makes
  `Stump.Discharge.unk` available, so it is the hypothesis a
  "finalization is sound" lemma needs at all; and it is what `qLet`'s
  non-standard INHABITATION premise rests on ("a carried stump always finalizes
  at ★"), which is currently prose. Still open next to it: `Finalize` does not
  require `p ∈ S.parked`, there is no `⇓*` closure beside `Wakes`, and there is
  no top-level entry judgement for `Run` — §B's last ✘ row — so `InferSound`'s
  `S′.parked = []` hypothesis cannot yet be discharged by anything.
  **THE SPENT PROMISE — STAGE 4, DONE 2026-09-18, AND THE EARLIER READING OF IT
  WAS WRONG.** If δ is already solved to a non-★ type when finalization reaches
  the stump — A-app writes into δ whenever the selection's result is USED — then
  `δ ≐ ★` clashes (★ is rigid) and F-★ has no derivation, so the run cannot
  finalize. That much was right, and it is now a stated verdict rather than a
  stuck derivation: `Ty.Spent` + `no_finalize_of_spent` (Infer.lean), a hard
  error by the same discipline a clash is rejected under (no rule applies).
  WHAT WAS WRONG: "by §D that program has no declarative derivation either, so
  rejecting is right". It does have one. `λx. λy. (x.l) y` runs to a state that is
  QUIESCENT and otherwise sound, with the stump still blocked, and cannot be
  finalized (`spentEx_infers`, `spentEx_cannot_finalize`) — while the program is
  typeable at `{(l: 𝓫 → 𝓫)} → 𝓫 → 𝓫` (`spentEx_declarative`). So this is the
  algorithm's **INCOMPLETENESS**, not the declarative system's rejection, and it
  is not §D's ★-elimination gap either, because no ★ is ever formed. The real
  gap: the algorithm commits `x` to `{r}` with `r` abstract and never guesses a
  concrete row, so its only possible answer is to CARRY `⟨r.l ↓ (α_y → β)⟩` —
  which cannot be written, because `Stump.res` is a `TyVar`. A stump's result
  position holds a variable, not a type.
  THREE EXITS, in increasing cost:
    * leave it a hard error and record the incompleteness — what the rules do now;
    * `Stump.res : Ty B`, so a spent promise is expressible. The discharge arms
      survive as they are (`hit` already compares up to ≈; `abs`/`unk` demand ★,
      which a spent arrow simply fails), but it touches `Stump`, `Discharge`,
      `QScheme.WF`, `selQ` and every principality theorem built on them;
    * a consistency relation `τ ~ ★` beside `≐`, which §D already prices as a
      real extension rather than a gap.
  GENERALIZATION INHERITS IT: A-let carries stumps into a scheme whose `QScheme.WF`
  wants each `res` among the binders, and a spent δ is not a binder. So whichever
  exit is taken, it is taken for both sites.
  This is a completeness limitation with a two-binder witness, which is the form
  the write-up wants.

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
  - *vs. L1*: BOTH directions — DONE 2026-09-18. ⊆ is Typed.toQ; the converse is
    `l1_rejects_two_use` / `l1_strictly_weaker` (Qualified.lean), so **L1 ⊊ L2 is a
    THEOREM**, no longer prose. The proof does NOT reuse no_plain_principal_scheme
    (that one is pinned to τ₀ = {ε} and to syntactic instance types); it re-runs the
    same argument mod ≈:
      * the `a` use pins a DEFINITE result — projecting label a out of the record
        type through `lookup_equiv` + `lookup_det` gives τa ≈ 𝓫_c, so the scheme has
        an instance whose result is 𝓫_c;
      * the `b` use forces an ε DOMAIN, and then instance-closedness forces that
        instance's RESULT to ★ (selEx_dom_empty_res, i.e. sel_var_unk read through
        the λ). A ★ domain is excluded outright: a selection on a ★-bound variable
        has NO typing at all (sel_var_of_unk), since every selection rule demands
        the scrutinee at a record type and ★ is ≈-rigid;
      * so σ.body's result position is a bare quantified variable, the domain cannot
        depend on it, and re-pointing it inside the ⊥-use's substitution yields the
        underivable instance {ε} → 𝓫_c.
    THE ONE STEP THAT IS NOT IN no_plain_principal_scheme is the last: there the
    ⊥-instance's domain was SYNTACTICALLY {ε}, here it is only ≈ {ε}, so "the domain
    does not mention the result variable" had to be earned. `Row.hasSing` +
    `rowEquiv_hasSing` + `hasSing_applySubst` + `applySubst_rowOnly` (Qualified.lean)
    do it: ≈ never creates or destroys a field, so a row ≈ ε is field-free ANYWHERE,
    a field-free row has no TYPE positions at all, and its substitution image
    therefore reads only θ.row — the re-point cannot reach it. That quartet is
    general and reusable; it is the field-count invariant in the form the
    mixed-instance construction needs.
    New L1 inversions this needed, none of which existed: `typed_let_inv'`,
    `typed_app_inv'` (typed_inv_aux only covers con/lam/rcd) and `tvar_inv` (the
    general-scheme `var_inst_inv`). `sel_var_unk` / `var_inst_inv` are no longer
    `private` in minimal.lean. Axiom-clean.
    A second, independent proof of the same theorem existed on
    worktree-prec-equiv-commutation (lean/Strictness.lean, 2026-09-15:
    `l1_strictly_weaker_than_l2` / `no_plain_scheme_two_use`). It was dropped as
    duplicate when that branch was merged — do not re-derive it.
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

