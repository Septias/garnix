
# 1. The rule today

Declaratively, T-let has three premises and is unremarkable:

    Γ ⊢ σ ok   (∀ τ₁. σ ≥ τ₁ ⟹ Γ ⊢ e₁: τ₁)   (∃ τ₁. σ ≥ τ₁)   Γ·(x: σ) ⊢ e₂: τ₂
    ------------------------------------------------------------------------------ T-let

All the weight sits in A-let. In Lean (`Infer.letE`, Infer.lean:944) it has
twelve premises:

| #  | Premise (Lean)                                          | Paper (algorithmic.typ:510)        |
|----|---------------------------------------------------------|------------------------------------|
| P1 | `Infer Γ S e₁ τ₁ S₁`                                    | same                               |
| P2 | `S₁.kinds.Assigns ᾱ κs`                                 | `κ̄ = S₁(ᾱ)`                        |
| P3 | `S₁.parked.Perm (Δq ++ Δγ)`                             | `Δ₁ ~ Δ_q ⊎ Δ_Γ`                   |
| P4 | `∀ p ∈ Δq, p.blocker ∈ ᾱ`                               | only in a comment                  |
| P5 | `∀ p ∈ Δγ, p.blocker ∉ ᾱ`                               | only in a comment                  |
| P6 | Γ-freshness `ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅`                        | same                               |
| P7 | ownership `Δq ∩ S.parked = ∅`                           | `Δ_q ∩ Δ = ∅`                      |
| P8 | `LetResults S₁ ᾱ Δq` (5 clauses, see §3)                | `results(Δ_q) at S₁ ok over ᾱ`     |
| P9 | `ᾱ ∩ ftv(⟦S₁⟧Δγ) = ∅` (row, key, result)                | same                               |
| P10| `ᾱ ∩ dom(S₁) = ∅`                                       | same                               |
| P11| independence: no Δq row/key mentions a Δq result var    | `Δ_q independent at S₁`            |
| P12| `Infer (Γ·x:σ) (S₁ with parked := Δγ) e₂ τ₂ S₂`         | same                               |

Plus, outside the rule: `greatestAlpha` (LetChoice.lean:426) picks the
greatest admissible ᾱ, and `runF` uses it (InferFn.lean:607). The relation
itself accepts any admissible ᾱ.


# 2. Critique of the presentation

- **The rule mixes three jobs**: inference (P1, P12), *choosing* ᾱ (P2-P11),
  and *checking* that the choice is sound. HM keeps the choice in a function
  (`gen`) and the rule stays two lines. Here the choice is spread over ten
  premises, and the fact that ᾱ is computed (`greatestAlpha`) is invisible in
  the rule.
- **P4/P5 are definitions, not premises.** Δ_q is *determined* by ᾱ
  (`letQ`/`letG` in LetChoice.lean already define them as filters). The paper
  rule does not even state them; it puts them in a comment.
- **P8 is opaque.** "results(Δ_q) at S₁ ok over ᾱ" hides five conditions: bound
  results, linear pattern, nodup, pairwise disjoint results, and a
  three-part spent clause. The reader cannot check the rule without the
  comment block, which is longer than the rule.
- **Notation.** `Δ_q ∩ Δ = ∅` compares stumps of S₁ with stumps of S, i.e.
  by stump, not by entry; `κ̄ = S₁(ᾱ)` reads like a lookup in Γ.
- **Failure is not "monomorphic let".** The thesis and `incompleteness.md`
  say a failing premise makes the binding monomorphic. Wrong granularity:
  `greatestAlpha` prunes only the *offending* variables and what they drag
  along. The let is partially generalized. The cases below happen to lose
  everything because the pruning cascades (see §5, nested selection).


# 3. What each premise is actually for

Traced through the soundness proof:

| Premise              | Needed by                                     | Without it                                           | Verdict                              |
|----------------------|-----------------------------------------------|------------------------------------------------------|--------------------------------------|
| P2 kinds             | instantiation draws at a recorded sort        | ᾱ var with no sort                                   | bookkeeping                          |
| P3-P5 split          | definition of Δq/Δγ                           | n/a                                                  | definitional                         |
| P6 Γ-fresh           | HM soundness                                  | `λy. let z = y in z ⇒ a → b`                         | **essential**                        |
| P7 ownership         | finalization reaches every stump              | `{a = λx. x.l, b = let y = c in c}` loses a's stump  | **essential**, maybe derivable (§4.3)|
| P8a results ⊆ ᾱ      | `QScheme.WF`                                  | a constraint pins a global var per instance          | **essential**                        |
| P8b linear pattern   | `Correctable.correct` only                    |                                                      | **incidental** (§4.4)                |
| P8c nodup            | `Correctable.correct` only                    |                                                      | **incidental**                       |
| P8d disjoint results | `Correctable.correct` only                    | `instEquivCorrects_false`                            | **incidental**                       |
| P8e spent fillable   | T-let's `∃ τ₁` (inhabitation)                 | vacuous scheme, progress fails                       | **essential as a property**,         |  
| P9 Δγ-fresh          | parked stumps read the same at every instance |                                                      | **essential**                        |
| P10 unsolved         | nothing a Clean state does not give           |                                                      | likely redundant (§4.1)              |
| P11 independence     | `Correctable.correct` only                    | `instEquivCorrects_false`                            | **incidental**                       |

The key observation: `QScheme.Correctable` is used in exactly one place,
the `qVar` case of `QTypedA.toQTyped` (InferSoundA.lean:440), to turn an
instance that discharges its constraints **up to ≈** (`QScheme.InstA`,
`Stump.DischargeEquiv`) into one that discharges them **exactly**
(`QScheme.Inst`, `Stump.Discharge` with `s.res.applySubst θ = τ`,
Qualified.lean:110). The `qLet` case only reads `hcs.1`, i.e. `WF`.

So P8b, P8c, P8d and P11 exist only because the declarative D-hit is
syntactic equality while the algorithm can only guarantee ≈.


# 4. Reductions

## 4.1 Bookkeeping into definitions (no proof impact)

- Δ_q := { p ∈ Δ₁ | blocker(p) ∈ ᾱ }, Δ_Γ := the rest. Drop P3-P5 from the
  paper rule.
- Choose ᾱ among ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ_q) instead of `KEnv.dom S₁.kinds`. With a
  Clean S₁, solved variables never occur in anything read at S₁, so P10 holds
  by construction, and every such variable was drawn, so P2 holds by
  construction. **[check]**: needs a "every ftv of a state-read type is
  kinded" invariant; `Infer.keeps` (ParkedInv.lean) is the place.

## 4.2 ᾱ as a function

Write the rule HM-style and move admissibility into its own judgement:

    ᾱ = gen_{Γ,S}(S₁, τ₁)        gen = greatest admissible ᾱ (greatestAlpha_spec)

The Lean relation can keep its explicit form; the paper should not. This alone
takes the rule from twelve premises to three, without changing anything.

## 4.3 One freshness premise instead of three

P6 + P9 say: ᾱ is fresh for the *environment*, where the environment is Γ
together with the stumps that stay parked. That is the HM(X) side condition,
and it reads as one premise:

    ᾱ ∩ ftv(⟦S₁⟧(Γ, Δ_Γ)) = ∅

P7 (ownership) is the odd one out: the captured stump's blocker is in neither Γ
nor Δ_Γ, it is a stale variable of an outer λ whose binder has left scope.
Conjecture: P7 follows from P6 + "ᾱ ⊆ variables drawn during e₁" (level-style
generalization). An outer stump can only be re-parked onto a variable drawn in
e₁ if e₁ solved its old blocker, and e₁ only reaches outer variables through
Γ, in which case P6 already excludes the new one. **[check]**: the chain
argument is not obvious when the connection goes through another outer stump
rather than Γ; hunt for a counterexample first (`Fuzz.lean` has the machinery).

## 4.4 Discharge up to ≈ (the big one)

Change D-hit from `θδ = τ_r` to `θδ ≈ τ_r`, i.e. make `QScheme.Inst` use
`Stump.DischargeEquiv`. Then:

- `toQTyped`'s qVar case is immediate, `Correctable.correct` is not needed,
  and `instEquivCorrects_false` stops mattering
- P8b (linear pattern), P8c (nodup), P8d (disjoint results) and P11
  (independence) can all be dropped
- the declarative system loses nothing: instance sets were already ≈-closed
  through T-eq, the scheme now says so directly

What has to be re-proved in Qualified.lean, with the tools that exist:

- substitution stability of discharge: `RowEquiv.applySubst` + `LookupQ.equiv`
  (LabelLookup.lean:357) give `(σθρ).l ↓ τ′` with `τ′ ≈ σθδ`
- let-β in `qPreservation`: unchanged shape, T-eq absorbs the ≈
- `selQ_principal`, `selQ_instance_closed`, `l1_strictly_weaker`: statements
  about `Inst`, restated up to ≈; one-constraint schemes, so mechanical
- D-⊥ / D-? stay exact (`θδ = ★`); ★ is ≈ only to itself, no change

Risk: moderate. The change touches `Inst`, which every scheme lemma reads.
The payoff is four premises and the whole correction machinery
(InferSoundA.lean:40-410).

## 4.5 Spent results: an inhabitation check, not a syntactic test

P8e demands that a generalized spent stump be *fillable*: literal key,
every stump on the same blocker literally keyed and identical if it has the
same key, no spent blocker inside a spent result. That is a sufficient
syntactic test for what T-let actually needs, `∃ τ₁. σ ≥ τ₁`.

Replace it by the property itself, decided by construction: run F-hit
(`⇓ₘ*`) on a scratch copy of Δ_q's spent stumps, with unknown keys sent to
fresh labels (`fillRow`). Success gives the witness instance. Failure means
the constraints are contradictory (same field demanded as two non-unifiable
types, or the blocker inside its own result: occurs) and the rejection is
justified. Guessing a key is fine here because it only builds a witness for
the proof; it never appears in the reported type (proof-state.md # Problems
makes the same distinction).

**[check]** whether contradictory spent constraints can be rescued by a less
general declarative σ. I do not see how: any declarative typing of e₁ is an
instance satisfying all of them.


# 5. Incompleteness, before and after

| Case                              | Example                                        | Failing premise                                                                                  | After §4                                                             |
|-----------------------------------|------------------------------------------------|--------------------------------------------------------------------------------------------------|----------------------------------------------------------------------|
| Nested selection                  | `let g = λx. (x.l).m in {a = g r₁; b = g r₂}`  | P11 (p₂'s row γ is p₁'s result `{γ}`), cascades through P8a and P9 until β, γ, δ₂ are all pruned | **fixed** by §4.4                                                    |
| Record literal in a spent result  | `λx. (x.l) {a = c}`                            | P8b (`{a: 𝓫} → β` is not a linear pattern)                                                       | **fixed** by §4.4                                                    |
| Same field spent twice on one row | `λx. {a = (x.l) c; b = (x.l) c}`               | P8d / P8e                                                                                        | **fixed** by §4.4 + §4.5 (one materialization hits both)             |
| Key-blocked spent stump           | `let f = λr. λa. r.(a) c`                      | P8e (no literal key)                                                                             | **fixed at the let** by §4.5; the top-level report stays kind 9      |
| Γ-mentioned variable              | `λy. let z = y in z`                           | P6                                                                                               | justified, HM                                                        |
| Outer stump capture               | `{a = λx. x.l, b = let y = c in c}`            | P7                                                                                               | justified                                                            |
| Δ_Γ-mentioned variable            | `λr. let f = λy. r.l in …` keeps δ monomorphic | P9                                                                                               | justified, and costs nothing: every instance would read the same r.l |

Nested selection, after §4.4: the scheme is

    g : ∀β γ δ₂. ⟨β.l ↓ {γ}⟩, ⟨γ.m ↓ δ₂⟩ ⇒ {β} → δ₂

and A-var handles the chain without help: instantiating at `{l = {m = c}}`
wakes the first stump, `{m:𝓫} ≐ {γ′}` solves γ′, saturation wakes the second.
The order the correction argument needed is supplied by saturation, so no
"ordered discharge" is needed either.

What remains is exactly the HM part: P6, P9, and P7 (if §4.3's conjecture
fails).


# 6. Proposed rule

    Γ; S ⊢ e₁ ⇒ τ₁; S₁      ᾱ = gen_{Γ,S}(S₁, τ₁)
    Γ·(x: ∀ᾱ. ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ ∖ Δ_q ⊢ e₂ ⇒ τ₂; S₂
    -------------------------------------------------- A-let
    Γ; S ⊢ let x = e₁ in e₂ ⇒ τ₂; S₂

    gen_{Γ,S}(S₁, τ₁) = the greatest ᾱ ⊆ ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁) with
      ᾱ ∩ ftv(⟦S₁⟧(Γ, Δ_Γ)) = ∅          environment-fresh (P6 + P9)
      Δ_q ∩ Δ = ∅                        ownership (P7, unless §4.3 derives it)
      ftv(⟦S₁⟧ results(Δ_q)) ⊆ ᾱ         results bound (P8a)
      Δ_q inhabited                      §4.5 (replaces P8e)

Twelve premises become three, and admissibility shrinks from nine conditions
to four. Union-closure (`LetAdmissible.union`) should get *easier*: the
clauses that needed Δ_Γ's premise to survive union (correctability,
independence, the spent clause) are gone. Inhabitation is not obviously
union-closed **[check]**: two separately inhabited Δ_q could conflict. If
not, keep the current syntactic test as the union-closed approximation, or
fall back to the greatest ᾱ under the other three conditions and demote
variables of uninhabited stumps.


# 7. Rejected alternatives

- **Materialize spent stumps at the let** (commit `β ≔ (l:δ | β′)` before
  generalizing). The move is forced for every instance, so it looks
  principal, but it changes the *shape* of the scheme. A use site that passes
  an open record `(m:𝓫 | γ)` then meets `(l:δ′ | β″) ≐ᵣ (m:𝓫 | γ)`, a
  crossfield (incompleteness kind 5), and goes stuck. The qualified scheme
  instead sets `β′ ≔ (m:𝓫 | γ)`, re-parks on γ and materializes at
  finalization. Keep spent stumps qualified; use materialization only as the
  inhabitation check.
- **Materialize eagerly, as soon as a result is spent** (a K-rule). Same
  hazard inside e₁, with no let boundary to protect it.
- **HM(X)-style duplication for P9**: put stumps blocked on Γ but mentioning
  ᾱ into the scheme *and* keep them parked. Sound, but the only thing it
  generalizes is a result every instance reads identically. No gain.
- **Ordered discharge** in place of independence: needs a well-founded order
  on stumps threaded through `Correctable.correct`. Subsumed by §4.4, which
  removes the correction altogether.


# 8. Order of work

1. §4.4 first: it is the largest reduction and fixes three of the four
   incompleteness cases. Start by changing `Stump.Discharge.hit` to ≈ and
   letting the build list what breaks in Qualified.lean.
2. §4.5, with the union-closure question settled before touching LetChoice.
3. §4.3's conjecture: counterexample hunt in Fuzz before any proof.
4. §4.1 / §4.2: paper-only once 1-3 are done; the Lean relation can follow.
