
## The one shared axis

Both extensions move the same notion in opposite directions:

    can this atom carry/shadow a field named l ?

  - FC-labels ADDS carriers: a var-labeled field `α: τ` may become any label, so
    it obstructs exactly like a row-var (fc §5, the *barrier* definition).
  - Negative info REMOVES carriers, label-relatively: `β ⊬ l` means β cannot
    host l, so at label l it stops obstructing (neg §2b, §6).

Everything below follows from the fact that this is one predicate, not two.

    canCarry Γ l a  :=  a = row-var β    and  (β ⊬ l) ∉ N
                     ∨  a = (ℓ: τ)       and  ¬ (Γ ⊢ ℓ # l)

**Define it once, context-taking and label-indexed, before either plan is
mechanized.** Both plans otherwise refactor the same detectors independently:
fc §6b rewrites `sHasVar` → `sHasBarrier` (a boolean), neg §6 rewrites the same
sites to `lacksAt l s` (label- and N-relative). The second refactor subsumes the
first; doing the boolean one first means doing S4 twice.


## The collision: who is right about U-clash

Both plans single out `projClash` / U-clash as the one site where getting the
refactor wrong destroys a *proved-sound* verdict — and they push it in opposite
directions.

  - fc §6b: without the barrier generalization, `(foo: τ) ≐ᵣ (α: τ′)` is reported
    clash though `α ≔ foo` solves it. Clash must fire LESS.
  - neg §6: `vars(s₂) = ⟨⟩` really means "nothing over there can carry l", which
    is weaker. Clash must fire MORE.

There is no conflict — they are the two halves of `canCarry` — but there is
exactly one soundness proof and it has to be done against the combined
predicate. Sequencing consequence: write the regressions from *both* plans
(fc §11's `(foo: τ) ≐ᵣ (α: τ′)` and neg §8's wand-with-β⊬l) into one suite, and
land U-clash once.


## The collision that is real: label-variable atoms

`⟨ρ.α ↓ ⊥⟩` with α a *label* variable is where the plans genuinely disagree with
themselves. neg §11 already flags it; the sharp version:

  - neg §1's decomposition lemma — the load-bearing one, the thing that keeps
    the constraint language a finite set of (row-var, label) pairs with
    membership as entailment — **requires the label to be a literal**. Keyed by a
    label-var, entailment becomes three-valued (`≡`/`#`/undecided) and the set
    becomes a term language with its own solving. That is precisely the
    qualified-types machinery neg §7 argues we are entitled to skip.
  - Inhabitation (neg §4, restriction B) survives either way: `θα ≔ ε` satisfies
    every negative atom regardless of how its label is keyed, since ε lacks
    everything. So the cost is entailment, not vacuity.

DECISION, to be taken now and written into both plans: **every negative atom
carries a literal label.** Revisit only after both extensions have landed
separately. This is the conservative answer both plans independently reach.


## Stuck: they do not cancel, and they do not compose

  - fc GROWS the stuck class — every undecided field pairing is a new stuck
    shape, and U-expand refuses more often because a var-labeled field counts as
    a candidate host (fc §6b).
  - neg SHRINKS it — neg §8's experiment, where one atom restores uniqueness in
    the wand configuration and U-expand fires.

The shapes are disjoint: a negative atom on a *row*-var resolves nothing about
an undecided *label* pairing. The only thing that would rescue fc's new stuck
shapes is a label-var-keyed atom — i.e. exactly the feature the section above
defers. Say this plainly in the thesis rather than letting the two claims sit in
different chapters: negative information improves the wand shapes, FC-labels add
shapes it does not reach.


## Shared metatheory — state once, not twice

Both plans reopen the same three lookup theorems, and for the same structural
reason: each adds a second source of improvable `?`.

| obligation     | fc adds                               | neg adds                        |
|----------------|---------------------------------------|---------------------------------|
| `lookup_det`   | the ≡/#/undecided split (L-?-lab)     | the L-α-⊥ arm                   |
| `lookup_mono`  | ? improves when a label-var is solved | ? improves when an atom arrives |
| `lookup_total` | rank argument over label solutions    | nothing (atoms are facts)       |

So do not restate monotonicity twice over two different extension orders. Define
the context extension order ONCE with three components — row solutions ∪ label
solutions ∪ negative atoms — and prove `lookup_mono` against it. `⊑ᵣ` is
unchanged in both plans (`r ⊑ᵣ ?` already has the right shape), so the precision
story absorbs both without a new relation.

Same for generalization: fc carries Label-sorted variables into ᾱ, neg carries
atoms whose subject lands in ᾱ. Both feed the *same* least fixpoint in A-let;
the bound becomes |Δ| + |N| and the argument is otherwise unchanged.


## Shared wake-up generalization

Today: Δ indexes a stump by a row-var blocker, wake-up fires when a row solution
is written. Both plans widen this, and the widenings are independent:

    blocker    b ::= row-var | label-var                  (fc §7)
    trigger      ::= solution written | atom added        (neg §6)

Implement the blocker and the trigger as two separate generalizations of the
same index. Neither plan's monotonicity argument is disturbed — a definite result
stays definite under both — but the argument should be made once over the pair.


## Cost and ordering

neg is cheap (no constructor is added to `Row`/`Ty`, so RowEquiv and the success
leg do not move; exposure is confined to the detectors and U-clash). fc is
invasive (`Row.sing` changes shape, which breaks minimal.lean, RowEquiv.lean and
all of RowUnify/, and U-ground's counting — the one fully-done axiom-clean
result — is exactly what var-labeled fields perturb).

Recommended order:

  1. **neg N0** (neg §8) — the rfl experiment on the refuted configurations.
     Throwaway, one afternoon, and its answer ("how many stuck shapes does one
     atom rescue?") is the input to how much of *either* plan is worth writing.
  2. **`canCarry`** — the shared predicate, label-indexed and context-taking from
     the start, with both regression suites. Even if the FC half is stubbed.
  3. **fc S0+S2** (LabelExp, lookup) — cheap, and they are what tell you whether
     the lookup metatheory survives before S1's mechanical breakage is paid for.
  4. Then the expensive halves, in whichever order the deadline allows.

The de-risking option in fc §9 (present FC-labels paper-only) applies to the
overlap too: `canCarry` and the combined extension order are worth defining on
paper even if only the negative half is mechanized.


## One editing pass, three paragraphs

Both plans require revising the *same* thesis text. Do it once:

  - @related-work's argument against qualified row predicates — neg §7 adds a
    predicate, so the paragraph has to distinguish "needs a search" from
    "needs a lookup" rather than rejecting the category.
  - §Sorts' claim that FC-labels "add a third sort and change nothing else about
    the discipline" — true of instantiation, false of ≈ and ≐ᵣ (fc §3).
  - whatever the thesis ends up saying `stuck` *means* — fc adds shapes, neg
    removes shapes, and the honest sentence mentions both.
