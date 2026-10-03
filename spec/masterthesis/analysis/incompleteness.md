# Overview
The contract is: *success ⟹ sound, clash ⟹ no unifier, occurs ⟹ no unifier,
stuck ⟹ nothing*. Every kind below is a place where the run fails (`stuck`,
`clash`, or a stump that is never answered) although the declarative system
types the program. A `stuck` from `≐ / ≐ᵣ` is a run failure, not a ★.

| #  | Kind                      | Shape                                        | mgu?                       | Verdict | Fixable by                             |
|----|---------------------------|----------------------------------------------|----------------------------|---------|----------------------------------------|
| 1  | Wand                      | `(β \| α) ≐ᵣ (l:𝓫)`                          | no (2 incomparable)        | stuck   | negative info + a host move            |
| 2  | Levi / two-sided          | `(α \| l:𝓫) ≐ᵣ (l:𝓫 \| β)`                   | no (infinitely many)       | stuck   | negative info only with Γ-relative ≈   |
| 3  | Swap                      | `(α \| β) ≐ᵣ (β \| α)`                       | no                         | stuck   | row equations as qualifiers            |
| 4  | Shift                     | `(α \| l:𝓫) ≐ᵣ (l:𝓫 \| α)`                   | no finite CSU              | stuck   | row equations as qualifiers            |
| 5  | Crossfield                | `(l:𝓫 \| α) ≐ᵣ (m:𝓫 \| β)`                   | yes                        | stuck   | unique-host expansion                  |
| 6  | Self-referential host     | `(l:{w}) ≐ᵣ (w \| v)`, `(l:{w}) ≐ᵣ (v \| w)` | yes                        | stuck   | unique-host expansion + depth filter   |
| 7  | Masked by sequencing      | `(k:{β\|α} \| β) ≐ᵣ (k:{l:𝓫} \| l:𝓫)`        | yes                        | stuck   | defer stuck sub-equations              |
| 8  | Keyed barriers            | `(foo:𝓫) ≐ᵣ (${α}:𝓫)`, `${α}:𝓫 ≐ᵣ ${β}:𝓫`    | yes (both)                 | stuck   | key-binding move at singleton/ends     |
| 9  | Key-blocked spent promise | `λr. λa. r.(a) c`                            | n/a (inference)            | no run  | guess key / qualified top level        |
| 10 | Monomorphic let           | `let g = λx.(x.l).m in …`                    | n/a (inference)            | clash   | ordered discharge (partly)             |
| 11 | Unrestricted T-★-intro    | `λg. {a = g {l=c}; b = g {m=c}}`             | n/a (typing, not unifier)  | clash   | blur-free completeness / join on clash |

Grouping, by how inevitable they are:

- **Irreducible** (1-4): no mgu exists. No algorithm returning one unifier can
  do better. A limit of principal types over scoped rows.
- **Forced-move cost** (5-8): an mgu exists, the algorithm does not take the
  step that finds it. A design choice (no move invents structure), each fixable
  in isolation.
- **Inference-level** (9-11): unification is not the culprit. Either a stump
  is never answered, a binding is not generalized, or the declarative system
  uses a rule (T-★-intro) with no algorithmic counterpart.

Out of scope here, because the declarative system rejects them too: ★ has no
eliminator, a non-label key is a type error. See # Not incompleteness.


# 1. Wand  `(β | α) ≐ᵣ (l:𝓫)`

**Unifier set.** The right side has one field and no variable, so
`θβ | θα ≈ (l:𝓫)`: by counting exactly one of θβ, θα carries the l-field,
the other is ε. Two unifiers, `{β ≔ ε, α ≔ l:𝓫}` and
`{β ≔ l:𝓫, α ≔ ε}`, neither an instance of the other.

**Why stuck.** Every move is dead: no
cancellation, no lone variable, no window match (the left side has no
fields), no ground match (left side has vars), no projection clash (left side
has vars). Nothing else could be reported.

**Program.** Any monomorphic function used at a concatenation and at a literal:

    λa. λb. λk. {x = k (a ‖ b); y = k {l = c}}

Declaratively typeable (`a : {}`, `b : {l:𝓫}`, or the other way round), but no
typing is principal. Illustrative, not run in Lean.

**Not to be confused with** the *lookup* wand `(a ‖ b).l`. That one never
reaches unification: the selection parks a stump blocked on `a`'s row, and
finalization answers ★. The unification wand is the same ambiguity arriving
through `≐` instead of `↓`, and it has no ★ fallback.


# 2. Levi / two-sided  `(α | l:𝓫) ≐ᵣ (l:𝓫 | β)`

**Unifier set.** Infinite and unbounded in two directions:
- `α ≔ ε, β ≔ ε`
- `α ≔ (l:𝓫 | γ), β ≔ (γ | l:𝓫)` for fresh γ (both sides become `l | γ | l`)
- `α ≔ R, β ≔ R` for any var-free R without l-fields (distinct labels commute)

The first has l-count 0, the second ≥ 1 at α: incomparable. This is the Levi
lemma for word equations (`xa = ay`), and reason for the name.

**Why stuck.** Both windows are closed by a variable at the other end (the
leading field on the right faces `α` on the left, the trailing field on the
left faces `β`), both sides have variables, so no ground match or clash.

**Lean.** `two_sided_no_mgu_on` (NoMgu.lean:268), `unify_two_sided_stuck`
(Regressions.lean:94).

**Program.**

    λa. λb. λk. {x = k (a ‖ {l = c}); y = k ({l = c} ‖ b)}

**Fixes.**
- The thesis claims negative info solves Levi. With context-free ≈ this does
  NOT hold. Take `α ⊬ l`. The candidate mgu is `α ≔ γ, β ≔ γ` with `γ ⊬ l`,
  but `γ | l:𝓫 ≉ l:𝓫 | γ`: a variable is a barrier, whatever is known about
  it. The surviving unifiers are `α, β ≔ R` for var-free l-free R, plus
  nothing more general. Still no mgu.
  - It works only if ≈ reads atoms (`γ ⊬ l` lets γ commute with l). The
    negative-info plan §5 explicitly declines a Γ-relative ≈.
  - So: either drop "Levi" from the claim in thesis @incompleteness-irreducible,
    or adopt Γ-relative ≈ and pay for it in RowEquiv. Needs a check, not yet
    kernel-checked either way.
- Row equations as scheme qualifiers: carry `(α | l) ≈ (l | β)` as a
  constraint. Principal by construction, at the cost of an equational
  constraint language (the thing P&X / ROSE pay for).


# 3. Swap  `(α | β) ≐ᵣ (β | α)`

**Unifier set.** `α ≔ l:𝓫, β ≔ ε` and `α ≔ ε, β ≔ l:𝓫` for every l, plus
`α ≔ β`, plus `α ≔ γ, β ≔ γ`. The last two are not most general: neither
covers `α ≔ l:𝓫, β ≔ ε`. The all-variable stuck class.

**Lean.** `allvar_swap_no_mgu_on` (NoMgu.lean:977).

**Fix.** None short of qualifiers. Negative info does not help, there is no
field to be absent.


# 4. Shift  `(α | l:𝓫) ≐ᵣ (l:𝓫 | α)`

**Unifier set.** α must commute with the l-field: `α ≔ R` for any var-free R
whose l-fields all have type 𝓫. One maximal unifier per l-count k. No finite
set of unifiers covers them all.

**Difference to Levi.** Same variable on both sides: not a choice of split but
a *count*. Counting is what kills finite complete sets.

**Lean.**
- `shift_no_finite_complete_set` (UnifType.lean:293): no finite complete set,
  relativized to `[α]`
- `unify_shift_stuck`, `unify_shift_stuck_mirror` (Regressions.lean:103)
- `unify_shift_inst_zero`, `unify_shift_inst_one`: the k = 0, 1 instances
  succeed, so stuck is not a clash in disguise

**Program.**

    λa. λk. {x = k (a ‖ {l = c}); y = k ({l = c} ‖ a)}

**Fixes.**
- Returning several answers does not help (no finite CSU)
- Negative info does not help with context-free ≈ (same argument as Levi)
- Row equations as scheme qualifiers: the only principal answer


# 5. Crossfield  `(l:𝓫 | α) ≐ᵣ (m:𝓫 | β)`, l ≠ m

**Unifier set.** `β ≔ (l:𝓫 | γ), α ≔ (m:𝓫 | γ)` is the mgu. Every unifier
puts an l-field at the front of β (`crossfield_host_forced`,
Solutions.lean:936), and symmetrically for α.

**Why stuck.** The step is forced but it *invents* γ, and no move invents
structure since U-expand was dropped (`plans/archive/drop-expand.md`). The
drop was a deliberate trade: U-expand was the only arm that renamed instead of
substituting, and it was behind both vacuous-success witnesses, the triangular
`Sol`, and the `depReach` disjunct blocking `occurs ⟹ ¬∃θ`.

**Lean.**
- `crossfield_stuck` (Driver.lean:71): the single most important regression
  for "no expansion arm crept back in"
- `unify_crossfield_mirror_stuck` (Regressions.lean:174)
- `nix_callback_crossfield_stuck` (Regressions.lean:330): the Nix shape

**Program.** The realistic one, a λ-bound callback used on two records
extended by different fields:

    f: p: q: { a = f (p // { x = …; }); b = f (q // { y = …; }); }

With a *shared* tail (`p` twice) there is no unifier at all
(`nix_callback_shared_tail_clash`), so that rejection is P&X's pitfall, not
ours. Crossfield is the one of the eleven kinds most likely to show up in
nixpkgs.

**Fix.** Unique-host expansion, but as an *applied* binding (`β ≔ (l:𝓫 | γ)`
substituted everywhere) rather than a spine-only rename. That avoids the
stale-payload problem that made the old U-expand expensive. Termination
measure has to absorb the invented γ; the stage-0 fuzz in drop-expand.md says
`Sol.Applied ⟺ no expansion fired`, so the applied variant is the one to try.


# 6. Self-referential host  `(l:{w}) ≐ᵣ (w | v)` and mirror

**Unifier set.** Hosting the l-field in w would force `θw ≈ (l:{θw} | …)`,
an occurs violation invisible to field counting (both sides count 1) but
visible to record depth (`Ty.rcdDepth`, ≈-invariant). So v is the forced host:
`w ≔ ε, v ≔ (l:{ε})`. Unique, hence most general.

**Lean.**
- `terminal_masks_mgu_terminal`, `terminal_masks_mgu_stuck`,
  `terminal_masks_mgu` (Refutations.lean:246-260): terminal, stuck, has mgu
- `terminalNoMgu_false`: so "terminal ⟹ no mgu" is false
- `selfref_filter_stuck` (Refutations.lean:445): the mirror `(v | w)`
- `selfref_lone_host_no_unifier`: the variant `(l:{w} | a) ≐ᵣ (m:𝓫 | w)` has
  NO unifier and is also reported stuck; correct, only less precise than occurs

**Why it matters.** It is the witness that kills the converse at the level of
configurations, not just of verdicts. Without it, one could hope "terminal ⟹
no mgu" and only blame sequencing (kind 7).

**Fix.** Same move as crossfield, plus the depth filter (`self_hosting_no_unifier`)
to discard hosts that occur in the payload. Watch the leading condition:
`shadow_order_matters` / `unrestricted_filter_refused` show that a host behind
another variable can reorder shadowing fields. The old `expandR` arm handled
`(w | v)` by emitting at the back.


# 7. Masked by sequencing  `(k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫)`

**Unifier set.** `β ≔ (l:𝓫), α ≔ ε`. Unique.

**Why stuck.** `matchL` pairs the k-fields and emits `{β|α} ≐ {l:𝓫}`, which is
a Wand (kind 1) and stuck. `UResM.seq` propagates that stuck before the
residual `β ≐ᵣ (l:𝓫)` is looked at, and the residual would have pinned β and
with it resolved the Wand.

**Lean.** `stuck_masks_mgu_reported`, `stuck_masks_mgu` (Refutations.lean:158,
166). `hbase_stableQ_false` is the same fact seen from the other side: the
emitted equation, as a conjunct, shrinks the Wand to a set with an mgu.

**Why it matters.** It is the reason "stuck ⟹ no mgu" is false *as a
statement about the verdict*, independently of any missing move. Every stuck
witness nested in a field payload can be rescued by a sibling equation.

**Fix.** Defer a stuck sub-equation, run the residual, retry the deferred one
under the residual's solution. This is parked stumps, moved inside
unification. Open: termination (deferred equations can be retried after each
residual step), confluence (does the order of retries matter?), and what to
report when a deferred equation is still stuck at the end. Do not attempt a
converse after this fix either: kind 6 needs no sequencing and is still
terminal.


# 8. Keyed barriers  (first-class labels, phase B)

A keyed field `${α}: τ` may become any label, so it is a barrier like a row
variable: no field commutes past it.

**8a. Literal vs key, `(foo:𝓫) ≐ᵣ (${α}:𝓫)`.** mgu `[α ≔ foo]` (label
component). Stuck (`unify_lit_vs_key_stuck`, Regressions.lean:352).

**8b. Different keys, `(${α}:𝓫) ≐ᵣ (${β}:𝓫)`.** Stuck
(`unify_diff_key_stuck`, Regressions.lean:393).

- FINDING: the justification in algorithmic.typ (U-key-R comment) and in the
  Lean comment is wrong for the singleton. It says `α ≔ β` and `α, β ≔ l`
  have no common generalization. They do: `α, β ≔ l` is `[β ≔ l] ∘ [α ≔ β]`.
  Every unifier needs `θα = θβ` as keys (≈-dfield needs the same key, ≈-dlab
  turns literal keys into fields), so `[α ≔ β]` is the mgu.
- The claim IS true in context. `(${α}:𝓫 | m:𝓫) ≐ᵣ (m:𝓫 | ${β}:𝓫)` has
  unifiers `α, β ≔ x` for every label x (x = m included), but `α ≔ β` is not
  one (`${β} | m ≉ m | ${β}`, barrier). No mgu.
- So 8a and singleton 8b are forced-move cost, contextual 8b is irreducible.
  Reword both comments and the "Costs" bullet in algorithmic.typ.

**8c. Occurs next to a keyed field.** Reported stuck instead of occurs. Both
fail the run, so this is a precision loss in the verdict, not a typing gap.

**Lean.** keyed Fuzz universe: 67% stuck (algorithmic.typ, # First-class labels
/ Costs). How much of that is 8a/8b-singleton is not measured.

**Fix.** A key-binding move when both sides are a single field (or the key is
the only barrier on both sides and the windows are otherwise empty). Small,
forced, needs no invention. Generalizing past singletons needs a proof that
the barrier sequence pins the pairing.


# 9. Key-blocked spent promise  `λr. λa. r.(a) c`

**Shape.** A dynamic selection `r.(a)` whose key a is never known. A-sel-dyn-?
parks `⟨ρ.α ↓ δ⟩` blocked on the KEY α. Applying the result to `c` spends δ:
`δ ≔ 𝓫 → β`. At finalization:
- F-★ would solve `δ ≐ ★`, which clashes with the spent `𝓫 → β`
- F-hit extends the BLOCKER row with the field. The blocker here is a label
  variable, not a row: there is nothing to extend

The stump is never answered, the run fails.

**Declaratively.** Typeable: `a : ⌊foo⌋`, `r : {foo: 𝓫 → β}`. So
incompleteness, not a justified rejection.

**Contrast with the row-blocked spent promise** `λx y. (x.l) y`:
`spentEx_infers`, `spentEx_cannot_finalize` (F-★ alone), `spentEx_declarative`
(InferSound.lean:899-929). That one is now fixed by F-hit. The key-blocked
variant has no Lean witness yet.

**Applied, it can still bite:** `(λr. λa. r.(a) c) {k = λz.z} ⌊k⌋` runs to `𝓫`.
The issue is only the open term's report.

**Fixes** (proof-state.md # Problems has the full comparison).
- A: guess the key, `α ≔ ⌊ℓ_fresh⌋`, then F-hit. Small, `runSound` carries
  over, but the answer mentions a made-up label: non-principal.
- B: qualified top-level type, `∀. ⟨ρ.(α) ↓ 𝓫 → β⟩ ⇒ {ρ} → α → β`. Principal,
  but `Run`, `RunSound`, `runF` and printed answers change, and soundness
  becomes "every instance is typed".
- B preferred. A is fine inside a proof (inhabitation witness), not in a
  reported type.


# 10. Monomorphic let

A-let generalizes only the variables that meet its side conditions;
`greatestAlpha` prunes the offending ones and whatever they drag along. In the
cases below the pruning cascades until the binding is monomorphic, and the
second use at a different record clashes (or goes stuck). Not every premise is
incompleteness. Full review and proposed reductions: `let-review.md`.


| Premise                          | Without it                                                  | Status                               |
|----------------------------------|-------------------------------------------------------------|--------------------------------------|
| Γ-freshness `ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅` | `λy. let z = y in z` infers `a → b`                         | required, justified                  |
| ownership `Δ_q ∩ Δ = ∅`          | a stump is filed under an unused scheme, never finalized    | required, justified                  |
| `ᾱ ∩ dom(S₁) = ∅`                | generalizing solved vars                                    | required, justified                  |
| `ᾱ ∩ ftv(⟦S₁⟧Δ_Γ) = ∅`           | parked Γ-stumps read differently per instance               | required, justified                  |
| results ok (linear pattern)      | an instance cannot be corrected to meet each lookup exactly | required for the correction argument |
| spent result fillable            | T-let's `∃ τ₁` fails                                        | required (inhabitation)              |
| independence                     | `instEquivCorrects_false` (Finalization.lean:345)           | relaxable                            |


**What it costs.**
- Nested selection: `let g = λx. (x.l).m in {a = g r₁; b = g r₂}`. The stump
  for `.m` sits on the result of the stump for `.l`: not independent. g is
  monomorphic, two different records clash.
- Record literal in a spent result: `(x.l) {a = c}`. The result is not a
  linear pattern, ≈ could reorder the literal and no re-choice of variables
  undoes that.
- Same field spent twice on one row: two stumps share a blocker and a label,
  the linearity condition fails.
- Key-blocked spent stump: no blocker to fill (kind 9 again, at a let).


**Fixes.**
- Independence → ordered discharge: discharge an instance's constraints in
  dependency order rather than one at a time. Would recover nested selection.
  Needs a well-founded order on stumps (the result-of relation is acyclic by
  construction, worth stating as a lemma) and a version of
  `QScheme.Correctable.correct` that threads the order.
- Linear-pattern and fillability are soundness-relevant; relaxing them means
  weakening `correct`, not the premise.
- `greatestAlpha_spec` (LetChoice.lean): ᾱ is canonical, so none of these is
  an artefact of choosing ᾱ badly.


# 11. Unrestricted T-★-intro

**Shape.**

    λg. {a = g {l = c}; b = g {m = c}}   :   (★ → 𝓫) → {a: 𝓫 | b: 𝓫}

Declaratively typed by sending both argument records up to ★. The algorithm
unifies `{l:𝓫} ≐ {m:𝓫}` and clashes. Claimed kernel-checked in
`plans/archive/fix-f-★-intro-incompleteness.md`, but the check was never
committed: no commit touching `lean/` contains it. Worth adding as a
regression.

**Why it is different from 1-10.** Here a *clash* is the failing verdict, and
clash is proved to mean "no unifier". The gap is not in unification at all:
T-★-intro is subsumption into a top type, ★ is the join of any two types, and
unification only computes common *instances*, never joins. So:
- "clash is soundness, not choice" holds for ≐, not for inference
- the same trick (`f : ★ → 𝓫`) types every stuck witness of kinds 1-8. Relative
  to the full declarative system, even the irreducible kinds have typings, just
  no principal ones

**Rejected restrictions.**
- T-★-intro only at selection results: breaks `qPreservation`, since a blurred
  `{a = ⌊l⌋}.a` steps to `⌊l⌋` (Qualified.lean:1260, 1268)
- Blur only under selection: still incomplete,
  `λg. {a = g ({x = {l = c}}.x); b = g ({x = {m = c}}.x)}` clashes

**Fixes.**
- A: state completeness against the blur-free fragment (★ only from lookup
  verdicts). Costs nothing, is honest, matches what the algorithm does.
- B: join on clash. Collect lower bounds `τ ≲ α`, set `α ≔ ★` when two clash,
  rely on the rigidity of ★ to reject later eliminations. Principal only on a
  *definite* clash; the unify-vs-★ choice in the non-definite case stays open.
  This is the step towards gradual typing (thesis @sec-gradual).


# Not incompleteness

Rejected by the declarative system too, so not a gap between the two
judgements. They are gaps against R3 (totality), argued in @sec-goals:

- **★ has no eliminator.** `{"…" = {m = 1};}.${toString builtins.currentTime}.m`
  runs and has no type: the dynamic selection yields ★, ★ cannot be selected
  from. Adding eliminators without gradual typing breaks progress and
  preservation (proof-state.md # Findings).
- **Non-label key.** `{foo = c}.(c)`, `{${c} = c}`: type error by design.
  Phase A's ★-keyed fields broke preservation.
- **⊥-lookups** are typed ★ + W. Sound, not a failure.
- **Clash / occurs** where no unifier exists (`nix_callback_shared_tail_clash`,
  `nix_callback_extension_clash`, `selfref_lone_host_no_unifier`): correct
  rejections.
