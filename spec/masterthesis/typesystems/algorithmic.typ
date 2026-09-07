== Algorithmic System
- Goal: efficiently computable, *no breaking points*
- Design inheritance: Paszke&Xie give unification for infix-extensible rows
  with row-/label-variables. We drop their *conditional tail check* (they
  reject when shadowing is unresolved) and replace it with stumps: park the
  lookup, emit ★ only when forced. Their *field-guessing* rule (LUtail) we do
  not drop outright but RESTRICT to the case where the host variable is
  FORCED (U-expand below)
- Working name: *Algorithm R*


== Architecture
S := (θ, Δ, W)
θ : substitution over type-vars AND row-vars
Δ : set of pending stumps
W : warnings (definite-absence flags, ★-degradations)


- The declarative system *reads* solutions via L-α; the algorithm *writes*
  them via unification. "θ only gets refined during solving".


== Judgments
Γ; S ⊢ e ⇒ τ; S′        (infer)
S ⊢ τ₁ ≐ τ₂ ⇝ S′        (unify types)
S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′       (unify rows)
θ ⊢ ρ.l ↓ r             (the lookup relation)

Fresh-variable discipline; ⇒-rules are syntax-directed, one per
term former, no T-eq/T-★-intro counterparts (those are what inversion-mod-≈
and re-blurring account for on the declarative side).

A-cons:  Γ; S ⊢ c ⇒ 𝓫_c; S

A-var:   x: ∀ᾱ.τ ∈ Γ   fresh β̄
--------------------------------
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S

A-lam:   fresh α   Γ·(x: α); S ⊢ e ⇒ τ; S′
------------------------------------------
Γ; S ⊢ (x: e) ⇒ α → τ; S′

A-app:   Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β
|        S₂ ⊢ τ₁ ≐ (τ₂ → β) ⇝ S₃
------------------------------------------------------------
Γ; S ⊢ e₁e₂ ⇒ β; S₃

A-conc:  Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂
|        S₂ ⊢ τ₁ ≐ {ρ₁} ⇝ S₃  S₃ ⊢ τ₂ ≐ {ρ₂} ⇝ S₄
---------------------------------------------------------------
Γ; S ⊢ e₁ ‖ e₂ ⇒ { ρ₂ | ρ₁ }; S₄

A-sel:   Γ; S ⊢ e ⇒ τ; S₁   fresh ρ   S₁ ⊢ τ ≐ {ρ} ⇝ S₂
--------------------------------------------------------
then case θ ⊢ ρ.l ↓ r:
r = τ′  ⟹  result τ′                       (T-sel)
r = ⊥   ⟹  result ★, flag (e.l, ⊥) in W    (T-sel-⊥)
r = ?   ⟹  fresh δ, park stump; result δ   (see Stumps)

A-rec:   fields as usual, literal rows are spine-var-free by construction

A-let:   see Generalization


== Stumps
A stump is a parked selection:

stump := ⟨blocker α, ρ.l ↓ δ⟩

where α is the row-var the lookup got stuck on (the ? came from L-α-free
on α) and δ is a fresh *result variable* standing for "whatever the lookup
will turn out to be".

- Declaratively, T-sel-★ types the selection ★ immediately and refinement
  means the term ALSO admits a better type later (typed_mono). Algorithmically
  we cannot return ★ immediately — *that would freeze the result and lose
  refinement* (the motivating λ-example would infer {β} → ★ and application
  could never recover τ). *The stump-var δ keeps the position writable.*
- *Wake-up*: when unification writes a solution α ≔ ρ′, wake every stump
  blocked on α and re-run its lookup under the new θ:
  found τ′  ⟹  δ ≐ τ′                 (refinement happened)
  ⊥         ⟹  δ ≐ ★, flag in W       (definite absence, T-sel-⊥)
  ? on α′   ⟹  re-park, blocker α′    (progressed to the next var)
- *Finalization* (end of inference, or generalization boundary — see below):
  surviving stumps resolve δ ≐ ★. *This is the algorithmic moment of T-sel-★.*
- Why this is sound and deterministic — the three standalone lemmas were
  built for exactly this (proof-state, Standalone Metatheory):
  *determinism*: a woken stump re-resolves to a unique result
  *monotonicity*: a resolved stump NEVER needs re-checking: found/⊥ are
  final under every future extension of θ. Wake-up lists
  never contain resolved stumps; no fixpoint iteration
  *totality*: under the occurs-check invariant (RowWF) every wake-up
  terminates with a result
- Each stump wakes at most (spine-depth of the eventual solution chain)
  times, and monotonicity de-duplicates work ⟹ with union-find on vars the
  whole solver should stay near-linear¿


== Unification: the five verdicts
≐ and ≐ᵣ return one of

- *success*
- *clash*
- *occurs*
- *stuck*   — no forced move remains. CONSERVATIVE: usually genuine ambiguity
  (Wand), but not always — see below
- *outOfFuel* — the budget ran out


*Two verdicts are conservative, not one.* `occurs` and `stuck` are both
syntactic give-ups: each fires on some problems that do have an mgu.

*occurs cuts ACROSS the other three* — it is a syntactic guard, not a
classification of the problem, and it is deliberately CONSERVATIVE:

- the row guard tests the other side's VAR SEQUENCE. α ≐ᵣ (β | α | γ) is
  reported `occurs` even though it has an mgu (occurs_allVar_hasMgu). Sharp
  incompleteness, knowingly paid for
- the type guard tests the full ftv, which spans BOTH sorts, so x ≐ {x} is
  rejected even though θ.ty x = {ε}, θ.row x = ε solves it


== Type unification ≐

- *T-var* one side is a variable α ⟹ bind, occurs-checked:
  α ≐ α ⟹ success ∅ (no binding written);
  α ∈ ftv(τ) ⟹ *occurs*; else α ≔ τ.
- *T-unk* ★ ≐ ★ ⟹ success ∅. *★ is RIGID*: it unifies with itself and
  clashes with everything else, matching "★ stays out of ≈" and TyPrec.unk_below
- *T-base* 𝓫 ≐ 𝓫′ ⟹ success if equal, else *clash*
- *T-fn* (a₁ → b₁) ≐ (a₂ → b₂) ⟹ solve a₁ ≐ a₂, APPLY the solution to b₁, b₂,
  then solve those; compose.
- *T-rcd* {ρ₁} ≐ {ρ₂} ⟹ hand both spines to ≐ᵣ
- otherwise *clash* (constructor mismatch)

== Row unification ≐ᵣ
> Replaces the earlier sketch. Adapts P&X's Fig. 10 to scoped rows +
> asymmetric concat. Headline deviation: their field-guessing rule (LUtail)
> is *restricted to the forced case*: we expand a variable only when it is the
> UNIQUE candidate host for the label. Everything else in the algorithm is
> forced too (solution-set preserving), which is what makes mgu-on-success
> nearly free.

*Normal form.* Rows normalize mod ≈-assoc and ≈-units to spines
a₁ | a₂ | … | aₙ with atoms a := l: τ | α, and a spine factors into an
alternation of *segments* and vars:

seg₀ | α₁ | seg₁ | α₂ | … | αₖ | segₖ

where a segment is a var-free run, read as a map label ↦ (ordered list of
types): ≈-comm swaps adjacent DISTINCT labels only, so within a segment
distinct labels commute freely while equal labels keep their relative
order (scopedness: (l:τ₁ | l:τ₂) ≠ (l:τ₁), shadowed fields participate).
Nothing crosses a var and vars never swap. Hence the *≈-CHARACTERIZATION*:

ρ₁ ≈ ρ₂  iff  same var sequence α₁…αₖ, and corresponding segments
have equal label sets with per-label type lists pointwise ≈

This is a partially-commutative (trace) monoid; the load-bearing algebraic
fact is that trace monoids are *LEFT- AND RIGHT-CANCELLATIVE* — cancelling a
shared var off either end is sound AND complete, which is exactly what
replaces P&X's shared-tail side condition ([Δ₂]ρ₁ = [Δ₁]ρ₁).

*Judgment.*  S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′, on spines s₁, s₂.

+ *U-ε-var* one side is exhausted (ε) ⟹ every var of the other side ≔ ε
  (forced: θ-images must concatenate to the empty trace); any remaining field
  ⟹ *clash*. ε ≐ᵣ ε is the degenerate case, so there is no separate U-ε rule.
  *Checked before the fuel guard*: an exhausted side needs no budget
+ *out of fuel* ⟹ `outOfFuel`
+ *U-var-refl* both spines start — or both end — with the SAME var α ⟹ strip
  it (*cancellativity*), recurse. Left end first, then right
+ *U-var-solve* one side is EXACTLY a single var α ⟹ occurs-check
  (α ∈ vars(other side) ⟹ `occurs`), else α ≔ other side. This is also the
  var-var rule (union-find merge in an implementation)
+ *U-field, left end* leftmost LHS field l:τ, and the RHS *window* (= leading
  segment, i.e. everything before the first var) contains l ⟹ match against
  the FIRST l-occurrence in the window (distinct-label transpositions =
  ≈-comm; first occurrence per label = scoped order), emit τ ≐ τ′, delete
  both, *solve the equation and apply it to both residuals*, recurse
+ *U-field, right end* the mirror, on the trailing segments
+ *U-ground* one side is VAR-FREE and some label has EQUAL POSITIVE counts on
  both sides ⟹ that side's counting rules the other side's vars out at l
  (they must contribute zero l-fields), so the pairing is positional: match
  the first l-occurrence ANYWHERE on each side (vars skipped), emit the type
  equation, recurse. This is worked example 2's "var count must collapse"
  made into a rule — the window rules alone do not cover it, which the
  mechanization surfaced
+ *U-expand* leftmost LHS field l:τ, and the RHS has EXACTLY ONE variable β
  and NO l-field at all ⟹ β is the only place l can come from, so the
  expansion is FORCED: fresh δ, β′; write δ ≔ τ and β ≔ (l: δ | β′), rename
  β to β′ in the residual, recurse. This is P&X's LUtail with the guess
  removed. Currently wired at the LEFT end only (in both argument orders);
  `expandR` exists but the driver does not call it
+ *U-clash* projection clash, checked *globally* (any position, not just the
  window): some label l has more concrete l-fields on one side than on the
  other AND the side with fewer has NO vars left to absorb the difference ⟹
  *clash* — the l-projection already has no unifier, hard error. Subsumes
  "leftmost field missing in a var-free RHS"; per-label counting is O(atoms)
+ *U-stuck* nothing above applies ⟹ *stuck*. By construction that means no
  projection clash (else U-clash) AND U-expand refused, which it does for
  exactly two reasons (uniqueHost_none): ≥ 2 candidate hosts (Wand's shape),
  or the label already occurs on the other side but BEHIND a variable (the
  two-sided shape). *No forced move remains* — which is usually, but not
  always, genuine ambiguity (see "stuck is conservative" below)

*Note on the ORDER of U-clash.* It sits at the BOTTOM, not at the top: a
clash is only diagnosed once every forced move is dead. The cheap clashes are
caught earlier and more precisely anyway — U-ε-var reports a leftover field
directly, and cancellativity turns α ≐ᵣ (l:𝓫 | α) into a *clash* rather than
an occurs-failure (strictly stronger information).

*Other invariants.*
- ★ in field types: ★ ≐ ★ succeeds; ★ ≐ τ (τ ≠ ★, not a var) CLASHES — ★ is a
  rigid constructor for ≐. Whether that clash rejects or degrades is the
  Failure-policy question, not unification's
- Shadowed fields are NOT quotiented away: ≈ does not erase them, and a
  coarser ≐ᵣ would break soundness-against-T-eq
- *Freshness*: names are drawn from a `Supply` derived LOCALLY from the
  problem (strictly longer than every name in it) and THREADED through the
  driver. Re-deriving it per call is non-monotone — a move that drops a field
  drops its type's variables, and the bound can fall below a name still in
  scope. A success therefore CARRIES its supply
- There is *no rank/telescope discipline* on solutions; the occurs check is
  the whole guard


== Examples

*Why LUtail must be restricted, not just adopted (deviation from P&X).*
P&X's (Rfield) search may hit a row var and then commits it to contain
the sought field: α ≔ (l: β | γ), fresh β γ. Two reasons we do not take it
unrestricted:

- It is not forced, and demonstrably loses solutions: (l: Int) ≐ᵣ (α | l: Int)
  has the unique mgu α ≔ ε, but LUtail commits α to contain l and fails.
  Our two-sided processing finds it: right-cancel the field (match l:Int
  against l:Int — both are the rightmost atoms and their windows are the
  trailing segments), leaving ε ≐ᵣ α, then U-ε-var. Forced throughout
  (`unify_lutail`)
- P&X NEED LUtail because their selection/extension elaborate to row
  constraints — *a field demand must flow into the row through unification*.
  Ours flow through the lookup relation and park as stumps; ≐ᵣ only ever
  states structural EQUALITY of two rows. Field demands and row equality
  are different judgments in this system, and the stump machinery absorbs
  the non-forced part of the rule. (*This is the algorithmic payoff of the
  T-sel/★ design, and worth saying loudly in the thesis*)
- What SURVIVES of LUtail is U-expand: when the other side has exactly one
  variable and no l-field, there is nothing to guess. `expandL_wand_refuses`
  and `expandL_lfield_refuses` are the two refusals that keep it honest

(l₁: Int | α) ≐ᵣ (l₂: Int | α), l₁ ≠ l₂
----------------------------------------
P&X's shared-tail pitfall: U-var-refl right-cancels α, leaving the var-free
(l₁: Int) ≐ᵣ (l₂: Int) that every forced move declines, so U-clash fires at the
bottom — correct rejection with no side condition and no loop risk (their
example motivating [Δ₂]ρ₁ = [Δ₁]ρ₁). `unify_shared_tail`

(α | l: Int | β) ≐ᵣ (l: Int)
-----------------------------
var count must collapse. The RHS is var-free and both sides have exactly one
l-field ⟹ *U-ground* pairs them positionally, then U-ε-var forces α ≔ ε,
β ≔ ε. Unique mgu, found. `unify_ground_collapse`

(β | α) ≐ᵣ (l: Int): θβ ++ θα = [l: Int]
-----------------------------------------
splits two incomparable ways ⟹ *U-stuck*. CORRECT to fail: this is Wand's
non-principality example in unification clothing — g: {l: Int} → τ applied to
x ‖ y with both arguments abstract genuinely has no principal typing without
lacks-constraints or unions of typings. Systems either backtrack (Wand),
constrain (lacks/disjointness), or fail (P&X, us). U-expand refuses here
precisely because TWO variables could host l. We fail ONLY when two abstract
concatenations must be aligned against each other; selection — the common
case — never asks for alignment thanks to stumps¿ (claim: check against a
corpus later, Towards Nix). `unify_wand`

(β | l: Int | α) ≐ᵣ (l′: Bool), l ≠ l′
---------------------------------------
*U-clash, NOT stuck* — the RHS is var-free with no l-field, so the
l-projection is unsolvable no matter what the vars do. A window-only clash
rule would misfile this under stuck (leading atoms are var vs. field); this
example is why U-clash must be projection-based. `unify_global_clash`

(α | l: Int) ≐ᵣ (l: Int | β)
-----------------------------
the ambiguous mirror: both windows are closed by a var and both sides have
vars, so Levi splits two ways ⟹ *U-stuck*. `unify_two_sided_stuck`

(l: 𝓫 | α) ≐ᵣ (m: 𝓫 | β), l ≠ m
--------------------------------
*U-expand's payoff.* The RHS has exactly one variable and no l-field, so β is
the FORCED host: β ≔ (l: δ | β′) with the emitted equation δ ≐ 𝓫 solved on the
spot. The residual α ≐ᵣ (m: 𝓫 | β′) is then a plain U-var-solve.
*Success with an mgu.* Without U-expand this was reported stuck, and that was
WRONG. `crossfield_success`

(k: {β} | β | α) ≐ᵣ (k: {l: Int} | l: Int)
-------------------------------------------
*The payoff of mutualizing ≐ and ≐ᵣ.* U-field peels k and emits the type
equation {β} ≐ {l: Int}, leaving the Wand residual (β | α) ≐ᵣ (l: Int) — which
ALONE is ambiguous. But the equation forces β ≈ (l: Int), hence α ≈ ε, so the
problem has a unique mgu. A two-pass architecture that collected the equation
and solved it later would report stuck; the mutual driver solves it, applies
it, and finds the mgu. `eq_rescued_solved`

α ≐ᵣ (l: Int | α)   vs.   α ≐ᵣ (l: Int | α | m: Int)
-----------------------------------------------------
the first *cancels*: the shared END-var strips (solution-preserving!), leaving
ε ≐ᵣ (l: Int) — a definite *clash*, strictly stronger than an occurs-failure.
The second is genuinely recursive (the var is interior, nothing cancels) ⟹
*occurs*. Cancellativity subsumes the end-aligned occurs cases.
`unify_occurs_cancelled`, `unify_occurs`


== Qualified Schemes
Schemes carry their unresolved lookups as constraints:

σ := ∀ᾱ. Q ⇒ τ        Q := { ⟨ρ.l ↓ δ⟩, … }     (δ ∈ ᾱ; plain HM: Q = ∅)

Declarative instantiation-with-discharge (replaces σ ≥ τ at T-var):

σ ≥\_Γ τ′ iff ∃θ fixed outside ᾱ: θτ = τ′ and every ⟨ρ.l ↓ δ⟩ ∈ Q
discharges: Γ ⊢ (θρ).l ↓ r  with
r = τ_r  ⟹  θδ = τ_r          (D-hit; the T-sel moment)
r = ⊥    ⟹  θδ = ★            (D-⊥; T-sel-⊥, W-flag)
r = ?    ⟹  θδ = ★            (D-?; T-sel-★: still-unknown stays blurred —
algorithmically this case RE-PARKS instead,
only finalization commits ★)

- The three-way discharge IS the per-instance case split of the Lean
  regression proof — instantiation replays T-sel / T-sel-⊥ / T-sel-★ for
  its chosen ρ. *Completeness against instance-closed T-let should fall out
  of this correspondence¿*
- Plain schemes embed: with Q = ∅ the discharge condition is vacuous and
  ≥\_Γ degenerates to the Γ-independent Scheme.Inst (`QScheme.inst_toQ`) —
  the seam between the declarative and the algorithmic system
- Instantiation becomes Γ-relative (lookup reads row-solutions): the *price*
  of cross-instantiation refinement. Determinism/monotonicity/totality of ↓
  keep discharge well-behaved — the same three mechanized lemmas that govern
  stump wake-up
- L2 is safe IN ITS OWN RIGHT: qProgress + qPreservation over QCtx, not
  routed through L1

Worked example (the regression program): f : ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
f {}         β ≔ ε          lookup ⊥    δ ≔ ★      : ★    (+ W-flag)
f {l = c}    β ≔ (l: 𝓫_c)   lookup hit  δ ≔ 𝓫_c    : 𝓫_c  (what L1 loses)
f y          β ≔ β′ free    lookup ?    δ ≔ ★ decl. / stump re-parks algo.

EVERY ≥\_Γ-instance of selQ = ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ is a declarative
typing of λx. x.l, in ANY Γ (`selQ_instance_closed`) — which is what no plain
∀ᾱ.τ scheme can manage (`no_plain_principal_scheme`). Qualified schemes are
FORCED, not a convenience.


== The Fourth Leg
The trichotomy for ≐ᵣ has three unconditional legs: `success` returns a
substitution that provably unifies, `clash` provably means no unifier exists,
`outOfFuel` claims nothing. A fourth leg would read

`stuck` ⟹ the equation has no most general unifier.

It is FALSE, and false at every formulation we tried. What stands in its place
is a pair of finite lists: three theorems that DO establish "no mgu", for
specific shapes, and three counterexamples that fix the boundary from the other
side. Both lists are kernel-checked (`NoMgu.lean`, `Refutations.lean`) and
axiom-clean.

=== What is proved: three no-mgu shapes
Each is stated as ¬ HasMgu ρ₁ ρ₂ — no unifier is an instance-ceiling for all
the others — and each is derived from a `HasMguOn V` refinement that only
demands generality on a named variable set. The three use three different
techniques, which is the point: there is no single argument covering them.

/ Wand (`vars_vs_field_no_mgu`): (v₁ | … | vₙ) ≐ᵣ (l: 𝓫), the vᵢ distinct and
  n ≥ 2. The single l-field must land in exactly one host, and any candidate
  mgu can be strictly undercut by the witness that parks the field in a
  DIFFERENT host — that host's l-count drops from ≥1 to 0, and instantiation
  can only grow counts. Technique: *count-shrink*.
/ Two-sided (`two_sided_no_mgu`): (α | l:𝓫) ≐ᵣ (l:𝓫 | β). Two witnesses pull in
  incompatible directions. One forces θα to be variable-free, and a
  variable-free row has a RIGID l-count — fixed under every further
  substitution — while the other witness demands that same count be 1.
  Technique: *rigidity*.
/ Swap (`allvar_swap_no_mgu`): (α | β) ≐ᵣ (β | α). Every unifier is an instance
  of the candidate, so the candidate's α and β must each absorb the other's
  fields; the witness (α ↦ l:★, β ↦ ε) then forces θβ to vanish under σ, which
  drags θα's variables down with it and collapses the l-field the witness put
  there. Technique: *non-commutativity* of `‖` under ≈.

These are the honest content of the fourth leg. They are shape-specific, not a
decision procedure, and the algorithm does not currently consult them.

=== What is conservative: three counterexamples
+ *The verdict is not the problem.* `(k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫)` is
  reported `stuck`, and has the unique mgu β ≔ (l:𝓫), α ≔ ε
  (`stuck_masks_mgu`). The driver evaluates an emitted equation first and `seq`
  propagates its `stuck` before the residual — which would have disambiguated
  it — is ever looked at. This is a DRIVER defect, in the evaluation order;
  deferring the equation and retrying after the residual would fix it.
+ *Terminality is not the problem either.* Retreating from the verdict to
  TERMINAL configurations — every move checked dead, one by one — does not
  rescue the leg. `(l:{w}) ≐ᵣ (w | v)` is terminal, U-expand refusing because
  the l-field has two candidate hosts (Wand's shape). But hosting in w would
  force θw ≈ (l:{θw}), an occurs violation that field counting cannot see
  because the recursion passes UNDER a record constructor. One of the two
  placements is ruled out, the unifier is unique, and a unique unifier is
  trivially most general (`terminalNoMgu_false`). This is a MOVE-SET defect,
  which deferral would not touch.
+ *The earlier reduction was false in its own right.* The four parked
  hypotheses (hbase, hexp, hsolve, hsolveTy) each threaded an unconstrained
  predicate `Q` through the unifier set; conjoining such a `Q` can shrink a set
  with no mgu down to one that has one, and requiring `Q` to be substitution-
  stable does not repair it (`hbase_shape_false`, `hbase_stableQ_false`).

*Terminality is a fact about the MOVES, not about the problem.* "No forced move
remains" does not imply "two placements are both realizable". That is the
one-line reason there is no general converse to prove, at any formulation.

The invariant that exposes the second counterexample is new and reusable:
`rcdDepth`, record NESTING depth, preserved by every ≈-constructor because `cat`
takes a max. It is the first invariant here that sees THROUGH a field payload,
which `sFieldCount` structurally cannot; it also yields `no_rcd_self_reference`,
¬ (ρ ≈ (l: {ρ})).

=== Consequences for principality
Two claims must be kept apart, and only the first is settled.

/ Principal types EXIST: no plain ∀ᾱ.τ scheme suffices, so principality is
  stated over L2's qualified schemes. This rests on `finalized_no_blur` and
  `no_plain_principal_scheme` and is untouched by anything above — it is a
  statement about the declarative system.
/ The algorithm FINDS them: open. `stuck` and `occurs` are both conservative
  give-ups, so inference can return ★ where the L2-principal type is definite.
  Soundness survives — ★ over-approximates — but the principality factoring
  "every declarative typing is ⊒ θ″(⟦S′⟧τ)" does not, because the inferred τ
  can be strictly blurrier than the principal one.

The gap between them is exactly the two conservativity examples: a driver defect
(fixable by deferral) and a move-set defect (not). Closing the first is a
scoped, mechanical change; the second needs an occurs-aware host filter in
U-expand, or an explicit admission that Algorithm R is precision-incomplete on
nested-record shapes. Until one of those lands, the principality obligation
below should be read as conditional on the equations reaching a verdict other
than `stuck`.


== Metatheory Obligations
*Principality*:
If  θ′(Γ) ⊢ e : τ′  then inference succeeds,
Γ; S₀ ⊢ e ⇒ τ; S′,  and there is θ″ with  τ′ ⊒ θ″(⟦S′⟧τ).
"Every declarative typing factors through the inferred one as
substitution-then-blur." CONDITIONAL: the conservativity of `stuck` and `occurs`
lets inference return ★ where a definite type exists, which breaks the factoring
even though it preserves soundness — see "Consequences for principality".

*Improvement corollary* (the "reduction only improves typing" claim):
∅ ⊢ e : τ  ∧  e → e′  ⟹  Types(e) ⊆ Types(e′)
so with L2 principality the principal qualified type of e′ covers that of
e — improvement under reduction is a two-line corollary. ALL remaining risk
sits in principality itself, none in the improvement statement.

*Termination*: OPEN. There is no closed-form fuel bound yet. Solve-and-apply
grows the spine, and the variable count can grow too (a type equation solved
inside a field may expand a row variable and hand the invented tail to the
residual), so no lexicographic measure decreases. The naive Rémy measure does
not close either: renaming adds no fields, so the host keeps count_l = 0 and
the same variable is re-expandable at the same label — the bound has to come
from the OTHER side's l-fields, which solve-and-apply can add. `outOfFuel`
makes this separable rather than blocking: a verdict that was REACHED is
fuel-independent, which is all any leg needs.

*The fourth leg* (`stuck` ⟹ no mgu): DOES NOT HOLD. See "The Fourth Leg"
above — three no-mgu theorems stand, the general converse does not, and the
principality obligation here is conditional on reaching a verdict other than
`stuck`.

*Determinism of results* (not of event order): lookup_det + monotonicity ⟹
final θ, warnings, and type independent of wake-up scheduling¿ (needs a
small confluence argument — candidate for mechanization later).

*Also open, one level up*: the covering order ⊴ on qualified schemes (needed
to even STATE "the principal type improves under reduction"); non-vacuity of
qualified schemes; strictness of the QTyped extension; the solver state
S = (θ, Δ, W) with stump wake-up and its confluence argument.


== Failure Policy
Which unification failures reject, which degrade to ★ + warning? The promise
"no breaking points" cannot mean "never fail": the declarative system does
NOT type `3 4` (★ has no elimination rules), so inference must be allowed to
reject it — soundness even demands it.

- Baseline policy: every genuine constructor clash (𝓫 ≐ 𝓫′, 𝓫 ≐ τ→τ, ★ ≐ τ,
  arity of rows, missing REQUIRED field in a var-free row) is a hard error —
  and each of these is a `clash` verdict, which is PROVED to mean "no unifier
  exists". Rejecting on clash is not a policy choice, it is soundness
- `stuck` and `occurs` are the policy questions, and BOTH are conservative, so
  neither may reject. `stuck` means only that no forced move remains: sometimes
  genuine ambiguity (Wand), sometimes an mgu the moves cannot find
  (`stuck_masks_mgu`, `terminal_masks_mgu`). Degrading to ★ + warning is
  defensible where rejecting on clash is not — but the reason is that ★ is the
  sound over-approximation of an unresolved constraint, NOT that the problem is
  ambiguous. `occurs` is mixed: the genuine case has no unifier at all
  (`occurs_field_no_unifier`, so it may reject like a clash), but the
  conservative case does (`occurs_allVar_hasMgu`) — a sharper guard would move
  those into `success` rather than into a warning
- Where a conservative give-up costs PRECISION it also costs principality: the
  algorithm returns ★ where a definite type exists. That is the link to check
  before claiming a principal-type result
- The tension case: a stump's δ was already unified (e.g. body forced
  δ ≐ Int) and wake-up finds τ′ = String. *hard error*
