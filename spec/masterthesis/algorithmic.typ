== Algorithmic System
- Goal: efficiently computable, *no breaking points*
- Design inheritance: Paszke&Xie give unification for infix-extensible rows
  with row-/label-variables. We drop their *conditional tail check* (they
  reject when shadowing is unresolved) and replace it with stumps: park the
  lookup, emit ★ only when forced
- Working name: *Algorithm R*


== Architecture
S := (θ, Δ, W)
θ : substitution over type-vars AND row-vars
Δ : set of pending stumps
W : warnings (definite-absence flags, ★-degradations)


- The declarative system *reads* solutions via L-α; the algorithm *writes*
  them via unification. "θ only gets refined during solving".
- The *occurs-check* is not an optimization: it maintains Ctx.RowWF (acyclic,
  rank-decreasing solutions), which is the hypothesis typed_ext and lookup
  totality consume. Occurs-check failure on a row-var = would-be recursive
  row = reject¿ (or ★-degrade? see Failure policy; Nix recursive attrsets
  suggest we eventually want equi-recursive rows, out of scope here)


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

== Row unification ≐ᵣ
> Replaces the earlier sketch. Adapts P&X's Fig. 10 to scoped rows +
> asymmetric concat. Headline deviation: we DROP their field-guessing rule
> (LUtail) — in our architecture selections never emit row constraints
> (they become stumps), so ≐ᵣ never has to guess a field into a var. What
> remains is a unification whose every step is FORCED (solution-set
> preserving), which is what makes mgu-on-success nearly free.

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

*Judgment.*  S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′  with both sides kept θ-normalized
(solved vars expanded, segments re-merged, then re-factored). Rules apply
from BOTH ends of the spines; every rule is forced; symmetric mirrors
(and right-end duals) omitted:

- *U-ε* ε ≐ᵣ ε  ⟹ ✓
- *U-field* leftmost LHS field l:τ, and the RHS *window* (= leading segment, i.e. everything before the first var) contains l ⟹ match against the FIRST l-occurrence in the window (distinct-label transpositions = ≈-comm; first occurrence per label = scoped order), emit τ ≐ τ', delete both, recurse
- *U-clash* projection-clash, checked *globally* (any position, not just the window): some label l has more concrete l-fields on one side than on the other AND the side with fewer has NO vars left to absorb the difference ⟹ *FAIL* (clash: the l-projection already has no unifier — hard error). Subsumes "leftmost field missing in a var-free RHS"; per-label counting is O(atoms) bookkeeping¿
- *U-var-refl* both spines start (or end) with the SAME var α ⟹ strip it (*cancellativity*), recurse U-var-solve  one side's remainder is exactly α ⟹ occurs-check + rank discipline (solution mentions only strictly-later vars — the telescope form of RowWF), write α ≔ remainder, *WAKE* stumps blocked on α
- *U-ε-var* one side exhausted, other side remainder r ⟹ every var in r ≔ ε (forced: θ-images must concatenate to the empty trace); any remaining field ⟹ FAIL (clash)
- *U-stuck* NO projection-clash (else U-clash), and: leftmost LHS field l:τ, RHS window lacks l, window ends at var β (l could come from β — or be shadowed by it); or both spines lead (and trail) with DISTINCT vars, neither side a whole-var remainder ⟹ *FAIL (ambiguous: solutions exist but no unique mgu, see Trichotomy). The projection-clash precondition is what keeps this class honest — solvable-but-ambiguous ONLY*

- *var-var* α ≐ᵣ β is *U-var-solve* (union-find merge in the implementation)
- ★ in field types: ★ ≐ ★ succeeds; ★ ≐ τ (τ ≠ ★, not a var) FAILS as an
  equation — ★ is a rigid constructor for ≐, matching "★ stays out of ≈"
  and ★-rigidity (TyPrec.unk_below). Whether that failure rejects or
  degrades is the Failure-policy question, not unification's
- Shadowed fields are NOT quotiented away: ≈ does not erase them, and a
  coarser ≐ᵣ would break soundness-against-T-eq.

== Examples
*Why no LUtail (deviation from P&X).*
P&X's (Rfield) search may hit a row var and then commits it to contain
the sought field: α ≔ (l: β | γ), fresh β γ. Two reasons we drop this:

- It is not forced, and demonstrably loses solutions: (l: Int) ≐ᵣ (α | l: Int)
  has the unique mgu α ≔ ε, but LUtail commits α to contain l and fails.
  Our two-sided processing finds it: right-cancel the field (match l:Int
  against l:Int — both are the rightmost atoms and their windows are the
  trailing segments), leaving ε ≐ᵣ α, then U-ε-var. Forced throughout.
- P&X NEED LUtail because their selection/extension elaborate to row
  constraints — *a field demand must flow into the row through unification*.
  Ours flow through the lookup relation and park as stumps; ≐ᵣ only ever
  states structural EQUALITY of two rows. Field demands and row equality
  are different judgments in this system, and the stump machinery absorbs
  exactly the non-forced rule. (*This is the algorithmic payoff of the
  T-sel/★ design, and worth saying loudly in the thesis*)


(l₁: Int | α) ≐ᵣ (l₂: Int | α), l₁ ≠ l₂
----------------------------------------
P&X's shared-tail pitfall: U-var-refl right-cancels α, then U-clash — correct rejection with no
side condition and no loop risk (their example motivating [Δ₂]ρ₁ = [Δ₁]ρ₁).

(α | l: Int | β) ≐ᵣ (l: Int)
-----------------------------
var count must collapse; right-match the
field, U-ε-var forces α ≔ ε, β ≔ ε. Unique mgu, found

(β | α) ≐ᵣ (l: Int): θβ ++ θα = [l: Int]
-----------------------------------------
splits two incomparable ways ⟹ U-stuck.
CORRECT to fail: this is Wand's non-principality example in
unification clothing — g: {l: Int} → τ applied to x ‖ y with both
arguments abstract genuinely has no principal typing without lacks-
constraints or unions of typings. Systems either backtrack (Wand),
constrain (lacks/disjointness), or fail (P&X, us). We fail ONLY when two
abstract concatenations must be aligned against each other; selection —
the common case — never asks for alignment thanks to stumps¿ (claim:
check against a corpus later, Towards Nix).

(β | l: Int | α) ≐ᵣ (l′: Bool), l ≠ l′:
-----------------------------------------
U-clash, NOT stuck — the RHS is var-free with no l-field, so the l-projection
is unsolvable no matter what the vars do. A window-only clash rule would
misfile this under stuck (leading atoms are var vs. field); this example is
why U-clash must be projection-based.


== Qualified Schemes
Schemes carry their unresolved lookups as constraints:

σ := ∀ᾱ. Q ⇒ τ        Q := { ⟨ρ.l ↓ δ⟩, … }     (δ ∈ ᾱ; plain HM: Q = ∅)

Declarative instantiation-with-discharge (replaces σ ≥ τ at T-var):

σ ≥\_Γ τ′ iff ∃θ fixed outside ᾱ: θτ = τ′ and every ⟨ρ.l ↓ δ⟩ ∈ Q
discharges: Γ ⊢ (θρ).l ↓ r  with
r = τ_r  ⟹  θδ = τ_r          (the T-sel moment)
r = ⊥    ⟹  θδ = ★            (T-sel-⊥; W-flag)
r = ?    ⟹  θδ = ★            (T-sel-★: still-unknown stays blurred)

- The three-way discharge IS the per-instance case split of the Lean
  regression proof — instantiation replays T-sel / T-sel-⊥ / T-sel-★ for
  its chosen ρ. *Completeness against instance-closed T-let should fall out
  of this correspondence¿*.
- Instantiation becomes Γ-relative (lookup reads row-solutions): the *price*
  of cross-instantiation refinement. Determinism/monotonicity/totality of ↓
  keep discharge well-behaved — the same three mechanized lemmas that govern
  stump wake-up.

Worked example (the regression program): f : ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
f {}         β ≔ ε          lookup ⊥    δ ≔ ★      : ★    (+ W-flag)
f {l = c}    β ≔ (l: 𝓫_c)   lookup hit  δ ≔ 𝓫_c    : 𝓫_c  (what L1 loses)
f y          β ≔ β′ free    lookup ?    δ ≔ ★ decl. / stump re-parks algo.


== Metatheory Obligations
*Principality*:
If  θ′(Γ) ⊢ e : τ′  then inference succeeds,
Γ; S₀ ⊢ e ⇒ τ; S′,  and there is θ″ with  τ′ ⊒ θ″(⟦S′⟧τ).
"Every declarative typing factors through the inferred one as
substitution-then-blur."

*Improvement corollary* (the "reduction only improves typing" claim):
∅ ⊢ e : τ  ∧  e → e′  ⟹  Types(e) ⊆ Types(e′)
so with L2 principality the principal qualified type of e′ covers that of
e — improvement under reduction is a two-line corollary. ALL remaining risk
sits in principality itself, none in the improvement statement.

*Termination*: unification by the usual size/rank measures; stump wake-ups by
the RowWF rank (a stump's blocker strictly descends the telescope).
Determinism of results (not of event order): lookup_det + monotonicity ⟹
final θ, warnings, and type independent of wake-up scheduling¿ (needs a
small confluence argument — candidate for mechanization later).


== Failure Policy
Which unification failures reject, which degrade to ★ + warning? The promise
"no breaking points" cannot mean "never fail": the declarative system does
NOT type `3 4` (★ has no elimination rules), so inference must be allowed to
reject it — soundness even demands it.

- Baseline policy: every genuine constructor clash (𝓫 ≐ 𝓫′, 𝓫 ≐ τ→τ, ★ ≐ τ,
  arity of rows, missing REQUIRED field in a var-free row) is a hard error.
- The tension case: a stump's δ was already unified (e.g. body forced
  δ ≐ Int) and wake-up finds τ′ = String. *hard error*


### Tetrachotomy (the clean statement to aim for):

- (a) success + mgu,
- (b) clash-fail and NO unifier exists, or
- (c) stuck-fail and solutions EXIST but no mgu does.

(c) is the trace-factorization argument: a unifier = a Levi-style
factorization of one side's blocks against the other's (Levi's lemma
for trace monoids; vars commute with nothing, so they never split
across an alignment boundary); a stuck configuration admits ≥2
factorizations pairing some concrete atom differently; concrete-atom
pairings are substitution-STABLE (θ can instantiate vars but never
re-pair existing concrete fields — per-label order is preserved), so
no single unifier generalizes both ⟹ no mgu.
SHARPNESS of (c) hangs on U-clash being projection-based (26-08-19):
with a window-only clash rule, (β | l: Int | α) ≐ᵣ (l′: Bool) would
land in stuck although no unifier exists at all — (c) would degrade to
"no unique mgu OR unsolvable". (a),(b) should be routine given the
≈-characterization. MECHANIZED (26-08-21, algorithmic.lean): the
≈-characterization itself — ρ₁ ≈ ρ₂ iff same var sequence ∧ all
l-projections pointwise-≈ at equal segment indices (rowEquiv_iff_char),
plus end-var cancellativity both sides (cancel_var_left/right) — it is
also what the T-eq completeness case consumes (lookup_equiv / ResEquiv
toolkit)
