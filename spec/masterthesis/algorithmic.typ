== Algorithmic System (brainstorm)
> Constraint generation + unification for the minimal calculus. Status: DESIGN,
> nothing settled unless marked. Companion to minimal.typ (declarative) and
> minimal.lean (mechanized safety + refinement).

- Goal restated (thesis.typ, Goal): efficiently computable, *no breaking
  points*
- Design inheritance: Paszke&Xie give unification for infix-extensible rows
  with row-/label-variables. We drop their *conditional tail check* (they
  reject when shadowing is unresolved) and replace it with stumps: park the
  lookup, emit ★ only when forced
- Working name: Algorithm R (rows / refinement)


== Architecture: W-with-state vs constraint generation
Two candidate shapes:

1. *Algorithm-W style*: syntax-directed recursion carrying a mutable solver
  state S. Unification happens at application/selection sites immediately.
2. *HM(X) style*: generate a constraint set C first, solve separately.
  Cleaner metatheory (soundness = "solutions of C are typings"), but
  Sulzmann's lesson (designing_record_systems): HM(X) does not tell you how
  to implement X — and X here (stumps) is stateful and order-sensitive
  in its *wake-up* behaviour, even if confluent in its results.

LEANING: W-with-state as the primary presentation (it is what garnix will
implement, and efficiency is a headline constraint), with the constraint
reading given informally for proofs. The state:

S := (θ, Δ, W)
θ : substitution over type-vars AND row-vars
Δ : set of pending stumps (below)
W : warnings (definite-absence flags, ★-degradations)

- KEY OBSERVATION (settled): the declarative rowEnv IS the row-restriction of
  θ. The declarative system "reads" solutions via L-α; the algorithm "writes"
  them via unification. Ctx.Ext (rowEnv only grows) is precisely "θ only gets
  refined during solving" — the mechanized typed_ext says every typing
  established at an earlier solver state survives to every later one. *That is
  the entire soundness-across-time argument, already proven.*
- The occurs-check is not an optimization: it maintains Ctx.RowWF (acyclic,
  rank-decreasing solutions), which is the hypothesis typed_ext and lookup
  totality consume. Occurs-check failure on a row-var = would-be recursive
  row = reject¿ (or ★-degrade? see Failure policy; Nix recursive attrsets
  suggest we eventually want equi-recursive rows, out of scope here)


== Judgment sketch
Γ; S ⊢ e ⇒ τ; S′        (infer)
S ⊢ τ₁ ≐ τ₂ ⇝ S′        (unify types)
S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′       (unify rows)
θ ⊢ ρ.l ↓ r             (the SAME lookup relation as minimal.typ, reading θ's row-solutions — nothing new to define)

Fresh-variable discipline as usual; ⇒-rules are syntax-directed, one per
term former, no T-eq/T-★-intro counterparts (those are what inversion-mod-≈
and re-blurring account for on the declarative side).


A-cons:  Γ; S ⊢ c ⇒ 𝓫_c; S

A-var:   x: ∀ᾱ.τ ∈ Γ   fresh β̄
--------------------------------
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S
(plus stump copying if schemes carry stumps — see Generalization)

A-lam:   fresh α   Γ·(x: α); S ⊢ e ⇒ τ; S′
------------------------------------------
Γ; S ⊢ (x: e) ⇒ α → τ; S′

A-app:   Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β
S₂ ⊢ τ₁ ≐ τ₂ → β ⇝ S₃
------------------------------------------------------------
Γ; S ⊢ e₁e₂ ⇒ β; S₃

A-conc:  Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂
S₂ ⊢ τ₁ ≐ {ρ₁} ⇝ S₃   S₃ ⊢ τ₂ ≐ {ρ₂} ⇝ S₄
---------------------------------------------------------------
Γ; S ⊢ e₁ ‖ e₂ ⇒ { ρ₂ | ρ₁ }; S₄

A-sel:   Γ; S ⊢ e ⇒ τ; S₁   fresh ρ   S₁ ⊢ τ ≐ {ρ} ⇝ S₂
then case θ ⊢ ρ.l ↓ r:
r = τ′  ⟹  result τ′                       (T-sel)
r = ⊥   ⟹  result ★, flag (e.l, ⊥) in W    (T-sel-⊥)
r = ?   ⟹  fresh δ, park stump; result δ   (see Stumps)

A-rec:   fields as usual, literal rows are spine-var-free by construction

A-let:   see Generalization


== Stumps (the design centerpiece)
A stump is a parked selection:

stump := ⟨blocker α, ρ.l ↓ δ⟩

where α is the row-var the lookup got stuck on (the ? came from L-α-free
on α) and δ is a fresh *result variable* standing for "whatever the lookup
will turn out to be".

- Declaratively, T-sel-★ types the selection ★ immediately and refinement
  means the term ALSO admits a better type later (typed_mono). Algorithmically
  we cannot return ★ immediately — that would freeze the result and lose
  refinement (the motivating λ-example would infer {β} → ★ and application
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
  *determinism* — a woken stump re-resolves to a unique result
  *monotonicity* — a resolved stump NEVER needs re-checking: found/⊥ are
  final under every future extension of θ. Wake-up lists
  never contain resolved stumps; no fixpoint iteration
  *totality*     — under the occurs-check invariant (RowWF) every wake-up
  terminates with a result
- Each stump wakes at most (spine-depth of the eventual solution chain)
  times, and monotonicity de-duplicates work ⟹ with union-find on vars the
  whole solver should stay near-linear¿ (formal cost analysis open)


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

This is a partially-commutative (trace) monoid¿; the load-bearing algebraic
fact is that trace monoids are *LEFT- AND RIGHT-CANCELLATIVE* — cancelling a
shared var off either end is sound AND complete, which is exactly what
replaces P&X's shared-tail side condition ([Δ₂]ρ₁ = [Δ₁]ρ₁).

*Judgment.*  S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′  with both sides kept θ-normalized
(solved vars expanded, segments re-merged, then re-factored). Rules apply
from BOTH ends of the spines; every rule is forced; symmetric mirrors
(and right-end duals) omitted:

U-ε          ε ≐ᵣ ε                        ⟹ ✓
U-field      leftmost LHS field l:τ, and the RHS *window* (= leading
segment, i.e. everything before the first var) contains l
⟹ match against the FIRST l-occurrence in the window
(distinct-label transpositions = ≈-comm; first occurrence
per label = scoped order), emit τ ≐ τ', delete both, recurse
U-clash      projection-clash, checked GLOBALLY (any position, not just
the window): some label l has more concrete l-fields on one
side than on the other AND the side with fewer has NO vars
left to absorb the difference
⟹ FAIL (clash: the l-projection already has no unifier —
hard error). Subsumes "leftmost field missing in a var-free
RHS"; per-label counting is O(atoms) bookkeeping¿
U-var-refl   both spines start (or end) with the SAME var α
⟹ strip it (*cancellativity*), recurse
U-var-solve  one side's remainder is exactly α
⟹ occurs-check + rank discipline (solution mentions only
strictly-later vars — the telescope form of RowWF), write
α ≔ remainder, WAKE stumps blocked on α
U-ε-var      one side exhausted, other side remainder r
⟹ every var in r ≔ ε (forced: θ-images must concatenate to
the empty trace); any remaining field ⟹ FAIL (clash)
U-stuck      NO projection-clash (else U-clash), and: leftmost LHS field
l:τ, RHS window lacks l, window ends at var β (l could come
from β — or be shadowed by it); or both spines lead (and
trail) with DISTINCT vars, neither side a whole-var remainder
⟹ *FAIL (ambiguous: solutions exist but no unique mgu, see
Trichotomy). The projection-clash precondition is what keeps
this class honest — solvable-but-ambiguous ONLY*

- var-var α ≐ᵣ β is U-var-solve (union-find merge in the implementation)
- ★ in field types: ★ ≐ ★ succeeds; ★ ≐ τ (τ ≠ ★, not a var) FAILS as an
  equation — ★ is a rigid constructor for ≐, matching "★ stays out of ≈"
  and ★-rigidity (TyPrec.unk_below). Whether that failure rejects or
  degrades is the Failure-policy question, not unification's
- shadowed fields are NOT quotiented away: ≈ does not erase them, and a
  coarser ≐ᵣ would break soundness-against-T-eq. They cost nothing

*Why no LUtail (deviation from P&X).* P&X's (Rfield) search may hit a row
var and then COMMITS it to contain the sought field: α ≔ (l: β | γ),
fresh β γ. Two reasons we drop this:
- It is not forced, and demonstrably loses solutions: (l: Int) ≐ᵣ (α | l: Int)
  has the unique mgu α ≔ ε, but LUtail commits α to contain l and fails.
  Our two-sided processing finds it: right-cancel the field (match l:Int
  against l:Int — both are the rightmost atoms and their windows are the
  trailing segments), leaving ε ≐ᵣ α, then U-ε-var. Forced throughout.
- P&X NEED LUtail because their selection/extension elaborate to row
  constraints — a field demand must flow into the row through unification.
  Ours flow through the lookup relation and park as stumps; ≐ᵣ only ever
  states structural EQUALITY of two rows. Field demands and row equality
  are different judgments in this system, and the stump machinery absorbs
  exactly the non-forced rule. (This is the algorithmic payoff of the
  T-sel/★ design, and worth saying loudly in the thesis)

*Worked examples.*
- P&X's shared-tail pitfall (l₁: Int | α) ≐ᵣ (l₂: Int | α), l₁ ≠ l₂:
  U-var-refl right-cancels α, then U-clash — correct rejection with no
  side condition and no loop risk (their example motivating [Δ₂]ρ₁ = [Δ₁]ρ₁)
- (α | l: Int | β) ≐ᵣ (l: Int): var count must collapse; right-match the
  field, U-ε-var forces α ≔ ε, β ≔ ε. Unique mgu, found
- (β | α) ≐ᵣ (l: Int): θβ ++ θα = [l: Int] splits two incomparable ways
  ⟹ U-stuck. CORRECT to fail: this is Wand's non-principality example in
  unification clothing — g: {l: Int} → τ applied to x ‖ y with both
  arguments abstract genuinely has no principal typing without lacks-
  constraints or unions of typings. Systems either backtrack (Wand),
  constrain (lacks/disjointness), or fail (P&X, us). We fail ONLY when two
  abstract concatenations must be aligned against each other; selection —
  the common case — never asks for alignment thanks to stumps¿ (claim:
  check against a corpus later, Towards Nix)
- (β | l: Int | α) ≐ᵣ (l′: Bool), l ≠ l′: U-clash, NOT stuck — the RHS is
  var-free with no l-field, so the l-projection is unsolvable no matter
  what the vars do. A window-only clash rule would misfile this under
  stuck (leading atoms are var vs. field); this example is why U-clash
  must be projection-based

*Metatheory obligations.*
- Soundness: S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′ and S″ ⊒ S′ final ⟹ ⟦S″⟧ρ₁ ≈ ⟦S″⟧ρ₂
  (mirrors P&X Thm 3.4; the ⇝-extension is Ctx.Ext, typed_ext transports)
- Forced-step invariance: every rule preserves the solution set
  {θ | θρ₁ ≈ θρ₂}. Corollary: mgu-on-success (P&X Thm 3.7, but without
  their "if it succeeds" asymmetry hiding lost solutions — our failures
  are classified instead)
- Trichotomy¿ (the clean statement to aim for): ≐ᵣ terminates with either
  (a) success + mgu, (b) clash-fail and NO unifier exists, or
  (c) stuck-fail and solutions EXIST but no mgu does.
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
  ≈-characterization. Mechanization candidate: the ≈-characterization
  itself (segments + var sequence) — it is also what the T-eq
  completeness case consumes (lookup_equiv / ResEquiv toolkit)
- Ground completeness (T-eq workhorse): on var-free rows ≐ᵣ decides ≈
  — direct from the characterization; no window bound needed, the window
  IS the whole row
- Termination: lexicographic (unsolved vars, total atom count); every
  rule solves a var or deletes atoms; rank discipline keeps RowWF

*Failure policy note.* U-stuck is a THIRD failure class besides clash and
occurs-check: the program may well be declaratively typable (pick either
Wand split), so stuck-fail genuinely breaks "inference never rejects what
the declarative system types" — the promise needs this caveat, stated
honestly: completeness/principality hold up to var-alignment ambiguity,
which is exactly the classical non-principality of asymmetric concat.
W should carry a dedicated diagnostic (which two vars, which label,
suggest annotating one side)¿


== Generalization (A-let) and the stump/scheme interaction
At  let x = e₁ in e₂ : infer e₁ ⇒ τ₁, then generalize
ᾱ = fv(θτ₁) ∖ fv(θΓ)  (Rémy-style levels for efficiency¿). Pending stumps
whose blocker or δ lies in ᾱ pose THE design fork:

*L1 — force at the boundary*: resolve such stumps to δ ≐ ★ before
generalizing. Schemes stay plain ∀ᾱ.τ.
⊕ simple; matches the declarative system EXACTLY (the regression example
`let f = (x: x.l) in f {}` gets f: ∀β. {β} → ★, as in minimal.lean)
⊖ loses cross-instantiation refinement: `let f = (x: x.l) in f {l = c}`
infers ★ where β ≔ (l: 𝓫_c) could have delivered 𝓫_c

*L2 — stumps in schemes (qualified types)*: generalize the stump with the
scheme, σ = ∀ᾱ. ⟨ρ.l ↓ δ⟩ ⇒ τ; A-var copies the stump with fresh vars per
instantiation, and each instance re-resolves independently.
⊕ strictly more precise; per-instance refinement
⊕ CORRESPONDENCE (the pretty one): the instance-closed T-let premise
"∀ τ₁ ≤ σ. Γ ⊢ e₁: τ₁" quantifies over instances — L2's per-instance
stump re-resolution is its algorithmic image. The Lean regression proof
(three-way lookup split per instance) is literally the L2 execution
trace. L2 is *natively* complete against instance-closed T-let, no
detour through the syntactic ftv-rule
⊖ schemes stop being plain HM schemes; instantiation allocates stumps
(cost); constraint-carrying schemes are the first step down the HM(X)
slope we criticized — how far before we lose "efficiently computable"?
⊖ declarative counterpart of an L2-refined use is a DIFFERENT σ at T-let
per program — completeness statement gets subtle (see below)

DECIDED (26-08-18): L1 principality is REFUTED without attempting the proof
— mechanized in minimal.lean ("Plain schemes are not principal"):
- *finalized_no_blur*: λx. x.l types declaratively at {(l: τ₀)} → τ₀ for EVERY
  τ₀ (T-sel/hit), but no substitution instance of the L1-finalized {β} → ★
  sits ⊑-below any of them with τ₀ ≠ ★ — the frozen ★ result cannot blur
  into a definite type (⊑-rigidity). The stated principality factoring fails.
- *no_plain_principal_scheme*: worse, no plain ∀ᾱ.τ scheme AT ALL is
  instance-closed while covering both the found-typing {(l: {ε})} → {ε} and
  the ⊥-typing {ε} → ★: its result position must be a quantified variable,
  and re-pointing that variable at {ε} in the ⊥-instance's substitution
  manufactures the instance {ε} → {ε}, which is NOT a typing (on x: {ε} the
  lookup is ⊥, the body types only at ★).
Consequence: L1 is the soundness-bearing baseline ONLY. Principality must be
stated over L2's qualified schemes — ∀β. ⟨β.l ↓ δ⟩ ⇒ {β} → δ is exactly the
principal type of λx. x.l that plain schemes cannot express (δ stays
writable per instance). L2 is not an optional refinement; it is where the
refinement claim lives.


== L2: qualified schemes (DRAFT¿ — consequence of the decided fork)
Schemes carry their unresolved lookups as constraints:

σ := ∀ᾱ. Q ⇒ τ        Q := { ⟨ρ.l ↓ δ⟩, … }     (δ ∈ ᾱ; plain HM: Q = ∅)

Declarative instantiation-with-discharge (replaces σ ≥ τ at T-var):

σ ≥\_Γ τ′   iff   ∃θ fixed outside ᾱ:  θτ = τ′  and every ⟨ρ.l ↓ δ⟩ ∈ Q
discharges:  Γ ⊢ (θρ).l ↓ r  with
r = τ_r  ⟹  θδ = τ_r          (the T-sel moment)
r = ⊥    ⟹  θδ = ★            (T-sel-⊥; W-flag)
r = ?    ⟹  θδ = ★            (T-sel-★: still-unknown stays blurred)

- The three-way discharge IS the per-instance case split of the Lean
  regression proof — instantiation replays T-sel / T-sel-⊥ / T-sel-★ for
  its chosen ρ. Completeness against instance-closed T-let should fall out
  of this correspondence¿ (the CORRESPONDENCE bullet above, now load-bearing).
- Instantiation becomes Γ-relative (lookup reads row-solutions): the price
  of cross-instantiation refinement. Determinism/monotonicity/totality of ↓
  keep discharge well-behaved — the same three mechanized lemmas that govern
  stump wake-up.

Worked example (the regression program): f : ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
f {}         β ≔ ε          lookup ⊥    δ ≔ ★      : ★    (+ W-flag)
f {l = c}    β ≔ (l: 𝓫_c)   lookup hit  δ ≔ 𝓫_c    : 𝓫_c  (what L1 loses)
f y          β ≔ β′ free    lookup ?    δ ≔ ★ decl. / stump re-parks algo.

Principality, restated over L2 (replaces the refuted L1 statement):
If  θ′(Γ) ⊢ e : τ′  then  Γ; S₀ ⊢ e ⇒ τ; S′  and some θ″ discharging S′'s
pending stumps has  τ′ ⊒ θ″τ.  The ⊒-blur now absorbs ONLY T-★-intro;
finalization never occurs inside the statement — δ's are discharged, not
frozen. (finalized_no_blur exhibits exactly the θ″ that L1 cannot provide:
the one discharging δ to a found τ₀.)

Improvement corollary (the "reduction only improves typing" claim):
∅ ⊢ e : τ  ∧  e → e′  ⟹  Types(e) ⊆ Types(e′)   (preservation, MECHANIZED)
so with L2 principality the principal qualified type of e′ covers that of
e — improvement under reduction is a two-line corollary. ALL remaining risk
sits in principality itself, none in the improvement statement.

Open (L2-specific):
- The covering order on qualified schemes needed to even STATE "the
  principal type improves": candidate — σ₁ ⊴ σ₂ iff every ≥\_Γ-instance of
  σ₂ is a ⊒-blur of a ≥\_Γ-instance of σ₁, uniformly in Γ¿
- A-var stump copying: fresh δ AND fresh blocker per use; cost of schemes
  with large Q (dedup identical ⟨ρ.l ↓ δ⟩ across uses¿)
- Does discharge need full Γ or only θ's row-image? (top-level rowEnv is
  empty; lookup_applySubst suggests the θ-image suffices¿)


== Failure policy (soft typing's hard question)
Which unification failures reject, which degrade to ★ + warning? The promise
"no breaking points" cannot mean "never fail": the declarative system does
NOT type `3 4` (★ has no elimination rules), so inference must be allowed to
reject it — soundness even demands it.

- Baseline policy (matches current declarative system): every genuine
  constructor clash (𝓫 ≐ 𝓫′, 𝓫 ≐ τ→τ, ★ ≐ τ, arity of rows, missing
  REQUIRED field in a var-free row) is a hard error. Stump machinery never
  produces hard errors by itself: lookup results found/⊥/? all continue.
- The tension case: a stump's δ was already unified (e.g. body forced
  δ ≐ Int) and wake-up finds τ′ = String. Under the baseline: hard error —
  and this is CORRECT against the current declarative system (there is no
  typing; the program really is a shadowing-dependent type clash).
  It still feels anti-soft¿: the program has a perfectly fine ↯-free run
  whenever the actual argument doesn't shadow l. Making it typeable needs
  ★-elimination rules (T-app-★ …) so that ★ can absorb the clash — this is
  the algorithmic face of plan item 3, and the flagging discipline it needs
  is exactly W: a clash inside a δ-rooted constraint degrades δ ≐ ★ + warn,
  a clash outside stays fatal. TABLED with item 3; the two must land
  together, with the declarative and algorithmic side of each ★-elim rule
  added in lockstep
- Diagnostics are part of the design, not an afterthought: W records WHY each
  ★ exists (stump lifecycle: born at e.l, blocked on α, forced at
  generalization / resolved ⊥). This is the answer to the "T-sel-⊥ and
  T-sel-★ are indistinguishable in the type" blind spot — the distinction
  lives in W, and the thesis should say so instead of pretending the type
  system carries it


== Metatheory obligations (statements only, proofs paper-level)
Soundness (against the declarative system, per solver step):
If  Γ; S ⊢ e ⇒ τ; S′  and  S″ ⊒ S′ is any final state (all stumps
resolved), then  ⟦S″⟧(Γ) ⊢ e : ⟦S″⟧(τ)
where ⟦S″⟧ applies θ and maps surviving δ's to ★. Proof plan: each rule
maps to its T-counterpart; stump resolutions map found/⊥/? to
T-sel/T-sel-⊥/T-sel-★; solver-state extension is Ctx.Ext, so typed_ext
(PROVEN) transports every intermediate typing to the final state. RowWF
invariant carried by the occurs-check.

Principality¿ (the real refinement claim, replacing the trivial declarative
∃τ′ ⊑ τ — this is where "the ★ actually improves" lives):
If  θ′(Γ) ⊢ e : τ′  (declarative, any ground instantiation) then inference
succeeds,  Γ; S₀ ⊢ e ⇒ τ; S′,  and there is θ″ with  τ′ ⊒ θ″(⟦S′⟧τ).
"Every declarative typing factors through the inferred one as
substitution-then-blur." ⊑ enters ONLY here — as forecast, safety never
needed it. Expected hard cases: T-eq (need ≐ᵣ complete for ≈ — the
lookup_equiv / ResEquiv toolkit should carry it¿) and T-★-intro (absorbed
by the ⊒-blur in the statement).
NOTE (26-08-18): as stated this FAILS for L1 — mechanized refutation, see
Generalization/DECIDED. The statement survives only over L2's qualified
schemes, where the stump-var δ stays writable and θ″ may instantiate it;
the ⊒-blur then only absorbs T-★-intro, never finalization. Restate
against L2 instantiation before proving.

Completeness w.r.t. T-let: against the INSTANCE-CLOSED rule. The algorithm
checks e₁ once at the generic instance; the type-substitution lemma (plan
item 4 — the same lemma that makes the syntactic ftv-rule admissible) is
what stretches one generic check to the ∀-instances premise. Item 4 is
therefore not optional bookkeeping: it is the completeness workhorse.
STATUS (26-08-18): item 4 is DONE and mechanized (typed_applySubst_aux +
renameScheme + tLet_syntactic, minimal.lean) — the completeness workhorse
is already in place.


Termination: unification by the usual size/rank measures; stump wake-ups by
the RowWF rank (a stump's blocker strictly descends the telescope).
Determinism of results (not of event order): lookup_det + monotonicity ⟹
final θ, warnings, and type independent of wake-up scheduling¿ (needs a
small confluence argument — candidate for mechanization later).


== Open questions
1. L1 vs L2: DECIDED (see Generalization) — L1 sound-only, L2 carries
  principality. Remaining: how much of the L2 metatheory gets proven (paper)
  vs sketched, and the qualified-scheme precision order it needs
2. Failure policy beyond baseline — coupled to ★-elimination (plan item 3);
  design the ⟨★-elim rule, degradation rule, warning⟩ triples in lockstep
3. ≐ᵣ: DRAFTED (see Row unification) — window question dissolved: the
  window is the leading segment, full ≈-completeness within it comes for
  free from the segment characterization. Remaining obligations: the
  trace-monoid ≈-characterization (mechanization candidate), cancellativity
  for our ≈, and the trichotomy claim (esp. the stuck ⟹ no-unique-mgu
  direction)
4. Recursive rows: occurs-check rejects `rec`-style attrsets; equi-recursive
  rows vs ★-degradation as the Nix answer (Towards Nix section)
5. Cost model: is near-linear provable (union-find + wake-lists), or only
  empirical? What does "efficiently computable" concretely claim in the
  thesis?
6. FC-labels add label-vars as a second stump *blocker kind* (selection
  stuck on an unknown label, cf. record-ts.md's T-look-FC rules) — check
  the stump abstraction survives a second sort before committing to it


== Relation to plan (proof-state.md)
- Realizes item 2 (paper-first design); soundness leans on typed_ext,
  Ctx.Ext, RowWF, lookup_det/mono/total — all mechanized
- Item 4 DONE (typed_applySubst, renameScheme, tLet_syntactic in
  minimal.lean) — the completeness workhorse is already mechanized
- L1/L2 fork DECIDED by mechanized refutation (finalized_no_blur,
  no_plain_principal_scheme): plain schemes cannot be principal
- ≐ᵣ DRAFTED (26-08-19): forced-step unification over the trace-monoid
  normal form, no LUtail (stumps absorb it), two-sided cancellation
  replaces P&X's shared-tail condition; failure trichotomy
  clash / occurs / stuck(no-unique-mgu = Wand ambiguity)
- CONFIRMS deferral of item 3, and sharpens it: ★-elimination must ship as
  declarative-rule + failure-policy + warning, jointly
