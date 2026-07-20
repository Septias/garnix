== Algorithmic System (brainstorm)
> Constraint generation + unification for the minimal calculus. Status: DESIGN,
> nothing settled unless marked. Companion to minimal.typ (declarative) and
> minimal.lean (mechanized safety + refinement).

- Goal restated (thesis.typ, Goal): efficiently computable, *no breaking
  points* — inference never rejects a program the declarative system types;
  failures inside the ★-fragment degrade to ★ + warning instead of erroring
- Design inheritance: Paszke&Xie give unification for infix-extensible rows
  with row-/label-variables. We drop their *conditional tail check* (they
  reject when shadowing is unresolved) and replace it with stumps: park the
  lookup, emit ★ only when forced
- Working name¿: Algorithm R (rows / refinement)


== Architecture: W-with-state vs constraint generation
Two candidate shapes:

1. *Algorithm-W style*: syntax-directed recursion carrying a mutable solver
  state S. Unification happens at application/selection sites immediately.
2. *HM(X) style*: generate a constraint set C first, solve separately.
  Cleaner metatheory (soundness = "solutions of C are typings"), but
  Sulzmann's lesson (designing_record_systems): HM(X) does not tell you how
  to implement X — and X here (stumps) is stateful and order-sensitive
  in its *wake-up* behaviour, even if confluent in its results.

LEANING¿: W-with-state as the primary presentation (it is what garnix will
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
  established at an earlier solver state survives to every later one. That is
  the entire soundness-across-time argument, already proven.
- The occurs-check is not an optimization: it maintains Ctx.RowWF (acyclic,
  rank-decreasing solutions), which is the hypothesis typed_ext and lookup
  totality consume. Occurs-check failure on a row-var = would-be recursive
  row = reject¿ (or ★-degrade? see Failure policy; Nix recursive attrsets
  suggest we eventually want equi-recursive rows, out of scope here)


== Judgment sketch
Γ; S ⊢ e ⇒ τ; S′        (infer)
S ⊢ τ₁ ≐ τ₂ ⇝ S′        (unify types)
S ⊢ ρ₁ ≐ᵣ ρ₂ ⇝ S′       (unify rows)
θ ⊢ ρ.l ↓ r              (the SAME lookup relation as minimal.typ, reading
θ's row-solutions — nothing new to define)

Fresh-variable discipline as usual; ⇒-rules are syntax-directed, one per
term former, no T-eq/T-★-intro counterparts (those are what inversion-mod-≈
and re-blurring account for on the declarative side).


A-cons:  Γ; S ⊢ c ⇒ 𝓫_c; S

A-var:   x: ∀ᾱ.τ ∈ Γ   fresh β̄
--------------------------------
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S
(plus stump copying if schemes carry stumps — see Generalization)

A-lam:   fresh α   Γ·(x: α); S ⊢ e ⇒ τ; S′
------------------------------------
Γ; S ⊢ (x: e) ⇒ α → τ; S′

A-app:   Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β
S₂ ⊢ τ₁ ≐ τ₂ → β ⇝ S₃
----------------------------------------------------
Γ; S ⊢ e₁e₂ ⇒ β; S₃

A-conc:  Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂
S₂ ⊢ τ₁ ≐ {ρ₁} ⇝ S₃   S₃ ⊢ τ₂ ≐ {ρ₂} ⇝ S₄
--------------------------------------------------------
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
  surviving stumps resolve δ ≐ ★. This is the algorithmic moment of T-sel-★.
- Why this is sound and deterministic — the three standalone lemmas were
  built for exactly this (proof-state, Standalone Metatheory):
  determinism  — a woken stump re-resolves to a unique result
  monotonicity — a resolved stump NEVER needs re-checking: found/⊥ are
  final under every future extension of θ. Wake-up lists
  never contain resolved stumps; no fixpoint iteration
  totality     — under the occurs-check invariant (RowWF) every wake-up
  terminates with a result
- Each stump wakes at most (spine-depth of the eventual solution chain)
  times, and monotonicity de-duplicates work ⟹ with union-find on vars the
  whole solver should stay near-linear¿ (formal cost analysis open)


== Row unification sketch
Adapting Paszke&Xie to scoped rows + asymmetric concat. Rows normalize
(mod ≈: assoc, ε-units) to spines  a₁ | a₂ | … | aₙ  with atoms
a := l: τ | α.

- ≈-comm (swap DISTINCT labels) gives limited reordering: a field may move
  left/right past distinctly-labeled fields but never past a row-var and
  never past an equal label. Unification must respect this: match fields
  greedily left-to-right, allowing distinct-label transpositions within the
  var-free prefix¿ (this is where P&X's machinery mostly carries over)
- (l: τ₁ | ρ₁) ≐ᵣ (l: τ₂ | ρ₂) with l leftmost on both sides ⟹
  τ₁ ≐ τ₂, ρ₁ ≐ᵣ ρ₂
- var-var: α ≐ᵣ β ⟹ union-find merge
- var-row: α ≐ᵣ ρ ⟹ occurs-check (rank discipline! solution may only
  mention strictly-later vars — the telescope form of RowWF), then write
  α ≔ ρ and WAKE stumps blocked on α
- Duplicate labels are legal (scoped!): (l:τ₁ | l:τ₂) is a valid spine and
  NOT equal to (l:τ₁); shadowed fields still participate in ≐ᵣ ¿ — or should
  unification quotient them away? Careful: shadowed fields are unobservable
  by lookup but ≈ does not erase them; quotienting would make ≐ᵣ coarser
  than ≈ and break soundness-against-T-eq. LEANING: keep shadowed fields,
  they cost nothing
- ★ in unification: ★ ≐ ★ succeeds; ★ ≐ τ (τ ≠ ★, not a var) FAILS as an
  equation — ★ is a rigid constructor for ≐, matching "★ stays out of ≈" and
  ★-rigidity (TyPrec.unk_below). Whether that failure rejects or degrades is
  the Failure-policy question, not unification's


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

LEANING¿: present L1 as the baseline theorem-bearing system, L2 as the
implementation's refinement with its own (weaker/sketched) claims. Decide
after attempting the L1 principality proof.


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

Completeness w.r.t. T-let: against the INSTANCE-CLOSED rule. The algorithm
checks e₁ once at the generic instance; the type-substitution lemma (plan
item 4 — the same lemma that makes the syntactic ftv-rule admissible) is
what stretches one generic check to the ∀-instances premise. Item 4 is
therefore not optional bookkeeping: it is the completeness workhorse.
Priority of item 4 RISES.


Termination: unification by the usual size/rank measures; stump wake-ups by
the RowWF rank (a stump's blocker strictly descends the telescope).
Determinism of results (not of event order): lookup_det + monotonicity ⟹
final θ, warnings, and type independent of wake-up scheduling¿ (needs a
small confluence argument — candidate for mechanization later).


== Open questions
1. L1 vs L2 at generalization (leaning: L1 for theorems, L2 for garnix)
2. Failure policy beyond baseline — coupled to ★-elimination (plan item 3);
  design the ⟨★-elim rule, degradation rule, warning⟩ triples in lockstep
3. Does ≐ᵣ need full ≈-completeness (distinct-label transpositions across
  the whole var-free prefix) or does left-to-right greedy with a bounded
  window suffice? P&X's proof should transfer; verify against ≈-comm's
  var-blocking restriction
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
- RAISES priority of item 4 (type-substitution lemma = completeness
  workhorse, not just figure-hygiene)
- CONFIRMS deferral of item 3, and sharpens it: ★-elimination must ship as
  declarative-rule + failure-policy + warning, jointly
