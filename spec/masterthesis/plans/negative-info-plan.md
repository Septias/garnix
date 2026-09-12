
## The observation

The lookup relation already computes ⊥ — definite absence, not "unknown". It is
the sharpest verdict in the system: τ says what a field is, ? says we gave up,
and ⊥ is the only thing the calculus ever *proves* about a field that is not
there. And we throw it away. T-sel-⊥ maps it to ★ and drops a W-flag; nothing
downstream can read it, nothing carries it across a binder, no rule consumes it.

Meanwhile ⊥ is exactly the fact that would unblock the things the system cannot
do. Every ★ in this calculus traces back to one question — *"might there be a
shadowing l hiding in that row-variable?"* — and ⊥ is the answer to it.

Stumps already give us the vehicle. ⟨ρ.l ↓ δ⟩ is a lookup parked with a
*variable* in the result position. Put ⊥ there instead and the grammar barely
moves:

    q ::= ⟨ρ.l ↓ r⟩        r ::= δ | ⊥
    α ⊬ l                   shorthand for the atomic case ⟨α.l ↓ ⊥⟩

That is the whole syntactic proposal. Everything below is what it costs and
what it buys.


## 1. Normal form (prove this first — it justifies everything else)

A negative constraint on a *composite* row is not primitive. Read off the
existing lookup rules:

    ⟨ε.l ↓ ⊥⟩                     trivially true                (L-ε)
    ⟨(l′: τ).l ↓ ⊥⟩               iff l ≠ l′                    (L-miss / L-hit)
    ⟨(ρ₁ | ρ₂).l ↓ ⊥⟩             iff ⟨ρ₁.l ↓ ⊥⟩ ∧ ⟨ρ₂.l ↓ ⊥⟩   (L-conc-skip)
    ⟨α.l ↓ ⊥⟩                     ATOMIC — irreducible          (L-α-free)

  - [ ] LEMMA (decomposition). Every negative constraint either normalizes to a
        finite conjunction of atoms α ⊬ l, or is refuted outright by a literal
        field.

This is the payoff of having ↓ defined recursively over concatenation instead of
over a flat row: the decomposition is free, where a classic `lacks` predicate has
to be *defined* to distribute. It means the constraint language never grows
beyond a finite set of (row-var, label) pairs — a set, not a term language, with
no unification of its own and no entailment search. Everything else in the plan
depends on this.


## 2. What it buys, ranked

### (a) Lookup past an uninstantiated row-variable — the headline

Today nothing can do this. L-α-free fires and the answer is ?. With an atom in
scope there is a third arm:

    α ⊬ l ∈ Γ
    ------------- L-α-⊥                      NEW
    Γ ⊢ α.l ↓ ⊥

and L-conc-skip then walks *past* α into the rest of the row. This is the only
mechanism in the calculus that resolves a lookup without instantiating the
variable it is blocked on. It is what gives the thesis' motivating example a
real type:

    a: b: (a ‖ b).l   ::   ∀(α β: Row)(δ: Type). β ⊬ l, ⟨α.l ↓ δ⟩ ⇒ {α} → {β} → δ

Read it as: *this is typable exactly when the right-hand record is known not to
shadow l.* The wand-ambiguity does not go away — it is made into a hypothesis
that the caller either discharges or does not. That is a strictly better answer
than ★ and it is still not a guess.

### (b) Unstuck the unifier

U-expand refuses when two variables could host the field — that refusal IS the
stuck verdict in the wand shape. A candidate that is known to lack l is not a
candidate. Uniqueness comes back, U-expand fires, and a stuck configuration
becomes a solved one. Same story one level down: U-ground's "other side is
var-free" and U-clash's `vars(s₂) = ⟨⟩` both really mean *"no variable over
there can carry an l"*, which is a weaker and label-relative test. See §5.

This is the concrete answer to the open question in the dailies about doing
better on `stuck`, and it is not speculation — Refutations.lean already shows
the mechanism (`hbase_stableQ_false`: conjoining a substitution-stable Q shrinks
the Wand unifier set to one that HAS an mgu). What is open is whether the
*lacks* shape of conjunct does it for the wand configuration specifically. See
§8 — it is a one-afternoon experiment and it should be the first thing done.

### (c) Diagnostics — the checker's only actionable message

⊥ is the one verdict that is both definite and bad news. "Field `foo` is
definitely absent from this attribute set" is the only thing this system can
ever say that a nixpkgs maintainer can act on without judgement; everything else
is ★ and a shrug. Note carefully that T-sel-⊥ must STAY at ★ and a warning —
laziness means the selection may never be forced, and rejecting it would violate
R2. *So negative information does not make the type system stricter*. It makes the
*linter* useful, which is what the evaluation chapter needs.

### (d) It is the enabling layer for occurrence typing

`if x ? l then x.l else d` is the Nix idiom for optional attributes. The
then-branch wants a positive fact, the else-branch wants exactly α ⊬ l. Without
negative information the else-branch cannot be expressed at all, so occurrence
typing — currently parked in @sec-extensions — is blocked on this plan and on
nothing else. Say so there.

### (e) It types removeAttrs without a restriction type-former

We declined P&X's `\l` operator. With negative facts, `builtins.removeAttrs`
gets a best-effort signature with no new type syntax:

    removeAttrs :: ∀(ρ ρ′: Row). ρ′ ⊬ l ⇒ {ρ} → {ρ′}

The result row is fresh and *known* to lack l. Weak — it forgets everything ρ
knew — but honest, and it is the only signature in @nix-features' "best-effort
signatures" row that currently has no home. `intersectAttrs` is the same shape.
`attrNames` is NOT: it needs first-class rows, which stays out of scope.


## 3. Where the facts come from

A constraint language with no producers is dead weight. Rank the sources by how
much of nixpkgs they reach:

  1. **Record literals and ground rows.** Free, already derivable, no constraint
     needed — L-ε and L-miss do it. This is the base case that makes discharge
     succeed in practice.
  2. **`?`-guards, via occurrence typing.** The biggest real-world source, and
     circular with (d): each unblocks the other. Do negative info first, since
     it is the smaller of the two.
  3. **Builtins.** `removeAttrs`, `intersectAttrs` — see (e).
  4. **Annotations.** R1 permits admitted-but-not-required annotations, so a
     user-written `β ⊬ l` is legitimate and is the honest story for the wand
     example. Do NOT let this become the only source; a feature that only works
     under annotation does not reach nixpkgs verbatim.
  5. **Inference.** Never. The algorithm must not invent a negative fact — that
     would be guessing about shadowing, which is the one thing the whole
     unification design refuses to do. Negative facts are *propagated*, never
     *proposed*.


## 4. The cost: inhabitation breaks

This is the sharpest technical consequence and the one to get right.

T-let's second premise (∃τ₁. σ ≥_Γ τ₁) currently holds *by construction*: a
parked stump always discharges — found, ⊥, or ★ — so a scheme is never vacuous.
The comment in algorithmic.typ already names what goes wrong otherwise ("with
Q ≠ ∅ a scheme can have NO Γ-instance, and `let x = (3 4) in 5` would type while
being stuck and progress would be false"). It is currently hypothetical. A
negative constraint makes it live: ⟨(l: Int).l ↓ ⊥⟩ has no instance at all.

Two ways out:

  A. Check satisfiability at generalization. Costs a decision procedure and a
     new failure mode at a binder.
  B. **Restrict negative constraints to atoms whose subject is a QUANTIFIED row
     variable.** Then θα ≔ ε always satisfies them, inhabitation is preserved
     verbatim, and no check is needed.

Take B. §1's decomposition lemma is what makes it a restriction and not a
mutilation: every negative constraint either normalizes into that shape or is
already refuted at generalization time, where refuting it is a plain error and
not a new kind of vacuity. The two results are the same theorem read twice.

  - [ ] restate T-let's inhabitation premise as still-by-construction under B
  - [ ] confirm F-★ / finalization is unaffected (it is: a negative atom has
        nothing to finalize — it is a fact, not a pending result)


## 5. Declarative rules

    Γ ::= … | Γ·(α ⊬ l)

    α ⊬ l ∈ Γ
    ------------- L-α-⊥
    Γ ⊢ α.l ↓ ⊥

Discharge gains an arm, mirroring D-hit / D-⊥ / D-?. A negative atom is
discharged by *running the lookup*, exactly like a positive stump — the
difference is that it can fail:

    Γ ⊢ (θρ).l ↓ ⊥                      Γ ⊢ (θρ).l ↓ τ
    ----------------- D-lacks           ------------------- (no rule)
    Γ; θ ⊢ ⟨ρ.l ↓ ⊥⟩                    instantiation REFUSED

    Γ ⊢ (θρ).l ↓ ?
    ----------------- D-lacks-?         ← DESIGN CHOICE, see §9
    ???

The `?` case is the one real question in the declarative design. Three options:
refuse (sound, but rejects programs the soft-typing position says we must
accept), accept-and-re-park (matches K-repark, keeps the obligation alive), or
accept-and-forget (unsound — it would let a violated fact through). Recommend
re-park, which is what the algorithm does anyway and what keeps D-lacks the
declarative image of the wake-up.

NOT recommended: making ≈ read negative facts. It is tempting — with α ⊬ l the
swap (α | l: τ) ≈ (l: τ | α) becomes shadowing-preserving, so a row-variable
stops being a barrier *at l*, and the spine theory gets finer. But ≈ is
currently a context-free syntactic relation, proved as such in RowEquiv.lean and
used by T-eq; making it Γ-relative touches the trace-monoid characterization,
cancellativity, and every proof that rewrites under ≈. Put the whole gain in ↓
and ≐ᵣ instead, where the context is already threaded. Revisit only if a
worked example demands it. (The label-relative-barrier idea itself is shared
with fc-labels-plan.md §8 — same notion, different motivation.)


## 6. Algorithmic side

Solver state grows a fourth component — a set, not a term store:

    S := (θ, Δ, W, N)        N := ∅ | α ⊬ l, N

INVARIANT: θ never binds a variable in a way that violates N. That single
sentence is the whole solver obligation, and it is checked at exactly the three
sites that bind a row-variable:

    U-var-solve   α ≔ s        for every (α ⊬ l) ∈ N, s must have no l-field;
                               if it does → clash. If s has variables, the
                               atoms MIGRATE: (α ⊬ l) becomes (β ⊬ l) for every
                               β ∈ vars(s).
    U-expand      β ≔ (l: δ | β′)   contradicts (β ⊬ l) directly → clash.
                               This is the good case: negative info turns a
                               guess into a decisive verdict.
    U-ε-var       β̄ ≔ ε        always consistent; ε lacks everything.

Detectors become label-relative. Define `lacksAt l s` = every variable in s is
known by N to lack l. Then:

    U-ground   "other side var-free"  →  "other side lacksAt l"
    U-clash    "vars(s₂) = ⟨⟩"        →  "lacksAt l s₂"
    U-expand   candidate hosts        →  variables NOT known to lack l

All three stay forced — the counting arguments are unchanged, they just run
against a weaker hypothesis. U-clash is the one to be careful with: relaxing it
makes clash fire MORE often, and clash is the verdict that is proved to mean
"no unifier exists". Its soundness now depends on N's soundness, so N must never
contain an atom the caller has not actually established. See §3 item 5.

Wake-up: a negative atom is *not* a stump and never resolves — it is checked, not
discharged. So Δ and N stay separate and no fixpoint is needed. But the blocker
index generalizes the same way as in fc-labels-plan.md §7: a parked stump
blocked on α wakes when α is *solved* or when α *gains a negative atom*, because
L-α-⊥ can now resolve it. That is the second wake-up trigger.

Generalization: an atom is carried iff its subject is generalized — the same
split as stumps, and it feeds the same least-fixpoint (carrying α ⊬ l pulls α
into ᾱ, which may pull in a stump mentioning α, and so on). Bounded by |N| + |Δ|.


## 7. Why this is affordable where ROSE is not

@related-work argues against qualified row predicates on the grounds that
entailment is left as a parameter, is unification modulo AC, and is NP-complete.
Adding ⟨ρ.l ↓ ⊥⟩ is adding a qualified predicate, so that paragraph has to be
revisited honestly rather than quietly. The distinction is real and worth stating
plainly:

  - ROSE's predicate is ρ₁ ⊙ ρ₂ ~ ρ₃ — an equation in a partial monoid. Deciding
    conjunctions of those is AC-unification.
  - Ours is unary, label-indexed, and decided by *running a relation we already
    have*. §1 says the constraint set is a finite set of (variable, label) pairs;
    entailment is a membership test; the solver obligation of §6 is a scan.

This is the `lacks` fragment of Gaster & Jones @gaster_jones, which is precisely
the fragment with a linear-time solver, and it is the part of the qualified-types
machinery we can take without taking the framework. Cite it directly. The
thesis' position becomes sharper, not weaker: *we reject the predicates that need
a search and keep the one that needs a lookup.*


## 8. First experiment — do this before anything else

Refutations.lean's wand configuration is, verbatim:

    ws₁ = [var β, var α]        ws₂ = [field l 𝓫]

All twelve moves are `none` by rfl and U-expand refuses on two candidate hosts.
Add the single atom β ⊬ l and the claim is that the configuration becomes
forced: β is no longer a candidate, α is the unique host, U-expand fires,
α ≔ (l: δ | α′), residual (β | α′) ≐ᵣ ⟨⟩ hits U-ε-var, and the run ends at
α ≔ (l: 𝓫), β ≔ ε — which should be the unique mgu of the constrained problem,
since any unifier must send β to something l-free contributing no fields.

  - [ ] check this by rfl against the existing definitions with N stubbed in
  - [ ] then check the OTHER refuted configurations the same way —
        stuck_masks_mgu (mρ₁/mρ₂) and terminal_masks_mgu. How many stuck shapes
        does one atom rescue? That count is the honest measure of what this
        extension is worth, and it is cheap to obtain.

If the answer is "one", this is a nice section. If it is "most of them", this is
a chapter, and the stuck leg of the trichotomy gets a conditional converse:
*terminal AND l-resolved ⟹ no mgu*. Note that a converse has been refuted three
times already (see proof-state.md) — do not start proving before the experiment
says which shape survives.


## 9. What it does NOT do

Say all of this out loud in the thesis; the feature invites overclaiming.

  - It is not negation of types. α ⊬ l is per-label absence, not a complement.
    No set-theoretic types, no unions, nothing changes about ★.
  - It does not remove ★. The wand example still needs a *source* for the fact
    (§3); with no source it is ★ exactly as today.
  - It does not make `stuck` complete. §8 may shrink the class; there is no
    reason to expect it to empty it.
  - It does not give `attrNames` a type. That needs first-class rows or a label
    set — out of scope in fc-labels-plan.md §3 and out of scope here.
  - It does not make the checker reject more programs. T-sel-⊥ stays soft (§2c);
    laziness and R2 forbid anything else.


## 10. Mechanization stages

Cheaper than the FC-label plan — nothing in the Row/Ty datatypes changes, which
is the whole difference. Keep the axiom count at zero between stages.

  - [ ] N0. The §8 experiment, with N stubbed as a plain list of (TyVar × Label)
        and the detectors taking it as an extra argument. Throwaway code; the
        point is the rfl-checked answer.
  - [ ] N1. `Lacks` as a context extension + `L-α-⊥` + re-prove lookup det /
        mono / total. Monotonicity is the interesting one: adding an atom
        REFINES ? to ⊥, so the extension order is (row solutions ∪ atoms) and
        the statement needs both. ⊑ᵣ already has the right shape (r ⊑ᵣ ?), so
        the precision story is unchanged.
  - [ ] N2. The decomposition lemma (§1). Small, self-contained, and it is what
        licenses the restriction of §4.
  - [ ] N3. `D-lacks` + the T-let inhabitation argument under restriction B.
  - [ ] N4. `lacksAt` and the relaxed detectors in RowUnify/Defs. Soundness
        obligation concentrates in U-clash — write the regression first, as in
        fc-labels-plan.md §S4.
  - [ ] N5. The binding-site checks and atom migration in U-var-solve.

RISK is low and the reason is worth noting: unlike FC-labels, this extension
adds no constructor to `Row` or `Ty`, so nothing in RowEquiv or the success leg
moves. The exposure is confined to the detectors and to U-clash.


## 11. Open questions

  - The D-lacks-? arm (§5). Re-park is the recommendation; confirm it against a
    worked example where the obligation outlives a `let`.
  - Atom migration on α ≔ s when s has several variables: (α ⊬ l) becomes an
    atom on EVERY variable of s. Is that forced, or is it over-strong? It looks
    forced — any of them could host the l — but check it against a case where s
    is later cancelled.
  - Interaction with the occurs legs. A negative atom shrinks the solution set;
    proof-state.md's `occurs_allVar_hasMgu` says α ≐ᵣ (β|α|γ) is reported occurs
    yet has an mgu. Does an atom change that verdict either way? Probably not —
    occurs is syntactic — but confirm rather than assume.
  - With FC-labels, ⟨ρ.α ↓ ⊥⟩ for a LABEL variable α is a genuine lacks
    predicate in the P&X sense and is what makes their `(x\l).foo` work. It also
    breaks §1's normal form, since the atom is no longer keyed by a concrete
    label. Decide whether to allow it BEFORE mechanizing either plan; the
    conservative answer is to require a literal label in every atom and revisit
    once both extensions are separately done.
  - Is there a dual worth having — a POSITIVE carried fact ⟨α.l ↓ τ⟩, "α is
    known to contain l: τ"? It is the `has` half of Gaster & Jones and it would
    type field access on an unconstrained argument. It is also a much bigger
    commitment (it must survive shadowing, so it interacts with precedence in a
    way absence does not). Out of scope, but name it, because a reader who knows
    the literature will ask why only half the predicate is here.
