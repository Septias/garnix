
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
- [~] Type inference
- [ ] FC-Labels
- [ ] Negative type information
- [?] Patterns
- [?] Occurrence Typing
- [?] Recursive Types
- [?] With
- [?] Inherit


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
  problem has no unifier. Stage 3's filter cannot see it: the second field is
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

# Current Notes
> Notes about the current state

## Unification
- Outcomes
  - [~] success: sound & complete — but a success can still be VACUOUS. Both
    theorems quantify over θ satisfying the returned solution, so a solution no
    θ satisfies makes them hold trivially. Two such classes are closed (Stage 2
    at U-var-solve, Stage 3 at a self-referential lone host); a third survives —
    a cycle through PAYLOADS across two bindings, e.g.
    `(b | a) ≐ᵣ (l:{l:𝓫 | a} | l:{a | b})` returning `aaaa ≔ {ε | b}` with
    `b ≔ (l:aa | l:aaaa | ε)`. The fuzzer's `solRankedB` is the live detector
    (0 / 576 / 8 across the three universes).
  - [x] clash: sound
  - [~] occurs
    - incomplete!
    - [x] the genuine case really has no unifier 
    - [x] SHARP incopmleteness: α ≐ᵣ (β|α|γ) is reported occurs yet has an MGU (occurs_allVar_hasMgu)
  - stuck
    - *incomplete!*
    - [X] so "stuck -> ¬mgu" is FALSE at the algorithm level.
    - (k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫) is reported
      stuck yet has a UNIQUE mgu β≔(l:𝓫), α≔ε.  matchL emits {β|α} ≐ {l:𝓫},
      which IS Wand and IS stuck, and UResM.seq propagates that before the
      residual β ≐ᵣ (l:𝓫) — which pins β — is ever looked at.
    - [X] and the retreat to TERMINAL configurations is false TOO
      (terminalNoMgu_false): (l:{w}) ≐ᵣ (w | v) is terminal — all thirteen moves
      none by rfl — yet hosting in w would force θw ≈ (l:{θw}), *an OCCURS
      violation that field counting cannot see* (the recursion passes under a
      record constructor). One placement is ruled out, the unifier is UNIQUE,
      hence most general.
      SINCE STAGE 3 the reason U-expand refuses has CHANGED: w IS now filtered
      out as a candidate, and v is the lone survivor — but v does not LEAD the
      spine, and the leading condition is what keeps the move sound. The mirror
      (l:{w}) ≐ᵣ (v | w) now succeeds. So the witness is one driver arm away
      from dying: right-end expansion emits the field at the END, where it
      commutes past the (empty) SUFFIX, and `expandR` is already defined — the
      driver just never calls it.
      LESSON: terminality is a fact about the MOVES, not about the problem. There
      is no general converse at any formulation. New tool: Ty/Row.rcdDepth —
      record nesting, ≈-invariant (every constructor, `cat` taking a max), the
      first invariant here that sees THROUGH a field payload.
    - [x] what the fourth leg actually IS: the three SPECIFIC no-mgu theorems
      (vars_vs_field = Wand, two_sided, allvar_swap — each also at the On level)
      plus the three conservativity examples (occurs_allVar_hasMgu,
      stuck_masks_mgu, terminal_masks_mgu). Complete and honest; just not a
      converse.
    - [x] step 1: four leading shapes once stripL/matchL are dead
    - [x] step 2: U-expand refuses for exactly 3 reasons (`NoHost`, Defs.lean)
      — no unique surviving candidate, an l-field already on the side, or the
      leading candidate is self-referential. The third is the benign one: there
      the problem has no unifier at all (`selfref_host_no_unifier`).
    - [ ] step 3 still needs terminal_masks_mgu gone, and the route is now
      concrete rather than a guessed side condition: add the expandR arm to the
      driver (see the terminal entry above). The semantic tools the dispatch
      needs already exist — `selfref_host_no_unifier` for the new NoHost
      disjunct, `deep_occurs_no_unifier` underneath it. Three successive
      formulations of this leg have been refuted, so hunt for a counterexample
      before proving. Groundwork that survives regardless:
      lone_field_no_foreign / lone_field_count_le / spine_eq_map_var_of_no_fields
      / lone_field_other_pure_var (facing a lone field, counting alone forces the
      other side to be a pure var spine) and terminal_leading_shape.
- Invariants
  - [x] fuel monotone: more budget never changes a verdict already reached
  - [x] bounded: a run only mentions names below the supply it returns
  - [x] unique-host expansion is forced
  - [x] ≗-congruence of substitution on rows — axiom-free
- Open
  - [x] `HasMguOn V` / `InstanceOfOn V` — mgu relativized to a var set (Defs.lean).
    `¬HasMguOn V` is the STRONGER statement (factoring on V only is an easier
    demand), so it yields the thesis-facing `¬HasMgu` for free via
    not_hasMgu_of_not_hasMguOn — and unlike strict InstanceOf it is insensitive
    to the vars the algorithm invents. Ported: hasMguOn_congr (axiom-free),
    hasMguOn_rowEquiv/_symm, instanceOfOn_fieldCount_mono and
    _eq_of_varFree (both now need `x ∈ V`), no_mgu_on_of_witness_shrinks
    (stated for an ARBITRARY unifier predicate, which is what A4 needs).
    All four base techniques re-proved at the On level —
    wand_no_mgu_count_on, vars_vs_field_no_mgu_on, field_vs_vars_no_mgu_on,
    two_sided_no_mgu_on, allvar_swap_no_mgu_on — with the old names kept as
    one-line corollaries, so nothing downstream moved.
    CAVEAT: A4 was to be its main consumer and A4 is cancelled, so HasMguOn is
    currently NOT load-bearing — the terminal-configuration statement involves
    no invented vars and strict HasMgu would do. Kept because ¬HasMguOn V is
    strictly stronger (the base theorems say more for free) and because a
    DEFERRING driver would need it. Do not oversell it.
  - [ ] the algorithm repair suggested by stuck_masks_mgu: on a stuck sub-call,
    DEFER the equation, run the residual, retry against the solution — the
    parked-stump idea inside unification. Open: termination, confluence, and
    whether mgu-on-success survives. The only route to a non-conservative
    `.stuck`.
  - [ ] the expandR driver arm — two more cases in `unifySpineMF`, two more
    `Terminal` fields, and the reversal transport for the four reflection
    lemmas. The only thing between Stage 3 and `terminal_masks_mgu`.
  - [ ] `uniqueHost` needs the SOLVER STATE to close the last vacuous-success
    class: the offending field is hosted in a fresh tail that an earlier binding
    already made part of another variable, and the detector only compares the
    payload with the host variable itself. The accumulated solution is never
    threaded back into the spines.
  - [ ] sorted ftv, second half. `Ty/Row.sortedFtv` (State.lean) and
    `Ty/Row.allRowVars` (Defs.lean) exist; `bindTy` still tests the sort-blind
    `τ.ftv`, which is the over-conservatism the thesis flags.
  - [ ] termination. Naive Rémy measure does not close: renaming adds no fields,
    so the host keeps count_l = 0 and the same var is re-expandable at the same
    label; the bound must come from the other side's l-fields, which solve-and-apply adds.
  - [ ] ⊴ covering order on qualified schemes — needed to even STATE "principal type improves under reduction"
  - [ ] solver state S = (θ, Δ, W), stump wake-up, confluence of the final state
- Not blocked by any of this: L2 type safety (qProgress/qPreservation) and
  selQ_instance_closed stand on their own. The open work is purely algorithmic.
- [ ] Termination
  - Fuzzing suggests, that the algorithm actually terminates
- [ ] Principality
  - [ ] covering order on schemes ⊴

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
- ⊴: "At least as general" (covering order on schemes)

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

