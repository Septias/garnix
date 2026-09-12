
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
    - can still be VACUOUS
  - [¡] occurs: *incomplete*
    - α ≐ᵣ (β|α|γ)
  - [~] stuck : *incomplete*
    - (k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫)
    - (l:{w}) ≐ᵣ (w | v)
  
- Open
  - [ ] the expandR driver arm — two more cases in `unifySpineMF`, two more
    `Terminal` fields, and the reversal transport for the four reflection
    lemmas. The only thing between the self-reference filter and
    `terminal_masks_mgu`. MEASURED: `expandR` already exists (Defs.lean:263) and
    is never called; the driver succeeds on (l:𝓫|α) ≐ᵣ (m:𝓫|β) and goes STUCK on
    the mirror (α|l:𝓫) ≐ᵣ (β|m:𝓫). On terminal_masks_mgu's own configuration
    (l:{w}) ≐ᵣ (w|v), `expandR` fires with host v and composes to exactly the
    mgu the prose derives by hand — so the arm INVALIDATES terminalNoMgu_false,
    the sharpest refutation of the fourth leg. (stuck_masks_mgu is untouched:
    that one is about UResM.seq, not about terminality.)
  - [ ] `uniqueHost` needs the SOLVER STATE to close the last vacuous-success
    class: the offending field is hosted in a fresh tail that an earlier binding
    already made part of another variable, and the detector only compares the
    payload with the host variable itself. The accumulated solution is never
    threaded back into the spines. This is the gate on `UnifyWF`
    (State.lean:810) and hence on ⟦S⟧ being a total function — i.e. on items 1
    and 2 of plans/inference-gap-analysis.md's critical path.
  - [ ] sorted ftv, second half. `Ty/Row.sortedFtv` (State.lean) and
    `Ty/Row.allRowVars` (Defs.lean) exist; `bindTy` still tests the sort-blind
    `τ.ftv`, which is the over-conservatism the thesis flags.
  - [ ] ⊴ covering order on qualified schemes
  - [ ] solver state S = (θ, Δ, W), stump wake-up, confluence of the final state

## Termination
- Fuzzing suggests, that the algorithm actually terminates


## Principality
- [ ] covering order on schemes ⊴


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

