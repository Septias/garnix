
> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative typesystem and extend it to an algorithmic one in a similar fashion.

Our contribution is a _lookup relation_ that tries to solve one motivating example: `a: b: (a || b).l` which is a lookup on a concatenation of two row-variables that can not be typed easily. This wand-example is actually unsolvable, even with our effort. The novelty of our approach is to lookup a type on a _best-effort_ basis and give back an unkown result ★ in the wand-example. Our lookup relation thus returns a result out of (τ | ⊥ | ?) where ⊥ symbolises definite absence of a field and ? means "we don't know" (yet). Our lookup relation `Γ ⊢ ρ.l ↓ r` is able to lookup row-variables in the context that were instantiated on application. This can also be done with normal substitution of type-variables, but already shows the algorithmic implementation.

This mechanism allows to _refine_ types on function application. See the example `x: ({l: τ} || x).l` of type `{β} → ★` since the lookup-relation can not look past the type-variable introduced by x. Only after instantiation, it becomes clear whether the label is _shadowed_ or not. Applying the argument `x = {}` promotes the unknown type ★ to τ because it becomes clear that x does not shadow the label defined in the literal record.

The type-safety proofs have to account for this new lookup-mechanism in two ways: Progress can only be proven for definite types, but ★ forms a boundary where programs can get stuck. The preservation proof has to account for type refinement by allowing types to become more precise during small steps.

Principality forces qualified schemes that use parked stumps during unification to get mgus in many cases. The algorithm outputs three solution: A sucees with MGU, a failure without MGU, a _ for the wand example and finally an outcome »occurs«, that cuts across the other output paths. The occurs class of outputs is a syntactic check for recursive row-variables that naturally occur in nix.


## Related Files
- minimal.typ: provides a semi-formal method of the typesystem
- minimal.lean: provides a fully formal type-system
- algorithmic.typ: Algorithmic ideas
- algorithmic.lean: Reexport:
  - Qualified.lean: L2 qualified schemes, discharge, principality, QTyped
  - RowEquiv.lean: the ≈-characterization / trace-monoid normal form 
  - RowUnify.lean: the ≐ᵣ algorithm + trichotomy legs
  - Regressions.lean: Some kernel-checked examples
  - Axioms.lean: axiom guard
- In the bib/plaintext folder there is the plaintext version of the Paszke&Xie paper

# Progress
- [x] Scoped Records
- [x] Asymmetric Concat
- [x] Row Equivalence ≈
- [x] Refinement ⊑
- [x] Unknown Type Abstraction
- [x] Let-Statements
- [x] Qualified Schemes
- [~] Unification 
- [ ] FC-Labels
- [ ] Patterns
- [?] Occurrence Typing
- [?] Recursive Types
- [?] With
- [?] Inherit

## Tisch
- Subtyping for pattern functions
- Occurence Typing with ifs


## Unification
- [x] all worked examples mechanized;
- [x] ≐ᵣ executable; clash-soundness (projClash) done;
- Soundness
  - [x] Success
   - a .stuck is genuine;
- [x] OCCURS CHARACTERIZED — `.
- [x] CLASH ALGORITHM-LEVEL done — unifyRow_clash_no_unifier;
- [x] MGU-ON-SUCCESS done
- [x] STUCK⟹NO-MGU — but only as a REDUCTION, the naive statement is FALSE.
  - reusable order-agnostic threading principle done
- [~] MUTUAL ≐/≐ᵣ (proof-plan.md) — the rebuild that turns the stuck leg
  unconditional. P1 done: ≗-congruence (axiom-free), Sol/UResM/Sol.Sat at both
  sorts, the apply-then-unify bridge, sApplySubst. P2 done: Supply/Avoids over
  minimal.lean's natName, sFtv, the set*-invisibility lemmas, residual
  freshness for every detector. P3a done: host_forced MECHANIZES crossfield's
  maximality (the by-hand argument below), expand_shift/expand_reflect(_fwd),
  detectors. Next: P3b, wiring the arm into the dispatch cascade.
- [x] ALL THREE base techniques now done:
  - count-shrink (vars_vs_field_no_mgu)
  - rigidity (two_sided_no_mgu)
  - non-commutativity
- [x] MATCH/GROUND congruence now done:
- CAVEAT: unlike strip these SHRINK the unifier set (intersect with eq-satisfiers), so they transport mgu-status but a stuck residual does not by itself kill the original — that additionally needs the base-technique witnesses to satisfy the accumulated eq (the genuine augmented-witness content, now expressible via HasMguP).
- [!] ≐ᵣ IS INCOMPLETE — a stuck verdict that is WRONG (26-09-02):
  (l:𝓫 | α) ≐ᵣ (m:𝓫 | β), l ≠ m, is reported stuck yet HAS an mgu
  (α ↦ (m:𝓫 | X), β ↦ (l:𝓫 | X)). crossfield_stuck_unifiable mechanizes the
  stuck verdict + the unifier; maximality is derived by hand off
  rowEquiv_iff_char (proj_m A = (0,𝓫)::proj_m B, proj_l B = (0,𝓫)::proj_l A,
  proj_k equal otherwise, vars equal ⟹ A ≈ (m:𝓫|R), B ≈ (l:𝓫|R), same R) and is
  NOT yet mechanized. The missing rule is Rémy-style variable EXPANSION: when the
  leading field's label is absent from the other side's window and exactly ONE
  variable there can host it, that variable is FORCED to β ≔ (l:δ | β′). ≐ᵣ
  refuses to guess a host — right for Wand, where TWO vars could host it — but
  refusing when the host is UNIQUE costs completeness for free. This is the exact
  COMPLEMENT of the base-arm dispatch, whose witness techniques all need ≥2
  candidate hosts. Adding the move shrinks the stuck class to precisely what the
  techniques can kill; it also GROWS the spine, so it forces the same
  fuel→well-founded rebuild the mutual ≐/≐ᵣ design needs. Do them together.
- [ ] Remaining for the full lift: assembling the general base arm from the three techniques (+ the all-var argument generalized beyond the swap) and re-running those base witnesses under the accumulated equations (carry them as the HasMguP predicate through the fuel induction);
- [ ] type-level ≐
- MUTUAL ≐/≐ᵣ (26-09-02, verified by running the algorithm): making type- and
  row-unification mutually recursive, so an emitted equation is SOLVED AND
  APPLIED before the row pass continues, fixes the rescued-stuck example —
  (l:𝓫 | α) ≐ᵣ (l:𝓫) = success [α ↦ ε]. Structurally it turns the Q side
  condition from a HYPOTHESIS into an INVARIANT: no leftover equation can
  mention a stuck row-var if each was discharged into θ first. Caveats: genuine
  Wand is untouched (no equation to solve, provably no mgu); the fuel measure
  |s₁|+|s₂| dies because applying a solution GROWS the spine, so it must become
  the lexicographic (#unsolved vars, #atoms) — which makes the occurs guard
  LOAD-BEARING for termination, retroactively justifying its conservatism; and a
  parked STUCK equation can still mention stuck row-vars, so the Q side
  condition survives for those. See proof-plan.md.


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

