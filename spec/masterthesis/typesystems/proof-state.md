
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
- [ ] FC-Labels
- [ ] Negative type information
- [?] Patterns
- [?] Occurrence Typing
- [?] Recursive Types
- [?] With
- [?] Inherit


# Property-Overview
*Soundness*
★-elim rules  →  A-app-degrade decided  ┐
UnifyAcyclic  →  ⟦S⟧ total  →  A-sel    ├→ InferSoundC → QTypedCDischarge → RunSound
SchemeImage (forward-only) →  A-let     ┘

*Termination*
unification measure  ┐
A-let Δ-fixpoint     ├→ Infer is a FUNCTION → W exists → "the scheme W produces" is sayable
↝* wake-up closure   ┘
                      + determinism up to α-renaming → W's output is unique

*Principality*
instance-closed  ←  RunSound
inhabited        ←  "stumps always finalize"  ←  spent promise (FALSE in general — needs one of its three exits)
covers ≼         ←  W exists  +  ⊴≼  (order done; the conjunct is unstateable without W)
                                    ↓
                        ∀e ∃σ. Principal Γ e σ


## Unification
- Unification Outcomes
  - [~] success: *sound & complete*
    - Is vacuous if s is unsatisfiable (is critical for InferSoundness)
      - Underspecified
  - [~] occurs: *sound where it is LOCAL*. 
    - can be unblocked by removed U-expand?
  - [~] stuck : *conservative, and there is NO general converse*
    - three no-mgu theorems, three witnesses
  
- Open
  - [ ] UnifyWF
    - has no counterexample
    - [ ] UnifyAcyclic
    - Needs a proper ranked measure (3 already disputed)
      - a binding never mentions the variable it binds *survives*
      - Problem: Cyclic dependencies
      - But: The depenency graph is a DAG


## Termination
- Fuzzing suggests, that the algorithm actually terminates


## Inference
- [~] InferSound
  - machinery is built
  - 6 / 13 arms not proved
  

## Principality
- [x] covering order on schemes ⊴ 
- [x] `Principal selQ (λx.x.l)`
- [ ] General principality


# Problems
> Problems found during mechanized proving and their proposed solutions

- [!] **★ IN AN ELIMINATION POSITION HAS NO A-RULE AT ALL**
  WHAT IS NEEDED is an `A-app-★` / `A-sel-★` / `A-conc-★` family: when the
  scrutinee's type is already ★, the elimination yields ★ (plus a W-flag)
  WITHOUT running an equation. *That is an algorithmic fix and it is small.*


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
- ⊴: "At least as general" (covering order on schemes) — σ ⊴ σ' : σ' covers σ
- ⊴⊑: covering up to precision — σ' answers each σ-instance with a ⊑ₜ-sharper one

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
  - *L1 ⊆ L2*: Typed.toQ embeds every plain derivation (Q = ∅ instances)
- IS L2 »SOUND & COMPLETE«? — audit 2026-09-13, build green, no sorries
  - *safety*: YES. qProgress/qPreservation are proven over ⊢_Q directly (Step/Value/Err
    reused from L1), preservation ON THE NOSE, axiom-guarded in Axioms.lean
  - *vs. L1*: BOTH directions — DONE 2026-09-18. ⊆ is Typed.toQ; the converse is
    `l1_rejects_two_use` / `l1_strictly_weaker` (Qualified.lean), so **L1 ⊊ L2 is a
    THEOREM**, no longer prose. The proof does NOT reuse no_plain_principal_scheme
    (that one is pinned to τ₀ = {ε} and to syntactic instance types); it re-runs the
    same argument mod ≈:
      * the `a` use pins a DEFINITE result — projecting label a out of the record
        type through `lookup_equiv` + `lookup_det` gives τa ≈ 𝓫_c, so the scheme has
        an instance whose result is 𝓫_c;
      * the `b` use forces an ε DOMAIN, and then instance-closedness forces that
        instance's RESULT to ★ (selEx_dom_empty_res, i.e. sel_var_unk read through
        the λ). A ★ domain is excluded outright: a selection on a ★-bound variable
        has NO typing at all (sel_var_of_unk), since every selection rule demands
        the scrutinee at a record type and ★ is ≈-rigid;
      * so σ.body's result position is a bare quantified variable, the domain cannot
        depend on it, and re-pointing it inside the ⊥-use's substitution yields the
        underivable instance {ε} → 𝓫_c.
    THE ONE STEP THAT IS NOT IN no_plain_principal_scheme is the last: there the
    ⊥-instance's domain was SYNTACTICALLY {ε}, here it is only ≈ {ε}, so "the domain
    does not mention the result variable" had to be earned. `Row.hasSing` +
    `rowEquiv_hasSing` + `hasSing_applySubst` + `applySubst_rowOnly` (Qualified.lean)
    do it: ≈ never creates or destroys a field, so a row ≈ ε is field-free ANYWHERE,
    a field-free row has no TYPE positions at all, and its substitution image
    therefore reads only θ.row — the re-point cannot reach it. That quartet is
    general and reusable; it is the field-count invariant in the form the
    mixed-instance construction needs.
    New L1 inversions this needed, none of which existed: `typed_let_inv'`,
    `typed_app_inv'` (typed_inv_aux only covers con/lam/rcd) and `tvar_inv` (the
    general-scheme `var_inst_inv`). `sel_var_unk` / `var_inst_inv` are no longer
    `private` in minimal.lean. Axiom-clean.
    A second, independent proof of the same theorem existed on
    worktree-prec-equiv-commutation (lean/Strictness.lean, 2026-09-15:
    `l1_strictly_weaker_than_l2` / `no_plain_scheme_two_use`). It was dropped as
    duplicate when that branch was merged — do not re-derive it.
  - *completeness w.r.t. inference*: NO, and not yet stateable. No W (algorithmic.lean is
    an import root); unifyRowM_success_iff is completeness for ≐ᵣ, not for ⊢_Q; and
    principality for L2 exists only as the single-example bookend qualified_principal_scheme
    (λx.x.l), not as "∀e ∃σ principal". The ⊴ order now exists (see Principality); what is
    still missing is a W and the covering conjunct for a scheme it produces
  - *watch item*: qLet's INHABITATION premise (∃τ₁. σ ≥_Γ τ₁) is non-standard — it exists
    because progress is false without it. Any future completeness proof must show inference
    always discharges it ("stumps always finalize")

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

