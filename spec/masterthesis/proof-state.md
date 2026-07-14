
> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative typesystem and extend it to an algorithmic one in a similar fashion.

Our contribution is a _lookup relation_ that tries to solve one motivating example: `a: b: (a || b).l` which is a lookup on a concatenation of two type-variables that can not be typed easily. The novelty of our approach is to lookup a type on a _best-effort_ basis. Our lookup relation thus returns a result out of (τ | ⊥ | ★) where ⊥ symbolises definite absence of a field and ★ means "we don't know" (yet). Our lookup relation `Γ ⊢ ρ.l ↓ r` is able to lookup row-variables in the context.

This mechanism allows to _refine_ types on function application. See the example `x: ({l: τ} || x).l` of type `? →★` since the lookup-relation can not look past the type-variable introduced by x. Only after instantiation it becomes clear whether the label is _shadowed_ or not. Applying the argument `x = {}` promotes the unknown type ★ to τ because it becomes clear that x does not shadow the label defined in the literal record.

The type-safety proofs have to account for this new lookup-mechanism in two ways: Progress can only be proven for definite types, but ★ forms a boundary where programs can get stuck. The preservation proof has to account for type refinement by allowing types to become more precise during small steps.

Our initial design immediately asks for two equivalences: We need a row-equivalence to provide width-subtyping to records. This relation is commutative, associative and transitive?, but only on the prefix of a row that does not contain a row- or label-variable since reordering past these could induce shadowing. The second relation is needed to equate ★ with ordinary types. This relation can not be transitive because it would deflate our type hierarchy.


## Current State
We have proven _type-safety_ for a minimal calculus with _scoped-rows_, _asymmetric concat_, _row-equivalence_ and an _unknown type_. We now want to add let-polymorphism similar to the one of Paszke&Xie.


## Future
The road to _type refinement_. Nothing fundamental blocks it — the design already anticipates it (rowEnv, *monotonicity*, the ↯-disjunct) — but four pieces are missing, in dependency order:

1. *Instantiation*: nothing ever solves a row-var yet — rowEnv is only read (L-α), never written, so the proofs at Γ = ∅ exercise none of it. Let-polymorphism (generalize row-vars at binders, solve at use sites) is the mechanism that creates the refinement event.
2. *Precision relation ⊑*: refinement means the ★ from T-sel-★ later becomes τ, so preservation must be stated as `∅ ⊢ e': τ'` with `τ' ⊑ τ`. This is the bottleneck: _head rigidity fails at ★_ (anything sits ⊑-below it), canonical forms weaken, and the whole inversion-mod-≈ machinery must be redone mod ≈-and-⊑. Interaction of ⊑ with ≈ has to be fixed (⊑ must not be transitive, see Motivation).
3. *★-elimination rules*: currently ★ is inert — nothing applies, selects on, or concatenates a ★-typed value. With T-app-★ etc. the ↯-disjunct stops being decorative: ★-typed selections can then reach records lacking the label, and progress-up-to-↯ becomes the real theorem.
4. *Typing monotonicity*: the actual refinement lemma — if `Γ ⊢ e: τ` and Γ' extends Γ's row-solutions, then `Γ' ⊢ e: τ'` with `τ' ⊑ τ`. *Monotonicity* of lookup is exactly its base case; it just has to be lifted through the typing rules. Needs ⊑ (2) to even be stated.

Note: refinement is purely a typing-side phenomenon — small steps never touch types. β replaces x by a concrete record and the reduct _admits_ a more precise type; preservation-modulo-⊑ is what "types become more precise during small steps" means formally.

## Related Files
- minimal.typ: provides a semi-formal method of the typesystem
- minimal.lean: provides a fully formal type-system
- In the bib/plaintext folder there is the plaintext version of the Paszke&Xie paper

# Progress
- [x] Scoped Records
- [x] Asymmetric Concat
- [x] Row equivalence
- [x] Unknown Type Abstraction
- [~] Let-Statements
- [ ] FC-Labels
- [ ] Patterns
- [¿] Occurrence Typing
- [¿] Recursive types
- [¿] With
- [¿] Inherit


## Proof Overview
Proofs are for _closed_ programs (Γ = ∅). e ↯ marks _lookup-errors_: a selection reached a record literal without the label. ★ makes such programs typeable, so progress only holds up to ↯. Preservation keeps the type on the nose — refinement only appears once instantiation is added.

*Progress*: If ∅ ⊢ e: τ then e ∈ Values, or ∃e' such that e → e', or e ↯
*Preservation*: If ∅ ⊢ e: τ and e → e' then ∅ ⊢ e': τ
*Soundness*: If ⊨ e: τ then ⊢ e: τ
*Completeness*: If ⊢ e: τ then ⊨ e: τ


## Lemma Overview
- Progress & Preservation:
  - *record inversion*: T-eq can wrap any derivation which has to be stripped.
  - *head rigidity*: ≈ₜ never changes the head constructor, so we can get "back" our shape
  - *lookup-equivalence*: lookup-category (τ | ⊥ | ★) is not changed by row-equivalence.
  - *term/type agreement*: Lookup on types carries over to syntax-lookup
- Progress:
  - *canonical forms*: A value's syntactic shape is determined by its type's head.
- Preservation:
  - *substitution*: substitution preserves typing, standard.
    - *context conversion*: typing only sees the context through lookups, so contexts that agree on lookups type the same terms. Subsumes weakening, exchange and shadowing.
    - *rowEnv congruence*: lookup only depends on row-solutions, so substitution leaves lookups untouched
  - *spine-var-freeness*: literal rows carry no row-var in their spine, so no ★
- Standalone Metatheory:
  - *determinism*: lookup is deterministic
  - *monotonicity*: definite results (τ/⊥) survive extending the row-solutions, only ★ can improve: the lemma that makes deferring lookups sound
  - *totality*: under acyclic row-solutions (RowWF) every lookup has a result


