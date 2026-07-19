
> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative typesystem and extend it to an algorithmic one in a similar fashion.

Our contribution is a _lookup relation_ that tries to solve one motivating example: `a: b: (a || b).l` which is a lookup on a concatenation of two type-variables that can not be typed easily. The novelty of our approach is to lookup a type on a _best-effort_ basis. Our lookup relation thus returns a result out of (τ | ⊥ | ★) where ⊥ symbolises definite absence of a field and ★ means "we don't know" (yet). Our lookup relation `Γ ⊢ ρ.l ↓ r` is able to lookup row-variables in the context that were instantiated on application.

This mechanism allows to _refine_ types on function application. See the example `x: ({l: τ} || x).l` of type `? →★` since the lookup-relation can not look past the type-variable introduced by x. Only after instantiation it becomes clear whether the label is _shadowed_ or not. Applying the argument `x = {}` promotes the unknown type ★ to τ because it becomes clear that x does not shadow the label defined in the literal record.

The type-safety proofs have to account for this new lookup-mechanism in two ways: Progress can only be proven for definite types, but ★ forms a boundary where programs can get stuck. The preservation proof has to account for type refinement by allowing types to become more precise during small steps.

We have two relations: A row equivalence (≈) relation that allows us to swap labels on the head of a row (up until the first type-variable) and a precision relation (≤) that allows us to type things as ★.


## Current State
We have proven _type-safety_ for a minimal calculus with _scoped-rows_, _asymmetric concat_, _row-equivalence_, an _unknown type_ and _let-polymorphism_ (schemes over both type- and row-variables, instantiation by substitution at T-var). Preservation holds **on the nose** — see the two design findings below for why that required two new rules and a non-standard T-let.

Design finding 1 (26-07-14): adding T-let as specced breaks preservation *fatally* — instantiation can demote a `?` lookup to `⊥` (or to a definite `τ`), leaving reducts untypeable, e.g. `let f = (x: x.l) in f {}` steps to `(x: x.l) {}` whose body needs a rule for `ε.l ↓ ⊥`. Fix (adopted, see minimal.typ): two new rules, *T-sel-⊥* (`ρ.l ↓ ⊥ ⟹ e.l: ★`, soft typing — flagged statically, ↯ at runtime) and *T-★-intro* (`e: τ ⟹ e: ★`, the non-transitive "second relation" from the Motivation, kept out of ≈ so head rigidity survives). With both, safety stays on the nose: refined types re-blur to ★, pushing ⊑ out of the safety proof entirely.

Design finding 2 (26-07-14): the standard side condition `ᾱ = ftv(τ₁) ∖ ftv(Γ)` is *syntactic about names* and breaks context conversion (a context above Γ in the Sub-preorder can mention ᾱ), which would force α-renaming/equivariance machinery through every proof. The mechanization instead uses the **instance-closed T-let**: `(∀ τ₁ ≤ σ. Γ ⊢ e₁: τ₁) ∧ Γ·(x: σ) ⊢ e₂: τ₂ ⟹ Γ ⊢ let x = e₁ in e₂: τ₂`. Generalization is sound by construction, the premise is precisely the hypothesis of the polymorphic substitution lemma at let-β, and no capture/renaming machinery exists anywhere. The syntactic rule is admissible via a type-substitution lemma (future work; that lemma is where the named-variable tax would be paid, quarantined away from safety). Every scheme has its own body as trivial instance (θ = id), so the premise also feeds progress. The regression example at the bottom of minimal.lean types `let f = (x: x.l) in f {}` at ★, exercising all three selection rules across the instances.


## Future
The road to _type refinement_. Nothing fundamental blocks it — the design already anticipates it (rowEnv, *monotonicity*, the ↯-disjunct) — in dependency order:

1. *Typing monotonicity* (the refinement theorem, **next Lean session**): if `Γ ⊢ e: τ` and `Γ ⊑ Γ'` (rowEnv only grows), then `Γ' ⊢ e: τ'` with `τ' ⊑ τ`. ⊑ SPECCED (26-07-19) in minimal.typ (Precision + Refinement sections): Siek-style precision — covariant in ALL positions incl. function domains, ★ top, rows pure congruence (no row-level ★, all imprecision bottlenecks through the type ★). Port as `TyPrec`/`RowPrec`/`ResPrec` inductives, restate lookup_mono's conclusion as `r' ⊑ r`, then induct on typing with lookup_mono as the T-sel-★ base case. Pre-declared helper lemmas: *⊑/≈ commutation* (the T-eq case) and *⊑-rigidity* (τ' ⊑ τ ∧ τ ≠ ★ ⟹ same head). Transitivity/antisymmetry-mod-≈ kept admissible, not rules — small inversions. This turns contribution 2 (refinement at instantiation) from example into theorem.

2. *Algorithmic system* (unification, paper-first): constraint generation + unification over scoped rows; unresolved lookups become deferred constraints, unification *writes* rowEnv — (1) is exactly why deferring is sound. Soundness statement needs ⊑: the inferred type sits below every declarative type. This is also where the ★-taming story lives: since T-★-intro makes "∃ derivation" nearly vacuous, "the checker flags e" must be defined against the *most precise* derivation the algorithm computes, not against existence. Mechanization of unification: out of scope unless time abounds.

3. *★-elimination rules*: DEFERRED behind (1) and (2) — deliberately. Two reasons (26-07-19): (a) every rule added before the monotonicity induction exists adds cases to it; (b) T-★-intro + T-app-★ makes the declarative system a universal sink (blur `3`, apply it: `3 4` types at ★), so elimination is only defensible once (2) defines flagging via principality. minimal.typ records the guard: ⊑-subsumption is NOT admissible, T-★-intro blurs only at top level — that line keeps the sink latent. Related honest-writing note: T-sel-⊥ and T-sel-★ both conclude ★, so "flagged statically" is checker instrumentation (which rule fired), not readable off the type. Partially delivered already by T-sel-⊥ (↯-disjunct non-decorative).

4. *Syntactic T-let admissibility*: staged and waiting — the *substitution stability* lemmas are proven and currently unused precisely for this. Medium effort; alternative is presenting the instance-closed rule in the thesis (the declarative figure currently shows the ftv-rule the mechanization does NOT prove — either fix the figure's claim or prove admissibility, don't leave the gap silent).

5. *Stretch, in order*: patterns (meeting-notes candidate, tractable), FC-labels (drags in label variables + kinds, likely paper-only in "Towards Nix").

Writing debt (parallel, no dependencies): thesis.typ Metatheory section still claims "a weak form of preservation" — backwards, preservation is on the nose and *progress* carries the ↯-weakening; the "Informal Description" stub = the design-finding-1 story; a discussion paragraph on the re-blurring trade-offs (preservation-at-★ is nearly vacuous, the `∨ τ = ★` inversion tax, the sink risk) before a reviewer writes it first.

## Related Files
- minimal.typ: provides a semi-formal method of the typesystem
- minimal.lean: provides a fully formal type-system
- In the bib/plaintext folder there is the plaintext version of the Paszke&Xie paper

# Progress
- [x] Scoped Records
- [x] Asymmetric Concat
- [x] Row equivalence
- [x] Unknown Type Abstraction
- [x] Let-Statements (instance-closed T-let; syntactic-rule admissibility open)
- [ ] FC-Labels
- [ ] Unification
- [ ] Patterns
- [¿] Occurrence Typing
- [¿] Recursive types
- [¿] With
- [¿] Inherit


## Proof Overview
Proofs are for _closed_ programs (Γ = ∅). e ↯ marks _lookup-errors_: a selection reached a record literal without the label. ★ makes such programs typeable (now also via T-sel-⊥), so progress only holds up to ↯ — and since T-sel-⊥, the ↯-disjunct is no longer decorative even without ★-elimination rules. Preservation keeps the type on the nose even under instantiation: T-★-intro re-blurs refined types to ★, so refinement is visible only as "the reduct *also* admits a more precise type" (the future monotonicity theorem), never as a weaker preservation statement.


*Progress* (`progress`): If Γ = ∅ and Γ ⊢ e: τ, then `Progress e` — i.e. ∃e' with e → e' (`step`), or e ∈ Values (`done`), or e ↯ (`err`).
*Preservation* (`preservation`): If ∅ ⊢ e: τ and e → e' then ∅ ⊢ e': τ — the type is preserved on the nose.
*Soundness*: If ⊨ e: τ then ⊢ e: τ
*Completeness*: If ⊢ e: τ then ⊨ e: τ


## Lemma Overview
- Progress & Preservation:
  - *record inversion*: T-eq and T-★-intro can wrap any derivation and have to be stripped; each inversion gains a `∨ τ = ★` disjunct (harmless for canonical forms since fn/rcd heads ≠ ★).
  - *head rigidity*: ≈ₜ never changes the head constructor, so we can get "back" our shape. Now includes ★-rigidity (★ ≈ σ ⟹ σ = ★) because T-★-intro lives outside ≈.
  - *lookup-equivalence*: lookup-category (τ | ⊥ | ★) is not changed by row-equivalence.
  - *term/type agreement*: Lookup on types carries over to syntax-lookup
- Progress:
  - *canonical forms*: A value's syntactic shape is determined by its type's head.
  - *scheme non-vacuity*: every scheme has its own body as instance (θ = id), so a let-binding is always typeable at something.
- Preservation:
  - *polymorphic substitution*: if x: σ and v types at every instance of σ, then e[x:=v] keeps its type — T-let's instance-closed premise is exactly this hypothesis; monotype β is the singleton case.
    - *context conversion*: typing only sees the context through lookups, so contexts that agree on lookups type the same terms. Subsumes weakening, exchange and shadowing (now at scheme granularity).
    - *rowEnv congruence*: lookup only depends on row-solutions, so substitution leaves lookups untouched
  - *spine-var-freeness*: literal rows carry no row-var in their spine, so no ★
- Standalone Metatheory:
  - *determinism*: lookup is deterministic
  - *monotonicity*: definite results (τ/⊥) survive extending the row-solutions, only ★ can improve: the lemma that makes deferring lookups sound
  - *totality*: under acyclic row-solutions (RowWF) every lookup has a result
  - *substitution stability*: definite lookups survive type substitution (syntactic analog of monotonicity); ≈ is a congruence for type substitution. Currently unused by safety — will serve the syntactic-T-let admissibility and refinement theorems.


