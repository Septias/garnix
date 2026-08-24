
> This file serves as an overview of the current formalization efforts. It should give a comprehensible overview of current effort but even more importantly, an outlook of what to do next. 


## Motivation
We are creating a calculus that can be used to type real Nixlang code and base it on a row theory inspired by Paszke&Xie extending it with an unknown type ★ and a _delayed lookup relation_ (`Γ ⊢ ρ.l ↓ r`) to form a soft typing system with _type refinement_. We use _scoped rows_ since they give a natural semantic to _asymmetric concat_ where all concatenations are stored in a "bag" and looked up with left-precedence. The row theory of Paszke&Xie shows how to form a _sound typesystem_ with row- and label-variables that can be efficiently solved by _unification_. We want to provide a declarative typesystem and extend it to an algorithmic one in a similar fashion.

Our contribution is a _lookup relation_ that tries to solve one motivating example: `a: b: (a || b).l` which is a lookup on a concatenation of two row-variables that can not be typed easily. The novelty of our approach is to lookup a type on a _best-effort_ basis. Our lookup relation thus returns a result out of (τ | ⊥ | ★) where ⊥ symbolises definite absence of a field and ★ means "we don't know" (yet). Our lookup relation `Γ ⊢ ρ.l ↓ r` is able to lookup row-variables in the context that were instantiated on application.

This mechanism allows to _refine_ types on function application. See the example `x: ({l: τ} || x).l` of type `? → ★` since the lookup-relation can not look past the type-variable introduced by x. Only after instantiation it becomes clear whether the label is _shadowed_ or not. Applying the argument `x = {}` promotes the unknown type ★ to τ because it becomes clear that x does not shadow the label defined in the literal record.

The type-safety proofs have to account for this new lookup-mechanism in two ways: Progress can only be proven for definite types, but ★ forms a boundary where programs can get stuck. The preservation proof has to account for type refinement by allowing types to become more precise during small steps.

Principality forces qualified schemes that use parked stumps to during unification to get mgus in many cases. The wand-example is re-stated in a similar form to the motivating example and forces a trichotomy: success + mgu, hard error or no mgu possible. In comparison we can drop the LU-tail rule which forces fields into rows since our parking-mechanism allows us to postpone the decision on what to do in this case. This allows us to type [strictly more]¿ examples.

## Related Files
- minimal.typ: provides a semi-formal method of the typesystem
- minimal.lean: provides a fully formal type-system (frozen — L2 lives in its own file)
- algorithmic.typ: Algorithmic ideas
- algorithmic.lean: qualified schemes, discharge, principality
- In the bib/plaintext folder there is the plaintext version of the Paszke&Xie paper

# Progress
- [x] Scoped Records
- [x] Asymmetric Concat
- [x] Row equivalence ≈ (trace-monoid characterization + full cancellativity mechanized)
- [~] Refinement ⊑
- [x] Unknown Type Abstraction
- [x] Let-Statements (instance-closed T-let; syntactic rule proven admissible)
- [~] L2 qualified schemes (declarative QTyped + discharge + embedding mechanized; safety and strictness open)
- [~] Unification (algebraic foundation + all worked examples mechanized; ≐ᵣ executable; clash-soundness (projClash) done; SUCCESS-SOUNDNESS fully done + axiom-clean — unifyRow_success_sound, incl. the hard groundMatch counting and full fuel-induction assembly; FUEL-SUFFICIENCY done — unifySpineF_fuel_irrel/_stable, each move eats 2 atoms so |s₁|+|s₂| fuel never runs out, a .stuck is genuine; OCCURS CHARACTERIZED — occurs is CONSERVATIVE: occurs_allVar_unifiable shows unifyRow α (β|α|γ)=occurs but β,γ↦ε unifies (algorithm incomplete on all-var occurrences), genuine occurs needs a field — occurs_field_no_unifier proven; CLASH ALGORITHM-LEVEL done — unifyRow_clash_no_unifier; MGU-ON-SUCCESS done — unifyRow_success_complete (any unifier satisfies the emitted σ,eqs; with soundness the algorithm computes a most general unifier), via the full FORWARD-REFLECTION layer (strip/match/ground_reflect_fwd + field_cancel_left/right) + allVarsEmpty_complete/cat_empty_split/solveVar_complete; STUCK⟹NO-MGU — reusable principle done: instanceOf_fieldCount_mono (θ'⊑θ ⟹ count_l(θ x)≤count_l(θ' x), since subst never deletes a field and ≈ preserves counts — an mgu is pointwise count-minimal) + no_mgu_of_witness_shrinks (every unifier strictly undercut somewhere ⟹ no mgu); wand_no_mgu_count re-proves Wand's (β|α)≐ᵣ(l:𝓫) through it by counting (the l-field's host is beaten 1→0 by the opposite-placement witness). Generalized to n vars: vars_vs_field_no_mgu — (v₁|…|vₙ)≐ᵣ(l:𝓫) for any nodup vs, |vs|≥2, has no mgu (wand = n=2). BOTH canonical stuck shapes done: two_sided_no_mgu — (α|l:𝓫)≐ᵣ(l:𝓫|β) has no mgu, via RIGIDITY (instanceOf_fieldCount_eq_of_varFree: a var-free component of θ is rigid under any instance); the ε,ε witness pins θα to count 0, the unifier eq forces θα var-free, so the doubling witness that needs count 1 there can't factor. Lift infrastructure: HasMgu abstraction + hasMgu_congr/hasMgu_rowEquiv (no-mgu depends only on the unifier set, is a ≈-invariant) + stripL/stripR_hasMgu_iff (strip moves preserve the unifier set, so the STRIP arms of the fuel induction are discharge-ready; demo wand_under_strip_no_mgu). ALL THREE base techniques now done: count-shrink (vars_vs_field_no_mgu), rigidity (two_sided_no_mgu), and non-commutativity (allvar_swap_no_mgu — (α|β)≐ᵣ(β|α), via witnesses forcing θα,θβ field-free + the combinatorial append_comm_subset: A++B=B++A ∧ B≠[] ⟹ vars(A)⊆vars(B) by first-occurrence index). MATCH/GROUND congruence now done: HasMguP (mgu-status of an arbitrary unifier PREDICATE — InstanceOf never mentions the rows) + hasMguP_congr (axiom-free), with matchL/matchR/groundMatch_hasMgu_iff transporting HasMgu (ofSpine s₁)(ofSpine s₂) to HasMguP (λθ. θτ≈ₜθτ' ∧ θ⊨ofSpine t₁≐ᵣofSpine t₂), the eq-constrained residual (all propext/Quot.sound-clean). CAVEAT: unlike strip these SHRINK the unifier set (intersect with eq-satisfiers), so they transport mgu-status but a stuck residual does not by itself kill the original — that additionally needs the base-technique witnesses to satisfy the accumulated eq (the genuine augmented-witness content, now expressible via HasMguP). Remaining for the full lift: assembling the general base arm from the three techniques (+ the all-var argument generalized beyond the swap) and re-running those base witnesses under the accumulated equations (carry them as the HasMguP predicate through the fuel induction); type-level ≐ still open)
- [ ] FC-Labels
- [ ] Patterns
- [?] Occurrence Typing
- [?] Recursive types
- [?] With
- [?] Inherit

## Symbols
- ↓: Row-lookup relation, three-way result r := (τ | ⊥ | ?)
- ★: Definite uncertainty, no elimination
- ⊑: Precision relation for ★
  - Every other type is below ★
- ≈: Row-equivalence relation
- ≤: Instantiation relation for type-schemes
- ≐: Type unification
- ≐ᵣ: Row unification
- ⊴: "At least as general" (covering order on schemes)

## Properties
- ↓: deterministic, monotone, total (under RowWF)
- ⊑: reflexive, transitive (limmited)
- ≈: refl, symm, trans, congruence under |; adjacent distinct labels commute, ε is a unit
- ρ: rows mod ≈ form a trace monoid (partially-commutative, cancellative) — MECHANIZED (rowEquiv_iff_char + cancel_var_left/right, algorithmic.lean)


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
- Progress & Preservation:
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

- L2 qualified schemes:
  - *plain embedding*: Q = ∅ degenerates ≥\_Γ to the Γ-independent Scheme.Inst (inst_toQ)
  - *discharge determinism*: two discharges substituting the row the same way pin δ to the same type (Discharge.det, via lookup_det)
  - *definite-stability*: found/⊥ discharges survive row-extension of Γ — "a resolved stump never re-checks", wake-up lists carry no resolved stumps (Discharge.mono_of_definite, via lookup_mono); the ?-case is deliberately unstable, wake-up exists to improve it
  - *worked-example table*: any lookup verdict Γ ⊢ ρ.l ↓ r yields the instance {ρ} → collapse(r) of selQ = ∀βδ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ (selQ_inst_of_lookup)
  - *instance-closedness restored*: every ≥\_Γ-instance of selQ is a declarative typing of λx. x.l (selQ_instance_closed), and the mixed instance {ε} → {ε} that broke every plain scheme is blocked by discharge (selQ_no_mixed). Bookend: qualified_principal_scheme states the exact shape whose plain-scheme version no_plain_principal_scheme refutes
  
- ≈-characterization (algorithmic.lean — the trace-monoid presentation behind ≐ᵣ):
  - *normal form*: rows flatten to spines (toSpine/ofSpine); refold ρ ≈ ofSpine ρ.toSpine consumes exactly assoc + units (toSpine_equiv)
  - *invariants*: var sequence (sVarSeq) and per-label projection (sProj — (segment index, type) per l-field in row order); both compositional under ++ (sVarSeq_append, sProj_append)
  - *the characterization*: ρ₁ ≈ ρ₂ iff same var sequence ∧ all l-projections pointwise-≈ at equal segment indices (rowEquiv_iff_char; soundness RowEquiv.char by rule induction, completeness char_complete by spine walk + extraction spine_extract — first l-field at index 0 bubbles to the front through distinct-label comm-swaps only)
  - *end-var cancellativity*: shared leading/trailing vars cancel — complete, not just sound (cancel_var_left / cancel_var_right); this is the load-bearing fact for U-var-refl, replacing P&X's shared-tail side condition
  - *full cancellativity*: ANY shared prefix/suffix row cancels (cancel_cat_left / cancel_cat_right) — the trace-monoid theorem proper, the Levi-lemma ingredient for the trichotomy's stuck ⟹ no-unique-mgu direction
  - *ground rows*: SpineVarFree ↔ empty var sequence (spineVarFree_iff_varSeq_nil); on ground rows ≈ is decided by the projections alone (ground_char — ≐ᵣ's ground-completeness workhorse)
  - *≐ᵣ regressions mechanized*: the shared-tail pitfall (l₁:𝓫 | α) ≐ᵣ (l₂:𝓫 | α) has NO unifier (shared_tail_no_unifier), and the LUtail example (l:𝓫) ≐ᵣ (α | l:𝓫) has unifiers exactly θα ≈ ε (lutail_unifier_iff) — the mgu P&X's LUtail misses
  - *Wand's ambiguity / trichotomy (c)*: (β | α) ≐ᵣ (l:𝓫) is solvable (wand_unifiable) but has NO mgu (wand_no_mgu, over the instance order InstanceOf) — the l-field can come from either side and a ground singleton can never substitute into ε, so the two witnesses are incomparable. U-stuck's failure class is genuine

- L2 typing relation (algorithmic.lean — QTyped over qualified contexts QCtx):
  - *the system*: qVar instantiates via ≥\_Γ (instantiation-with-discharge, reading QCtx's row-solutions), qLet's instance-closed premise quantifies over DISCHARGED instances only; all other rules mirror Typed verbatim
  - *embedding*: every declarative typing is an L2 typing (Typed.toQ / TypedBody.toQ — plain schemes embed with Q = ∅, discharge vacuous, lookups transport on the nose). L2 EXTENDS the declarative system
  - *the two-use program*: `let f = (x: x.l) in { a = f {l = c} | b = f {} } : {a: 𝓫_c | b: ★}` is QTyped (qtyped_two_use) — one binding used at the found- AND the ⊥-instance simultaneously, the combination no_plain_principal_scheme proves impossible for any plain scheme. The instance-closed premise is discharged by selQ_instance_closed lifted through the embedding
  - IMPORTANT consequence (not yet mechanized): the extension is STRICT — the declarative system cannot type the two-use program at this precise type, so "algorithm sound against minimal.typ's declarative system" must be stated with L1-finalization (⟦S″⟧ maps δ to ★), while the refined typings live only in the L2 system. The thesis should present L2 as the primary declarative system
- ≐ᵣ, the algorithm (algorithmic.lean — unifyRow/unifySpine, executable):
  - *forced steps only*: end-var stripping (U-var-refl, both ends), occurs-checked whole-var solving (U-var-solve), two-ended window field-matching (U-field), U-ε-var, U-ground counting, global projection clash (U-clash), stuck otherwise. No LUtail — field demands park as stumps, never flow through ≐ᵣ. Type equations are EMITTED (τ ≐ τ' pairs), not solved
  - *four-way result*: success (row-var solutions + residual type equations) / clash (no unifier) / occurs (recursive row) / stuck (no unique mgu)
  - *kernel-checked regressions* (rfl runs the algorithm): shared-tail → clash, LUtail → success α ≔ ε, Wand → stuck, ground-collapse → α,β ≔ ε, global-clash example → clash, interior occurs → occurs, two-sided ambiguity → stuck
  - *mechanization findings*: (1) U-ground must be an EXPLICIT rule — the window rules alone do not cover worked example 2's "var count must collapse"; the counting argument (ground side ⟹ vars label-free ⟹ positional pairing) is a separate forced move. (2) End-var cancellation SUBSUMES end-aligned occurs-failures into definite clashes (α ≐ᵣ (l:𝓫 | α) cancels to ε ≐ᵣ (l:𝓫) → clash, strictly stronger than occurs)
  - *field-count invariant* (algorithmic.lean): ≈ preserves l-field count (rowEquiv_fieldCount_eq, via sProj_length_eq_sFieldCount + ProjEquiv.length_eq); substitution only increases count (sFieldCount_applySubst_le); var-free rows' counts are fixed (sFieldCount_applySubst_varFree). spine roundtrip: ofSpine_toSpine.
  - *projClash soundness* (algorithmic.lean): projClash s₁ s₂ → no unifier (projClash_no_unifier) — the U-clash direction of the trichotomy. Proof: ground-side count fixed, non-ground side ≥ original count, ≈ forces equality, omega closes. All four theorems depend only on propext + Quot.sound (no sorry).
  - *SUCCESS SOUNDNESS — DONE, axiom-clean* (algorithmic.lean, "SUCCESS SOUNDNESS" section): `unifyRow_success_sound` — if `unifyRow ρ₁ ρ₂ = success σ eqs` and θ meets the row-var solution list σ (`SolSat`, each binding α≔ρ up to ≈) and the residual type equations eqs (`EqsSat`, each pair ≈-equal), then `Unifies θ ρ₁ ρ₂`. propext/Quot.sound only, no sorry. Built from:
    - MOVE-REFLECTION lemmas (each "θ unifies the residual ofSpine tᵢ ⟹ θ unified the original ofSpine sᵢ"): `stripL_reflect`/`stripR_reflect` (shared end-var drops out; `stripL_inv`/`stripR_inv` + `ofSpine_append`), `solveVar_reflect` (α≔ofSpine s₂ via unitR), `matchL_reflect` (field bubbled to front by `windowExtract_equiv`, comm past distinct-label fields only), `matchR_reflect` (trailing mirror via `revRow` + `RowEquiv.revRow` + `ofSpine_reverse_equiv` + `windowExtract_reverse_equiv`), `allVarsEmpty_sound` (empty side ⟹ leftover vars ≔ ε). s₂-s₁ variants reuse these by RowEquiv symmetry; `URes.addEq_success` inverts addEq for the field moves.
    - U-GROUND, the former hard gap, now CLOSED: `field_comm_lfree` (a field ≈-commutes past a var-free AND l-free row — both hypotheses essential; a field does NOT commute past a var, shadowing) drives `removeField_equiv_of` (pulls the matched l-field to the front under θ, given every spine var is var-free+l-free under θ). The side condition is discharged by the COUNTING: `allVars_varFree_of` + `allVars_lfree_of` read hrec back — the ground side (s₂) is var-free so ≈ pins the l-count of `(ofSpine t₁)θ` to `(ofSpine t₂)θ`, and `sFieldCount l s₁ = sFieldCount l s₂` (groundMatch condition) forces θ to add zero l-fields across s₁'s vars. Support: `removeField_sVarSeq`, `removeField_sFieldCount`, `varSeq_applySubst_nil`, `groundMatch_inv`.
    - `unifySpineF_success_sound`: induction on the fuel, discharging every match arm (stripL/R, solveVar×2, matchL×2, matchR×2, groundMatch×2, base allVarsEmpty) by the reflection lemma; lifted to `unifyRow` via `toSpine_equiv` congruence.
  - *open (rest of trichotomy)*: mgu-on-success, occurs ⟹ no unifier and stuck ⟹ no unique mgu (all need the same control-flow inversion — occurs/stuck are only correct AFTER end-stripping has run, so not local to solveVar; wand_no_mgu is the worked stuck instance), fuel-sufficiency; the type-level driver ≐


