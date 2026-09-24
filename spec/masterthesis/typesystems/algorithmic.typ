== L2 Calculus
> Functions, scoped records, record concat, row-vars, let-poly, qualified schemes, parked lookups

l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x | (x: e) | e₁e₂ | (e₁ ‖ e₂) | e.l | { ξ } | let x = e₁ in e₂
ξ := ε | l = e | (ξ₁ | ξ₂)

τ := α | 𝓫 | ★ | τ -> τ | { ρ }
ρ := ε | α | l: τ | (ρ₁ | ρ₂)
κ := Type | Row

// the L2-algorithmic part
q := ⟨ρ.l ↓ δ⟩
Q := ∅ | q, Q
σ := ∀(ᾱ: κ̄). Q ⇒ τ
Γ := • | Γ·(x: σ) | Γ·(α: κ) | Γ·(α = ρ)


== Sorts
- κ classifies *variables*, nothing else: τ and ρ are already disjoint
  syntactic categories, so every closed phrase reads its sort off the grammar.
  α at a type position and α at a row position are not the same variable
- Only T-λ-I and T-let carry a sorting premise: they are the only rules whose
  conclusion mentions a type resp. a scheme not already determined by the
  premises
- Substitutions respect sorts: (Γ ⊢ θ: ᾱ:κ̄) means θ is the identity outside ᾱ
  and Γ ⊢ θα: κ for every (α: κ) ∈ ᾱ:κ̄
- A row-solution (α = ρ) ∈ Γ binds α at Row


α: Type ∈ Γ
------------ S-var
Γ ⊢ α: Type


------------ S-base
Γ ⊢ 𝓫: Type


---------- S-★
Γ ⊢ ★: Type


Γ ⊢ τ₁: Type   Γ ⊢ τ₂: Type
----------------------------- S-λ
Γ ⊢ τ₁ -> τ₂: Type


Γ ⊢ ρ: Row
-------------- S-rcd
Γ ⊢ {ρ}: Type


α: Row ∈ Γ
----------- S-ρ-var
Γ ⊢ α: Row


---------- S-ε
Γ ⊢ ε: Row


Γ ⊢ τ: Type
---------------- S-field
Γ ⊢ (l: τ): Row


Γ ⊢ ρ₁: Row   Γ ⊢ ρ₂: Row
--------------------------- S-conc
Γ ⊢ (ρ₁ | ρ₂): Row


Γ ⊢ ρ: Row   δ: Type ∈ Γ
-------------------------- S-stump
Γ ⊢ ⟨ρ.l ↓ δ⟩ ok


(∀ q ∈ Q. Γ·(ᾱ: κ̄) ⊢ q ok)   Γ·(ᾱ: κ̄) ⊢ τ: Type
------------------------------------------------- S-scheme
Γ ⊢ (∀(ᾱ: κ̄). Q ⇒ τ) ok


== Stumps
- A stump ⟨ρ.l ↓ δ⟩ is a parked selection: the lookup of l in ρ blocked on a
  row-var, with δ the *result variable* standing for whatever the lookup will
  turn out to be
- δ ∈ ᾱ, drawn from the quantifier like every other variable: the constraint
  pins δ's image at *instantiation* time instead of freezing it at
  generalization time (that is L1, and it loses the found-instances)
- Plain schemes embed as Q = ∅; the discharge premise is then vacuous and
  ≥\_Γ degenerates to the Γ-independent σ ≥ τ of the minimal calculus
- ∀(β: Row)(δ: Type). ⟨β.l ↓ δ⟩ ⇒ {β} → δ is the principal scheme of (x: x.l);
  no plain ∀ᾱ. τ scheme covers both its found- and its ⊥-instances


== Declarative

----------- T-cons
Γ ⊢ c: 𝓫_c


x: σ ∈ Γ   σ ≥\_Γ τ
-------------------- T-var
Γ ⊢ x: τ


Γ ⊢ e: τ₁   τ₁ ≈ τ₂
--------------------- T-eq
Γ ⊢ e: τ₂


Γ ⊢ τ₁: Type   Γ·(x: τ₁) ⊢ e: τ₂
---------------------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ σ ok   (∀ τ₁. σ ≥\_Γ τ₁ ⟹ Γ ⊢ e₁: τ₁)   (∃ τ₁. σ ≥\_Γ τ₁)   Γ·(x: σ) ⊢ e₂: τ₂
------------------------------------------------------------------------------------ T-let
Γ ⊢ let x = e₁ in e₂: τ₂
// Instance-closed over *discharged* instances. The inhabitation premise is not
// bureaucracy: with Q ≠ ∅ a scheme can have NO Γ-instance, and the
// instance-closed premise then says nothing about e₁ — `let x = (3 4) in 5`
// would type while being stuck and progress would be false. Plain schemes
// satisfy it by I-inst with θ = id; the solver satisfies it by construction,
// a parked stump discharging at ★ if nothing better.


Γ ⊢ e₁: { ρ₁ }  Γ ⊢ e₂: { ρ₂ }
-------------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ τ
--------------------------- T-sel
Γ ⊢ e.l: τ


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ?
-------------------------- T-sel-★
Γ ⊢ e.l: ★


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ⊥
-------------------------- T-sel-⊥
Γ ⊢ e.l: ★


Γ ⊢ e: τ
--------- T-★-intro
Γ ⊢ e: ★


Γ ⊢ ξ: ρ
----------------- T-rec
Γ ⊢ { ξ }: { ρ }


--------- T-ξ-empty
Γ ⊢ ε: ε


Γ ⊢ e: τ
-------------------- T-ξ-field
Γ ⊢ (l = e): (l: τ)


Γ ⊢ ξ₁: ρ₁   Γ ⊢ ξ₂: ρ₂
-------------------------- T-ξ-conc
Γ ⊢ (ξ₁ | ξ₂): (ρ₁ | ρ₂)


== Instantiation
- (σ ≥\_Γ τ) replaces (σ ≥ τ) at T-var: instantiate all quantifiers at once via
  a *sort-respecting* θ over ᾱ, then discharge every q ∈ Q
- Γ-relative, unlike the minimal calculus: discharge reads Γ's row-solutions.
  That is the price of cross-instantiation refinement
- *No tail check needed* (unlike λ⟨⟩): By monotonicity of ↓, instantiating a
  row-var can never invalidate a definite lookup result — every position it
  could shadow was already ?-poisoned

Γ ⊢ θ: ᾱ:κ̄   (∀ q ∈ Q. Γ; θ ⊢ q)
---------------------------------- I-inst
(∀(ᾱ: κ̄). Q ⇒ τ) ≥\_Γ θτ


== Discharge
- (Γ; θ ⊢ q) replays the parked lookup under θ and pins δ to the verdict
- The three arms are exactly T-sel / T-sel-⊥ / T-sel-★, replayed per instance
- Determinism, monotonicity and totality of ↓ are what keep discharge
  well-behaved

Γ ⊢ (θρ).l ↓ τ_r   θδ = τ_r
----------------------------- D-hit
Γ; θ ⊢ ⟨ρ.l ↓ δ⟩


Γ ⊢ (θρ).l ↓ ⊥   θδ = ★
------------------------- D-⊥
Γ; θ ⊢ ⟨ρ.l ↓ δ⟩


Γ ⊢ (θρ).l ↓ ?   θδ = ★
------------------------- D-?
Γ; θ ⊢ ⟨ρ.l ↓ δ⟩
// Declaratively a still-unknown lookup stays blurred; algorithmically this
// case RE-PARKS instead, and only finalization commits ★.


== Row-Lookup
- (Γ ⊢ ρ.l ↓ r) with (r := τ | ⊥ | ?)
- This statement recursively searches rows for a label l
- Γ's *row-solutions* (α = ρ) are a partial map from row-vars to the rows they
  stand for. L-α consults it; L-α-free fires when α is unsolved. (α = ρ is a
  row binding, NOT a record type — a row-var ranges over rows ρ, not over
  record types {ρ}.)
- Absent means the label provably does not exist in ρ; T-sel-⊥ still types the
  selection at ★ (soft typing: the checker flags it, the ↯-disjunct of progress
  catches it at runtime)
- Unknown means an unconstrained row-var could contain l, so no definite type
  can be derived


------------ L-ε
Γ ⊢ ε.l ↓ ⊥


l₁ = l₂
-------------------- L-hit
Γ ⊢ (l₁: τ).l₂ ↓ τ


l₁ ≠ l₂
-------------------- L-miss
Γ ⊢ (l₁: τ).l₂ ↓ ⊥


α = ρ ∈ Γ   Γ ⊢ ρ.l ↓ r
------------------------ L-α
Γ ⊢ α.l ↓ r


α unsolved in Γ
--------------- L-α-free
Γ ⊢ α.l ↓ ?


Γ ⊢ ρ₁.l ↓ τ
-------------------- L-conc-hit
Γ ⊢ (ρ₁ | ρ₂).l ↓ τ


Γ ⊢ ρ₁.l ↓ ⊥   Γ ⊢ ρ₂.l ↓ r
----------------------------- L-conc-skip
Γ ⊢ (ρ₁ | ρ₂).l ↓ r


Γ ⊢ ρ₁.l ↓ ?
-------------------- L-conc-★
Γ ⊢ (ρ₁ | ρ₂).l ↓ ?


== Row-Equivalence
- ρ₁ ≈ ρ₂, lifted to types congruently (τ₁ ≈ τ₂)
- Rows are equal up to reassociation, ε-units and swapping distinct labels

------ ≈-refl
ρ ≈ ρ


ρ₂ ≈ ρ₁
-------- ≈-symm
ρ₁ ≈ ρ₂


ρ₁ ≈ ρ₂   ρ₂ ≈ ρ₃
------------------ ≈-trans
ρ₁ ≈ ρ₃


τ₁ ≈ τ₂
------------------ ≈-ext
(l: τ₁) ≈ (l: τ₂)


ρ₁ ≈ ρ₁′   ρ₂ ≈ ρ₂′
------------------------ ≈-conc
(ρ₁ | ρ₂) ≈ (ρ₁′ | ρ₂′)


------------------------------------- ≈-assoc
((ρ₁ | ρ₂) | ρ₃) ≈ (ρ₁ | (ρ₂ | ρ₃))


----------- ≈-unit-l
(ε | ρ) ≈ ρ


----------- ≈-unit-r
(ρ | ε) ≈ ρ


l₁ ≠ l₂
----------------------------------------- ≈-comm
(l₁: τ₁ | l₂: τ₂) ≈ (l₂: τ₂ | l₁: τ₁)


== Spines
- Rows normalize mod ≈-assoc and ≈-units to *spines*; ⌈ρ⌉ is the spine of ρ
- A spine factors into var-free *segments* separated by vars. ≈-comm swaps
  distinct labels inside a segment only, equal labels keep their order
  (scopedness), and nothing crosses a var
- ≈-characterization: ρ₁ ≈ ρ₂ iff same var sequence and, per label, the
  (segment index, type) lists agree pointwise
- A trace monoid, hence left- and right-cancellative: stripping a shared end
  var is sound AND complete. This replaces P&X's shared-tail side condition

a := l: τ | α
s := ⟨⟩ | a·s

|s|\_l          number of l-fields in s
vars(s)        the var sequence of s
win\_l(s)       remove the first l-field of the LEADING var-free window
win^R\_l(s)     remove the last l-field of the TRAILING var-free window
rem\_l(s)       remove the first l-field ANYWHERE (vars skipped)


== Unification
- Two mutual judgements, one per sort: (τ₁ ≐ τ₂ ⇝ v) at Type and
  (s₁ ≐ᵣ s₂ ⇝ v) at Row. Sorting makes the split a consequence rather than a
  coincidence, and a solution is sort-respecting by construction
- Fuel is explicit and shared: both passes spend a unit per cross-call, so the
  block is structurally recursive and `no-fuel` is its own verdict — every
  verdict actually reached is budget-independent
- A success carries the name supply it stopped at. Threaded, not re-derived
  per call: a move that drops a field drops its type's variables, and a locally
  recomputed bound can fall below a name still in scope
- Type equations emitted by the row pass are solved ON THE SPOT and their
  solution applied to the residual before recursing — never deferred, which is
  what makes `stuck` mean something

v := θ | clash | occurs | stuck | no-fuel


=== Type unification  τ₁ ≐ τ₂ ⇝ v

--------- U-refl
α ≐ α ⇝ ∅


τ ≠ α   α ∉ ftv_Ty(τ)
-------------------- U-bind
α ≐ τ ⇝ [α ≔ τ]


τ ≠ α   α ∈ ftv_Ty(τ)
-------------------- U-occurs
α ≐ τ ⇝ occurs
// SORTED: ftv_Ty collects the occurrences at TYPE positions only, so
// α ≐ {… α …} with a row-var α is a BINDING, not a failure — the binding never
// reaches that occurrence. Mechanized as `Ty.tyVars` (RowUnify/Defs.lean); the
// rejections it keeps are genuine no-unifiers (`bindTy_occurs_no_unifier`,
// by constructor depth).


----------- U-★
★ ≐ ★ ⇝ ∅
// ★ is RIGID: it unifies with itself and clashes with everything else.


𝓫 = 𝓫′
------------ U-base
𝓫 ≐ 𝓫′ ⇝ ∅


𝓫 ≠ 𝓫′
---------------- U-base-clash
𝓫 ≐ 𝓫′ ⇝ clash


⌈ρ₁⌉ ≐ᵣ ⌈ρ₂⌉ ⇝ θ
------------------ U-rcd
{ρ₁} ≐ {ρ₂} ⇝ θ


τ₁ ≐ τ₁′ ⇝ θ₁   θ₁τ₂ ≐ θ₁τ₂′ ⇝ θ₂
----------------------------------- U-fn
(τ₁ -> τ₂) ≐ (τ₁′ -> τ₂′) ⇝ θ₂ ∘ θ₁


head(τ₁) ≠ head(τ₂)
--------------------- U-clash
τ₁ ≐ τ₂ ⇝ clash


=== Row unification  s₁ ≐ᵣ s₂ ⇝ v
- Tried in this order, first trigger wins: U-ε-var, U-ε-clash (both BEFORE the
  fuel guard — an exhausted side needs no budget), U-var-refl-L, U-var-refl-R,
  U-var-solve, U-var-occurs, U-field-L, U-field-R, U-ground, U-expand,
  U-clash, U-stuck
- U-var-solve, U-var-occurs, U-field-L, U-field-R, U-ground and U-expand are
  also tried with the two sides exchanged; U-ε-var, U-ε-clash and U-clash are symmetric
- Every move except U-expand is FORCED (solution-set preserving). U-expand
  invents structure and fires only when the host variable is unique, which is
  P&X's LUtail with the guess removed
- U-clash sits at the BOTTOM, not the top: a clash is diagnosed only once every
  forced move is dead. Cancellativity turns α ≐ᵣ (l: 𝓫 | α) into a clash rather
  than an occurs-failure, which is strictly better information
- No rule pushes a field demand INTO a var: lookups park as stumps, so the
  algorithm never guesses a field into a variable outside U-expand

s field-free   vars(s) = β̄
--------------------------- U-ε-var
⟨⟩ ≐ᵣ s ⇝ [β̄ ≔ ε]


(l: τ) ∈ s
-------------------- U-ε-clash
⟨⟩ ≐ᵣ s ⇝ clash


t₁ ≐ᵣ t₂ ⇝ θ
------------------------ U-var-refl-L
α·t₁ ≐ᵣ α·t₂ ⇝ θ


t₁ ≐ᵣ t₂ ⇝ θ
------------------------ U-var-refl-R
t₁·α ≐ᵣ t₂·α ⇝ θ


α ∉ rowvars(s)
---------------------- U-var-solve
α ≐ᵣ s ⇝ [α ≔ s]


α ∈ vars(s)   s field-free   k = |α|_s
------------------------------------------------ U-var-collapse
α ≐ᵣ s ⇝ [γ ≔ ε : γ ∈ vars(s), γ ≠ α or k ≥ 2]
// NOT a failure. The ≈-characterization reads |m(θα)| = k·|m(θα)| + Σ_{γ≠α},
// at the var-sequence length and at every field count alike, so at k = 1 every
// OTHER spine variable is forced to ε and α stays free (ε | α | ε ≈ α), and at
// k ≥ 2 α is forced too. Both are unique, hence mgus (`allvar_occurs_mgu`).


α ∈ rowvars(s)   ¬(α ∈ vars(s) ∧ s field-free)
---------------------------------------------- U-var-occurs
α ≐ᵣ s ⇝ occurs
// What is left after U-var-collapse: α under a record constructor (depth grows
// — `deep_occurs_no_unifier`) or on the spine with a field present (counting —
// `occurs_field_no_unifier`). Both are genuine no-unifiers, so the case
// analysis CLOSES: `solveVarM_occurs_no_unifier`. rowvars(s) is `allRowVars` —
// the row variables at any depth, the row half of the sorted occurs check.


win\_l(s₂) = (τ′, t₂)   τ ≐ τ′ ⇝ θ   θt₁ ≐ᵣ θt₂ ⇝ θ′
----------------------------------------------------- U-field-L
(l: τ)·t₁ ≐ᵣ s₂ ⇝ θ′ ∘ θ


win^R\_l(s₂) = (τ′, t₂)   τ ≐ τ′ ⇝ θ   θt₁ ≐ᵣ θt₂ ⇝ θ′
------------------------------------------------------- U-field-R
t₁·(l: τ) ≐ᵣ s₂ ⇝ θ′ ∘ θ


vars(s₂) = ⟨⟩   |s₁|\_l = |s₂|\_l > 0   rem\_l(sᵢ) = (τᵢ, tᵢ)   τ₁ ≐ τ₂ ⇝ θ   θt₁ ≐ᵣ θt₂ ⇝ θ′
----------------------------------------------------------------------------------------------- U-ground
s₁ ≐ᵣ s₂ ⇝ θ′ ∘ θ
// Counting rules the other side's vars out at l, so the pairing is positional.
// Not derivable from the window rules — the mechanization surfaced the gap.


vars(s₂) = β   |s₂|\_l = 0   δ, β′ fresh   t₁ ≐ᵣ s₂[β′/β] ⇝ θ
-------------------------------------------------------------- U-expand
(l: τ)·t₁ ≐ᵣ s₂ ⇝ θ ∘ [δ ≔ τ, β ≔ (l: δ | β′)]
// δ is fresh, so τ ≐ δ has the single solution δ ≔ τ and needs no cross-call —
// the only field rule that stays inside ≐ᵣ. Wired at the LEFT end only, in
// both argument orders; expandR exists but the driver never calls it.


|s₁|\_l > |s₂|\_l   vars(s₂) = ⟨⟩
----------------------------------- U-clash
s₁ ≐ᵣ s₂ ⇝ clash


no rule above applies
----------------------- U-stuck
s₁ ≐ᵣ s₂ ⇝ stuck
// U-expand refused for one of exactly two reasons (uniqueHost): ≥ 2 candidate
// hosts (Wand's shape), or l already occurs on the other side but BEHIND a
// var. No forced move remains — usually, but not always, genuine ambiguity.


== Solver State
- MECHANIZED as of 2026-09-18 (lean/Infer.lean): the state, the A-rules, wake-up,
  saturation, finalization and the entry judgement below are all relations in
  Lean, and `selEx_infers` / `selEx_runs` derive (x: x.l) through them end to end.
  What is still DESIGN rather than theorem: that the algorithm TERMINATES, that it
  is deterministic up to renaming, and soundness itself (`InferSound` / `RunSound`
  are named statements, not proofs)
- θ is sort-respecting, hence really two maps — one per sort. ⟦S⟧τ applies S's
  substitution as a closure
- Δ indexes each parked stump by its *blocker*: the row-var whose L-α-free
  produced the ?. The blocker is a wake-up index, NOT part of the constraint —
  the declarative stump ⟨ρ.l ↓ δ⟩ carries none
- W collects definite-absence flags and ★-degradations, named by the site that
  produced them — the label for A-sel-⊥, K-⊥, F-★ and A-sel-deg, the application
  itself for A-app-deg. It never affects typing, only diagnostics
- The declarative system READS solutions via L-α; the algorithm WRITES them via
  unification. θ is only ever refined

S := (θ, Δ, W)
Δ := ∅ | ⟨α ▷ ρ.l ↓ δ⟩, Δ

S ⊎ q          park a stump
S ∖ Δ′         drop a set of stumps
fresh α: κ     draw a name at sort κ from the threaded supply

- *Quiescence*, the state invariant: every stump in Δ is genuinely blocked on the
  blocker it records, `⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ? on α` for each `⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ`. A
  blocker is then never a SOLVED variable, which is what makes the annotation
  mean anything
- It is not automatic, and the mechanization found that out: any rule that writes
  a solution can solve some other stump's blocker, and only A-var ran wake-up. So
  every equation in the A-rules below is *solve-then-saturate*

S ⊢ τ ≐ τ′ ⇝! S′    solve, then re-run wake-up on what the solution staled
S ⊢ Δ ↝! S′         saturation itself: step on stale stumps until quiescent
S ⊢ Q ↝\*! S′       A-var's closure, then saturation


== Inference
- (Γ; S ⊢ e ⇒ τ; S′) is syntax-directed, one rule per term former. There are no
  counterparts to T-eq and T-★-intro — inversion-mod-≈ and re-blurring account
  for those on the declarative side
- Failure policy: a `clash` is a hard error (it is PROVED to mean no unifier
  exists, so rejecting is soundness, not choice). So is `occurs` where the guard
  is local — that is now proved at both sorts. `stuck` is conservative and may
  NOT reject; it degrades to ★ with a W-flag
- The degradation is a RULE, not a side remark: A-app-deg and A-sel-deg are the
  two sites that emit a type equation, and without them the judgement is simply
  undefined on a conservative verdict. A clash has no rule at ALL — that absence
  IS the hard error. `no-fuel` has none either, for the opposite reason: it is a
  verdict about the budget, not about the problem, so the answer is a bigger one
- Both degradation rules keep the state they had reached and add the flag. They
  do NOT emit the equation's would-be solution: a verdict that may not reject
  may not commit either

------------------- A-cons
Γ; S ⊢ c ⇒ 𝓫_c; S


x: ∀(ᾱ: κ̄). Q ⇒ τ ∈ Γ   fresh β̄: κ̄   S ⊢ Q[β̄/ᾱ] ↝\*! S′
-------------------------------------------------------- A-var
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S′
// A-var IS I-inst: the instantiated constraints are submitted to wake-up, which
// resolves the ones the current θ already decides and parks the rest. Parking
// is the algorithmic image of D-? — only finalization commits ★. The blockers
// of Q[β̄/ᾱ] are not given here; ↝\* computes each one, by K-park.


fresh α: Type   Γ·(x: α); S ⊢ e ⇒ τ; S′
----------------------------------------- A-lam
Γ; S ⊢ (x: e) ⇒ α -> τ; S′


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β: Type   S₂ ⊢ τ₁ ≐ (τ₂ -> β) ⇝! S₃
------------------------------------------------------------------------------------- A-app
Γ; S ⊢ e₁e₂ ⇒ β; S₃


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β: Type   S₂ ⊢ τ₁ ≐ (τ₂ -> β) ⇝ v   v ∈ {stuck, occurs}
--------------------------------------------------------------------------------------------------------- A-app-deg
Γ; S ⊢ e₁e₂ ⇒ ★; S₂ +W
// The arrow equation gave up, so the application blurs. β is drawn and then
// abandoned — nothing is written for it, since the verdict decides nothing. The
// W-entry is named by the application site, the one selection-free site that
// raises a flag.


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂: Row   S₂ ⊢ τ₁ ≐ {ρ₁} ⇝! S₃   S₃ ⊢ τ₂ ≐ {ρ₂} ⇝! S₄
--------------------------------------------------------------------------------------------------------- A-conc
Γ; S ⊢ e₁ ‖ e₂ ⇒ { ρ₂ | ρ₁ }; S₄


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝! S₂   ⟦S₂⟧ ⊢ ρ.l ↓ τ′
----------------------------------------------------------------------- A-sel
Γ; S ⊢ e.l ⇒ τ′; S₂


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝! S₂   ⟦S₂⟧ ⊢ ρ.l ↓ ⊥
----------------------------------------------------------------------- A-sel-⊥
Γ; S ⊢ e.l ⇒ ★; S₂ +W


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝! S₂   ⟦S₂⟧ ⊢ ρ.l ↓ ? on α   fresh δ: Type
-------------------------------------------------------------------------------------------- A-sel-?
Γ; S ⊢ e.l ⇒ δ; S₂ ⊎ ⟨α ▷ ρ.l ↓ δ⟩
// NOT ★: returning ★ here would freeze the result and lose every later
// refinement — (x: x.l) would infer {β} -> ★ and no application could recover
// the field type. The stump-var δ keeps the position writable.


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝ v   v ∈ {stuck, occurs}
-------------------------------------------------------------------------- A-sel-deg
Γ; S ⊢ e.l ⇒ ★; S₁ +W
// The RECORD equation gave up, before any lookup happens — so this is not a
// third lookup verdict next to A-sel-⊥ and A-sel-?, it is the case where the
// scrutinee never became a row to look in. No stump is parked: a stump needs a
// blocker, and there is no ρ-solution to be blocked on. ★ here is final, unlike
// A-sel-?'s δ.


Γ; S ⊢ ξ ⇒ ρ; S′
-------------------------- A-rec
Γ; S ⊢ { ξ } ⇒ { ρ }; S′


------------------- A-ξ-empty
Γ; S ⊢ ε ⇒ ε; S


Γ; S ⊢ e ⇒ τ; S′
------------------------------ A-ξ-field
Γ; S ⊢ (l = e) ⇒ (l: τ); S′


Γ; S ⊢ ξ₁ ⇒ ρ₁; S₁   Γ; S₁ ⊢ ξ₂ ⇒ ρ₂; S₂
------------------------------------------- A-ξ-conc
Γ; S ⊢ (ξ₁ | ξ₂) ⇒ (ρ₁ | ρ₂); S₂


== Wake-up and Finalization
- (S ⊢ q ↝ S′) wakes one stump, (↝\* its list closure), (S ⊢ Δ ↝! S′) runs it to
  quiescence, (S ⊢ q ⇓ S′) finalizes one and (⇓\* its list closure). K-hit, K-⊥ and
  F-★ are D-hit, D-⊥ and D-? — the difference is WHEN: discharge fires once per
  instantiation, wake-up fires each time θ grows
- K-repark has no declarative counterpart: declaratively D-? commits to ★
  immediately, algorithmically the lookup has merely progressed to the next var
- ↝\* runs over a Δ-shaped list, so its elements are blocker-annotated. A-var
  submits its instantiated Q with the blockers LEFT FREE: each step of the
  closure determines one — K-hit and K-⊥ by resolving the constraint outright,
  K-park by the ? -witness
- K-park is the ↝\* rule that A-var needs and ↝ cannot state: a FRESHLY
  instantiated constraint arrives with no blocker yet, so there is no single-step
  wake-up to take. K-repark is the same move for a stump already in Δ — the
  difference is only whether an old entry is dropped first
- Wake-up fires when a solution α ≔ ρ is written, and only for stumps blocked
  on α
- *Monotone*: a stump resolved found/⊥ is final under every later θ, so resolved
  stumps never re-enter Δ and no fixpoint iteration is needed
- *Deterministic*, but NOT for the reason first written here. "Lookup is
  deterministic, so the final θ, W and τ do not depend on the wake-up order" was
  FALSE of the earlier rules: F-★ carried no premise about the lookup, so at a
  state whose stump had gone stale both K-hit and F-★ applied and committed δ to
  different types (`fStar_wake_star_disagree`, lean/InferSound.lean). What holds,
  and is proved, is the repaired form: at a quiescent state no wake-up step can
  commit anything (`Quiescent.wake_no_commit` — the only available step is a
  K-repark onto the same blocker), and wherever F-★ applies the same is true of
  that stump (`Finalize.wake_no_commit`). So finalization is the only progress
  left at the end of a run, which is what determinism needed to mean. Order
  independence for the WHOLE run is still design, not theorem
- *Saturation is a relation, not a function*: a saturating function owes a
  termination measure and K-repark has none — each repark moves the blocker to a
  new variable. "A quiescent state is always reachable" therefore joins the open
  list, and it is CONDITIONAL, not just unproved: a K-hit needs the lookup to be
  total (acyclic θ) and its equation can clash — the tension case below

⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ τ′   S ⊢ δ ≐ τ′ ⇝ S′
---------------------------------------- K-hit
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩


⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ⊥   S ⊢ δ ≐ ★ ⇝ S′
--------------------------------------------- K-⊥
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩) +W


⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ? on α′
------------------------------------------------------------ K-repark
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S ∖ ⟨α ▷ ρ.l ↓ δ⟩) ⊎ ⟨α′ ▷ ρ.l ↓ δ⟩


--------- K-nil
S ⊢ ∅ ↝\* S


S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S₁   S₁ ⊢ Δ′ ↝\* S′
----------------------------------------- K-cons
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ↝\* S′


⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ? on α   (S ⊎ ⟨α ▷ ρ.l ↓ δ⟩) ⊢ Δ′ ↝\* S′
----------------------------------------------------------- K-park
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ↝\* S′
// The blocker is DETERMINED, not supplied: the premise is what fixes α, so an
// instantiated stump cannot be parked on a variable that is not the one
// actually blocking its lookup. That is the state invariant A-var would
// otherwise be free to break.


S quiescent
-------------- K!-done
S ⊢ Δ ↝! S


⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ   ⟦S⟧ ⊬ (⟦S⟧ρ).l ↓ ? on α   S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S₁   S₁ ⊢ Δ₁ ↝! S′
-------------------------------------------------------------------------------------- K!-step
S ⊢ Δ ↝! S′
// A step only on a STALE stump — one whose recorded blocker no longer blocks its
// lookup, which is exactly what a solution write creates. ↝ then resolves it
// (K-hit/K-⊥) or moves the annotation (K-repark). ⇝! and ↝\*! are this composed
// after ≐ and ↝\* respectively.


⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ   ⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ? on α   S ⊢ δ ≐ ★ ⇝ S′
--------------------------------------------------------------- F-★
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ (S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩) +W


--------- F-nil
S ⊢ ∅ ⇓\* S


S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ S₁   S₁ ⊢ Δ′ ⇓\* S′
--------------------------------------- F-cons
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ⇓\* S′

// The algorithmic moment of T-sel-★. Runs at the end of inference and at every
// generalization boundary that does not carry the stump.
//
// THE `? on α` PREMISE IS NOT DECORATION. Without it F-★ commits δ to ★ over a
// lookup that LANDS, and the resulting state answers a declarative question
// nobody asked: Stump.Discharge offers ★ only under D-⊥ (K-⊥'s job) or D-?, so
// D-? is the arm F-★ implements and `?` is what it has to check. Every sibling
// states its verdict the same way — K-hit its τ′, K-⊥ its ⊥, K-repark its `? on
// α′`. Refuted without it: `finalize_star_no_discharge`, lean/InferSound.lean.
// It costs the algorithm nothing: at a quiescent state the premise holds of every
// parked stump, and every state a run reaches is quiescent
// (`Infer.quiescent` + `Finalize.of_quiescent`).

// TENSION CASE: if δ is already solved and wake-up finds a different τ′, the
// emitted δ ≐ τ′ CLASHES ⟹ hard error, not a degradation.
//
// THE SAME TENSION AT FINALIZATION, and it is the sharper one. A-sel-? returns δ
// so the position stays WRITABLE, and any USE of the selection's value writes to
// it: in (x: (y: (x.l) y)) the application emits δ ≐ (α_y -> β) and it SUCCEEDS.
// The promise is then spent on an arrow, F-★'s own δ ≐ ★ clashes (★ is rigid), and
// the run has no way to finish — a hard error by the same discipline as a clash,
// no rule applies (no_finalize_of_spent, lean/Infer.lean; the run that reaches it
// is spentEx_infers, and the state it reaches is quiescent and otherwise sound).
//
// This one is INCOMPLETENESS, not a justified rejection: that program IS typeable,
// at {(l: 𝓫 -> 𝓫)} -> 𝓫 -> 𝓫 (spentEx_declarative). No ★ is ever formed, so it is
// not the ★-elimination gap either. The algorithm commits x to {ρ} with ρ abstract
// and never guesses a concrete row, so its only possible answer is to CARRY
// ⟨ρ.l ↓ (α_y -> β)⟩ — which it cannot write, because a stump's result position
// holds a VARIABLE. Exits: leave it a hard error and record the incompleteness
// (what the rules do now); or let a stump's result be a TYPE, which touches
// Discharge, QScheme.WF, selQ and the principality theorems; or add a consistency
// relation τ ~ ★ beside ≐, which is a real extension. Generalization inherits it:
// A-let carries stumps into a scheme whose δ's must be among its binders, and a
// spent δ is not a binder.


== Entry
- (⊢ e ⇒ τ; S′) is the TOP LEVEL: infer from the empty state, then finalize what
  is still parked. Everything above is a judgement about a state; this is the one
  that takes a program and returns an answer, and it is what soundness of the
  ALGORITHM — as opposed to soundness of one rule — is stated about

∅; (id, ∅, ∅) ⊢ e ⇒ τ; S₁   S₁ ⊢ Δ₁ ⇓\* S′   Δ′ = ∅
---------------------------------------------------- Entry
⊢ e ⇒ τ; S′
// Δ′ = ∅ is REQUIRED, not derived: ⇓ drops the stump it discharged by its RESULT
// variable, so finalizing all of Δ empties Δ exactly when those variables are
// pairwise distinct. True of the δ's inference draws; not yet proved of them.
//
// Worked, both shapes: (x: x.l) runs to {β} → ★ with l flagged, F-★ supplying the
// ★ — the L1-finalized type that finalized_no_blur says nothing can sharpen back
// (selEx_runs, lean/Infer.lean). And (x: {a = x.l}) {l = c} runs to {a: 𝓫} with Δ
// already EMPTY: the application solved the blocker, saturation woke the stump
// with K-hit, and finalization had nothing left to blur (fStarEx_runs,
// lean/InferSound.lean). The second is the one the earlier rules got wrong.


== Generalization
- A stump is carried iff its blocker is generalized. One blocked on a var free
  in Γ belongs to the enclosing scope and stays in Δ
- Inhabitation (T-let's second premise) holds by construction: a carried stump
  always finalizes at ★ if nothing better, so the scheme has at least one
  instance

Γ; S ⊢ e₁ ⇒ τ₁; S₁   Δ₁ = Δ_Γ ⊎ Δ_q   ᾱ = (ftv(⟦S₁⟧τ₁) ∪ ftv(Δ_q)) ∖ ftv(⟦S₁⟧Γ)
κ̄ = Γ(ᾱ)   Γ·(x: ∀(ᾱ: κ̄). Δ_q ⇒ ⟦S₁⟧τ₁); S₁ ∖ Δ_q ⊢ e₂ ⇒ τ₂; S₂
-------------------------------------------------------------------------------- A-let
Γ; S ⊢ let x = e₁ in e₂ ⇒ τ₂; S₂
// Δ_q are the stumps whose blocker lands in ᾱ, Δ_Γ the rest. The split is a
// least FIXPOINT: putting a stump in Δ_q adds its free vars to ᾱ, which can
// pull in further stumps. Monotone and bounded by |Δ₁|, so it terminates.
// Surviving stumps of Δ_Γ are NOT finalized here — they outlive the boundary.


== Precision
- τ′ ⊑ τ reads "τ′ is at least as precise as τ": ★ is the top, everything else
  is structural congruence.


------ ⊑-refl
τ ⊑ τ


------ ⊑-★
τ ⊑ ★


τ₁ ⊑ τ₁′   τ₂ ⊑ τ₂′
--------------------- ⊑-fn
τ₁ → τ₂ ⊑ τ₁′ → τ₂′


ρ ⊑ ρ′
----------- ⊑-rec
{ρ} ⊑ {ρ′}


----------- ⊑-ρ-refl
ρ ⊑ ρ


τ ⊑ τ′
------------------- ⊑-field
(l: τ) ⊑ (l: τ′)


ρ₁ ⊑ ρ₁′   ρ₂ ⊑ ρ₂′
------------------------ ⊑-ρ-conc
(ρ₁ | ρ₂) ⊑ (ρ₁′ | ρ₂′)


- Lookup-result precision r′ ⊑ᵣ r: only ? can be improved, definite results are
  final — the relational form of lookup monotonicity. Found-results are
  congruent in ⊑ (their types may sharpen once row precision is in play; on a
  fixed row they stay on the nose), so ⊑-r-refl is derivable

τ′ ⊑ τ
--------- ⊑-r-found
τ′ ⊑ᵣ τ


----------- ⊑-r-⊥
⊥ ⊑ᵣ ⊥


----------- ⊑-r-?
r ⊑ᵣ ?
