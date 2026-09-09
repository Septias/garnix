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
  All sorting adds is that α is bound at a sort — α at a type position and α at
  a row position are no longer the same variable
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
----------------------------- S-fn
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
