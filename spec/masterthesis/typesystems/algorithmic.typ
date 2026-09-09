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


τ ≠ α   α ∉ ftv(τ)
-------------------- U-bind
α ≐ τ ⇝ [α ≔ τ]


τ ≠ α   α ∈ ftv(τ)
-------------------- U-occurs
α ≐ τ ⇝ occurs
// Sorted, the guard tests ftv at Type only. The mechanization still uses the
// unsorted ftv, which spans both sorts, so it also rejects α ≐ {… α …} when
// the inner α is a row-var — solvable, and exactly the conservatism sorting
// removes.


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


α ∉ vars(s)
---------------------- U-var-solve
α ≐ᵣ s ⇝ [α ≔ s]


α ∈ vars(s)
---------------------- U-var-occurs
α ≐ᵣ s ⇝ occurs
// Conservative: α ≐ᵣ (β | α | γ) is reported occurs though it has an mgu.
// A row-monoid fact, NOT a sorting one — sorting does not remove this leg.


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
- Everything below is DESIGN, not mechanization: ≐/≐ᵣ are mechanized
  (RowUnify), this layer is not
- θ is sort-respecting, hence really two maps — one per sort. ⟦S⟧τ applies S's
  substitution as a closure
- Δ indexes each parked stump by its *blocker*: the row-var whose L-α-free
  produced the ?. The blocker is a wake-up index, NOT part of the constraint —
  the declarative stump ⟨ρ.l ↓ δ⟩ carries none
- W collects definite-absence flags and ★-degradations, named by the selection
  site that produced them. It never affects typing, only diagnostics
- The declarative system READS solutions via L-α; the algorithm WRITES them via
  unification. θ is only ever refined

S := (θ, Δ, W)
Δ := ∅ | ⟨α ▷ ρ.l ↓ δ⟩, Δ

S ⊎ q          park a stump
S ∖ Δ′         drop a set of stumps
fresh α: κ     draw a name at sort κ from the threaded supply


== Inference
- (Γ; S ⊢ e ⇒ τ; S′) is syntax-directed, one rule per term former. There are no
  counterparts to T-eq and T-★-intro — inversion-mod-≈ and re-blurring account
  for those on the declarative side
- Failure policy: a `clash` is a hard error (it is PROVED to mean no unifier
  exists, so rejecting is soundness, not choice). `stuck` and `occurs` are
  conservative and may NOT reject; they degrade to ★ with a W-flag

------------------- A-cons
Γ; S ⊢ c ⇒ 𝓫_c; S


x: ∀(ᾱ: κ̄). Q ⇒ τ ∈ Γ   fresh β̄: κ̄   S ⊢ Q[β̄/ᾱ] ↝\* S′
-------------------------------------------------------- A-var
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S′
// A-var IS I-inst: the instantiated constraints are submitted to wake-up, which
// resolves the ones the current θ already decides and parks the rest. Parking
// is the algorithmic image of D-? — only finalization commits ★.


fresh α: Type   Γ·(x: α); S ⊢ e ⇒ τ; S′
----------------------------------------- A-lam
Γ; S ⊢ (x: e) ⇒ α -> τ; S′


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β: Type   S₂ ⊢ τ₁ ≐ (τ₂ -> β) ⇝ S₃
------------------------------------------------------------------------------------- A-app
Γ; S ⊢ e₁e₂ ⇒ β; S₃


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂: Row   S₂ ⊢ τ₁ ≐ {ρ₁} ⇝ S₃   S₃ ⊢ τ₂ ≐ {ρ₂} ⇝ S₄
--------------------------------------------------------------------------------------------------------- A-conc
Γ; S ⊢ e₁ ‖ e₂ ⇒ { ρ₂ | ρ₁ }; S₄


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝ S₂   ⟦S₂⟧ ⊢ ρ.l ↓ τ′
----------------------------------------------------------------------- A-sel
Γ; S ⊢ e.l ⇒ τ′; S₂


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝ S₂   ⟦S₂⟧ ⊢ ρ.l ↓ ⊥
---------------------------------------------------------------------- A-sel-⊥
Γ; S ⊢ e.l ⇒ ★; S₂ +W


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝ S₂   ⟦S₂⟧ ⊢ ρ.l ↓ ? on α   fresh δ: Type
------------------------------------------------------------------------------------------- A-sel-?
Γ; S ⊢ e.l ⇒ δ; S₂ ⊎ ⟨α ▷ ρ.l ↓ δ⟩
// NOT ★: returning ★ here would freeze the result and lose every later
// refinement — (x: x.l) would infer {β} -> ★ and no application could recover
// the field type. The stump-var δ keeps the position writable.


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
// Literal rows are spine-var-free by construction.


== Wake-up and Finalization
- (S ⊢ q ↝ S′) wakes one stump, (↝\* its list closure), (S ⊢ q ⇓ S′) finalizes one. K-hit, K-⊥ and F-★
  are D-hit, D-⊥ and D-? — the difference is WHEN: discharge fires once per
  instantiation, wake-up fires each time θ grows
- K-repark has no declarative counterpart: declaratively D-? commits to ★
  immediately, algorithmically the lookup has merely progressed to the next var
- Wake-up fires when a solution α ≔ ρ is written, and only for stumps blocked
  on α
- *Monotone*: a stump resolved found/⊥ is final under every later θ, so resolved
  stumps never re-enter Δ and no fixpoint iteration is needed
- *Deterministic*: lookup is deterministic, so the final θ, W and τ do not
  depend on the wake-up order

⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ τ′   S ⊢ δ ≐ τ′ ⇝ S′
---------------------------------------- K-hit
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩


⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ⊥   S ⊢ δ ≐ ★ ⇝ S′
--------------------------------------------- K-⊥
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩) +W


⟦S⟧ ⊢ (⟦S⟧ρ).l ↓ ? on α′
------------------------------------------------------------ K-repark
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S ∖ ⟨α ▷ ρ.l ↓ δ⟩) ⊎ ⟨α′ ▷ ρ.l ↓ δ⟩


S ⊢ δ ≐ ★ ⇝ S′
------------------------------------------- F-★
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ (S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩) +W
// The algorithmic moment of T-sel-★. Runs at the end of inference and at every
// generalization boundary that does not carry the stump.

// TENSION CASE: if δ is already solved and wake-up finds a different τ′, the
// emitted δ ≐ τ′ CLASHES ⟹ hard error, not a degradation.


== Generalization
- The L1/L2 fork, and the whole reason for qualified schemes. At a let-boundary
  a stump whose result var δ would be generalized can either be FINALIZED
  (δ ≐ ★, L1) or CARRIED into the scheme as a constraint (L2)
- L1 freezes the result and loses every found-instance: no plain ∀ᾱ. τ scheme
  covers both the found- and the ⊥-typing of (x: x.l). L2 is FORCED, not a
  convenience
- A stump is carried iff its blocker is generalized. One blocked on a var free
  in Γ belongs to the enclosing scope and stays in Δ
- Inhabitation (T-let's second premise) holds by construction: a carried stump
  always finalizes at ★ if nothing better, so the scheme has at least one
  instance

Γ; S ⊢ e₁ ⇒ τ₁; S₁   Δ₁ = Δ_Γ ⊎ Δ_q   ᾱ = (ftv(⟦S₁⟧τ₁) ∪ ftv(Δ_q)) ∖ ftv(⟦S₁⟧Γ)   κ̄ = Γ(ᾱ)   Γ·(x: ∀(ᾱ: κ̄). Δ_q ⇒ ⟦S₁⟧τ₁); S₁ ∖ Δ_q ⊢ e₂ ⇒ τ₂; S₂
--------------------------------------------------------------------------------------------------------------------------------------------------- A-let
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
