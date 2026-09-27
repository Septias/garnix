== L2 Calculus
> Functions, scoped records, record-concat, row-vars, fc-labels, let-poly, qualified schemes, parked lookups

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
Γ := • | Γ·(x: σ) | Γ·(α: κ)


== Sorts
- κ classifies variables
- τ and ρ are syntactically disjoint


α: Type ∈ Γ
------------ S-var
Γ ⊢ α: Type


------------ S-base
Γ ⊢ 𝓫: Type


----------- S-★
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


----------- S-ε
Γ ⊢ ε: Row


Γ ⊢ τ: Type
----------------- S-field
Γ ⊢ (l: τ): Row


Γ ⊢ ρ₁: Row   Γ ⊢ ρ₂: Row
--------------------------- S-conc
Γ ⊢ (ρ₁ | ρ₂): Row


Γ ⊢ ρ: Row   δ: Type ∈ Γ
-------------------------- S-stump
Γ ⊢ ⟨ρ.l ↓ δ⟩ ok


(∀ q ∈ Q. Γ·(ᾱ: κ̄) ⊢ q ok)   Γ·(ᾱ: κ̄) ⊢ τ: Type
-------------------------------------------------- S-scheme
Γ ⊢ (∀(ᾱ: κ̄). Q ⇒ τ) ok


== Stumps
- A stump ⟨ρ.l ↓ δ⟩ is a parked selection: the lookup of l in ρ blocked on a
  row-var, with δ the *result variable* standing for whatever the lookup will
  turn out to be
- δ ∈ ᾱ, drawn from the quantifier like every other variable: the constraint
  pins δ's image at *instantiation* time instead of freezing it at
  generalization time
- Plain schemes embed as Q = ∅; the discharge premise is then vacuous and
  ≥ degenerates to the σ ≥ τ of the minimal calculus


== Declarative

----------- T-cons
Γ ⊢ c: 𝓫_c


x: σ ∈ Γ   σ ≥ τ
-------------------- T-var
Γ ⊢ x: τ


Γ ⊢ e: τ₁   τ₁ ≈ τ₂
--------------------- T-eq
Γ ⊢ e: τ₂


Γ ⊢ τ₁: Type   Γ·(x: τ₁) ⊢ e: τ₂
----------------------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ σ ok   (∀ τ₁. σ ≥ τ₁ ⟹ Γ ⊢ e₁: τ₁)   (∃ τ₁. σ ≥ τ₁)   Γ·(x: σ) ⊢ e₂: τ₂
------------------------------------------------------------------------------------ T-let
Γ ⊢ let x = e₁ in e₂: τ₂


Γ ⊢ e₁: { ρ₁ }  Γ ⊢ e₂: { ρ₂ }
-------------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ e: {ρ}   ρ.l ↓ τ
--------------------------- T-sel
Γ ⊢ e.l: τ

// -------------------- The soft-typing rules -----------------------
Γ ⊢ e: {ρ}   ρ.l ↓ ?
-------------------------- T-sel-★
Γ ⊢ e.l: ★


Γ ⊢ e: {ρ}   ρ.l ↓ ⊥
-------------------------- T-sel-⊥
Γ ⊢ e.l: ★


Γ ⊢ e: τ
--------- T-★-intro
Γ ⊢ e: ★

// ------------------------------------------------------------------


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
- (σ ≥ τ) instantiates all quantifiers at once via a *sort-respecting* θ over ᾱ,
  then discharges every q ∈ Q
- *No tail check needed*: By substitution-stability of ↓,
  instantiating a row-var can never invalidate a definite lookup result — every
  position it could shadow was already ?-poisoned

Γ ⊢ θ: ᾱ:κ̄   (∀ q ∈ Q. θ ⊢ q)
---------------------------------- I-inst
(∀(ᾱ: κ̄). Q ⇒ τ) ≥ θτ


== Discharge
- (θ ⊢ q) replays the parked lookup under θ and pins δ to the verdict
- The three arms are exactly T-sel / T-sel-⊥ / T-sel-★, replayed per instance
- Determinism and totality of ↓ are what keep discharge well-behaved

(θρ).l ↓ τ_r   θδ = τ_r
----------------------------- D-hit
θ ⊢ ⟨ρ.l ↓ δ⟩


(θρ).l ↓ ⊥   θδ = ★
------------------------- D-⊥
θ ⊢ ⟨ρ.l ↓ δ⟩


(θρ).l ↓ ?   θδ = ★
------------------------- D-?
θ ⊢ ⟨ρ.l ↓ δ⟩
// Declaratively a still-unknown lookup stays blurred; algorithmically this
// case RE-PARKS instead, and only finalization commits ★.


== Row-Lookup
- (ρ.l ↓ r) with (r := τ | ⊥ | ?)
- This statement recursively searches rows for a label l


------------ L-ε
ε.l ↓ ⊥


l₁ = l₂
-------------------- L-hit
(l₁: τ).l₂ ↓ τ


l₁ ≠ l₂
-------------------- L-miss
(l₁: τ).l₂ ↓ ⊥


--------------- L-α-free
α.l ↓ ?


ρ₁.l ↓ τ
-------------------- L-conc-hit
(ρ₁ | ρ₂).l ↓ τ


ρ₁.l ↓ ⊥   ρ₂.l ↓ r
----------------------------- L-conc-skip
(ρ₁ | ρ₂).l ↓ r


ρ₁.l ↓ ?
-------------------- L-conc-★
(ρ₁ | ρ₂).l ↓ ?


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
vars(s)         the var sequence of s
win\_l(s)       remove the first l-field of the LEADING var-free window
win^R\_l(s)     remove the last l-field of the TRAILING var-free window
rem\_l(s)       remove the first l-field ANYWHERE (vars skipped)


== Unification
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


----------- U-★
★ ≐ ★ ⇝ ∅


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
- Every move is FORCED (solution-set preserving).
- Tried top-to-bottom
- No rule pushes a field demand INTO a var: lookups park as stumps, so *the
  algorithm never guesses a field into a variable*

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


α ∈ vars(s)   s field-free   k = |α|\_s
------------------------------------------------ U-var-collapse
α ≐ᵣ s ⇝ [γ ≔ ε : γ ∈ vars(s), γ ≠ α or k ≥ 2]
// NOT a failure. The ≈-characterization reads |m(θα)| = k·|m(θα)| + Σ_{γ≠α},
// at the var-sequence length and at every field count alike, so at k = 1 every
// OTHER spine variable is forced to ε and α stays free (ε | α | ε ≈ α), and at
// k ≥ 2 α is forced too. Both are unique, hence mgus (`allvar_occurs_mgu`).

α ∈ rowvars(s)   ¬(α ∈ vars(s) ∧ s field-free)
---------------------------------------------- U-var-occurs
α ≐ᵣ s ⇝ occurs


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


|s₁|\_l > |s₂|\_l   vars(s₂) = ⟨⟩
----------------------------------- U-clash
s₁ ≐ᵣ s₂ ⇝ clash


no rule above applies
----------------------- U-stuck
s₁ ≐ᵣ s₂ ⇝ stuck


== Solver State
- θ: Sort-respecting substitution. ⟦S⟧τ applies the substition of S to τ.
- Δ: Indexed list of parked stumps
- W: List of warnings
- S: Solvest state

S := (θ, Δ, W)
Δ := ∅ | ⟨α ▷ ρ.l ↓ δ⟩, Δ

S ⊎ q          park a stump
S ∖ Δ′         drop a set of stumps
fresh α: κ     draw a name at sort κ from the threaded supply

- *Quiescence*, the state invariant: every stump in Δ is genuinely blocked on the
  blocker it records, `(⟦S⟧ρ).l ↓ ? on α` for each `⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ`.
- Every equation in the A-rules below has to be  *solve-then-saturate*

S ⊢ τ ≐ τ′ ⇝! S′    solve, then re-run wake-up on what the solution staled
S ⊢ Δ ↝! S′         saturation itself: step on stale stumps until quiescent
S ⊢ Q ↝\*! S′       A-var's closure, then saturation


== Inference
- Statements of the form: Γ; S ⊢ e ⇒ τ; S′
- S stores the constraints


------------------- A-cons
Γ; S ⊢ c ⇒ 𝓫_c; S


x: ∀(ᾱ: κ̄). Q ⇒ τ ∈ Γ   fresh β̄: κ̄   S ⊢ Q[β̄/ᾱ] ↝\*! S′
-------------------------------------------------------- A-var
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S′


fresh α: Type   Γ·(x: α); S ⊢ e ⇒ τ; S′
----------------------------------------- A-lam
Γ; S ⊢ (x: e) ⇒ α -> τ; S′


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β: Type   S₂ ⊢ τ₁ ≐ (τ₂ -> β) ⇝! S₃
------------------------------------------------------------------------------------- A-app
Γ; S ⊢ e₁e₂ ⇒ β; S₃


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂: Row   S₂ ⊢ τ₁ ≐ {ρ₁} ⇝! S₃   S₃ ⊢ τ₂ ≐ {ρ₂} ⇝! S₄
--------------------------------------------------------------------------------------------------------- A-conc
Γ; S ⊢ e₁ ‖ e₂ ⇒ { ρ₂ | ρ₁ }; S₄


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝! S₂   (⟦S₂⟧ρ).l ↓ τ′
----------------------------------------------------------------------- A-sel
Γ; S ⊢ e.l ⇒ τ′; S₂


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝! S₂   (⟦S₂⟧ρ).l ↓ ⊥
----------------------------------------------------------------------- A-sel-⊥
Γ; S ⊢ e.l ⇒ ★; S₂ +W


Γ; S ⊢ e ⇒ τ; S₁   fresh ρ: Row   S₁ ⊢ τ ≐ {ρ} ⇝! S₂   (⟦S₂⟧ρ).l ↓ ? on α   fresh δ: Type
-------------------------------------------------------------------------------------------- A-sel-?
Γ; S ⊢ e.l ⇒ δ; S₂ ⊎ ⟨α ▷ ρ.l ↓ δ⟩


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

Γ; S ⊢ e₁ ⇒ τ₁; S₁   κ̄ = S₁(ᾱ)   Δ₁ ~ Δ_q ⊎ Δ_Γ   ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅   ᾱ ∩ dom(S₁) = ∅
Δ_q ∩ Δ = ∅   results(Δ_q) at S₁ ⊆ ᾱ, injective   ᾱ ∩ ftv(⟦S₁⟧Δ_Γ) = ∅   Δ_q independent at S₁
Γ·(x: ∀(ᾱ: κ̄). ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ ∖ Δ_q ⊢ e₂ ⇒ τ₂; S₂
---------------------------------------------------------------------------------------------- A-let
Γ; S ⊢ let x = e₁ in e₂ ⇒ τ₂; S₂
// Δ_q are the stumps whose blocker lands in ᾱ, Δ_Γ the rest.
// Surviving stumps of Δ_Γ are NOT finalized here — they outlive the boundary.
// Every premise past the first is one that soundness or completeness turned out to need
// - κ̄ = S₁(ᾱ): kinds are read off the DRAW, not off Γ — ᾱ is disjoint from Γ,
//   so Γ(ᾱ) was never writable. Every generalized binder is one inference
//   invented at a known sort (A-var's draw records one, see there)
// - Δ₁ ~ Δ_q ⊎ Δ_Γ: a PARTITION up to order. Δ₁ is ordered by parking time, and
//   a prefix split could not generalize a stump parked after a Γ-stump
// - ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅, per variable of Γ and at both sorts: Γ-freshness was
//   MISSING and `λy. let z = y in z` inferred a → b
//   (`runSound_false_unguarded_let`, lean/LetSound.lean)
// - ᾱ ∩ dom(S₁) = ∅: nothing already solved is generalized
// - Δ_q ∩ Δ = ∅: Δ_q is e₁'s OWN. Without it `{a = λx. x.l, b = let y = c in c}`
//   files a's stump under y's unused scheme and nothing ever finalizes it
//   (`runSound_false_let_captures`)
// - results at S₁: each generalized stump's result, read at S₁ (δ itself while
//   unsolved, the variable it was aliased to otherwise), is a variable in ᾱ, and
//   distinct stumps read as distinct variables. Reading it RAW rejected
//   `let h = λy. g y`, where A-app aliases δ ≔ β. A spent δ reads as a
//   non-variable and still fails here — the spent promise below
// - ᾱ ∩ ftv(⟦S₁⟧Δ_Γ) = ∅: what stays parked reads the same at every instance
// - independent: no generalized stump's row, read at S₁, mentions another's
//   result. Then an instance's constraints discharge one at a time
//   (`QScheme.Correctable.correct`); without it they need not
//   (`instEquivCorrects_false`)
//
// ᾱ is CANONICAL (`greatestAlpha_spec`, lean/LetChoice.lean): admissible choices
// are closed under union, so a GREATEST one exists, and `greatestAlpha` computes
// it by deleting variables no admissible ᾱ can contain until nothing is deleted.
// This replaces the least-fixpoint reading of the split: the algorithm never has
// to guess.


== Wake-up and Finalization
- (S ⊢ q ↝ S′) wakes one stump
- ↝\* its list closure,
- (S ⊢ Δ ↝! S′) runs it to quiescence
- (S ⊢ q ⇓ S′) finalizes one
- (⇓\* its list closure).


(⟦S⟧ρ).l ↓ τ′   S ⊢ δ ≐ τ′ ⇝ S′
---------------------------------------- K-hit
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩


(⟦S⟧ρ).l ↓ ⊥   S ⊢ δ ≐ ★ ⇝ S′
--------------------------------------------- K-⊥
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩) +W


(⟦S⟧ρ).l ↓ ? on α′
------------------------------------------------------------ K-repark
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S ∖ ⟨α ▷ ρ.l ↓ δ⟩) ⊎ ⟨α′ ▷ ρ.l ↓ δ⟩


--------- K-nil
S ⊢ ∅ ↝\* S


S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S₁   S₁ ⊢ Δ′ ↝\* S′
----------------------------------------- K-cons
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ↝\* S′


(⟦S⟧ρ).l ↓ ? on α   (S ⊎ ⟨α ▷ ρ.l ↓ δ⟩) ⊢ Δ′ ↝\* S′
----------------------------------------------------------- K-park
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ↝\* S′
// The blocker is DETERMINED, not supplied: the premise is what fixes α, so an
// instantiated stump cannot be parked on a variable that is not the one
// actually blocking its lookup. That is the state invariant A-var would
// otherwise be free to break.


S quiescent
-------------- K!-done
S ⊢ Δ ↝! S


⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ   ¬ (⟦S⟧ρ).l ↓ ? on α   S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S₁   S₁ ⊢ Δ₁ ↝! S′
-------------------------------------------------------------------------------------- K!-step
S ⊢ Δ ↝! S′
// A step only on a STALE stump — one whose recorded blocker no longer blocks its
// lookup, which is exactly what a solution write creates. ↝ then resolves it
// (K-hit/K-⊥) or moves the annotation (K-repark). ⇝! and ↝\*! are this composed
// after ≐ and ↝\* respectively.


⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ   (⟦S⟧ρ).l ↓ ? on α   S ⊢ δ ≐ ★ ⇝ S′
--------------------------------------------------------------- F-★
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ (S′ ∖ ⟨α ▷ ρ.l ↓ δ⟩) +W


--------- F-nil
S ⊢ ∅ ⇓\* S


S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ S₁   S₁ ⊢ Δ′ ⇓\* S′
--------------------------------------- F-cons
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ⇓\* S′

// This one is INCOMPLETENESS, not a justified rejection: that program IS typeable,
// at {(l: 𝓫 -> 𝓫)} -> 𝓫 -> 𝓫 (spentEx_declarative). No ★ is ever formed, so it is
// not the ★-elimination gap either. The algorithm commits x to {ρ} with ρ abstract
// and never guesses a concrete row, so its only possible answer is to CARRY
// ⟨ρ.l ↓ (α_y -> β)⟩ — which it cannot write, because a stump's result position


== Entry
Toplevel (⊢ e ⇒ τ; S′)

∅; (id, ∅, ∅) ⊢ e ⇒ τ; S₁   S₁ ⊢ Δ₁ ⇓\* S′
------------------------------------------- Entry
⊢ e ⇒ τ; S′


== Precision
- τ′ ⊑ τ reads "τ′ is at least as precise as τ"
- ★ is the top


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
  final — the relational form of lookup substitution-stability. Found-results are
  congruent in ⊑ (their types may sharpen once row precision is in play; on a
  fixed row they stay on the nose), so ⊑-r-refl is derivable

τ′ ⊑ τ
--------- ⊑-r-found
τ′ ⊑ᵣ τ


----------- ⊑-r-⊥
⊥ ⊑ᵣ ⊥


----------- ⊑-r-?
r ⊑ᵣ ?



== First-class labels (phase A: selection)
- ⌊α⌋ is α, α ≔ ⌊l⌋ is ordinary type binding.
- ⌊l⌋ is rigid and nullary
- ⌊l⌋ ≐ ⌊l′⌋ is the whole label pass: equal labels unify, distinct ones clash

e ::= … | 'l | e₁.(e₂)          τ ::= … | ⌊l⌋

- Runtime: 'l is a value; e₁.(e₂) evaluates the record, then the key, and
  (rcd b).('l) steps like (rcd b).l.
  A missing label is ↯, and so is a key thatis not a label


=== Label lookup  Γ ⊢ ρ.q ↓ r

ρ.l ↓ r
----------- LQ-lit
ρ.⌊l⌋ ↓ r


-------------- L-?-lab
(l: τ).α ↓ ?


q is neither ⌊l⌋ nor a variable
--------------------------------- L-junk
ρ.q ↓ ⊥


=== Declarative

---------------- T-lab
Γ ⊢ 'l: ⌊l⌋


Γ ⊢ e₁: {ρ}   Γ ⊢ e₂: q   ρ.q ↓ τ
---------------------------------------- T-sel-dyn
Γ ⊢ e₁.(e₂): τ



=== Algorithmic

--------------------- A-lab
Γ; S ⊢ 'l ⇒ ⌊l⌋; S


Γ; S ⊢ e₁ ⇒ τ₁; S₁   fresh r: Row   S₁ ⊢ τ₁ ≐ {r} ⇝! S₂   Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃   (⟦S₃⟧r).(⟦S₃⟧τ₂) ↓ τ′
--------------------------------------------------------------------------------------------------- A-sel-dyn
Γ; S ⊢ e₁.(e₂) ⇒ τ′; S₃
