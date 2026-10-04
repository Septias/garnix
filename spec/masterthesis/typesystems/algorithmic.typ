== L2 Calculus
> Functions, scoped records, record-concat, row-vars, fc-labels, let-poly, qualified schemes, parked lookups

l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x  | (x: e)  | e₁e₂ | (e₁ ‖ e₂) | e.l | { ξ } | let x = e₁ in e₂
| 'l | e₁.(e₂) | { \${e₁} = e₂ }
ξ := ε | l = e | (ξ₁ | ξ₂)

k := l | α
τ := α | 𝓫 | ★ | τ -> τ | { ρ } | ⌊k⌋
ρ := ε | α | l: τ | \${k}: τ | (ρ₁ | ρ₂)
κ := Type | Row | Label

// the L2-algorithmic part
q := ⟨ρ.k ↓ δ⟩
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


Γ ⊢ ρ: Row   Γ ⊢ δ: Type
-------------------------- S-stump
Γ ⊢ ⟨ρ.l ↓ δ⟩ ok


(∀ q ∈ Q. Γ·(ᾱ: κ̄) ⊢ q ok)   Γ·(ᾱ: κ̄) ⊢ τ: Type
-------------------------------------------------- S-scheme
Γ ⊢ (∀(ᾱ: κ̄). Q ⇒ τ) ok


== Stumps
- A stump ⟨ρ.k ↓ δ⟩ is a parked selection: the lookup of k = (l | α) in ρ blocked on a
  row-var, with δ the *result* standing for whatever the lookup will turn out
  to be
- δ is a type. A-sel-? parks a fresh variable, but unification may write into
  it before the lookup resolves (`λx y. (x.l) y` sets δ ≔ α_y -> β): the promise
  is *spent*, and the stump carries the type it was spent on
- ftv(δ) ⊆ ᾱ, drawn from the quantifier like every other variable: the
  constraint pins δ's image at *instantiation* time instead of freezing it at
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
- A _trace monoid_, hence left- and right-cancellative: stripping a shared end
  var is sound AND complete. This replaces P&X's shared-tail side condition

a := l: τ | α
s := ⟨⟩ | a·s

|s|\_l          number of l-fields in s
vars(s)         the var sequence of s
rowvars(s)      the row vars of s at ANY depth (incl. inside field types)
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
- Every move is forced (solution-set preserving).
- Tried top-to-bottom
- No rule pushes a field demand into a var: lookups park as stumps, so *during
  inference the algorithm never guesses a field into a variable*. Only F-hit
  does, at finalization, and only for a spent promise


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


α ∈ rowvars(s)   ¬(α ∈ vars(s) ∧ s field-free)
----------------------------------------------- U-var-occurs
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
S ∖ q          drop every entry carrying the stump q (equal stumps discharge alike)
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


------------------ A-cons
Γ; S ⊢ c ⇒ 𝓫_c; S


x: ∀(ᾱ: κ̄). Q ⇒ τ ∈ Γ   fresh β̄: κ̄   S ⊢ Q[β̄/ᾱ] ↝\*! S′
-------------------------------------------------------- A-var
Γ; S ⊢ x ⇒ τ[β̄/ᾱ]; S′


fresh α: Type   Γ·(x: α); S ⊢ e ⇒ τ; S′
----------------------------------------- A-lam
Γ; S ⊢ (x: e) ⇒ α -> τ; S′


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh β: Type   S₂ ⊢ τ₁ ≐ (τ₂ -> β) ⇝! S₃
-------------------------------------------------------------------------------------- A-app
Γ; S ⊢ e₁e₂ ⇒ β; S₃


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh ρ₁ ρ₂: Row   S₂ ⊢ τ₁ ≐ {ρ₁} ⇝! S₃   S₃ ⊢ τ₂ ≐ {ρ₂} ⇝! S₄
----------------------------------------------------------------------------------------------------------- A-conc
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


----------------- A-ξ-empty
Γ; S ⊢ ε ⇒ ε; S


Γ; S ⊢ e ⇒ τ; S′
----------------------------- A-ξ-field
Γ; S ⊢ (l = e) ⇒ (l: τ); S′


Γ; S ⊢ ξ₁ ⇒ ρ₁; S₁   Γ; S₁ ⊢ ξ₂ ⇒ ρ₂; S₂
------------------------------------------ A-ξ-conc
Γ; S ⊢ (ξ₁ | ξ₂) ⇒ (ρ₁ | ρ₂); S₂


Γ; S ⊢ e₁ ⇒ τ₁; S₁   κ̄ = S₁(ᾱ)   Δ₁ ~ Δ_q ⊎ Δ_Γ   ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅   ᾱ ∩ dom(S₁) = ∅
Δ_q ∩ Δ = ∅   results(Δ_q) at S₁ ok over ᾱ   ᾱ ∩ ftv(⟦S₁⟧Δ_Γ) = ∅   Δ_q independent at S₁
Γ·(x: ∀(ᾱ: κ̄). ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ ∖ Δ_q ⊢ e₂ ⇒ τ₂; S₂
--------------------------------------------------------------------------------------------- A-let
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
// - ᾱ ∩ dom(S₁) = ∅: nothing already solved is generalized
// - Δ_q ∩ Δ = ∅: Δ_q is e₁'s OWN. Without it `{a = λx. x.l, b = let y = c in c}`
//   files a's stump under y's unused scheme and nothing ever finalizes it
// - results ok at S₁: each generalized stump's result, read at S₁, is a LINEAR
//   PATTERN over ᾱ — a variable, or the type a spent promise was spent on, with
//   records only as {β} and no variable twice — and distinct stumps share no
//   result variable. A pattern lets an instance be corrected to meet each
//   lookup's answer EXACTLY (≈ could reorder a record literal, which no
//   re-choice of variables undoes). Reading at S₁, not raw, accepts
//   `let h = λy. g y`, where A-app aliases δ ≔ β
//   A SPENT result must also be met at some instance (T-let's ∃ τ₁), by
//   extending its blocker with the field as F-hit does: its key is literal,
//   every stump on the same blocker is literally keyed and is it if it has its
//   key, and no spent stump's blocker occurs in a spent result
// - ᾱ ∩ ftv(⟦S₁⟧Δ_Γ) = ∅: what stays parked reads the same at every instance
// - independent: no generalized stump's row or key, read at S₁, mentions a
//   variable of another's result. Then an instance's constraints discharge one
//   at a time. Nested selection `λx. (x.l).m` fails it: monomorphic let
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
- (S ⊢ q ⇓ₘ S′) materializes a spent one, (⇓ₘ\*) its list closure
- (S ⊢ q ⇓ S′) finalizes one
- (⇓\* its list closure).


(⟦S⟧ρ).l ↓ τ′   S ⊢ δ ≐ τ′ ⇝ S′
------------------------------------ K-hit
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S′ ∖ ⟨ρ.l ↓ δ⟩


(⟦S⟧ρ).l ↓ ⊥   S ⊢ δ ≐ ★ ⇝ S′
----------------------------------------- K-⊥
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S′ ∖ ⟨ρ.l ↓ δ⟩) +W


(⟦S⟧ρ).l ↓ ? on α′
------------------------------------------------------ K-repark
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ (S ∖ ⟨ρ.l ↓ δ⟩) ⊎ ⟨α′ ▷ ρ.l ↓ δ⟩


----------- K-nil
S ⊢ ∅ ↝\* S


S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S₁   S₁ ⊢ Δ′ ↝\* S′
----------------------------------------- K-cons
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ↝\* S′


(⟦S⟧ρ).l ↓ ? on α   (S ⊎ ⟨α ▷ ρ.l ↓ δ⟩) ⊢ Δ′ ↝\* S′
----------------------------------------------------------- K-park
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ↝\* S′


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


⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ   ⟦S⟧δ spent   (⟦S⟧ρ).l ↓ ? on α   fresh ρ′: Row   S ⊢ {α} ≐ {l: δ | ρ′} ⇝! S′
------------------------------------------------------------------------------------------------ F-hit
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ₘ S′
// ⟦S⟧δ is SPENT when it is neither a variable nor ★: F-★'s δ ≐ ★ would clash.
// `λx y. (x.l) y` spends δ ≔ α_y -> β, yet it IS typeable, at
// {(l: 𝓫 -> 𝓫)} -> 𝓫 -> 𝓫. The lookup waits on α, which nothing has committed,
// so extending α with the field makes it HIT; saturation then wakes the stump
// itself (K-hit, δ ≐ δ) and everything else waiting on α.
// A key-blocked stump (e₁.(e₂) with the key never known) has no row to extend
// and stays a spent promise: incompleteness.


------------ M-nil
S ⊢ ∅ ⇓ₘ\* S


S ⊢ q ⇓ₘ S₁   S₁ ⊢ Δ′ ⇓ₘ\* S′
------------------------------ M-cons
S ⊢ (q, Δ′) ⇓ₘ\* S′


S ⊢ Δ′ ⇓ₘ\* S′
-------------------- M-skip
S ⊢ (q, Δ′) ⇓ₘ\* S′
// q is not spent, or an earlier step's saturation already discharged it


⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ   (⟦S⟧ρ).l ↓ ? on α   S ⊢ δ ≐ ★ ⇝ S′
-------------------------------------------------------- F-★
S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ (S′ ∖ ⟨ρ.l ↓ δ⟩) +W


----------- F-nil
S ⊢ ∅ ⇓\* S


S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ⇓ S₁   S₁ ⊢ Δ′ ⇓\* S′
--------------------------------------- F-cons
S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) ⇓\* S′


== Entry
Toplevel (⊢ e ⇒ τ; S′)

∅; (id, ∅, ∅) ⊢ e ⇒ τ; S₁   S₁ ⊢ Δ₁ ⇓ₘ\* S₂   S₂ ⊢ Δ₂ ⇓\* S′
--------------------------------------------------------------- Entry
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



== First-class labels
- Labels are values ('l) with singleton types ⌊l⌋
- *Keys are their own sort*: a key k is a label l or a label variable α: Label.
  Substitutions have three components (Type, Row, Label), and θα for α: Label is
  again a key.
- ⌊l⌋ is rigid and nullary, like 𝓫

=== Sorts

α: Label ∈ Γ
-------------- S-key-var
Γ ⊢ α: Label


-------------- S-key-lit
Γ ⊢ l: Label


Γ ⊢ k: Label
--------------- S-lab
Γ ⊢ ⌊k⌋: Type


Γ ⊢ k: Label   Γ ⊢ τ: Type
--------------------------- S-dfield
Γ ⊢ (\${k}: τ): Row


=== Key comparison k ⋈ k′ ∈ {=, ≠, ?}
- = for the same label or the same label variable, ≠ for distinct labels, ? otherwise.
- Both = and ≠ are stable under substitution, otherwise ?


=== Keyed lookup ρ.k ↓ r
- Generalizes ρ.l ↓ r from labels to keys


ε.k ↓ ⊥  k ⋈ l = =
------------------- LQ-hit
(l: τ).k ↓ τ


k ⋈ l = ≠
-------------- LQ-miss
(l: τ).k ↓ ⊥


k ⋈ l = ?
-------------- LQ-?-lab
(l: τ).k ↓ ?


k ⋈ k′ = =
------------------ LQ-dhit
(\${k′}: τ).k ↓ τ


k ⋈ k′ = ≠
------------------ LQ-dmiss
(\${k′}: τ).k ↓\ ⊥


k ⋈ k′ = ?
------------------ LQ-d?
(\${k′}: τ).k ↓ ?


--------- LQ-α-free
β.k ↓ ?


ρ₁.k ↓ τ
----------------- LQ-conc-hit
(ρ₁ | ρ₂).k ↓ τ


ρ₁.k ↓ ⊥   ρ₂.k ↓ r
--------------------- LQ-conc-skip
(ρ₁ | ρ₂).k ↓ r


ρ₁.k ↓ ?
----------------- LQ-conc-?
(ρ₁ | ρ₂).k ↓ ?


- The label lookup ρ.l ↓ r of the Row-Lookup section is exactly ρ.k ↓ r at
  k = l (`LookupQ.lab_iff`)


=== Blockers  ρ.k ↓ ? on β
- Which variable a ? waits on. It may now be a label variable: the lookup key
  (λa. {x = c}.(a) waits on a) or a field key ({\${γ}: τ}.foo waits on γ)

-------------- B-var
β.k ↓ ? on β


------------------- B-sunk
(l: τ).α ↓ ? on α


α ⋈ k′ = ?
----------------------- B-dkey
(\${k′}: τ).α ↓ ? on α


----------------------- B-dfield
(\${γ}: τ).l ↓ ? on γ


ρ₁.k ↓ ⊥    ρ₂.k ↓ ? on β
-------------------------- B-skip
(ρ₁ | ρ₂).k ↓ ? on β


ρ₁.k ↓ ? on β
---------------------- B-conc
(ρ₁ | ρ₂).k ↓ ? on β


=== Row equivalence
- A keyed field is congruent only under the same key, and a literal key is the
literal field. Substitution does not normalize \${l}: τ to l: τ; ≈ does


τ₁ ≈ τ₂
------------------------ ≈-dfield
(\${k}: τ₁) ≈ (\${k}: τ₂)


------------------ ≈-dlab
(\${l}: τ) ≈ (l: τ)


=== Spines with keyed fields
- a := l: τ | α |\${α}: τ (a literal key normalizes to a field)
- A keyed field is a barrier, like a var: it may become ANY label, so a field cannot commute past it (it might be the same label and shadow it)
- ≈-characterization (`rowEquiv_iff_char`), now three-way:
- the same barrier sequence (vars and keys, in order)
- per label, the (segment index, type) lists agree pointwise; segments are separated by barriers
- the keyed fields' types agree pointwise, in order
- win\_l stops at a keyed field. "field-free" / "var-free" in the row rules below read "barrier-free", and |s|\_l does not count keyed fields


=== Unification
- The label pass is one rule over keys, flat like 𝓫


--------------- U-key-refl
⌊α⌋ ≐ ⌊α⌋ ⇝ ∅

l = l′
---------------- U-lab
⌊l⌋ ≐ ⌊l′⌋ ⇝ ∅

l ≠ l′
-------------------- U-lab-clash
⌊l⌋ ≐ ⌊l′⌋ ⇝ clash

k ≠ α
----------------------- U-key-bind
⌊α⌋ ≐ ⌊k⌋ ⇝ [α ≔ k]
// α: Label, so this binds the LABEL component; no occurs check is needed.
// ⌊k⌋ against any other head clashes (U-clash)
- α ≐ {\${α}: σ} is NOT an occurrence: the key α is a label variable, the other
α a type variable
- Row side, changes to the moves above:
- U-ε-clash also fires on a keyed field: ⟨⟩ has no field of any key
- U-var-occurs: an occurrence next to a keyed field is `stuck`, not occurs
(a key-aware counting argument would be needed; stuck rejects just the same)
- U-clash and U-ground treat keyed fields as barriers, so
(foo: τ) ≐ᵣ (\${α}: τ′) is stuck, not clash (α ≔ foo solves it)


τ ≐ τ′ ⇝ θ   θt₁ ≐ᵣ θt₂ ⇝ θ′
------------------------------------------- U-key-L
(\${α}: τ)·t₁ ≐ᵣ (\${α}: τ′)·t₂ ⇝ θ′ ∘ θ


τ ≐ τ′ ⇝ θ   θt₁ ≐ᵣ θt₂ ⇝ θ′
------------------------------------------- U-key-R
t₁·(\${α}: τ) ≐ᵣ t₂·(\${α}: τ′) ⇝ θ′ ∘ θ
// Only at the very ends: a field before ${α} could be the label α becomes.
// Complete by cancellation (`RowEquiv.dsing_cancel_left/right`): the barrier
// sequences share key α, the keyed projections start with τ and τ′.
// Different unknown keys, ${α}: τ ≐ᵣ ${β}: τ′, stay stuck: α ≔ β and
// α, β ≔ l are unifiers with no common generalization.


=== Declarative

------------ T-lab
Γ ⊢ 'l: ⌊l⌋


Γ ⊢ e₁: {ρ}   Γ ⊢ e₂: ⌊k⌋   ρ.k ↓ τ
------------------------------------ T-sel-dyn
Γ ⊢ e₁.(e₂): τ


Γ ⊢ e₁: {ρ}   Γ ⊢ e₂: ⌊k⌋   ρ.k ↓ ?
------------------------------------ T-sel-dyn-★
Γ ⊢ e₁.(e₂): ★


Γ ⊢ e₁: {ρ}   Γ ⊢ e₂: ⌊k⌋   ρ.k ↓ ⊥
------------------------------------ T-sel-dyn-⊥
Γ ⊢ e₁.(e₂): ★


Γ ⊢ e₁: ⌊k⌋   Γ ⊢ e₂: τ
-------------------------------- T-rcd-dyn
Γ ⊢ { ${e₁} = e₂ }: {${k}: τ }


- Discharge replays keyed stumps with the keyed lookup: θ ⊢ ⟨ρ.k ↓ δ⟩ reads
  (θρ).(θk) ↓ r, arms as D-hit / D-⊥ / D-?


=== Runtime
- 'l is a value. e₁.(e₂): the record, then the key, then the lookup;
  (rcd b).('l) steps like (rcd b).l
- { \${e₁} = e₂ }: the key is evaluated, the field stays a thunk (lazy, like a
  record literal): {\${'l} = e } ⟶ { l = e }
- ↯ on a missing label and on a key that is not a label, for both forms
- qProgress / qPreservation hold; T-rcd-dyn's preservation case is ≈-dlab


=== Algorithmic

--------------------- A-lab
Γ; S ⊢ 'l ⇒ ⌊l⌋; S


Γ; S ⊢ e₁ ⇒ τ₁; S₁   fresh r: Row   S₁ ⊢ τ₁ ≐ {r} ⇝! S₂   Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃
fresh κ: Label   S₃ ⊢ τ₂ ≐ ⌊κ⌋ ⇝! S₄   (⟦S₄⟧r).(⟦S₄⟧κ) ↓ τ′
----------------------------------------------------------------------------------- A-sel-dyn
Γ; S ⊢ e₁.(e₂) ⇒ τ′; S₄


…   (⟦S₄⟧r).(⟦S₄⟧κ) ↓ ⊥
------------------------------------------ A-sel-dyn-⊥
Γ; S ⊢ e₁.(e₂) ⇒ ★; S₄ +W


…   (⟦S₄⟧r).(⟦S₄⟧κ) ↓ ? on α   fresh δ: Type
------------------------------------------------ A-sel-dyn-?
Γ; S ⊢ e₁.(e₂) ⇒ δ; S₄ ⊎ ⟨α ▷ r.κ ↓ δ⟩
// α may be a label variable: the stump waits on the KEY


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   fresh κ: Label   S₂ ⊢ τ₁ ≐ ⌊κ⌋ ⇝! S₃
------------------------------------------------------------------------------------ A-rcd-dyn
Γ; S ⊢ { ${e₁} = e₂ } ⇒ {${κ}: τ₂ }; S₃
// No lookup, so nothing parks. Shaped like A-app


=== Wake-up, let, finalization
- K-rules and saturation are unchanged, over keyed lookups: solving a key wakes
  the stumps blocked on it (λa. {foo = c, bar = ε}.(a) applied to 'foo hits)
- F-hit materializes only a stump with a literal key blocked on its row; a
  key-blocked spent stump stays a spent promise
- F-★ never binds a label variable: it solves δ ≐ ★ with a solution whose row
  and label parts are empty. Key blockers survive it untouched, so phase A's
  KeySafe premise is gone
- A-let: a generalized spent stump must be fillable, i.e. literally keyed with
  a blocker that is not a label variable of its row; nothing else in key
  position may reach a key blocker (KeyFresh); what stays parked must not
  mention ᾱ in row, key or result


=== Headlines
- λa. λx. x.(a)   :: ∀(α: Label)(β: Row)(δ: Type). ⟨β.α ↓ δ⟩ ⇒ ⌊α⌋ -> {β} -> δ
  (`selDynQ_instance_closed`)
- λa. λv. {\${a} = v} :: ∀(α: Label)(δ: Type). ⌊α⌋ -> δ -> {\${α}: δ}
  (`rcdDynQ_instance_closed`); runF infers exactly this
- r // {${n} = v} shadows an older n: (λn. λr. r ‖ {${n} = c}) 'x {x = {}} .x ⇒ 𝓫


=== Costs
- A non-label key is a type error ({foo = c}.(c), {\${c} = c} clash)
  - Keyed fields are barriers: (foo: τ) ≐ᵣ (\${α}: τ′) and different unknown keys
  are stuck (keyed Fuzz universe: 67% stuck)
- A key-blocked spent promise is not materialized (incompleteness)
