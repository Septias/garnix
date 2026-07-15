== Minimal Calculus
> Functions, scoped records, record concat, row-vars, let-poly


l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x | (x: e) | e₁e₂ | e₁ ‖ e₂ | e.l | { ξ } | let x = e₁ in e₂
ξ := ε | l = e | (ξ₁ | ξ₂)

τ := α | 𝓫 | ★ | τ -> τ | { ρ }
ρ := ε | α | l: τ | (ρ₁ | ρ₂)
σ := ∀ᾱ. τ | τ


== Declarative

----------- T-cons
Γ ⊢ c: 𝓫_c


x: σ ∈ Γ   σ ≥ τ
------------------ T-var
Γ ⊢ x: τ


Γ ⊢ e: τ₁   τ₁ ≈ τ₂
--------------------- T-eq
Γ ⊢ e: τ₂


Γ·(x: τ₁) ⊢ e: τ₂
--------------------- T-λ-I
Γ ⊢ (x: e): τ₁ -> τ₂


Γ ⊢ e₁: τ₁ -> τ₂   Γ ⊢ e₂ : τ₁
-------------------------------- T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: τ₁   ᾱ = ftv(τ₁) ∖ ftv(Γ)   Γ·(x: ∀ᾱ. τ₁) ⊢ e₂: τ₂
------------------------------------------------------------ T-let
Γ ⊢ let x = e₁ in e₂: τ₂

// The mechanization (minimal.lean) uses the equivalent *instance-closed* form
//   ∀ τ₁ ≤ σ. Γ ⊢ e₁: τ₁    Γ·(x: σ) ⊢ e₂: τ₂  ⟹  Γ ⊢ let x = e₁ in e₂: τ₂
// which is sound by construction (no ᾱ ∩ ftv(Γ) side condition, hence no
// variable-capture/renaming machinery) and feeds the polymorphic substitution
// lemma its exact hypothesis at let-β. The syntactic rule above is admissible
// via a type-substitution lemma (future work).


Γ ⊢ e₁: {ρ₁}  Γ ⊢ e₂: {ρ₂}
----------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ τ
------------------------------- T-sel
Γ ⊢ e.l: τ


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ?
------------------------------- T-sel-★
Γ ⊢ e.l: ★


Γ ⊢ e: {ρ}   Γ ⊢ ρ.l ↓ ⊥
------------------------------- T-sel-⊥
Γ ⊢ e.l: ★


Γ ⊢ e: τ
----------- T-★-intro
Γ ⊢ e: ★


Γ ⊢ ξ: ρ
------------------ T-rec
Γ ⊢ { ξ }: { ρ }


----------- T-ξ-empty
Γ ⊢ ε: ε


Γ ⊢ e: τ
-------------------- T-ξ-field
Γ ⊢ (l = e): (l: τ)


Γ ⊢ ξ₁: ρ₁   Γ ⊢ ξ₂: ρ₂
------------------------- T-ξ-conc
Γ ⊢ (ξ₁ | ξ₂): (ρ₁ | ρ₂)


== Instantiation
- σ ≥ τ strips quantifiers; α in type position takes a type, in row position a row
  (written ≥, not ⊑ — ⊑ is reserved for the precision relation of type refinement)
- No tail check needed (unlike λ⟨⟩): by monotonicity of ↓, instantiating a
  row-var can never invalidate a definite lookup result — every position it
  could shadow was already ?-poisoned
- But instantiation CAN demote indefinite results: a ? lookup may become ⊥ or τ
  after the row-var is filled in. T-sel-⊥ and T-★-intro exist precisely to absorb
  this — without them preservation fails with untypeable reducts:
  - `let f = (x: x.l) in f {}`: f: ∀β. {β} → ★ instantiated at β ≔ ε; the reduct
    (x: x.l) {} needs x: {ε} ⊢ x.l, but ε.l ↓ ⊥ had no rule (fixed by T-sel-⊥)
  - `let w = (f: (y: f (y.l))) in w (z: c) {l = c′}`: after β ≔ (l: 𝓫′), y.l
    refines from ★ to 𝓫′, but the λ-bound f: ★ → 𝓫 has a frozen domain ★
    (fixed by T-★-intro: re-blur 𝓫′ to ★)
- T-★-intro is the "second relation" from the design notes (equate ★ with
  ordinary types): kept out of ≈ so head rigidity survives, non-transitive by
  construction since nothing sits above ★ and ★ has no elimination rules yet
- With both rules, type safety stays on the nose: refined types can always be
  re-blurred to ★, so the precision relation ⊑ is only needed for the
  refinement theorem, not for progress/preservation


----------- I-refl
τ ≥ τ


σ[τ′/α] ≥ τ
------------- I-ty
(∀α. σ) ≥ τ


σ[ρ/α] ≥ τ
------------- I-row
(∀α. σ) ≥ τ


== Row-Lookup
- Γ ⊢ ρ.l ↓ r with r := τ | ⊥ | ?

- This statement recursively searches rows for a label l
- absent means the label provably does not exist in ρ; T-sel-⊥ still types the
  selection at ★ (soft typing: the checker flags it, the ↯-disjunct of progress
  catches it at runtime)
- unknown means an unconstrained row-var could contain l, so no definite type
  can be derived


----------------- L-ε
Γ ⊢ ε.l ↓ ⊥


l₁ = l₂
----------------------------- L-hit
Γ ⊢ (l₁: τ).l₂ ↓ τ


l₁ ≠ l₂
----------------------------- L-miss
Γ ⊢ (l₁: τ).l₂ ↓ ⊥


Γ ⊢ α: {ρ}   Γ ⊢ ρ.l ↓ r
----------------------------- L-α
Γ ⊢ α.l ↓ r


α ∉ Γ
----------------------------- L-α-free
Γ ⊢ α.l ↓ ?


Γ ⊢ ρ₁.l ↓ τ
----------------------------- L-conc-hit
Γ ⊢ (ρ₁ | ρ₂).l ↓ τ


Γ ⊢ ρ₁.l ↓ ⊥   Γ ⊢ ρ₂.l ↓ r
--------------------------------- L-conc-skip
Γ ⊢ (ρ₁ | ρ₂).l ↓ r


Γ ⊢ ρ₁.l ↓ ?
----------------------------- L-conc-★
Γ ⊢ (ρ₁ | ρ₂).l ↓ ?


== Row-Equivalence
- ρ₁ ≈ ρ₂, lifted to types congruently (τ₁ ≈ τ₂)
- Rows are equal up to reassociation, ε-units and swapping distinct labels
- Deliberately not derivable: swapping equal labels (changes shadowing),
  moving a field past a row-var (its instantiation could shadow it)


----------- ≈-refl
ρ ≈ ρ


ρ₂ ≈ ρ₁
----------- ≈-symm
ρ₁ ≈ ρ₂


ρ₁ ≈ ρ₂   ρ₂ ≈ ρ₃
------------------ ≈-trans
ρ₁ ≈ ρ₃


τ₁ ≈ τ₂
------------------- ≈-ext
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
