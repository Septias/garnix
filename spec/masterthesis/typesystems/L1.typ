== Minimal Calculus
> Functions, scoped records, record concat, row-vars, let-poly


l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x | (x: e) | e₁e₂ | (e₁ ‖ e₂) | e.l | { ξ } | let x = e₁ in e₂
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


(∀ τ₁. σ ≥ τ₁ ⟹ Γ ⊢ e₁: τ₁)   Γ·(x: σ) ⊢ e₂: τ₂
------------------------------------------------- T-let
Γ ⊢ let x = e₁ in e₂: τ₂
// Instance-closed: e₁ must type at *every* instance of σ, so generalization is
// sound by construction — no ᾱ ∩ ftv(Γ) = ∅ side condition. The standard
// syntactic rule (generalize ᾱ = ftv(τ₁) ∖ ftv(Γ) from one derivation) is
// admissible via the type-substitution lemma.


Γ ⊢ e₁: { ρ₁ }  Γ ⊢ e₂: { ρ₂ }
-------------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ e: {ρ}   ρ.l ↓ τ
--------------------------- T-sel
Γ ⊢ e.l: τ


Γ ⊢ e: {ρ}   ρ.l ↓ ?
-------------------------- T-sel-★
Γ ⊢ e.l: ★


// Allows to not error on lazy errors
Γ ⊢ e: {ρ}   ρ.l ↓ ⊥
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
- (σ ≥ τ) instantiates all quantifiers at once via a substitution θ over ᾱ;
  each α ∈ ᾱ takes a type at type positions and a row at row positions (θ acts
  as identity outside ᾱ). I-refl is the ᾱ = ∅ case.
- *No tail check needed* (unlike λ⟨⟩): By substitution-stability of ↓,
  instantiating a row-var can never invalidate a definite lookup result — every
  position it could shadow was already ?-poisoned

θ = [ᾱ ↦ τ̄ | ρ̄]
---------------- I-inst
(∀ᾱ. τ) ≥ θτ


== Row-Lookup
- (ρ.l ↓ r) with (r := τ | ⊥ | ?)
- This statement recursively searches rows for a label l
- *Context-free*: the judgement reads nothing but the row. There is no Γ, and in
  particular no *row-solutions* (α = ρ) for a rule to consult — a row-var that
  stands for a known row is *substituted away* before the lookup runs, which is
  what substitution-stability below says. An earlier presentation carried row-solutions in Γ
  and added a rule L-α to chase them; that rule was an implementation of
  substitution, and it cost three things: ↓ was no longer total by structural
  recursion (a solution chain can cycle, so totality needed a rank function on
  row-vars), L-α-free's premise "α unsolved in Γ" was *negative* and therefore
  not preserved by context extension, and instantiation had to become
  Γ-relative. Dropping it costs nothing and removes all three.
- Absent means the label provably does not exist in ρ; T-sel-⊥ still types the
  selection at ★ (soft typing: the checker flags it, the ↯-disjunct of progress
  catches it at runtime)
- Unknown means a row-var could contain l, so no definite type can be derived.
  A row-var is therefore *always* unknown — it is the only rule that mentions
  one


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


- *Determinism*: every row shape matches exactly one rule
- *Totality*: by structural recursion on ρ — no side condition. ↓ is therefore
  a total function of (ρ, l)
- *Substitution-stability*: if (ρ.l ↓ r) with r definite, then
  (θρ.l ↓ θr). Only ? can change category, and that demotion is exactly what
  T-sel-⊥ / T-★-intro absorb. This is the rule L-α used to implement: "chase
  the solution for α" and "substitute α away, then look up" agree wherever the
  answer is definite


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
  final — the relational form of lookup substitution-stability. Found-results are congruent in
  ⊑ (their types may sharpen once row precision is in play; on a fixed row they
  stay on the nose), so ⊑-r-refl is derivable

τ′ ⊑ τ
--------- ⊑-r-found
τ′ ⊑ᵣ τ


----------- ⊑-r-⊥
⊥ ⊑ᵣ ⊥


----------- ⊑-r-?
r ⊑ᵣ ?

