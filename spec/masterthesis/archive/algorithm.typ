= Inference Algorithm
> Constraint generation + solving. Equality solving follows λ⟨⟩ [Fig. 9/10].
> Lookups become deferrable constraints (stumps). The algorithm never produces
> ? during solving — a lookup either resolves, suspends, or collapses to ★ at
> the outermost boundary. ? exists only in the declarative system.


== Syntax
- unification vars α, β, γ (type or row position), result vars δ

C := ⊤ | τ₁ ∼ τ₂ | ρ₁ ∼ ρ₂ | ρ.l ↓ δ | C ∧ C
Δ := • | Δ·(α = τ) | Δ·(α = ρ)
σ := ∀ᾱ[C]. τ

- Δ is idempotent: its range mentions no vars of its domain (also what makes L-α terminate)
- [Δ]τ applies all solutions; Δ ∘ (α = τ) solves α and re-substitutes everywhere
- a *stump* ρ.l ↓ δ is a lookup whose row still contains unsolved vars
- δ stands for the type of the selection, discharged by δ ∼ τ (hit) or δ ∼ ★ (collapse)
- schemes carry residual stumps [C] — HM(X)-style, no user-facing predicates


== Generation
Γ ⊢ e: τ ⤳ C
- easy rules (constants, records pointwise as T-ξ-\*) skipped


x: ∀ᾱ[C]. τ ∈ Γ   fresh ᾱ′
--------------------------------------------- G-var
Γ ⊢ x: τ[ᾱ′/ᾱ] ⤳ C[ᾱ′/ᾱ]      (re-emit the residual stumps)


fresh α   Γ·(x: α) ⊢ e: τ ⤳ C
-------------------------------- G-λ-I
Γ ⊢ (x: e): α -> τ ⤳ C


Γ ⊢ e₁: τ₁ ⤳ C₁   Γ ⊢ e₂: τ₂ ⤳ C₂   fresh β
------------------------------------------------ G-λ-E
Γ ⊢ e₁e₂: β ⤳ C₁ ∧ C₂ ∧ (τ₁ ∼ τ₂ -> β)


Γ ⊢ e₁: τ₁ ⤳ C₁   Γ ⊢ e₂: τ₂ ⤳ C₂   fresh α₁ α₂
--------------------------------------------------- G-conc
Γ ⊢ e₁ ‖ e₂: {(α₂ | α₁)} ⤳ C₁ ∧ C₂ ∧ (τ₁ ∼ {α₁}) ∧ (τ₂ ∼ {α₂})


Γ ⊢ e: τ ⤳ C   fresh α δ
------------------------------------------ G-sel
Γ ⊢ e.l: δ ⤳ C ∧ (τ ∼ {α}) ∧ (α.l ↓ δ)


*G-let*
- Γ ⊢ e₁: τ₁ ⤳ C₁, solve C₁ to a fixpoint: Δ₁ ⊢ C₁ ⟹ \* Δ₁′ ⊢ C₁′
- ᾱ := ftv([Δ₁′]τ₁) ∖ ftv([Δ₁′]Γ)
- split C₁′: stumps blocked on ᾱ go into the scheme, the rest floats outward
- Γ·(x: ∀ᾱ[C_ᾱ]. [Δ₁′]τ₁) ⊢ e₂: τ₂ ⤳ C₂


== Solving
Δ ⊢ C ⟹ Δ′ ⊢ C′
- equality (∼) as in λ⟨⟩ Fig. 10: decompose, swap, solve with occurs-check,
  row unification via label lookup with fresh-tail instantiation (LUtail)
- inherit the (Rfield) side condition: two rows with a shared tail but
  different prefixes must not unify — otherwise tail-instantiation loops

*Stumps*
- Δ ⊨ ρ.l ↓ r: evaluate the L-rules under Δ, reading (α = ρ′) ∈ Δ at L-α
- no L-α-free during solving: an unsolved α in decisive position *blocks*


(S-hit)   Δ ⊢ ρ.l ↓ δ   ⟹   Δ ⊢ δ ∼ τ        if Δ ⊨ ρ.l ↓ τ
(S-fail)  Δ ⊢ ρ.l ↓ δ   ⟹   fail             if Δ ⊨ ρ.l ↓ ⊥   (label provably missing)
(S-red)   Δ ⊢ ρ.l ↓ δ   ⟹   Δ ⊢ ρ′.l ↓ δ     drop definite ⊥-prefixes, inline solved vars
(S-susp)  otherwise no rule applies: the stump stays in C, blocked on the
leftmost unsolved var of its row; solving that var substitutes
into C and the stump wakes automatically


== Collapse
- at the outermost boundary (top level / exported definition), residual stumps
  are discharged by default: δ ∼ ★, stump dropped
- ★ is produced *only* here: it is exactly the residue of a stump that
  survives to the outermost boundary
- ¿ alternative policy: collapse at every let — simpler, no [C] in schemes,
  but ★s out the motivating example (let-bound ‖-functions)


== Metatheory
- *sound wake-ups*: lookup monotonicity (lean/minimal.lean: lookup_mono) —
  definite results survive Δ-extension, so a resumed stump never invalidates
  the ⊥-prefixes it already skipped
- *termination*: every S-step shrinks the stump's row or discharges it,
  every wake-up consumes one solution, Δ stays idempotent
- *no tail check*: λ⟨⟩ restricts instantiation to prevent new shadowing;
  here every shadowable position is blocked/collapsed, never wrongly definite
- *principality up to ★*: where no stump collapses the algorithm coincides
  with λ⟨⟩ and returns the MGU; each collapse trades precision for totality
- ¿ soundness wrt the declarative system: algorithm result τ implies a
  declarative derivation (collapsed δ's matched by T-sel-★)


== Example
let f = x: y: (x ‖ y).a in f {a = 1} {}

- x: {α}, y: {β}; G-conc + G-sel emit stump ((β | α).a ↓ δ)   (right-preference: y's row first)
- stump blocks on β, survives solving of C₁
- G-let: f: ∀αβδ[(β | α).a ↓ δ]. {α} -> {β} -> δ
- G-var at the call re-emits ((β′ | α′).a ↓ δ′) with fresh vars
- unification against the arguments: α′ = (a: Int), β′ = ε
- wake: (ε | a: Int).a ↓ δ′ ⟹ S-red drops ε ⟹ S-hit: δ′ ∼ Int
- result: Int — λ⟨⟩ rejects this program (no MGU), naive ?-lookup gives ★

let g = x: y: (x ‖ y).a in g          (never applied)
- stump survives to the top level ⟹ collapse δ ∼ ★
- g: {α} -> {β} -> ★
