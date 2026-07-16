#import "../functions.typ": *

== Minimal Calculus
_Functions, scoped records, record concat, row-vars, let-poly_

#let syntax = figure(
  caption: "The minimal calculus.",
  box(width: 100%, stack(
    spacing: 20pt,
    align(center, flexbox(
      $#type_name("Labels") l ∈ 𝓛$,
      $#type_name("Variables") x ∈ 𝓧$,
      $#type_name("Basetypes") 𝓫 ∈ 𝓑$,
      $#type_name("Constants") c ∈ 𝓒$,
    )),
    subbox(caption: "Terms")[
      $
        #type_name("Term") e & ::= c | x | (x: e) | e₁e₂ | e₁ ‖ e₂ | e.l | { ξ } | #b[let] x = e₁ #b[in] e₂ \
        #type_name("Record Body") ξ & ::= ε | l = e | (ξ₁ | ξ₂) \
      $
    ],
    subbox(caption: "Types")[
      $
               #type_name("Type") τ & ::= α | 𝓫 | ★ | τ -> τ | { ρ } \
                #type_name("Row") ρ & ::= ε | α | l: τ | (ρ₁ | ρ₂) \
        #type_name("Type Scheme") σ & ::= ∀macron(α). τ | τ \
      $
    ],
  )),
)
#syntax


== Declarative

// The mechanization (minimal.lean) uses the equivalent *instance-closed* form
// of T-let
//   ∀ τ₁ ≤ σ. Γ ⊢ e₁: τ₁    Γ·(x: σ) ⊢ e₂: τ₂  ⟹  Γ ⊢ let x = e₁ in e₂: τ₂
// which is sound by construction (no ᾱ ∩ ftv(Γ) side condition, hence no
// variable-capture/renaming machinery) and feeds the polymorphic substitution
// lemma its exact hypothesis at let-β. The syntactic rule below is admissible
// via a type-substitution lemma (future work).

#let declarative = figure(
  caption: "Declarative typing rules.",
  flexbox(
    derive("T-cons", (), $Γ ⊢ c: 𝓫_c$),
    derive("T-var", ($x: σ ∈ Γ$, $σ ≥ τ$), $Γ ⊢ x: τ$),
    derive("T-eq", ($Γ ⊢ e: τ₁$, $τ₁ ≈ τ₂$), $Γ ⊢ e: τ₂$),
    derive("T-λ-I", ($Γ · (x: τ₁) ⊢ e: τ₂$,), $Γ ⊢ (x: e): τ₁ -> τ₂$),
    derive("T-λ-E", ($Γ ⊢ e₁: τ₁ -> τ₂$, $Γ ⊢ e₂: τ₁$), $Γ ⊢ e₁e₂: τ₂$),
    derive(
      "T-let",
      (
        $Γ ⊢ e₁: τ₁$,
        $macron(α) = "ftv"(τ₁) ∖ "ftv"(Γ)$,
        $Γ · (x: ∀macron(α). τ₁) ⊢ e₂: τ₂$,
      ),
      $Γ ⊢ #b[let] x = e₁ #b[in] e₂: τ₂$,
    ),
    derive(
      "T-conc",
      ($Γ ⊢ e₁: {ρ₁}$, $Γ ⊢ e₂: {ρ₂}$),
      $Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }$,
    ),
    derive("T-sel", ($Γ ⊢ e: {ρ}$, $Γ ⊢ ρ.l ↓ τ$), $Γ ⊢ e.l: τ$),
    derive("T-sel-★", ($Γ ⊢ e: {ρ}$, $Γ ⊢ ρ.l ↓ ?$), $Γ ⊢ e.l: ★$),
    derive("T-sel-⊥", ($Γ ⊢ e: {ρ}$, $Γ ⊢ ρ.l ↓ ⊥$), $Γ ⊢ e.l: ★$),
    derive("T-★-intro", ($Γ ⊢ e: τ$,), $Γ ⊢ e: ★$),
    derive("T-rec", ($Γ ⊢ ξ: ρ$,), $Γ ⊢ { ξ }: { ρ }$),
    derive("T-ξ-empty", (), $Γ ⊢ ε: ε$),
    derive("T-ξ-field", ($Γ ⊢ e: τ$,), $Γ ⊢ (l = e): (l: τ)$),
    derive(
      "T-ξ-conc",
      ($Γ ⊢ ξ₁: ρ₁$, $Γ ⊢ ξ₂: ρ₂$),
      $Γ ⊢ (ξ₁ | ξ₂): (ρ₁ | ρ₂)$,
    ),
  ),
)
#declarative


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
    `(x: x.l) {}` needs x: {ε} ⊢ x.l, but ε.l ↓ ⊥ had no rule (fixed by T-sel-⊥)
  - `let w = (f: (y: f (y.l))) in w (z: c) {l = c′}`: after β ≔ (l: 𝓫′), y.l
    refines from ★ to 𝓫′, but the λ-bound f: ★ → 𝓫 has a frozen domain ★
    (fixed by T-★-intro: re-blur 𝓫′ to ★)
- T-★-intro is the "second relation" from the design notes (equate ★ with
  ordinary types): kept out of ≈ so head rigidity survives, non-transitive by
  construction since nothing sits above ★ and ★ has no elimination rules yet
- With both rules, type safety stays on the nose: refined types can always be
  re-blurred to ★, so the precision relation ⊑ is only needed for the
  refinement theorem, not for progress/preservation

#let instantiation = figure(
  caption: "Instantiation.",
  flexbox(
    derive("I-refl", (), $τ ≥ τ$),
    derive("I-ty", ($σ[τ′\/α] ≥ τ$,), $(∀α. σ) ≥ τ$),
    derive("I-row", ($σ[ρ\/α] ≥ τ$,), $(∀α. σ) ≥ τ$),
  ),
)
#instantiation


== Row-Lookup
- Γ ⊢ ρ.l ↓ r with r := τ | ⊥ | ?
- This statement recursively searches rows for a label l
- absent means the label provably does not exist in ρ; T-sel-⊥ still types the
  selection at ★ (soft typing: the checker flags it, the ↯-disjunct of progress
  catches it at runtime)
- unknown means an unconstrained row-var could contain l, so no definite type
  can be derived

#let row_lookup = figure(
  caption: "Row lookup.",
  stack(
    spacing: 15pt,
    align(center, $#type_name("Lookup Result") r ::= τ | ⊥ | ?$),
    flexbox(
      derive("L-ε", (), $Γ ⊢ ε.l ↓ ⊥$),
      derive("L-hit", ($l₁ = l₂$,), $Γ ⊢ (l₁: τ).l₂ ↓ τ$),
      derive("L-miss", ($l₁ ≠ l₂$,), $Γ ⊢ (l₁: τ).l₂ ↓ ⊥$),
      derive("L-α", ($Γ ⊢ α: {ρ}$, $Γ ⊢ ρ.l ↓ r$), $Γ ⊢ α.l ↓ r$),
      derive("L-α-free", ($α ∉ Γ$,), $Γ ⊢ α.l ↓ ?$),
      derive("L-conc-hit", ($Γ ⊢ ρ₁.l ↓ τ$,), $Γ ⊢ (ρ₁ | ρ₂).l ↓ τ$),
      derive(
        "L-conc-skip",
        ($Γ ⊢ ρ₁.l ↓ ⊥$, $Γ ⊢ ρ₂.l ↓ r$),
        $Γ ⊢ (ρ₁ | ρ₂).l ↓ r$,
      ),
      derive("L-conc-★", ($Γ ⊢ ρ₁.l ↓ ?$,), $Γ ⊢ (ρ₁ | ρ₂).l ↓ ?$),
    ),
  ),
)
#row_lookup


== Row-Equivalence
- ρ₁ ≈ ρ₂, lifted to types congruently (τ₁ ≈ τ₂)
- Rows are equal up to reassociation, ε-units and swapping distinct labels
- Deliberately not derivable: swapping equal labels (changes shadowing),
  moving a field past a row-var (its instantiation could shadow it)

#let row_equivalence = figure(
  caption: "Row equivalence.",
  flexbox(
    derive("≈-refl", (), $ρ ≈ ρ$),
    derive("≈-symm", ($ρ₂ ≈ ρ₁$,), $ρ₁ ≈ ρ₂$),
    derive("≈-trans", ($ρ₁ ≈ ρ₂$, $ρ₂ ≈ ρ₃$), $ρ₁ ≈ ρ₃$),
    derive("≈-ext", ($τ₁ ≈ τ₂$,), $(l: τ₁) ≈ (l: τ₂)$),
    derive("≈-conc", ($ρ₁ ≈ ρ₁′$, $ρ₂ ≈ ρ₂′$), $(ρ₁ | ρ₂) ≈ (ρ₁′ | ρ₂′)$),
    derive("≈-assoc", (), $((ρ₁ | ρ₂) | ρ₃) ≈ (ρ₁ | (ρ₂ | ρ₃))$),
    derive("≈-unit-l", (), $(ε | ρ) ≈ ρ$),
    derive("≈-unit-r", (), $(ρ | ε) ≈ ρ$),
    derive("≈-comm", ($l₁ ≠ l₂$,), $(l₁: τ₁ | l₂: τ₂) ≈ (l₂: τ₂ | l₁: τ₁)$),
  ),
)
#row_equivalence
