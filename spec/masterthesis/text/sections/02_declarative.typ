#import "../functions.typ": *


= The Declarative Calculus <declarative>

Next, we show MiniNix, our declarative calculus. @syntax gives the syntax of terms and types. A term can be a constacnt $c$ from a set of constants $𝓒$, a variables $x$, a function definition $(x: e)$, or function eliminition $e₁e₂$. We extend the λ-calculus with records and their operations: A record ${ξ}$ consists of a record body $ξ$ between braces and we add the two operations lookups $e.l$ and record concatenation $(e₁ ‖ e₂)$. We use standart let-bindings without multiple definitions and their recursion behaviour of NixLang to simplify the presentation.

The syntax of types follows. Type variables $α ∈ 𝓐$ are standard and we assume a finite set 𝓑 of base types $𝓫$, together with a total mapping $c ↦ 𝓫_c$ that assigns each constant its base type.The unknown type ★  sits at the top of the type hirarchy and types definitely absent fields. A record type ${ρ}$ is similar to a syntactic record ${ξ}$ since both are formed as inductive trees from an empty base, the binding of a single label and concatenation. A record type also allows row variables $α$ as element, so two sorts `Type` and `Row` are needed to distinguish between type- and row-variables.

Finally, we add a polymorphic type $σ$ that quantifies over monotypes and rows and hosts a set $Q$ of constraints. Each constraint hosts a pending lookup and we call it a _stump_. A _stump_ is the first ingredient of our approach and is used to describe lookups. A  _stump_ ⟨ρ.l ↓ δ⟩ consists of the lookup label $l$ and the row $ρ$ on which the lookup got stuck. The type variable $δ$ is a placeholder for the type the lookup will eventually turn into (if successfull).

#let syntax = figure(
  caption: "The syntax of MiniNix.",
  box(width: 100%, stack(
    spacing: 20pt,
    align(center, flexbox(
      $#type_name("Labels") l ∈ 𝓛$,
      $#type_name("Variables") x ∈ 𝓧$,
      $#type_name("Typ and Row Variables") α ∈ 𝓐$,
      $#type_name("Base types") 𝓫 ∈ 𝓑$,
      $#type_name("Constants") c ∈ 𝓒$,
    )),
    subbox(caption: "Terms")[
      $
        #type_name("Term") e & ::= c | x | (x: e) | e₁e₂ | { ξ } | (e₁ ‖ e₂) | e.l | #b[let] x = e₁ #b[in] e₂ \
        #type_name("Record Body") ξ & ::= ε | l = e | (ξ₁ | ξ₂) \
      $
    ],
    subbox(caption: "Types")[
      $
               #type_name("Sort") κ & ::= "Type" | "Row" \
               #type_name("Type") τ & ::= α | 𝓫 | ★ | τ -> τ | { ρ } \
                #type_name("Row") ρ & ::= ε | α | l: τ | (ρ₁ | ρ₂) \
        #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.l ↓ δ⟩ \
        #type_name("Type Scheme") σ & ::= ∀(macron(α): macron(κ)). Q ⇒ τ | τ \
      $
    ],
  )),
)
#syntax <syntax>


== Typing Rules
#let declarative = figure(
  caption: "Declarative typing rules.",
  flexbox(
    derive("T-cons", (), $Γ ⊢ c: 𝓫_c$),
    derive("T-var", ($x: σ ∈ Γ$, $σ ≥ τ$), $Γ ⊢ x: τ$),
    derive("T-eq", ($Γ ⊢ e: τ₁$, $τ₁ ≈ τ₂$), $Γ ⊢ e: τ₂$),
    derive(
      "T-λ-I",
      ($Γ ⊢ τ₁: "Type"$, $Γ · (x: τ₁) ⊢ e: τ₂$),
      $Γ ⊢ (x: e): τ₁ -> τ₂$,
    ),
    derive("T-λ-E", ($Γ ⊢ e₁: τ₁ -> τ₂$, $Γ ⊢ e₂: τ₁$), $Γ ⊢ e₁e₂: τ₂$),
    derive("T-rec", ($Γ ⊢ ξ: ρ$,), $Γ ⊢ { ξ }: { ρ }$),
    derive("T-ξ-empty", (), $Γ ⊢ ε: ε$),
    derive("T-ξ-field", ($Γ ⊢ e: τ$,), $Γ ⊢ (l = e): (l: τ)$),
    derive(
      "T-conc",
      ($Γ ⊢ e₁: {ρ₁}$, $Γ ⊢ e₂: {ρ₂}$),
      $Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }$,
    ),
    derive(
      "T-ξ-conc",
      ($Γ ⊢ ξ₁: ρ₁$, $Γ ⊢ ξ₂: ρ₂$),
      $Γ ⊢ (ξ₁ | ξ₂): (ρ₁ | ρ₂)$,
    ),
    derive(
      "T-let",
      (
        $Γ ⊢ σ #h(3pt) "ok"$,
        $∀τ₁. #h(0.3em) σ ≥ τ₁ ⟹ Γ ⊢ e₁: τ₁$,
        $∃τ₁. #h(0.3em) σ ≥ τ₁$,
        $Γ · (x: σ) ⊢ e₂: τ₂$,
      ),
      $Γ ⊢ #b[let] x = e₁ #b[in] e₂: τ₂$,
    ),
    derive("T-sel", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ τ$), $Γ ⊢ e.l: τ$),
    derive("T-sel-★", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ #h(0.2em) ?$), $Γ ⊢ e.l: ★$),
    derive("T-sel-⊥", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ ⊥$), $Γ ⊢ e.l: ★$),
    derive("T-★-intro", ($Γ ⊢ e: τ$,), $Γ ⊢ e: ★$),
  ),
)
#declarative <declarative-rules>

@declarative-rules gives the typing rules. T-cons types each constant at its designated base type $b_c$ and T-var reads a type from the type environment, possibly instantiating type schemes using the instantiation relation from @instantiation. T-eq relates two types under row equivalence @row-equivalence and the two T-λ-∗ rules introduce and eliminate functions. T-rec, T-ξ-empty and T-ξ-field type record literals field by field and T-conc combines them; the concatenation of record bodies is handled equally by T-ξ-conc.

T-let differs from Hindley-Milner, because its schemes may carry stumps. Instead of generalizing a type of e₁, it binds x to a scheme σ and constrains σ by three premises. The first premise $Γ ⊢ σ "ok"$ guarantees well-sortedness using the sorting rules @sorting. The second premise ensures that every instance of σ is a typing of e₁, so x may be used at any of them. We call this _instance-closed_. The third makes σ _inhabited_, meaning it has at least one instance. Without inhabitation, [`let x = (3 4) in 5`](evaluates to 5 lule) would get a type while being stuck and it can be uninhabited when two different definitions for δ exist. The last premise types e₂ with x bound to σ.

Our record calculus admits three lookup rules (T-sel, T-sel-★ and T-sel-⊥) for the three possible outcomes of the lookup judgement. T-sel is standard and selects a definite type but T-sel-★ and T-sel-⊥ are new. [Nix is a lazy language and a faulty record lookup might never be evaluated.](cbv-semantic) This is why T-sel-⊥ types a definitely failing lookup at ★ which might seem nonsensical at first. T-sel-★ is the sensible version that types an unknown lookup result at the unknown type. Finally, T-★-intro blurs any concrete type into the unknown.


// #example(name: [Two ways to type a selection at ★])[
//   The selection `{}.l` has type ★ by T-sel-⊥, since $ε.l ↓ ⊥$, and it evaluates to a lookup error by ↯-sel. In a context with $β: "Row"$, the selector `x: x.l` has type ${β} → ★$ by T-sel-★, since $β.l ↓ #h(0.2em) ?$. Applied to `{l = c}` it evaluates to c, applied to `{}` to a lookup error. Both selections receive ★, but T-sel-⊥ marks a selection that fails whenever it is evaluated, while T-sel-★ marks one whose outcome the row does not yet determine.
// ] <ex-sel-star>


== Row Lookup
#let row_lookup = figure(
  caption: "Row lookup.",
  stack(
    spacing: 15pt,
    align(center, $#type_name("Lookup Result") r ::= τ | ⊥ | ?$),
    flexbox(
      derive("L-ε", (), $ε.l ↓ ⊥$),
      derive("L-hit", ($l₁ = l₂$,), $(l₁: τ).l₂ ↓ τ$),
      derive("L-miss", ($l₁ ≠ l₂$,), $(l₁: τ).l₂ ↓ ⊥$),
      derive("L-var", (), $α.l ↓ #h(0.2em) ?$),
      derive("L-conc-hit", ($ρ₁.l ↓ τ$,), $(ρ₁ | ρ₂).l ↓ τ$),
      derive(
        "L-conc-skip",
        ($ρ₁.l ↓ ⊥$, $ρ₂.l ↓ r$),
        $(ρ₁ | ρ₂).l ↓ r$,
      ),
      derive("L-conc-?", ($ρ₁.l ↓ #h(0.2em) ?$,), $(ρ₁ | ρ₂).l ↓ #h(0.2em) ?$),
    ),
  ),
)
#row_lookup <row-lookup>

A row type is traversed by a dedicated lookup judgement ⟨ρ.l ↓ r⟩ read as "the lookup of label $l$ in row $ρ$ has result $r$" with $r := τ | ⊥ | #h(0.2em) ?$. The judgement answers with either a definite type τ, definite absence ⊥ or the unknown marker ? when a row variable renders shadowing unclear. L-ε, L-hit and L-miss form the base cases for row-var free lookups and L-var returns the unknown marker when a row variable is encountered. The concatenation rules #smallcaps("L-conc-∗") bubble up a definite result and preserve the right-precedence semantics of asymmetric concatenation.

A lookup can advance further into a row as soon as a blocking row variable α is solved by ρ' during unification $α := ρ'$. An unknown lookup-result $?$ can then become definite if the "unlocked" row behind it contains the searched label or it blocks again on a later row variable. The addition of an unknown result makes our lookup relation total:

=== Properties of the Lookup Relation <lookup-metatheory>

#lemma(name: [Determinism and totality], lean: "lookup_total, LookupQ.det")[
  For every row ρ and label l there is exactly one lookup result r with $ρ.l ↓ r$.
] <lem-lookup-det>

#proof[By induction on ρ: the shape of ρ, the comparison of the labels and, for a concatenation, the result for ρ₁ select exactly one rule.]

A definite derivation never uses L-var, so no substitution can change its result; only the unknown result ? can be refined:

#lemma(name: [Stability under substitution], lean: "lookup_applySubst")[
  If $ρ.l ↓ r$ and $r ≠ #h(0.2em) ?$, then $(θ ρ).l ↓ θ r$ for every substitution θ.
] <lem-lookup-stable>

#proof[By induction on the derivation of $ρ.l ↓ r$. A definite derivation never uses L-var, so it never reaches a row-variable, and θ changes no label it inspects.]


== Row Equivalence
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
    derive("≈-rcd", ($ρ₁ ≈ ρ₂$,), ${ρ₁} ≈ {ρ₂}$),
    derive("≈-fn", ($τ₁ ≈ τ₁′$, $τ₂ ≈ τ₂′$), $(τ₁ -> τ₂) ≈ (τ₁′ -> τ₂′)$),
  ),
)
#row_equivalence <row-equivalence>

The order of fields in a record carries no meaning: the records `{ a = 1; b = 2; }` and `{ b = 2; a = 1; }` behave identically, so their rows $(a: τ | b: τ′)$ and $(b: τ′ | a: τ)$ should be equivalent. Row equivalence ≈ permits this by allowing shadowing-preserving reordering of fields using controlled swaps of fields. @row-equivalence defines ≈ as the least congruence that makes concatenation associative with unit ε and that swaps adjacent fields with distinct labels (≈-comm). The base types, ★ and type variables are equivalent only to themselves, and ≈ is a congruence below records and arrows (≈-rcd and ≈-fn). ≈-ext handles singular fields and ≈-conc the concatenation of two rows.


== Sorts
#let sorting = figure(
  caption: "Sorting.",
  flexbox(
    derive("S-base", (), $Γ ⊢ 𝓫: "Type"$),
    derive("S-★", (), $Γ ⊢ ★: "Type"$),
    derive("S-var", ($α: "Type" ∈ Γ$,), $Γ ⊢ α: "Type"$),
    derive(
      "S-fn",
      ($Γ ⊢ τ₁: "Type"$, $Γ ⊢ τ₂: "Type"$),
      $Γ ⊢ τ₁ -> τ₂: "Type"$,
    ),
    derive("S-rcd", ($Γ ⊢ ρ: "Row"$,), $Γ ⊢ {ρ}: "Type"$),
    derive("S-ρ-var", ($α: "Row" ∈ Γ$,), $Γ ⊢ α: "Row"$),
    derive("S-ε", (), $Γ ⊢ ε: "Row"$),
    derive("S-field", ($Γ ⊢ τ: "Type"$,), $Γ ⊢ (l: τ): "Row"$),
    derive(
      "S-conc",
      ($Γ ⊢ ρ₁: "Row"$, $Γ ⊢ ρ₂: "Row"$),
      $Γ ⊢ (ρ₁ | ρ₂): "Row"$,
    ),
    derive(
      "S-scheme",
      (
        $Γ · (macron(α): macron(κ)) ⊢ τ: "Type"$,
        $∀⟨ρ.l ↓ δ⟩ ∈ Q. #h(0.3em) Γ · (macron(α): macron(κ)) ⊢ ρ: "Row" and (macron(α): macron(κ)) ⊢ δ: "Type"$,
      ),
      $Γ ⊢ (∀(macron(α): macron(κ)). Q ⇒ τ) #h(3pt) "ok"$,
    ),
  ),
)
#sorting <sorting>

@sorting gives the sorting rules of our calculus. We only differentiate between `Type` and `Row`. Sorts are needed only for variables: every other type or row is recognisable by its form, but a variable can stand for either, so Γ records its sort. The first two rules sort the base, S-var the variables and S-fn types functions by recursing into the domain and codomain. S-rcd shows how a row constitutes a type by placing it between braces, S-ρ-var is the equivalent of S-var for rows and S-ε, S-field and S-conc sort rows. S-scheme guarantees well-formedness of type schemes with two premises: The first fixes the parameters' sorts and the second premise fixes the sorts in stumps. The result δ of a stump is sorted under the parameters $macron(α)$ alone, so it mentions no variable of Γ. First-class labels add a `Label` sort and change nothing else about the discipline (@sec-fc-labels).


== Qualified Schemes <qualified-schemes>
#let instantiation = figure(
  caption: "Instantiation.",
  flexbox(
    derive(
      "I-inst",
      (
        $Γ ⊢ θ: (macron(α): macron(κ))$,
        $∀q ∈ Q. #h(0.3em) θ ⊨ q$,
      ),
      $(∀(macron(α): macron(κ)). Q ⇒ τ) ≥ θ τ$,
    ),
    derive("D-hit", ($(θ ρ).l ↓ τ$, $θ δ ≈ τ$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-⊥", ($(θ ρ).l ↓ ⊥$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-?", ($(θ ρ).l ↓ #h(0.2em) ?$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
  ),
)
#instantiation <instantiation>

@instantiation defines the instance relation $σ ≥ τ$, used by T-var. The first premise of I-inst makes θ respect the sorts and the second is a _discharge_. [A discharge $θ ⊨ q$ replays every stump at the instance: the lookup is performed on θρ, and θδ is pinned to its verdict such that both coincide. The replay allows derivations up to ≈ by D-hit, D-⊥ and D-\?. D-hit asks for $θ δ ≈ τ$ rather than equality. This loses nothing, since T-eq closes every typing under ≈.](give some more motivation here)

#example(name: [Instances of selQ])[
  Consider $"selQ" = ∀(β: "Row", δ: "Type"). ⟨β.l ↓ δ⟩ ⇒ {β} → δ$.
  - $θ = [β ≔ (l: 𝓫), δ ≔ 𝓫]$ discharges the stump by D-hit, since $(l: 𝓫).l ↓ 𝓫$, so ${(l: 𝓫)} → 𝓫$ is an instance.
  - $θ = [β ≔ ε, δ ≔ ★]$ discharges it by D-⊥, since $ε.l ↓ ⊥$, so ${ε} → ★$ is an instance.
  - $θ = [β ≔ ε, δ ≔ {ε}]$ violates D-⊥, which demands $θ δ = ★$. Hence ${ε} → {ε}$ is not an instance, although it is an instance of the plain scheme $∀(β: "Row", δ: "Type"). {β} → δ$.
] <ex-selq-inst>

Without stumps (Q = ε), our instantiation is plain Hindley-Milner instantiation, but with qualified schemes, a scheme can be uninhabited because two stumps may pin the same δ to different verdicts. This is why T-let demands inhabitation explicitly.

#example(name: [An uninhabited scheme])[
  The scheme $∀(δ: "Type"). ⟨(l: 𝓫).l ↓ δ⟩, ⟨ε.l ↓ δ⟩ ⇒ δ$ is well-sorted but has no instance. The first stump hits, so D-hit demands $θ δ ≈ 𝓫$. The second misses, so D-⊥ demands $θ δ = ★$. Since ★ is equivalent only to itself, no θ satisfies both.
] <ex-uninhabited>


== Operational Semantics
#let semantics = figure(
  caption: "Operational semantics.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Values and Evaluation Contexts")[
      $
        #type_name("Value") v & ::= c | (x: e) | { ξ } \
        #type_name("Context") E & ::= [·] | E e | v E | E.l | (E ‖ e) | (v ‖ E) | #b[let] x = E #b[in] e \
      $
    ],
    subbox(caption: "Reduction")[
      #flexbox(
        derive("E-β", (), $(x: e) v → e[x ≔ v]$),
        derive("E-let", (), $#b[let] x = v #b[in] e → e[x ≔ v]$),
        derive("E-sel", ($ξ(l) = e$,), ${ξ}.l → e$),
        derive("E-conc", (), ${ξ₁} ‖ {ξ₂} → {ξ₂ | ξ₁}$),
        derive("E-ctx", ($e → e′$,), $E[e] → E[e′]$),
      )
    ],
    subbox(caption: "Lookup Error")[
      #flexbox(
        derive("↯-sel", ($ξ(l) "undefined"$,), ${ξ}.l ↯$),
        derive("↯-ctx", ($e ↯$,), $E[e] ↯$),
      )
    ],
  )),
)
#semantics <semantics>

@semantics gives the call-by-value small-step reduction of the mechanization. The definitions are entirely standard with one subtlety: a record literal is a value and its fields are only evaluated when they are selected. NixLang is a lot more complicated, as Broekhoff and Krebbers @verified show by giving a small-step semantics based on delayed substitution that faithfully tracks the overwriting behaviour of `with e₁; e₂` and laziness in records, let-statements and operators. In this thesis, we concern ourselves with type inference and limit the operational semantics to a bare minimum. $ξ(l)$ denotes the leftmost binding of l in ξ, so selection agrees with ↓, and E-conc places the right operand first, as T-conc does. The judgement $e ↯$ is a _lookup error_: a selection reaches a record literal that lacks the selected label. First-class labels extends this with another error case (@sec-fc-labels).
