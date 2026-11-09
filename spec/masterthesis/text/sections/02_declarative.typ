#import "../functions.typ": *


= The Declarative Calculus <declarative>

MiniNix adds records and let-bindings to a λ-calculus with constants (@syntax). A record literal ${ξ}$ wraps a _record body_ $ξ$ with braces. The record body is built from the empty body $ε$, single bindings $l = e$ and concatenation $(ξ₁ | ξ₂)$. A body may bind the same label multiple times and selection $e.l$ takes the leftmost occurrence (@semantics). MiniNix has two concatenation forms, one for the bindings of one literal $(ξ₁ | ξ₂)$ and asymmetric record concatenation $(e₁ ‖ e₂)$ which combines two records and corresponds to `//` in NixLang. A let-binding binds a single variable and is not recursive, unlike in NixLang (@outlook).

Types describe terms, and rows describe record bodies. A type is a variable $α$, a base type $𝓫$ from a finite set $𝓑$, the unknown type ★, a function type $τ → τ′$ or a record type ${ρ}$. A total mapping $c ↦ 𝓫_c$ assigns each constant its base type. The unknown type ★ is the type of what the analysis cannot decide. It has no elimination form, so a term of type ★ can be passed around but not consumed. Unlike the dynamic type of gradual typing @gradual_siek, ★ is not consistent with every type. A record type ${ρ}$ encloses a row $ρ$, which mirrors a record body: It is built from the empty row $ε$, a single field $l: τ$, the concatenation $(ρ₁ | ρ₂)$ of two rows, or a row-variable $α$. Types and rows draw their variables from one set $𝓐$, and the sorts $"Type"$ and $"Row"$ tell type-variables and row-variables apart (@sorting). Constraints $Q$ and type schemes $σ$ follow in @qualified-schemes. A type environment $Γ$ assigns type schemes to term variables and sorts to type- and row-variables, and $Γ · (x: σ)$ and $Γ · (α: κ)$ extend $Γ$ by single bindings.

#let syntax = figure(
  caption: "The syntax of MiniNix.",
  box(width: 100%, stack(
    spacing: 20pt,
    align(center, flexbox(
      $#type_name("Constants") c ∈ 𝓒$,
      $#type_name("Variables") x ∈ 𝓧$,
      $#type_name("Type and Row Variables") α ∈ 𝓐$,
      $#type_name("Labels") l ∈ 𝓛$,
      $#type_name("Base types") 𝓫 ∈ 𝓑$,
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
                 #type_name("Type") τ, δ & ::= α | 𝓫 | ★ | τ -> τ | { ρ } \
                     #type_name("Row") ρ & ::= ε | α | l: τ | (ρ₁ | ρ₂) \
             #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.l ↓ δ⟩ \
             #type_name("Type Scheme") σ & ::= ∀(macron(α): macron(κ)). Q ⇒ τ | τ \
        #type_name("Type Environment") Γ & ::= ∅ | Γ · (x: σ) | Γ · (α: κ) \
      $
    ],
  )),
)
#syntax <syntax>


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

Sorting (@sorting) ensures that type-variables and row-variables only occur where they are expected. Only variables are stored in Γ because all other forms can be determined by their structure. The first two rules sort the base types, S-var the variables and S-fn sorts functions by recursing into the domain and codomain. S-rcd shows how a row constitutes a type by placing it between braces, S-ρ-var is the equivalent of S-var for rows and S-ε, S-field and S-conc sort rows. S-scheme guarantees well-sortedness of type schemes with two premises: The first sorts $τ$ under the parameters and the second premise fixes the sorts in stumps (@qualified-schemes). The result $δ$ of a stump is sorted only under the parameters $macron(α)$ because it mentions no variable of $Γ$. First-class labels add another $"Label"$-sort but change nothing else about the discipline (@fc-labels).


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

Selection is typed by the lookup judgement $ρ.l ↓ r$ (@row-lookup), read as "the lookup of label $l$ in row $ρ$ has result $r$". The result is a definite type $τ$, definite absence $⊥$, or the undecided result $?$ when the lookup reaches a row-variable before it finds $l$. L-ε, L-hit and L-miss form the base cases for row-var free lookups and L-var returns the undecided result $?$ when a row variable is encountered. The concatenation rules #smallcaps("L-conc-∗") bubble up a definite result and search the left operand first. Together with T-conc, which places the right operand of $‖$ first, this gives the right-precedence of concatenation.

A lookup can advance further into a row as soon as a blocking row variable $α$ is solved by substitution. An undecided lookup result $?$ can then become definite if the "unlocked" row behind it contains the searched label or it blocks again on a later row variable. The addition of an undecided result makes our lookup relation total.

=== Properties of the Lookup Relation <lookup-metatheory>

#lemma(name: [Determinism and totality], lean: "lookup_total, LookupQ.det")[
  For every row $ρ$ and label $l$ there is exactly one lookup result $r$ with $ρ.l ↓ r$.
] <lem-lookup-det>

#proof[By induction on $ρ$: the shape of $ρ$, the comparison of the labels and, for a concatenation, the result for $ρ₁$ select exactly one rule.]

A definite derivation never uses L-var, so no substitution can change its result. Only the undecided result $?$ can be refined:

#lemma(name: [Stability under substitution], lean: "lookup_applySubst")[
  If $ρ.l ↓ r$ and $r ≠ #h(0.2em) ?$, then $(θ ρ).l ↓ θ r$ for every substitution $θ$, where $θ ⊥ = ⊥$.
] <lem-lookup-stable>

#proof[By induction on the derivation of $ρ.l ↓ r$. A definite derivation never uses L-var, so it never reaches a row-variable, and $θ$ changes no label it inspects.]


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

The order of fields with distinct labels carries no meaning: both ${a = 1 | b = 2}$ and ${b = 2 | a = 1}$ behave identically, so their rows $(a: τ | b: τ′)$ and $(b: τ′ | a: τ)$ should be equivalent. Row equivalence $≈$ permits this by allowing shadowing-preserving reordering of fields using controlled swaps of fields. @row-equivalence defines row equivalence ($≈$) as the least congruence that makes concatenation associative with unit $ε$ and that swaps adjacent fields only when labels are distinct (≈-comm), so $≈$ never reorders what a lookup can observe. For instance, the same-label swap: $(l: 𝓫 | l: ★).l ↓ 𝓫$ but $(l: ★ | l: 𝓫).l ↓ ★$, and moving a label in front of a row variable: $(l: 𝓫 | α).l ↓ 𝓫$ but $(α | l: 𝓫).l ↓ #h(0.2em) ?$ are both forbidden. The base types, ★ and type-variables are equivalent only to themselves.

#lemma(name: [Lookup respects $≈$], lean: "lookup_equiv")[
  If $ρ₁ ≈ ρ₂$ and $ρ₁.l ↓ r$, then $ρ₂.l ↓ r′$ for some $r′ ≈ r$, where $≈$ extends to lookup results by $⊥ ≈ ⊥$ and $? ≈ ?$.
] <lem-lookup-equiv>

#proof[By induction on the derivation of $ρ₁ ≈ ρ₂$, for both directions at once, since ≈-symm swaps them. In the case ≈-comm, $l₁ ≠ l₂$ ensures that at most one of the two fields carries $l$.]


== Qualified Schemes <qualified-schemes>

A type scheme $σ$ quantifies over type- and row-variables and carries a set $Q$ of constraints. Each constraint is a pending lookup, which we call a _stump_. A stump $⟨ρ.l ↓ δ⟩$ consists of the row $ρ$ on which the lookup is blocked, the label $l$ and the type $δ$ that names the result. When inference parks a stump, $δ$ is a fresh type-variable that unification may solve later (@inference). Stumps are needed because the type of a selection depends on the row through the lookup relation, and no type constructor expresses this dependency. The selector $x: x.l$ has the typings ${l: {ε}} → {ε}$ and ${ε} → ★$. A plain scheme with both instances needs a variable as its result type, and then it also admits ${ε} → {ε}$, which is no typing (@thm-no-plain). A stump connects the function's result to the lookup and every instance can then fix $δ$ to its own result (@instantiation).

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

@instantiation defines the instance relation $σ ≥ τ$, used by T-var. The first premise of I-inst requires $θ$ to map each parameter $α_i$ to a type or row of sort $κ_i$, and the second _discharges_ every stump at the instance. A discharge $θ ⊨ ⟨ρ.l ↓ δ⟩$ performs the lookup on $θ ρ$ and fixes $θ δ$ as the selection rules would: D-hit corresponds to T-sel, D-⊥ to T-sel-⊥ and D-\? to T-sel-★. An instance thus gives a selection the type it would receive if it were typed at $θ ρ$ directly. D-hit asks for $θ δ ≈ τ$ rather than equality to accept more typings, but this is okay because T-eq closes every typing under $≈$.

Without stumps ($Q = ε$), I-inst is plain Hindley-Milner instantiation. With stumps, a scheme can be uninhabited, that is, have no instance: in $∀(δ: "Type"). ⟨(l: 𝓫).l ↓ δ⟩, ⟨ε.l ↓ δ⟩ ⇒ δ$, the first stump hits and D-hit demands $θ δ ≈ 𝓫$, while the second misses and D-⊥ demands $θ δ = ★$. Since ★ is equivalent only to itself, no $θ$ satisfies both.

#example(name: [Instances of $"selQ"$])[
  Consider $"selQ" = ∀(β: "Row", δ: "Type"). ⟨β.l ↓ δ⟩ ⇒ {β} → δ$.
  - $θ = [β ≔ (l: 𝓫), δ ≔ 𝓫]$ discharges the stump by D-hit, since $(l: 𝓫).l ↓ 𝓫$, so ${l: 𝓫} → 𝓫$ is an instance.
  - $θ = [β ≔ ε, δ ≔ ★]$ discharges it by D-⊥, since $ε.l ↓ ⊥$, so ${ε} → ★$ is an instance.
  - $θ = [β ≔ ε, δ ≔ {ε}]$ violates D-⊥, which demands $θ δ = ★$. Hence ${ε} → {ε}$ is not an instance, which is correct.
] <ex-selq-inst>


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

Most rules of @declarative-rules are standard for a λ-calculus with records. T-conc differs only in that it places the right operand first, so its fields shadow those of the left operand under our lookup relation. T-var instantiates a type scheme using the instantiation relation from @instantiation, and T-eq closes typing under row equivalence (@row-equivalence).

T-let departs from Hindley-Milner, which types $e₁$ once and generalizes the resulting typing. This cannot yield the scheme $"selQ"$ of $x: x.l$ (@ex-selq-inst). Its body ${β} → δ$ is no typing of $x: x.l$ because the lookup of $l$ in $β$ answers $?$ so T-sel-★ gives the result ★. The stump $⟨β.l ↓ δ⟩$ closes this gap: every instance discharges it and fixes $θ δ$ to the result of the lookup on $θ β$ (@instantiation), so every instance of $"selQ"$ is a typing although its body is not. A single typing carries no stumps, so generalization cannot produce them. T-let therefore takes $σ$ with its stumps as given and constrains it directly. The first premise guarantees well-sortedness (@sorting). The second makes $σ$ _instance-closed_, meaning every instance of $σ$ is a typing of $e₁$, so $x$ may be used at any of them. The stumps restrict this premise to the instances that discharge them: for $"selQ"$, D-⊥ excludes ${ε} → {ε}$, which is no typing of $x: x.l$. In Hindley-Milner, instance-closure follows from the typing of $e₁$ by the substitution lemma. T-let states it as a premise instead. The last premise types $e₂$ with $x$ bound to $σ$.

Three selection rules cover the three results of the lookup judgement. T-sel is standard: it types a selection at the definite type that the lookup finds. T-sel-⊥ types a selection that definitely fails at ★. NixLang is lazy, so the selection may never be evaluated, and rejecting the program would reject working code, so we accept this code under ★. T-sel-★ types a selection with an undecided result at the unknown type and T-★-intro blurs any type to ★.


== Operational Semantics
#let semantics = figure(
  caption: "Operational semantics.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Values and Evaluation Contexts")[
      $
          #type_name("Value") v & ::= c | (x: e) | { ξ } \
        #type_name("Context") E & ::= [·] | E e | E.l | (E ‖ e) | (v ‖ E) \
      $
    ],
    subbox(caption: "Reduction")[
      #flexbox(
        derive("E-β", (), $(x: e) e′ → e[x ≔ e′]$),
        derive("E-let", (), $#b[let] x = e′ #b[in] e → e[x ≔ e′]$),
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

Type safety (@type-safety) is stated over the call-by-name small-step reduction of @semantics. A record literal is a value, so its fields are evaluated only when they are selected, and E-β and E-let substitute arguments and bindings unevaluated. Reduction does not go under a binder, so in a closed program the substituted term is closed and no capture arises. $ξ(l)$ denotes the leftmost binding of $l$ in $ξ$, so selection agrees with $↓$, and E-conc places the right operand first, as T-conc does. The judgement $e ↯$ is a lookup error: a selection reaches a record literal that lacks the selected label. The reduction is deliberately minimal, since this thesis concerns type inference. Broekhoff and Krebbers @verified give a small-step semantics of NixLang, based on delayed substitution, that tracks the overwriting behaviour of `with e₁; e₂` and laziness in records, let-expressions and operators faithfully.
