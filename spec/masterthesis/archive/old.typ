#import "./functions.typ": *
#set document(
  title: "A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  description: "Masterthesis about a Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)



#let hidden = [
  == Limitations
  - We currently need _closedness_ in the proofs, which does not hold due to `with; e`
  - There is no negative information in our typesystem.


  == Goal
  > I want to create a typesystem that handles Nix as best as possible. It should be efficiently computable and have no "breaking" points. Meaning, there is nothing in it that makes it immediately unfeasible for Nix. This is why we need a soft-typing type as well as row- and label-variables. The result should be efficiently computable.
]


= A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix

Asymmetric record concatenation with left-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that can not be statically typed: The expression `a: b: (a ‖ b).l` concatenates two type variables but can not be given a type without instantiating at least b, because of field-precedence and shadowing behaviour.
We propose a novel _soft type system_ based upon the work of Paszke&Xie with scoped-records, row-variables, asymmetric record concatenation, let-polymorphism, row-equivalence and an unknown type that delineates the exact cases in which the so called wand-ambiguity is hit. Using this soft-typing feature, our calculus positions itself between existing record literature, exploring a new approach. We mechanically prove _type safety_ of the declarative system in Lean and give an sound, efficient but incomplete unification algorithm for a minimal calculus.

#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

= A Note about Nix
> This section motivates our work in regard to practical application, also the nix language features are guiding the features we are exposing.


```nix
self: super: { foo = super.foo // { meta = …; }; }   # overlay
mkDerivation (args // { buildInputs = …; })          # the callPackage idiom
{ config, lib, ... }: { … }                          # module system, mkMerge```


┌────────────────────────────────────────┬──────────────────────────────────┬──────────────────────────────────┐
│                                        │            difficulty            │          what it forces          │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ // with collision                      │ precedence is a runtime fact     │ scoped rows, total concat        │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ .${e}, ?, getAttr                      │ labels are values                │ label sort / FC labels           │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ with e; body                           │ scope is a runtime value         │ name resolution can be ★         │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ rec, fixpoints, overlays               │ recursive rows                   │ occurs class, not a technicality │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ laziness                               │ non-closedness, unforced errors  │ soft, per-binding ★              │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ attrNames, removeAttrs, intersectAttrs │ need negative + label-level info │ best-effort ★ signatures         │
├────────────────────────────────────────┼──────────────────────────────────┼──────────────────────────────────┤
│ no annotations anywhere                │ everything inferred              │ HM, whole-fixpoint scale         │
└────────────────────────────────────────┴──────────────────────────────────┴──────────────────────────────────┘

NixLang is the fundamental language of one of the largest bodies of untyped functional code in existence and a language that extends beyond the usual λ-calculus features. The foundational core of the language are records, with a garmut of language constructs and builtin functions to create, change and deconstruct these. Two features make static typing notably hard: *first-class labels* and the *asymmetric record concatenation* operation with only a few typesystems in existence that supported these features.


NixLang is the motivation for our work and guiding principle for the calculus we are concerned with. NixLang powers the most up-to-date package repository with more than 100.000 packages, continuously checked, updated and rolled out from one central repository: nixpkgs on Github. All of nix' code including the standart library, module system, and operating system NixOs roots in a single file at the root of that repository.

// Gradual typing is semantically unavailable
From this problem surface we derive two constraints for our work: First, from our assesment, it is unrealistic to force breaking changes in a software project of this size so the requirements needed to adopt the typesystem should be as benign as possible. This is why gradual typesystems @gradual_siek @gradual_tobin @agt that typically provide a surface and (¿) language that inserts casts is not an option. Instead, we take a soft-typing @soft_typing @soft_typing approach and admit an unknown type ★, similar to the undefined type of typescript.

Secondly, we want to create a typesystem that is applicable and thus needs *effective* type-inference. This rules out another class of inference approaches such as ROSE @rose and it's descendants @extensible_rec_funcs @generic_with_extensible as these provide no efficient inference algorithm.


// This gives reason for why we need a soft typing system, maybe it is misplaced)
// we might have to remove this section and merge it into `typesystem`)
#[
  A complete typesystem for Nix is hindered by impurities (in an otherwise pure language) that can poison typeability. Using first-class labels and the impure builtin `builtins.currentTime`, it is possible to form an expression that looks up a record field based on the wall-clock time:

  ```nix
  { before = "moin"; after = 0; }.${if builtins.currentTime < 1767225600 then "before" else "after"}
  ```

  The type of this selection depends on the moment of evaluation, so this is an obviously untypable operation: typing it would predict the future.
]

The design constraints for our Nix typesystem are as follows: Full record calculus strength with first-class labels and the problematic asymmetric concat operation are essential to provide usable type-inference. Computability is an essential design constraint as backtracking would render type inference unusably slow. Lastly, a typesystem is needed that admits unavoidable uncertainty with an unknown type ★ similar to the one used in TypeScript or occurrence typing spearheaded by Castagna.

// (note: maybe the section can have *subheadings* for the three main constraints?)


= Motivation
// > Explain the typesystem and why we chose its features the way we did
//
// TODO
// - Note that we don't have width-subtyping (same problem as remy, can gobble fields)
// - Section about Absence in general (Parreaux, Castagna, etc.)
//
// Structure
// - Row-poly
// - Plottwist: Constraint systems
// - (Giuseppe & Parreaux) ?
//
// Reason for rows: Concatenation & FC-Labels "obvious"
// Reason for rows: Good examples from Morris & (P&X)
//
// Reason for rows: P&X show efficient unification
// Reason against Parreaux: No real concatenation, co-complete…?
// Reason against Castagna: No free concatenation
// We loose: The full type-connective algebra properties
//
//
//

Asymmetric record concatenation is a central problem that many record calculi address. Its set-or-update behaviour in combination with polymorphism makes tracking of fields extremely hard, wording:[and multiple approaches have been suggested that weight benefits againts drawbacks]. *Row polymorphism* is a method to track positive information of records (…)

// longer part about lacks predicates?
Without negative information and width-subtyping, overwriting fields is an unrecoverable operation, since width-subtyping can remove a field `a: {l: τ} -> a: {}` without a trace, and concatenating such a record with b: { l: τ'} can not be clearly resolved due to shadowing.

Lacks-predicates @? allow to reason about negative information, but are verbose¿ Another option are stronger type systems like the one by Ohori @ohori1995polymorphic or the line of work of Morris @extensible_rec_funcs @rose @another_inference that faithfully track positive and negative information with constraint or dependent types. But both come at the cost of computability. The systems of Morris are theoretically astonishing and even though ROSE @rose is HM, it gives all the heavy lifting the row theory left open. Recent work @another_inference reiterates on type inference but is unable to improve the situation. The systems of Ohori add the full dependent-type complexity.

// note: should we keep the arguing that ★ can improve even though we don't do it during unification?
Our approach, RowNix, positions itself in the middle of both extremes and admits the uncertainty that different kinds of operations can induce by using an *unknown type* that directly marks uncertainty. Our motivating example admits such a type [`a: b: (a ‖ b).l :: α → β → ★`]¡ because it is statically not possible to determine the return type. By surrendering to some form of uncertainty we can adjust the unification algorithm of Paszke&Xie to a system that can be computed efficiently¿.

// note: I think we should only talk about the three-way lookup relation in explicit system as it is not really a notable contribution
To retain as much information as possible, we use scoped rows and a concatenation operation that glues together two records without simplifying either side directly. This, in combination with our three-way lookup relation allows us to faithfully track field presence, absence and ambiguity.


_Contributions_ We contribute the following items:

1. *A best-effort lookup relation.* Our lookup relation `Γ ⊢ ρ.l ↓ r` extends the usual positive and negative results of lookup to a three-way result (τ | ⊥ | ?).
3. *Mechanized type safety.* We prove _progress_ under erroring terms ↯ and _preservation_ in Lean.
4. *An algorithmic system.* We give an efficient, sound but incomplete unification algorithm for the minimal calculus, extending the algorithm of Paszke&Xie to rows containing the unknown type.


= Informal Description of the TS and it's tricks
- Row-equality up to type-vars
- We don't need the tail check :)
- Explain why ★ is covariant in all positions for functions

// Structure
// - Show the typesystem P1?
// - I don't think that is really showable without the actual introduction
// -


The concatenation inside the example `a: ({l: τ} ‖ a).l` will produce a row `(α | l: τ)` with a type variable for the function argument. Upon instantiation at the application site, the row-variable can be eliminated such that the lookup relation that was previously stuck before finding a field can advance further into the row, find the l: τ binding, and return a proper type τ.


= Minimal Calculus
> _Functions, scoped records, record concat, row-vars, let-poly_

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
               #type_name("Sort") κ & ::= "Type" | "Row" \
        #type_name("Type Scheme") σ & ::= ∀(macron(α): macron(κ)). τ | τ \
      $
    ],
  )),
)
#syntax <syntax>

@syntax shows the term- and type-syntax of a standard lambda-calculus extended with records, record-concatenation and let-polymorphism. Functions use the unusual syntax (x: e) where x is the variable to be replaced in the function body e. This distinction is chosen because it's Nix' syntax for functions. We admit a finite set 𝓒 of constants $c ∈ 𝓒$ that can be typed by basetypes 𝓫 from the finite set of basetypes 𝓑 and require that 𝓑 has at least the types needed to type every constant such that `c: 𝓫_c` is a complete mapping. We admit an "unknown" ★ type for our soft-typing system that can be used to type expressions the typesystem can not reason about. Term-rows ${ξ}$ and row-types ${ρ}$ are both trees that shows their similarity. As per the usual, we stratify our typesystem with a polymorphic σ-type that subsumes the monomorphic types τ to sidestep set-paradoxes.


== Sorts
#let sorting = figure(
  caption: "Sorting.",
  flexbox(
    derive("S-var", ($α: "Type" ∈ Γ$,), $Γ ⊢ α: "Type"$),
    derive("S-base", (), $Γ ⊢ 𝓫: "Type"$),
    derive("S-★", (), $Γ ⊢ ★: "Type"$),
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
      ($Γ · (macron(α): macron(κ)) ⊢ τ: "Type"$,),
      $Γ ⊢ (∀(macron(α): macron(κ)). τ) #h(3pt) "ok"$,
    ),
  ),
)
#sorting <sorting>

@sorting classifies every type-level phrase as a `Type` or a `Row`. The judgement is almost content-free: τ and ρ are already disjoint syntactic categories, so every closed phrase wears its sort on its face and S-base through S-conc merely walk the grammar. The work is done by S-var and S-ρ-var, which read a *variable*'s sort off the context — Γ records a sort for every type variable in scope, written $α: κ ∈ Γ$. Sorting is therefore not a type system for types but a partition of the grammar: no arrow sort, no sort variables, nothing to infer.

This is deliberately less than Paszke and Xie have. Their kinds are $κ ::= ★ | κ₁ → κ₂ | "Label" | "Row"$, and they need the arrow because their types include type application, first-class rows and label singletons; ours include none of these, so the arrow kind and its application rule would be borrowed machinery. We also avoid their notation, in which ★ _is_ the type kind — here ★ is the unknown type, and the sorts are named instead. A `Label` sort is absent for the same reason: the minimal calculus has concrete labels only. First-class labels, which Nix needs for `e.${e'}`, add a third sort and change nothing else about the discipline.

Two typing rules acquire a premise. T-λ-I is the only rule that conjures a type from nothing, so it checks $Γ ⊢ τ₁: "Type"$, and T-let is the only one that conjures a scheme, so its generalization records the sorts $macron(κ)$ that Γ already assigns to the variables it abstracts. Every other rule's types are fixed by its premises and are well-sorted whenever they are.


= Declarative

// TODO:
// - small section why plain schemes can not result in a principaled ts
// - Then update the rules for qualified schemes

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
    derive(
      "T-let",
      (
        $Γ ⊢ e₁: τ₁$,
        $macron(α) = "ftv"(τ₁) ∖ "ftv"(Γ)$,
        $macron(κ) = Γ(macron(α))$,
        $Γ · (x: ∀(macron(α): macron(κ)). τ₁) ⊢ e₂: τ₂$,
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
#declarative <declarative>

The declarative system's typing rules follow the standard λ-calculus rules. T-cons is used to type the set of constants of the language with their respective type $𝓫_c$. T-var not only looks up variables in the context Γ, but also instantiates polymorphic types using the instantiation rules from @instantiation discussed in the following section. T-eq equates types equal up to the row-equivalence relation from @row-equivalence. T-conc concatenates two row types by concatenating their type representation and T-sel types record lookups by lifting the hard work to the row-lookup relation, defined in @row-lookup.


T-sel-★ and T-sel-⊥ is needed to type otherwise stuck terms and T-★-intro to blur a type into the unknown. The rules T-rec, T-ξ-empty, T-ξ-field and T-ξ-conc type record literals. (…)


== Instantiation
#let instantiation = figure(
  caption: "Instantiation.",
  flexbox(
    derive(
      "I-inst",
      ($Γ ⊢ θ: (macron(α): macron(κ))$,),
      $(∀(macron(α): macron(κ)). τ) ≥ θ τ$,
    ),
  ),
)
#instantiation <instantiation>

@instantiation defines the instance relation $σ ≥ τ$, read »τ is an instance of σ«. It is consumed by T-var, the only rule that ever opens a scheme, and it discharges every quantifier at once: the side condition $Γ ⊢ θ: (macron(α): macron(κ))$ says that θ is the identity outside $macron(α)$ and sends each $α: κ$ to a phrase of sort κ. Instantiation is _predicative_ — the witnesses are monotypes and rows, never schemes — which is what keeps the system a rank-1 HM calculus.

Sorting is what makes this a *single* rule. Without the annotation a quantifier could be discharged with a type or with a row, and the relation needed two rules with identical conclusions differing only in the witness's sort, with nothing to say which was intended and no account at all of a variable standing at positions of both kinds. The sort settles it at the binder: $α: "Row"$ takes a row, $α: "Type"$ takes a type, and θ is one sort-respecting map. This is the concrete payoff of @sorting. The mechanization, which predates it, encodes the same discipline as a *pair* of maps over a single untagged namespace — a workaround the annotation makes unnecessary.

Two degenerate cases are worth recording. A monotype scheme has itself as its only instance, so on the monotypes the relation collapses to I-refl. And every scheme has at least its own body as an instance, witnessed by the identity substitution; schemes are never vacuous, which is what lets the progress proof extract _some_ typing for a `let`-bound expression.


== Row-Lookup
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
#row_lookup <row-lookup>

@row-lookup gives the derivation rules for record-type lookups. The judgement $Γ ⊢ ρ.l ↓ r$ is read as »In Context $Γ$, the lookup of label $l$ in row $ρ$ has result $r$« with $r := τ | ⊥ | #v(1em) ?$. The lookup succeeds either with a definite type τ due to a successful lookup, ⊥ when no definite type can be found or ? if the lookup relation encounters a row- or label variable. Accordingly, L-ε and L-miss return with a negative lookup result, L-hit with a positive result and the rules L-conc-hit and L-conc-skip recurse into the left and right subtrees a row can form. The rule L-α consults the context to find out about instantiated type variables α and recurses into their definite value – if present. This is the essential ingredient that enables refinement of ★ types at function application where type variables are instantiated but is only typvar-instantiation under the hood. If α is not yet bound in Γ, L-α-free terminates the search with the unknown result ? and finally L-conc-★ is used to bubble up such a result.


== Row-Equivalence
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
#row_equivalence <row-equivalence>

@row-equivalence gives the row-equivalence rules of our calculus. The equivalence of rows can be lifted to an equivalence on types `τ₁ ≈ τ₂`. The relation is symmetric, transitive, associative, commutative and admits left- and right units. We note that l₁ ≠ l₂ is only decidable for concrete labels and as such, row-equivalence does not go beyond label and row-variables as that would break the shadowing behaviour.

= Unification
// Our algorithm is very similar to the one from P&X, but we depert in a few ways:
// Our rows are trace-monoids and we cancel on both sides, we can do so because we
// we have added delayed lookups (stumps) such that we are not forced to make a decision
// This let's use type strictly more types: see (…)
// Our algorithm is sound and complete in the success case, sound in the clash case and
// incomplete in the occurs and stuck case. These are the two examples where mgus are missed:
// - (…)
// - (…)
//
// We have to make sure, that the δ are not substituted during unification.
// That is what D-* is for…
// Actually, it is to replay the lookup under substition to…?


We present our unification algorithm, which is sound and efficient but incomplete. It is a _mutual_ pair of judgements: $τ₁ ≐ τ₂ ⇝ v$ solves an equation between types and $s₁ scripts(≐)_r s₂ ⇝ v$ one between rows.  @unification-helpers fixes this vocabulary.

#let u_clash = smallcaps("Clash")
#let u_occurs = smallcaps("Occurs")
#let u_stuck = smallcaps("Stuck")
#let u_fuel = smallcaps("No fuel")

#figure(
  caption: "Spines, verdicts and the judgements of the algorithm.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Spines and Verdicts")[
      $
           #type_name("Atom") a & ::= l: τ | α \
          #type_name("Spine") s & ::= ⟨⟩ | a · s \
        #type_name("Verdict") v & ::= θ | #u_clash | #u_occurs | #u_stuck | #u_fuel \
      $
    ],
    subbox(caption: "Notation")[
      #flexbox(
        $#type_name("Spine of a row") ⌈ρ⌉$,
        $#type_name("Field count") |s|_l$,
        $#type_name("Var sequence") "vars"(s)$,
        $#type_name("Type equation") τ₁ ≐ τ₂ ⇝ v$,
        $#type_name("Row equation") s₁ scripts(≐)_r s₂ ⇝ v$,
      )
    ],
  )),
)<unification-helpers>

Rows are not unified as trees but as _spines_ — the lists of atoms $a ::= l: τ | α$ obtained by flattening the concatenation tree — because associativity and the units of @row-equivalence are then quotiented away by construction, and the only residue of ≈ is that distinct labels commute within a var-free segment while nothing crosses a row-variable.

A verdict is either a solution θ or one of four refusals. #u_clash and #u_occurs are the familiar ones; #u_stuck  carries the wand-ambiguity, reported when every move is dead but nothing is provably wrong; #u_fuel separates »the budget ran out« from »the problem is unsolvable«, which is what makes every verdict the algorithm does reach independent of the budget it was given.

== Type Unification
#let unification_ty = figure(
  caption: "Unification of types.",
  flexbox(
    derive("U-refl", (), $α ≐ α ⇝ ∅$),
    derive("U-bind", ($τ ≠ α$, $α ∉ "ftv"(τ)$), $α ≐ τ ⇝ [α ≔ τ]$),
    derive("U-occurs", ($τ ≠ α$, $α ∈ "ftv"(τ)$), $α ≐ τ ⇝ #u_occurs$),
    derive("U-★", (), $★ ≐ ★ ⇝ ∅$),
    derive("U-base", ($𝓫 = 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ ∅$),
    derive("U-base-clash", ($𝓫 ≠ 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ #u_clash$),
    derive("U-rcd", ($⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ θ$,), ${ρ₁} ≐ {ρ₂} ⇝ θ$),
    derive(
      "U-fn",
      ($τ₁ ≐ τ₁′ ⇝ θ₁$, $θ₁ τ₂ ≐ θ₁ τ₂′ ⇝ θ₂$),
      $(τ₁ -> τ₂) ≐ (τ₁′ -> τ₂′) ⇝ θ₂ ∘ θ₁$,
    ),
    derive("U-clash", ($"head"(τ₁) ≠ "head"(τ₂)$,), $τ₁ ≐ τ₂ ⇝ #u_clash$),
  ),
)
#unification_ty <unification-ty>

@unification-ty is standard first-order unification, read top-to-bottom, with two deviations. First, ★ is *rigid*: U-★ unifies it with itself and U-clash rejects it against everything else, so the unknown is never silently absorbed into another type — a ★ in a solution is always one that the lookup relation put there. Second, the occurs check of U-occurs is sort-indexed: it tests ftv at `Type` only. Here too the mechanization predates @sorting and uses an ftv spanning *both* sorts, so it also refuses $α ≐ {… α …}$ when the inner α occurs as a row-variable and the equation is in fact solvable — a conservatism sorting removes. What the guard must do either way is make a binding _eliminate_ its variable, since eliminating variables is what bounds the recursion.

U-bind and U-occurs are stated for a variable on the left and are tried on both sides. U-fn is the only place where the type pass sequences: the solution of the argument equation is applied to the results before they are unified, and the two solutions are composed.

== Row Unification
The row pass is a deterministic cascade. Every move it makes is _forced_ — it preserves the solution set of the problem rather than choosing among alternatives — with the single exception of U-expand, which invents structure and is therefore tried last. @unification-cascade gives the order in which the moves are attempted; the first one whose trigger fires decides the step, and if none fires the configuration is terminal.

#let u_band = (
  exhaust: oklch(95%, 0.025, 250deg),
  cancel: oklch(95%, 0.028, 195deg),
  solve: oklch(95%, 0.03, 155deg),
  matchp: oklch(95%, 0.035, 95deg),
  guess: oklch(95%, 0.035, 320deg),
  give: oklch(94%, 0.012, 285deg),
)
#let u_ok = text(fill: oklch(48%, 0.11, 155deg), weight: "bold", "✓")
#let u_no = text(fill: oklch(48%, 0.13, 25deg), weight: "bold", "✗")
#let u_go = text(fill: zink_700, weight: "bold", "↻")
#let u_both = text(fill: zink_700, size: 8pt, "⇄")

#let u_phase(name, color, n) = table.cell(
  rowspan: n,
  fill: color,
  inset: (x: 4pt, y: 6pt),
  align: center + horizon,
  std.rotate(-90deg, reflow: true, text(
    size: 7.5pt,
    fill: zink_900,
    tracking: 0.8pt,
    smallcaps(name),
  )),
)

#let u_step(name, both: false, trigger, effect, mark) = (
  table.cell(align: left + horizon, inset: (left: 10pt, right: 8pt, y: 7pt))[
    #text(size: 9pt, fill: zink_900, smallcaps(name))#if both [ #u_both]
  ],
  table.cell(align: left + horizon)[#text(size: 10pt, trigger)],
  table.cell(align: left + horizon)[#text(size: 10pt, effect)],
  table.cell(align: center + horizon)[#mark],
)

#let unification_cascade = figure(
  kind: image,
  caption: "The order in which the row-moves are tried.",
  box(width: 100%, stack(
    spacing: 12pt,
    align(center, box(
      inset: (x: 14pt, y: 7pt),
      radius: 4pt,
      fill: luma(96%),
      stroke: 0.5pt + luma(70%),
      $s₁ scripts(≐)_r s₂$,
    )),
    align(center, text(size: 14pt, fill: luma(60%), "↓")),
    table(
      columns: (auto, auto, auto, 1fr, auto),
      stroke: (x, y) => (top: if y > 0 { 0.4pt + luma(88%) }),
      inset: (x: 8pt, y: 7pt),
      row-gutter: 0pt,

      u_phase("exhaust", u_band.exhaust, 2),
      ..u_step(
        "U-ε-var",
        [$⟨⟩ scripts(≐)_r s$ with $s$ field-free],
        $[overline(β) ≔ ε]$,
        u_ok,
      ),
      ..u_step(
        "U-ε-clash",
        [$⟨⟩ scripts(≐)_r s$ with a field in $s$],
        u_clash,
        u_no,
      ),

      u_phase("cancel", u_band.cancel, 2),
      ..u_step(
        "U-var-refl-L",
        $α · t₁ scripts(≐)_r α · t₂$,
        [cancel, recurse on $t₁, t₂$],
        u_go,
      ),
      ..u_step(
        "U-var-refl-R",
        $t₁ · α scripts(≐)_r t₂ · α$,
        [cancel, recurse on $t₁, t₂$],
        u_go,
      ),

      u_phase("solve", u_band.solve, 2),
      ..u_step(
        "U-var-solve",
        both: true,
        $α scripts(≐)_r s$ + [, ] + $α ∉ "vars"(s)$,
        $[α ≔ s]$,
        u_ok,
      ),
      ..u_step(
        "U-var-occurs",
        both: true,
        $α scripts(≐)_r s$ + [, ] + $α ∈ "vars"(s)$,
        u_occurs,
        u_no,
      ),

      u_phase("match", u_band.matchp, 3),
      ..u_step(
        "U-field-L",
        both: true,
        [leading $l: τ$, $l$ in the other side's leading window],
        [emit $τ ≐ τ′$, apply, recurse],
        u_go,
      ),
      ..u_step(
        "U-field-R",
        both: true,
        [trailing $l: τ$, $l$ in the other side's trailing window],
        [emit $τ ≐ τ′$, apply, recurse],
        u_go,
      ),
      ..u_step(
        "U-ground",
        both: true,
        [other side var-free, $|s₁|_l = |s₂|_l > 0$],
        [pair the first $l$'s, emit $τ ≐ τ′$, recurse],
        u_go,
      ),

      u_phase("guess", u_band.guess, 1),
      ..u_step(
        "U-expand",
        both: true,
        [leading $l: τ$, other side has a unique var $β$ and no $l$],
        $β ≔ (l: δ | β′)$ + [, recurse],
        u_go,
      ),

      u_phase("give up", u_band.give, 2),
      ..u_step(
        "U-clash",
        [some $l$ with $|s₁|_l > |s₂|_l$ and $s₂$ var-free],
        u_clash,
        u_no,
      ),
      ..u_step(
        "U-stuck",
        [no move above fires],
        u_stuck + [ — wand-ambiguity],
        u_no,
      ),
    ),
    align(center, text(
      size: 8.5pt,
      fill: zink_700,
      [#u_both tried with the sides exchanged #h(12pt) #u_go apply and recurse on the
        residual #h(12pt) #u_ok solution #h(12pt) #u_no verdict],
    )),
  )),
)
#unification_cascade <unification-cascade>

The two exhaustion rules are checked at every depth and before the budget: once a side is empty, its counterpart's variables are forced to ε and a surviving field has nowhere to come from. The cancellation rules exploit that spines cancel at both ends, which is exactly the trace-monoid property @row-equivalence buys us. U-var-solve fires only when one side is a *lone* variable, so the binding really eliminates it; its occurs-check is the same conservative guard as at the type sort.

The three matching moves differ in how far they may look for a partner. A _window_ is a maximal var-free segment at one end of a spine: U-field-L and U-field-R may pair a field only inside it, because a row-variable in between could be instantiated to a colliding field and the pairing would be a guess about shadowing. U-ground lifts that restriction by *counting*: if one side has no variables at all and some label occurs equally often and positively on both sides, then the other side's variables cannot carry that label, the pairing is positional, and it is again forced. This rule is not derivable from the window rules — the mechanization surfaced it as a genuine gap.

U-expand is the one Rémy-style move that invents structure: when the other side has exactly one variable β and no field for the label at hand, β is the only place the field can live, so $β ≔ (l: δ | β′)$ with fresh δ and β′. Uniqueness is what makes it forced; with two candidate hosts the placement would be a choice, and that is precisely the wand-ambiguity the algorithm refuses to resolve. The fresh names are drawn from a supply that is _threaded_ through the recursion rather than re-derived at each call: a move that drops a field also drops the variables its type mentioned, so a locally recomputed bound could fall below a name still in scope. Finally U-clash is a global projection check rather than a per-window one, and U-stuck reports a terminal configuration.

#let unification_row = figure(
  caption: "Unification of rows, on spines.",
  stack(
    spacing: 15pt,
    align(center, flexbox(
      $#type_name("Leading window") "win"_l (s) = (τ, s′)$,
      $#type_name("Trailing window") "win"^R_l (s) = (τ, s′)$,
      $#type_name("Anywhere") "rem"_l (s) = (τ, s′)$,
    )),
    flexbox(
      derive(
        "U-ε-var",
        ($s$ + [ field-free], $"vars"(s) = overline(β)$),
        $⟨⟩ scripts(≐)_r s ⇝ [overline(β) ≔ ε]$,
      ),
      derive("U-ε-clash", ($(l: τ) ∈ s$,), $⟨⟩ scripts(≐)_r s ⇝ #u_clash$),
      derive(
        "U-var-refl-L",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $α · t₁ scripts(≐)_r α · t₂ ⇝ θ$,
      ),
      derive(
        "U-var-refl-R",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $t₁ · α scripts(≐)_r t₂ · α ⇝ θ$,
      ),
      derive("U-var-solve", ($α ∉ "vars"(s)$,), $α scripts(≐)_r s ⇝ [α ≔ s]$),
      derive(
        "U-var-occurs",
        ($α ∈ "vars"(s)$,),
        $α scripts(≐)_r s ⇝ #u_occurs$,
      ),
      derive(
        "U-field-L",
        (
          $"win"_l (s₂) = (τ′, t₂)$,
          $τ ≐ τ′ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $(l: τ) · t₁ scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-field-R",
        (
          $"win"^R_l (s₂) = (τ′, t₂)$,
          $τ ≐ τ′ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $t₁ · (l: τ) scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-ground",
        (
          $"vars"(s₂) = ⟨⟩$,
          $|s₁|_l = |s₂|_l > 0$,
          $"rem"_l (s_i) = (τ_i, t_i)$,
          $τ₁ ≐ τ₂ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $s₁ scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-expand",
        (
          $"vars"(s₂) = β$,
          $|s₂|_l = 0$,
          $δ, β′ #[fresh]$,
          $t₁ scripts(≐)_r s₂[β′\/β] ⇝ θ$,
        ),
        $(l: τ) · t₁ scripts(≐)_r s₂ ⇝ θ ∘ [δ ≔ τ, β ≔ (l: δ | β′)]$,
      ),
      derive(
        "U-clash",
        ($|s₁|_l > |s₂|_l$, $"vars"(s₂) = ⟨⟩$),
        $s₁ scripts(≐)_r s₂ ⇝ #u_clash$,
      ),
      derive(
        "U-stuck",
        ([no rule above applies],),
        $s₁ scripts(≐)_r s₂ ⇝ #u_stuck$,
      ),
    ),
  ),
)
#unification_row <unification-row>

@unification-row states the moves as rules. They are to be read *in the order of @unification-cascade*, and U-var-solve, U-var-occurs, U-field-L, U-field-R, U-ground and U-expand are additionally tried with the two sides exchanged; U-ε-var, U-ε-clash and U-clash are symmetric as stated. Note that no rule ever pushes a *field demand* into a row-variable: field lookups do not travel through $scripts(≐)_r$, they park as stumps, so the algorithm never guesses a field into a variable except in the unique-host case of U-expand.

The two passes recurse into each other — U-rcd hands a row problem to $scripts(≐)_r$, and U-field-L, U-field-R, U-ground hand a type problem back to $≐$ — and each cross-call consumes one unit of an explicit budget. This makes the definition structurally recursive, which is what lets the mechanization compute verdicts by `rfl` and check worked examples in the kernel; exhausting the budget is the separate verdict #u_fuel, so the four real verdicts are never an artefact of the bound. Crucially, the type equations a matching move emits are solved *on the spot* and their solution applied to the residual before the row pass continues. Deferring them instead would make #u_stuck meaningless: an equation must be discharged, or fatal, or itself stuck, never merely postponed.


= Metatheory
In the following section we lay out the metatheory of our calculus. We prove progress and a weak form of preservation that admits possible runtime errors in the ★-typed part of the language – a standard version for gradual and soft typing systems.

*Preservation*: If $∅ ⊢ e: τ$ and $e → e'$ then $∅ ⊢ e': τ$
*Progress*: If $∅ ⊢ e: τ$ then $e ∈ "Values"$, or $∃e'$ such that $e → e'$, or $e ↯$


== Generalization and Principality

HM-style generalization works at `let`-binding sites: to type `let x = e₁ in e₂`, the algorithm infers a monotype τ₁ for e₁, abstracts over the free type variables $macron(α)$ not appearing in the context, and binds x to the scheme $∀ macron(α). τ₁$ for typing e₂. This proceeds smoothly in standard HM. In our system, however, the inference of e₁ may produce _stumps_ — pending lookups of the form $⟨ρ.l ↓ δ⟩$ that arise when a field selection blocks on an unresolved row-variable. When the stump's result variable δ belongs to the generalized set $macron(α)$, a choice arises: resolve the stump at the `let`-boundary before abstracting, or carry it along in the scheme. These two strategies are L1 and L2.


== Extensions to the minimal Calculus
- Occurrence typing using if's?
- Inherit statements?
- With-construct?


== Towards Nix
> Section about extended features, limitations etc.


== Related Work
_Record concatenation in classic record calculi._ Typing record concatenation is an old and notoriously hard problem. Wand @concat4multiinher first studied type inference for concatenation in the context of multiple inheritance, where the set-or-replace semantics of asymmetric concat already surfaces: his system needs to case-split over which side a field comes from, and typings are unions of alternatives rather than principal types. Harper and Pierce @symm_concat sidestep shadowing by restricting to _symmetric_ concatenation, which is only defined on records with disjoint fields, tracked by compatibility constraints; they also observe that concatenation and width-subtyping do not mix: subtyping can silently forget a field that concatenation later resurrects, breaking soundness — the same observation that steers our calculus away from subsumption and towards row-equivalence. Rémy @concat4free shows that concatenation can be simulated "for free" in a language with polymorphic record extension by abstracting over the extension point, at the price of encoding-style types. Ohori¿ obtains efficient compilation for a polymorphic record calculus, but restricts records to selection and functional update — concatenation is exactly the operation his index-passing compilation scheme cannot support. In the disjoint-polymorphism line @xie2020row the merge operator subsumes symmetric concatenation, with disjointness playing the role of the lacks-constraints. All of these systems either forbid the colliding case that makes Nix' `‖` interesting, or pay for it with non-principal or encoded types; none types the motivating example `a: b: (a ‖ b).l` as-is.

_Scoped rows and first-class labels._ Our row theory descends from Leijen's extensible records with scoped labels @extensible_recs, where duplicate labels are kept in the row and lookup resolves them with left-precedence — precisely the "bag" semantics that makes asymmetric concat a total operation instead of a partially defined one. Leijen later added first-class labels @fc_labels, which Nix needs for its dynamic field selection `e.${e'}`. Paszke and Xie @extensible_tabular combine both into infix-extensible rows with a unification-based inference algorithm over row- and label-variables; their system is the direct basis of ours. It cannot, however, model set-or-replace: extension always happens on a known side of the row, and their conditional tail-check rejects programs whose shadowing behaviour is unresolved — our lookup relation instead accepts them at ★ and refines later.

_Expressive row theories._ The line of work started by Morris and McKinna @rose abstracts rows behind an algebra of containment and combination constraints strong enough to type asymmetric concatenation faithfully, and has been extended to generic programming @generic_with_extensible, extensible recursive functions @extensible_rec_funcs and ad-hoc polymorphism @extensible_data_adhoc. These systems track strictly more information than ours, and Rose is in fact a rank-1 Hindley-Milner language that does establish principal types — but the principality is that of qualified types @qualified_types: inference produces a principal _constrained_ scheme and defers the row predicates to an entailment relation that the framework leaves as a parameter, required only to be invariant under row equivalence, monotone and transitive, and nowhere shown to be decidable. The entailment rules actually given decide only ground predicates and discharge everything else by assumption lookup, so no solving procedure — and hence no complexity bound — is offered for the predicates with row variables that inference actually generates. Since combination is an equation in a partial monoid, deciding conjunctions of such predicates is unification modulo associativity and commutativity rather than syntactic unification, and is NP-complete @ac_unification. The successor systems move to System Fω and are explicitly typed, with type reconstruction left as future work @generic_with_extensible; unrestricted second-order reconstruction is in any case undecidable @undecidable. Sulzmann @designing_record_systems designs record systems in the HM(X) framework, where concatenation becomes a constraint; HM(X) however only stipulates that a constraint solver exists without providing one, and predicate-based systems in general trade completeness of inference for expressiveness¿. Our position is dual: we keep plain unification-based inference and instead weaken the types themselves with ★ where the row theory would need a disjunction.

_Subtyping-based systems._ Algebraic subtyping @algebraic_subtyping @mlsub and its simplifications @simplesub give principal inference for structural subtyping, and MLstruct @mlstruct extends this to a Boolean algebra of types with unions, intersections and negations — negation being one way to express the absence information that shadowing destroys. However, these systems support record extension and field update rather than general concatenation of unknown records, and by the width-subtyping argument above @symm_concat, adding `‖` to a subtyping-based system is problematic at the core: a record can always forget the very fields that decide precedence. We deliberately keep our system subtyping-free; the only ordering is the precision gained by instantiation.

_Set-theoretic types and dynamic languages._ Castagna's programme of semantic subtyping @frisch_semantic @castagna2023programming types dynamic languages with unions, intersections and negations, including detailed accounts of records, maps and structs @typing_records_etc and polymorphic records for Elixir @poly_records, together with occurrence typing to refine types along control flow @revisiting_occurrence @on_occurrence and a gradual guard-based system deployed for Elixir @gradual_elixir. This is the most expressive treatment of records for a dynamic language to date, and occurrence typing is a natural future extension of our calculus (@?). The cost is the full set-theoretic machinery: inference is local rather than let-polymorphic, and the subtyping problems with concatenation resurface. Our ★ plays a role similar to their `Dynamic()`¿, but is introduced by the _lookup relation_ itself rather than by explicit annotation.

_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all, and has been instantiated for extensible rows by Sekiyama and Igarashi @gradual_extensible_rows, the system closest in spirit to our ★-typed rows. We differ in a fundamental way: Nix programs cannot be instrumented, so there are no casts, no blame, and no runtime monitoring. Our system is instead a _soft_ typing system in the tradition of Cartwright and Fagan¿ and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory honestly reports the residual risk as the ↯-disjunct of progress. Industrial gradual systems such as TypeScript @typescript and Flow @flow make the same pragmatic choice of an unsound `any`, but without a formal account of when `any` arises; in our system ★ is introduced only by the lookup relation and T-★-intro, and its origin is therefore always explainable.

_Typing Nix._ Work on Nix itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for the Nix expression language — the semantic foundation our step relation is modelled after¿ — but do not attempt a type system. An earlier system by the author @simplenix applies off-the-shelf HM inference to a Nix subset and fails exactly on the record operations this thesis addresses. The long-standing community issue @nix-ts-issue documents both the demand for and the difficulty of typing Nix; Nickel¿, a Nix-inspired configuration language, opts for gradual typing with row polymorphism but forbids the colliding concatenations we target.


== Conclusion
We have shown that we have such a nice typesystem with so many (much wow) nice properties and we are very happy and thank all the people that helped us accomplish such an outstanding result wow nice.

#bib

