#import "./functions.typ": *

#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

#set document(
  title: "A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  description: "Masterthesis about a Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

= A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix
== Goal
> I want to create a typesystem that handles Nix as best as possible. It should be efficiently computable and have no "breaking" points. Meaning, there is nothing in it that makes it immediately unfeasible for Nix. This is why we need a soft-typing type as well as row- and label variables. The result should be efficiently computable.


== Abstract
Asymmetric record concatenation with left-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that can not be statically typed: The expression `a: b: (a ‖ b).l` concatenates two type variables but can not be given a type without instantiating at least b, because of field-precedence and shadowing behaviour.
We propose a novel _soft type system_ based upon the work of Paszke&Xie with scoped-records, row-variables, asymmetric record concatenation, let-polymorphism, row-equivalence and an unknown type that solves¿ the motivating example using a new lookup derivation _Γ ⊢ ρ.l ↓ r_ to delay record lookups and a _refinement technique_ upon type variable instantiation to narrow types at term application.
We mechanically prove _type safety_ of the declarative system in Lean and give an efficient unification algorithm for a minimal calculus.


== A Note about Nix
NixLang is the fundamental language of one of the largest bodies of untyped functional code in existence and a language that extends beyond the usual λ-calculus features. The foundational core of the language are records, with many language constructs to create, change and deconstruct these. Two features that make static typing notably hard are first-class labels and the asymmetric record concatenation operation. Only a few systems exist in literature that can handle both features.

A complete typesystem for Nix is not possible due to impurities (in an otherwise pure language) that can poison typeability. Using first-class labels and the impure builtin `builtins.currentTime`, it is possible to form an expression that looks up a record field based on the wall-clock time:

```nix
{ before = "moin"; after = 0; }.${if builtins.currentTime < 1767225600 then "before" else "after"}
```

The type of this selection depends on the moment of evaluation, so this is an obviously untypable operation: typing it would predict the future.

The design constraints for a typesystem that types Nix are as follows: Full record calculus strength with first-class labels and the problematic asymmetric concat operation are essential to provide usable type-inference. Computability is an essential design constraint as backtracking would render type inference unusably slow. Lastly, a typesystem is needed that admits unavoidable uncertainty with an unknown type ★ similar to the one used in TypeScript or occurrence typing spearheaded by Castagna.


== Motivation
Asymmetric record concatenation is a central problem that many record calculi address. Its set-or-update behaviour in combination with polymorphism makes tracking of fields extremely hard, and multiple approaches have been suggested that come at different costs. Row polymorphism is a method to track positive information of records but is unable to track the absence of fields. Without negative information and width-subtyping, overwriting fields is an unrecoverable operation, since width-subtyping can remove a field a: {l: τ} -> a: {} without a trace, and concatenating such a record with b: { l: τ'} can not be clearly resolved due to shadowing.

This unfortunate situation can be remedied by lacks-predicates, or stronger type systems like the one by Ohori or the line of work of (Abstracting Records…) that faithfully track positive and negative information with constraint or dependent types. But both come at the cost of computability. The systems by @? are theoretically astonishing but reduce to System F, where type inference is known to be undecidable and the systems of Ohori add the full dependent-type complexity to type systems¿.

Our approach, RowNix, positions itself in the middle of both extremes and admits the uncertainty that different kinds of operations can induce by using an unknown type that directly marks uncertainty. Our motivating example admits such a type `a: b: (a ‖ b).l :: ? → ? → ★` because it is statically not possible to determine the return type. By surrendering to some form of uncertainty we can adjust the unification algorithm of Paszke&Xie to a system that can be computed efficiently¿.

In our record calculus, uncertainty is recorded during field-lookup and remedied as well as possible upon function application. To retain as much information as possible, we use scoped rows and a concatenation operation that glues together two records without simplifying either side directly. This, in combination with our three-way lookup relation, gives surprising expressiveness to our system. In our system, lookup is a relation that extends the usual negative and positive results of lookup with an unknown marker `?` that is emitted as soon as a row-variable is hit, since shadowing behaviour after that point is not clear.

The concatenation inside the example `a: ({l: τ} ‖ a).l` will produce a row `(α | l: τ)` with a type variable for the function argument. Upon instantiation at the application site, the row-variable can be eliminated such that the lookup relation that was previously stuck before finding a field can advance further into the row, find the l: τ binding, and return a proper type τ.

== Contributions

- *A declarative soft type system for records.* We extend the row theory of Paszke&Xie with _scoped rows_, _asymmetric concatenation_ with left-precedence, _row-equivalence_, _let-polymorphism_ and an _unknown type_ ★ that marks statically unresolvable operations instead of rejecting the program.

- *A best-effort lookup relation.* Our lookup relation `Γ ⊢ ρ.l ↓ r` extends the usual positive and negative results of lookup to a three-way result (τ | ⊥ | ★). The relation can consult row-solutions in the context, and its _monotonicity_ — definite results survive extending the solutions, only ★ can improve — is what makes deferring lookups sound.

- *Type refinement at instantiation.* Uncertainty introduced by lookup is remedied at application sites: instantiating a type variable lets a previously stuck lookup advance further into the row and promote ★ to a definite type.

- *Mechanized type safety.* We prove a form of _progress_ that admits some runtime errors and _preservation_ for the minimal calculus in Lean.

- *An algorithmic system.* We give an efficient unification algorithm for the minimal calculus, extending the algorithm of Paszke&Xie to rows containing the unknown type.

== Informal Description of the TS and it's tricks
- Row-equality up to type-vars
- T-sel-⊥ and T-★-intro


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
#syntax <syntax>

@syntax shows the term- and type-syntax of a standard lambda-calculus extended with records, record-concatenation (‖) and let-polymorphism. Functions use the unusual syntax (x: e) where x is the variable to be replaced in the function body e. This distinction is chosen because it's Nix' syntax for functions. We admit a finite set 𝓒 of constants $c ∈ 𝓒$ that can be typed by basetypes 𝓫 from the finite set of basetypes 𝓑 and state that 𝓑 has at least the types needed to type every constant such that `c: 𝓫_c` is a complete mapping. We admit an "unknown" ★ type for our soft-typing system that can be used to type expressions the typesystem can not reason about. Term-rows ${ξ}$ and row-types ${ρ}$ are both [what form of trees]¿ that shows their similarity. As per the usual, we stratify our typesystem with a polymorphic σ-type that subsumes the monomorphic types τ.


== Declarative
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
#declarative <declarative>

The declarative system's typing rules follow the standard λ-calculus rules. T-cons is used to type the set of constants of the language with their respective type $𝓫_c$. T-var not only looks up variables in the context Γ, but also instantiates polymorphic types using the instantiation rules from @instantiation discussed in the following section. T-eq equates types equal up to the row-equivalence relation from @row-equivalence. T-conc concatenates two row types by concatenating their type representation and T-sel types record lookups by giving the hard work to the row-lookup relation, defined in @row-lookup. T-sel-★ is needed (as discussed in TODO-section) to type otherwise stuck terms and T-★-intro to blur a type into nothingness. The rules T-rec, T-ξ-empty, T-ξ-field and T-ξ-conc are needed to type record literals.


== Instantiation
#let instantiation = figure(
  caption: "Instantiation.",
  flexbox(
    derive("I-refl", (), $τ ≥ τ$),
    derive("I-ty", ($σ[τ′\/α] ≥ τ$,), $(∀α. σ) ≥ τ$),
    derive("I-row", ($σ[ρ\/α] ≥ τ$,), $(∀α. σ) ≥ τ$),
  ),
)
#instantiation <instantiation>


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

@row-lookup gives the derivation rules for record-type lookups. The judgement $Γ ⊢ ρ.l ↓ r$ is read as »In Context Γ, the lookup of label l in row ρ has result r« with $r := τ | ⊥ | ?$. The lookup succeeds either with a definite type τ due to a successful lookup, ⊥ when no definite type can be found and ? if the lookup relation encounters a type-variable. Accordingly, L-ε and L-miss return with a negative lookup result, L-hit with a positive result and the rules L-conc-hit and L-conc-skip recurse into the left and right subtrees a row can form. The rule L-α consults the context to find out about instantiated type variables α and recurses into their definite value. This is the essential ingredient that enables refinement of ★ types at function application where type variables are instantiated. If α is not yet bound in Γ, L-α-free terminates the search with the unknown result ? and finally L-conc-★ is used to bubble up such a result.


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

@row-equivalence gives the row-equivalence rules of our calculus (and the one of Paszke&Xie?). The equivalence of rows can be lifted to an equivalence on types `τ₁ ≈ τ₂`. The relation is symmetric, transitive, associative, commutative and admits left- and right units. We note that l₁ ≠ l₂ is only decidable for concrete labels and as such, row-equivalence does not go beyond label and row-variables as that would break the shadowing behaviour.


== Formal: The Typesystem


== Formal: Metatheory
In the following section we lay out the metatheory of our calculus. We prove progress and a weak form of preservation that admits possible runtime errors in the ★-typed part of the language – a standard version for gradual and soft typing systems.

*Preservation*: If $∅ ⊢ e: τ$ and $e → e'$ then $∅ ⊢ e': τ$
*Progress*: If $∅ ⊢ e: τ$ then $e ∈ "Values"$, or $∃e'$ such that $e → e'$, or $e ↯$


== Extensions to the minimal Calculus
- Occurrence typing using if's?
- Inherit statements?
- With-construct?


== Towards Nix
> Section about extended features, limitations etc.


== Related Work
- Discuss the work of Parreaux, Castagna, Leijen, Belgier, old record calculi, Ohori etc.

Main: @verified

We base our work upon @extensible_tabular but extend it with a soft typing system and

Symmetric concatenation has been dicsussed in @symm_concat @concat4multiinher @concat4free

First class labels in @fc_labels

The typesystems in @extensible_data_adhoc @extensible_rec_funcs but they are not inferrable. (at least not shown)

- Subtyping based systems: It has been shown by ? that width-subtyping and the concat operation don't mix well because we can forget fields that will still affect the operational semantics.

== TMP: Comparison



== Conclusion
We have shown that we have such a nice typesystem with so many (much wow) nice properties and we are very happy and thank all the people that helped us accomplish such an outstanding result wow nice.

#bib
