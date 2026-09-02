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

#[
  = Materialsammlung
  - Closedenss
    - Lazyness von Nix erlaubt non-closedness
      - Closedness müsste man auf das ausgewertete Fragment reduzieren
    - With killt auch closedness
  - Keine Recursiveness

  == Goal
  > I want to create a typesystem that handles Nix as best as possible. It should be efficiently computable and have no "breaking" points. Meaning, there is nothing in it that makes it immediately unfeasible for Nix. This is why we need a soft-typing type as well as row- and label variables. The result should be efficiently computable.
]


= A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix
== Abstract
Asymmetric record concatenation with left-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that can not be statically typed: The expression `a: b: (a ‖ b).l` concatenates two type variables but can not be given a type without instantiating at least b, because of field-precedence and shadowing behaviour.
We propose a novel _soft type system_ based upon the work of Paszke&Xie with scoped-records, row-variables, asymmetric record concatenation, let-polymorphism, row-equivalence and an unknown type that [solves]¡ the motivating example using a novel lookup derivation $Γ ⊢ ρ.l ↓ r$ to delay record lookups and a _refinement technique_ upon type variable instantiation to narrow types at term application.
We mechanically prove _type safety_ of the declarative system in Lean and give an efficient unification algorithm for a minimal calculus.


== A Note about Nix
NixLang is the fundamental language of one of the largest bodies of untyped functional code in existence and a language that extends beyond the usual λ-calculus features. The foundational core of the language are records, with many language constructs to create, change and deconstruct these. Two features that make static typing notably hard are first-class labels and the asymmetric record concatenation operation. Only a few systems exist in literature that can handle both features.

A complete typesystem for Nix is hindered by impurities (in an otherwise pure language) that can poison typeability. Using first-class labels and the impure builtin `builtins.currentTime`, it is possible to form an expression that looks up a record field based on the wall-clock time:

```nix
{ before = "moin"; after = 0; }.${if builtins.currentTime < 1767225600 then "before" else "after"}
```

The type of this selection depends on the moment of evaluation, so this is an obviously untypable operation: typing it would predict the future.

The design constraints for a typesystem that types Nix are as follows: Full record calculus strength with first-class labels and the problematic asymmetric concat operation are essential to provide usable type-inference. Computability is an essential design constraint as backtracking would render type inference unusably slow. Lastly, a typesystem is needed that admits unavoidable uncertainty with an unknown type ★ similar to the one used in TypeScript or occurrence typing spearheaded by Castagna.


== Motivation
Asymmetric record concatenation is a central problem that many record calculi address. Its set-or-update behaviour in combination with polymorphism makes tracking of fields extremely hard, and multiple approaches have been suggested that weight benefits againts drawbacks. Row polymorphism is a method to track positive information of records but is unable to track the absence of fields. Without negative information and width-subtyping, overwriting fields is an unrecoverable operation, since width-subtyping can remove a field a: {l: τ} -> a: {} without a trace, and concatenating such a record with b: { l: τ'} can not be clearly resolved due to shadowing.

[This unfortunate situation]¿ can be remedied by lacks-predicates, or stronger type systems like the one by Ohori or the line of work of (Abstracting Records…) that faithfully track positive and negative information with constraint or dependent types. But both come at the cost of computability. [The systems by @? are theoretically astonishing but reduce to System F, where type inference is known to be undecidable]¡ and the systems of Ohori add the full dependent-type complexity.

Our approach, RowNix, positions itself in the middle of both extremes and admits the uncertainty that different kinds of operations can induce by using an unknown type that directly marks uncertainty. Our motivating example admits such a type [`a: b: (a ‖ b).l :: ? → ? → ★`]¡ because it is statically not possible to determine the return type. By surrendering to some form of uncertainty we can adjust the unification algorithm of Paszke&Xie to a system that can be computed efficiently¿.

[In our record calculus, uncertainty is recorded during field-lookup and remedied as well as possible upon function application.]¡ To retain as much information as possible, we use scoped rows and a concatenation operation that glues together two records without simplifying either side directly. This, in combination with our three-way lookup relation, [gives surprising expressiveness to our system](overy optimistic). In our system, lookup is a relation that extends the usual negative and positive results of lookup with an unknown marker `?` that is emitted as soon as a row-variable is hit, since shadowing behaviour after that point is not clear.

The concatenation inside the example `a: ({l: τ} ‖ a).l` will produce a row `(α | l: τ)` with a type variable for the function argument. Upon instantiation at the application site, the row-variable can be eliminated such that the lookup relation that was previously stuck before finding a field can advance further into the row, find the l: τ binding, and return a proper type τ.


_Contributions_ We contribute the following items:

1. [*A best-effort lookup relation.*](wack) Our lookup relation `Γ ⊢ ρ.l ↓ r` extends the usual positive and negative results of lookup to a three-way result (τ | ⊥ | ?).
2. [*Type refinement at instantiation.*](wack) Uncertainty introduced by lookup is remedied at application sites: instantiating a type variable lets a previously stuck lookup advance further into the row and promote ★ to a definite type.
3. *Mechanized type safety.* We prove _progress_ under erroring terms ↯ and _preservation_ in Lean.
4. *An algorithmic system.* We give an efficient unification algorithm for the minimal calculus, extending the algorithm of Paszke&Xie to rows containing the unknown type.


== Informal Description of the TS and it's tricks
- Row-equality up to type-vars
- We don't need the tail check :)
- Explain why ★ is covariant in all positions for functions

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

@syntax shows the term- and type-syntax of a standard lambda-calculus extended with records, record-concatenation and let-polymorphism. Functions use the unusual syntax (x: e) where x is the variable to be replaced in the function body e. This distinction is chosen because it's Nix' syntax for functions. We admit a finite set 𝓒 of constants $c ∈ 𝓒$ that can be typed by basetypes 𝓫 from the finite set of basetypes 𝓑 and state that 𝓑 has at least the types needed to type every constant such that `c: 𝓫_c` is a complete mapping. We admit an "unknown" ★ type for our soft-typing system that can be used to type expressions the typesystem can not reason about. Term-rows ${ξ}$ and row-types ${ρ}$ are both trees that shows their similarity. As per the usual, we stratify our typesystem with a polymorphic σ-type that subsumes the monomorphic types τ to sidestep [the risk of undecadbility]¿.


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

The declarative system's typing rules follow the standard λ-calculus rules. T-cons is used to type the set of constants of the language with their respective type $𝓫_c$. T-var not only looks up variables in the context Γ, but also instantiates polymorphic types using the instantiation rules from @instantiation discussed in the following section. T-eq equates types equal up to the row-equivalence relation from @row-equivalence. T-conc concatenates two row types by concatenating their type representation and T-sel types record lookups by lifting the hard work to the row-lookup relation, defined in @row-lookup. T-sel-★ is needed (as discussed in TODO-section) to type otherwise stuck terms and T-★-intro to blur a type into the unknown. The rules T-rec, T-ξ-empty, T-ξ-field and T-ξ-conc type record literals.


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

@row-lookup gives the derivation rules for record-type lookups. The judgement $Γ ⊢ ρ.l ↓ r$ is read as »In Context $Γ$, the lookup of label $l$ in row $ρ$ has result $r$« with $r := τ | ⊥ | #v(1em) ?$. The lookup succeeds either with a definite type τ due to a successful lookup, ⊥ when no definite type can be found or ? if the lookup relation encounters a row- or label variable. Accordingly, L-ε and L-miss return with a negative lookup result, L-hit with a positive result and the rules L-conc-hit and L-conc-skip recurse into the left and right subtrees a row can form. The rule L-α consults the context to find out about instantiated type variables α and recurses into their definite value – if present. This is the essential ingredient that enables refinement of ★ types at function application where type variables are instantiated. If α is not yet bound in Γ, L-α-free terminates the search with the unknown result ? and finally L-conc-★ is used to bubble up such a result.


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


== Formal: Metatheory
In the following section we lay out the metatheory of our calculus. We prove progress and a weak form of preservation that admits possible runtime errors in the ★-typed part of the language – a standard version for gradual and soft typing systems.

*Preservation*: If $∅ ⊢ e: τ$ and $e → e'$ then $∅ ⊢ e': τ$
*Progress*: If $∅ ⊢ e: τ$ then $e ∈ "Values"$, or $∃e'$ such that $e → e'$, or $e ↯$


== Generalization and Principality

HM-style generalization works at `let`-binding sites: to type `let x = e₁ in e₂`, the algorithm infers a monotype τ₁ for e₁, abstracts over the free type variables $macron(α)$ not appearing in the context, and binds x to the scheme $∀ macron(α). τ₁$ for typing e₂. This proceeds smoothly in standard HM. In our system, however, the inference of e₁ may produce _stumps_ — pending lookups of the form $⟨ρ.l ↓ δ⟩$ that arise when a field selection blocks on an unresolved row-variable. When the stump's result variable δ belongs to the generalized set $macron(α)$, a choice arises: resolve the stump at the `let`-boundary before abstracting, or carry it along in the scheme. These two strategies are L1 and L2.

*L1 — finalize at the boundary.* Before generalizing over $macron(α)$, resolve every pending stump by setting δ ≔ ★. Schemes remain plain $∀ macron(α). τ$, as in standard HM. This is sound and simple: if e₁ produces a stump, T-sel-★ already types the selection at ★, so the inferred scheme faithfully records that uncertainty. For `let f = (x: x.l) in f {}`, L1 correctly gives `f : ∀β. {β} → ★`. The cost appears only at polymorphic call sites: `let f = (x: x.l) in f {l = c}` should deliver type `𝓫_c` — the lookup of l in `{l = c}` succeeds definitively — but L1 has frozen δ at ★ before the argument is known, and no refinement can recover it.

*L2 — stumps in schemes.* Carry each pending stump as a _constraint_ on the scheme, extending the scheme syntax:

$
  σ := ∀ macron(α). Q ⇒ τ #h(3em)
  Q := { ⟨ρ.l ↓ δ⟩, … } #h(2em)
  δ ∈ macron(α)
$

A plain scheme $∀ macron(α). τ$ is the special case Q = ∅. At each use of a variable x : σ, the scheme is instantiated with fresh copies of $macron(α)$ and each constraint is _discharged_ against the current typing environment under the instantiation substitution θ. Discharge (@discharge) replays the three-way lookup case split:

#let discharge = figure(
  caption: "Constraint discharge at instantiation (L2). θ is the instantiation substitution; δ is the stump's result variable. A still-blocked lookup is re-stated over the substituted row instead of resolving δ.",
  flexbox(
    derive("D-hit", ($Γ ⊢ (θ ρ).l ↓ τ_r$,), $θ δ = τ_r$),
    derive("D-⊥", ($Γ ⊢ (θ ρ).l ↓ ⊥$,), $θ δ = ★$),
    derive("D-defer", ($Γ ⊢ (θ ρ).l ↓ #v(1em) ?$,), $⟨(θ ρ).l ↓ δ⟩ "residual"$),
  ),
)
#discharge <discharge>

D-hit corresponds to T-sel and D-⊥ to T-sel-⊥. D-defer covers the case where the substituted row still blocks the lookup — θ replaced the old blocker but the walk now stops at a new variable β inside θρ. Instead of freezing δ at ★, the constraint is re-stated over the substituted row and stays pending with β as its new blocker: it joins the constraint set of the enclosing generalization (or, algorithmically, re-parks as a stump). δ is resolved to ★ only at _finalization_ — the top level of the program, where no further instantiation can arrive — and that is the moment corresponding to T-sel-★. The correspondence runs deep: the instance-closed reading of T-let quantifies over all instances of σ and requires e₁ to be typeable at each — discharge is exactly its algorithmic image, re-evaluating the lookup for each call's concrete substitution θ independently. With L2, `let f = (x: x.l) in f {l = c}` correctly delivers `𝓫_c`: the instantiation θ maps β ≔ (l: 𝓫_c), discharge applies L-hit to (l: 𝓫_c).l, and sets δ ≔ 𝓫_c.

=== L1 is not sufficient: a mechanized refutation

The choice between L1 and L2 is not a trade-off in precision — it is forced. We prove in Lean that L1 _cannot be principal_: no plain ∀ᾱ.τ scheme captures all declarative typings of a term that generates a stump.

The witness is λx. x.l. The declarative system types it at `{l: τ₀} → τ₀` for every type τ₀, via T-sel with ρ = (l: τ₀) (the lookup hits immediately), and also at `{β} → ★` for any fresh row-variable β, via T-sel-★ with ρ = β (the lookup of l in β returns ?). An L1 algorithm infers the scheme `∀β. {β} → ★` — the stump is finalized immediately and its result frozen.

Two mechanized Lean lemmas close the case.

_finalized_no_blur_: No substitution instance of `{β} → ★` lies ⊑-below a found-typing `{l: τ₀} → τ₀` with τ₀ ≠ ★. The precision relation ⊑ is _rigid_: if τ′ ⊑ τ and τ ≠ ★, then τ′ and τ share the same head constructor. The frozen ★ in the result position of `{β} → ★` can therefore never be blurred into a definite type. The principality factoring — "every declarative typing is ⊑-below some instance of the principal scheme" — fails for all found-typings.

_no_plain_principal_scheme_: No plain ∀ᾱ.τ scheme is simultaneously instance-closed while having both the found-typing `{(l: {ε})} → {ε}` and the ⊥-typing `{ε} → ★` as instances. Such a scheme must place a quantified variable α in the result position to cover both. Setting α ≔ {ε} for the found-typing and α ≔ ★ for the ⊥-typing together produce a third candidate instance `{ε} → {ε}`. But this is not a valid typing: when the argument has type {ε}, the lookup of l in ε returns ⊥, so the body can only receive type ★, not {ε}. The scheme is not instance-closed. Since a quantified variable in the result position is the only candidate structure, no plain scheme works.

The consequence is that L1 is a sound baseline — it never assigns a contradictory type — but principality must be stated over L2's qualified schemes. The L2-principal type of λx. x.l is:

$∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ$

Here δ remains writable: D-hit binds it to the found type at each call site, D-⊥ binds it to ★, and a still-blocked lookup stays pending via D-defer until finalization forces ★. A plain scheme has no mechanism to express this per-instantiation resolution. L2 is therefore not an optional refinement; it is the necessary form of let-generalization for a calculus with lookup-stumps.


== Extensions to the minimal Calculus
- Occurrence typing using if's?
- Inherit statements?
- With-construct?


== Towards Nix
> Section about extended features, limitations etc.


== Related Work
_Record concatenation in classic record calculi._ Typing record concatenation is an old and notoriously hard problem. Wand @concat4multiinher first studied type inference for concatenation in the context of multiple inheritance, where the set-or-replace semantics of asymmetric concat already surfaces: his system needs to case-split over which side a field comes from, and typings are unions of alternatives rather than principal types. Harper and Pierce @symm_concat sidestep shadowing by restricting to _symmetric_ concatenation, which is only defined on records with disjoint fields, tracked by compatibility constraints; they also observe that concatenation and width-subtyping do not mix: subtyping can silently forget a field that concatenation later resurrects, breaking soundness — the same observation that steers our calculus away from subsumption and towards row-equivalence. Rémy @concat4free shows that concatenation can be simulated "for free" in a language with polymorphic record extension by abstracting over the extension point, at the price of encoding-style types. Ohori¿ obtains efficient compilation for a polymorphic record calculus, but restricts records to selection and functional update — concatenation is exactly the operation his index-passing compilation scheme cannot support. In the disjoint-polymorphism line @xie2020row the merge operator subsumes symmetric concatenation, with disjointness playing the role of the lacks-constraints. All of these systems either forbid the colliding case that makes Nix' `‖` interesting, or pay for it with non-principal or encoded types; none types the motivating example `a: b: (a ‖ b).l` as-is.

_Scoped rows and first-class labels._ Our row theory descends from Leijen's extensible records with scoped labels @extensible_recs, where duplicate labels are kept in the row and lookup resolves them with left-precedence — precisely the "bag" semantics that makes asymmetric concat a total operation instead of a partially defined one. Leijen later added first-class labels @fc_labels, which Nix needs for its dynamic field selection `e.${e'}`. Paszke and Xie @extensible_tabular combine both into infix-extensible rows with a unification-based inference algorithm over row- and label-variables; their system is the direct basis of ours. It cannot, however, model set-or-replace: extension always happens on a known side of the row, and their conditional tail-check rejects programs whose shadowing behaviour is unresolved — our lookup relation instead accepts them at ★ and refines later.

_Expressive row theories._ The line of work started by Morris and McKinna @abstracting_extensible_data abstracts rows behind an algebra of containment and combination constraints strong enough to type asymmetric concatenation faithfully, and has been extended to generic programming @generic_with_extensible, extensible recursive functions @extensible_rec_funcs and ad-hoc polymorphism @extensible_data_adhoc. These systems track strictly more information than ours — but they are formulated over System F with explicit type abstraction and application, and type inference is not shown (and full System F inference is undecidable @undecidable). Sulzmann @designing_record_systems designs record systems in the HM(X) framework, where concatenation becomes a constraint; HM(X) however only stipulates that a constraint solver exists without providing one, and predicate-based systems in general trade completeness of inference for expressiveness¿. Our position is dual: we keep plain unification-based inference and instead weaken the types themselves with ★ where the row theory would need a disjunction.

_Subtyping-based systems._ Algebraic subtyping @algebraic_subtyping @mlsub and its simplifications @simplesub give principal inference for structural subtyping, and MLstruct @mlstruct extends this to a Boolean algebra of types with unions, intersections and negations — negation being one way to express the absence information that shadowing destroys. However, these systems support record extension and field update rather than general concatenation of unknown records, and by the width-subtyping argument above @symm_concat, adding `‖` to a subtyping-based system is problematic at the core: a record can always forget the very fields that decide precedence. We deliberately keep our system subtyping-free; the only ordering is the precision gained by instantiation.

_Set-theoretic types and dynamic languages._ Castagna's programme of semantic subtyping @frisch_semantic @castagna2023programming types dynamic languages with unions, intersections and negations, including detailed accounts of records, maps and structs @typing_records_etc and polymorphic records for Elixir @poly_records, together with occurrence typing to refine types along control flow @revisiting_occurrence @on_occurrence and a gradual guard-based system deployed for Elixir @gradual_elixir. This is the most expressive treatment of records for a dynamic language to date, and occurrence typing is a natural future extension of our calculus (@?). The cost is the full set-theoretic machinery: inference is local rather than let-polymorphic, and the subtyping problems with concatenation resurface. Our ★ plays a role similar to their `Dynamic()`¿, but is introduced by the _lookup relation_ itself rather than by explicit annotation.

_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all, and has been instantiated for extensible rows by Sekiyama and Igarashi @gradual_extensible_rows, the system closest in spirit to our ★-typed rows. We differ in a fundamental way: Nix programs cannot be instrumented, so there are no casts, no blame, and no runtime monitoring. Our system is instead a _soft_ typing system in the tradition of Cartwright and Fagan¿ and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory honestly reports the residual risk as the ↯-disjunct of progress. Industrial gradual systems such as TypeScript @typescript and Flow @flow make the same pragmatic choice of an unsound `any`, but without a formal account of when `any` arises; in our system ★ is introduced only by the lookup relation and T-★-intro, and its origin is therefore always explainable.

_Typing Nix._ Work on Nix itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for the Nix expression language — the semantic foundation our step relation is modelled after¿ — but do not attempt a type system. An earlier system by the author @simplenix applies off-the-shelf HM inference to a Nix subset and fails exactly on the record operations this thesis addresses. The long-standing community issue @nix-ts-issue documents both the demand for and the difficulty of typing Nix; Nickel¿, a Nix-inspired configuration language, opts for gradual typing with row polymorphism but forbids the colliding concatenations we target.


== Conclusion
We have shown that we have such a nice typesystem with so many (much wow) nice properties and we are very happy and thank all the people that helped us accomplish such an outstanding result wow nice.

#bib
