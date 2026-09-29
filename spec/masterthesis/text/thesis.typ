#import "./functions.typ": *
#set document(
  title: "A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  description: "Master thesis about a Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

#let title = [A Soft-Typing Records Calculus with Asymmetric Concatenation for Nix]

#page(footer: align(
  center,
  "Department of Computer Science – University Freiburg",
))[
  #align(center, text(25pt)[
    #image("assets/logo.png", width: 30%)
    *#title*
    #set text(16pt)

    \

    *Master Thesis* - Sebastian Klähn\
    #text(12pt)[*sebastian.klaehn\@email.uni-freiburg.de*]

    #set text(12pt)

    \

    #text(tracking: 0.5pt)[*Examiner*]: #h(2pt) Prof. Dr. Peter Thiemann \
    #text(tracking: 0.5pt)[*Second Examiner*]: #h(2pt) TBD \
    #text(tracking: 0.5pt)[*Advisor*]: #h(2pt) Prof. Dr. Peter Thiemann

    \
  ])
  #align(center)[
    #set par(justify: true)
    #pad(x: 15pt, top: 10pt, bottom: 20pt)[
      = Abstract
      Asymmetric record concatenation with right-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that cannot be statically typed: The expression `a: b: (a ‖ b).l` concatenates two type variables but cannot be given a type without instantiating at least b, because of field-precedence and shadowing behaviour.
      We propose a novel _soft type system_ based upon the work of Paszke&Xie with scoped-records, row-variables, asymmetric record concatenation, let-polymorphism, first class labels, row-equivalence and an unknown type that delineates the exact cases in which the so-called wand-ambiguity is hit. We mechanically prove _type safety_ of the declarative system with qualified Types in Lean and give a sound, efficient but incomplete inference algorithm for a minimal calculus.
    ]
  ]
]

#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

= Motivation <sec-motivation>
The Nix programming language @nix-language-2-28 @dolstra_phd, we dub NixLang, is the fundamental language of one of the largest bodies of untyped functional code in existence and a language with features that extend well beyond the basic λ-calculus. Its foundational data structure is the _attribute set_ — a record — and the language provides a gamut of constructs and builtin functions to create, extend, deconstruct and reflect upon them. NixLang powers nixpkgs, a package repository of more than 100,000 packages that is continuously evaluated, updated and rolled out from one central repository, and the same repository carries the Nix standard library, the NixOS module system and the definition of the NixOS distribution itself @nixos_short @nixos_long. Every one of those artefacts is an attribute set assembled out of other attribute sets. NixLang is thus both the motivation for our work and the guiding principle behind the features of our calculus.

Our continuous efforts strive to design a type system that can be used as-is for this existing body of code and aid programmers of existing code in writing safer code more easily. Concretely, we distance ourselves from projects such as Nickel @nickel that tackle type inference from the other side: Create a sound type system and then compile it to Nix. Such efforts might overcome structural impediments of existing systems but cannot support and enhance them without significant changes.

From our goal to type existing Nix code and create an applicable type system, we conclude the following design constraints:

Firstly, since the ultimate goal is to aid programmers today, we need an _efficiently computable_ algorithm that knows its own limitations. Using only the builtin functions, it is possible to write code that is not statically typable and breaks completeness on the nose:

```nix
{ .. }.${builtins.currentTime}
```

This simple example combines records, impure functions and first-class labels to dynamically look up a record field based on the system time at program execution. Whether this program will succeed or error cannot be determined statically, as deciding it would predict the time of execution and as such, the future. An unknown type (★) that can be used to delineate the cases in which type inference has to stop is thus a natural decision for our type system. This unknown type absorbs uncertainty of two kinds: behaviour that is dynamic by nature, as in the example, and placeholders for source code that could be typed by a stronger type system in the future. We thus introduce the ★-type, an _unknown type_ that directly marks places where the type inference algorithm gives up to keep its termination and soundness properties.

In conclusion, we state the following design goals for a reasonable type system for Nix:
/ R1 (No source changes): The analysis must run on today's nixpkgs verbatim. No program needs rewriting to become analysable.
/ R2 (No semantic change): Evaluation must produce exactly what it produced before, down to derivation hashes.
/ R3 (Totality): Every program must receive a type. Where the analysis cannot decide, the result is ★ or a reported stuck verdict, never a rejection of a program that runs.
/ R4 (Effective inference): The analysis must scale to a repository of more than 100,000 packages, evaluated as one program.

== Guiding features of Nix
#figure(
  caption: [The three idioms that make asymmetric concatenation unavoidable.],
  box(width: 100%, align(left, ```nix
  # 1. an overlay: extend or replace fields of a package set given to us
  self: super: { hello = super.hello // { meta = { broken = false; }; }; }

  # 2. the callPackage idiom: override parts of an argument set we did not build
  mkDerivation (args // { buildInputs = args.buildInputs ++ [ extra ]; })

  # 3. the module system: merge configuration fragments from many files
  { config, lib, ... }: { services.nginx = lib.mkMerge [ base config.extra ]; }
  ```)),
)<nix-idioms>

Our own design goals, together with the features that Nix provides, immediately position our type system between existing record literature:

Nix is a configuration language, and its most important features and capabilities are the guiding principle for our language. Records in Nix can be constructed from a set of labels, separated by semicolons `{ a = 1; b = 2;}`, and two features have immediate effects on the type system needed to support them: (1) Asymmetric record concatenation and (2) first-class labels.

Asymmetric record concatenation is a record operation that uses the fields of the first operand and extends or overwrites them with the fields of the second operand. The hardest part of this operation is the faithful tracking of labels under abstract operands. The wand example `(a || b).l` is the prime example for this case and many approaches have been tried to tackle the resulting ambiguity.

//the selection must resolve $l$ in $(β | α)$. To answer it we must know whether $β$ contains $l$: if it does, the answer is $β$'s binding for $l$; if it definitely does _not_, the answer is $α$'s. Neither is known, and no amount of positive information about $β$ will settle it.

Overwriting existing fields is one of the most important features for Nix because it is used ubiquitously to merge configurations, overwrite package fields and to extend or overwrite the existing package set. An example of all three cases can be seen in @nix-idioms. This requirement immediately rules out record calculi that restrict themselves to _symmetric concatenation_ @symm_concat where both halves (the two operands) have disjoint labels. It also rules out another feature commonly seen in record calculi. Using _width-subtyping_, one can remove or forget fields ${l: τ} <= {}$ in a record. This, in combination with asymmetric record concatenation, leads to unsoundness, because previously forgotten and then untracked fields overwrite existing fields semantically but cannot be tracked by the record system. Lacks-predicates can salvage this situation by denoting the absence of a field, but they have to be supplied explicitly, which contradicts our requirement R2.
We therefore keep the calculus subtyping-free; the only ordering it admits is the precision gained by instantiation and row equivalence (≈).

With _scoped records_, the concatenation operation merely becomes a concatenation operation. Our calculus uses rows ρ that hold label-type bindings and type variables. Such a row ρ can be put into curly braces to form a record type {ρ}. For concatenation, both record-halves {ρ} and {ρ'} are just glued together to form {ρ | ρ'} and no information is lost. We combine this with a novel lookup relation of the form `(ρ.l ↓ r)` (@row-lookup) that traverses the natural structure of rows to look up a label l in the row ρ. The result r can be of three kinds:

The obvious two results are either a type τ in case the lookup was successful or a marker ⊥ in case the lookup did not succeed. A row can consist of fixed label-type bindings but also row-variables and our lookup relation stops at these, returning a result ? because shadowing behaviour after this point is not clear. Type substitution will clear those type variables and make the lookup advance further into the record. Only at the end of typing, every still remaining ? will be turned into the unknown type ★.

// _Why not unions of typings?_ Wand's original treatment @concat4multiinher @wand_complete case-splits over which side contributes each field and yields a set of alternative typings rather than a principal one. The number of alternatives is exponential in the number of contested labels, which violates R4, and the absence of principal types propagates into every `let` in the program.

// _Why not set-theoretic types?_ Castagna's programme @frisch_semantic @castagna2023programming @typing_records_etc @poly_records is the most expressive treatment of records for a dynamic language to date, and negation types express precisely the absence information shadowing destroys. The cost is that inference is local rather than let-polymorphic, and that subtyping — with the concatenation problem above — returns in full.

// _Why not lacks-predicates?_ Qualified types with $α backslash l$ constraints @gaster_jones @qualified_types are the cheapest source of negative information available, and they are a good fit for this calculus — cheap enough, in fact, that we return to them in @sec-extensions as the most promising extension. The reason they are absent from the minimal calculus is not the usual complaint that they make types verbose; in a system that requires no annotations, nobody ever writes them. The reason is that *nothing in the calculus generates them*. Lacks-constraints are forced upon Gaster and Jones because record _extension_ is partial and its typing rule must demand absence. Scoped rows make concatenation total, so no rule ever demands anything, and a constraint form with no introduction site buys nothing. The three Nix constructs that _would_ generate them — closed function patterns, `removeAttrs`, and `?`-guards under `if` — all live outside the minimal calculus.



// #figure(
//   caption: [The trilemma. No system attains all three, and each line names what it surrenders.],
//   table(
//     columns: (auto, auto, auto, auto, 1fr),
//     align: (left, center, center, center, left),
//     inset: 6pt,
//     stroke: 0.4pt + luma(200),
//     table.header([*System*], [*P*], [*I*], [*S*], [*What is given up*]),

//     [Gaster–Jones @gaster_jones], [·], [●], [●], [no concatenation at all],

//     [Rose @rose],
//     [●],
//     [●],
//     [·],
//     [entailment is a parameter: no solver, no bound],

//     [MLstruct @mlstruct],
//     [●],
//     [●],
//     [·],
//     [subtyping-constraint solving, and `‖` is unsound under width subtyping],

//     [Castagna @castagna2023programming],
//     [●],
//     [·],
//     [·],
//     [local inference, not let-polymorphic],

//     [Ur @ur], [●], [·], [●], [the programmer supplies disjointness witnesses],

//     [This work], [◐], [●], [●], [precision — but only locally, and marked],
//   ),
// )<trilemma>


// == Our position: two judgements instead of one <our-position>

The design rests on a separation that, to our knowledge, has not been made before in this setting.

In a row-based system, a field selection is ordinarily _elaborated into a row constraint_ and handed to unification: `e.l` demands that `e`'s row contain $l$, and unification must discharge that demand — which, when the row ends in a variable, means guessing that the variable contains the field. Paszke and Xie's search rule @extensible_tabular does exactly this, and it is the point at which the systems discussed in @related-work either backtrack, or solves modulo associativity and commutativity, or defers to an unspecified entailment relation.

We take selection out of unification altogether. *Field demands and row equality are different judgements.* Row unification $scripts(≐)_r$ only ever states structural _equality_ of two rows; it never receives a demand of the form "this row must contain $l$", and consequently never has to guess a field into a variable. Field demands are instead answered by a separate _lookup relation_ $ρ.l ↓ r$ whose result is three-valued — a definite type $τ$, definite absence $⊥$, or _don't know_ $?$ — and a demand that cannot be answered yet simply parks until instantiation unblocks it.

The consequence is that uncertainty is recorded as a *type* rather than as a *constraint*. Where the row theory would need a disjunction, we write ★. What this buys is stated plainly: inference remains ordinary syntactic first-order unification. There is no constraint solver to supply, no entailment relation left as a parameter, no unification modulo AC, and no backtracking over alternative typings. Paszke and Xie's search rule can be dropped entirely, and their conditional tail-check is replaced by an algebraic property of rows rather than a side condition. Every move of the algorithm is forced; where none applies, the configuration is reported as stuck rather than resolved by a guess.



== Contributions <contributions>

+ *Lookup and row equivalence.* A context-free, total lookup $ρ.l ↓ r$ with a three-way result $(τ | ⊥ | ?)$ separates field demands from row equality, and rows modulo ≈ form a trace monoid, which replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ *A type-safe calculus with forced qualified schemes.* Progress (up to lookup errors ↯) and preservation are mechanized in Lean. No plain scheme is principal for `x: x.l`, while a scheme carrying its pending lookups is (@type-safety, @principality).
+ *Sound unification and inference.* Row unification on spines returns most general unifiers, its clash and occurs verdicts are sound, and it terminates. Inference parks unanswered lookups as stumps, terminates, and returns declarative typings (@unification, @inference).
+ *A delimited incompleteness.* Some row equations have no most general unifier and no algorithm returning one answer can solve them; others are stuck at the price of forced moves; inference adds limits of its own (@incompleteness).

The remainder of this thesis is organised as follows. @declarative shows the declarative calculus with functions, scoped records, concatenation, row-variables and let-polymorphism. @unification develops the unification algorithm on spines and @inference the inference algorithm built on it. The metatheory of each component is stated where the component is introduced; @metatheory collects the results about the calculus as a whole — type safety, refinement and the principality argument that forces qualified schemes — and @incompleteness the places where the algorithm gives up. @sec-extensions discusses extensions towards full NixLang — first-class labels, patterns, `with`, `inherit`, occurrence typing, and lacks-predicates as the most promising source of the negative information this section identified as the crux — before @related-work places the work in the literature.

= Declarative <declarative>

#let syntax = figure(
  caption: "The minimal calculus.",
  box(width: 100%, stack(
    spacing: 20pt,
    align(center, flexbox(
      $#type_name("Labels") l ∈ 𝓛$,
      $#type_name("Variables") x ∈ 𝓧$,
      $#type_name("Base types") 𝓫 ∈ 𝓑$,
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
        #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.l ↓ δ⟩ \
        #type_name("Type Scheme") σ & ::= ∀(macron(α): macron(κ)). Q ⇒ τ | τ \
      $
    ],
  )),
)
#syntax <syntax>

@syntax shows the term- and type-syntax of a standard lambda-calculus extended with records, record-concatenation and let-polymorphism. Functions use the unusual syntax (x: e) where x is the variable to be replaced in the function body e. This distinction is chosen because it is Nix's syntax for functions. We admit a finite set 𝓒 of constants $c ∈ 𝓒$ that can be typed by base types 𝓫 from the finite set of base types 𝓑 and require that 𝓑 has at least the types needed to type every constant such that `c: 𝓫_c` is a complete mapping. We admit an "unknown" ★ type for our soft-typing system that can be used to type expressions the type system cannot reason about. Term-rows ${ξ}$ and row-types ${ρ}$ are both trees, which shows their similarity. As usual, we stratify our type system with a polymorphic σ-type that subsumes the monomorphic types τ to sidestep set-paradoxes. Unlike Hindley-Milner schemes, ours are _qualified_: a scheme carries a list Q of pending lookups ⟨ρ.l ↓ δ⟩, called _stumps_, whose result δ is one of the scheme's own quantified variables. A plain scheme is the special case Q = ε. @principality shows that the qualification is forced rather than chosen.


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
      (
        $Γ · (macron(α): macron(κ)) ⊢ τ: "Type"$,
        $∀⟨ρ.l ↓ δ⟩ ∈ Q. #h(0.3em) Γ · (macron(α): macron(κ)) ⊢ ρ: "Row" and δ: "Type" ∈ (macron(α): macron(κ))$,
      ),
      $Γ ⊢ (∀(macron(α): macron(κ)). Q ⇒ τ) #h(3pt) "ok"$,
    ),
  ),
)
#sorting <sorting>

@sorting classifies every type-level phrase as a `Type` or a `Row`. The judgement is almost content-free: τ and ρ are already disjoint syntactic categories, so every closed phrase wears its sort on its face and S-base through S-conc merely walk the grammar. The work is done by S-var and S-ρ-var, which read a *variable*'s sort off the context — Γ records a sort for every type variable in scope, written $α: κ ∈ Γ$. Sorting is therefore not a type system for types but a partition of the grammar: no arrow sort, no sort variables, nothing to infer.

This is deliberately less than Paszke and Xie have. Their kinds are $κ ::= ★ | κ₁ → κ₂ | "Label" | "Row"$, and they need the arrow because their types include type application, first-class rows and label singletons; ours include none of these, so the arrow kind and its application rule would be borrowed machinery. We also avoid their notation, in which ★ _is_ the type kind — here ★ is the unknown type, and the sorts are named instead. A `Label` sort is absent for the same reason: the minimal calculus has concrete labels only. First-class labels, which Nix needs for `e.${e'}`, add a third sort and change nothing else about the discipline.

Two typing rules of @declarative-rules acquire a premise. T-λ-I is the only rule that conjures a type from nothing, so it checks $Γ ⊢ τ₁: "Type"$, and T-let is the only one that conjures a scheme, so it checks $Γ ⊢ σ "ok"$: the body is well-sorted, every stump looks up in a row, and every stump's result is a quantified `Type` variable of σ itself. Every other rule's types are fixed by its premises and are well-sorted whenever they are.


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
        $Γ ⊢ σ #h(3pt) "ok"$,
        $∀τ₁. #h(0.3em) σ ≥ τ₁ ⟹ Γ ⊢ e₁: τ₁$,
        $∃τ₁. #h(0.3em) σ ≥ τ₁$,
        $Γ · (x: σ) ⊢ e₂: τ₂$,
      ),
      $Γ ⊢ #b[let] x = e₁ #b[in] e₂: τ₂$,
    ),
    derive(
      "T-conc",
      ($Γ ⊢ e₁: {ρ₁}$, $Γ ⊢ e₂: {ρ₂}$),
      $Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }$,
    ),
    derive("T-sel", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ τ$), $Γ ⊢ e.l: τ$),
    derive("T-sel-★", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ #h(0.2em) ?$), $Γ ⊢ e.l: ★$),
    derive("T-sel-⊥", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ ⊥$), $Γ ⊢ e.l: ★$),
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
#declarative <declarative-rules>

The declarative system's typing rules follow the standard λ-calculus rules. T-cons is used to type the set of constants of the language with their respective type $𝓫_c$. T-var not only looks up variables in the context Γ, but also instantiates polymorphic types using the instantiation rules from @instantiation discussed in the following section. T-let binds x to any well-sorted scheme σ that is _instance-closed_ for e₁ — every instance of σ is a typing of e₁ — and _inhabited_. The second premise is not bureaucracy: a plain scheme always has its own body as an instance, but a qualified one can have none, and then instance-closure says nothing about e₁ at all. Without it, `let x = (3 4) in 5` would type while being stuck. T-eq equates types equal up to the row-equivalence relation from @row-equivalence. T-conc concatenates two row types by concatenating their type representation, with the right operand first: lookup prefers the left of a row, so left-precedence on rows realizes the right-precedence of ‖. T-sel types record lookups by lifting the hard work to the row-lookup relation, defined in @row-lookup.


T-sel-★ and T-sel-⊥ are needed to type otherwise stuck terms and T-★-intro to blur a type into the unknown. The rules T-rec, T-ξ-empty, T-ξ-field and T-ξ-conc type record literals.


== Instantiation
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
    derive("D-hit", ($(θ ρ).l ↓ τ$, $θ δ = τ$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-⊥", ($(θ ρ).l ↓ ⊥$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-?", ($(θ ρ).l ↓ #h(0.2em) ?$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
  ),
)
#instantiation <instantiation>

@instantiation defines the instance relation $σ ≥ τ$, read »τ is an instance of σ«. It is consumed by T-var, the only rule that ever opens a scheme, and it discharges every quantifier at once: the side condition $Γ ⊢ θ: (macron(α): macron(κ))$ says that θ is the identity outside $macron(α)$ and sends each $α: κ$ to a phrase of sort κ. Instantiation is _predicative_ — the witnesses are monotypes and rows, never schemes — which is what keeps the system a rank-1 HM calculus.

Sorting is what makes this a *single* rule. Without the annotation a quantifier could be discharged with a type or with a row, and the relation needed two rules with identical conclusions differing only in the witness's sort, with nothing to say which was intended and no account at all of a variable standing at positions of both kinds. The sort settles it at the binder: $α: "Row"$ takes a row, $α: "Type"$ takes a type, and θ is one sort-respecting map. This is the concrete payoff of @sorting. The mechanization, which predates it, encodes the same discipline as a *pair* of maps over a single untagged namespace — a workaround the annotation makes unnecessary.

The second premise, _discharge_ $θ ⊨ q$, is what the qualification adds. Each stump is replayed per instance: θ is applied to the stump's row, the lookup is performed, and the stump's result variable δ is pinned to the verdict — to the found type by D-hit, and to ★ by D-⊥ and D-?. These are exactly T-sel, T-sel-⊥ and T-sel-★ of @declarative-rules once more, now evaluated at instantiation time instead of generalization time, which is how one `let`-bound selector can answer a definite type at one use and ★ at another. Discharge reads nothing but the substituted row, so ≥ does not depend on Γ.

Two degenerate cases are worth recording. A monotype scheme has itself as its only instance, so on the monotypes the relation collapses to identity. And with Q = ε the discharge premise is vacuous, so on plain schemes ≥ is the Hindley-Milner instance relation and every plain scheme has its own body as an instance. A qualified scheme need not have any instance — two stumps may pin the same δ to different verdicts — which is why T-let demands inhabitation explicitly.


== Row-Lookup
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

@row-lookup gives the derivation rules for record-type lookups. The judgement $ρ.l ↓ r$ is read as »the lookup of label $l$ in row $ρ$ has result $r$« with $r := τ | ⊥ | #h(0.2em) ?$. The lookup succeeds either with a definite type τ due to a successful lookup, ⊥ when the label is definitely absent, or ? if the search reaches a row-variable before it finds the label. Accordingly, L-ε and L-miss return with a negative lookup result, L-hit with a positive result and the rules L-conc-hit and L-conc-skip recurse into the left and right subtrees a row can form, with left precedence. L-var terminates the search at a row-variable α with the unknown result ?, since α may or may not shadow l, and L-conc-? bubbles such a result up.

The relation is _context-free_: it reads nothing but the row. In particular it never consults what a row-variable has been solved to. That job belongs to substitution — solving $α ≔ ρ'$ replaces α by ρ' in the row, and looking the label up again lets the search advance past the former variable. Refinement of a $?$ is thus plain substitution of a row-variable, at instantiation or at application, followed by a fresh lookup, and never a property of the lookup relation itself.


=== Properties of the lookup relation <lookup-metatheory>

Lookup is deterministic and total: every row shape matches exactly one rule, so each $ρ.l$ has exactly one result. Totality needs no side condition, because the relation never leaves the row it was given. Determinism makes ★ a verdict rather than a choice.

Lookup is stable under substitution. If $ρ.l ↓ r$ with $r ≠ #h(0.2em) ?$, then $(θ ρ).l ↓ θ r$ for every θ, since a definite derivation never reaches a row-variable. Only $?$ may change, to whatever the substituted row yields. This makes parking sound: a deferred lookup can be re-asked after every substitution, and a definite one never needs to be.


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
    derive("≈-rcd", ($ρ₁ ≈ ρ₂$,), ${ρ₁} ≈ {ρ₂}$),
    derive("≈-fn", ($τ₁ ≈ τ₁′$, $τ₂ ≈ τ₂′$), $(τ₁ -> τ₂) ≈ (τ₁′ -> τ₂′)$),
  ),
)
#row_equivalence <row-equivalence>

@row-equivalence gives the row-equivalence rules of our calculus. The relation is an equivalence and a congruence, admits associativity and the units ε, and lets two fields commute only when their labels are distinct. Adjacent fields with the same label keep their order, and so does every field next to a row-variable, since a variable may stand for a row that contains the label and shadowing must be preserved. That $l₁ ≠ l₂$ is decidable for concrete labels is what makes ≈-comm a rule rather than a side problem. ≈-rcd and ≈-fn lift ≈ to types: base types, ★ and type variables are equivalent only to themselves, and ≈ is a congruence below records and arrows, with fields handled by ≈-ext.

=== Row equivalence is a trace monoid <trace-monoid>

Read a row as a word over an alphabet of fields $(l: τ)$ and row-variables α. Two letters may swap when they are fields with distinct labels; a row-variable commutes with nothing, and neither do two fields with the same label. Rows modulo ≈ are then the free partially commutative monoid, a _trace monoid_, on this independence relation, with ε as unit.

This has two consequences. First, ≈ is decidable and has a normal form: two rows are equivalent exactly when they agree on their sequence of row-variables and, for every label, on the subsequence of fields carrying that label. Second, the monoid is _cancellative on both sides_: $ρ | ρ₁ ≈ ρ | ρ₂$ implies $ρ₁ ≈ ρ₂$, and likewise from the right. Unification uses both. It processes a row as a _spine_ from either end and cancels a common prefix or suffix without guessing, and the forced moves of @unification-cascade are counting arguments over this projection.


= Unification <unification>


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

A verdict is either a solution θ or one of four refusals. #u_clash and #u_occurs are the familiar ones; #u_stuck carries the wand-ambiguity, reported when every move is dead but nothing is provably wrong; #u_fuel separates »the budget ran out« from »the problem is unsolvable«, which is what makes every verdict the algorithm does reach independent of the budget it was given.

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

U-bind and U-occurs are stated for a variable on the left and are tried on both sides. U-fn is the only place where the type pass sequences: the solution of the argument equation is applied to the results before they are unified, and the two solutions are composed. A refusal from either premise of U-fn, or from the row problem of U-rcd, is the verdict of the conclusion.

== Row Unification
The row pass is a deterministic cascade. Every move it makes is _forced_ — it preserves the solution set of the problem rather than choosing among alternatives — and no move invents structure. @unification-cascade gives the order in which the moves are attempted; the first one whose trigger fires decides the step, and if none fires the configuration is terminal.

#let u_band = (
  exhaust: oklch(95%, 0.025, 250deg),
  cancel: oklch(95%, 0.028, 195deg),
  solve: oklch(95%, 0.03, 155deg),
  matchp: oklch(95%, 0.035, 95deg),
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

The two exhaustion rules are checked at every depth and before the budget: once a side is empty, its counterpart's variables are forced to ε and a surviving field has nowhere to come from. The cancellation rules exploit that spines cancel at both ends, which is exactly the trace-monoid property of @trace-monoid. U-var-solve fires only when one side is a *lone* variable, so the binding really eliminates it; its occurs-check is the same conservative guard as at the type sort.

The three matching moves differ in how far they may look for a partner. A _window_ is a maximal var-free segment at one end of a spine: U-field-L and U-field-R may pair a field only inside it, because a row-variable in between could be instantiated to a colliding field and the pairing would be a guess about shadowing. U-ground lifts that restriction by *counting*: if one side has no variables at all and some label occurs equally often and positively on both sides, then the other side's variables cannot carry that label, the pairing is positional, and it is again forced. This rule is not derivable from the window rules — the mechanization surfaced it as a genuine gap.

No move places a field into a row-variable. A field facing only variables on the other side is left where it is, even when a single variable is its only possible host: that placement would be forced, but it is not made, and the configuration is reported as stuck. Finally U-clash is a global projection check rather than a per-window one, and U-stuck reports a terminal configuration.

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

@unification-row states the moves as rules. They are to be read *in the order of @unification-cascade*, and U-var-solve, U-var-occurs, U-field-L, U-field-R and U-ground are additionally tried with the two sides exchanged; U-ε-var, U-ε-clash and U-clash are symmetric as stated. Note that no rule ever pushes a *field demand* into a row-variable: field lookups do not travel through $scripts(≐)_r$, they park as stumps, so the algorithm never guesses a field into a variable.

The two passes recurse into each other — U-rcd hands a row problem to $scripts(≐)_r$, and U-field-L, U-field-R, U-ground hand a type problem back to $≐$ — and each cross-call consumes one unit of an explicit budget. This makes the definition structurally recursive, which is what lets the mechanization compute verdicts by `rfl` and check worked examples in the kernel; exhausting the budget is the separate verdict #u_fuel, so the four real verdicts are never an artefact of the bound. Crucially, the type equations a matching move emits are solved *on the spot* and their solution applied to the residual before the row pass continues. Deferring them instead would make #u_stuck meaningless: an equation must be discharged, or fatal, or itself stuck, never merely postponed.



== What unification proves <unification-metatheory>

The algorithm terminates: every problem has a budget at which it returns a verdict, and a verdict reached at one budget is reached, with the same solution, at every larger one.

*Success is most general.* A successful run returns a solution $s$ whose models are exactly the unifiers of the problem, up to agreement on the problem's own variables:

$ {θ : θ "unifies" ρ₁, ρ₂} quad = quad {θ : θ ⊨ s} $

*Clash and occurs are sound.* Either verdict means that no unifier exists.

*Stuck is conservative.* Every move preserves the solution set, but not every solvable problem is solved. Where the algorithm stops, and why it must, is the subject of @incompleteness.


= Inference <inference>

The inference algorithm is a judgement $Γ; S ⊢ e ⇒ τ; S′$ that threads a _solver state_ S through the term. It differs from algorithm W in one respect only: a selection whose lookup answers $?$ is not turned into a row constraint but *parked* as a stump, and the state carries these stumps until a later substitution lets them advance or the end of inference forces them to ★. Unification, as developed in @unification, is only ever called on equations between types; it never sees a field demand.

#let st_park = $⊎$
#let st_drop = $∖$
#let st_solve = $scripts(⇝)^!$
#let st_wake = $↝$
#let st_sat = $scripts(↝)^!$
#let st_fin = $⇓$

#let solver_state = figure(
  caption: "Solver state and the judgements of inference.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "State")[
      $
         #type_name("Solver state") S & ::= (θ, Δ, W) \
        #type_name("Parked stumps") Δ & ::= ∅ | ⟨α ▷ ρ.l ↓ δ⟩, Δ \
         #type_name("Blocked lookup") & (⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α \
      $
    ],
    subbox(caption: "Operations")[
      #flexbox(
        $#type_name("Park a stump") S #st_park q$,
        $#type_name("Drop stumps") S #st_drop Δ′$,
        $#type_name("Add a warning") S "+W"$,
        $#type_name("Draw a name") "fresh" α: κ$,
      )
    ],
    subbox(caption: "Judgements")[
      #flexbox(
        $#type_name("Inference") Γ; S ⊢ e ⇒ τ; S′$,
        $#type_name("Solve, then saturate") S ⊢ τ ≐ τ′ #st_solve S′$,
        $#type_name("Wake one stump") S ⊢ q #st_wake S′$,
        $#type_name("Saturate") S ⊢ Δ #st_sat S′$,
        $#type_name("Finalize one stump") S ⊢ q #st_fin S′$,
      )
    ],
  )),
)
#solver_state <solver-state>

@solver-state fixes the vocabulary. The state is a triple of a sort-respecting substitution θ, a list Δ of parked stumps and a list W of warnings; ⟦S⟧τ applies S's substitution to τ. A parked stump $⟨α ▷ ρ.l ↓ δ⟩$ is a lookup of l in ρ whose result has been promised to the fresh variable δ, annotated with the row-variable α that *blocks* it — the variable at which L-var stopped the search. The blocker is not bookkeeping: it is what lets the algorithm tell, after a solution has been written, which stumps might now advance. Stumps are ordered by the time they were parked, and warnings record every place a ★ was committed, so that the user learns where the analysis gave up.

The state is kept under one invariant, *quiescence*: every stump in Δ is genuinely blocked on the variable it records, $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$. Every solution write can break it — it may solve the very blocker a stump is waiting on — so no equation is ever solved on its own. The judgement $S ⊢ τ ≐ τ′ #st_solve S′$ runs the unifier of @unification and then _saturates_: it re-examines the stumps the solution made stale until the state is quiescent again.

== Inference rules

#let inference = figure(
  caption: "Algorithmic typing rules.",
  flexbox(
    derive("A-cons", (), $Γ; S ⊢ c ⇒ 𝓫_c; S$),
    derive(
      "A-var",
      (
        $x: ∀(macron(α): macron(κ)). Q ⇒ τ ∈ Γ$,
        $"fresh" macron(β): macron(κ)$,
        $S ⊢ Q[macron(β)\/macron(α)] scripts(↝)^(*!) S′$,
      ),
      $Γ; S ⊢ x ⇒ τ[macron(β)\/macron(α)]; S′$,
    ),
    derive(
      "A-λ",
      ($"fresh" α: "Type"$, $Γ · (x: α); S ⊢ e ⇒ τ; S′$),
      $Γ; S ⊢ (x: e) ⇒ α -> τ; S′$,
    ),
    derive(
      "A-app",
      (
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁$,
        $Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$,
        $"fresh" β: "Type"$,
        $S₂ ⊢ τ₁ ≐ (τ₂ -> β) #st_solve S₃$,
      ),
      $Γ; S ⊢ e₁e₂ ⇒ β; S₃$,
    ),
    derive(
      "A-conc",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂ #h(2em) "fresh" ρ₁, ρ₂: "Row"$,
        $S₂ ⊢ τ₁ ≐ {ρ₁} #st_solve S₃ #h(2em) S₃ ⊢ τ₂ ≐ {ρ₂} #st_solve S₄$,
      ),
      $Γ; S ⊢ e₁ ‖ e₂ ⇒ {ρ₂ | ρ₁}; S₄$,
    ),
    derive(
      "A-sel",
      (
        $Γ; S ⊢ e ⇒ τ; S₁$,
        $"fresh" ρ: "Row"$,
        $S₁ ⊢ τ ≐ {ρ} #st_solve S₂$,
        $(⟦S₂⟧ρ).l ↓ τ′$,
      ),
      $Γ; S ⊢ e.l ⇒ τ′; S₂$,
    ),
    derive(
      "A-sel-⊥",
      (
        $Γ; S ⊢ e ⇒ τ; S₁$,
        $"fresh" ρ: "Row"$,
        $S₁ ⊢ τ ≐ {ρ} #st_solve S₂$,
        $(⟦S₂⟧ρ).l ↓ ⊥$,
      ),
      $Γ; S ⊢ e.l ⇒ ★; S₂ "+W"$,
    ),
    derive(
      "A-sel-?",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e ⇒ τ; S₁ #h(2em) "fresh" ρ: "Row" #h(2em) S₁ ⊢ τ ≐ {ρ} #st_solve S₂$,
        $(⟦S₂⟧ρ).l ↓ #h(0.2em) ? "on" α #h(2em) "fresh" δ: "Type"$,
      ),
      $Γ; S ⊢ e.l ⇒ δ; S₂ #st_park ⟨α ▷ ρ.l ↓ δ⟩$,
    ),
    derive("A-rec", ($Γ; S ⊢ ξ ⇒ ρ; S′$,), $Γ; S ⊢ {ξ} ⇒ {ρ}; S′$),
    derive("A-ξ-empty", (), $Γ; S ⊢ ε ⇒ ε; S$),
    derive("A-ξ-field", ($Γ; S ⊢ e ⇒ τ; S′$,), $Γ; S ⊢ (l = e) ⇒ (l: τ); S′$),
    derive(
      "A-ξ-conc",
      ($Γ; S ⊢ ξ₁ ⇒ ρ₁; S₁$, $Γ; S₁ ⊢ ξ₂ ⇒ ρ₂; S₂$),
      $Γ; S ⊢ (ξ₁ | ξ₂) ⇒ (ρ₁ | ρ₂); S₂$,
    ),
  ),
)
#inference <inference-rules>

#let generalization = figure(
  caption: "Generalization.",
  flexbox(
    derive(
      "A-let",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) macron(κ) = S₁(macron(α)) #h(2em) Δ₁ ∼ Δ_q ⊎ Δ_Γ$,
        $macron(α) ∩ "ftv"(⟦S₁⟧Γ) = ∅ #h(2em) macron(α) ∩ "dom"(S₁) = ∅ #h(2em) Δ_q ∩ Δ = ∅$,
        $"results"(Δ_q) "at" S₁ ⊆ macron(α) "injectively" #h(2em) macron(α) ∩ "ftv"(⟦S₁⟧Δ_Γ) = ∅ #h(2em) Δ_q "independent at" S₁$,
        $Γ · (x: ∀(macron(α): macron(κ)). ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ #st_drop Δ_q ⊢ e₂ ⇒ τ₂; S₂$,
      ),
      $Γ; S ⊢ #b[let] x = e₁ #b[in] e₂ ⇒ τ₂; S₂$,
    ),
  ),
)
#generalization <generalization>

@inference-rules gives the syntax-directed counterpart of the declarative rules of @declarative-rules. T-eq and T-★-intro have no algorithmic counterpart: the former is built into row unification, and the latter is never needed to produce a typing, only to blur one. A-cons, A-λ and the record-literal rules are the familiar ones. A-app and A-conc introduce fresh variables for the shapes their premises demand and hand the resulting equations to $#st_solve$; A-conc glues the two row variables together exactly as T-conc does, so concatenation itself never forces a decision.

Selection is where the algorithm departs from W. The subject is first unified with a record ${ρ}$ of a fresh row, and then the lookup relation of @row-lookup is asked about the row *as solved so far*. Its three answers select the three rules. A definite type τ′ is returned by A-sel; a definite absence ⊥ yields ★ and a warning by A-sel-⊥, mirroring T-sel-⊥. Only when the lookup reaches a row-variable α does A-sel-\? fire: it returns a fresh δ in place of the answer it does not have and parks the stump $⟨α ▷ ρ.l ↓ δ⟩$ — the promise that δ will be pinned to whatever the lookup eventually answers. No equation mentioning l is ever emitted, and this is the entire reason row unification never has to guess a field into a variable.

A-var instantiates a qualified scheme with fresh variables of the binder's sorts. The scheme's stumps are instantiated alongside its body and immediately replayed by $scripts(↝)^(*!)$: those that the instantiation already decides are answered on the spot, and the others are parked on the variable that actually blocks them. This is the algorithmic reading of the discharge premise of I-inst (@instantiation) — each use of a `let`-bound variable discharges its own copy of the stumps.

A-let, given separately in @generalization, is the only rule with real content. It generalizes a set $macron(α)$ of variables and *splits* the stumps of $e₁$: $Δ_q$, whose results lie in $macron(α)$, travel into the scheme as its qualifier Q, while $Δ_Γ$ stays parked in the state and outlives the binding. The side conditions each exclude a concrete failure that soundness turned up. The generalized variables must be free in neither Γ nor the solved part of the state; the split is a partition up to order, since a stump that is generalized may have been parked after one that is not; $Δ_q$ consists only of stumps parked while inferring $e₁$; each generalized stump's result, read at $S₁$, is a distinct variable of $macron(α)$; what stays parked mentions none of $macron(α)$, so it reads the same at every instance; and no generalized stump's row mentions another's result, so that an instance's stumps can be discharged one at a time. The algorithm does not choose $macron(α)$: the admissible choices are closed under union, so a greatest one exists and is computed by deleting variables until nothing more is deleted.

== Wake-up and finalization

#let wakeup = figure(
  caption: "Waking and saturating stumps.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Wake one stump")[
      #flexbox(
        derive(
          "K-hit",
          ($(⟦S⟧ρ).l ↓ τ′$, $S ⊢ δ ≐ τ′ ⇝ S′$),
          $S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_wake S′ #st_drop ⟨α ▷ ρ.l ↓ δ⟩$,
        ),
        derive(
          "K-⊥",
          ($(⟦S⟧ρ).l ↓ ⊥$, $S ⊢ δ ≐ ★ ⇝ S′$),
          $S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_wake (S′ #st_drop ⟨α ▷ ρ.l ↓ δ⟩) "+W"$,
        ),
        derive(
          "K-repark",
          ($(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α′$,),
          $S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_wake (S #st_drop ⟨α ▷ ρ.l ↓ δ⟩) #st_park ⟨α′ ▷ ρ.l ↓ δ⟩$,
        ),
      )
    ],
    subbox(caption: "Replay a list of stumps")[
      #flexbox(
        derive("K-nil", (), $S ⊢ ∅ scripts(↝)^* S$),
        derive(
          "K-cons",
          ($S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_wake S₁$, $S₁ ⊢ Δ′ scripts(↝)^* S′$),
          $S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) scripts(↝)^* S′$,
        ),
        derive(
          "K-park",
          (
            $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
            $(S #st_park ⟨α ▷ ρ.l ↓ δ⟩) ⊢ Δ′ scripts(↝)^* S′$,
          ),
          $S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) scripts(↝)^* S′$,
        ),
      )
    ],
    subbox(caption: "Saturate")[
      #flexbox(
        derive("K!-done", ($S "quiescent"$,), $S ⊢ Δ #st_sat S$),
        derive(
          "K!-step",
          (
            $⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ$,
            $¬(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
            $S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_wake S₁$,
            $S₁ ⊢ Δ₁ #st_sat S′$,
          ),
          $S ⊢ Δ #st_sat S′$,
        ),
      )
    ],
  )),
)
#wakeup <wakeup>

#let finalization = figure(
  caption: "Finalization and the entry point.",
  flexbox(
    derive(
      "F-★",
      (
        $⟨α ▷ ρ.l ↓ δ⟩ ∈ Δ$,
        $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
        $S ⊢ δ ≐ ★ ⇝ S′$,
      ),
      $S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_fin (S′ #st_drop ⟨α ▷ ρ.l ↓ δ⟩) "+W"$,
    ),
    derive("F-nil", (), $S ⊢ ∅ scripts(#st_fin)^* S$),
    derive(
      "F-cons",
      ($S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ #st_fin S₁$, $S₁ ⊢ Δ′ scripts(#st_fin)^* S′$),
      $S ⊢ (⟨α ▷ ρ.l ↓ δ⟩, Δ′) scripts(#st_fin)^* S′$,
    ),
    derive(
      "Entry",
      ($∅; (id, ∅, ∅) ⊢ e ⇒ τ; S₁$, $S₁ ⊢ Δ₁ scripts(#st_fin)^* S′$),
      $⊢ e ⇒ τ; S′$,
    ),
  ),
)
#finalization <finalization>

@wakeup gives the judgements that wake stumps. Waking a stump re-asks its lookup against the current state and acts on the verdict, exactly as discharge does in @instantiation: K-hit unifies the promised δ with the found type, K-⊥ pins it to ★ and warns, and K-repark moves the stump to the new blocker α′ when the substitution let the search advance but not finish. The stability lemma of @lookup-metatheory is what makes this sound — a lookup that has once answered definitely answers the same after any further substitution, so a woken stump never needs to be revisited, and only $?$ can change.

$scripts(↝)^*$ replays a whole list, as A-var needs for the stumps of an instantiated scheme. K-park parks a stump that is still blocked, and it reads the blocker off its own premise: the annotation is *determined* by the lookup rather than supplied, so an instantiated stump cannot be filed under a variable that does not actually block it. Saturation $#st_sat$ steps only on _stale_ stumps — those whose recorded blocker no longer blocks — which are exactly the ones a solution write creates, and stops when the state is quiescent. $#st_solve$ is unification followed by saturation, and $scripts(↝)^(*!)$ is $scripts(↝)^*$ followed by saturation.

Finalization, in @finalization, happens once, at the top. Entry runs inference from the empty state and then forces every stump still parked to ★ with F-★, recording a warning for each. Stumps that were generalized by A-let are not affected — they live in their scheme and are discharged per use — and, conversely, nothing is finalized at a `let` boundary: a stump blocked on a variable of Γ may still be answered by an application further out. This is the point at which the remaining $?$ of @sec-motivation become ★, and it is the only point.

== What inference proves <inference-metatheory>

Inference terminates and is sound: a successful run yields a declarative typing once the final substitution is applied,

$ ⊢ e ⇒ τ; S′ quad ==> quad ∅ ⊢ e : ⟦S′⟧τ $

Inference is not complete; @incompleteness lists where it gives up.


= Metatheory <metatheory>

The metatheory of each component is stated where the component is introduced. This section gives the results about the declarative system as a whole. All results of this thesis are mechanized in Lean 4.


== Type Safety <type-safety>

$ "Progress." quad ∅ ⊢ e : τ quad ==> quad e ∈ "Values" or ∃e'. e → e' or e ↯ $
$ "Preservation." quad ∅ ⊢ e : τ and e → e' quad ==> quad ∅ ⊢ e' : τ $

Values are constants, functions and record literals of values, and → is call-by-value small-step reduction, where selecting from a record literal returns the leftmost binding of the label, in agreement with ↓. The judgement $e ↯$ is a _lookup error_: a selection reaches a record literal that lacks the selected label or, with first-class labels, a key that is not a label. The ↯-disjunct is the price of typing an absent field at ★ by T-sel-⊥ (@declarative-rules). Lookup errors arise only at such a selection and are otherwise only propagated. Preservation holds on the nose: the type is unchanged, not merely refined.


== Refinement and the rigidity of ★ <refinement>

Solving a row-variable can only improve a typing. Let ⊑ be the precision order in which ★ is top. For every θ that has an image for each scheme in Γ,

$ Γ ⊢ e : τ quad ==> quad θ Γ ⊢ e : θ τ $

By @lookup-metatheory, definite lookups survive θ and a $?$ re-resolves against the substituted row. For `x: ({l = c} ‖ x).l` with $x : {β}$, the type ${β} → ★$ becomes ${ε} → 𝓫_c$ under $[β ≔ ε]$, while ${ε} → ★$ stays derivable by T-★-intro.

Conversely, ★ is never sharpened: for $τ₀ ≠ ★$ there is no θ with $θ({β} → ★) ⊑ {(l: τ₀)} → τ₀$. This is the rigidity of ★ in unification (@unification-ty), and it drives the principality result below.


== Principality forces qualified schemes <principality>

A scheme σ is _principal_ for e in Γ when all its instances are typings of e, it has one, and every typing of e is matched by an instance at least as precise:

$
  "Principal"(Γ, e, σ) quad :≡ quad (∀τ. #h(0.3em) σ ≥ τ ⟹ Γ ⊢ e : τ) and (∃τ. #h(0.3em) σ ≥ τ) and (∀τ. #h(0.3em) Γ ⊢ e : τ ⟹ ∃τ'. #h(0.3em) σ ≥ τ' and τ' ≼ τ)
$

Here ≼ is ≈ followed by ⊑, since typings are closed under T-eq and T-★-intro while instance sets are not.

*No plain scheme is principal.* `x: x.l` types at ${(l: τ₀)} → τ₀$ for every $τ₀$ and at ${ε} → ★$. A plain scheme covering both must quantify its result, and then also admits ${ε} → {ε}$, which is not a typing. Choosing ★ as result does not help, since ★ is never sharpened.

*A qualified scheme is.* The scheme

$ "selQ" quad = quad ∀(β: "Row", δ: "Type"). #h(0.3em) ⟨β.l ↓ δ⟩ ⇒ {β} → δ $

is principal for `x: x.l`: discharge pins δ to the lookup's verdict, so ${ε} → {ε}$ is excluded. One binding then serves incompatible uses,

$
  ∅ ⊢ #h(0.3em) bold("let") f = (x: x.l) bold("in") { a = f {l = c}; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$

a program the system with plain schemes rejects. Writing L₁ for the system with plain schemes and L₂ for the qualified one, L₂ types strictly more programs than L₁. Qualified schemes are thus forced, not chosen.


= Incompleteness <incompleteness>

The algorithm is sound but not complete. This section collects the places where it gives up, in decreasing order of inevitability. The first family is a limit of the row theory, the second a price paid for forced moves, the third a limit of the inference algorithm on top.

== Irreducible problems <incompleteness-irreducible>

Some row equations have no most general unifier, and no algorithm that returns a single unifier can solve them.

- The _wand_ $(β | α) scripts(≐)_r (l: 𝓫)$: either variable may carry the field, and neither choice subsumes the other.
- The _two-sided_ problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$, and the swap of two variables: the same deadlock at both ends of the spine.
- The _shift_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers either, so returning several answers does not help.

All three are stuck in the algorithm because there is nothing else it could report. The wand and the two-sided problem become solvable with negative information, that is, lacks-predicates on row variables (@sec-extensions). The shift survives it; only row equations as scheme qualifiers would cover it.

What is shown is a limit of principal solutions over scoped rows, not of typing: a program whose inference is stuck may still be typed by placing ★ where the problem sits.

== The price of forced moves <incompleteness-forced>

Two stuck problems do have a most general unifier.

- The _crossfield_ problem $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | β)$ is solved by extending each variable with the other side's field, but that is an expansion of a variable and not a forced pairing. Dropping the expansion move made every move forced, and crossfield is the cost. A repair is to expand only when the host is unique, as an applied binding rather than a rename.
- A stuck equation between payloads is propagated before a residual equation pins the variable it depends on, which masks a solution that exists. Deferring the stuck equation and retrying after the residual repairs it.

Stuck is therefore conservative: it does not imply that no most general unifier exists, not even in a configuration where no move applies.

== Inference <incompleteness-inference>

- *Monomorphic lets.* A `let` generalizes only when its side conditions hold: the bound term's parked lookups must be independent of the environment, and each must have a linear result whose blocker can be filled. When they fail, the binding stays monomorphic and a second use at another record clashes, as in `let g = x: (x.l).m` used on two records. Independence also bites nested selection. A record literal in the result, the same field demanded twice on one row, and an unresolved key are further failures. Ordered discharge would relax independence; the remaining conditions are justified.
- *Spent promises.* A lookup whose answer was already consumed can be blocked on its key rather than on a row, so there is no row to extend and nothing to wake. Such a stump is never answered unless the key is supplied. The declarative system types the program, so this is incompleteness and not rejection. The repairs are to guess the key, which answers non-principally with an invented label, or to report a qualified type at the top level, which is principal but changes what a run returns.
- *Unrestricted T-★-intro.* The declarative system may replace any type by ★, so it types programs the algorithm clashes on: `g: { a = g {l = c}; b = g {m = c} }` has type $(★ → 𝓫) → {a: 𝓫 | b: 𝓫}$, while inference unifies the two argument records and clashes. Every stuck witness above becomes typable in the same way. A clash therefore certifies the absence of a unifier, but not the absence of a typing.


= Extensions to the minimal Calculus <sec-extensions>
- Occurrence typing using if's?
- Inherit statements?
- With-construct?


== Towards Nix
> Section about extended features, limitations etc.


= Related Work <related-work>
_Record concatenation in classic record calculi._ Typing record concatenation is an old and notoriously hard problem. Wand @concat4multiinher first studied type inference for concatenation in the context of multiple inheritance, where the set-or-replace semantics of asymmetric concat already surfaces: his system needs to case-split over which side a field comes from, and typings are unions of alternatives rather than principal types. Harper and Pierce @symm_concat sidestep shadowing by restricting to _symmetric_ concatenation, which is only defined on records with disjoint fields, tracked by compatibility constraints; they also observe that concatenation and width-subtyping do not mix: subtyping can silently forget a field that concatenation later resurrects, breaking soundness — the same observation that steers our calculus away from subsumption and towards row-equivalence. Rémy @concat4free shows that concatenation can be simulated "for free" in a language with polymorphic record extension by abstracting over the extension point, at the price of encoding-style types. Ohori @ohori1995polymorphic obtains efficient compilation for a polymorphic record calculus, but restricts records to selection and functional update — concatenation is exactly the operation his index-passing compilation scheme cannot support. In the disjoint-polymorphism line @xie2020row the merge operator subsumes symmetric concatenation, with disjointness playing the role of the lacks-constraints. All of these systems either forbid the colliding case that makes Nix's `‖` interesting, or pay for it with non-principal or encoded types; none types the motivating example `a: b: (a ‖ b).l` as-is.

_Scoped rows and first-class labels._ Our row theory descends from Leijen's extensible records with scoped labels @extensible_recs, where duplicate labels are kept in the row and lookup resolves them with left-precedence — precisely the "bag" semantics that makes asymmetric concat a total operation instead of a partially defined one. Leijen later added first-class labels @fc_labels, which Nix needs for its dynamic field selection `e.${e'}`. Paszke and Xie @extensible_tabular combine both into infix-extensible rows with a unification-based inference algorithm over row- and label-variables; their system is the direct basis of ours. It cannot, however, model set-or-replace: extension always happens on a known side of the row, and their conditional tail-check rejects programs whose shadowing behaviour is unresolved — our lookup relation instead accepts them at ★ and refines later.

_Expressive row theories._ The line of work started by Morris and McKinna @rose abstracts rows behind an algebra of containment and combination constraints strong enough to type asymmetric concatenation faithfully, and has been extended to generic programming @generic_with_extensible, extensible recursive functions @extensible_rec_funcs and ad-hoc polymorphism @extensible_data_adhoc. These systems track strictly more information than ours, and Rose is in fact a rank-1 Hindley-Milner language that does establish principal types — but the principality is that of qualified types @qualified_types: inference produces a principal _constrained_ scheme and defers the row predicates to an entailment relation that the framework leaves as a parameter, required only to be invariant under row equivalence, monotone and transitive, and nowhere shown to be decidable. The entailment rules actually given decide only ground predicates and discharge everything else by assumption lookup, so no solving procedure — and hence no complexity bound — is offered for the predicates with row variables that inference actually generates. Since combination is an equation in a partial monoid, deciding conjunctions of such predicates is unification modulo associativity and commutativity rather than syntactic unification, and is NP-complete @ac_unification. The successor systems move to System Fω and are explicitly typed, with type reconstruction left as future work @generic_with_extensible; unrestricted second-order reconstruction is in any case undecidable @undecidable. Sulzmann @designing_record_systems designs record systems in the HM(X) framework, where concatenation becomes a constraint; HM(X) however only stipulates that a constraint solver exists without providing one. Our position is dual: we keep plain unification-based inference and instead weaken the types themselves with ★ where the row theory would need a disjunction.

_Subtyping-based systems._ Algebraic subtyping @algebraic_subtyping @mlsub and its simplifications @simplesub give principal inference for structural subtyping, and MLstruct @mlstruct extends this to a Boolean algebra of types with unions, intersections and negations — negation being one way to express the absence information that shadowing destroys. However, these systems support record extension and field update rather than general concatenation of unknown records, and by the width-subtyping argument above @symm_concat, adding `‖` to a subtyping-based system is problematic at the core: a record can always forget the very fields that decide precedence. We deliberately keep our system subtyping-free; the only ordering is the precision gained by instantiation.

_Set-theoretic types and dynamic languages._ Castagna's programme of semantic subtyping @frisch_semantic @castagna2023programming types dynamic languages with unions, intersections and negations, including detailed accounts of records, maps and structs @typing_records_etc and polymorphic records for Elixir @poly_records, together with occurrence typing to refine types along control flow @revisiting_occurrence @on_occurrence and a gradual guard-based system deployed for Elixir @gradual_elixir. This is the most expressive treatment of records for a dynamic language to date, and occurrence typing is a natural future extension of our calculus (@sec-extensions). The cost is the full set-theoretic machinery: inference is local rather than let-polymorphic, and the subtyping problems with concatenation resurface. Our ★ plays a role similar to their `dynamic()` @elixir_design_principles, but is introduced by the _lookup relation_ itself rather than by explicit annotation.

_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all, and has been instantiated for extensible rows by Sekiyama and Igarashi @gradual_extensible_rows, the system closest in spirit to our ★-typed rows. We differ in a fundamental way: Nix programs cannot be instrumented, so there are no casts, no blame, and no runtime monitoring. Our system is instead a _soft_ typing system in the tradition of Cartwright and Fagan @soft_typing @practical_soft_typing and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory honestly reports the residual risk as the ↯-disjunct of progress (@type-safety). Industrial gradual systems such as TypeScript @typescript and Flow @flow make the same pragmatic choice of an unsound `any`, but without a formal account of when `any` arises; in our system ★ is introduced only by the lookup relation and T-★-intro, and its origin is therefore always explainable.

_Typing Nix._ Work on Nix itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for the Nix expression language, but do not attempt a type system. An earlier system by the author @simplenix applies off-the-shelf HM inference to a Nix subset and fails exactly on the record operations this thesis addresses. The long-standing community issue @nix-ts-issue documents both the demand for and the difficulty of typing Nix; Nickel @nickel, a Nix-inspired configuration language, opts for gradual typing with row polymorphism but forbids the colliding concatenations we target.


= Conclusion
We have shown that we have such a nice type system with so many (much wow) nice properties and we are very happy and thank all the people that helped us accomplish such an outstanding result wow nice.

#bib
