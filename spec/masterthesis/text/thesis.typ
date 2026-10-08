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
  "Department of Computer Science - University Freiburg",
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
      Asymmetric record concatenation with right-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that has no principal type in a row calculus that must decide where a field comes from: the expression `a: b: (a ‖ b).l` concatenates two records of unknown shape, and because of field-precedence and shadowing behaviour, whether l is taken from a or from b is only known once b is instantiated.
      We propose a novel [_soft type system_](¡our system can fail) based upon the work of Paszke and Xie with scoped records, row-variables, asymmetric record concatenation, let-polymorphism, first-class labels, row equivalence and an unknown type that marks the places where the so-called wand-ambiguity is left unresolved. We mechanically prove _type safety_ of the declarative system with qualified types in Lean, together with soundness and termination of an incomplete, unification-based inference algorithm.
    ]
  ]
]

#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

= Introduction <sec-motivation>
// meta: this paragraph still feels all over the place
The Nix programming language @nix-language-2-28 @dolstra_phd (NixLang) is the language of nixpkgs, a single repository of more than four million lines of untyped functional code#footnote[Counted over the `.nix` files of nixpkgs 26.05.] and one of the ten GitHub projects with the most contributors @octoverse2024. It is a language with features that extend well beyond the basic λ-calculus. Its foundational data structure is the _attribute set_ — a record […](maybe add that it is important for configuration?) — and the language provides a wide range of constructs and builtin functions to create, extend, deconstruct and inspect them. NixLang powers nixpkgs, a package repository of more than 100,000 packages @repology that is continuously evaluated, updated and rolled out from one central repository, and the same repository carries the Nix standard library, the NixOS module system and the definition of the NixOS distribution itself @nixos_short @nixos_long. Every one of those artefacts is an attribute set assembled out of other attribute sets. NixLang is thus both the motivation for our work and the guiding principle behind the features of our calculus, MiniNix.

// meta: maybe one or to more paragraphs before we disclose our goal
Our aim is a type system that applies to this existing body of code as it is and helps its programmers write safer code.

From our goal to type existing NixLang code and create an applicable type system, we derive our design constraints. Since the ultimate goal is to aid programmers today, we need an _efficiently computable_ algorithm that knows its own limitations. Using only the builtin functions, it is possible to write code that no static analysis can type completely:

```nix
{ "1759190400" = 1; }.${toString builtins.currentTime}
```

This simple example combines records, impure functions and first-class labels to dynamically look up a record field based on the system time at program execution. Whether this program will succeed or error cannot be determined statically, as deciding it would predict the time of execution and, as such, the future. [¡](this conclusion comes "too fast") Our type system therefore includes an _unknown type_ ★, which marks the places where type inference gives up in order to preserve termination and soundness. It absorbs uncertainty of two kinds: behaviour that is dynamic by nature, as in the example, and placeholders for source code that could be typed by a stronger type system in the future.

// meta: also too fast and out of place
In conclusion, we state the following design goals for a reasonable type system for NixLang. @sec-goals assesses how far it reaches each of them.

/ R1 (No source changes): The analysis should run on today's nixpkgs verbatim. No program should need rewriting to become analysable.
/ R2 (No semantic change): Evaluation should produce exactly what it produced before, down to derivation hashes.
/ R3 (Totality): Every program that runs should receive a type. Where the analysis cannot decide, the result should be ★ or a reported stuck verdict rather than a rejection.
/ R4 (Effective inference): The analysis should scale to a repository of more than 100,000 packages, evaluated as one program.

// meta: maybe a small paragraph to tell why each goal is important
// meta: other letter than r (= requirement)

== The Problem: Asymmetric Concatenation
#figure(
  caption: [The three idioms that make asymmetric concatenation unavoidable.],
  box(width: 100%, align(left, ```nix
  # 1. an overlay: extend or replace fields of a package set given to us
  self: super: { hello = super.hello // { meta = { broken = false; }; }; }

  # 2. an override: replace parts of an argument set we did not build
  mkDerivation (args // { buildInputs = args.buildInputs ++ [ extra ]; })

  # 3. a conditional extension: add fields only on some platforms
  { pname = "hello"; } // lib.optionalAttrs stdenv.isDarwin { NIX_LDFLAGS = "-liconv"; }
  ```)),
)<nix-idioms>

NixLang is a configuration language, and its features determine those of MiniNix. Attribute sets in NixLang are written as a sequence of bindings separated by semicolons, `{ a = 1; b = 2; }`, and two of their operations determine the type system needed to support them: (1) asymmetric record concatenation and (2) first-class labels.

Asymmetric record concatenation, written `//` in NixLang and ‖ in this thesis, is a record operation that uses the fields of the first operand and extends or overwrites them with the fields of the second operand. The hardest part of this operation is the faithful tracking of labels under abstract operands. The prime example is the wand-ambiguity [](first entdeckt by wand \@ref) of `(a ‖ b).l`. With $a : {α}$ and $b : {β}$, the selection must resolve $l$ in $(β | α)$. To answer it we must know whether $β$ contains $l$: if it does, the answer is $β$'s binding for $l$; if it definitely does _not_, the answer is $α$'s. Neither is known, and no amount of positive information about $β$ can settle it.

// meta: doubles the previous paragraph, also mixed up strangely
Overwriting existing fields is one of the most important features of NixLang because it is used ubiquitously to extend or overwrite the package set, to override the arguments of a package and to add fields conditionally. An example of all three cases can be seen in @nix-idioms. Our answer rests on two ingredients. _Scoped records_ keep every field of both operands, so that concatenation never loses information, and a three-valued _lookup relation_ answers a field demand with a type, with definite absence, or with _don't know_; the last answer is recorded as the unknown type ★ rather than resolved by a guess. @sec-overview shows both at work, and @sec-background positions them against the existing record calculi.

== Contributions <contributions>

+ Lookup separated from row equality. Field selection is answered by a lookup relation on rows with three results: a type, definite absence, or _don't know_. Row unification only equates rows and never receives a field demand. Rows up to equivalence form a trace monoid, which yields a normal form and replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ MiniNix, a type-safe calculus whose schemes carry pending lookups. Progress up to lookup errors and preservation are mechanized in Lean. No plain Hindley-Milner scheme is principal for `x: x.l`, while a scheme that carries its pending lookup and replays it at every use is (@type-safety, @principality). The results hold for MiniNix extended with first-class labels (@sec-fc-labels).
+ Sound and terminating unification and inference. When row unification succeeds, it returns a most general unifier, and its failure verdicts are sound. Inference defers a lookup that cannot be answered yet until a later substitution decides it, terminates, and produces declarative typings (@unification, @inference).
+ Delimited incompleteness. Some row equations have no most general unifier, so no algorithm that returns a single unifier can solve them. Others have one that the algorithm does not find, because it makes only moves that preserve every solution. Inference adds limits of its own (@incompleteness).[¡](this should be reworded)

The remainder of this thesis is organized as follows. @sec-overview introduces MiniNix through examples and @sec-background recalls the record calculi it builds on. @declarative presents the declarative calculus with functions, scoped records, concatenation, row-variables and let-polymorphism, and @metatheory establishes its type safety, refinement and the principality argument that forces qualified schemes. @unification develops the unification algorithm on spines and @inference the inference algorithm built on it, each with its metatheory, and @incompleteness collects the places where the algorithm gives up. @sec-mechanization maps the results to the Lean development. @sec-extensions discusses extensions towards full NixLang, @sec-discussion measures the result against the design goals stated above, and @related-work places the work in the literature. [!](sounds weird)

= Overview <sec-overview>
This section introduces MiniNix informally through examples. The formal definitions follow in @declarative.

== Scoped Rows and Three-Valued Lookup
With _scoped records_, concatenation of record types becomes juxtaposition of rows. MiniNix uses rows ρ that hold label-type bindings and row-variables. Such a row ρ can be put into curly braces to form a record type {ρ}. [¡](I don't like the introduction) For concatenation, the rows of both operands, {ρ} and {ρ'}, are joined to form {ρ | ρ'}, and no information is lost. We combine this with a lookup relation of the form $ρ.l ↓ r$ (@row-lookup) that traverses the natural structure of rows to look up a label l in the row ρ. The result r can be of three kinds:

The first two results are a type τ, if the lookup finds the label, and a marker ⊥, if the label is definitely absent. A row can consist of fixed label-type bindings but also row-variables, and our lookup relation stops at these, returning a result ? because shadowing behaviour after this point is not clear. [](good!) Substitution solves those row-variables and lets the lookup advance further into the record. A $?$ that no substitution resolves is typed at the unknown type ★. [¡](This is somehow »nichtssagend«)


== Selection
The smallest [interesting](bad word) program is a selector, `x: x.l`. Its argument must be a record ${ρ}$, and the lookup $ρ.l$ decides the result. For $ρ = (l: τ₀)$ the lookup hits and the selector types at ${(l: τ₀)} → τ₀$; for $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$, where ★ marks a selection that fails whenever it is run.[¡](this sounds bad if you don't know that ★ is usually only uncertainty)  For a row-variable $ρ = β$ the lookup answers $?$, and the result is ★ as well.


== The Wand-Ambiguity
The motivating example concatenates two unknown records and selects from the result:
$ #b[let] f = (a: b: (a ‖ b).l) #b[in] … $
With $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$ — the right operand first, since lookup prefers the left of a row. The lookup $(β | α).l$ stops at β with $?$: whether the field comes from b or from a depends on whether β contains l. A plain type can only record this as ${α} → {β} → ★$. MiniNix instead keeps the pending lookup in the scheme of f,
$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $
and replays it at every use: `f {l = c₁} {}` looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$, `f {l = c₁} {l = c₂}` yields $𝓫_(c₂)$ because the right operand shadows, and `f {} {}` yields ★. Each use receives the most precise type its arguments permit.[](that is remarkable)


== One Binding, Two Uses
The same mechanism lets a single binding serve incompatible uses,
$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$
The first use needs the result of f to be a record, the second applies f to a record without l. No plain scheme covers both, and @principality shows that this forces qualified schemes.


== A Stump Wakes Up
[Inference cannot always answer a lookup when it meets it](wording). In `(x: x.l) {l = c}`, the selector is inferred first: x receives a fresh row ρ, the lookup $ρ.l$ answers $?$, and inference returns a fresh δ for the result while _parking_ the pending lookup as a stump $⟨ρ ▷ ρ.l ↓ δ⟩$, blocked on ρ. Applying the selector then solves $ρ ≔ (l: 𝓫_c)$. The solution unblocks the stump, the lookup is re-asked and hits, and δ is unified with $𝓫_c$. For `(x: x.l) {}` the same stump wakes to definite absence, δ becomes ★, and a warning records the place. Row unification itself never sees the field demand; @inference develops this mechanism in full.

= Background <sec-background>
[This section recalls the record calculi the thesis builds on and states which of their features it keeps](wording).

== Row Polymorphism
In Hindley-Milner inference with rows @concat4multiinher @remy_typechecking, a record type ${ρ}$ is built from a row ρ of label-type pairs that may end in a row-variable. A function that selects l from its argument is [polymorphic in the rest of the row](interesting), and unification solves row-variables as it solves type variables. Most such calculi keep labels unique within a row, so that extension must demand the absence of the label it adds.

== Scoped Labels
Leijen's scoped labels @extensible_recs drop uniqueness. A row may contain a label several times, and lookup resolves duplicates with left-precedence. Extension is then total, and a shadowed field is kept rather than removed. [This is the row theory of MiniNix, extended from extension by a single field to the concatenation of two rows](this is actually inspired by @extensible_tabular).

== Why Not Symmetric Concatenation, Subtyping or Lacks-Predicates
The [ubiquity](wording) of overwriting rules out record calculi that restrict themselves to _symmetric concatenation_ @symm_concat, in which both operands have disjoint labels. It also rules out another feature commonly seen in record calculi. Using _width-subtyping_, one can remove or forget fields ${l: τ} <= {}$ in a record. This, in combination with asymmetric record concatenation, [leads to unsoundness, because previously forgotten and then untracked fields overwrite existing fields semantically but cannot be tracked by the record system](good). Lacks-predicates @gaster_jones @qualified_types can salvage this situation by denoting the absence of a field. They are absent from MiniNix not because they would burden the programmer — in a system without annotations, nobody writes them — [but because nothing in MiniNix generates them](recheck, nix has constructs that generate them). [Gaster and Jones need them because record extension is partial and its typing rule must demand absence. With scoped rows, concatenation is total, no rule demands anything, and a constraint form without an introduction site buys nothing](is this really an argument?). [The NixLang constructs that would generate them — closed function patterns, `removeAttrs` and `?`-guards under `if` — lie outside MiniNix, which is why @sec-extensions returns to them as the most promising extension](reduce this somehow, either drop entirely or…).
We therefore keep MiniNix subtyping-free; the only ordering it admits is the precision gained by instantiation and row equivalence (≈).

// meta: this is somewhat out of place. Put it in front mayebe?
// meta: Maybe clear up the heritance right in the abstract?
== Paszke and Xie
Paszke and Xie @extensible_tabular combine scoped labels with first-class labels into infix-extensible rows and give a unification-based inference algorithm over row- and label-variables. Their system is the direct basis of ours. Its field selection is handled by a search rule inside unification, and a conditional tail-check rejects programs whose shadowing behaviour is unresolved.

The difference shows already on the selector `x: x.l`. The search rule turns the selection into the demand that the row of x contain l, and since that row is a variable, unification places the field into it: the selector types at ${(l: δ | ρ)} → δ$. Every use must then supply l, so `(x: x.l) {}` is rejected, and so is the program of @principality that applies one selector to a record with l and to one without. MiniNix answers the lookup with $?$ instead, keeps it as a stump, and types the selector with the qualified scheme selQ, whose instances include ${ε} → ★$. On the motivating example `a: b: (a ‖ b).l` the search must decide whether the row of b contains l, which the tail-check cannot, while MiniNix again keeps the lookup as a stump (@sec-overview).


== Our Position: Two Judgements Instead of One <our-position>

The design rests on a separation between field demands and row equality.

[In a row-based system, a field selection is ordinarily _elaborated into a row constraint_ and handed to unification: `e.l` demands that `e`'s row contain $l$, and unification must discharge that demand — which, when the row ends in a variable, means guessing that the variable contains the field. Paszke and Xie's search rule @extensible_tabular does exactly this, and it is the point at which the systems discussed in @related-work either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.](best sentence so far)

[We take selection out of unification altogether](!). Field demands and row equality are different judgements. Row unification $scripts(≐)_r$ only ever states structural _equality_ of two rows; it never receives a demand of the form "this row must contain $l$", and consequently never has to guess a field into a variable. Field demands are instead answered by a separate _lookup relation_ $ρ.l ↓ r$ whose result is three-valued — a definite type $τ$, definite absence $⊥$, or _don't know_ $?$ — [and a demand that cannot be answered yet simply parks until instantiation unblocks it](explain parking a bit more?).

The consequence is that a field demand is never a constraint to be solved. A demand that cannot be answered yet is either answered later, [by running the lookup again on a more instantiated row, or recorded as ★](this is the gist). [Schemes do carry such pending lookups as qualifiers](good) (@principality), but their entailment is evaluation: a qualifier is discharged by computing a total and deterministic lookup, never by search. Where the row theory would need a disjunction, we write ★. [As a result, inference remains ordinary syntactic first-order unification.](good) There is no constraint solver to supply, no entailment relation left as a parameter, no unification modulo AC, and no backtracking over alternative typings. Paszke and Xie's search rule can be dropped entirely, and their conditional tail-check is replaced by an algebraic property of rows rather than a side condition. Every move of the algorithm is forced; where none applies, the configuration is reported as stuck rather than resolved by a guess.
// meta: maybe not that this is due to efficiency reasons?

= The Declarative Calculus <declarative>

#let syntax = figure(
  caption: "The syntax of MiniNix.",
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

@syntax shows the term- and type-syntax of a standard λ-calculus extended with records, record concatenation and let-polymorphism. Functions use the unusual syntax (x: e) where x is the variable to be replaced in the function body e. This NixLang's syntax for functions. We admit a finite set 𝓒 of constants $c ∈ 𝓒$ that can be typed by base types 𝓫 from the finite set of base types 𝓑 and require that 𝓑 has at least the types needed to type every constant such that $c ↦ 𝓫_c$ is a complete mapping. We admit an "unknown" ★ type for our soft-typing[-inspired](?) system that can be used to type expressions the type system cannot reason about. Term-rows ${ξ}$ and row-types ${ρ}$ are both trees, which shows their similarity. As in Hindley-Milner, polymorphism is stratified: a scheme σ quantifies over monotypes and rows only, which keeps instantiation predicative and inference decidable. Unlike Hindley-Milner schemes, ours are _qualified_: a scheme carries a list Q of pending lookups ⟨ρ.l ↓ δ⟩, called _stumps_, whose result δ is one of the scheme's own quantified variables. A plain scheme is the special case Q = ε. @principality shows the addition of qualification is forced by the language constraints.


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

@row-lookup gives the derivation rules for record-type lookups. The judgement $ρ.l ↓ r$ is read as "the lookup of label $l$ in row $ρ$ has result $r$" with $r := τ | ⊥ | #h(0.2em) ?$. The lookup succeeds either with a definite type τ due to a successful lookup, ⊥ when the label is definitely absent, or ? if the search reaches a row-variable before it finds the label. Accordingly, L-ε and L-miss return with a negative lookup result, L-hit with a positive result and the rules L-conc-hit and L-conc-skip recurse into the left and right subtrees a row can form, with left precedence. L-var terminates the search at a row-variable α with the unknown result ?, since α may or may not shadow l, and L-conc-? bubbles such a result up.

// meta: context-freeness is an old artifact. Don't flex it.
// maybe add an intuition section explaining my own reasoning
The relation is _context-free_: it reads nothing but the row. Solving $α ≔ ρ'$ replaces α by ρ' in the row, and looking the label up again lets the search advance past the former variable. Refinement of a $?$ is thus plain substitution of a row-variable, at instantiation or at application, followed by a fresh lookup, and never a property of the lookup relation itself.


=== Properties of the Lookup Relation <lookup-metatheory>

#lemma(name: [Determinism and totality], lean: "lookup_total, LookupQ.det")[
  For every row ρ and label l there is exactly one lookup result r with $ρ.l ↓ r$.
] <lem-lookup-det>

#proof[By induction on ρ: every row shape matches exactly one rule.]

[Determinism makes ★ a verdict rather than a choice](wording).
// meta: remove the mention of a possible side condition which is not clear what it is.


// meta: parking is not introduced yet, no?
#lemma(name: [Stability under substitution], lean: "lookup_applySubst")[
  If $ρ.l ↓ r$ and $r ≠ #h(0.2em) ?$, then $(θ ρ).l ↓ θ r$ for every substitution θ.
] <lem-lookup-stable>

#proof[By induction on the derivation of $ρ.l ↓ r$. A definite derivation never uses L-var, so it never reaches a row-variable, and θ acts on no part of the row it inspects.]

Only $?$ may change, to whatever the substituted row yields. This makes parking sound: a deferred lookup can be re-asked after every substitution, and a definite one never needs to be.


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

@row-equivalence gives the row-equivalence rules of MiniNix. The relation is an equivalence and a congruence, admits associativity and the units ε, and lets two fields commute only when their labels are distinct. Adjacent fields with the same label keep their order, and so does every field next to a row-variable, since a variable may stand for a row that contains the label and shadowing must be preserved. For concrete labels the premise $l₁ ≠ l₂$ of ≈-comm is decidable, so ≈ needs no separate constraint on labels. ≈-rcd and ≈-fn lift ≈ to types: base types, ★ and type variables are equivalent only to themselves, and ≈ is a congruence below records and arrows, with fields handled by ≈-ext.

=== Row Equivalence Is a Trace Monoid <trace-monoid>
//meta: this section could use some more life
Read a row as a word over an alphabet of fields $(l: τ)$ and row-variables α. Two letters may swap when they are fields with distinct labels; a row-variable commutes with nothing, and neither do two fields with the same label. Rows modulo ≈ are then the free partially commutative monoid, a _trace monoid_, on this independence relation, with ε as unit.

This has two consequences. First, ≈ is decidable and has a normal form.

#theorem(name: [Characterization of ≈], lean: "rowEquiv_iff_char")[
  $ρ₁ ≈ ρ₂$ if and only if ρ₁ and ρ₂ have the same sequence of row-variables and, for every label l and every var-free segment, the subsequences of fields labelled l agree pointwise up to ≈.
] <thm-char>

Equivalently, the projections of the two rows onto every pair of dependent letters coincide, and sorting each var-free segment stably by label yields a representative of the class. For example, $(m: 𝓫 | l: 𝓫 | α | l: 𝓫′ | m: 𝓫)$ splits into the segments $(m: 𝓫 | l: 𝓫)$ and $(l: 𝓫′ | m: 𝓫)$ around α, and its representative is $(l: 𝓫 | m: 𝓫 | α | l: 𝓫′ | m: 𝓫)$: the first segment is reordered, and no field crosses α. Second, the monoid is cancellative.

#lemma(name: [Cancellativity], lean: "RowEquiv.cancel_cat_left, RowEquiv.cancel_cat_right")[
  If $(ρ | ρ₁) ≈ (ρ | ρ₂)$ or $(ρ₁ | ρ) ≈ (ρ₂ | ρ)$, then $ρ₁ ≈ ρ₂$.
] <lem-cancel>

Unification uses both. [to…](todo) It processes a row as a _spine_ from either end and cancels a common prefix or suffix without guessing, and the forced moves of @unification-cascade are [counting arguments](wasn't introduced!) over the projection onto a single label.


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


@sorting, in @app-sorting, classifies every type-level phrase as a `Type` or a `Row`. Since τ and ρ are disjoint syntactic categories, the only rules with content are S-var and S-ρ-var, which read the sort of a variable off Γ. The sorts are deliberately fewer than the kinds of Paszke and Xie @extensible_tabular, $κ ::= ★ | κ₁ → κ₂ | "Label" | "Row"$: the arrow kind serves type application, first-class rows and label singletons, none of which MiniNix has, and we name the sorts because ★ is the unknown type here. First-class labels add a `Label` sort and change nothing else about the discipline (@sec-fc-labels).

// meta: remove conjure
[Two typing rules of @declarative-rules acquire a premise](why not mention them??). T-λ-I is the only rule that conjures a type from nothing, so it checks $Γ ⊢ τ₁: "Type"$, and T-let is the only one that conjures a scheme, so it checks $Γ ⊢ σ "ok"$: the body is well-sorted, every stump looks up in a row, and every stump's result is a quantified `Type` variable of σ itself. Every other rule's types are fixed by its premises and are well-sorted whenever they are.


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

[The declarative system's typing rules follow the standard λ-calculus rules](wording). T-cons is used to type the set of constants of the language with their respective type $𝓫_c$. T-var not only looks up variables in the context Γ, but also instantiates polymorphic types using the instantiation rules from @instantiation discussed in the following section. T-let binds x to any well-sorted scheme σ that is _instance-closed_ for e₁ — every instance of σ is a typing of e₁ — and _inhabited_. The inhabitation premise is necessary: a plain scheme always has its own body as an instance, but a qualified one can have none, and then instance-closure says nothing about e₁ at all. Without it, `let x = (3 4) in 5` would type while being stuck. T-eq equates types equal up to the row-equivalence relation from @row-equivalence. T-conc concatenates two row types by concatenating their type representation, with the right operand first: lookup prefers the left of a row, so left-precedence on rows realizes the right-precedence of ‖. T-sel types a selection by delegating to the row-lookup relation of @row-lookup.

// meta: the reader already read something similar before
Schemes are qualified because a `let`-bound selector must answer differently at different uses. In `let f = x: x.l in …`, the lookup in the body of f reaches the row-variable of x and can only answer $?$. A plain scheme has to fix that answer once, at generalization: either as ★ for every use, or as a variable that some instance sends to a type the lookup never produces. A qualified scheme keeps the lookup as a stump $⟨β.l ↓ δ⟩$ and replays it at every instance, so that δ becomes the found type where the argument has l and ★ where it does not (@instantiation). @principality shows that no plain scheme is principal for `x: x.l`, so the qualification is forced rather than chosen.

// meta: expand this section bescause these two rules are non-standart
T-sel-★ and T-sel-⊥ are needed to type otherwise stuck terms and T-★-intro to blur a type into the unknown. The rules T-rec, T-ξ-empty, T-ξ-field and T-ξ-conc type record literals.

#example(name: [Two ways to type a selection at ★])[
  The selection `{}.l` has type ★ by T-sel-⊥, since $ε.l ↓ ⊥$, and it evaluates to a lookup error by ↯-sel. In a context with $β: "Row"$, the selector `x: x.l` has type ${β} → ★$ by T-sel-★, since $β.l ↓ #h(0.2em) ?$. Applied to `{l = c}` it evaluates to c, applied to `{}` to a lookup error. Both selections receive ★, but T-sel-⊥ marks a selection that fails whenever it is evaluated, while T-sel-★ marks one whose outcome the row does not yet determine.
] <ex-sel-star>

T-sel-⊥ is needed even for programs that never fail. Record fields are evaluated only when selected (@semantics), so `{a = {}.l; b = c}.b` evaluates to c. Without T-sel-⊥ the field a has no type, and neither does the program.


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
    derive("D-hit", ($(θ ρ).l ↓ τ$, $θ δ ≈ τ$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-⊥", ($(θ ρ).l ↓ ⊥$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-?", ($(θ ρ).l ↓ #h(0.2em) ?$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
  ),
)
#instantiation <instantiation>

// meta: too verbose
@instantiation defines the instance relation $σ ≥ τ$, read "τ is an instance of σ". It is consumed by T-var, the only rule that ever opens a scheme, and it discharges every quantifier at once: the side condition $Γ ⊢ θ: (macron(α): macron(κ))$ says that θ is the identity outside $macron(α)$ and sends each $α: κ$ to a phrase of sort κ. Instantiation is _predicative_ — the witnesses are monotypes and rows, never schemes — which is what keeps the system a rank-1 HM calculus.
The second premise, _discharge_ $θ ⊨ q$, is what the qualification adds. Each stump is replayed per instance: θ is applied to the stump's row, the lookup is performed, and the stump's result δ is pinned to the verdict — to the found type up to row equivalence by D-hit, and to ★ by D-⊥ and D-?. Asking for ≈ rather than equality in D-hit loses nothing, since T-eq already closes every set of instances under ≈. It matters for inference, which can guarantee the found type only up to ≈ when the stumps of one instance are discharged in turn. These are exactly T-sel, T-sel-⊥ and T-sel-★ of @declarative-rules once more, now evaluated at instantiation time instead of generalization time, which is how one `let`-bound selector can answer a definite type at one use and ★ at another.

#example(name: [Instances of selQ])[
  Consider $"selQ" = ∀(β: "Row", δ: "Type"). ⟨β.l ↓ δ⟩ ⇒ {β} → δ$ of @principality.
  - $θ = [β ≔ (l: 𝓫), δ ≔ 𝓫]$ discharges the stump by D-hit, since $(l: 𝓫).l ↓ 𝓫$, so ${(l: 𝓫)} → 𝓫$ is an instance.
  - $θ = [β ≔ ε, δ ≔ ★]$ discharges it by D-⊥, since $ε.l ↓ ⊥$, so ${ε} → ★$ is an instance.
  - $θ = [β ≔ ε, δ ≔ {ε}]$ violates D-⊥, which demands $θ δ = ★$. Hence ${ε} → {ε}$ is not an instance, although it is an instance of the plain scheme $∀(β: "Row", δ: "Type"). {β} → δ$.
] <ex-selq-inst>

Two degenerate cases are worth recording. A monotype scheme has itself as its only instance, so on the monotypes the relation collapses to identity. And with Q = ε the discharge premise is vacuous, so on plain schemes ≥ is the Hindley-Milner instance relation and every plain scheme has its own body as an instance. A qualified scheme need not have any instance — two stumps may pin the same δ to different verdicts — which is why T-let demands inhabitation explicitly.

#example(name: [An uninhabited scheme])[
  The scheme $∀(δ: "Type"). ⟨(l: 𝓫).l ↓ δ⟩, ⟨ε.l ↓ δ⟩ ⇒ δ$ is well-sorted but has no instance. The first stump hits, so D-hit demands $θ δ ≈ 𝓫$. The second misses, so D-⊥ demands $θ δ = ★$. Since ★ is equivalent only to itself, no θ satisfies both.
] <ex-uninhabited>


== Operational Semantics
#let semantics = figure(
  caption: "Operational semantics.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Values and evaluation contexts")[
      $
        #type_name("Value") v & ::= c | (x: e) | { ξ } \
        #type_name("Context") E & ::= [·] | E e | v E | E.l | E ‖ e | v ‖ E | #b[let] x = E #b[in] e \
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
    subbox(caption: "Lookup error")[
      #flexbox(
        derive("↯-sel", ($ξ(l) "undefined"$,), ${ξ}.l ↯$),
        derive("↯-ctx", ($e ↯$,), $E[e] ↯$),
      )
    ],
  )),
)
#semantics <semantics>

@semantics gives the small-step reduction → of the mechanization. Application and `let` are call-by-value, but a record literal is a value as it stands: its fields are evaluated only when they are selected, as in NixLang. $ξ(l)$ denotes the leftmost binding of l in ξ, so selection agrees with ↓, and E-conc places the right operand first, as T-conc does. The judgement $e ↯$ is a _lookup error_: a selection reaches a record literal that lacks the selected label. With the first-class labels of @sec-extensions, a dynamic selection whose key does not evaluate to a label is a lookup error as well.


= Metatheory <metatheory>

This section gives the metatheoretic results about the declarative system as a whole. The results of this thesis are mechanized in Lean 4; @sec-mechanization lists their Lean names and the points where the development encodes the presentation differently.


== Type Safety <type-safety>
We prove type safety up to lookup errors for the declarative system in the syntactic style of Wright and Felleisen @wright_felleisen, by progress and preservation over the reduction of @semantics.

#theorem(name: [Progress], lean: "qProgress")[
  If $∅ ⊢ e : τ$, then $e$ is a value, $e → e′$ for some $e′$, or $e ↯$.
] <thm-progress>

#theorem(name: [Preservation], lean: "qPreservation")[
  If $∅ ⊢ e : τ$ and $e → e′$, then $∅ ⊢ e′ : τ$.
] <thm-preservation>

Both theorems are stated for the qualified system. Progress holds only up to lookup errors: the ↯-disjunct is the price of typing an absent field at ★ by T-sel-⊥. Lookup errors arise only at such a selection and are otherwise only propagated. Preservation holds exactly: the type is unchanged, not merely refined. Its central lemma is substitution for `let`: a value that types at every instance of a scheme may replace a variable bound to that scheme, which is exactly what the instance-closure premise of T-let provides.


== Refinement and the Rigidity of ★ <refinement>

Solving a row-variable never loses a typing.

#lemma(name: [Type substitution], lean: "qtyped_applySubst")[
  Let θ be a substitution that has an image for each scheme in Γ. If $Γ ⊢ e : τ$, then $θ Γ ⊢ e : θ τ$.
] <lem-subst>

It may, however, admit a more precise one. By @lookup-metatheory, definite lookups survive θ, while a $?$ re-resolves against the substituted row and can become definite.

#definition(name: [Precision])[
  The _precision order_ ⊑ is the least preorder on types that has ★ as top element and is a congruence for → and for the field types of rows. It is covariant in both positions of →: it measures information content and is not a subtyping relation. We write $τ ≼ τ′$ when $τ ≈ τ″ ⊑ τ′$ for some τ″.
] <def-precision>

For `x: ({l = c} ‖ x).l` with $x : {β}$, the typing ${β} → ★$ is carried to ${ε} → ★$ under $[β ≔ ε]$ by @lem-subst, and ${ε} → 𝓫_c ⊑ {ε} → ★$ becomes derivable as well, since the lookup now hits. Conversely, ★ is never sharpened.

#lemma(name: [Rigidity of ★], lean: "finalized_no_blur")[
  Let $τ₀ ≠ ★$. There is no substitution θ with $θ({β} → ★) ⊑ {(l: τ₀)} → τ₀$.
] <lem-rigid>

@lem-rigid drives the principality result below and reappears as the rigidity of ★ in unification (@unification-ty).


== Principality Forces Qualified Schemes <principality>

#definition(name: [Principal scheme])[
  A scheme σ is _principal_ for e in Γ if
  $
    (∀τ. #h(0.3em) σ ≥ τ ⟹ Γ ⊢ e : τ) and (∃τ. #h(0.3em) σ ≥ τ) and (∀τ. #h(0.3em) Γ ⊢ e : τ ⟹ ∃τ′. #h(0.3em) σ ≥ τ′ and τ′ ≼ τ).
  $
] <def-principal>

The three conjuncts are instance-closure, inhabitation and coverage: every instance of σ is a typing of e, σ has an instance, and every typing of e is matched by an instance at least as precise. Coverage is stated up to ≼ rather than equality, since typings are closed under T-eq and T-★-intro while instance sets are not.

#theorem(name: [No plain scheme is principal], lean: "no_plain_principal_scheme")[
  There is no plain scheme σ whose instances are all typings of `x: x.l` and which has both ${(l: {ε})} → {ε}$ and ${ε} → ★$ as instances. In particular, no plain scheme is principal for `x: x.l`.
] <thm-no-plain>

#proof[
  Both instances are typings, so a scheme covering them must have a quantified variable as its result, since ${ε}$ and ★ are rigid. Sending that variable to ${ε}$ in the substitution of the second instance leaves the argument at ${ε}$ and yields the instance ${ε} → {ε}$, which is not a typing. Choosing ★ as result does not help, by @lem-rigid.
]

A qualified scheme is principal. Let

$ "selQ" quad = quad ∀(β: "Row", δ: "Type"). #h(0.3em) ⟨β.l ↓ δ⟩ ⇒ {β} → δ. $

#theorem(name: [Qualified principality], lean: "selQ_principal, selQ_greatest")[
  selQ is principal for `x: x.l` in the empty context. Moreover, selQ is greatest among the schemes whose instances are all typings of `x: x.l`: every instance τ of such a scheme is matched by an instance τ′ of selQ with $τ′ ≼ τ$.
] <thm-selq>

Discharge pins δ to the lookup's verdict, so ${ε} → {ε}$ is excluded. One binding then serves incompatible uses.

#corollary(name: [Qualified schemes type strictly more], lean: "l1_strictly_weaker")[
  Let L₁ be the system with plain schemes and L₂ the qualified one. There are e and τ with $∅ ⊢ e : τ$ in L₂ but not in L₁, for instance
  $
    ∅ ⊢ #h(0.3em) bold("let") f = (x: x.l) bold("in") { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}.
  $
] <cor-strict>

Qualified schemes are thus forced, not chosen.


= Unification <unification>

We present our unification algorithm, which is sound and terminating but incomplete. It is a _mutual_ pair of judgements: $τ₁ ≐ τ₂ ⇝ v$ solves an equation between types and $s₁ scripts(≐)_r s₂ ⇝ v$ one between rows. @unification-helpers [fixes this vocabulary](wording).

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

Rows are not unified as trees but as _spines_ — the lists of atoms $a ::= l: τ | α$ obtained by flattening the concatenation tree — because associativity and the units of @row-equivalence are then quotiented away by construction, and the only residue of ≈ is that distinct labels commute within a var-free segment while nothing crosses a row-variable. For example, $((l: 𝓫 | ε) | (α | m: 𝓫))$ and $(l: 𝓫 | (α | (m: 𝓫 | ε)))$ both have the spine $⟨l: 𝓫, α, m: 𝓫⟩$.

// meta: reword the fuel sentence. Do we even want to show it?
A verdict is either a solution θ or one of four refusals. #u_clash and #u_occurs are the familiar ones; #u_stuck carries the wand-ambiguity, reported when every move is dead but nothing is provably wrong; #u_fuel separates "the budget ran out" from "the problem is unsolvable", which is what makes every verdict the algorithm does reach independent of the budget it was given.

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

@unification-ty is standard first-order unification, read top-to-bottom, with one deviation: ★ is rigid. U-★ unifies it with itself and U-clash rejects it against everything else, so the unknown is never silently absorbed into another type — a ★ in a solution is always one that the lookup relation put there. The occurs check of U-occurs ensures that a binding _eliminates_ its variable, and the elimination of variables is what bounds the recursion.

U-bind and U-occurs are stated for a variable on the left and are tried on both sides. U-fn is the only place where the type pass sequences: the solution of the argument equation is applied to the results before they are unified, and the two solutions are composed. A refusal from either premise of U-fn, or from the row problem of U-rcd, is the verdict of the conclusion.

== Row Unification
// meta: the rules are not really tanglible for me
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

The two exhaustion rules are checked at every depth and before the budget: once a side is empty, its counterpart's variables are forced to ε and a surviving field has nowhere to come from. The cancellation rules exploit that spines cancel at both ends, which is exactly the trace-monoid property of @trace-monoid. U-var-solve fires only when one side is a lone variable, and its occurs check ensures that the binding eliminates that variable.

The three matching moves differ in how far they may look for a partner. A _window_ is a maximal var-free segment at one end of a spine: U-field-L and U-field-R may pair a field only inside it, because a row-variable in between could be instantiated to a colliding field and the pairing would be a guess about shadowing. In $⟨m: 𝓫, l: 𝓫, α, l: 𝓫′⟩$ the leading window is $⟨m: 𝓫, l: 𝓫⟩$ and the trailing window is $⟨l: 𝓫′⟩$. A leading $l: τ$ on the other side is paired with $l: 𝓫$. Against $⟨m: 𝓫, α, l: 𝓫′⟩$ it has no partner in the leading window, and pairing it with $l: 𝓫′$ would assume that α carries no l. U-ground lifts that restriction by counting: if one side has no variables at all and some label occurs equally often and positively on both sides, then the other side's variables cannot carry that label, the pairing is positional, and it is again forced.

// what does global do here?
// explain the F-★ step somewhere?
[No move places a field into a row-variable](this sentenc appears way too often). A field facing only variables on the other side is left where it is, even when a single variable is its only possible host: that placement would be forced, but it is not made, and the configuration is reported as stuck (@incompleteness-forced). The only place where a field is placed into a row-variable is the materialization of spent promises at finalization (@inference), after unification has run. Finally U-clash is a global projection check rather than a per-window one, and U-stuck reports a terminal configuration.

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

// meta: expand this a bit with interesting explanation of the extra rules
@unification-row states the moves as rules. They are to be read in the order of @unification-cascade, and U-var-solve, U-var-occurs, U-field-L, U-field-R and U-ground are additionally tried with the two sides exchanged; U-ε-var, U-ε-clash and U-clash are symmetric as stated. Note that no rule ever pushes a field demand into a row-variable: field lookups do not travel through $scripts(≐)_r$, they park as stumps, so row unification never guesses a field into a variable.

#example(name: [Matching, then solving])[
  In $⟨l: γ, α⟩ scripts(≐)_r ⟨l: 𝓫, m: 𝓫⟩$ no side is empty, no variable is shared at an end, and neither side is a lone variable. The leading $l: γ$ finds the partner $l: 𝓫$ in the leading window of the right side, so U-field-L emits $γ ≐ 𝓫 ⇝ [γ ≔ 𝓫]$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫⟩$. Now α is a lone variable, and U-var-solve returns $[α ≔ ⟨m: 𝓫⟩]$. The solution is $[γ ≔ 𝓫, α ≔ (m: 𝓫)]$.
] <ex-unif-match>

#example(name: [Pairing by counting])[
  In $⟨α, l: γ, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ both windows of the left side are empty, since the spine begins and ends with a variable, so neither U-field-L nor U-field-R applies. The right side, however, is var-free and $|s₁|_l = |s₂|_l = 1$. Whatever α and β become, they cannot carry an l without making the counts differ, so U-ground pairs the two l-fields and emits $γ ≐ 𝓫$. The residual $⟨α, β⟩ scripts(≐)_r ⟨⟩$ is solved by U-ε-var with $[α ≔ ε, β ≔ ε]$.
] <ex-unif-ground>

#example(name: [Cancelling a shared tail])[
  For $l₁ ≠ l₂$, the problem $(l₁: 𝓫 | α) scripts(≐)_r (l₂: 𝓫 | α)$ has spines that both end in α. U-var-refl-R cancels it and leaves $⟨l₁: 𝓫⟩ scripts(≐)_r ⟨l₂: 𝓫⟩$, where no field finds a partner and U-clash fires, since $|⟨l₁: 𝓫⟩|_(l₁) = 1 > 0$ and the right side is var-free. By @lem-cancel the cancellation loses no unifier, so the clash is sound. Paszke and Xie @extensible_tabular need a side condition on the shared tail to reach the same verdict.
] <ex-unif-cancel>

#example(name: [A stuck problem])[
  In $⟨α, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ no move fires. The left side has no field and no lone variable, the field $l: 𝓫$ has no partner in the empty windows of the left side, U-ground needs a positive count on both sides, and U-clash needs the side with fewer l-fields to be var-free. The verdict is #u_stuck. This is the wand-ambiguity, which has no most general unifier (@prop-irreducible).
] <ex-unif-stuck>

The two passes recurse into each other — U-rcd hands a row problem to $scripts(≐)_r$, and U-field-L, U-field-R, U-ground hand a type problem back to $≐$ — [and each cross-call consumes one unit of an explicit budget](no one cares). This makes the definition structurally recursive, [which is what lets the mechanization compute verdicts by `rfl` and check worked examples in the kernel](no one cares again); exhausting the budget is the separate verdict #u_fuel, so the four real verdicts are never an artefact of the bound. Crucially, the type equations a matching move emits are solved on the spot and their solution applied to the residual before the row pass continues. Deferring them instead would make #u_stuck meaningless: an equation must be discharged, or fatal, or itself stuck, never merely postponed.

== Unification Metatheory <unification-metatheory>

#theorem(name: [Termination], lean: "unifyRowM_terminates, unifyM_fuel_mono")[
  For all rows ρ₁ and ρ₂ there is a budget at which $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉$ returns a verdict other than #u_fuel, and every larger budget returns the same verdict.
] <thm-unif-term>

A substitution θ _unifies_ ρ₁ and ρ₂ if $θ ρ₁ ≈ θ ρ₂$, and it _satisfies_ a solution Θ if $θ α ≈ θ(Θ α)$ for every binding of Θ.

#theorem(name: [Success is most general], lean: "unifyRowM_success_mgu")[
  Let $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ Θ$ and let V be the variables of ρ₁ and ρ₂. Then Θ unifies ρ₁ and ρ₂, and for every unifier θ of ρ₁ and ρ₂ there is a θ′ that agrees with θ on V and satisfies Θ.
] <thm-unif-mgu>

The restriction to V is necessary: Θ may introduce fresh variables, on which an arbitrary unifier θ need not agree with any extension of Θ.

#theorem(name: [Failure is sound], lean: "unifyRowM_clash_no_unifier, unifyM_occurs_no_unifier")[
  If $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ #u_clash$ or $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ #u_occurs$, then no substitution unifies ρ₁ and ρ₂.
] <thm-unif-fail>

Stuck is conservative. Every move preserves the solution set, but not every solvable problem is solved. Where the algorithm stops, and why it must, is the subject of @incompleteness.


= Inference <inference>

The inference algorithm is a judgement $Γ; S ⊢ e ⇒ τ; S′$ [that threads a _solver state_ S through the term](threading is weird here). It differs from algorithm W in one respect: a selection whose lookup answers $?$ is not turned into a row constraint but parked as a stump, and the state carries these stumps until a later substitution lets them advance or the end of inference forces them to ★. Unification, as developed in @unification, is only ever called on equations between types; it never sees a field demand.

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
         #type_name("Solver state") S & ::= (θ, Δ, W, macron(κ)) \
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
        $#type_name("Sort of a name") S(α)$,
        $#type_name("Stumps of") S_i #h(0.3em) Δ_i$,
      )
    ],
    subbox(caption: "Judgements")[
      #flexbox(
        $#type_name("Inference") Γ; S ⊢ e ⇒ τ; S′$,
        $#type_name("Solve") S ⊢ τ ≐ τ′ ⇝ S′$,
        $#type_name("Solve, then saturate") S ⊢ τ ≐ τ′ #st_solve S′$,
        $#type_name("Wake one stump") S ⊢ q #st_wake S′$,
        $#type_name("Saturate") S ⊢ Δ #st_sat S′$,
        $#type_name("Finalize one stump") S ⊢ q #st_fin S′$,
      )
    ],
  )),
)
#solver_state <solver-state>

@solver-state [fixes the vocabulary](weird wording, again). The state is a quadruple of a sort-respecting substitution θ, a list Δ of parked stumps, a list W of warnings and a record $macron(κ)$ of the sort at which each name was drawn. ⟦S⟧τ applies S's substitution to τ, $S(α)$ reads the sort of α off $macron(κ)$, and $Δ_i$ denotes the parked stumps of $S_i$. A parked stump $⟨α ▷ ρ.l ↓ δ⟩$ is a lookup of l in ρ whose result has been promised to the fresh variable δ, annotated with the row-variable α that blocks it — the variable at which L-var stopped the search. The blocker is what lets the algorithm tell, after a solution has been written, which stumps might now advance. Stumps are ordered by the time they were parked, and warnings record every place a ★ was committed, so that the user learns where the analysis gave up.

//meta: an example for why quiescence is important might be nice
The state is kept under one invariant, _quiescence_: every stump in Δ is genuinely blocked on the variable it records, $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$. Every solution write can break it — it may solve the very blocker a stump is waiting on — so outside of saturation no equation is solved on its own. The judgement $S ⊢ τ ≐ τ′ ⇝ S′$ runs the unifier of @unification and writes its solution into the state, and $S ⊢ τ ≐ τ′ #st_solve S′$ does so and then _saturates_: it re-examines the stumps the solution made stale until the state is quiescent again.

== Inference Rules

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
  stack(
    spacing: 14pt,
    flexbox(
      derive(
        "A-let",
        stack(
          spacing: 8pt,
          $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) macron(α) = "gen"_(Γ, S)(S₁, τ₁)$,
          $Γ · (x: ∀(macron(α): S₁(macron(α))). ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ #st_drop Δ_q ⊢ e₂ ⇒ τ₂; S₂$,
        ),
        $Γ; S ⊢ #b[let] x = e₁ #b[in] e₂ ⇒ τ₂; S₂$,
      ),
    ),
    align(
      left,
      $
        Δ_q &= {p ∈ Δ₁ | "blocker"(p) ∈ macron(α)} #h(3em) Δ_Γ = Δ₁ #st_drop Δ_q \
        "gen"_(Γ, S)(S₁, τ₁) &= "the greatest" macron(α) ⊆ "ftv"(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁) "such that" \
        & #h(1em) macron(α) ∩ "ftv"(⟦S₁⟧(Γ, Δ_Γ)) = ∅ \
        & #h(1em) Δ_q ∩ Δ = ∅ \
        & #h(1em) "ftv"(⟦S₁⟧"results"(Δ_q)) ⊆ macron(α) \
        & #h(1em) Δ_q "fillable at" S₁
      $,
    ),
  ),
)
#generalization <generalization>

@inference-rules gives the syntax-directed counterpart of the declarative rules of @declarative-rules. T-eq and T-★-intro have no algorithmic counterpart: the former is built into row unification, and the latter is never needed to produce a typing, only to blur one. A-cons, A-λ and the record-literal rules are the familiar ones. A-app and A-conc introduce fresh variables for the shapes their premises demand and hand the resulting equations to $#st_solve$; A-conc glues the two row-variables together exactly as T-conc does, so concatenation itself never forces a decision.

Selection is where the algorithm departs from W. The subject is first unified with a record ${ρ}$ of a fresh row, and then the lookup relation of @row-lookup is asked about the row as solved so far. Its three answers select the three rules. A definite type τ′ is returned by A-sel; a definite absence ⊥ yields ★ and a warning by A-sel-⊥, mirroring T-sel-⊥. Only when the lookup reaches a row-variable α does A-sel-\? fire: it returns a fresh δ in place of the answer it does not have and _parks_ the stump $⟨α ▷ ρ.l ↓ δ⟩$ — the promise that δ will be pinned to whatever the lookup eventually answers. No equation mentioning l is ever emitted, and this is the entire reason row unification never has to guess a field into a variable.

A-var instantiates a qualified scheme with fresh variables of the binder's sorts. The scheme's stumps are instantiated alongside its body and immediately replayed by $scripts(↝)^(*!)$: those that the instantiation already decides are answered on the spot, and the others are parked on the variable that actually blocks them. This is the algorithmic reading of the discharge premise of I-inst (@instantiation) — each use of a `let`-bound variable discharges its own copy of the stumps.

A-let, given separately in @generalization, is the only rule with real content. As in algorithm W, the choice of what to generalize is delegated to a function, here $"gen"_(Γ, S)$, and the rule itself only infers $e₁$, binds the scheme and continues with $e₂$. A choice $macron(α)$ determines the split of the stumps of $e₁$: $Δ_q$, the stumps blocked on a variable of $macron(α)$, travel into the scheme as its qualifier Q, while $Δ_Γ$ stays parked in the state and outlives the binding. Four conditions make a choice admissible. The generalized variables must be fresh for the environment, which here consists of Γ together with the stumps that stay parked; this is the side condition of HM(X), and it ensures that a parked stump reads the same at every instance. $Δ_q$ may contain only stumps parked while inferring $e₁$, since a stump of an outer binding that is captured by the scheme would never be finalized. This condition does not follow from the others: in `{a = x: x.l; b = let y = c in c}` the stump of field a is blocked on a variable that neither Γ nor any remaining stump mentions, so the remaining three conditions admit its generalization into the scheme of y. Every variable in a generalized stump's result must itself be generalized, since otherwise each instance would pin a variable of the environment. Finally, the scheme must be inhabited, as the declarative T-let requires through $∃ τ₁. σ ≥ τ₁$. A stump whose result is still a variable is harmless, since its result can be sent to ★ while its lookup stays blocked. A stump whose result has been spent on a non-variable type must instead hit at some instance. The algorithm does not decide this property but checks a sufficient syntactic condition, written $Δ_q$ fillable at $S₁$: every spent stump has a literal label, all stumps on its blocker have literal labels, a stump that shares its label is spent as well and its result agrees with the first once every type variable is replaced by ★, and no spent blocker occurs in a spent result. Under this condition the instance that sends the generalized type variables to ★ and extends each blocker by the fields its spent stumps demand discharges every stump of $Δ_q$. The admissible choices are closed under union, so a greatest one exists and is computed by deleting variables until nothing more is deleted. A failing condition therefore does not make the binding monomorphic; it removes the offending variables and whatever they drag along.

As an example, consider `let f = x: x.l in f {l = c}`. For the bound term, A-λ gives x a fresh α, and A-sel-\? unifies α with ${ρ}$ for a fresh ρ, finds that $ρ.l$ answers $?$ on ρ, and parks $⟨ρ ▷ ρ.l ↓ δ⟩$ with a fresh δ. The result is $⟦S₁⟧(α → δ) = {ρ} → δ$. Since α is solved, $macron(α) = ρ, δ$; the single stump has its result in $macron(α)$ and moves into the scheme, and nothing stays parked. f is bound to $∀(ρ: "Row", δ: "Type"). ⟨ρ.l ↓ δ⟩ ⇒ {ρ} → δ$, which is selQ of @principality. At the use, A-var instantiates the scheme with fresh ρ′ and δ′ and replays the stump, which is still blocked and is parked on ρ′. A-app then solves $ρ′ ≔ (l: 𝓫_c)$ and identifies δ′ with the result variable. Saturation finds the stump stale, K-hit re-asks the lookup, which now hits, and the result is unified with $𝓫_c$.

== Wake-Up and Finalization

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

// meta: explain the phases and why they need to be done this way
$scripts(↝)^*$ replays a whole list, as A-var needs for the stumps of an instantiated scheme. K-park parks a stump that is still blocked, and it reads the blocker off its own premise: the annotation is determined by the lookup rather than supplied, so an instantiated stump cannot be filed under a variable that does not actually block it. Saturation $#st_sat$ steps only on _stale_ stumps — those whose recorded blocker no longer blocks — which are exactly the ones a solution write creates, and stops when the state is quiescent. $#st_solve$ is unification followed by saturation, and $scripts(↝)^(*!)$ is $scripts(↝)^*$ followed by saturation.

Finalization, in @finalization, happens once, at the top. Entry runs inference from the empty state and then forces every stump still parked to ★ with F-★, recording a warning for each. A stump whose promised result has already been used at a non-variable type — a _spent promise_ — cannot be forced to ★. It is _materialized_ first: its blocking row-variable is extended with the field, so that the lookup hits and the stump is discharged by saturation. Materialization is the one step of the algorithm that places a field into a row-variable. It is not a forced move: the field could as well be supplied further along the row, and the typing it produces is one of several. It is sound, since the result is a declarative typing, and it happens after inference proper, where no further equation depends on the choice. Stumps that were generalized by A-let are not affected — they live in their scheme and are discharged per use — and, conversely, nothing is finalized at a `let` boundary: a stump blocked on a variable of Γ may still be answered by an application further out. The remaining $?$ of @sec-motivation become ★ here and nowhere else.

#example(name: [Waking, re-parking and finalizing])[
  In `r: (x: x.l) (r ‖ {m = c})`, A-sel-\? parks the stump $⟨ρ ▷ ρ.l ↓ δ⟩$, where ρ is the fresh row of x. The argument has type ${m: 𝓫_c | ρ_r}$ by A-conc, where $ρ_r$ is the row of r, and A-app solves $ρ ≔ (m: 𝓫_c | ρ_r)$. This write makes the stump stale, since its recorded blocker ρ is solved. Saturation re-asks the lookup, which skips m by L-miss and stops at $ρ_r$, and K-repark files the stump under $ρ_r$. Nothing solves $ρ_r$ afterwards, so finalization pins δ to ★ by F-★ and records a warning. The program has type ${ρ_r} → ★$.
] <ex-repark>

#example(name: [A spent promise])[
  In `x: (x.l) c`, A-sel-\? parks $⟨ρ ▷ ρ.l ↓ δ⟩$, and A-app unifies δ with $𝓫_c → γ$ for a fresh γ. The promise is spent: F-★ would have to unify $𝓫_c → γ$ with ★, which clashes because ★ is rigid. Materialization instead solves $ρ ≔ (l: 𝓫_c → γ | ρ′)$ for a fresh ρ′. Saturation wakes the stump, the lookup now hits, and K-hit unifies the found type with the promised one. The program has type ${l: 𝓫_c → γ | ρ′} → γ$, without a warning.
] <ex-spent>

== Inference Metatheory <inference-metatheory>

#theorem(name: [Termination of inference], lean: "runF_terminates, satStep_wf")[
  Saturation admits no infinite run, and for every term e the entry point $⊢ e ⇒ τ; S′$ returns a verdict.
] <thm-infer-term>

#theorem(name: [Soundness of inference], lean: "runSound, run_typed")[
  If $⊢ e ⇒ τ; S′$, then $∅ ⊢ e : ⟦S′⟧τ$.
] <thm-infer-sound>

Inference is not complete; @incompleteness lists where it gives up.


= Incompleteness <incompleteness>

The algorithm is sound but not complete. This section collects the places where it gives up, in decreasing order of inevitability. The first family is a limit of the row theory, the second a price paid for forced moves, the third a limit of the inference algorithm on top.

== Irreducible Problems <incompleteness-irreducible>

Some row equations have no most general unifier, and no algorithm that returns a single unifier can solve them. Most general is meant relative to the variables of the problem, as in @thm-unif-mgu.

#proposition(name: [Irreducible problems], lean: "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set")[
  + (_Wand-ambiguity._) For pairwise distinct $α₁, …, α_n$ with $n ≥ 2$, $(α₁ | … | α_n) scripts(≐)_r (l: 𝓫)$ has no most general unifier.
  + (_Two-sided._) $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has no most general unifier.
  + (_Shift._) $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers.
] <prop-irreducible>

In the wand-ambiguity, every variable may carry the field, and no choice subsumes the others. The two-sided problem is the same deadlock at both ends of the spine. The shift has a unifier for every number of l-fields in α, so returning several answers does not help either.

#example(name: [Unifiers of the irreducible problems])[
  - The wand-ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the unifiers $[α ≔ (l: 𝓫), β ≔ ε]$ and $[α ≔ ε, β ≔ (l: 𝓫)]$. Both are ground and they differ, so neither is an instance of the other, and a most general unifier would have to be more general than both while placing the single field in one of the two variables.
  - The two-sided problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has the unifiers $[α ≔ ε, β ≔ ε]$ and $[α ≔ (l: 𝓫), β ≔ (l: 𝓫)]$, which disagree on the number of l-fields in α.
  - The shift $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ is unified by $[α ≔ (l: 𝓫)^n]$ for every $n ≥ 0$, where $(l: 𝓫)^n$ is the row of n copies of the field. Each unifier in a finite set fixes the number of l-fields in α, so no finite set covers all of them.
] <ex-irreducible>

// meta: re-check
All three are stuck in the algorithm because there is nothing else it could report. The wand-ambiguity and the two-sided problem become solvable with negative information, that is, lacks-predicates on row-variables (@sec-extensions). The shift survives it; only row equations as scheme qualifiers would cover it.

What is shown is a limit of principal solutions over scoped rows, not of typing: a program whose inference is stuck may still be typed by placing ★ where the problem sits.

== The Price of Forced Moves <incompleteness-forced>

Two stuck problems do have a most general unifier.

- The _crossfield_ problem $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | β)$ is solved by extending each variable with the other side's field, but that is an expansion of a variable and not a forced pairing. The algorithm has no expansion move, since expansion is not forced, and crossfield is therefore stuck. It could be solved by expanding a variable only when it is the unique possible host of the field.
- A stuck equation between the types of two paired fields is reported before the remaining row equation solves the variable it depends on, which masks an existing solution (@prop-masks). Deferring the stuck equation and letting the residual run first would solve this instance, but it abandons the discipline of @unification that every emitted equation is discharged, fatal or stuck on the spot. Whether the resulting algorithm is confluent, that is, whether its verdict is independent of the order in which deferred equations are retried, is open.

#proposition(name: [Stuck masks an mgu], lean: "stuck_masks_mgu")[
  The problem $(k: {β | α} | β) scripts(≐)_r (k: {l: 𝓫} | l: 𝓫)$ is stuck, but $[β ≔ (l: 𝓫), α ≔ ε]$ is a most general unifier.
] <prop-masks>

Stuck is therefore conservative: it does not imply that no most general unifier exists, not even in a configuration where no move applies.

== Limits of Inference <incompleteness-inference>

// meta: this is just a huge block
- Partially generalized lets. A-let generalizes the greatest admissible $macron(α)$, and a variable that violates a condition stays monomorphic together with whatever it drags along; a second use at another record may then clash. Freshness for Γ and for the stumps that stay parked is the side condition of Hindley-Milner and HM(X), and a stump of an outer binding may not be captured, since it would never be finalized. These losses are inherent to let-polymorphism. The fillability test for spent results is not: it is a syntactic approximation of inhabitation and rejects a spent stump blocked on its key, as in `let f = r: a: r.(a) c`, together with spent results that differ in more than their type variables. Nested selection such as `let g = x: (x.l).m`, record literals in a spent result and the same field spent twice on one row are generalized.
- Spent promises. With first-class labels (@sec-fc-labels), a lookup whose answer was already consumed can be blocked on its key rather than on a row, so there is no row to extend and nothing to wake. Such a stump is never answered unless the key is supplied. _The declarative system types the program, so this is incompleteness and not rejection_. Two remedies are possible: guessing the key, which answers non-principally with an invented label, or reporting a qualified type at the top level, which is principal but changes what a run returns.
- Unrestricted T-★-intro. The declarative system may replace any type by ★, so it types programs the algorithm clashes on: `g: { a = g {l = c}; b = g {m = c} }` has type $(★ → 𝓫) → {a: 𝓫 | b: 𝓫}$, while inference unifies the two argument records and clashes. Every stuck witness above becomes typable in the same way. A clash therefore certifies the absence of a unifier, but not the absence of a typing. T-★-intro acts as subsumption into a top type, and ★ is the join of any two distinct types, whereas unification only computes common instances. Closing the gap therefore requires either inference that joins incompatible lower bounds at ★ or a completeness statement restricted to typings without T-★-intro.


= Mechanization <sec-mechanization>

The metatheory of this thesis is mechanized in Lean 4. [The development comprises 38 files and about 31,000 lines](who cares?), and it contains no `sorry`: a dedicated file pins the axiom dependencies of every headline theorem with `#guard_msgs`, so that the build fails if a proof comes to depend on `sorryAx` or on any axiom beyond `propext`, `Quot.sound` and `Classical.choice`. @lean-map relates the results of this thesis to their Lean names.

#let lean_row(result, name, sec) = (result, raw(name), sec)

#[
  #set par(justify: false)
  #figure(
    caption: [Results of this thesis and their Lean counterparts.],
    table(
      columns: (2fr, 3fr, auto),
      align: (left, left, left),
      inset: 6pt,
      stroke: (x, y) => (top: if y > 0 { 0.4pt + luma(85%) }),
      table.header([*Result*], [*Lean*], [*Section*]),
      ..lean_row([Characterization of ≈], "rowEquiv_iff_char", [@trace-monoid]),
      ..lean_row(
        [Lookup is total and deterministic],
        "lookup_total, LookupQ.det",
        [@lookup-metatheory],
      ),
      ..lean_row(
        [Lookup is stable under substitution],
        "lookup_applySubst",
        [@lookup-metatheory],
      ),
      ..lean_row([Progress up to ↯], "qProgress", [@type-safety]),
      ..lean_row([Preservation], "qPreservation", [@type-safety]),
      ..lean_row([Type substitution], "qtyped_applySubst", [@refinement]),
      ..lean_row(
        [selQ is principal for `x: x.l`],
        "selQ_principal, selQ_greatest",
        [@principality],
      ),
      ..lean_row(
        [L₂ has typings L₁ lacks],
        "l1_strictly_weaker",
        [@principality],
      ),
      ..lean_row(
        [Unification terminates],
        "unifyRowM_terminates",
        [@unification-metatheory],
      ),
      ..lean_row(
        [Budget monotonicity],
        "unifyM_fuel_mono",
        [@unification-metatheory],
      ),
      ..lean_row(
        [Success is most general],
        "unifyRowM_success_mgu",
        [@unification-metatheory],
      ),
      ..lean_row(
        [Clash is sound],
        "unifyRowM_clash_no_unifier",
        [@unification-metatheory],
      ),
      ..lean_row(
        [Occurs is sound],
        "unifyM_occurs_no_unifier",
        [@unification-metatheory],
      ),
      ..lean_row(
        [Irreducible problems have no mgu],
        "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set",
        [@incompleteness-irreducible],
      ),
      ..lean_row(
        [Stuck masks an existing mgu],
        "stuck_masks_mgu",
        [@incompleteness-forced],
      ),
      ..lean_row(
        [Inference is sound],
        "runSound, run_typed",
        [@inference-metatheory],
      ),
      ..lean_row(
        [Inference and saturation terminate],
        "runF_terminates, satStep_wf",
        [@inference-metatheory],
      ),
      ..lean_row(
        [A-let's choice of ᾱ is greatest],
        "greatestAlpha_spec",
        [@generalization],
      ),
    ),
  )<lean-map>
]

The development covers MiniNix extended with the first-class labels of @sec-fc-labels, and the theorems of @lean-map are stated for that extension; MiniNix is its label-free fragment. Two parts of the presentation differ from the development. First, sorts are tracked by the inference algorithm, whose state records the sort at which each name was drawn, but not by the declarative system: its schemes quantify over untagged names, and a substitution is a triple of maps, for types, rows and keys, over a single namespace. A name therefore has a reading at each sort, and the occurs check of unification tests only the sort at which a variable is bound. The sorting of @declarative and its single sort-respecting substitution are the intended reading of this encoding, not a mechanized result. Second, the Lean inference algorithm materializes spent promises before finalization, a step that @inference describes in prose but does not give as a rule.

= Extensions Towards NixLang <sec-extensions>

== First-Class Labels <sec-fc-labels>
//meta: note that ★ nicely carries over to this development


NixLang computes labels at run time. A field is selected by an arbitrary string expression, `x.${name}`, and a record literal may bind a computed label, `{ ${name} = v; }`. The introductory example of @sec-motivation is of the first kind, and the second is how the builtins and the module system construct records from lists of names. This section extends MiniNix by both forms. type safety, the soundness of unification and inference and the termination of both carry over.

#let fc_syntax = figure(
  caption: [Syntax of first-class labels, as an extension of @syntax.],
  box(width: 100%, stack(
    spacing: 20pt,
    subbox(caption: "Terms")[
      $
        #type_name("Term") e & ::= … | \"l\" | e₁.\${e₂} | { \${e₁} = e₂ } \
      $
    ],
    subbox(caption: "Types")[
      $
                #type_name("Key") k & ::= l | α \
               #type_name("Type") τ & ::= … | ⌊k⌋ \
                #type_name("Row") ρ & ::= … | \${k}: τ \
               #type_name("Sort") κ & ::= "Type" | "Row" | "Label" \
        #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.k ↓ δ⟩ \
      $
    ],
  )),
)
#fc_syntax <fc-syntax>

@fc-syntax gives the syntax extension. A label becomes a value $\"l\"$, written as a string literal as in NixLang, and it is typed by the singleton type $⌊l⌋$. Dynamic selection $e₁.\${e₂}$ selects the field named by the value of e₂, and the dynamic record ${ \${e₁} = e₂ }$ binds the field named by the value of e₁. At the type level, a _key_ k is either a label or a variable [of the new sort `Label`](wording). Keys occur in two places: in the singleton type $⌊k⌋$ and in the _keyed field_ $\${k}: τ$ of a row, whose label is not yet known. Stumps are keyed as well, $⟨ρ.k ↓ δ⟩$, so that a scheme can defer a lookup under a key it abstracts over.


// meta: revert the order
_Keys are a sort of their own._ A substitution acquires a third component, and it sends a label variable to a key again. A key is therefore never ★, a base type or any other type, and no operation of MiniNix, whether unification, instantiation or finalization, can make it one. The alternative, in which a key is an arbitrary type and a non-label key yields ★, does not preserve types: ${ \${e₁} = e₂ }$ with $e₁ : ★$ steps to a literal record ${ l = e₂ }$ whose type is not equivalent to a field keyed by ★, and treating ★ as equal to itself makes lookup unsound at run time. The price of the separate sort is that a non-label key is a type error. It is the one place where the extension rejects rather than records uncertainty (@sec-goals).


=== Keyed Lookup

#let fc_lookup = figure(
  caption: [Key comparison and keyed lookup. The rules L-ε, L-var and the three concatenation rules of @row-lookup carry over with k in place of l.],
  stack(
    spacing: 15pt,
    align(
      center,
      $#type_name("Key comparison") l ⋈ l = (=) quad l ⋈ l′ = (≠) "if" l ≠ l′ quad α ⋈ α = (=) quad k ⋈ k′ = (?) "otherwise"$,
    ),
    flexbox(
      derive("L-hit", ($k ⋈ l = (=)$,), $(l: τ).k ↓ τ$),
      derive("L-miss", ($k ⋈ l = (≠)$,), $(l: τ).k ↓ ⊥$),
      derive("L-key-?", ($k ⋈ l = (?)$,), $(l: τ).k ↓ #h(0.2em) ?$),
      derive("L-dhit", ($k ⋈ k′ = (=)$,), $(\${k′}: τ).k ↓ τ$),
      derive("L-dmiss", ($k ⋈ k′ = (≠)$,), $(\${k′}: τ).k ↓ ⊥$),
      derive("L-d?", ($k ⋈ k′ = (?)$,), $(\${k′}: τ).k ↓ #h(0.2em) ?$),
    ),
  ),
)
#fc_lookup <fc-lookup>

@fc-lookup generalizes the lookup relation from labels to keys. Two keys compare equal when they are the same label or the same label variable, distinct when they are distinct labels, and undecided otherwise. Both definite answers are stable under substitution, since a substitution maps a label to itself and a variable to one key. The keyed lookup is thus again deterministic, total and stable under substitution (@lookup-metatheory), and at a label key it coincides with the lookup of @row-lookup. A new source of $?$ appears: a field whose key cannot yet be compared. The lookup of l in $(\${γ}: τ)$ may hit or miss depending on the label γ becomes, and a lookup under a variable key α may hit any literal field. The variable that blocks a $?$ is accordingly no longer always a row-variable. It is the row-variable at which the search stopped, the variable key of the lookup, or the variable key of a field, and inference must wake a stump when any of these is solved.

=== Row Equivalence and Spines

#let fc_equiv = figure(
  caption: [Row equivalence for keyed fields, extending @row-equivalence.],
  flexbox(
    derive("≈-dfield", ($τ₁ ≈ τ₂$,), $(\${k}: τ₁) ≈ (\${k}: τ₂)$),
    derive("≈-dlab", (), $(\${l}: τ) ≈ (l: τ)$),
  ),
)
#fc_equiv <fc-equiv>

A keyed field is congruent only under the same key, and a keyed field with a literal key is the literal field (@fc-equiv). ≈-comm is not extended. A keyed field $\${α}: τ$ may become a field of any label, and a literal field next to it may carry the same label and be shadowed by it, so the two must keep their order. In the trace monoid of @trace-monoid a keyed field is therefore a _barrier_, like a row-variable: it commutes with nothing. The characterization of ≈ becomes three-way. Two rows are equivalent exactly when they have the same sequence of barriers, that is, row-variables and keys in order, when segment by segment they agree on the fields of every label, and when the types of their keyed fields agree pointwise. Spines acquire a third atom, $a ::= l: τ | α | \${α}: τ$, where a literal key is normalized to a field, and the monoid remains cancellative on both sides.

=== Typing and Evaluation

#let fc_typing = figure(
  caption: "Typing rules and reduction for first-class labels.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Typing")[
      #flexbox(
        derive("T-lab", (), $Γ ⊢ \"l\": ⌊l⌋$),
        derive(
          "T-sel-dyn",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ τ$),
          $Γ ⊢ e₁.\${e₂}: τ$,
        ),
        derive(
          "T-sel-dyn-★",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ #h(0.2em) ?$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-sel-dyn-⊥",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ ⊥$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-rcd-dyn",
          ($Γ ⊢ e₁: ⌊k⌋$, $Γ ⊢ e₂: τ$),
          $Γ ⊢ { \${e₁} = e₂ }: { \${k}: τ }$,
        ),
      )
    ],
    subbox(caption: "Reduction")[
      $
          #type_name("Value") v & ::= … | \"l\" \
        #type_name("Context") E & ::= … | E.\${e} | v.\${E} | { \${E} = e } \
      $
      #flexbox(
        derive("E-sel-dyn", ($ξ(l) = e$,), ${ξ}.\${\"l\"} → e$),
        derive("E-rcd-dyn", (), ${ \${\"l\"} = e } → { l = e }$),
        derive("↯-sel-dyn", ($ξ(l) "undefined"$,), ${ξ}.\${\"l\"} ↯$),
        derive(
          "↯-key",
          ($v ≠ \"l\" "for all" l$,),
          ${ξ}.\${v} ↯ quad { \${v} = e } ↯$,
        ),
      )
    ],
  )),
)
#fc_typing <fc-typing>

@fc-typing gives the typing rules and the reduction. The three rules for dynamic selection are those of static selection with the label replaced by the key of e₂'s type, and T-rcd-dyn records the key of e₁ in a keyed field. Discharge (@instantiation) replays a keyed stump with the keyed lookup: θ is applied to both the row and the key, $(θ ρ).(θ k) ↓ r$, [and δ is pinned as in D-hit, D-⊥ and D-\?. At run time, dynamic selection evaluates the record, then the key, and then selects as E-sel does](does the reader know all this?). A dynamic record evaluates its key but leaves the field unevaluated, as a record literal does. A missing label and a key that is not a label are both lookup errors. Progress up to ↯ and preservation hold for MiniNix with first-class labels. [The only new case of preservation is E-rcd-dyn, where ${ l = e }$ has type ${l: τ}$ and ≈-dlab relates it to the keyed field of T-rcd-dyn](wut?).

Qualified schemes abstract over keys as they do over rows. The dynamic selector and the dynamic record constructor have the schemes

$
  #raw("a: x: x.${a}") & quad : quad ∀(α: "Label", β: "Row", δ: "Type"). #h(0.3em) ⟨β.α ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ \
  #raw("a: v: { ${a} = v; }") & quad : quad ∀(α: "Label", δ: "Type"). #h(0.3em) ⌊α⌋ → δ → { \${α}: δ }
$

and both are instance-closed (`selDynQ_instance_closed`, `rcdDynQ_instance_closed`). The first scheme defers the lookup until both the key and the row are known, so that one binding answers a definite type at a use whose record has the field and ★ at a use whose record lacks it.

=== Unification

#let fc_unif = figure(
  caption: [Unification of keys and of keyed fields, extending @unification-ty.],
  flexbox(
    derive("U-key-refl", (), $⌊α⌋ ≐ ⌊α⌋ ⇝ ∅$),
    derive("U-lab", ($l = l′$,), $⌊l⌋ ≐ ⌊l′⌋ ⇝ ∅$),
    derive("U-lab-clash", ($l ≠ l′$,), $⌊l⌋ ≐ ⌊l′⌋ ⇝ #u_clash$),
    derive("U-key-bind", ($k ≠ α$,), $⌊α⌋ ≐ ⌊k⌋ ⇝ [α ≔ k]$),
    derive(
      "U-key-L",
      ($τ ≐ τ′ ⇝ θ$, $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$),
      $(\${α}: τ) · t₁ scripts(≐)_r (\${α}: τ′) · t₂ ⇝ θ′ ∘ θ$,
    ),
    derive(
      "U-key-R",
      ($τ ≐ τ′ ⇝ θ$, $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$),
      $t₁ · (\${α}: τ) scripts(≐)_r t₂ · (\${α}: τ′) ⇝ θ′ ∘ θ$,
    ),
  ),
)
#fc_unif <fc-unification>

A singleton type is rigid and nullary, like a base type, and the key pass of @fc-unification is flat: two labels unify when they are equal and clash otherwise, and a label variable is bound to the other key. A key is atomic, so U-key-bind needs no occurs check. $⌊k⌋$ against any other head clashes by U-clash.

On rows, the moves of @unification-cascade treat a keyed field as a barrier. U-ε-clash fires on a keyed field as on any field, since the empty spine has no field of any key. U-clash and U-ground count only literal fields and require the other side to be barrier-free, so that $(l: τ) scripts(≐)_r (\${α}: τ′)$ is stuck rather than a clash: $[α ≔ l]$ solves it. An occurrence of a variable next to a keyed field is reported as stuck rather than as an occurs failure, since the [counting argument](not shown, no?) behind U-var-occurs does not account for keys. The only new move pairs keyed fields. U-key-L and U-key-R cancel two keyed fields under the same variable key at the front or the back of both spines and unify their types. They are restricted to the ends of the spines, since a literal field in front of $\${α}: τ$ may carry the label that α becomes. Both moves are forced: by cancellativity, the two sides share the barrier α and their keyed projections begin with τ and τ′ respectively. Keyed fields under different unknown keys stay stuck. $(\${α}: τ) scripts(≐)_r (\${β}: τ′)$ is solved by $[α ≔ β]$ and by $[α ≔ l, β ≔ l]$ for every label l, and the unifiers that identify the keys have no common generalization with those that choose labels. The metatheory of @unification-metatheory holds as stated: success is most general, clash and occurs are sound, and the algorithm terminates.

=== Inference

#let fc_infer = figure(
  caption: [Algorithmic rules for first-class labels, extending @inference-rules. A-sel-dyn-⊥ and A-sel-dyn-\? share the premises of A-sel-dyn.],
  flexbox(
    derive("A-lab", (), $Γ; S ⊢ \"l\" ⇒ ⌊l⌋; S$),
    derive(
      "A-sel-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) "fresh" ρ: "Row" #h(2em) S₁ ⊢ τ₁ ≐ {ρ} #st_solve S₂ #h(2em) Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃$,
        $"fresh" κ: "Label" #h(2em) S₃ ⊢ τ₂ ≐ ⌊κ⌋ #st_solve S₄ #h(2em) (⟦S₄⟧ρ).(⟦S₄⟧κ) ↓ τ′$,
      ),
      $Γ; S ⊢ e₁.\${e₂} ⇒ τ′; S₄$,
    ),
    derive(
      "A-sel-dyn-⊥",
      ($…$, $(⟦S₄⟧ρ).(⟦S₄⟧κ) ↓ ⊥$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ ★; S₄ "+W"$,
    ),
    derive(
      "A-sel-dyn-?",
      ($…$, $(⟦S₄⟧ρ).(⟦S₄⟧κ) ↓ #h(0.2em) ? "on" α$, $"fresh" δ: "Type"$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ δ; S₄ #st_park ⟨α ▷ ρ.κ ↓ δ⟩$,
    ),
    derive(
      "A-rcd-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$,
        $"fresh" κ: "Label" #h(2em) S₂ ⊢ τ₁ ≐ ⌊κ⌋ #st_solve S₃$,
      ),
      $Γ; S ⊢ { \${e₁} = e₂ } ⇒ { \${κ}: τ₂ }; S₃$,
    ),
  ),
)
#fc_infer <fc-inference>

@fc-inference gives the algorithmic rules. Dynamic selection follows A-sel, A-sel-⊥ and A-sel-\? of @inference-rules. The key is unified with the singleton type of a fresh label variable [κ](do I want this meta-var?), and the keyed lookup is asked about the row and the key as solved so far. When it answers $?$, the stump is parked on its blocker, which may now be a label variable: in `a: {l = c; m = {}}.${a}` the stump waits on the key and not on a row. A-rcd-dyn performs no lookup and parks nothing. It is shaped like A-app: the key is unified with $⌊κ⌋$ for a fresh κ, and the keyed field records κ. Wake-up and saturation are unchanged over keyed lookups, so solving a key wakes the stumps blocked on it: applying the selector above to $\"l\"$ solves κ, and the stump hits.

Finalization needs no new premise. F-★ solves the promised δ with ★ and never binds a label variable, so the blockers of other stumps survive it untouched and no key ever becomes ★. A-let generalizes keyed stumps under the side conditions of @generalization, read over rows, keys and results. A spent promise is materialized only when its key is a literal label and it is blocked on its row. A spent promise blocked on its key has no row to extend, and it remains unanswered (@incompleteness-inference).

Concatenation and computed labels interact as NixLang programmers expect. In `((n: r: r // { ${n} = c; }) "x" { x = {}; }).x`, the keyed field is placed in front of the row of r by T-conc, the application solves the key to x, and the selection yields $𝓫_c$: the computed field shadows the older one.

=== Costs

The extension has three costs. A non-label key is a type error: ${l = c}.\${c}$ and ${ \${c} = c }$ are rejected by U-clash, where MiniNix records every uncertainty as ★. Keyed fields are barriers, so a keyed field against a literal field and two keyed fields under different unknown keys are stuck. On randomly generated row problems with keys, about two thirds are stuck. And a spent promise blocked on its key is not materialized, which leaves inference incomplete on programs the declarative system types. The first cost follows from giving keys their own sort, the second from the absence of negative information about keys, and the third from the absence of top-level qualified types.

== Lacks-Predicates
todo

== Patterns, `with` and `inherit`
todo

== Occurrence Typing
todo


= Discussion <sec-discussion>

== The Idioms of the Introduction <sec-idioms>

We return to the three idioms of @nix-idioms. MiniNix has no lists, booleans or library, so each idiom is encoded with the missing parts abstracted as parameters: `false` becomes a constant c, `mkDerivation` and `++ [extra]` become parameters mk and app, and `lib.optionalAttrs stdenv.isDarwin` becomes an application of a parameter opt to a parameter cond. The types below are the results of the inference algorithm of @inference, computed by the Lean implementation, with variables renamed.

#example(name: [Overlay])[
  $
    #raw("self: super: { hello = super.hello // { meta = { broken = c; }; }; }") \
    quad : quad α → {"hello": {ρ₁} | ρ₂} → {"hello": {"meta": {"broken": 𝓫} | ρ₁}}
  $
  The selection `super.hello` parks a stump on the row of super, and the concatenation spends its promise on a record ${ρ₁}$. Finalization materializes the field $"hello": {ρ₁}$ in the row of super. The result is faithful to the semantics of `//`: the new meta shadows any meta of `super.hello`, and every other field of `super.hello` survives in ρ₁.
] <ex-idiom-overlay>

#example(name: [Override])[
  $
    #raw("mk: app: args: mk (args // { buildInputs = app args.buildInputs; })") \
    quad : quad ({"buildInputs": β | ρ} → γ) → (★ → β) → {ρ} → γ
  $
  The argument handed to mk carries the new buildInputs in front of the row of args, so it shadows the old one. The selection `args.buildInputs` cannot be answered, since the row of args is open, and it is unspent, since app accepts any argument. Finalization therefore pins it to ★ with a warning, and the domain of app becomes ★. Under a `let` the stump would instead travel into the scheme and be answered at each use, as in @principality.
] <ex-idiom-override>

#example(name: [Conditional extension])[
  $
    #raw("opt: cond: { pname = c; } // opt cond { NIX_LDFLAGS = c; }") \
    quad : quad (α → {"NIX_LDFLAGS": 𝓫} → {ρ}) → α → {ρ | "pname": 𝓫}
  $
  The result keeps pname behind the row ρ returned by opt, since that row may carry a pname of its own. A later lookup of pname answers $?$ until opt is known, which is exactly the wand-ambiguity of @sec-motivation.
] <ex-idiom-cond>

Applied to concrete package sets, the overlay shows both sides of R3. Applied to `{}` and `{ hello = { pname = c; }; }`, it types at ${"hello": {"meta": {"broken": 𝓫} | "pname": 𝓫}}$. Applied to `{}` and `{}`, inference reports a clash: the lookup of hello misses, so the promised result is pinned to ★, but the concatenation has already used it as a record. The declarative system rejects the program as well, since ★ cannot be concatenated (@sec-goals). The program nevertheless evaluates without error, because the field hello is never selected.

== Design Goals Revisited <sec-goals>

[@sec-motivation stated four goals for a type system for NixLang. MiniNix meets one of them by construction, and the other three in part. This section records the distance that remains.](wording)

_R1 (No source changes)._ Inference requires no annotations. Every construct of MiniNix is typed without input from the programmer, and uncertainty is recorded as ★ rather than demanded as a lacks-predicate or a disjointness witness. The goal is thus met for MiniNix, but not yet for NixLang: patterns, `with`, `inherit`, recursive records and the builtins lie outside MiniNix (@sec-extensions), [and today's nixpkgs cannot be analysed verbatim until they are covered](wording).

_R2 (No semantic change)._ The analysis is purely static. It inserts no casts and does not instrument the program, so evaluation is unchanged, and the goal is met by construction. The price is carried by the metatheory: progress holds only up to lookup errors ↯ (@type-safety), which the analysis reports as warnings but cannot prevent. [](note our further goals?)

_R3 (Totality)._ This goal is met in part. Uncertainty that originates in a lookup is absorbed: a selection that cannot be answered is typed at ★, and inference parks it rather than failing. Two sources of rejection remain. First, ★ is rigid and has no elimination form, so a ★ that is applied, selected from or concatenated leaves a program untypable even in the declarative system. The motivating example, extended by one selection,

```nix
{ "1759190400" = { m = 1; }; }.${toString builtins.currentTime}.m
```

receives no type, since the dynamic selection yields ★ and ★ cannot be selected from. Second, unification reports a clash on programs that the declarative system types, such as the example of @incompleteness-inference: a clash certifies the absence of a unifier, not the absence of a typing. Stuck verdicts, in contrast, are reported as the goal permits.
// meta: This is just a very weak thing lule

_R4 (Effective inference)._ The algorithm is designed for this goal but not yet measured against it. Inference is syntactic first-order unification: there is no constraint solver, no unification modulo associativity and commutativity, and no backtracking over alternative typings. Every move of the row pass is forced, and a parked stump is re-examined only when its blocker is solved. These are structural reasons to expect the analysis to scale. [The thesis, however, proves termination rather than a complexity bound, and the analysis has not been run on nixpkgs](wording).

== Outlook: Towards Gradual Typing <sec-gradual>

The remaining gap in R3 has a common cause. ★ is introduced by the lookup relation and by T-★-intro, but it is never consumed, so every use of an unknown value demands a type that ★ cannot supply. Gradual typing @gradual_siek resolves the same situation by replacing type equality with _consistency_, under which the dynamic type is compatible with every type. Reading ★ as the dynamic type would give it elimination forms — a ★ could be applied, selected from and concatenated — and would allow inference to join incompatible uses at ★ instead of reporting a clash, which is the repair @incompleteness-inference names for unrestricted T-★-intro. Sekiyama and Igarashi @gradual_extensible_rows show that consistency extends to extensible rows.

Two obstacles distinguish this setting from the usual one. First, gradual type systems insert casts at the boundary between static and dynamic code, and they rely on these casts for their safety theorem and for the gradual guarantee @gradual_criteria. R2 forbids instrumentation, so a gradual extension of MiniNix would be gradual in its static discipline only. Its safety theorem would acquire a further disjunct for failures at the eliminations of ★, in the same way that ↯ records lookup errors today. Second, the rigidity of ★ is what the principality result of @principality rests on. A ★ that is consistent with every type can be sharpened, and whether qualified schemes remain principal under consistency is open.

= Related Work <related-work>
_Record concatenation in classic record calculi._ Typing record concatenation is an old and notoriously hard problem. Wand @concat4multiinher first studied type inference for concatenation in the context of multiple inheritance, where the set-or-replace semantics of asymmetric concatenation already surfaces: his system needs to case-split over which side a field comes from, and typings are unions of alternatives rather than principal types. Harper and Pierce @symm_concat sidestep shadowing by restricting to _symmetric_ concatenation, which is only defined on records with disjoint fields, tracked by compatibility constraints; they also observe that concatenation and width-subtyping do not mix: subtyping can silently forget a field that concatenation later resurrects, breaking soundness — the same observation that steers MiniNix away from subsumption and towards row equivalence. Rémy @concat4free shows that concatenation can be simulated "for free" in a language with polymorphic record extension by abstracting over the extension point, at the price of encoding-style types. Ohori @ohori1995polymorphic obtains efficient compilation for a polymorphic record calculus, but restricts records to selection and functional update — [concatenation is exactly the operation his index-passing compilation scheme cannot support](check!). In the disjoint-polymorphism line @xie2020row the merge operator subsumes symmetric concatenation, with disjointness playing the role of the lacks-constraints. All of these systems either forbid the colliding case which is forced by NixLang, or pay for it with non-principal or encoded types; [none types the motivating example `a: b: (a ‖ b).l` as-is.](check)

_Scoped rows and first-class labels._ Our row theory descends from Leijen's extensible records with scoped labels @extensible_recs, where duplicate labels are kept in the row and lookup resolves them with left-precedence — precisely the "bag" semantics that makes asymmetric concatenation a total operation instead of a partially defined one. Leijen later added first-class labels @fc_labels, which NixLang needs for its dynamic field selection `e.${e'}`. Paszke and Xie @extensible_tabular combine both into infix-extensible rows with a unification-based inference algorithm over row- and label-variables; their system is the direct basis of ours. It cannot, however, model set-or-replace: extension always happens on a known side of the row, and their conditional tail-check rejects programs whose shadowing behaviour is unresolved — our lookup relation instead accepts them at ★ and refines later.

_Expressive row theories._ The line of work started by Morris and McKinna @rose abstracts rows behind an algebra of containment and combination constraints strong enough to type asymmetric concatenation faithfully, and has been extended to generic programming @generic_with_extensible, extensible recursive functions @extensible_rec_funcs and ad-hoc polymorphism @extensible_data_adhoc. These systems track strictly more information than ours, and Rose is in fact a rank-1 Hindley-Milner language that does establish principal types — but the principality is that of qualified types @qualified_types: inference produces a principal _constrained_ scheme and defers the row predicates to an entailment relation that the framework leaves as a parameter, required only to be invariant under row equivalence, monotone and transitive, and nowhere shown to be decidable. The instantiated entailment rules decide only ground predicates and discharge everything else by assumption lookup, so no solving procedure — and hence no complexity bound — is offered for the predicates with row-variables that inference generates. Since combination is an equation in a partial monoid, deciding conjunctions of such predicates is unification modulo associativity and commutativity rather than syntactic unification, and is NP-complete @ac_unification. The successor systems move to System Fω and are explicitly typed, with type reconstruction left as future work @generic_with_extensible; unrestricted second-order reconstruction is in any case undecidable @undecidable. Sulzmann @designing_record_systems designs record systems in the HM(X) framework, where concatenation becomes a constraint; HM(X) however only stipulates that a constraint solver exists without providing one. Our position is dual: we keep plain unification-based inference and instead weaken the types themselves with ★ where the row theory would need a disjunction.

_Subtyping-based systems._ Algebraic subtyping @algebraic_subtyping @mlsub and its simplifications @simplesub give principal inference for structural subtyping, and MLstruct @mlstruct extends this to a Boolean algebra of types with unions, intersections and negations — negation being one way to express the absence information that shadowing destroys. However, these systems support record extension and field update rather than general concatenation of unknown records, and by the width-subtyping argument above @symm_concat, adding `‖` to a subtyping-based system is problematic at the core: a record can always forget the very fields that decide precedence. We deliberately keep our system subtyping-free; the only ordering is the precision gained by instantiation. [](check! newest version (boolean-algerra) has extension. Also, we might want to note that their records _must be tagged_)

_Set-theoretic types and dynamic languages._ Castagna's programme of semantic subtyping @frisch_semantic @castagna2023programming types dynamic languages with unions, intersections and negations, including detailed accounts of records, maps and structs @typing_records_etc and polymorphic records for Elixir @poly_records, together with occurrence typing to refine types along control flow @revisiting_occurrence @on_occurrence and a gradual guard-based system deployed for Elixir @gradual_elixir. This is the most expressive treatment of records for a dynamic language to date, and occurrence typing is a natural future extension of MiniNix (@sec-extensions). The cost is the full set-theoretic machinery: [inference is local rather than let-polymorphic](check: what does that mean?), and the subtyping problems with concatenation resurface. Our ★ plays a role similar to their `dynamic()` @elixir_design_principles, but is introduced by the _lookup relation_ itself rather than by explicit annotation. [](we might want to note that open record extension is still open for his work)

_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all, and has been instantiated for extensible rows by Sekiyama and Igarashi @gradual_extensible_rows, the system closest in spirit to our ★-typed rows. We differ in a fundamental way: [NixLang programs cannot be instrumented, so there are no casts, no blame, and no runtime monitoring](check!). Our system is instead a _soft_ typing system in the tradition of Cartwright and Fagan @soft_typing @practical_soft_typing and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory states the residual risk as the ↯-disjunct of progress (@type-safety). The term is used in a narrower sense than theirs. Cartwright and Fagan accept every program and insert run-time checks where the analysis fails, whereas our system inserts no checks and can reject programs, at a clash or at an elimination of ★ (@sec-goals). What the two share is that the analysis never changes the program and reports what it cannot decide instead of forbidding it. Industrial gradual systems such as TypeScript @typescript and Flow @flow make the same pragmatic choice of an unsound `any`, but without a formal account of when `any` arises; in our system ★ is introduced only by the lookup relation and T-★-intro, and its origin is therefore always explainable.

_Typing NixLang._ Work on NixLang itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for NixLang, but do not attempt a type system. An earlier system by the author @simplenix applies off-the-shelf HM inference to a NixLang subset and fails exactly on the record operations this thesis addresses. The long-standing community issue @nix-ts-issue documents both the demand for and the difficulty of typing NixLang; Nickel @nickel, a configuration language inspired by NixLang, opts for gradual typing with row polymorphism but [forbids the colliding concatenations we target](todo!).


= Conclusion
This thesis [set out to](wording) type asymmetric record concatenation, the operation at the centre of NixLang, without annotations and without changing the meaning of a single program. Its canonical obstacle, `a: b: (a ‖ b).l`, admits no principal typing in a row calculus that must decide where the field comes from. We have taken the decision out of the calculus. Scoped rows make concatenation a total juxtaposition of rows, a context-free lookup relation answers a field demand with a type, with definite absence or with _don't know_, a pending answer is kept as a qualifier that is decided by evaluating the lookup at each use, and an answer that never arrives is recorded as the unknown type ★.

The resulting calculus, MiniNix, is type-safe up to lookup errors, and progress and preservation are mechanized in Lean. Principality forces its schemes to carry their pending lookups: no plain scheme is principal for `x: x.l`, while a qualified one is, and one binding then serves uses that no plain scheme can serve together. Row unification operates on spines, makes only forced moves and returns most general unifiers; the inference algorithm built on it parks unanswered lookups as stumps, terminates and produces declarative typings. Where the algorithm gives up, the reason is delimited: [some](wording) row equations have no most general unifier at all, some are the price of forced moves, and some are limits of inference itself.

Measured against the design goals of @sec-motivation, MiniNix requires no source changes and leaves evaluation untouched, but it reaches totality only in part and has not yet been measured on nixpkgs (@sec-goals). The path towards NixLang is correspondingly twofold. The remaining constructs of the language — patterns, `with`, `inherit` and the builtins — must be covered, with lacks-predicates as the most promising source of the negative information that scoped rows lack (@sec-extensions). And ★ must become usable rather than merely recorded, for which a gradual reading of the unknown type, without casts, is the natural next step.



// -------------- Bibliography ----------------
#pagebreak()
#bib

// -------------- Appendix -------------------

#set figure(placement: none)
#outline(target: heading.where(supplement: [Appendix]), title: [Appendix])
#show: appendix
#show figure: set block(breakable: true)

= Sorting <app-sorting>
#sorting <sorting>
