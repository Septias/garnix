#import "functions.typ": *
#set heading(numbering: "1.")
#set page(margin: 4em)

#text("Improving Nothing", size: 17pt)
#linebreak()
The nix programming language is a pure and lazy programming languge with real-world usage in the nix package manager and NixOs operating system, and even though it has existed for over 15 years and is used close to exclusively in the nix-ecosystem with over 100.000 files written in it, it has not received a proper type system yet. The reason for that is not clear to the authors, but we suspect it roots from the unique features of the language, its unintuitive shadowing behaviour and laziness, that complicate principled type inference in a general sense.
Only the recent works of Lionell Parreux and Stephen Dolan surrounding _algebraic subtyping_ have opened a new perspective to type inference for such an expressive languagage and motiviated this paper where we try to lay out the current state of type inference in nix, define a comprehensive operational semantic and ultimately a type system for a reduced part fo the language. We also provide an implementation of a language server written in rust.

= Origin of the Nix Language
During his phd. Eelco Dolstra developed the package manager `nix` @dolstra_phd that has two unique properties: _reproducibly_ and _purity_. Together, these properties allow for predictable builds on different machines, fearless package upgrades, overcoming the DDL-hell @nixos_long and easy rollbacks in case something goes wrong. Since its initial release in 2002, the ecosystem has seen continuous growth, surging especially in the last two years. The reason for this great success are the underlying properties which make dependency sharing and shipping fearless and reliable. The greates disjunction and integral origin of these properties is the underling nix language that was created in conjunction with the package manager and deeply integrates with the ecosystem, boasting features that are a perfect fit for the usecase.

The conceptual idea behind the nix the development is quite easy: In a pure language, the output of a function is soley defined by its inputs. Since no interior mutability can change the functions output, it can be stored and _reused_ if the function is called with the same arguments again. Every output is also a strict result of its inputs making it a lot easier to understand the computation. The meat of Eelco Dostras phd. was then to elevate this simple concept to package managing and creating a system, that redefines dependency management.

To lift functional pureness to pure package management, it has to be possible to create packages in the first place. We continue distributable software packages even though Eelco Dostra calls them components since we believe package is the more intuitive wording in this context. In the nix language, a package can be created using the `derivation` function. This function is built into the language and is exectued by the _evaluator_ which creates an isolated environment and runs the build script defined in it. This build script can refer to (a) other packages by dynamically adding them in the build srcript or (b) system paths which form a base type in the nix language. The nix evaluator tracks these references and upon instantiation, builds them before the current package. The build script can then write the build-artifacts to `$out` which is similar to function output. (a) and (b) give motivation for _dyanmic accsess_ and `path` as a pritimive in the final language.

Also the very integral part of the language, _laziness_, is founded by a need. The need to not build packages if they are not really needed because building or downloading a package can take multiple hours and such must not happen if one only wants to acces the package meta-data. Laziness is thus integrated into the language to distinguish between what has to be evaluated and what not. A package can then be referenced in a file but will never be built, if the outpot does not depend on it.

It is also a product of writing configurations, that the language revolves largely around records (attr-sets in nix) which turn out to be essetial to define key-value maps of usual configurations. Function patterns only revoled from the easier use of pattern destructuring at the beginnign of a function and the inherit-statement is only syntactic sugar to create records more easy. The problematic with-statement is used to open a record for easier access in following expressions and as such, also a usability feature.


== Things done in this paper
Since nix was built as a domain specfic language with usability as its greatest design goal, the system boasts a lot of features that make type inference hard or even impossible. In language theory, the approach is mostly the opposit where one starts from a simple calculus like ML, SystemF, λ and carefully extends it with features to form a wieldy and interesting semantics. When trying to retrofit a type-system onto a language like \@typescript \@flow \@castagna_elixier one has to decide which features one can and wants to support.

In this paper we restrict ourselves to:
1. Basetypes (Record, Array)
2. Datatype Operators (Record-extension, Array-concatenation)
3. Special language constructs (with-statements, inherit-statements)

We defer these features to later efforts of research:
1. Dynamic accesses
2. Assert statements?
3. Deprecated let-attrset
4. Deprecated uris

In essence our contributions are:

1. A comprehensive operational semantic for the nix language
2. A Typesystem based on Mlstruct

= Syntax <syn>
$oi(E)$ denotes $0 … n$ repititions of a syntax construct and the index $i$ is omitted if obvious.

#let basetypes = subbox(caption: "Literals")[
  $
                                  c & ::= "[^\"$\\] | $(?!{) | \\."  \
                            "inter" & ::= "${"\^} *"}"               \
             #type_name("String") s & ::= "\"(c"*" inter)"*" c"*"\"" \
       #type_name("Ident String") s & ::= "''todo''"                 \
            #type_name("Boolean") b & ::= "true" | "false"           \
    #type_name("File-Path") rho.alt & ::= "(./|~/|/)([a-zA-Z.]+/?)+" \
             #type_name("Number") n & ::= "([0-9]*\.)?[0-9]+"        \
              #type_name("Label") l & ::= "[A-Za-z_][A-Za-z0-9_'-]*" \
        #type_name("Search Path") l & ::= "<[A-Za-z_]*> TODO"        \
    // #type_name("Variable") v & ::= "[A-Za-z_][A-Za-z0-9_'-]*" \
  $
]

// TODO: convert to code
// TODO: Note that uri is deprecated

#let general = subbox(caption: "Terms")[
  $
    t, t_1, t_2 ::= &| b | s | rho.alt | n | l | v | "null" \
    #type_name("Record") &| {overline(a\;)} | #b[rec] {overline(a\;)} \
    #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
    #type_name("Has-Attribute") &| t #b[ ? ] l \
    #type_name("Has-Attribute-Or") &| t.l #b[or] t \
    #type_name("Record-Concat") &| t "//" t \
    #type_name("Array-Concat") &| t "⧺" t \
    #type_name("Lookup") &| t "." l \
    (#type_name("Dynamic-Lookup") &| t "." t )\
    #type_name("Function") &| overline(p) "@ "h : t \
    #type_name("Let-statements") &| #b[let] overline(a\;) #b[in] t \
    #type_name("Conditionals") &| #b[if] t #b[then] t #b[else] t \
    #type_name("With-Statement") &| #b[with] t; t \
    #type_name("Assert-Statement") &| #b[assert] t; t \
    #type_name("Operator") &| t • t \
  $
]

#let inherit = subbox(caption: "Assignment")[
  $
    #type_name("Inherit") ι & ::= #b[inherit] overline(l\;) | #b[inherit] (ρ) space overline(l\;) \
    #type_name("Path") ρ & ::= l | ρ.l \
    #type_name("Assignment") a & ::= l = t; " | " ι \
  $
]

#let patterns = box([
  #text(weight: "bold", smallcaps("Patterns"))
  $
    d, h & ::= t | ε                                       \
       e & ::= l | l space ? space d                       \
       p & ::= { overline(e\,) } | { overline(e\,) … } | l \
  $])

#figure(
  rect(width: 100%, grid(
    columns: 2,
    align: left,
    inset: 8pt,
    grid.cell(rowspan: 2, general),
    basetypes,
    inherit,
    subbox(caption: "Operators")[
      $• ::= #b[or] | "//" | ⧺ | " ? "$

    ],
    patterns,
    subbox(caption: "Shorthands")[
      #set math.equation(numbering: "(1)")
      $ p : t space @ space ε = p : t $
      $ l space ? space ε : l $
    ]
  )),
  caption: "Supported Syntax of Nix",
)

Since nix is a real-world language it supports a big range of _literals_ in comparison to purely technical languages that get along with only one literal to form the simplest kind of syntax. The syntax is given following the official regex formulas to follow the specification \@typedef. _Records_ follow a standart notation where multiple fields can be defined using `key = value;` assignments to define multiple fields. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. _Arrays_ are introduced in a similar fashion where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as seperator. Both datatypes are generally _immutable_, but there are concat operations (Record-Concat and Array-Concat) that can be used to create new, bigger datatypes. Other than that, records come equipped with the usual lookup syntax and two specialities. The first being a dynamic label check that returns a boolean as a result and secondly a way to specify a default value in case the previous check turned out to be negative.

Functions take one argument, a _pattern_. This pattern can be a single label or adher to a _record-like_ structure, allowing multiple fields to be present, possibly with _default arguments_. This way a function taking multiple arguments can be created without resorting to currying. These functions can then be called with a record from which the "single arguments" are taken. This forms a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.
Patterns can alse be marked _open_ with the ellipsis (…), otherwise their are regarded as _closed_. Thye can also be given default arguments with the `?` syntax. An example would be `{a, b ? "pratt", …}` which is an _open_ pattern with a default value of "pratt" for the label $b$.

Let-expressions can have multiple bindings $a_1 = t_1; … ; a_n = t_n$ before the `in` keyword appears, possibly referencing each other in a _recursive way_. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. This feature is only syntactic sugar to build records and let-expressions easier and does not complicate the typesystem.
Let statements can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing existing variables. This is further discussed in @with.

== Reduction Rules
#figure(
  caption: "Function reduction",
  rect(width: 100%, inset: 20pt)[
    #align(
      left,
      stack(
        spacing: 20pt,
        $
          #rule_name("R-Fun")&& (l: t_2)t_1 & arrow.long t_2[l := t_1] \
          #rule_name("R-Fun-Pat")&& ({oi(l_i)}: t){oi(l_i \= t_i)} & arrow.long
          t [oi(l_i := t_i)] \
          #rule_name("R-Fun-Pat-Open")&& ({oi(l_i)\, ...}: t) {oj(l_i = t_i)} & arrow.long
          t [oi(l_i := t_i)] #h(0.5cm) &&&∀i. ∃ j. i eq j \
          #rule_name("R-Fun-Pat-Default")&&({oi(e_i)}): t{oj(l_j = t_j)} & arrow.long
          t [oj(l_j = t_j)][oi(l_i := d_i)] \
          #rule_name("R-Fun-Pat-Default-Open")&&({oi(e_i), …}): t{oj(l_j = t_j), …} & arrow.long
          t [oj(l_j = t_j)][oi(l_i := d_i)] &&&∀i. ∃ j. i eq j\
        $,
      ),
    )
  ],
)

Since nix supports patterns with default values and the _open_ modifiers, the function reduction rules become quite verbose. The simplest case is R-Fun which takes an argument t₁ and replaces the occurences of $l$ with said argument in the function body t₂. The next function rules R-Fun-Pat-∗ reduces functions taking patterns, the R-Fun-Pat being the simplest of such. We draw i,j from the index Set ℐ and range them over labels such that if i = j then l_i = l_j.
Since the same index $i$ is used for both the argument and pattern in R-Fun-Pat, they must agree on the same labels which resembles closed-pattern function calls. In the contrary case where the pattern is open, the argument-record can range over arbitray labels (possibly more than in the pattern). In this case, the side-condition enforces that at least the pattern fields are present (R-Fun-Pat-Open).

The R-fun-Pat-Default-∗ rules range over pattern elements $e$ which can be either single labels $l$ or labels with a default values like $l : d$. The former case can be converted to the latter with ε-extension transforming $l$ to $l ? ε$ which is equivalent to $l$ due to the shorthands (TODO: can you do this?). The variables of the body are then substituted twice. First with the argument values and then with the default values to "fill the gaps". The open case needs a side-condition analogous to the former open case.

Since ${oi(e_i)}$ strictly subsumes ${oi(l_i)}$ due to its inner structure, rule 2 and 3 are only stated as a mental stepping stone for the reader but not mentioned further.

#figure(
  caption: "Reduction rules of nix",
  rect(width: 100%, inset: 20pt)[
    #align(
      left,
      stack(
        spacing: 20pt,
        $
          #rule_name("R-Lookup")&& {oi(l_i = t_i\;)}.l & arrow.long t_i #h(0.5cm) &&&"if" ∃i. l_i = l \
          #rule_name("R-Lookup-Null")&& {oi(l_i = t_i\;)}.l & arrow.long "null" &&&"if" ∄i. l_i = l \
          #rule_name("R-Lookup-Default-Pos")&& {oi(l_i = t_i\;)}.l" or "t & arrow.long
          t_i &&&"if" ∃i. l_i = l \
          #rule_name("R-Lookup-Default-Neg")&& {oi(l_i = t_i\;)}.l" or "t & arrow.long
          t &&&"if" ∄i. l_i = l \
          #rule_name("R-Has-Pos")&& {oi(l_i = t_i\;)}.l" ? "t & arrow.long "true" &&&"if" ∃i. l_i = l \
          #rule_name("R-Has-Neg")&& {oi(l_i = t_i\;)}.l" ? "t & arrow.long "false" &&&"if" ∄i. l_i = l \
          #rule_name("R-Let")&& #b[let] oi(l_i \= t_i\;) "in" t_2 & arrow.long t_2 [oi(l_i = t_i)] \
          #rule_name("R-With")&& #b[with] {oi(l_i \= t_i\;)}; t_2 & arrow.long
          t_2[oi(l_i = t_i) ] &&& i ∈ {i : i in.not Γ} \
          #rule_name("R-Cond-True")&& #b[if ] "true" #b[ then ] t_1 #b[ else ]t_2 & arrow.long t_1 \
          #rule_name("R-Cond-False")&& #b[if] "false" #b[then ] t_1 #b[ else ]t_2 & arrow.long t_2 \
          #rule_name("R-Array-Concat")&& [ oi(t_(1i))] ⧺ [oj(t_(2j))] & arrow.long
          [ oi(t_(1i)) oj(t_(2j)) ] \
          #rule_name("R-Record-Concat")&& {oi(l_i = t_i\;)} "//" {oj(l_j \= t_j\;)} & arrow.long
          {oi(l_i = t_i\;) space overline(l_b = t_b\;)^b} &&& b ∈ { j: exists.not i. l_i = l_j } \
          && t arrow.long t' &==> E[t] → E[t']
        $,
        subbox(caption: "Values")[$
            v ::= p: t | todo(l) | {overline(a\;)} | #b[rec] {overline(a\;)}
          $],
        subbox(
          caption: "Evaluation Context",
          $
            E[□] & := □ | □ space t | (□).l | (v).□      \
                 & | #b[if ] □ #b[ then ] t #b[ else ] t \
                 & | #b[with ] □; t | #b[with ] v; □     \
                 & | #b[inherit ] (ρ) space □;           \
                 & | □ • t | v • t                       \
          $,
        ),
        linebreak(),
      ),
    )
  ],
) <reduction>

= Type System
What follows are the typing and subtyping rules as well as an overview over the constraint subroutine.


#figure(
  caption: "Types of nix.",
  rect(
    grid(
      columns: 1,
      align: left,
      inset: 8pt,
      grid.cell(rowspan: 2, subbox(
        caption: "Types",
        $
          #type_name("Type") tau ::= & "bool" | "string" | "path" | "num" \
          & | τ -> τ | {l: τ} | [τ] | [overline(τ)] | alpha \
          & | ⊥^diamond.small | τ ∨^diamond.small τ | ⦃ oi(p) ⦄^b \
          #type_name("Pattern Element") p & := τ | τ^? \
          #type_name("Polymorphic type") σ & := ∀Xi. τ \
          #type_name("Mode") diamond.small & := · | ↻\
        $,
      )),
      subbox(
        caption: "Contexts",
        $
          #type_name("Typing Context") Γ & ::= ε | Γ · (l : τ) | Γ · (l : σ) \
          #type_name("Subtyping Context") Σ & ::= Xi | Σ · (τ ≤ τ) | Σ · ⊳(τ ≤ τ) \
          #type_name("Constraint Context") Xi & ::= ε | Xi · (τ ≤ τ) | Xi · (τ ≤ α) | Xi · #text(weight: "bold", "err") \
        $,
      ),
    ),
  ),
)

Garnix models all literal syntax categories with the respective atom types bool, string, path and num. Notice, that we do not distinguish between float and int as they are coerced during interpretation and thus used interspersely in practice. We also add the usual types for fuctions, records and arrays and note that records types only define a _single_ label to type mapping instead of multiple. This is due to the use of subtyping constraints and their accumuation on type variables during type inferene. This mechanism is further discussed in \@section_todo. Also, we introduce two types for arrays, one for homogenous arrays of the same type and one accumulative for the case that an array has many distinct elements.
To form a boolean algebra of types we add the expected type connectives $union.sq, inter.sq, ~$ as well as a top and bottom type which represent the least type which is subsumed by every other type and the greatest type which subsumes every other type respectively.
Lastely, we add a single type for patterns. Even thought a pattern is similar in strucuter to a record, the pattern type is an accumulated type with multiple fields. This distinction is made due to the syntactical difference of the two. Patterns are introduced and eliminated atomically unlike a record where every fild acces $"record.field"$ results in new independent constraints. The superscript b can be true or false, ascribing whether the pattern is _open_ or _closed_.


#figure(
  caption: "Typing rules",
  rect(
    inset: 20pt,
    stack(
      spacing: 3em,
      sub_typing_rules(
        caption: "Standartrules",
        derive("T-Var1", ($Γ(x) = τ$,), $Ξ, Γ tack x: τ$),
        derive(
          "T-Var2",
          ($Γ(x) = σ$, $Ξ tack σ ≤^∀ ∀ε.τ$),
          $Ξ, Γ tack x: τ[arrow(α) \\ arrow(τ)]$,
        ),
        derive(
          "T-Abs",
          ($Ξ, Γ · (x: τ_1) tack t: τ_2$,),
          $Ξ, Γ tack (x: t): τ_1 → τ_2$,
        ),
        derive(
          "T-App",
          ($Ξ, Γ tack t_1: τ_1 → τ_2$, $Ξ, Γ tack t_2: τ_1$),
          $Ξ,Γ tack t_1 t_2: τ_2$,
        ),
        derive(
          "T-Sub",
          ($Ξ, Γ tack t: τ_1$, $Ξ, Γ tack τ_1 <= τ_2$),
          $Ξ, Γ tack t: τ_2$,
        ),
        derive("T-Asc", ($Ξ,Γ ⊢ t : τ$,), $Ξ,Γ ⊢ (t: τ) : τ$),
      ),
      line(length: 100%),
      sub_typing_rules(
        caption: "Functions",
        derive(
          "T-Abs-Pat",
          ($Ξ, Γ, oi(x\: τ) tack t: τ_2$,),
          $Ξ, Γ tack ({oi(x)}: t): oi(τ) → τ_2$,
        ),
        derive("T-Abs-Pat-Opt", ($"TODO"$,), $"TODO"$),
      ),
      line(length: 100%),
      sub_typing_rules(
        caption: "Records",
        derive(
          "T-Rcd",
          ($Ξ, Γ tack t_0: τ_0$, "...", $Ξ, Γ tack t_n: τ_n$),
          $Ξ, Γ tack {arrow(l): arrow(t)}: {arrow(l): arrow(τ)}$,
        ),
        derive("T-Proj", ($ Ξ, Γ tack t: {l: τ} $,), $Ξ, Γ tack t.l: τ$),
        derive(
          "T-Rec-Concat",
          ($Ξ, Γ tack a: { oi(l\: τ) }$, $Ξ, Γ tack b: { l_j: τ_j }$),
          todo[$Ξ, Γ tack a "//" b: {..b, ..a}$],
        ),
      ),
      line(length: 100%),
      sub_typing_rules(
        caption: "Lists",

        derive(
          "T-Lst-Hom",
          ($Ξ, Γ tack t_0: τ$, "...", $Ξ, Γ tack t_n: τ$),
          $Ξ, Γ tack [ " " t_0 " " t_1 " " ... " " t_n " "]: [τ]$,
        ),
        derive(
          "T-Lst-Agg",
          (
            $Ξ, Γ tack t_0: τ_0$,
            "...",
            $Ξ, Γ tack t_n: τ_n$,
            $∃ i, j. τ_i != τ_j$,
          ),
          $Ξ, Γ tack [space t_0 space t_1 space ... " " t_n] : [ τ_0 space τ_1 space ... space τ_n]$,
        ),
        derive(
          "T-List-Concat-Hom",
          ($Ξ, Γ tack a: "[τ]"$, $Ξ, Γ tack b: "[τ]"$),
          $Ξ, Γ tack a "⧺" b: "[τ]"$,
        ),
        derive(
          "T-List-Concat-Multi",
          ($Ξ, Γ tack a: [arrow(τ_1)]$, $Ξ, Γ tack b: [arrow(τ_2)]$),
          $Ξ, Γ tack a "⧺" b: [arrow(τ_1)arrow(τ_2)]$,
        ),
      ),
    ),
  ),
)
#figure(caption: "Mlstruct things", rect(inset: 20pt, stack(
  spacing: 3em,
  sub_typing_rules(),
)))

#figure(caption: "Typing rules (continued)", rect(inset: 20pt, stack(
  spacing: 3em,
  sub_typing_rules(
    caption: "Operators",
    derive(
      "T-Or-Neg",
      ($Xi, Γ tack t_1: {l: τ_1}$, $Xi, Γ tack t_2: τ_2$),
      $Xi, Γ tack (t_1).l "or" t_2: τ_1$,
    ),
    derive(
      "T-Or-Pos",
      ($Xi, Γ tack t_1: τ_1$, $l ∉ τ_1$, $Xi, Γ tack t_2: τ_2$),
      $Xi, Γ tack (t_1).l "or" t_2: τ_2$,
    ),
    derive("T-Negate", ($Xi, Γ tack e: "bool"$,), $Xi, Γ tack !e: "bool"$),
    derive("T-Check", ($Xi, Γ tack e: {..}$,), $Xi, Γ tack e ? l: "bool"$),
  ),
  line(length: 100%),
  sub_typing_rules(
    caption: "Language Constructs",
    derive(
      "T-Multi-Let",
      (
        $Γ overline([x_i: τ_i tack t_i : τ_i]^i)$,
        $Γ overline([x_i:∀ arrow(α). τ_i]^i) tack t: τ$,
      ),
      $Γ tack "let" x_0 = t_0; ... ; x_n = t_n "in" t: τ$,
    ),
    derive(
      "T-If",
      ($Γ tack t_1: "bool"$, $Γ tack t_2: τ$, $Γ tack t_3: τ$),
      $ "if" t_1 "then" t_2 "else" t_3: τ $,
    ),
    derive(
      "T-With",
      (
        $Γ tack t_1 : {arrow(l): arrow(τ)}$,
        $Γ, l_0 : τ_0, ..., l_n: τ_n tack t_2: τ$,
        $l_i in.not Γ$,
      ),
      $Γ tack "with" t_1; t_2 : τ$,
    ),
    derive(
      "T-Assert-Pos",
      ($Γ tack t_1: "bool"$, $Γ tack t_2: τ_2$),
      $Γ tack "assert" t_1; t_2: τ_2$,
    ),
  ),
)))
- We have a standard typing context Γ, pre-filled with the standard library functions from @prelude and functions to handle the basic logic, arithmetic and comparison operators.
- $∀ arrow(a)$ represents a _type scheme_ with many polymorphic variables α_i. These are used for let-polymorphism.

- TODO: Handle function type inference for patterns with default values
- TODO: "T-Rec-Concat" doesn't work really because of the generic subtyping rule. Further discussed in @records
- TODO: T-multi-let can be made simpler because we can always rewrite multi-let to let-chains. Recursion has to be accounted for, that is still an open question.
- TODO: T-With $l_i in.not Γ$ is too restrictive because shadowing labels are allowed, they will just not be used.

#figure(caption: "Suptyping rules", rect(inset: 20pt)[
  #flexwrap(
    main-spacing: 20pt,
    cross-spacing: 10pt,
    derive("S-Refl", (), $τ <= τ$),
    derive(
      "S-Trans",
      ($Σ tack τ_0 <= τ_1$, $Σ tack τ_1 <= τ_2$),
      $Σ tack τ_0 <= τ_2$,
    ),
    derive("S-Weaken", ($H$,), $Σ tack H$),
    derive("S-Assume", ($Σ,gt.tri H tack H$,), $Σ tack H$),
    derive("S-Hyp", ($H in Σ$,), $Σ tack H$),
    derive("S-Rec", (), $μ α.τ eq.triple [μ α.τ slash α]τ$),
    derive(
      "S-Or",
      ($∀ i, exists j,Σ tack τ_i <= τ'_j$,),
      $Σ tack union.sq_i τ_i <= union.sq_j τ'_j$,
    ),
    derive(
      "S-And",
      ($∀ i, exists j,Σ tack τ_j <= τ'_i$,),
      $Σ tack inter.sq_j τ_j <= inter.sq_i τ'_i$,
    ),
    derive(
      "S-Fun",
      ($lt.tri Σ tack τ_0 <= τ_1$, $lt.tri Σ tack τ_2 <= τ_3$),
      $Σ tack τ_1 arrow.long τ_2 <= τ_0 arrow.long τ_3$,
    ),
    derive(
      "S-Rcd",
      (),
      ${arrow(t) : arrow(τ)} eq.triple inter.sq_i {l_i : t_i}$,
    ),
    derive(
      "S-Depth",
      ($lt.tri Σ tack τ_1 <= τ_2$,),
      $Σ tack {l: τ_1} <= { l: τ_2}$,
    ),
    derive("S-Lst", ($ Γ tack τ_1 <= τ_2 $,), $Γ tack [τ_1] <= [τ_2]$),
  )
  $lt.tri(H_0, H_1) = lt.tri H_0, lt.tri H_1$
  $lt.tri(gt.tri H) = H$
  $lt.tri ( τ_0 <= τ_1) = τ_0 <= τ_1$
])

- ⊳ and ⊲ are used to add and remove _typing hypotheses_ that are formed during subtyping. Since applying such a hypothesis right after assumption, the later modality ⊳ is added and can only be removed after subtyping passed through a function or record construct. *TODO: check*
- The general idea of the typing algorithm is, that typing progresses and finally constraints are installed on type-variables. The rules need to be chosen in a way, that this general approach is possible.

What follows are the constraining rules used in the constrain subroutine of the implementation. It uses the subtyping rules and applies them to types. The underlying algorithm uses _levels_ to distinguish type variables that should be generalized and not. When entering a let-binding, the level is increased as every new type variable should adhere to _let-polymorphism_. During type inference, the algorithm also keeps track of the current level and only generalizes variables that are above the current level.
Instatiation is done by cloning the inherent structure of the type but adding new type variables above the current level which is done by the `freshen_above()` function.

#figure(caption: "Constraining rules")[
  Constraining takes two types τ₁ and τ₂ and constraints the first type to be subtype of the other.
  #v(1cm)
  $
    "constrain"((τ_1 → τ_2), (τ_3 → τ_4)) &arrow.squiggly "constrain"(τ_3, τ_1); "constrain"(τ_2, τ_4) &&#rule_name("C-Fun")\
    {τ_1}, {τ_2} &arrow.squiggly ∀i ∈ τ_2. "constrain"(τ_(1i), τ_(2i)) "  if A" &&#rule_name("C-Rec")\
    {τ_1},({τ_2}, #text("true", weight: "bold")) &arrow.squiggly ∀i ∈ τ_2. "constrain"(τ_(1i), τ_(2i))"   if A" &&#rule_name("C-Pat-Open") \
    {τ_1} , ({τ_2}, #text("false", weight: "bold")) &arrow.squiggly ∀i ∈ τ_2. "constrain"(τ_(1i), τ_(2i)) "  if A ∧ B  "&&#rule_name("C-Pat-Closed")\
    [τ_1] , [τ_2] &arrow.squiggly "constrain"(τ_1, τ_2) &&#rule_name("C-Array") \
    ("lo", "up")^n, τ^m "  if" m <= n &arrow.squiggly "up" ⩲ τ; ∀l ∈ "lo". "constrain"(l, τ) &&#rule_name("C-Var-⋆")\
    τ_1^n, τ_2 &arrow.squiggly "constrain("τ_1", extrude("τ_2", false, n))" &&#rule_name("C-Var-⋆")\
    τ^n , ("lo", "up")^m "if" n <= m &arrow.squiggly "lo" ⩲ τ; ∀u ∈ "ul". "constrain"(τ, u) &&#rule_name("C-⋆-Var")\
    τ_1, t_2^m &arrow.squiggly "constrain(extrude("τ_1", true, m), "τ_2")" &&#rule_name("C-⋆-Var")\
  $
  #v(1cm)
  *Conditions*:
  - A: Fields in $τ_2$ must be present in $τ_1$
  - B: $τ_1$ must only have the fields in $τ_2$

  *Remarks*
  - $("lo", "up")^n$ is used to match a _type variable_ and their lower and upper bounds. The superscript gives the _level_ of the variable that is used to handle generalization of variables.
  - $"lo" ⩲ τ$ is a shorthand for $"lo" = "lo" + τ$ and used to extend the list of upper or lower bounds.
]

- C-Fun is standard function subtyping.
- C-Rec implements width-subtyping of records in the standard manner. It also adds depth-subtyping due to recursion.
- C-Pat-open handles open patterns and has similar semantics to record constraining. The rule C-pat-Closed handles closed patterns with the extra condition that $t_1$ can not have any additional fields to $t_2$ which is inforced in condition $B$.
- Homogenous arrays are constrained as one would expect. Heterogenous arrays with many different field types, are constrained in order.
- What follows are the typvariable constraining rules. These depend on the levels of variables and their bounds $("lo", "up")$. C-Var-∗ handles the case where the constrained var is of higher type than the constraining var.
- $"extrude"(t)$ is used to create a new type of similar shape to the input but fixed type variables. We need this because lower bounds could refer to variables of higher level than the vars level letting them "escape".


= Equality
Attribute sets and lists are compared recursively, and are therefore fully evaluated.

= Datatypes
== Records <records>
Records are defined very simply in this type system. The only supported record type is a list of `label: type` mappings which can be added during subtyping. There is no way to reorder them, or remove some. During typing, multiple object constraints are concatenated, so there is a way to add new fields.

Two problems occur with the current implementation. Firstly, we have the `//` operator which implements _open record extension_. Given two records `A: { X: string, Y: int }` and `B: { X: int }` the open record concatenation between the two records `(C = A \\ B)` is `C: {X: int, Y: int}`. This together with the generic subtyping rule T-Sub leaves the type system unsound, because fields can be removed, leaving the record B empty (`T-SUB: B -> {}`). In this case, the type system would predict `A.X` to be of type `string` which is simply wrong after the application.

Since there is no way to remove labels from a record, we don't need lacks predicates! The only thing we need to care about is, how to merge record constraints.


== Context Strings
Context strings and dynamic lookup share the same syntax in that you can insert some arbitrary term `t` into braces with the following syntax `${t}`. For ordinary strings and paths, the value of `t` will be coerced into a string and added literally. From a typing perspective, this is the easy case because inserted values get a constraint of string and that's it. For dynamic lookup it gets trickier though.


== Dynamic Lookup <dynamic_lookup>
Context strings allow lookups of the form `a.${t}` where t is allowed to be any expression that ultimately reduces to a string. The reduced string is then used to index the record which a is supposed to be. Since a type system only computes a type and not the actual value, the only possible approach to handle first-class labels is to evaluate nix expressions to some extent. Writing a full evaluator is probably too much, but there could be heuristics for simple evaluation. One approach would be to work backwards from return statements in functions up until it gets too unwieldy.
This would also mean implementing the standard library functions like map, readToString etc. One ray of hope is that these were probably already implemented in Tvix.


= Constructs
== With Statements <with>
With statements allow introducing all bindings of a record into the following expression. For this, the first expression (A) in $"with " A"; "B$ has to reduce to a record. If that does not work, typing should raise an _error_. For explicit records, the following typing is straightforward. Just introduce all fields to the scope without shadowing and continue typechecking $B$. For the case that A is a type variable, it gets tricky however because of the generic subsumption rule. When A is subtyped like follows $A: {X: "int"} arrow A: {}$, then the field X would not be accessible in the function body.
The second problem is what I call the _attribution problem_. It occurs when there is a chain of with statements $"with "A; ("with "B;) t$ and A and B are type variables. Now when trying to lookup $x$ in t, it is unclear whether x came from B or A.


== Inherit Statements
Inherit statements can be handled as syntactic sugar.

== Function Patterns
Functions are pure and functional which helps in inferring a proper type. Patterns are given as records, showing which exact fields are wanted "as parameters". The ellipsis `(…)` allSows for arbitrary extra fields, and the `?` question mark syntax for default values.
To handle these, all expected record fields need to be present in the function argument so a record constraint with these fields can be added to the argument of the function. If a default value is given for some record fields, a constraint can be made on the arguments as well.

== Dunder Methods
There seem to be some special dunder methods for representations which are handled specially by the evaluator. I have not had the chance to look into it further.
An example is the `__functor` field that can be set on a record and lets the function be used as a functor.

== Closures
Its possible to capture variables in nix:

nix-repl> (let a = 2; in (b: a + b)) 3
5


= Laziness and Recursiveness
Laziness and recursion occur in two language constructs. The first one being _recursive records_ and the second one being _let bindings_. To evaluate them, a lazy evaluation scheme is needed which is currently implemented as follows:
When typing a let binding or record, the algorithm adds all name bindings to the context up-front. This way, referenced values will not be undefined when looked up, even if their definition was not type checked yet. The typecheck algorithm then starts with some arbitrary first label $A$ which may contain an unchecked expression labeled $B$.
When this undefined label $B$ is found, it is simply used to create upper and lower bounds (constraints). For empty type variables that is fine to do, but when we actually check this $B$, it will unfold and be constrained with upper and lower bounds. These bounds are missing on the typecheck run of $A$ then. An example would be `let f = a: a + 1; x = f b; b = "hi" in {}` In this case b would be constrained to be a number (because of the application and its implication) but afterwards it will get its "real" type which is string. Currently, the constraint error would be placed at the wrong location (that of the true definition).

#figure(
  ```nix
  rec { x = { x = x;};}.x;       # → { x = «repeated»; }
  let x = {x = y;}; y = x; in x  # → { x = «repeated»; }
  ```,
  caption: [Examples of recursive patterns from the nix repl],
)

= A Note on Implementation
One unique problem of nix is that everything (all 100,000 packages, the operating system, and the standard library) are rooted in a _single file_ at #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether you use a flake based system or not. To not get lost in the weeds, the nix evaluator heavily relies on the laziness features of the language to not evaluate all of the packages exhaustively. For the ultimate goal of auto-completing nixos options one would have to parse and type this very file with the goal to resolve the module system. This includes the standard library and bootstrapping code for the module system. To even reach it, the type inference algorithm has to support the same kind of laziness the nix evaluator uses to not get lost.


== Practical Type Inference in Face of Huge Syntax
Code inference in the general case is similar to depth-first search, digging down one syntax tree and only returning as soon as all branches have been exhausted. Since nix trees are huge, this approach is not feasible and one has to lean towards a breadth-first search style, which focuses on the currently inferred file and stops when "too far away". To achieve this behavior, the inference algorithm at some point has to decide to stop inference and jump to another unfinished function, remembering at which place it left off.
In the nix language, there are two natural places to do so. Laziness of records and let statements gives the natural approach that every newly named binding is a stop-point at which inference only proceeds as far as needed. One heuristic could be to go two more functions down and then return to the let or record to generate at least some approximation of the final type.

The import statement semantics of nix come in very handy at this point. Import statements act just as function calls with the only difference being, that the goto location is defined by path and not by name. Other than that, they can take arguments just as a function, and then try to apply given arguments to the file's expression.

This language design comes in very handy because that way, import statements do not occur at the top of the file where it would need to be decided how to continue typechecking them. They occur right at the location where they are needed, sometimes in let-expressions or record fields. This way, the laziness of records and let-expressions could already be enough to get laziness into the language.
As for the practical approach, I propose a new marker type which can be set to bindings of a context. This marker type should contain all the information to go back to type inference at a previous location. This probably means cloning the context or restoring it to the previous state – cloning is probably easier. Another approach could be to keep the names undefined and add another mapping between names and reconstruction information somewhere that acts as a fallback.


Some real-world example of import:
```nix
let
  overrides = {
    builtins = builtins // overrides;
  }
  // import ./lib.nix;
in
scopedImport overrides ./imported.nix
```

== Type Inference in a Language Server Setting
A language server setting adds one more level of complexity. A language server has to handle the communication between client (an editor like vim, emacs, vscode, etc.) and the server itself. It will be notified frequently of code changes and has to adapt to these changes almost immediately to not annoy the user. This is why rust-analyzer and nil, which I take as template for my own efforts, have chosen to use or create _incremental computation_ frameworks for the rust language.
The one used by rust-analyzer and nil (which is based off of rust-analyzer) is _salsa_. The name stems from the underlying red-green algorithm that decides whether a function needs to be reevaluated because its arguments changed or whether the memoized return value can be returned immediately.
In the end, salsa consists of _inputs_, _tracked functions_ and _tracked structs_. Inputs are divided into their durability and given to tracked functions. These tracked functions record the inputs and do some arbitrary computation with them. During these computations, the functions might create immutable tracked structs which can act as new inputs to other tracked functions. TraScked structs are interned into a db and act as a single identifier which are cheap to copy around and provide great performance benefits. With these components alone it is possible to create a hierarchy of pure functions that allow for reproducibility.

When implementing this incrementality framework one has to decide where to draw the line between tracking everything too closely such that the framework bloat adds latency and tracking too few intermediate results such that recomputation is heavy again. I currently choose to track inputs, and functions as well as initial calls.

The generalized structure of the three language servers has this structure. A user opens a file and the lsp client sends the text to the language server. The language server stores the text somewhere and adds it to the typing pipeline. The first step of this pipeline is of course lexing and parsing the file. Nil already provides a parser for lossless syntax trees that are handy for error reporting. The file is then lowered into another HIR which is more or less syntax independent and thus changes less frequently. This is necessary because otherwise everything would have to be recomputed all the time. After this, the HIR is given to the inference algorithm that tries to infer a type.

I am currently working to transition from salsa 0.17-pre2 to salsa 0.24 which is the newest version of salsa. As a lot has changed and virtually every part of code is touched, this is very time consuming.

= Code Overview
*Inputs of LSP*:
- `File {content: string, }`

*Inputs of infer:*
- `AST { With(ExprId, ExprId) }` (lowered AST with expressions from the arena)

*Tracked structs:*
- `Ty { Lambda(Ty), With(Ty, Ty)}` (enum that stores the whole AST)
- `Context {bindings: Vec<_>, }`
- `TyVar {lower_bounds: Vec<Ty>, upper_bounds: Vec<Ty>, level: int}`

*Functions:*
- `infer` (main work)
  - calls itself with subtrees of the AST and new contexts
  - *Mutates* context
- `constrain` (constrains two types to be the same)
  - calls itself with subtrees of Ty and might cycle
  - *Mutates* Type variables → *Changes context*
- `coalesce` (reduce types to unions and intersections)
  - Create new types
- `extrude` (fix levels of problematic variables in a type scheme)
  - only creates new types
- `freshen_above` (Add new type variables at level > x)
  - only creates new types

#pagebreak()
= Appendix A <prelude>
== List of Builtins
- *abort* `s` : Abort Nix expression evaluation and print the error message `s`.
- *add* `e1 e2` : Return the sum of the numbers `e1` and `e2`.
- *addDrvOutputDependencies* `s` : Copy string `s` while turning constant string context elements into derivation-deep string context.
- *all* `pred list` : Return `true` if `pred` returns `true` for all elements of `list`, else `false`.
- *any* `pred list` : Return `true` if `pred` returns `true` for any element of `list`, else `false`.
- *attrNames* `set` : Return the attribute names of `set`, sorted alphabetically.
- *attrValues* `set` : Return the values of attributes in `set`, ordered by sorted names.
- *baseNameOf* `x` : Return the last component of path or string `x`.
- *bitAnd* `e1 e2` : Bitwise AND of integers `e1` and `e2`.
- *bitOr* `e1 e2` : Bitwise OR of integers `e1` and `e2`.
- *bitXor* `e1 e2` : Bitwise XOR of integers `e1` and `e2`.
- *break* `v` : In debug mode, pause evaluation and enter REPL; otherwise return `v`.
- *builtins* : A set containing all built-in functions and values.
- *catAttrs* `attr list` : Collect the attribute `attr` from each set in `list`, ignoring sets without it.
- *ceil* `double` : Round `double` up to the nearest integer.
- *compareVersions* `s1 s2` : Compare version strings; returns `-1`, `0`, or `1`.
- *concatLists* `lists` : Flatten a list of lists into a single list.
- *concatMap* `f list` : Equivalent to `concatLists (map f list)`.
- *concatStringsSep* `sep list` : Join strings in `list` with separator `sep`.
- *convertHash* `args` : Convert a hash string between formats (base16, sha256, SRI, etc.).
- *currentSystem* : System string like `"x86_64-linux"`.
- *currentTime* : Unix time at moment of evaluation (cached).
- *deepSeq* `e1 e2` : Like `seq`, but fully evaluate nested structures in `e1` first.
- *dirOf* `s` : Directory component of string `s`.
- *div* `e1 e2` : Integer division.
- *elem* `x xs` : `true` if `x` is in list `xs`.
- *elemAt* `xs n` : Return the `n`-th element of `xs`.
- *false* : Boolean literal `false`.
- *fetchClosure* `args` : Fetch a store path closure from a binary cache.
- *fetchGit* `args` : Fetch a Git repo or revision.
- *fetchTarball* `args` : Download and unpack a tarball.
- *fetchTree* `input` : Fetch a tree or file with metadata.
- *fetchurl* `arg` : Download a URL and return store path.
- *filter* `f list` : Return elements where `f` yields `true`.
- *filterSource* `pred path` : Copy sources filtering by `pred`.
- *findFile* `search lookup` : Search `lookup` in `search` path.
- *floor* `double` : Round `double` down to nearest integer.
- *foldl'* `op nul list` : Left fold over `list` with `op`.
- *fromJSON* `e` : Parse JSON string `e` into Nix value.
- *fromTOML* `e` : Parse TOML string `e` into Nix value.
- *functionArgs* `f` : Return formal argument set of function `f`.
- *genList* `generator length` : Generate list of given `length` using `generator`.
- *genericClosure* `attrset` : Compute transitive closure of a relation.
- *getAttr* `s set` : Return attribute `s` from `set`.
- *getContext* `s` : Return derivation context of string `s`.
- *getEnv* `s` : Return environment variable `s`.
- *getFlake* `args` : Fetch flake reference and outputs.
- *groupBy* `f list` : Group elements by key `f(element)`.
- *hasAttr* `s set` : `true` if `set` has attribute `s`.
- *hasContext* `s` : `true` if string `s` has nonempty context.
- *hashFile* `type p` : Compute hash of file at `p`.
- *hashString* `type s` : Compute hash of string `s`.
- *head* `list` : First element of `list`.
- *import* `path` : Load and evaluate Nix file at `path`.
- *intersectAttrs* `e1 e2` : Attributes in `e2` whose names occur in `e1`.
- *isAttrs* `e` : `true` if `e` is an attribute set.
- *isBool* `e` : `true` if `e` is a boolean.
- *isFloat* `e` : `true` if `e` is a float.
- *isFunction* `e` : `true` if `e` is a function.
- *isInt* `e` : `true` if `e` is an integer.
- *isList* `e` : `true` if `e` is a list.
- *isNull* `e` : `true` if `e` is `null`.
- *isPath* `e` : `true` if `e` is a path.
- *isString* `e` : `true` if `e` is a string.
- *langVersion* : Integer of current Nix language version.
- *length* `e` : Length of list `e`.
- *lessThan* `e1 e2` : `true` if `e1 < e2`.
- *listToAttrs* `e` : Convert list of `{name, value}` to attrset.
- *map* `f list` : Apply `f` to each element of `list`.
- *mapAttrs* `f attrset` : Apply `f` to each attribute in `attrset`.
- *match* `regex str` : If `regex` matches `str`, return capture groups, else `null`.
- *mul* `e1 e2` : Multiply integers `e1 * e2`.
- *nixPath* : List of search path entries for lookups.
- *nixVersion* : String version of Nix.
- *null* : Literal `null`.
- *outputOf* `drv out` : Return output path of derivation.
- *parseDrvName* `s` : Parse a derivation name into components.

#page[
  #bibliography("all.bib")
]
