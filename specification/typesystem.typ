#import "functions.typ": *
#import "comparison.typ": comparison
#set page(height: auto)

== Todo
1. Proper shadowing behaviour
2. Proper substitutions (rec and none-rec)
3. Paste in other semantic
4. Reduction of
5. Document dunder methods


#let char = `[^"$\] | $(?!{) | \.`
#let interstr = `${^}*}`
#let string = `"(c* inter)* c*`
#let identstring = `''todo''`
#let boolean = `true | false`
#let filepath = `(./|~/|/)([a-zA-Z.]+/?)+`
#let number = `([0-9]*.)?[0-9]+`
#let label = `[A-Za-z_][A-Za-z0-9_'-]*`
#let searchpath = `<[A-Za-z_]*>`

== Syntax
#let basetypes = subbox(caption: "Literals")[
  $
                                  c & ::= char \
                            "inter" & ::= interstr \
             #type_name("String") s & ::= string \
       #type_name("Ident String") s & ::= identstring \
            #type_name("Boolean") b & ::= boolean \
    #type_name("File-Path") rho.alt & ::= filepath \
             #type_name("Number") n & ::= number \
              #type_name("Label") l & ::= label \
        #type_name("Search Path") l & ::= searchpath \
  $
]

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
    d, h & ::= t | ε \
       e & ::= l | l space ? space d \
       p & ::= { overline(e\,) } | { overline(e\,) … } | l \
  $])

#let syntax = figure(
  caption: "Subset of Nix Syntax.",
  rect(width: 100%, grid(
    columns: 2,
    align: left,
    inset: 8pt,
    grid.cell(rowspan: 3, general),
    basetypes,
    inherit,
    patterns,
    subbox(caption: "Shorthands")[
      #set math.equation(numbering: "(1)")
      $ p : t space @ space ε = p : t $
      $ l space ? space ε : l $
      $ • ::= #b[or] | "//" | ⧺ | " ? " $
    ],
  )),
)
#syntax


== Reduction Rules
#let reduction = figure(
  caption: "Nix reduction rules, context and values.",
  rect(width: 100%, inset: 20pt, stack(
    spacing: 20pt,
    subbox(caption: "Values")[$
      v ::= p: t | l | {overline(a\;)} | #b[rec] {overline(a\;)}
    $],
    subbox(
      caption: "Evaluation Context",
      $
        E[□] & := □ | □ space t | (□).l | (v).□ \
             & | #b[if ] □ #b[ then ] t #b[ else ] t \
             & | #b[with ] □; t | #b[with ] v; □ \
             & | #b[inherit ] (ρ) space □; \
             & | □ • t | v • t \
      $,
    ),
    subbox(
      caption: "Reduction rules",
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
      $
        #rule_name("R-Fun")&& (l: t_2)t_1 & arrow.long t_2[l := t_1] \
        #rule_name("R-Fun-Pat")&& ({oi(l_i)}: t){oi(l_i \= t_i)} & arrow.long
        t [oi(l_i := t_i)] \
        #rule_name("R-Fun-Pat-Open")&& ({oi(l_i)\, ...}: t) {oj(l_i = t_i)} & arrow.long
        t [oi(l_i := t_i)] #h(0.5cm) &&&∀i. ∃ j. i eq j \
        #rule_name("R-Fun-Pat-Default")&&({oi(e_i)}: t){oj(l_j = t_j)} & arrow.long
        t [oj(l_j = t_j)][oi(l_i := d_i)] \
        #rule_name("R-Fun-Pat-Default-Open")&&({oi(e_i), …}: t){oj(l_j = t_j), …} & arrow.long
        t [oj(l_j = t_j)][oi(l_i := d_i)] &&&∀i. ∃ j. i eq j\
      $,
    ),
  )),
)
#reduction

== Types
#let types = figure(
  caption: "Types of nix.",
  rect(width: 100%, inset: 20pt, grid(
    columns: 1,
    align: left,
    inset: 8pt,
    grid.cell(rowspan: 2, subbox(
      caption: "Types",
      $
        #type_name("Type") tau ::= & "bool" | "string" | "path" | "num" \
        & | τ -> τ | ⦃ oi(p) ⦄^(b,τ) -> τ| {l: τ} | [τ] | [overline(τ)] | alpha \
        & | ⊥^diamond.small | τ ∨^diamond.small τ \
        #type_name("Pattern Element") p & := τ | τ^? \
        #type_name("Polymorphic type") σ & := ∀Xi. τ \
        #type_name("Mode") diamond.small & := · | arrow.r.turn\
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
  )),
)
#types

== Shorthands
#let shorthands = figure(
  caption: "Shorthands",
  rect(width: 100%, inset: 20pt, stack(
    derive(
      "T-with",
      (
        $#b[let] _"with" {overline(#b[nonrec] d)} #b[in] t$,
      ),
      $#b[with] {overline(#b[nonrec] d)}; t_2$,
    ),
    derive(
      "T-let-in",
      (
        $#b[let] _"abs" {overline(#b[nonrec] d)} #b[in] t$,
      ),
      $#b[let] oi(l_i \= t_i\;) #b[in] t$,
    ),
  )),
)
#shorthands

== Typing Rules
#let typing_rules = figure(
  caption: "Nix typing rules",
  rect(width: 100%, inset: 20pt, stack(
    spacing: 3em,
    many_wrapping_derives(
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
      derive("T-Negate", ($Xi, Γ tack e: "bool"$,), $Xi, Γ tack !e: "bool"$),
      // derive("T-Asc", ($Ξ,Γ ⊢ t : τ$,), $Ξ,Γ ⊢ (t: τ) : τ$),
    ),
    // line(length: 100%),
    many_wrapping_derives(
      caption: "Language Constructs",
      derive(
        "T-Multi-Let",
        (
          $Γ overline([x_i: τ_i tack t_i : τ_i]^i)$,
          $Γ overline([x_i:∀ arrow(α). τ_i]^i) tack t: τ$,
        ),
        $Γ tack #b[let] x_0 = t_0; ... ; x_n = t_n #b[in] t: τ$,
      ),
      derive(
        "T-If",
        ($Γ tack t_1: "bool"$, $Γ tack t_2: τ$, $Γ tack t_3: τ$),
        $ #b[if] t_1 #b[then] t_2 #b[else] t_3: τ $,
      ),
      derive(
        "T-With",
        (
          $Γ tack t_1 : {arrow(l): arrow(τ)}$,
          $Γ, l_0 : τ_0, ..., l_n: τ_n tack t_2: τ$,
          $l_i in.not Γ$,
        ),
        $Γ tack #b[with] t_1; t_2 : τ$,
      ),
      derive(
        "T-Assert-Pos",
        ($Γ tack t_1: "bool"$, $Γ tack t_2: τ_2$),
        $Γ tack #b[assert] t_1; t_2: τ_2$,
      ),
    ),
    many_wrapping_derives(
      caption: "Functions with Patterns",
      derive(
        "T-Abs-Pat",
        (todo[$Ξ, Γ, oi(x\: τ) tack t: τ_2$],),
        $Ξ, Γ tack ({oi(e_i)}: t): oi(τ) → τ_2$,
      ),
      derive(
        "T-Abs-Pat-Open",
        (todo[$Ξ, Γ, oi(x\: τ) tack t: τ_2$],),
        $Ξ, Γ tack ({oi(e_i), …}: t): oi(τ) → τ_2$,
      ),
    ),
    // line(length: 100%),
    many_wrapping_derives(
      caption: "Records",
      derive(
        "T-Rcd",
        ($Ξ, Γ tack t_0: τ_0$, "...", $Ξ, Γ tack t_n: τ_n$),
        $Ξ, Γ tack {arrow(l): arrow(t)}: {arrow(l): arrow(τ)}$,
      ),
      derive("T-Proj", ($ Ξ, Γ tack t: {l: τ} $,), $Ξ, Γ tack t.l: τ$),
      derive(
        "T-Or-Neg",
        ($Xi, Γ tack t_1: {l: τ_1}$, $Xi, Γ tack t_2: τ_2$),
        $Xi, Γ tack (t_1).l #b[or] t_2: τ_1$,
      ),
      derive(
        "T-Or-Pos",
        ($Xi, Γ tack t_1: τ_1$, $l ∉ τ_1$, $Xi, Γ tack t_2: τ_2$),
        todo($Xi, Γ tack (t_1).l #b[or] t_2: τ_2$),
      ),
      derive(
        "T-Rec-Concat",
        ($Ξ, Γ tack a: { oi(l\: τ) }$, $Ξ, Γ tack b: { l_j: τ_j }$),
        todo[$Ξ, Γ tack a "//" b: {..b, ..a}$],
      ),
      derive(
        "T-Check",
        ($Xi, Γ tack e: {..}$,),
        $Xi, Γ tack e #b[#v(0.1pt)?#v(0.1pt)] l: "bool"$,
      ),
    ),
    // line(length: 100%),
    many_wrapping_derives(
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
    // sub_typing_rules(
    //   caption: "Operators",
    // ),
    // line(length: 100%),
  )),
)
#typing_rules

== Subtyping Rules
#let subtyping = figure(
  caption: "Nix suptyping rules.",
  rect(width: 100%, inset: 20pt)[
    #flexwrap(
      main-spacing: 20pt,
      cross-spacing: 10pt,
      derive("S-Refl", (), $τ <= τ$),
      derive("S-ToB", (), $τ rotate(≤) rotate(top)$),
      derive("S-CompL", (), $τ ∨ ¬τ rotate(≥) rotate(top)$),
      derive("S-NegInv", ($Σ tack τ_1 ≤ τ_2$,), $Σ tack ¬τ_1 <= ¬τ_2$),
      derive("S-AndOr11", (), $τ_1 rotate(∨) τ_2 rotate(≥) τ_1$),
      derive("S-AndOr11", (), $τ_1 rotate(∨) τ_2 rotate(≥) τ_2$),
      derive("S-AndOr2", (), $τ_1 rotate(∨) τ_2 rotate(≥) τ_2$),
      derive(
        "S-Distrib",
        (),
        $τ rotate(∧) (τ_1 rotate(∨) τ_2) rotate(≤) (τ rotate(∧) τ_1) rotate(∨)(τ rotate(∧) τ_2)$,
      ),

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
  ],
)
#subtyping

== Constraining
#let constraining = figure(
  caption: "New Constraining Rules using normal forms",
  rect(width: 100%, inset: 20pt)[
    #subrules(caption: $Σ ⊢ τ ≪ τ => Ξ$, flexwrap(
      main-spacing: 20pt,
      cross-spacing: 10pt,
      derive("C-Hyp", ($(τ_1 ≪ τ_2) ∈ Σ$,), $Σ ⊢ τ_1 ≪ τ_2 => ε$),
      derive(
        "C-Assum",
        ($(τ_1 ≪τ_2) ∉ Σ$, $Σ ·⊳(τ_1 ≤ τ_2) ⊢ "dnf"^0_Σ (τ_1 ∧ ¬τ_2) => Ξ$),
        $$,
      ),
      derive(
        "C-Or",
        ($Σ ⊢ D^0 => Ξ$, $Ξ · Σ ⊢ C^0 => Ξ'$),
        $D^0 ∨ C^0 => Ξ · Ξ'$,
      ),
      derive("C-Bot", ($$,), $Σ ⊢ ⊥ => ε$),
      derive("C-Not-Bot", ($$,), $Σ ⊢ I^0 ∧ ¬⊥ => #b[err]$),
    )),
    #subrules(caption: $Σ ⊢ τ ≪ τ => Ξ$, flexwrap(
      main-spacing: 20pt,
      cross-spacing: 10pt,
      derive(
        "C-Fun1",
        ($⊲Σ ⊢ D_3 ≪ D_1 => Ξ$, $Ξ ·⊲Σ ⊢ D_2 ≪ D_4 => Ξ'$),
        $Σ ⊢ 𝓘[D_1 -> D_2] ∧ ¬(D_3 -> D_4) => Ξ ·Ξ'$,
      ),
      derive("C-Fun2", ($$,), $Σ ⊢ 𝓘^-> [top]∧¬(D_1 -> D_2) => #b[err]$),
      derive(
        "C-Rcd1",
        ($y ∈ S$, $⊲Σ ⊢ D_y ≪ D => Ξ$),
        $Σ ⊢ I[{#overline[x: D_x]^{x ∈ S}}]∧¬{y: D} => Ξ$,
      ),
      derive(
        "C-Rcd2",
        ($y ∉ S$,),
        $Σ ⊢ I[{#overline[x: D_x]^{x ∈ S}}]∧¬{y: D} => #b[err]$,
      ),
      derive("C-Rcd3", ($$,), $Σ ⊢ 𝓘^({})[top] ∧ ¬{x: D} => #b[err]$),
      derive(
        "C-Var1",
        ($Σ ·(α ≪ ¬C) ⊢ "lb"_Σ ≪ ¬C => Ξ$,),
        $Σ ⊢ C ∧ a => Ξ ·(α ≪ ¬C)$,
      ),
      derive(
        "C-Var2",
        ($Σ ·(C ≤ a) ⊢ C ≪ "ub"_Σ(α) => Ξ$,),
        $Σ ⊢ C ∧ ¬α => Ξ · (C ≤ α)$,
      ),
    ))
  ],
)
#constraining
