#import "functions.typ": *
#set page(height: auto)


== Syntax
#let literals = subbox(caption: "Literals")[
  #set raw(lang: none)
  #show raw: set text(fill: red)

  #let strChar = `[^"$\\]|\$(?!\{)|\\.`
  #let iStrChr = `[^$']|\$\$|\$(?!\{)|''[$']|''\\.|'(?!')`

  #let interpol = $\${ t }$
  #let string = `"(c* i)* c*"`
  #let identstring = `''(c* i)* c*''`
  #let boolean = `true | false`
  #let filepath = `(./|~/|/)([a-zA-Z.]+/?)+`
  #let number = `([0-9]*.)?[0-9]+`
  #let label = `[A-Za-z_][A-Za-z0-9_'-]*`
  #let searchpath = `<[A-Za-z_]*>`
  #let uri = `[a-zA-Z][a-zA-Z0-9+.-]*://[^[ ]]+`


  $
      #type_name("Interpolation") i & ::= interpol \
             #type_name("String") s & ::= string \
                                    & "where" c ::= strChar \
         #type_name("Ident String") & | identstring \
                                    & "where" c ::= omitted) \
            #type_name("Boolean") b & ::= boolean \
    #type_name("File-Path") rho.alt & ::= filepath \
             #type_name("Number") n & ::= number \
              #type_name("Label") l & ::= label \
      #type_name("Search Path") Rho & ::= searchpath \
                #type_name("Uri") u & ::= uri \
  $
]

#let general = subbox(caption: "Terms")[
  $
    t ::= b &| s | rho.alt | Rho | n | l | #b[null] \
    #type_name("Record") &| {oa} | #b[rec] {oa} \
    #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
    #type_name("Function") &| p "@ "h : t #v(2em) #type_name("where") h ::= l | ε \
    #type_name("Let-Statement") &| #b[let] oa #b[in] t\
    #type_name("Conditional") &| #b[if] t #b[then] t #b[else] t \
    #type_name("With-Statement") &| #b[with] t; t \
    #type_name("Assert-Statement") &| #b[assert] t; t \
    #type_name("Import-Statement") &| #b[import] t; \
  $
]

#let operators = subbox(caption: "Operators")[
  $
    #type_name("Algebraic") & ast.op.o := && | t + t | t - t | t * t | t \/ t \
    #type_name("Logic") & && | t -> t | ! t | t "&&" t \
    #type_name("Comparison") & && | t < t | t <= t | t == t \
    & && | t "!=" t | t > t | t >= t \
    #type_name("Pipe") & && | #b[<|] | #b[|>] \
    #type_name("Record") & && | t space ? ρ | t.ρ #b[or] t | t \/\/ t | t.l | t.i | t.s \
    #type_name("Array") & && | t ⧺ t \
  $
]

#let assignment = subbox(caption: "Assignment")[
  $
    #type_name("Label") l & := ℓ | s | \${t} "TODO" \
    #type_name("Assignment") α & ::= l = t; | s = t; | \${t} = t; ι \
    #type_name("Inherit") ι & ::= #b[inherit] overline(l); | #b[inherit] (ρ) space overline(l); \
    #type_name("Path") ρ & ::= l | ρ.l | ρ.i \
  $
]

#let rewrites = subbox(
  caption: "Rewrites",
  $
    #rule_name("RR-Inherit")&& #b[inherit] overline(l); & arrow.twohead overline(x := nonrec x); \
    #rule_name("RR-Inherit")&& #b[inherit] (ρ) space overline(l); & arrow.twohead overline(x := ρ.x); \
    #rule_name("RR-Def-Inner")&& { l_1 . l_2 space … space .l_n = t; } &arrow.twohead {l_1 = { l_2 = {l_n = t;};};} \
    #rule_name("RR-Rec")&& #b[rec] {oa} &arrow.twohead { l = #b[rec] t | l = t; ∈ α } \
    #rule_name("RR-Non-Rec")&& {oa} &arrow.twohead { l = #b[nonrec] t | l = t; ∈ α } \
  $,
)

#let patterns = box([
  #text(weight: "bold", smallcaps("Patterns"))
  $
    d & ::= t | ε \
    e & ::= l | l space ¿ space d \
    p & ::= { overline(e) } | { overline(e), … } | x \
  $])

#let syntax = figure(
  caption: "The Nix language.",
  box(width: 120%, grid(
    columns: 2,
    align: left,
    inset: 8pt,
    general, literals,
    operators, assignment,
    patterns,

    subbox(caption: "Shorthands")[
      // #set math.equation(numbering: "(1)")
      $
        p : t space @ space ε & eq.def p : t \
                   h" @ "p: t & eq.def p" @ "h: t \
            l space ¿ space ε & eq.def l \
      $
    ],
    grid.cell(colspan: 2, rewrites),
  )),
)

#syntax


== Reduction Rules
#let reduction = figure(
  caption: "Nix reduction rules, evaluation context and values.",
  box(width: 100%, stack(
    spacing: 20pt,
    subbox(caption: "Values")[$
      v ::= p: t | l | {overline(α)} | #b[rec] {overline(α)}
    $],
    subbox(
      caption: "Evaluation Context",
      $
        E[□] & := □ space t | (□).l | (□).dyn | (□)."s" | v.□ \
             & | #b[if ] □ #b[ then ] t #b[ else ] t | #b[with ] □; t | #b[with ] v; □ \
             & | #b[inherit ] (ρ) space □; | □ ast.op.o t | v ast.op.o t \
      $,
    ),
    subbox(
      caption: "Reduction rules",
      [
        $
          #rule_name("Kind") k := #b[with] | #b[abs] #h(5cm) #rule_name("Recursiveness") ω := #b[rec] | #b[nonrec]
        $
        $
          #rule_name("R-Final")&& x_("Some" (k space e)) &arrow.long e \
          #rule_name("R-Attr-Rec")&& {overline(a)} &arrow.long {"unfold" overline(a)} &&&"if" ∃x,d. space x := rec d ∈ overline(a) \
          #rule_name("R-Abs")&& (x: t_1) t_2 &arrow.long t_1[x := abs t_2] \
          #rule_name("R-Match")&& (m: t) {overline(#b[nonrec] d)} &arrow.long t["indirects" oα] &&&"if" m ~ overline(d) arrow.squiggly oα \
          #rule_name("R-With")&& #b[with] {oa}; t &arrow.long t[{ x := abs t | l = t ∈ oa }] \
          #rule_name("R-Let")&& #b[let] oi(l_i = t_i\;) #b[in] t &arrow.long t[{ l_i := abs t_i }]\
          #rule_name("R-Let-Rec")&& #b[let] {oi(l_i = t_i\;) "body" = t} &arrow.long t[{ l_i := abs t_i }] \
          #rule_name("R-Cond-True")&& #b[if] "true" #b[ then ] t_1 #b[ else ]t_2 & arrow.long t_1 \
          #rule_name("R-Cond-False")&& #b[if] "false" #b[then ] t_1 #b[ else ]t_2 & arrow.long t_2 \
          #rule_name("R-Lookup")&& {oa}.l & arrow.long t &&&"if" k space l = t ∈ oa\
          #rule_name("R-Lookup-str")&& {oa}.s & arrow.long t &&&"if" k space s = t ∈ oa\
          #rule_name("R-Lookup-dyn")&& {oa}.\${s} & arrow.long {oa}.s \
          #rule_name("R-Lookup-Default-Pos")&& {oa}.l #b[or] t & arrow.long
          t &&&"if" k space l ∈ oa \
          #rule_name("R-Lookup-Default-Neg")&& {oa}.l #b[or] t & arrow.long
          t &&&"if" k space l ∉ oa \
          #rule_name("R-Has-Pos")&& {overline(α)}" ? "l & arrow.long "true" &&&"if" k space l ∈ oa \
          #rule_name("R-Has-Neg")&& {overline(α)}" ? "l & arrow.long "false" &&&"if" k space l ∉ oa \
          #rule_name("R-Has-Path-Pos")&& {overline(α)}" ? "l.ρ & arrow.long "true" \&\& space (t " ? " ρ) &&&"if" k space l = t ∈ oa \
          #rule_name("R-Has-Path-Neg")&& {overline(α)}" ? "l.ρ & arrow.long "false" \&\& space (t " ? " ρ) &&&"if" k space l = t ∉ oa\
          #rule_name("R-Array-Concat")&& [overline(t_1) ] ⧺ [overline(t_2)] & arrow.long [overline(t_1) space overline(t_2)] \
          #rule_name("R-Record-Concat")&& {oa_1} "//" {oa_2} & arrow.long {oa_1} union.arrow {oa_2 } \
          #rule_name("R-Import")&& #b[import] 𝜚; & arrow.long t &&&"if" 𝜚 arrow.squiggly t \
          #rule_name("R-Context") && t arrow.long t' &==> E[t] arrow.long E[t'] \
        $
      ],
    ),
    subbox(
      caption: "Auxiliaries",
      $
        "unfold" oα := &{ x := #b[nonrec] t | x := #b[nonrec] t ∈ oα} union
        &{ x := #b[nonrec] t["indirects" oα] | x := #b[rec] t ∈ oα} \
        "indirects" oα := &{x := #b[abs] {oα}.x | x ∈ oα }
      $,
    ),
  )),
)
#reduction

== Substitution
#let subs = $overline(sigma.alt)$
#let substitutions = $
       x_(σ?)[subs] & := cases(
                        x_("Some" ("abs" d)) & "if" x = "with" e ∈ subs "and" sigma^? = "Some"(abs d),
                        x_("Some" (k space e)) & "if" x = k space e ∈ subs,
                        x_(σ^?) & otherwise,
                      ) \
     (λ x. e)[subs] & := λ x. e[subs] \
  (λ {p?}. e)[subs] & := λ {p[subs]}: e[subs] \
$

== Matching

#let matching = figure(
  caption: "Matching.",
  box(flexbox(
    derive(
      "M-Id",
      (),
      ${∅, …} ~ overline(d) arrow.squiggly ∅$,
    ),
    derive(
      "M-Var",
      (
        ${overline(e), …} ~ overline(d) arrow.squiggly oα$,
        $x ∉ "dom "overline(e)$,
        $x ∉ "dom "overline(d)$,
      ),
      ${overline(e)⟨x ¿ ε⟩, …} ~ overline(d)⟨x := d⟩ arrow.squiggly oα⟨x := #b[nonrec] d⟩$,
    ),
    derive(
      "M-Default",
      (
        ${overline(e), …} ~ overline(d) arrow.squiggly oα$,
        $x ∉ "dom" overline(e)$,
        $x ∉ "dom" overline(d)$,
      ),
      ${overline(e)⟨x ¿ e⟩, …} ~ overline(d) arrow.squiggly oα⟨x := #b[rec] e⟩$,
    ),
    derive(
      "M-Pat-Open",
      (
        ${overline(e), …} ~ overline(d) arrow.squiggly oα$,
        $"dom "overline(d) subset.eq "dom "overline(e)$,
      ),
      ${overline(e)} ~ overline(d) arrow.squiggly oα$,
    ),
    derive(
      "M-Binding",
      (
        $p ~ overline(d) arrow.squiggly oα$,
      ),
      $x space @ space p ~ overline(d) arrow.squiggly oα⟨x := overline(d)⟩$,
    ),
  )),
)
#matching



== Types
#let types = box(width: 100%, grid(
  columns: 1fr,
  align: left,
  grid.cell(rowspan: 2, subbox(
    caption: "Types",
    $
      #type_name("Type")&& tau & ::= τ -> τ | ⦃ oi(p) ⦄^b -> τ| {l: τ} | [τ] | [overline(τ)] | alpha \
      #type_name("Groundtypes")&& & | "bool" | "string" | "path" | "float" | "int"\
      #type_name("Connectives")&& & | ⊥ | top | τ ∨ τ | τ ∧ τ | ¬τ \
      #type_name("Pattern Element")&& p & := τ | τ^? \
      #type_name("Polymorphic type")&& σ & := ∀Xi. τ \
      // #type_name("Mode")&& diamond.small & := + | -\
    $,
  )),
  subbox(
    caption: "Contexts",
    $
      #type_name("Typing Context") Γ & ::= ε | Γ · (l : τ) | Γ · (l : σ) \
      // #type_name("Subtyping Context") Σ & ::= Xi | Σ · (τ ≤ τ) | Σ · ⊳(τ ≤ τ) \
      // #type_name("Constraint Context") Xi & ::= ε | Xi · (τ ≤ τ) | Xi · (τ ≤ α) | Xi · #text(weight: "bold", "err") \
    $,
  ),
))

#types


== Typing Rules

#let basic_typing_rules = figure(
  caption: "Basic nix typing rules.",
  box(width: 100%, [
    #flexbox(
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
      derive(
        "T-If",
        ($Γ tack t_1: "bool"$, $Γ tack t_2: τ$, $Γ tack t_3: τ$),
        $ #b[if] t_1 #b[then] t_2 #b[else] t_3: τ $,
      ),
      derive(
        "T-Assert",
        ($Γ tack t_1: "bool"$, $Γ tack t_2: τ_2$),
        $Γ tack #b[assert] t_1; t_2: τ_2$,
      ),
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
        ($Ξ, Γ tack a: [overline(τ_1)]$, $Ξ, Γ tack b: [overline(τ_2)]$),
        $Ξ, Γ tack a "⧺" b: [overline(τ_1) space overline(τ_2)]$,
      ),
    )
  ]),
)
#basic_typing_rules


#let record_typing_rules = flexbox(
  derive(
    "T-Rcd",
    ($Ξ, Γ ⊢ t_0: τ_0$, "...", $Ξ, Γ ⊢ t_n: τ_n$),
    $Ξ, Γ ⊢ {oa}: {oa}$,
  ),
  derive("T-Proj", ($ Ξ, Γ ⊢ t: {l: τ} $,), $Ξ, Γ ⊢ t.l: τ$),
  derive(
    "T-Or-Neg",
    ($Ξ, Γ ⊢ t_1: {l: τ_1}$, $l ∈ τ_1$, $Ξ, Γ ⊢ t_2: τ_2$),
    $Ξ, Γ ⊢ (t_1).l #b[or] t_2: τ_1$,
  ),
  derive(
    "T-Or-Pos",
    ($Ξ, Γ ⊢ t_1: τ_1$, $l ∉ τ_1$, $Ξ, Γ ⊢ t_2: τ_2$),
    $Ξ, Γ ⊢ (t_1).l #b[or] t_2: τ_2$,
  ),
  derive(
    "T-Rec-Concat",
    ($Ξ, Γ ⊢ a: { oa }$, $Ξ, Γ ⊢ b: { oa' }$),
    $Ξ, Γ ⊢ a "//" b: {oa} union.arrow {oa}$,
  ),
  derive(
    "T-Check",
    ($Ξ, Γ ⊢ e: {..}$,),
    $Ξ, Γ ⊢ e #b[?] l: "bool"$,
  ),
  derive(
    "T-Acc-dyn",
    ($Γ ⊢ a: { l: τ }$, $t : "Lab" l$),
    $Γ ⊢ a.\${t} : τ$,
  ),
)
#figure(caption: "Record typing rules", record_typing_rules)

#let function_typing_rules = flexbox(
  derive("T-Abs1", $Γ, x: τ_1 ⊢ e : τ_2$, $Γ ⊢ (x: e) : τ_1 → τ_2$),
  derive(
    "T-Abs2",
    $Γ, overline(e_i : τ_i) ⊢ e: τ_2$,
    $Γ ⊢ ({oa}: e) : {α}^- → τ_2$,
  ),


  derive(
    "T-Abs3",
    $Γ, overline(e_i : τ_i) ⊢ e: τ_2$,
    $Γ ⊢ ({oa,...}: e) : {α}^+ → τ_2$,
  ),


  derive(
    "T-App1",
    ($Γ ⊢ e_1: τ_1 → τ_2$, $Γ ⊢ e_2: τ_3 ≤ τ_1$),
    $Γ ⊢ (x: e_1) e_2: τ_2$,
  ),


  derive(
    "T-App2",
    ($Γ ⊢ e_1: {overline(α)}^- → τ_2$, $Γ ⊢ e_2: τ_3 ≤ τ_1$),
    $Γ ⊢ (x: e_1) e_2: τ_2$,
  ),


  derive(
    "T-App3",
    ($Γ ⊢ e_1: {overline(α)}^+ → τ_2$, $Γ ⊢ e_2: τ_1$),
    $Γ ⊢ (x: e_1) e_2: τ_2$,
  ),
)


#let operator_typing_rules = figure(caption: "Operator typing rules.", flexbox(
  derive(
    "T-Op-Arith",
    ($Γ tack t_1: num$, $Γ tack t_2: num$, $"op" ϵ space [-, +, \/, *]$),
    $Γ tack t_1 "op" t_2: num$,
  ),
  derive(
    "T-Op-Logic",
    ($Γ tack t_1: bool$, $Γ tack t_2: bool$, $"op" ϵ space [->, ∨, ∧]$),
    $Γ tack t_1 "op" t_2: bool$,
  ),
  derive(
    "T-Add-Num",
    ($Γ tack t_1: num$, $Γ tack t_2: num$),
    $Γ tack t_1 + t_2: num$,
  ),
  derive(
    "T-Add-Str",
    ($Γ tack t_1: str$, $Γ tack t_2: str union.sq path$),
    $Γ tack t_1 + t_2: str$,
  ),
  derive(
    "T-Add-Path",
    ($Γ tack t_1: path$, $Γ tack t_2: path union.sq str$),
    $Γ tack t_1 + t_2: path$,
  ),
  derive(
    "T-Compare",
    (
      $Γ tack t_1: τ_1$,
      $Γ tack t_2: τ_2$,
      $τ_1 eq.triple τ_2$,
      $"op" in [<, <=, >=, >, ==, !=]$,
    ),
    $Γ tack t_1 "op" t_2: bool$,
  ),
  derive("T-Negate", ($Γ tack e: bool$,), $Γ tack !e: bool$),
  derive("T-Check", ($Γ tack e: {l: τ}$,), $Γ tack e ? l: bool$),
  derive(
    "T-Or",
    ($Γ tack t_1: {l: τ_1}$, $Γ tack t_2: τ_2$),
    $Γ tack t_1.l "or" t_2: τ_1 union.sq τ_2$,
  ),
))
#operator_typing_rules



== Subtyping Rules
#let subtyping = figure(
  caption: "Nix suptyping rules.",
  [
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
        "S-Rcd",
        (),
        ${arrow(t) : arrow(τ)} eq.triple inter.sq_i {l_i : t_i}$,
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
  [
    #subrules(caption: $Σ ⊢ τ ≪ τ => Ξ$, flexwrap(
      main-spacing: 20pt,
      cross-spacing: 10pt,
      derive("C-Hyp", ($(τ_1 ≪ τ_2) ∈ Σ$,), $Σ ⊢ τ_1 ≪ τ_2 => ε$),
      derive(
        "C-Assum",
        ($(τ_1 ≪τ_2) ∉ Σ$, $Σ ·⊳(τ_1 ≤ τ_2) ⊢ "dnf"^0_Σ (τ_1 ∧ ¬τ_2) => Ξ$),
        $Σ ⊢ τ_1 ≪ τ_2 => Ξ$,
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
        $Σ ⊢ I[{overline(x\: D_x)^(x ∈ S)}]∧¬{y: D} => Ξ$,
      ),
      derive(
        "C-Rcd2",
        ($y ∉ S$,),
        $Σ ⊢ I[{overline(x\: D_x)^(x ∈ S)}]∧¬{y: D} => #b[err]$,
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


== Dunder
$
  #type_name("R-functor")&& {"__functor" = "self": x : t } &arrow.twohead x: t \
  #type_name("R-overrides")&& {"__overrides" = record; oj(l_j = t_j) } &arrow.twohead todo({}) \
$
