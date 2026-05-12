#import "functions.typ": *
#set page(height: auto)
#show: template


== Syntax
#let literals = subbox(caption: "Literals")[
  #set raw(lang: none)
  #show raw: set text(fill: red)

  #let strChar = `[^"$\\]|\$(?!\{)|\\.`
  #let iStrChr = `[^$']|\$\$|\$(?!\{)|''[$']|''\\.|'(?!')`

  #let interpol = $\${ t }$
  #let string = `"(c* i)* c*"`
  #let identstring = `''(c* i)* c*''`
  #let boolean = $"true "| "false"$
  #let filepath = `(./|~/|/)([a-zA-Z.]+/?)+`
  #let number = `([0-9]*.)?[0-9]+`
  #let label = `[A-Za-z_][A-Za-z0-9_'-]*`
  #let searchpath = `<[A-Za-z_]*>`
  #let uri = `[a-zA-Z][a-zA-Z0-9+.-]*://[^[ ]]+`


  $
      #type_name("Interpolation") i & ::= interpol \
             #type_name("String") s & ∈ string \
                                    & "where" c ∈ strChar \
         #type_name("Ident String") & | identstring \
                                    & "where" c ∈ omitted \
            #type_name("Boolean") b & ::= boolean \
    #type_name("File-Path") rho.alt & ∈ filepath \
             #type_name("Number") n & ∈ number \
              #type_name("Label") ℓ & ∈ label \
      #type_name("Search Path") Rho & ∈ searchpath \
                #type_name("Uri") u & ∈ uri \
  $
]

#let general = subbox(caption: "Terms")[
  $
    t ::= x &| s | b | rho.alt | n | Rho | u | #b[null] \
    #type_name("Record") &| {oa} | #b[rec] {oa} \
    #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
    #type_name("Function") &| p "@ "h : t #v(2em) #type_name("where") h ::= ℓ | ε \
    #type_name("Let-Statement") &| #b[let] oa #b[in] t\
    #type_name("Conditional") &| #b[if] t #b[then] t #b[else] t \
    #type_name("With-Statement") &| #b[with] t; t \
    #type_name("Assert-Statement") &| #b[assert] t; t \
    #type_name("Import-Statement") &| #b[import] t; t\
  $
]

#let operators = subbox(caption: "Operators")[
  $
    #type_name("Algebraic") & ast.op.o := && | t + t | t - t | t * t | t \/ t \
    #type_name("Logic") & && | t -> t | ! t | t "&&" t \
    #type_name("Comparison") & && | t < t | t <= t | t == t \
    & && | t "!=" t | t > t | t >= t \
    #type_name("Pipe") & && | t #b[<|] t| t #b[|>] t \
    #type_name("Record") & && | t space ? ρ | t.ρ #b[or] t | t \/\/ t | t.l | t.i | t.s \
    #type_name("Array") & && | t ⧺ t \
  $
]

#let assignment = subbox(caption: "Assignment")[
  $
    #type_name("Label") l & := ℓ | s | \${t} \
    #type_name("Assignment") α & ::= l = t; ι \
    #type_name("Inherit") ι & ::= #b[inherit] overline(l); | #b[inherit] (ρ) space overline(l); \
    #type_name("Path") ρ & ::= l | ρ.l | ρ.i \
  $
]

#let rewrites = subbox(
  caption: "Rewrites",
  $
    // #rule_name("RR-Inherit")&& #b[inherit] overline(l); & arrow.twohead overline(x := nonrec x); \
    // #rule_name("RR-Inherit")&& #b[inherit] (ρ) space overline(l); & arrow.twohead overline(x := ρ.x); "TODO" \
    #rule_name("RR-Def-Inner")&& { l_1 . l_2 space … space .l_n = t; } &arrow.twohead {l_1 = { l_2 = {l_n = t;};};} \
    #rule_name("RR-Rec")&& #b[rec] {oa} &arrow.twohead { l = #b[rec] t | l = t; ∈ α } \
    #rule_name("RR-Non-Rec")&& {oa} &arrow.twohead { l = #b[nonrec] t | l = t; ∈ α } \
  $,
)

#let patterns = box([
  #text(weight: "bold", smallcaps("Patterns"))
  $
    d & ::= t | ε \
    e & ::= ℓ | ℓ space ¿ space d \
    p & ::= { overline(e) } | { overline(e), … } | x \
  $])

#let syntax = figure(
  caption: "The Nix language.",
  box(width: 120%, grid(
    columns: 2,
    align: left,
    inset: 8pt,
    // grid.cell(colspan: 2, flexbox(
    //   $#type_name("Variables") x ∈ cal(X)$,
    // )),
    general, literals,
    operators, assignment,
    patterns,

    subbox(caption: "Shorthands")[
      $
        p : t space @ space ε & eq.def p : t \
                   h" @ "p: t & eq.def p" @ "h: t \
            ℓ space ¿ space ε & eq.def ℓ \
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
      v ::= x &| s | b | rho.alt | n | Rho | n | #b[null] | [t_0 t_1 … t_n] | {overline(α)} | #b[rec] {overline(α)} | (p: t)
    $],
    subbox(
      caption: "Evaluation Context",
      $
        E[□] & := □ space t | (□).l | v.□ | #b[if] □ #b[ then ] t #b[ else ] t | #b[with ] □; t | #b[assert] □; t \
        & #b[import] □; t | \${□} | □ ast.op.o t | v ast.op.o t \
      $,
    ),
    subbox(
      caption: "Reduction rules",
      [
        $
          #rule_name("Kind") k := #b[with] | #b[abs] #h(5cm) #rule_name("Recursiveness") ω := #b[rec] | #b[nonrec]
        $
        $
          #rule_name("R-Final")&& x_("Some" (k space t)) &arrow.long t \
          #rule_name("R-Attr-Rec")&& {overline(a)} &arrow.long {"unfold" overline(a)} &&&"if" ∃x,d. space x := rec d ∈ overline(a) \
          #rule_name("R-Abs")&& (x: t_1) t_2 &arrow.long t_1[x := abs t_2] \
          #rule_name("R-Match")&& (m: t) {overline(#b[nonrec] d)} &arrow.long t["indirects" oα] &&&"if" m ~ overline(d) arrow.squiggly oα \
          #rule_name("R-With")&& #b[with] {oa}; t &arrow.long t[{ l := with t | l = t ∈ oa }] \
          #rule_name("R-Let")&& #b[let] oi(l_i = t_i\;) #b[in] t &arrow.long t[{ l_i := abs t_i }]\
          #rule_name("R-Let-Rec")&& #b[let] {oi(l_i = t_i\;) "body" = t} &arrow.long t[{ l_i := abs t_i }] \
          #rule_name("R-Cond-True")&& #b[if] "true" #b[ then ] t_1 #b[ else ]t_2 & arrow.long t_1 \
          #rule_name("R-Cond-False")&& #b[if] "false" #b[then ] t_1 #b[ else ]t_2 & arrow.long t_2 \
          #rule_name("R-Lookup")&& {oa}.l & arrow.long t &&&"if" ω space l = t ∈ oa\
          #rule_name("R-Lookup-str")&& {oa}.s & arrow.long t &&&"if" ω space s = t ∈ oa\
          #rule_name("R-Dyn")&& \${s} & arrow.long s \
          #rule_name("R-Lookup-Default-Pos")&& {oa}.l #b[or] t & arrow.long
          t' &&&"if" ω space l = t' ∈ oa \
          #rule_name("R-Lookup-Default-Neg")&& {oa}.l #b[or] t & arrow.long
          t &&&"if" ω space l ∉ oa \
          #rule_name("R-Has-Pos")&& {overline(α)}" ? "l & arrow.long "true" &&&"if" ω space l ∈ oa \
          #rule_name("R-Has-Neg")&& {overline(α)}" ? "l & arrow.long "false" &&&"if" ω space l ∉ oa \
          #rule_name("R-Has-Path-Pos")&& {overline(α)}" ? "l.ρ & arrow.long "true" \&\& space (t " ? " ρ) &&&"if" ω space l = t ∈ oa \
          #rule_name("R-Has-Path-Neg")&& {overline(α)}" ? "l.ρ & arrow.long "false" \&\& space (t " ? " ρ) &&&"if" ω space l = t ∉ oa\
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
                        x_("Some" ("abs" d)) & "if" x = "with" t ∈ subs "and" sigma^? = "Some"(abs d),
                        x_("Some" (k space t)) & "if" x = k space t ∈ subs,
                        x_(σ^?) & otherwise,
                      ) \
     (λ x. t)[subs] & := λ x. t[subs] \
  (λ {p?}. t)[subs] & := λ {p[subs]}: t[subs] \
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
#let types = box(
  width: 100%,
  [
    #align(center, flexbox(
      $#type_name("Basetypes") b ∈ cal(B)$,
      $#type_name("Type Variables") α ∈ cal(A)$,
      $#type_name("Labels") l ∈ cal(L)$,
    ))
    $
      #type_name("Type")&& tau & ::= b | α | τ -> τ | ⦃ overline(p) ⦄^+ -> τ | ⦃ overline(p) ⦄^- -> τ \
      #type_name("Datatypes")&& &| {overline(l\: τ)} | [τ] | [overline(τ)] \
      #type_name("Connectives")&& & | ⊥ | top | τ ∨ τ | τ ∧ τ | ¬τ \
      #type_name("Pattern Field")&& p & := τ | τ^τ \
      #type_name("Polymorphic type")&& σ & := ∀Xi. τ \
      // #type_name("Mode")&& diamond.small & := + | -\
      #type_name("Typing Context")&& Γ & ::= ε | Γ · (x : τ) \
    $
  ],
)

#types


== Typing Rules

#let basic_typing_rules = figure(
  caption: "Basic Nix typing rules.",
  box(width: 100%, [
    #flexbox(
      derive("T-Const", (), $Γ ⊢ c: b_c$),
      derive("T-Var", ($Γ(x) = τ$,), $Γ ⊢ x: τ$),
      derive(
        "T-Abs1",
        ($Γ · (x: τ_1) ⊢ t: τ_2$,),
        $Γ ⊢ (x: t): τ_1 → τ_2$,
      ),
      derive(
        "T-App1",
        ($Γ ⊢ t_1: τ_1 → τ_2$, $Γ ⊢ t_2: τ_1$),
        $⊢ t_1 t_2: τ_2$,
      ),
      derive(
        "T-Sub",
        ($Γ ⊢ t: τ_1$, $Γ ⊢ τ_1 <= τ_2$),
        $Γ ⊢ t: τ_2$,
      ),
      derive(
        "T-If",
        ($Γ ⊢ t_1: "bool"$, $Γ ⊢ t_2: τ$, $Γ ⊢ t_3: τ$),
        $ #b[if] t_1 #b[then] t_2 #b[else] t_3: τ $,
      ),
      derive(
        "T-Assert",
        ($Γ ⊢ t_1: "bool"$, $Γ ⊢ t_2: τ_2$),
        $Γ ⊢ #b[assert] t_1; t_2: τ_2$,
      ),
      derive(
        "T-Lst-Hom",
        ($Γ ⊢ t_0: τ$, "...", $Γ ⊢ t_n: τ$),
        $Γ ⊢ [ " " t_0 " " t_1 " " ... " " t_n " "]: [τ]$,
      ),
      derive(
        "T-Lst-Agg",
        (
          $Γ ⊢ t_0: τ_0$,
          "...",
          $Γ ⊢ t_n: τ_n$,
        ),
        $Γ ⊢ [space t_0 space t_1 space ... " " t_n] : [ τ_0 space τ_1 space ... space τ_n]$,
      ),
      derive(
        "T-List-Concat-Hom",
        ($Γ ⊢ a: "[τ]"$, $Γ ⊢ b: "[τ]"$),
        $Γ ⊢ a "⧺" b: "[τ]"$,
      ),
      derive(
        "T-List-Concat-Multi",
        ($Γ ⊢ a: [overline(τ_1)]$, $Γ ⊢ b: [overline(τ_2)]$),
        $Γ ⊢ a "⧺" b: [overline(τ_1) space overline(τ_2)]$,
      ),
    )
  ]),
)
#basic_typing_rules


#let record_typing_rules = flexbox(
  derive(
    "T-Rcd",
    ($Γ ⊢ t_1: ⦅l⦆$, $t_2 : τ$),
    $Γ ⊢ {t_1 = t_2}: {l: τ}$,
  ),
  derive("T-Proj", ($Γ ⊢ t_1: {l: τ | ρ}$, $Γ ⊢ t_2: ⦅l⦆$), $Γ ⊢ t_1.t_2: τ$),
  derive(
    "T-Or-Pos",
    ($Γ ⊢ t_1: {l: τ | ρ}$, $Γ ⊢ t_2: ⦅l⦆$),
    $Γ ⊢ (t_1).t_2 #b[or] t_3: τ$,
  ),
  derive(
    "T-Or-Neg",
    ($Γ ⊢ t_1: {ρ}$, $l ∉ ρ$, $Γ ⊢ t_2: ⦅l⦆$, $Γ ⊢ t_3: τ$),
    $Γ ⊢ (t_1).t_2 #b[or] t_3: τ$,
  ),
  derive(
    "T-Rec-Concat",
    ($Γ ⊢ t_1: { ρ }$, $Γ ⊢ t_2: { ρ'}$),
    $Γ ⊢ t_1 "//" t_2: {ρ | ρ'}$,
  ),
  derive(
    "T-Check",
    ($Γ ⊢ e: {ρ}$, $t : ⦅l⦆$),
    $Γ ⊢ e #b[?] t: "bool"$,
  ),
)
#figure(caption: "Record typing rules", record_typing_rules)

#let needed(x) = $floor.l #x floor.r$
#let ceiling(x) = $ceil.l #x ceil.r$

#let function_typing_rules = stack(
  spacing: 15pt,
  flexbox(
    derive(
      "T-Abs2",
      $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
      $Γ ⊢ ({overline(e)}: t) : ⦃α⦄^- → τ_2$,
    ),
    derive(
      "T-Abs3",
      $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
      $Γ ⊢ ({overline(e),...}: t) : ⦃α⦄^+ → τ_2$,
    ),
    derive(
      "T-App2",
      (
        $Γ ⊢ t_1: ⦃overline(α)⦄^- → τ_2$,
        $Γ ⊢ t_2: τ_1$,
        $τ_1 ≤ needed(overline(α))$,
        $ceiling(overline(α)) ≤ τ_1$,
      ),
      $Γ ⊢ (x: t_1) t_2: τ_2$,
    ),
    derive(
      "T-App3",
      (
        $Γ ⊢ t_1: ⦃overline(α)⦄^+ → τ_2$,
        $Γ ⊢ t_2: τ_1$,
        $τ_1 ≤ needed(overline(α))$,
      ),
      $Γ ⊢ (x: t_1) t_2: τ_2$,
    ),
  ),
  flexbox(
    $ceiling(oa) = { τ | τ ∈ oa ∨ τ^τ' ∈ oa}$,
    $floor(oa) = { τ | τ ∈ oa }$,
  ),
)

#let other_constructs = figure(
  caption: "Extra constructs typing rules.",
  flexbox(
    derive(
      "T-With",
      ($Γ ⊢ t₁ : {ρ}$, $Γ,Ξ · {ρ} ⊢ t₂ : τ$),
      $Γ ⊢ with t₁; t₂ : τ$,
    ),
    derive("R-Inherit", $x ∈ Γ$, $Γ ⊢ { inherit x; } -> { x = Γ(x);}$),
    derive(
      "R-Inherit-path",
      $x ∈ Γ$,
      $Γ ⊢ { inherit (ρ) space x; } -> { x = "lookup"(ρ, x)}$,
    ),
    derive(
      "T-Import",
      ($𝜚 arrow.squiggly t$, $Γ ⊢ t: τ$),
      $Γ ⊢ #b[import] 𝜚: τ$,
    ),
  ),
)

#let occurrence_typing = figure(caption: "Occurrence Typing.", flexbox(
  derive(
    "T-Cond-pos",
    ($Γ ⊢ t: "true" => Ξ$, $Γ, Ξ ⊢ t_1 : τ$),
    $Γ ⊢ #b[if] t_1 #b[then] t_2 #b[else] t_3: τ$,
  ),
  derive(
    "T-Cond-neg",
    ($Γ ⊢ t: "false" => Ξ$, $Γ, ¬Ξ ⊢ t_1 : τ$),
    $Γ ⊢ #b[if] t_1 #b[then] t_2 #b[else] t_3: τ$,
  ),
  derive("T-Ground", $$, $Γ, Ξ ⊢ "is"_b (t) => Ξ · (t: b)$),
  derive("T-Has", $Γ ⊢ t: {l : τ}$, $Γ, Ξ ⊢ t #b[?] l => Ξ · (t: {l: τ})$),
  derive(
    "T-Or",
    ($Γ, Ξ ⊢ t_1 => Ξ'$, $Γ, Ξ ⊢ t_2 => Ξ''$),
    $Γ, Ξ ⊢ t_1 space || space t_2 => Ξ$,
  ),
  derive(
    "T-And",
    ($Γ, Ξ ⊢ t_1 => Ξ'$, $Γ, Ξ ⊢ t_2 => Ξ''$),
    $Γ, Ξ ⊢ t_1 space \&\& space t_2 => Ξ · Ξ' · Ξ''$,
  ),
  derive("T-Neg", $Γ, Ξ ⊢ t_2 => Ξ'$, $Γ, Ξ ⊢ !t_2 => Ξ · ¬Ξ'$),
))

#let operator_typing_rules = figure(
  caption: "Operator typing rules. Not including Record operator typing rules.",
  [
    $
      "add": & (str → (str ∨ path) → str) \
             & ∧ (path → (str ∨ path) → path) \
             & ∧ (int → int → int) \
             & ∧ (num → num → float)
    $
    #flexbox(
      derive(
        "T-Op-Logic",
        ($Γ tack t_1: bool$, $Γ tack t_2: bool$, $"op" ϵ space [->, ∨, ∧]$),
        $Γ tack t_1 "op" t_2: bool$,
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
      derive("T-Negate", $Γ tack e: bool$, $Γ tack !e: bool$),
    )
  ],
)
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
    - Occurrence typing in typed scheme paper (2008)?
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
