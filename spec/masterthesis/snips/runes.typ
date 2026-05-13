#import "../functions.typ": *
#set page(height: auto)

#table(
  columns: (auto, auto),
  table.header([*Term*], [*Meaning*]),
  $ ~ $, [Consistency],
  $ lt.double $, [Constraining],
  $ ≤ $, [Subtyping],
  $ eq.triple $, [Equality],
  $ tilde.eq $, [Equality of types due to sets? castagna ],
  $ ⊢ $, [Proves],
  $ tack.double $, [ Variation of proves ],
  $ models $, [ Models ],
  $ {} $, [ Record ],
  $ ⟨⟩ $, [ Variants ],
  $ ⟦⟧ $, [ Denotation ],
  $ l_i $, $ l_1 ... l_n $,
  $ x, y, z $, [ Object-lang: Unkown but fixed variable ],
  $ t, e $, [ Meta-lang: Expression or terms ],
  $ α, β, γ $, [ Typevariable ],
  $t ⊲ s$, [Type cast],
  $t arrow.double^p s$, [blame],
  $t[x arrow.bar v]$, [x substituted *by* v],
  $t[x \/ v]$, [x for v],
)

= General Typing Rules
#rect(width: 100%, inset: 10pt, flexbox(
  derive("T-sub", ($t: τ_1$, $τ_2 ≤ τ_1$), $t: τ_2$),
  derive("T-Abs", $Γ, x: τ_1 ⊢ t: τ_2$, $Γ ⊢ λ x. t: (τ_1 → τ_2)$),
  derive("T-App", ($Γ ⊢ x: τ_1$, $Γ ⊢ t: (τ_1 → τ_2)$), $Γ ⊢ (λ x. t) x : τ_2$),
  derive(
    "T-Rec",
    ($Γ ⊢ t_1 : τ_1$, $…$, $Γ ⊢ t_n : τ_n$),
    $record: recordType$,
  ),
  derive("T-Sel", ($Γ ⊢ t: {l: τ}$,), $Γ ⊢ t.l: τ$),
  derive("T-Asc", ($Ξ,Γ ⊢ t : τ$,), $Ξ,Γ ⊢ (t: τ) : τ$),
))


= Records
#let cast_fn = $λ^{ and_(i∈I) τ_i → τ_i}$
#rect(width: 100%, inset: 10pt, flexbox(
  derive("Sel", ($Γ ⊢ e: τ ≤ { l = 𝟙}$,), $Γ ⊢ e.l : τ.l$),
  derive("Del", ($Γ ⊢ e: τ ≤ {}$,), $Γ ⊢ e without l : τ without l$),
  derive(
    "Conc",
    ($Γ ⊢ e_1: τ_1 ≤ {}$, $Γ ⊢ e_2: τ_2 ≤ {}$),
    $e_1 + e_2 : τ_1 + τ_2$,
  ),
  [
    *Record Type merging*

    $(r_1 +_t r_2)(l) = cases(r_2(l) &r_2(l) ∧ t ≤ 𝟘, (r_2(l) without t) ∨ r_1(l) &otherwise)$,
  ],
))


== Castagna
$λ^((τ_1 → τ_2)_(1..n ∈ S))x. t$
$t_1 <t_2 <- t_3> | "case" (t_1 ? τ) t_2 : t_3$


== Extensible Recursive Functions @extensible_rec_funcs
#rect(width: 100%, inset: 10pt)[
  #grid(
    columns: (1fr, 1fr),
    rows: 2,
    gutter: 5pt,
    $
      #type_name("Term variables") x ∈ cal(X) \
      #type_name("Type variables") x ∈ cal(A) \
      #type_name("Labels") l ∈ cal(L)
    $,
    $
      #type_name("Type environments") Γ ::= ε | Γ,x: τ \
      #type_name("Predicate envorionments") Φ ::= ε | Φ,v: π \
      #type_name("Kind environments") Δ ::= ε | Δ, α: κ \
    $,
    grid.cell(colspan: 2)[
      $
        #type_name("Kinds") && κ & ::= ∗ | L | R^κ | κ → κ \
        #type_name("Predicates") && π, ψ & ::= ρ lt.approx ρ | ρ ⊙ ρ ~ ρ \
        #type_name("Types") && cal(T) in.rev τ & ::= α | T | π ⇒ τ | ∀α: κ.τ | τ τ | {xi_i ⊳ τ_i}_(i ∈ 0..m) | l | \#τ | ϕ^∗ | ρ without ρ \
        #type_name("Terms") && cal(E) in.rev M, N &::= x | k | λ x : - .M | M N | Lambda α: κ.M | M [τ] | Lambda υ : π.M | M[Q] \
        && &| \#τ | M ⊳^Ξ N | M \/^Ξ N #h(2cm) Ξ ∈ {Pi, Sigma}\
      $
    ],
  )
]

== Infix Extensible Record Types @extensible_tabular
#rect(width: 100%, inset: 10pt, grid(
  columns: 1fr,
  rows: 2,
  flexbox(
    $#type_name("Type Variables") α ∈ cal(V)_t$,
    $#type_name("Labels") l_c ∈ cal(L)$,
  ),
  $
         #type_name("Kinds") && κ & ::= ∗ | κ_1 → κ_2 | "Row" | "Label" \
    #type_name("Poly types") && σ & ::= ∀a: κ. σ | τ \
          #type_name("Type") && τ & ::= a | "Int" | → | τ_1 τ_2 | {ρ} | ⟨ρ⟩ | ⦅l⦆ \
           #type_name("Row") && τ & ::= a | "Empty" | l: τ | (ρ_1 | ρ_2) \
         #type_name("Label") && l & ::= a | l_c \
       #type_name("Context") && Γ & ::= • | Γ, a: κ | Γ, x: τ \
  $,
  subbox(caption: "kinding", flexbox(
    derive("", $a: κ ∈ Γ$, $Γ ⊢ a : κ$),
    derive("", $$, $Γ ⊢ "Int": ∗$),
    derive("", $$, $Γ ⊢ →: ∗ → ∗ → ∗$),
    derive("", ($τ_1: κ_1 → κ_2$, $Γ ⊢ τ_2: κ_1$), $Γ ⊢ τ_1τ_2 : κ_2$),
    derive("", $ρ : "Row"$, $Γ ⊢ {ρ} : ∗$),
    derive("", $ρ: "Row"$, $Γ ⊢ ⟨ρ⟩: ∗$),
    derive("", $l: "Label"$, $Γ ⊢ ⦅l⦆: ∗$),
    derive("", $$, $Γ ⊢ l_c : "Label"$),
    derive("", $$, $Γ ⊢ "Empty": "Row"$),
    derive("", ($Γ ⊢ ρ_1: "Row"$, $Γ ⊢ (ρ_1 | ρ_2): "Row"$), $Γ ⊢ ⦅l⦆: ∗$),
    derive("", ($l: "Label"$, $Γ ⊢ τ: ∗$), $Γ ⊢ (l: τ): "Row"$),
  )),
))

== Abstracting Extensible Data Types
Operations:
- Proj: λr prj r: ∀z₁z₂. z₂ ⧀ z₁ => Πz₁ -> Πz₂
  - Record elimination
- Concat: M₁ ∗ M₂: ∀z₁z₂z₃. z₁ ⊙ z₂ ~ z₃ => Πz₁ -> Πz₂ -> Πz₃
  - Record ?
- Inj: λxy. inj (And ⊳ (x, y)): ∀t₁t₂ z.(And ⊳ t₁ × t₂) ⧀ z => t₁ -> t₂ -> Σz
  - Variant introduction
- Case: ∇
  - Variant elimination

Types: $A, B ::= a | A → B | ∀α. A | ⊕ {A_i} | ⊗ {A_i}$
Terms: $E, F ::= x | λ x: A. E | F E | Λ a. E | E[A] | (E₁, …, E\ₙ) | π\ᵢE | ι\ᵢE | "case" E {F₁, …, F\ₙ}$


= Recursion
== Amber Rules @quicksub

#flexbox(
  derive(
    "Amber-Var",
    $α ≤ β ∈ Δ$,
    $Δ ⊢ α ≤ β$,
  ),

  derive(
    "Amber-Rec",
    $Δ, α ≤ β ⊢ A ≤ B$,
    $Δ ⊢ μ α. A ≤ μ β. B$,
  ),

  derive(
    "Amber-Self",
    $$,
    $Δ ⊢ μ α. A ≤ μ α. A$,
  ),
)

== QuickSub
#flexbox(
  derive("QS-Nat", $$, $Ψ ⊢ "nat" attach(≈, br: ∅) "nat"$),
  derive("QS-TopEq", $$, $Ψ ⊢ T attach(≈, br: ∅) T$),
  derive("QS-TopLT", $A ≠ T$, $Ψ ⊢ A < t$),
  derive("QS-VarPos", $α^⊕ ∈ Ψ$, $Ψ ⊢ A attach(≈, br: ∅) α$),
  derive("QS-VarNeg", $α^(overline(⊕)) ∈ Ψ$, $Ψ ⊢ α attach(≈, br: α) α$),
  derive("QS-RecT", $Ψ, α^⊕ ⊢ A_1 < A_2 ⊢$, $Ψ ⊢ μ α. A_1 < μ α A_2$),
  derive(
    "QS-RecEq",
    ($Ψ, α^⊕ ⊢ A_1 attach(≈, br: S) A_2 ⊢$, $α ∉ S$),
    $Ψ ⊢ μ α. A_1 attach(≈, br: S) μ α A_2$,
  ),
  derive(
    "QS-RecEqIn",
    ($Ψ, α^⊕ ⊢ A_1 attach(≈, br: S)A_2$, $a ∈ S$),
    $Ψ ⊢ μ α. A_1 ≈_(((S union"FV"(A_1)) without {α})) μ. α A_2$,
  ),
  derive(
    "QS-Arrow",
    (
      $Ψ, α^overline(⊕) ⊢ A_2 attach(lt.approx, br: 1) A_1$,
      $Ψ ⊢ B_1 attach(lt.approx, br: 2) B_2$,
    ),
    $Ψ ⊢ A_1 → A_2 (attach(lt.approx, br: 1) • attach(lt.approx, br: 2)) B_1 → B_2$,
  ),
)




= Occurrence Typing

#rect(inset: 10pt)[
  *Domain-merging* @revisiting_occurrence

  $
    t space square.filled.tiny space s = "dom"(t) ∧ or.big_(i ∈ I)(and.big_({P subset.eq P_i | s ≤ or.big_(p∈P) ¬t_p }) (or.big_(p ∈ P) ¬s_p))
  $

  *Occurence Typing Case-Rule* @revisiting_occurrence
  #derive(
    "Case",
    (
      $Γ ⊢ e: t_0$,
      $Γ ⊢^"Env"_(e, t) Γ_1$,
      $Γ_1 ⊢ e_1: t'$,
      $Γ ⊢^"Env"_(e,¬t) Γ_2$,
      $Γ_2 ⊢ e_2: t'$,
    ),
    $Γ ⊢ (e ∈ t)? e_1 : e_2: t'$,
  )

  *Path-resolution* @revisiting_occurrence
  #flexbox(
    "",
    $e arrow.b ε = e$,
    $e_0e_1 arrow.b i.pi.alt = e_i$,
    $(e_1, e_2) arrow.b l.pi.alt = e_1 arrow.b pi.alt$,
    $(e_1, e_2) arrow.b r.pi.alt = e_2 arrow.b pi.alt$,
    $pi_1 e arrow.b f.pi.alt = e arrow.b pi.alt$,
    $pi_2 e arrow.b s.pi.alt = e arrow.b pi.alt$,
  )

  *Record Merging* @revisiting_occurrence
  $
    t_1 + t_2 = min(
      {
        u | ∀l ∈ "Labels"
        cases(
          u.l ≥ t_2.l & "if" t_2.l ≤ ¬"Udef",
          u.l ≥ t_1.l ∨ (t_2.l without "Udef") & otherwise
        )
      }
    )
  $
]
== On type-cases, union elimination and Occurrence Typing @on_occurrence
#flexbox(
  derive(
    "T-case1",
    ($Γ ⊢ e : τ$, $Γ ⊢ e_1: τ_1$),
    $Γ ⊢ ((e ∈ τ)? e_1 : e_2): τ_1$,
  ),

  derive(
    "T-case2",
    ($Γ ⊢ e : ¬τ$, $Γ ⊢ e_2: τ_2$),
    $Γ ⊢ ((e ∈ τ)? e_1 : e_2): τ_2$,
  ),

  derive(
    "Union-Elim",
    ($Γ ⊢ e' : τ_1 ∨ τ_2$, $Γ, x : τ_1: τ$, $Γ, x : τ_2: τ$),
    $Γ ⊢ e{x \/ e'}: τ$,
  ),
)

== Flow Typing @pearce_flowtyping

#flexbox(
  derive(
    "T-app",
    ($Γ ⊢ t_1: τ_1$, $Γ ⊢ f: τ_2 → T_3$, $Γ ⊢ τ_1 ≤ τ_2$),
    $Γ ⊢ f t_1: τ_3$,
  ),

  derive(
    "T-dec",
    (
      $Γ [x arrow.bar τ_1] ⊢ t_2 : τ_2$,
      $Γ [f arrow.bar τ_1 → τ_2 ] ⊢ t_3 : τ_3$,
    ),
    $Γ ⊢ f (τ_1 x) = t_2 in t_3: τ_3$,
  ),

  derive(
    "T-if",
    ($Γ[x arrow.bar Γ(x) ∧ τ_1] ⊢ τ_3$, $Γ[x arrow.bar Γ(x) ∧ ¬τ_1] ⊢ τ_3$),
    $"if" (x "is" τ_1) t_2 "else" t_3: τ_2 ∨ τ_3$,
  ),
)

= Matching
Given any pattern p, we can define a type $bag.l p bag.r$ that characterizes exactly the set of values that match the pattern:

#let pat(x) = $bag.l #x bag.r$

#flexbox(
  $pat(p) = 𝟙$,
  $pat(t) = t$,
  $pat((x := c)) = 𝟙$,
  $pat({l = p}) = {l = pat(p)}$,
  $pat(p_1 ∧ p_2) = pat(p_1) ∧ pat(p_2)$,
  $pat(p_1 ∨ p_2) = pat(p_1) ∨ pat(p_2)$,
)

It can be shown that for every pattern p and well-typed value we have $v/p != "fail"$ iff $∅ ⊢ v : pat(v)$.

Given a type τ and a pattern p with $bag.l p bag.r ≤ τ$, the operator τ/p produces the _type environment_ assumed for the variables in p when a value of type τ is matched against p and the matching succeeds. It is defined as:

#flexbox(
  $τ\/τ' = ∅$,
  $τ\/x = x: τ$,
  $τ\/(x := c) = x : b_c$,
  $τ\/{ l = p} = τ.l \/ p$,
  $p_1 ∧ p_2 = (t \/ p_1) ∪ (t \/ p_2)$,
  $p_1 ∨ p_2 = ((t ∧ pat(p_1)) \/ p_1) ∪ (t ∧ (pat(p_2)) \/ p_2)$,
)

and satisfies the property that for every τ, p and v, if $∅ ⊢ v: τ$ and $v \/ p = σ$, then, for every variable x in p, the judgment $∅ ⊢ x σ : (τ\/p)(x)$ holds.


= Deferred Substitutions
#derive(
  "T-str",
  ($$,),
  $x_("Some" k space e) -> e$,
)

#let subs = $overline(sigma.alt)$

$
       x_(σ?)[subs] & := cases(
                        x_("Some" ("abs" d)) & "if" x = "with" e ∈ subs "and" sigma^? = "Some"(abs d),
                        x_("Some" (k space e)) & "if" x = k space e ∈ subs,
                        x_(σ^?) & otherwise,
                      ) \
     (λ x. e)[subs] & := λ x. e[subs] \
  (λ {p?}. e)[subs] & := λ {p[subs]}: e[subs] \
$





= Gradual Typing

#rect(inset: 10pt, width: 100%)[
  *Consistency*

  #let uk = $star.op$
  $A ~ A #h(1cm) A ~ uk #h(1cm) uk ~ A$

  *Typingrules*
  #flexbox(
    derive(
      "",
      ($A_1 ~ B_1$, $A_1 ~ B_1$),
      $A_1 → A_2 ~ B_1 → B_2$,
    ),
    derive("", ($A ~ B$,), $∀a. A ~ ∀α. B$),
    derive("ForallL", ($Γ, α ⊢ A <= B$,), $ A <= ∀α. B $),
    derive("ForallL", ($Γ ⊢ τ$, $Γ, α ⊢ A[α -> τ] <= B$), $ ∀α. A <= B $),
  )
]


= Qualified Types

$
  φ & ::= τ | π => φ \
  σ & ::= φ ϕ
$

= Misc
$
  "unfold"_1 oα := &{ x := #b[nonrec] t | x := #b[nonrec] t ∈ oα} attach(union, tr: <) \
  &{ x := #b[nonrec] t["indirects" oα] | x := #b[rec] t ∈ oα} \
  "indirects" oα := &{x := #b[abs] {oα}.x | x ∈ oα }
$




#bib
