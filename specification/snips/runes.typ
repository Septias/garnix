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


== Occurrence Typing
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

= On Type-Cases, Union Elimination, and Occurrence Typing

#derive(
  "T-case1",
  ($Γ ⊢ e : τ$, $Γ ⊢ e_1: τ_1$),
  $Γ ⊢ ((e ∈ τ)? e_1 : e_2): τ_1$,
)

#derive(
  "T-case2",
  ($Γ ⊢ e : ¬τ$, $Γ ⊢ e_2: τ_2$),
  $Γ ⊢ ((e ∈ τ)? e_1 : e_2): τ_2$,
)

#derive(
  "Union-Elim",
  ($Γ ⊢ e' : τ_1 ∨ τ_2$, $Γ, x : τ_1: τ$, $Γ, x : τ_2: τ$),
  $Γ ⊢ e{x \/ e'}: τ$,
)


= Flow Typing @pearce_flowtyping

- No recursion
#derive(
  "T-app",
  ($Γ ⊢ t_1: τ_1$, $Γ ⊢ f: τ_2 → T_3$, $Γ ⊢ τ_1 ≤ τ_2$),
  $Γ ⊢ f t_1: τ_3$,
)

#derive(
  "T-dec",
  ($Γ [x arrow.bar τ_1] ⊢ t_2 : τ_2$, $Γ [f arrow.bar τ_1 → τ_2 ] ⊢ t_3 : τ_3$),
  $Γ ⊢ f (τ_1 x) = t_2 in t_3: τ_3$,
)

#derive(
  "T-if",
  ($Γ[x arrow.bar Γ(x) ∧ τ_1] ⊢ τ_3$, $Γ[x arrow.bar Γ(x) ∧ ¬τ_1] ⊢ τ_3$),
  $"if" (x "is" τ_1) t_2 "else" t_3: τ_2 ∨ τ_3$,
)

= Typing
#flexbox(
  derive("T-sub", ($t: τ_1$, $τ_2 ≤ τ_1$), $t: τ_2$),
  derive("T-Abs", ($Γ, x: τ_1 ⊢ t: τ_2$,), $Γ ⊢ λ x. t: (τ_1 → τ_2)$),
  derive("T-App", ($Γ ⊢ x: τ_1$, $Γ ⊢ t: (τ_1 → τ_2)$), $Γ ⊢ (λ x. t) x : τ_2$),
  derive(
    "T-Rec",
    ($Γ ⊢ t_1 : τ_1$, $…$, $Γ ⊢ t_n : τ_n$),
    $record: recordType$,
  ),
  derive("T-Sel", ($Γ ⊢ t: {l: τ}$,), $Γ ⊢ t.l: τ$),
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


= Subtyping
#flexbox(
  derive(
    "S-depth",
    (
      $Γ ⊢ record: recordType$,
      $Γ ⊢ {oj(t_j \= τ_j\;)}: {oj(t_j\: τ_j)}$,
      $∀i τ_i < τ_j$,
    ),
    $$,
  ),
  derive("S-width", ($$,), $$),
)


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

= Records

#let cast_fn = $λ^{ and_(i∈I) τ_i → τ_i}$

#derive("Sel", ($Γ ⊢ e: τ ≤ { l = 𝟙}$,), $Γ ⊢ e.l : τ.l$)
#derive("Del", ($Γ ⊢ e: τ ≤ {}$,), $Γ ⊢ e without l : τ without l$)
#derive(
  "Conc",
  ($Γ ⊢ e_1: τ_1 ≤ {}$, $Γ ⊢ e_2: τ_2 ≤ {}$),
  $e_1 + e_2 : τ_1 + t_2$,
)

$(r_1 +_t r_2)(l) = cases(r_2(l) &r_2(l) ∧ t ≤ 𝟘, (r_2(l) without t) ∨ r_1(l) &otherwise)$


= Gradual typing

The gradual type: $star.op$

#let uk = $star.op$

#flexbox(
  "Consistency",
  $A ~ A$,
  $A ~ uk$,
  $uk ~ A$,
  derive(
    "",
    ($A_1 ~ B_1$, $A_1 ~ B_1$),
    $A_1 → A_2 ~ B_1 → B_2$,
  ),
  derive("", ($A ~ B$,), $∀a. A ~ ∀α. B$),
)

#derive("ForallL", ($Γ, α ⊢ A <= B$,), $ A <= ∀α. B $)
#derive("ForallL", ($Γ ⊢ τ$, $Γ, α ⊢ A[α -> τ] <= B$), $ ∀α. A <= B $)


== Misc
$
  "unfold"_1 oα := &{ x := #b[nonrec] t | x := #b[nonrec] t ∈ oα} attach(union, tr: <) \
  &{ x := #b[nonrec] t["indirects" oα] | x := #b[rec] t ∈ oα} \
  "indirects" oα := &{x := #b[abs] {oα}.x | x ∈ oα }
$

#derive("T-Asc", ($Ξ,Γ ⊢ t : τ$,), $Ξ,Γ ⊢ (t: τ) : τ$)



#bib
