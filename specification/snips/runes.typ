#import "../functions.typ": *
#set page(height: auto)

#table(
  columns: (auto, auto),
  table.header([*Symbol*], [*Meaning*]),
  $ ~ $, [Consistency],
  $ lt.double $, [Constraining],
  $ ≤ $, [Subtyping],
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
)

= Occurrence Typing
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
                                 u.l ≥ t_2.l & "if" t_2.l ≤ ¬"Udef" \
        u.l ≥ t_1.l ∨ (t_2.l without "Udef") & otherwise
      )
    }
  )
$

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
Given a type τ and a pattern p with $bag.l p bag.r ≤ τ$, the operator t/p produces the type environment assumed for the variables in p when a value of type t is matched against p and the matching succeeds.

#flexbox(
  $t/t' =$,
)


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



#bib
