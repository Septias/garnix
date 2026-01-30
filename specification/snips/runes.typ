#import "../functions.typ": *


Γ, Ξ ⊢
------------


$t bullet s = "dom"(t) ∧ or.big_(i ∈ I)(and.big_({P subset.eq P_i | s ≤ or.big_(p∈P) ¬t_p }) (or.big_(p ∈ P) ¬s_p)) $

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

#many_wrapping_derives(
  "",
  $e arrow.b ε = e$,
  $e_0e_1 arrow.b i.pi.alt = e_i$,
  $(e_1, e_2) arrow.b l.pi.alt = e_1 arrow.b pi.alt$
  $(e_1, e_2) arrow.b r.pi.alt = e_2 arrow.b pi.alt$
  $pi_1 e arrow.b f.pi.alt = e arrow.b pi.alt$
  $pi_2 e arrow.b s.pi.alt = e arrow.b pi.alt$
)

