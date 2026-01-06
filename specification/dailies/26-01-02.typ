#import "../functions.typ": *

We decided to diverge from the formulation given in \@verified by not adding inherit as syntactic sugar but as a inference rule with a premise that ensures that no new recursion was introduced during type inference. This is done by checking the context whether this variable exists.

#derive(
  "T-Inherit",
  (${ p.l = l }$, $p.l ∈ Γ$),
  ${ #b[inherit] (p) l }$,
)

#derive(
  "T-Inherit-2",
  (${ l = l; }$, $l ∈ Γ$),
  ${ #b[inherit] l; }$,
)


Regarding
