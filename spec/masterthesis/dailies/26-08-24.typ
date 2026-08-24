

== Todo
- Bisschen Prep für Treffen mit Thiemann


== Finding: the ≐ᵣ occurs-check is *conservative* (incomplete)

While extending the unification meta-theory (`algorithmic.lean`) I found that
the `occurs` verdict does *not* mean "no unifier". It is a sound *give-up*, not
a completeness claim — and on one class of inputs the algorithm is genuinely
incomplete.

*The counterexample* (kernel-checked, `occurs_allVar_unifiable`):
$ "unifyRow" alpha (beta | alpha | gamma) = "occurs" $
yet a unifier exists — take $beta, gamma |-> epsilon$, giving
$alpha approx epsilon | alpha | epsilon approx alpha$. So the check
`(sVarSeq s₂).contains α` fires on an *all-variable interior occurrence* that is
in fact solvable by collapsing the surrounding row-variables to $epsilon$.

*Why it happens.* The occurs-check is the classic "α = t[α] has no finite
solution" test. But rows live in a monoid *with a unit* $epsilon$ and
cancellation, so $alpha approx beta | alpha | gamma$ *does* have a solution
whenever the surrounding material can vanish ($beta, gamma |-> epsilon$). A
syntactic `contains α` cannot tell "vanishing var" from "rigid content" apart,
so it rejects too eagerly. Worse, that missed solution is even *principal*
(by an atom-count argument only $epsilon$ works on both sides), so we are
dropping the unique mgu.

*What is actually true.* occurs is a real non-unifier exactly when the
recursive variable is pinned by a *field*. Proven (`occurs_field_no_unifier`,
axiom-clean): if $alpha in "vars"(s_2)$ and some label $l$ has
$0 < "count"_l (s_2)$, then no $theta$ unifies $alpha$ with $"ofSpine" s_2$.
The field-count argument mirrors `projClash_no_unifier`: $theta alpha$ would
have to carry $l$'s field-count both on its own (as the lhs) and *again* inside
the rhs, an impossible strict growth that $approx$ forbids. Supporting lemma
`fieldCount_var_lower`: a recursive variable splices its whole $l$-count into
$theta rho$ on top of $rho$'s own explicit $l$-fields.

*Decision needed (for Thiemann?).* Two honest options:
+ *Present occurs as conservative* — document it as a sound give-up, and state
  the trichotomy's occurs leg only as `occurs_field_no_unifier`. Cheapest.
+ *Refine the check* to `contains α ∧ (∃ field in s₂)` — makes occurs precise on
  this class. The all-var case would then fall through to another move (probably
  `stuck`, since no single forced move applies), which needs re-checking against
  the regression suite.

My lean: option 1 for the thesis text (it is the truth about the current
algorithm), with a remark that a refined check recovers completeness on the
all-var class. Either way the two theorems above already pin down the exact
boundary.

Also landed today: *fuel-sufficiency* (`unifySpineF_fuel_irrel` / `_stable`) —
each move eats exactly two atoms, so the starting fuel $|s_1| + |s_2|$ never
runs out and a `stuck` result is genuine rather than an out-of-fuel artifact.
