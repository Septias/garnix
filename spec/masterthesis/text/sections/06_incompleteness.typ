#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= Incompleteness <incompleteness>

The algorithm is sound but not complete. This section collects the places where it gives up, in decreasing order of inevitability. The first family is a limit of the row theory, the second a price paid for forced moves, the third a limit of the inference algorithm on top.

== Irreducible Problems <incompleteness-irreducible>

Some row equations have no most general unifier, and no algorithm that returns a single unifier can solve them. Most general is meant relative to the variables of the problem, as in @thm-unif-mgu.

#proposition(
  name: [Irreducible problems],
  lean: "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set",
)[
  + (_Wand ambiguity._) For pairwise distinct $α₁, …, α_n$ with $n ≥ 2$, $(α₁ | … | α_n) scripts(≐)_r (l: 𝓫)$ has no most general unifier.
  + (_Two-sided._) $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has no most general unifier.
  + (_Shift._) $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers.
] <prop-irreducible>

In the Wand ambiguity, every variable may carry the field, and no choice subsumes the others. The two-sided problem is the same deadlock at both ends of the spine. The shift has a unifier for every number of l-fields in α, so returning several answers does not help either.

#example(name: [Unifiers of the irreducible problems])[
  - The Wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the unifiers $[α ≔ (l: 𝓫), β ≔ ε]$ and $[α ≔ ε, β ≔ (l: 𝓫)]$. Both are ground and they differ, so neither is an instance of the other. A most general unifier would have to be more general than both, but it cannot leave α and β open: $[α ≔ γ, β ≔ γ′]$ and every other substitution that maps α or β to a row containing a variable is no unifier, since a variable on the left would have to face a variable on the right. Nor can it send both to ε, since $(ε | ε) ≈ ε$ and ε lacks the field. So it places the single field in one of the two variables and is no more general than the unifier that places it in the other.
  - The two-sided problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has the unifiers $[α ≔ ε, β ≔ ε]$ and $[α ≔ (l: 𝓫), β ≔ (l: 𝓫)]$, which disagree on the number of l-fields in α. Unlike in the Wand ambiguity, $[α ≔ ε, β ≔ ε]$ is a unifier, but it is not most general: it is ground, its only instance is itself, and so it does not cover the second unifier. Conversely, every instance of a unifier that gives α at least one l-field, such as $[α ≔ (l: 𝓫 | γ), β ≔ (γ | l: 𝓫)]$, gives α at least one, so none covers $[α ≔ ε, β ≔ ε]$.
  - The shift $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ is unified by $[α ≔ (l: 𝓫)^n]$ for every $n ≥ 0$, where $(l: 𝓫)^n$ is the row of n copies of the field. Each unifier in a finite set fixes the number of l-fields in α, so no finite set covers all of them.
] <ex-irreducible>

All three are reported as #u_stuck: no move applies, and any move would prefer one unifier to another. Negative information narrows the choice. If a lacks-predicate states that β has no l-field, the Wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the single unifier $[α ≔ (l: 𝓫), β ≔ ε]$. For the two-sided problem and the shift, a lacks-predicate on α excludes the unifiers that give α an l-field, but it leaves those that give α fields of other labels, such as $α ≔ (m: 𝓫)$ next to $α ≔ ε$. These remain incomparable as long as no field crosses a row-variable, so lacks-predicates resolve the two problems only together with a row equivalence in which a variable commutes with the fields of the labels it lacks (@sec-extensions).

// PROF: Inference has no rule that places star at a stuck problem; specify it.
What is shown is a limit of principal solutions over scoped rows, not of typing: a program whose inference is stuck may still be typed by placing ★ where the problem sits.

== The Price of Forced Moves <incompleteness-forced>

Two stuck problems do have a most general unifier.

- The _mirrored crossfield_ problem $(α | l: 𝓫) scripts(≐)_r (β | m: 𝓫)$ has the most general unifier $[β ≔ (β′ | l: 𝓫), α ≔ (β′ | m: 𝓫)]$, the trailing-end image of @ex-unif-host. U-host is stated for the leading end only, so the problem is stuck. [The same move at the trailing end would solve it](explain why this is not done).
- A stuck equation between the types of two paired fields is reported before the remaining row equation solves the variable it depends on, which masks an existing solution (@prop-masks). Deferring the stuck equation and letting the residual run first would solve this instance, but it abandons the discipline of @unification that every emitted equation is discharged, fatal or stuck on the spot. Whether the resulting algorithm is confluent, that is, whether its verdict is independent of the order in which deferred equations are retried, is open.

#proposition(name: [Stuck masks an mgu], lean: "stuck_masks_mgu")[
  The problem $(k: {β | α} | β) scripts(≐)_r (k: {l: 𝓫} | l: 𝓫)$ is stuck, but $[β ≔ (l: 𝓫), α ≔ ε]$ is a most general unifier.
] <prop-masks>

Stuck is therefore conservative: it does not imply that no most general unifier exists, not even in a configuration where no move applies.

== Limits of Inference <incompleteness-inference>

The remaining limits lie in the inference algorithm rather than in unification.

=== Partially Generalized Lets

A-let generalizes the greatest admissible $macron(α)$. A variable that violates one of the conditions stays monomorphic, together with everything it drags along, and a second use at another record may then clash. Two of the conditions are inherent to let-polymorphism. Freshness for Γ and for the stumps that stay parked is the side condition of Hindley-Milner and HM(X), and a stump of an outer binding may not be captured, since it would never be finalized. [The fillability test is not inherent](what is that test?). It is a syntactic approximation of inhabitation and rejects two kinds of spent stumps: those blocked on their key, as in `let f = r: a: r.${a} c`, and spent results that differ in more than their type variables. Nested selection such as `let g = x: (x.l).m`, record literals in a spent result and the same field spent twice on one row pass the test and are generalized.

=== Spent Promises Blocked on a Key

With first-class labels (@sec-fc-labels), a lookup whose answer was already consumed can be blocked on its key rather than on a row. There is then no row to extend and nothing to wake, and the stump is never answered unless the key is supplied. The declarative system types such programs, so this is incompleteness rather than rejection. Two remedies are possible: guessing the key, which answers non-principally with an invented label, or reporting a qualified type at the top level, which is principal but changes what a run returns.

=== Unrestricted T-★-intro

The declarative system may replace any type by ★, so it types programs on which the algorithm clashes. The program
$
  #raw("g: { a = g {l = c}; b = g {m = c}; }") quad : quad (★ → 𝓫) → {a: 𝓫 | b: 𝓫}
$
is typed by sending the parameter type of g to ★, while inference unifies the two argument records and clashes. Every stuck witness above becomes typable in the same way. A clash therefore certifies the absence of a unifier, but not the absence of a typing. T-★-intro acts as subsumption into a top type, and ★ is the join of any two distinct types, whereas unification only computes common instances. Closing the gap requires either inference that joins incompatible lower bounds at ★ or a completeness statement restricted to typings without T-★-intro.
