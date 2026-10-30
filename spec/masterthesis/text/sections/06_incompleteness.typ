#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= Incompleteness <incompleteness>

The algorithm is sound but not complete. This section collects the places we know of where it rejects a program that the declarative system types, or returns a less general answer than the declarative system admits. They fall into three groups: problems without a most general unifier, problems that have one but are not solved by forced moves, and limits of the inference algorithm itself. We do not prove that these groups are exhaustive.

== Irreducible Problems <incompleteness-irreducible>

The Wand ambiguity is the prime example of a unification problem without a most general unifier. It has a finite complete set of unifiers, but no single unifier covers the others. Two more problems lack a most general unifier as well:

#proposition(
  name: [Irreducible problems],
  lean: "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set",
)[
  + _(Wand ambiguity)_ For pairwise distinct $α₁, …, α_n$ with $n ≥ 2$, $(α₁ | … | α_n) scripts(≐)_r (l: 𝓫)$ has no most general unifier.
  + _(Two-sided)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has no most general unifier.
  + _(Shift)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers.
] <prop-irreducible>

The wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ describes a situation in which a single field cannot be allocated to a definite row variable. Either variable may carry the field and neither choice subsumes the other. There is one unifier for every row variable, and none of them is most general. An algorithm must therefore either search among the candidates or carry the complete set, as Wand does @concat4multiinher. The two-sided problem is a variant with the field on both sides: the two fields can be partners with both variables empty, or each variable can carry an l-field of its own. The shift is the two-sided problem with $β = α$.

#example(name: [Unifiers of the irreducible problems])[
  - The Wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the unifiers $[α ≔ (l: 𝓫), β ≔ ε]$ and $[α ≔ ε, β ≔ (l: 𝓫)]$. Both are ground and they differ, so neither is an instance of the other. A most general unifier would have to be more general than both, but it cannot leave α and β open: $[α ≔ γ, β ≔ γ′]$ and every other substitution that maps α or β to a row containing a variable is no unifier, since a variable on the left would have to face a variable on the right. Nor can it send both to ε, since $(ε | ε) ≈ ε$ and ε lacks the field. So it places the single field in one of the two variables and is no more general than the unifier that places it in the other.
  - The two-sided problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has the unifiers $[α ≔ ε, β ≔ ε]$ and $[α ≔ (l: 𝓫), β ≔ (l: 𝓫)]$, which disagree on the number of l-fields in α. Unlike in the Wand ambiguity, $[α ≔ ε, β ≔ ε]$ is a unifier, but it is not most general: it is ground, its only instance is itself, and so it does not cover the second unifier. Conversely, every instance of a unifier that gives α at least one l-field, such as $[α ≔ (l: 𝓫 | γ), β ≔ (γ | l: 𝓫)]$, gives α at least one, so none covers $[α ≔ ε, β ≔ ε]$.
  - The shift $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ is unified by $[α ≔ (l: 𝓫)^n]$ for every $n ≥ 0$, where $(l: 𝓫)^n$ is the row of n copies of the field. Each unifier in a finite set fixes the number of l-fields in α, so no finite set covers all of them.
] <ex-irreducible>

All three are reported as #u_stuck: no move applies, and any move would prefer one unifier to another.

What is shown is a limit of principal solutions over scoped rows, not of typing: a typing needs only some unifier, not a most general one. Inference nevertheless rejects a program at a stuck verdict, since only a successful unification has a rule. Typing the problem at ★ instead is not sound, since ★ on the result of an application leaves the equation between the argument and the domain unsolved (@sec-gradual).

== The Price of Forced Moves <incompleteness-forced>

Two stuck problems do have a most general unifier.

- The _mirrored crossfield_ problem $(α | l: 𝓫) scripts(≐)_r (β | m: 𝓫)$ has the most general unifier $[β ≔ (β′ | l: 𝓫), α ≔ (β′ | m: 𝓫)]$, the trailing-end image of @ex-unif-host. U-host is stated for the leading end only, so the problem is stuck.
// maybe add that we don't add the host on the other side as terminaton could not be proven this way
- A stuck equation between the types of two paired fields is reported before the remaining row equation solves the variable it depends on, which masks an existing solution (@prop-masks). Deferring the stuck equation and letting the residual run first would solve this instance, but it abandons the discipline of @unification that every emitted equation is discharged, fatal or stuck on the spot. Whether the resulting algorithm is confluent, that is, whether its verdict is independent of the order in which deferred equations are retried, is open.

#proposition(name: [Stuck masks an mgu], lean: "stuck_masks_mgu")[
  The problem $(k: {β | α} | β) scripts(≐)_r (k: {l: 𝓫} | l: 𝓫)$ is stuck, but $[β ≔ (l: 𝓫), α ≔ ε]$ is a most general unifier.
] <prop-masks>

Stuck is therefore conservative: it does not imply that no most general unifier exists, not even in a configuration where no move applies.

== Limits of Inference <incompleteness-inference>

The remaining limits lie in the inference algorithm rather than in unification.

=== Partially Generalized Lets

A-let generalizes the greatest admissible $macron(α)$. A variable that violates one of the conditions stays monomorphic, together with everything it drags along, and a second use at another record may then clash. Only freshness is inherent to let-polymorphism: freshness for Γ and for the stumps that stay parked is the side condition of Hindley-Milner generalization. The other conditions concern stumps and are specific to our algorithm. A stump of an outer binding may not be captured, since it would never be discharged, and the fillability test of A-let (@generalization) checks that every spent stump can be answered at some instance. The test is a syntactic approximation of inhabitation and rejects spent results on one field that differ in more than their type variables and that the merge does not identify, as in `let f = x: {p = (({m = c} ‖ x).l) c; q = x.l}`, which selects l on two different rows. Nested selection such as `let g = x: (x.l).m`, record literals in a spent result, the same field spent twice on one row, a field selected once spent and once unspent on one row, and a spent stump blocked on a key of the binding, as in `let f = r: a: r.${a} c` with first-class labels (@sec-fc-labels), pass the test and are generalized.

=== Unrestricted T-★-intro

The declarative system may replace any type by ★, so it types programs on which the algorithm clashes. The program
$
  #raw("g: { a = g {l = c}; b = g {m = c}; }") quad : quad (★ → 𝓫) → {a: 𝓫 | b: 𝓫}
$
is typed by sending the parameter type of g to ★, while inference unifies the two argument records and clashes. Every stuck witness above becomes typable in the same way. A clash therefore certifies the absence of a unifier, but not the absence of a typing. T-★-intro acts as subsumption into a top type, and ★ is the join of any two types with different head constructors, whereas unification only computes common instances. Inference that joins incompatible lower bounds at ★ would remove the clash, but not the gap. In `g: x: { a = g x; b = g {l = c}; }` inference never clashes: it fixes x to ${l: 𝓫_c}$, while the declarative system sends the domain of g to ★ and types x at any type. A successful run can therefore return a scheme that is not principal, and a completeness statement has to be restricted to typings without T-★-intro.
