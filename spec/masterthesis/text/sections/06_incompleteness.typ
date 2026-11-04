#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= Incompleteness <incompleteness>

The inference algorithm is sound but not complete. This section describes where it rejects a program that the declarative system types. The cases fall into three groups: problems without a most general unifier, stuck problems that have one, and limits of the inference algorithm itself.


== Irreducible Problems <incompleteness-irreducible>

The Wand ambiguity is the prime example of a unification problem without a most general unifier. It has a finite complete set of unifiers, one for every origin of the field, but no single unifier covers the others. Two more problems lack a most general unifier as well:

#proposition(
  name: [Irreducible problems],
  lean: "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set",
)[
  + _(Wand ambiguity)_ For pairwise distinct $α₁, …, α_n$ with $n ≥ 2$, $(α₁ | … | α_n) scripts(≐)_r (l: 𝓫)$ has no most general unifier.
  + _(Two-sided)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has no most general unifier.
  + _(Shift)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers.
] <prop-irreducible>

The Wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the unifiers $[α ≔ (l: 𝓫), β ≔ ε]$ and $[α ≔ ε, β ≔ (l: 𝓫)]$: the field comes from either operand, and neither choice is an instance of the other. The two-sided problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has the unifiers $[α ≔ ε, β ≔ ε]$ and $[α ≔ (l: 𝓫 | γ), β ≔ (γ | l: 𝓫)]$: α holds either no l-field or at least one, and no unifier leaves this open. The shift $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ is the two-sided problem with $β = α$. It has the unifier $[α ≔ (l: 𝓫)^n]$ for every $n ≥ 0$, where $(l: 𝓫)^n$ is the row of n copies of the field, and each of these unifiers fixes the number of copies.

All three are reported as #u_stuck: no move applies, and any move would prefer one unifier to another. Inference cannot answer ★ for an application with a stuck equation, since a ★ at the result does not discharge the equation: T-★-intro blurs a finished typing, and T-λ-E still demands that argument and parameter agree. Without the equation, `f: a: b: {p = f {l = z: z} | q = f (a ‖ b)}` would accept the arguments `x: (x.l) c`, `{l = c}` and `{}`, for which q evaluates to `c c`, neither a value nor a lookup error. The declarative system types this program only by sending the parameter type of f to ★ (@incompleteness-inference), a choice that inference would have to make at the binder of f.

== Solvable Stuck Problems <incompleteness-forced>

We identify two stuck problems that do have a most general unifier:

- The _mirrored crossfield_ problem $(α | l: 𝓫) scripts(≐)_r (β | m: 𝓫)$ has the most general unifier $[β ≔ (β′ | l: 𝓫), α ≔ (β′ | m: 𝓫)]$, the trailing-end image of @ex-unif-host. U-host is stated for the leading end only, so the problem is stuck. We tried several measures, but could not prove termination for the version with both ends.
- A stuck equation between the types of two paired fields is reported before the remaining row equation solves the variable it depends on, which masks an existing solution (@prop-masks). Deferring the stuck equation and letting the residual run first would solve this instance, but it abandons the discipline of @unification that every emitted equation is discharged, fatal or stuck on the spot. Whether the resulting algorithm is confluent is open.

#proposition(name: [Stuck masks an mgu], lean: "stuck_masks_mgu")[
  The problem $(k: {β | α} | β) scripts(≐)_r (k: {l: 𝓫} | l: 𝓫)$ is stuck, but $[β ≔ (l: 𝓫), α ≔ ε]$ is a most general unifier.
] <prop-masks>

Stuck is therefore conservative: it does not imply that no most general unifier exists.

== Limits of Inference <incompleteness-inference>

The remaining limits lie in the inference algorithm rather than in unification.

=== Partially Generalized Lets

A-let generalizes the greatest admissible $macron(α)$. A variable that violates one of the conditions stays monomorphic, together with everything it drags along, and a second use at another record may then clash. The fillability test of A-let is a syntactic approximation of inhabitation and rejects spent results on one field that differ in more than their type variables and that the merge does not identify, as in `let f = x: {p = (({m = c} ‖ x).l) c | q = x.l}`, which selects l on two different rows.

// todo: explain this more thoroughly
Nested selection such as `let g = x: (x.l).m`, record literals in a spent result, the same field spent twice on one row, a field selected once spent and once unspent on one row, and a spent stump under a key of the binding, as in `let f = r: a: r.${a} c` with first-class labels (@fc-labels), pass the test and are generalized.


=== Unrestricted T-★-intro

The declarative system may replace any type by ★, so it types programs on which the algorithm clashes. The program
$
  #raw("g: {a = g {l = c} | b = g {m = c}}") quad : quad (★ → 𝓫) → {a: 𝓫 | b: 𝓫}
$
is typed by sending the parameter type of g to ★, while inference unifies the two argument records and clashes. T-★-intro acts as subsumption into a top type, and ★ is the join of any two types with different head constructors, whereas unification only computes common instances. A successful run can therefore return a scheme that is not principal, and a completeness statement has to be restricted to typings without T-★-intro.
