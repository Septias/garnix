#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= Incompleteness <incompleteness>

The inference algorithm is sound but not complete. This section describes where it rejects a program that the declarative system types. The cases fall into three groups: problems without a most general unifier, stuck problems that have one, and limits of the inference algorithm itself.


== Irreducible Problems <incompleteness-irreducible>

The Wand ambiguity is our running example of a unification problem with no most general unifier. The field accessed in $a: b: (a ‖ b).l$ can be attributed to $a$ or $b$ but no choice is more general than the other. There are two more problems with this structure:

#proposition(
  name: [Irreducible problems],
  lean: "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set",
)[
  + _(Wand ambiguity)_ For pairwise distinct $α₁, …, α_n$ with $n ≥ 2$, $(α₁ | … | α_n) scripts(≐)_r (l: 𝓫)$ has no most general unifier.
  + _(Two-sided)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has no most general unifier.
  + _(Shift)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers.
] <prop-irreducible>

The unification problem of the Wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the unifiers $[α ≔ (l: 𝓫), β ≔ ε]$ and $[α ≔ ε, β ≔ (l: 𝓫)]$. The field comes from either operand but no choice is more general than the other. The two-sided problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ splits by whether α holds an l-field. If it does, $[α ≔ (l: 𝓫 | γ), β ≔ (γ | l: 𝓫)]$ covers every unifier. If it does not, α and β are the same row without l and without row-variables, such as ε or $(m: 𝓫)$, since a row-variable cannot swap past the field l. No unifier of one case is an instance of the other. The shift problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ is the two-sided problem with $β = α$. Its unifiers send α to the rows without row-variables whose l-fields all have type 𝓫, such as $(m: 𝓫)$ or $(l: 𝓫)^n$ for every $n ≥ 0$, where $(l: 𝓫)^n$ is the row of n copies of the field. Each unifier fixes the number of l-fields, so no finite set of unifiers is complete.

All three are reported as #u_stuck because no forced move applies and stuck is the fallback. We can also not use ★ to type the expression because a stuck verdict leaves the inference equation unanswered.

== Solvable Stuck Problems <incompleteness-forced>

We identify two stuck problems with a most general unifier the algorithm does not find:

- The problem $(α | l: 𝓫) scripts(≐)_r (β | m: 𝓫)$ has the most general unifier $[β ≔ (β′ | l: 𝓫), α ≔ (β′ | m: 𝓫)]$, the trailing-end image of @ex-unif-host. U-host is stated for the leading end only, so the problem is reported as stuck. We tried several measures with both sides added, but could not prove termination for it.
- The problem $(k: {β | α} | β) scripts(≐)_r (k: {l: 𝓫} | l: 𝓫)$ has the unifier $[β ≔ (l: 𝓫), α ≔ ε]$ which can be found by noticing that unification of the two k-fields determines $β = (l: 𝓫)$. It is however not found by our algorithm because the problem is reported as stuck. Deferring the stuck row-equation and letting the residual type unification run first would solve this instance, but whether the resulting algorithm is confluent is not clear.

Stuck is therefore conservative: it does not imply that no most general unifier exists.

== Limits of Inference <incompleteness-inference>

The remaining limits lie in the inference algorithm rather than in unification.

- *Stuck Equations* A typing needs only some unifier, not a most general one, so the declarative system types programs whose equations are reported as stuck (@incompleteness-irreducible, @incompleteness-forced). Inference nevertheless rejects such a program, since only a successful unification has a rule.
- *Partially Generalized Lets* A-let generalizes the greatest admissible $macron(α)$. A variable that violates one of the conditions stays monomorphic, together with everything it drags along, and a second use at another record may then clash.
- *Dead Bindings* T-let constrains e₁ only through the instances of σ. If σ has no instance, e₁ is unconstrained: `let x = c c in c` is typed at $𝓫_c$ and evaluates to c, since E-let discards the binding. A-let infers e₁ whether x is used or not, so inference reports a clash. A completeness statement has to exclude such bindings.
- *Unrestricted T-★-intro* The declarative system may replace any type by ★, so it types programs on which the algorithm clashes. The program $#raw("g: {a = g {l = c} | b = g {m = c}}") quad : quad (★ → 𝓫) → {a: 𝓫 | b: 𝓫}$ is typed by sending the parameter type of g to ★, while inference unifies the two argument records and clashes. T-★-intro acts as subsumption into a top type, and ★ is the join of any two types with different head constructors, whereas unification only computes common instances. A completeness statement would have to be restricted to typings without T-★-intro.
