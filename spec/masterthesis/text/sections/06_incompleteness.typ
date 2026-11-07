#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= Incompleteness <incompleteness>

The inference algorithm is sound but not complete. This section describes where it rejects a program that the declarative system types. The cases fall into three groups: problems without a most general unifier, stuck problems that have one, and limits of the inference algorithm itself.


== Irreducible Problems <incompleteness-irreducible>

The Wand ambiguity is our running example of a unification problem without a most general unifier. A single unifier can attribute the field to both sides and no unifier is more general than the other. There is two more problems with this structure:

#proposition(
  name: [Irreducible problems],
  lean: "vars_vs_field_no_mgu_on, two_sided_no_mgu_on, shift_no_finite_complete_set",
)[
  + _(Wand ambiguity)_ For pairwise distinct $α₁, …, α_n$ with $n ≥ 2$, $(α₁ | … | α_n) scripts(≐)_r (l: 𝓫)$ has no most general unifier.
  + _(Two-sided)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has no most general unifier.
  + _(Shift)_ $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ has no finite complete set of unifiers.
] <prop-irreducible>

The Wand ambiguity $(α | β) scripts(≐)_r (l: 𝓫)$ has the unifiers $[α ≔ (l: 𝓫), β ≔ ε]$ and $[α ≔ ε, β ≔ (l: 𝓫)]$: the field comes from either operand, and neither one is more general than the other. The two-sided problem is similar: $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | β)$ has the unifiers $[α ≔ ε, β ≔ ε]$ and $[α ≔ (l: 𝓫 | γ), β ≔ (γ | l: 𝓫)]$ where α holds either no l-field or at least one. None is more general than the other. The shift problem $(α | l: 𝓫) scripts(≐)_r (l: 𝓫 | α)$ is the two-sided problem with $β = α$. This problem has the unifier $[α ≔ (l: 𝓫)^n]$ for every $n ≥ 0$, where $(l: 𝓫)^n$ is the row of n copies of the field. Every choice of n gives a new unifier so this problem does not even have a finite set of unifiers.

All three are reported as #u_stuck because no forcing move applies and stuck is the fallback. We can also not use ★in this situation because a stuck unifier leaves the inference equation unanswered. Two completely unrelated terms could then be equated which leaves a reduct which is not a value and where no moves apply. It is also not a lookup error so progress fails.

== Solvable Stuck Problems <incompleteness-forced>

We identify two stuck problems that do have a most general unifier but it is not found by unification:

- The _mirrored crossfield_ problem $(α | l: 𝓫) scripts(≐)_r (β | m: 𝓫)$ has the most general unifier $[β ≔ (β′ | l: 𝓫), α ≔ (β′ | m: 𝓫)]$, the trailing-end image of @ex-unif-host. U-host is stated for the leading end only, so the problem is reported as stuck. We tried several measures with both sides added, but could not prove termination for it.
- The unification algorithm reports stuck for a unification problem before emitted unification problems are solved in the recursive call. The problem $(k: {β | α} | β) scripts(≐)_r (k: {l: 𝓫} | l: 𝓫)$ has the unifier $[β ≔ (l: 𝓫), α ≔ ε]$ which can be found by noticing that unification of the two k-fields forces $β = (l: 𝓫)$. Deferring the stuck equation and letting the residual run first would solve this instance, but whether the resulting algorithm is confluent is not clear.

Stuck is therefore conservative: it does not imply that no most general unifier exists.

== Limits of Inference <incompleteness-inference>

The remaining limits lie in the inference algorithm rather than in unification.

- *Partially Generalized Lets* A-let generalizes the greatest admissible $macron(α)$. A variable that violates one of the conditions stays monomorphic, together with everything it drags along, and a second use at another record may then clash.
- *Dead Bindings* T-let constrains e₁ only through the instances of σ. If σ has no instance, e₁ is unconstrained: `let x = c c in c` is typed at $𝓫_c$ and evaluates to c, since E-let discards the binding. A-let infers e₁ whether x is used or not, so inference reports a clash. A completeness statement has to exclude such bindings.
- *Fillability Test* The fillability test of the entry point is a syntactic approximation of inhabitation. It rejects a blocker of a spent stump in the result of a spent stump, as in the overlay form `self: super: {foo = super.foo ‖ {meta = super.foo.meta ‖ {broken = false}}}`, whose scheme is inhabited. The blocker of the meta stump occurs in the spent result of the foo stump, so the second clause fails. Nested selection such as `x: (x.l).m`, record literals in a spent result, the same field spent twice on one row, a field selected once spent and once unspent on one row, and a spent stump under a key of the binding, as in `r: a: r.${a} c` with first-class labels (@fc-labels), pass the test.
- *Unrestricted T-★-intro* The declarative system may replace any type by ★, so it types programs on which the algorithm clashes. The program $#raw("g: {a = g {l = c} | b = g {m = c}}") quad : quad (★ → 𝓫) → {a: 𝓫 | b: 𝓫}$ is typed by sending the parameter type of g to ★, while inference unifies the two argument records and clashes. T-★-intro acts as subsumption into a top type, and ★ is the join of any two types with different head constructors, whereas unification only computes common instances. A successful run can therefore return a scheme that is not principal, and a completeness statement has to be restricted to typings without T-★-intro.
