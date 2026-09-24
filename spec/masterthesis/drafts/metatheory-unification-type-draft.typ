
== What unification cannot prove <unification-type>

The obstruction is a property of row equivalence itself, and no algorithm that returns a solution — or finitely many — can avoid it.

*The classification.* Given an equational theory, order its solutions by subsumption: θ′ is an _instance_ of θ when $θ′ = σ ∘ θ$ for some σ, modulo the theory (`InstanceOf`, and `InstanceOfOn` for the relativized form @unification-metatheory already argued for). A set of unifiers is _complete_ when every unifier is an instance of some member (`CompleteOn`), and _minimal_ when its members are pairwise incomparable. Siekmann's classification @unification_theory grades a theory by the size a minimal complete set can be forced to: _unitary_ if a single most general unifier always suffices, _finitary_ if a finite set always does, _infinitary_ if some problem needs an infinite one, and _nullary_ if some solvable problem admits no minimal complete set at all. Syntactic unification is unitary, and so are the row theories of Rémy @remy_typechecking and Paszke&Xie that ours departs from; associativity alone — word equations — is already infinitary.

Operationally the grade is a statement about what an algorithm may return. Unitary licenses one answer and with it principal types. Finitary licenses a finite disjunction: principality is lost, but completeness survives — this is exactly Wand's position @concat4multiinher, whose exponential set of alternative typings @sec-motivation rejects on cost grounds rather than on principle. Infinitary licenses neither. There is then no complete algorithm with finite output, and the honest responses are to postpone the constraint or to refuse it.

*Our theory is not finitary.* The wand-ambiguity of @unification already shows that row equivalence is not unitary: $(β | α) scripts(≐)_r (l: 𝓫)$ is solvable — the field may live in either variable — with two incomparable maximal unifiers and no mgu (`wand_no_mgu`). That is a finitary obstruction — a complete set of size two exists — so on its own it leaves the Wand escape open. It is closed by a smaller problem, which we call the _shift problem_:

$ (α | l: 𝓫) quad scripts(≐)_r quad (l: 𝓫 | α) $

the same row-variable on both sides of one field. Three facts about it, all mechanized in `RowUnify/UnifType.lean`:

- *It is solvable.* $α ≔ ε$ is a unifier (`shift_unifiable`).
- *Every unifier makes α variable-free.* If θ unifies the shift problem then $"vars"(θ α) = []$ (`shift_unifier_varFree`). *The equation admits no symbolic answer whatsoever*. The mechanism is the trace-monoid presentation of @trace-monoid read backwards: the trailing field sits at segment index $|"vars"(θ α)|$ on the left and at index 0 on the right, and pointwise agreement of the $l$-projections walks the list down until $|"vars"(θ α)| = 0$ (`shift_proj_forces_zero`).
- *The solutions form an infinite antichain.* $α ≔ (l: 𝓫)^k$ is a unifier for every $k ∈ NN$ (`shiftSub_unifies`), and no two are comparable (`shiftSub_antichain`). Incomparability is counting again: covering fixes the field count $|s|_l$ exactly once the covered image is variable-free (`instanceOfOn_fieldCount_eq_of_varFree`), and the previous point supplies that hypothesis for free.

Together they give no most general unifier (`shift_no_mgu`) and, more sharply, no finite complete set of unifiers at all (`shift_no_finite_complete_set`, packaged as `rowUnification_not_finitary`). Row equivalence under asymmetric concatenation is therefore *at least infinitary*. The reason is structural and worth naming: admitting duplicate labels means two fields at the same label do not commute, so the quotient monoid of @trace-monoid contains a free monoid of rank two, and the shift problem is the word equation $x a = a x$ transported into rows.

*What this buys the algorithm.* The verdict on the shift problem is #u_stuck, in both orientations, while its instances $k = 0$ and $k = 1$ succeed — kernel-checked as `unify_shift_stuck`, `unify_shift_stuck_mirror`, `unify_shift_inst_zero` and `unify_shift_inst_one`. So the incompleteness reported in @unification-metatheory is not a missing arm, and U-expand's uniqueness side condition is not over-cautious: *no terminating algorithm with finite output is complete on this theory*, and #u_stuck is the shape that refusal has to take. The design of @our-position — refuse, rather than guess or enumerate — is thus forced from below as well as motivated from above.

*What is left open.* We prove the theory is neither unitary nor finitary; we do not separate infinitary from nullary. That separation asks whether every solvable problem has a _minimal_ complete set, and the antichain above does not settle it, because it is not itself complete: for $m ≠ l$ the substitution $α ≔ (m: 𝓫)$ is another unifier (`offSub_unifies`) covered by no member of the family (`shift_antichain_not_complete`), since a label other than $l$ is unconstrained and commutes freely past it. The full solution set of the shift problem is "every variable-free row whose $l$-fields are all 𝓫, arbitrary elsewhere", so the natural candidate for a minimal complete set is indexed by traces rather than by $k$. Proving that set complete would place the theory at infinitary; failing to would make it nullary, and nullary would matter — it would rule out the constraint-postponing reformulation that infinitary still permits, since there would be no canonical solution set to postpone to.

// ---------------------------------------------------------------------------
// BIB ENTRIES NEEDED (not added to bib/ — paste into bib/misc.bib if the
// section lands). `@concat4multiinher`, `@remy_typechecking` and
// `@wand_complete` already exist in bib/records.bib.
//
// @incollection{unification_theory,
//   author    = {Baader, Franz and Snyder, Wayne},
//   title     = {Unification Theory},
//   booktitle = {Handbook of Automated Reasoning},
//   editor    = {Robinson, Alan and Voronkov, Andrei},
//   publisher = {Elsevier and MIT Press},
//   year      = {2001},
//   volume    = {I},
//   pages     = {445--533},
// }
//
// If you would rather cite the classification at its source than at the survey:
// @article{siekmann_unification,
//   author  = {Siekmann, J{\"o}rg H.},
//   title   = {Unification Theory},
//   journal = {Journal of Symbolic Computation},
//   volume  = {7},
//   number  = {3--4},
//   pages   = {207--274},
//   year    = {1989},
// }
