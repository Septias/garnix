#import "../functions.typ": *

// extend this section a bit

= Mechanization <sec-mechanization>
The metatheory of this thesis is mechanized in Lean 4, sorry-free and not using axioms beyond `propext`, `Quot.sound` and `Classical.choice`. The development started with MiniNix and first-class labels but has been extended with routine features such as booleans, conditionals, lists and builtins, to form a calculus we call MacroNix. We give a short overview of this work in @app-macronix.

The development comprises about 35,000 lines of Lean in 41 files, with 1,339 theorems over 546 definitions. It builds with Lean 4.31.0 and uses neither Mathlib nor Batteries. Row unification takes the largest share with 13,700 lines, followed by inference with 9,300 and the declarative system with 8,200. MacroNix adds 700 lines, and 3,000 lines check the development itself.

The checks are of three kinds. `Axioms.lean` compares the axioms of 215 headline theorems with a fixed list, so a `sorry` or a new axiom below any of them fails the build; 25 of them use no axiom and 88 use `Classical.choice`. `ThesisExamples.lean` replays the worked examples of the text: unification examples move by move, inference examples by running the implementation, and declarative typings as proofs. `Regressions.lean` fixes 59 verdicts of the unifier by `rfl`, and `Refutations.lean` keeps 28 refuted conjectures as theorems. Every theorem, lemma, proposition and corollary of the text names its Lean counterpart. The build reports no errors and only linter warnings.

Two checks run outside the build. `NixSuite.lean` runs the 21 programs of the Nix test suite of @sec-goals. A fuzzer runs the unifier on all 968,539 pairs of spines with at most three atoms over four small alphabets. No pair exhausts a fuel of 64, and every solution the unifier returns is well-formed. The second property is not proved, so the fuzzer is its only evidence.

We deviate only slightly from the shown results: the sorts of variables are recorded on the inference algorithms state and not by the declarative system. Type schemes then quantify over untagged names and the substition differentiates between different sorts. The sorting of @declarative and its single sort-respecting substitution are the intended reading of this encoding but not a mechanized result.

Row unification is not structurally recursive, so the mechanization defines it with a fuel parameter and an additional verdict for depleted fuel. @thm-unif-term is mechanized as the statement that some fuel suffices for every problem (`unifyRowM_terminates`) and that every larger fuel returns the same verdict (`unifyM_fuel_mono`), so $scripts(≐)_r$ is a function. The termination measure relies on drawn names being new. The unifier draws its fresh names from a supply that starts above the variables of the problem. During inference, this is the supply of the solver state, advanced past the variables of the problem and those that θ binds, so a name drawn by U-host is new to both and θ stays applied (`unifyM_draws`).
