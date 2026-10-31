#import "../functions.typ": *


// say here that all proves are AI-generated?

= Mechanization <sec-mechanization>

The metatheory and all examples of this thesis are mechanized in Lean 4, sorry-free and not using axioms beyond `propext`, `Quot.sound` and `Classical.choice`. The development started with MiniNix and first-class labels but has been extended with routine features such as booleans, conditionals, lists and builtins, to form a calculus we call MacroNix. We give a short overview of this work in @app-macronix.

The development consists of about 35,000 lines of Lean code in 41 files, with 1,339 theorems and 545 definitions. It is run on Lean 4.31.0 and built without libraries: Neither Mathlib nor Batteries is used. Row unification takes the largest share of the development with 13,700 lines, followed by inference with 9,300 and the declarative calculus with 8,200. MacroNix adds another 700 lines. 3,000 lines check the development itself and include the axiom-guards, a fuzzer, and regression-checks.

We deviate only slightly from the shown results: In our proofs, the sorts of variables are recorded on the inference algorithms state and not by the declarative system. Type schemes then quantify over untagged names and the substition differentiates between different sorts. The sorting of @declarative and its single sort-respecting substitution are the intended reading of this encoding but not a mechanized result.

Row unification is not structurally recursive, so the mechanization defines it with a fuel parameter and an additional verdict for depleted fuel. @thm-unif-term is mechanized as the statement that some fuel suffices for every problem (`unifyRowM_terminates`) and that every larger fuel returns the same verdict (`unifyM_fuel_mono`).
