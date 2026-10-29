#import "../functions.typ": *

= Mechanization <sec-mechanization>
The metatheory of this thesis is mechanized in Lean 4, sorry-free and not using axioms beyond `propext`, `Quot.sound` and `Classical.choice`. The development started with MiniNix and first-class labels but has been extended with routine features such as booleans, conditionals, lists and builtins, to form a calculus we call MacroNix. We give a short overview of this work in @app-macronix.

We deviate only slightly from the shown results: the sorts of variables are recorded on the inference algorithms state and not by the declarative system. Type schemes then quantify over untagged names and the substition differentiates between different sorts. The sorting of @declarative and its single sort-respecting substitution are the intended reading of this encoding but not a mechanized result.

Row unification is not structurally recursive, so the mechanization defines it with a fuel parameter and an additional verdict for depleted fuel. @thm-unif-term is mechanized as the statement that some fuel suffices for every problem (`unifyRowM_terminates`) and that every larger fuel returns the same verdict (`unifyM_fuel_mono`), so $scripts(≐)_r$ is a function. The termination measure relies on drawn names being new. The unifier draws its fresh names from a supply that starts above the variables of the problem. During inference, this is the supply of the solver state, advanced past the variables of the problem and those that θ binds, so a name drawn by U-host is new to both and θ stays applied (`unifyM_draws`).
