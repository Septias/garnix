#import "../functions.typ": *

= Mechanization <sec-mechanization>
The metatheory of this thesis is mechanized in Lean 4, sorry-free and not using axioms beyond `propext`, `Quot.sound` and `Classical.choice`. The development started with Mininix and first-class labels but has been extended with trivial features such as booleans, conditionnals, lists and builtins, to form a calculus we call MacroNix. We give a short overview of this work in @app-macronix.

We differentiate only slightly from the shown results: the sorts of variables are recorded on the inference algorithms state and not by the declarative system. Type schemes then quantify over untagged names and the substition differentiates between different sorts. The sorting of @declarative and its single sort-respecting substitution are the intended reading of this encoding but not a mechanized result.

