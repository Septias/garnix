#import "../functions.typ": *

= Mechanization <sec-mechanization>

The metatheory of this thesis is mechanized in Lean 4. The development contains no `sorry`: a dedicated file pins the axiom dependencies of every headline theorem with `#guard_msgs`, so that the build fails if a proof comes to depend on `sorryAx` or on any axiom beyond `propext`, `Quot.sound` and `Classical.choice`. Every mechanized result in this thesis names its Lean counterpart.

The development covers MiniNix extended with the first-class labels of @sec-fc-labels, and the mechanized theorems are stated for that extension; MiniNix is its label-free fragment. Two parts of the presentation differ from the development. First, sorts are tracked by the inference algorithm, whose state records the sort at which each name was drawn, but not by the declarative system: its schemes quantify over untagged names, and a substitution is a triple of maps, for types, rows and keys, over a single namespace. A name therefore has a reading at each sort, and the occurs check of unification tests only the sort at which a variable is bound. The sorting of @declarative and its single sort-respecting substitution are the intended reading of this encoding, not a mechanized result. Second, the two finalization phases of @finalization are the relations `Materializes` and `Finalizes` of the development, and F-hit is its `Materialize`.
