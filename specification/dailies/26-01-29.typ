#import "../functions.typ": *


== Flow / Gradual / Occurrence
- *Gradual typing* systems was parallely introduced by Siek et. al and Tobin et al. @gradual_siek @gradual_tobin and solves the problem of migrating codebases from untyped to typed code. This system defines two-in-one calculus consisting of a fully typed and untyped calculus. Since we would like to avoid adding type annotations in forms of comments @typescript to the language, we want to avoid it.
- *Occurrence Typing* systems refine the type of variables based on their usage. That means they refine the types of variables based on type-cases. We don't have them in nix, but conditionals with the builtin type-checks can be translated.
- *Flow Typing* systems subsume occurence typing systems because they allow single variables to change their types should they be assigned to new ones. In nix it is not even possible to assign a variable twice (if not shadowed), meaning this extension is not needed.


#bib
