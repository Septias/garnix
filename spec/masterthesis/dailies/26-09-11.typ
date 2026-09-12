
./26-09-12.typ
== Fäden
- Negative type information
- FC labels
- Inferenzregeln


== Tisch
- Fix for some `stuch => no mgu` cases (lazy)


== Misc
- Different agents can do (FC-labels, negative-info, L2-typ-inference) proofs in different files.


== FC-Labels
- FC-Labels give are actually (pretty easy) to add
- Unification only looks up one label, so we always exhaust it directly
  - How does that work for rows currently? Do I remove them?
    - I think we just add to the substitution?


== Claude Prompts
- [x] Create a plan on how to add FC-labels to the typesystem
- [x] Create a plan on how to add negative label information to the typesystem
- [x] What are the general results we want to show in thesis.typ? And how far are we from them? Analyze the lean code and take proof-state.md as entry point.


== Claude Outputs
D. A design gap that is not a statement gap

★ is rigid and has no elimination, so A-app on a ★-typed function emits ★ ≐ (τ₂→β) ⟹ clash ⟹ hard error — and declaratively (e.l) e₂ has no derivation either. The systems agree, so no theorem is blocked, but the soft-typing claim "every program keeps its untyped semantics" is false at the checker level: those programs are rejected. Fixing it needs a consistency relation τ ~ ★ alongside ≐ — an extension, not a gap.
