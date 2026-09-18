== Claude Prompts
- [ ] The success leg can be vacuous, tell me how "bad" that is and propose a fix.
- [ ] What is the unification type of row equivalence under asymmetric concatenation? Unitary, finitary, infinitary, or nullary?
- [ ] What is the expandR driver arm?
- [ ] Try to prove `occurs => ¬no unifier` (use the plan for it)
- [ ] What is the best naming-strateg for real? Should we make architectural changes to switch to deBrujin for example?


== Find: the degradation rules are unsound
Turned up while proving `InferSound` case by case (lean/InferSound.lean, on
branch worktree-infer-sound).

`A-app-degrade` fires when `τ₁ ≐ τ₂ → β` is stuck or occurs, and returns ★.
Declaratively the application then types *nowhere*:
- `qApp` wants a literal arrow,
- `qEq` only moves along ≈,
- ≈ relates ★ to nothing but itself (`TyEquiv.unk_inv`),
so `qUnk` cannot manufacture the arrow either. What is missing is a DECLARATIVE
rule — application at ★, the ★-elimination the failure policy assumes and the
type system does not have.

This is *not* the gap inference-gap-analysis.md had recorded. That row asked for
"replacing a position by ★ preserves declarative typeability", which is about a
term that already types. Row corrected there.

`A-sel-degrade` is plausibly unreachable: `r` is drawn fresh immediately before
`τ ≐ {r}`, so that equation can only clash or succeed. `A-app-degrade` is not —
its equation descends into an arbitrary row problem and can genuinely stick.

Open: either add T-app-★ declaratively, or prove the rule unreachable. No
counterexample program mechanized — this is an argument, not a refutation.
