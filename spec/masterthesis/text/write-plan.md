# Organization

## General
We are rewriting the thesis text, section for section, paragraph for paragraph. The Sub-heading »fixes« shows which parts have open problems currently. The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch.

## General: Points
- FC-labels are not fully proven yet, but I'm confindent it will land so act like its there


# Section Content
## Introduction
- [x] We are motivated by the nix language itself
- [?] Nix is the largest body of untyped functional code 
- [x] It revolves largely around records (3 examples)
- [x] That is why we want to solve *asymmetric concat*
- [x] Our path: We use *scoped records* & *unknown type*
- [x] The unknown type is motivated by the *untypability*, similar to typescript
- [x] This is a *natural choice*
- [x] It oftenly uses open record concatenation (unhandled in many cases)
- [x] The motivating example of our work is the `//` operator, it can be used in nix this way (•)


## Declarative
- [ ] L2 vorstellen
- [ ] Warum brauchen wir T-sel-★ und T-★-intro
- [ ] Explain why `T-sel-⊥` ★ typed
- [ ] Motivation für qualified types?
- [ ] Trace monoid
- [ ] The winning example
- [ ] Principality


# Section Fixes
- Global
  - [ ] Mechanization mismatch: sorting + sort-indexed occurs check not in Lean (pair of maps, two-sorted ftv)
- Motivation
  - [ ] Old positioning
- Declarative
  - [ ] Stump result δ a variable in Q — Lean now `Stump.res : Ty B`
  - Row lookup:
    - [ ] `selDyn` not covered
- Inference
  - [ ] No F-hit / materialization / spent promises
  - [ ] "Finalization … is the only point" and "result is a single variable" outdated
  - [ ] `x: (x.l).m` "not inferred" — check against materialization; name remaining limits (key-blocked, independence, `let g = λx.(x.l).m` used twice)
- Refinement
  - [ ] ⊑ defined inline — move to declarative
- Extensions
  - [ ] Only stub bullets
  - [ ] Towards Nix placeholder
- Related work
  - [ ] Missing citations marked ¿: Ohori, Cartwright–Fagan, `Dynamic()`, Broekhoff–Krebbers step relation, "expressiveness", Nickel
- Conclusion
  - [ ] Placeholder text
