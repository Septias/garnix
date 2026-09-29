# Organization

## General
We are rewriting the thesis text, section for section, paragraph for paragraph. The Sub-heading »fixes« shows which parts have open problems currently. The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch.

## General: Points
- FC-labels are not fully proven yet, but I'm confindent it will land so act like its there


# Section Content
## Introduction
- [ ] We are motivated by the nix language itself
- [ ] Nix is the largest body of untyped functional code 
- [ ] It revolves largely around records (3 examples)
- [ ] That is why we want to solve *asymmetric concat*
- [ ] Our path: We use *scoped records* & *unknown type*
- [ ] The unknown type is motivated by the *untypability*, similar to typescript
- [ ] This is a *natural choice*
- [ ] It oftenly uses open record concatenation (unhandled in many cases)
- [ ] The motivating example of our work is the `//` operator, it can be used in nix this way (•)




# Section Fixes
- Global
  - [ ] Declarative sorting not in Lean (pair of maps); only stated in Mechanization
- Declarative
  - [ ] Stump result δ a variable in Q — Lean now `Stump.res : Ty B`
  - [ ] Lookup properties: drop unclear "no side condition" in totality
  - [ ] Row equivalence: reword "rather than a side problem"
  - [ ] Spine: add example (uncommon technique)
  - [ ] Sorts: too boring for 2 paragraphs, shorten
  - [ ] T-sel-★ / T-sel-⊥: expand, non-standard
  - [ ] Instantiation: too verbose; sorting para removable
  - Row lookup:
    - [ ] `selDyn` not covered
- Refinement
  - [ ] ⊑ defined inline — move to declarative
- Unification
  - [ ] Fuel sentence: reword or drop
  - [ ] Rules not tangible — examples
  - [ ] Window: add sketch
- Inference
  - [ ] F-hit / materialization only in prose, no rule
  - [ ] Quiescence: example why it matters
  - [ ] A-let: explain mechanism, list side conditions literally
  - [ ] Finalization: explain phases and their order
- Incompleteness
  - [ ] Irreducible: say why α = β = ε fails (wand) / is not most general (two-sided)
  - [ ] Irreducible: re-check last paragraph
- Extensions
  - [ ] Stub subsections only
