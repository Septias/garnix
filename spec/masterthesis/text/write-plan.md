# Organization

## General
The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch.


## General: Points
- FC-labels are not fully proven yet, but I'm confindent it will land so act like its there


## Misc
- Could use some more mathematical slang, even though it should be well understandable
- The rule descriptions are generally good


## Honest Contribution
- Lookup is seperate and unification stays plain first-order
- Deterministic lookup instead of entailment relation
- Trace monoid representation
- Mechanization


## Missing
- Closed type families. A stump is essentially a stuck application Lookup ρ l ~ δ. L-var returning ? is GHC's apartness check failing, and waking a stump is the solver re-trying a stuck family application. Eisenberg et al. (POPL 2014, closed type families) is missing from related work, and so are PureScript's Row.Lacks/Union and Morris's instance chains. A reviewer who knows GHC will raise this first. Positioning against it helps you: your "family" is total, and it's resolved at instantiation under let-polymorphism with no annotations.
- A section about U-expand


## Sessions
- Split & clear up motivation section
- Add examples in various places
- Add P&X comparison & introduction
- Actual Evaluation
- Give a nicer introduction to qualified schemes
- Note the limmitation of the semantic (lazyness and recursiveness)


## Quick Fixes
- Sefaty theorem doent mention ↯
- Fix the usage of the letters in unfity (s)
- Do proper math prose (theorem and lemma blocks, numbering, )
- Remove every mention of minimal calculus


## Keller
- Prove: "a typing that uses neither T-sel-⊥ nor T-sel-★ (or an inference run with no warnings) never reaches ↯"


# Section Content
## Introduction
- [ ] We are motivated by NixLang itself
- [ ] It revolves largely around records (3 examples)
- [ ] That is why we want to solve *asymmetric concat*
- [ ] Our path: We use *scoped records* & *unknown type*
- [ ] The unknown type is motivated by the *untypability*, similar to typescript
- [ ] This is a *natural choice*
- [ ] It oftenly uses open record concatenation (unhandled in many cases)
- [ ] The motivating example of our work is the `//` operator, it can be used in nix this way (•)


# Section Fixes
- Declarative
  - [ ] Stump result δ a variable in Q — Lean now `Stump.res : Ty B`
  - [ ] Lookup properties: drop unclear "no side condition" in totality
  - [ ] Row equivalence: reword "rather than a side problem"
  - [ ] Spine: add example (uncommon technique)
  - [ ] T-sel-★ / T-sel-⊥: expand, non-standard
  - [ ] Instantiation: too verbose; sorting para removable
  - Row lookup:
    - [ ] `selDyn` not covered
- Refinement
  - [ ] ⊑ defined inline — move to declarative
- Unification
  - [ ] Fuel sentence: reword or drop
  - [ ] Rules not tangible — examples
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
