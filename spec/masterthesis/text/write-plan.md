# Organization

## General
The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good.


## Honest Contribution
- Lookup is seperate and unification stays plain first-order
- Deterministic lookup instead of entailment relation
- Trace monoid representation
- Mechanization


## Sessions
- Split & clear up motivation section
- Give a nicer introduction to qualified schemes
- Note the limmitation of the semantic (lazyness and recursiveness)


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
  - [ ] Instantiation: sorting para removable
- Incompleteness
  - [ ] Irreducible: re-check last paragraph
- Extensions
  - [ ] Stub subsections only
