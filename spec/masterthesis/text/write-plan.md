# Organization

## General Framing
- Framing as "I have these constraints, this is the best solution, this is the result"
- Make it more about what we found out *due to proving*


## Contributions
- The unification algorthim
- Rows are trace monoids, actually
  - The single strict improvement 


## Limitations
- We currently need _closedness_ in the proofs, which does not hold due to `with; e`
- There is no negative information in our typesystem.


# Structure
## Introduction
- [x] We are motivated by the nix language itself
- [x] Nix is the largest body of untyped functional code
- [x] It resolves largely around records (3 examples)
- [x] That is why we want to solve *asymmetric concat*
- [x] Our path: We use *scoped records* & *unknown type*
- [!!] This positions us uniquely among other typesystems
  - Fucking boring lule
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


## Algorithmisch
- Unification Regeln
- Sortedness
- Inference rules
- State must be a closure
- Blockers and constraint resolution


## Was muss ich alles fürs algorithmische System zeigen?
- Inference rules


