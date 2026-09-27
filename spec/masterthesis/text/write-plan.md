# Organization
The current


## Limitations
- We currently need _closedness_ in the proofs, which does not hold due to `with; e`
- There is no negative information in our typesystem.


# Structure
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
