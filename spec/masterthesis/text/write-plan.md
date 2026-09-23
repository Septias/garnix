
## Fragen
- Should I write some comparison into the introductory part?
  - It motivates the work in a sense, but I think a clean introduction of *our contribution* is better
  - Do we have enough content for it?
  - We can also add references in retrospective
- What is currently best style?


## Things
- Introduction: Should motivate the reader
  - Motivation (Nix)
  - Reasoning (Features)
  - Differences (Other TSs)
    - Positioning


## Introduction
// Problem: I want a catchy introduction and it feels like this should go into the weeds directly instead of the usual banter 
// - Maybe only with … ?

- We are motivated by the nix language itself
- Nix is the largest body of untyped functional code
- It resolves largely around records (3 examples)
- That is why we want to solve *asymmetric concat*
- Our path: We use *scoped records* & *unknown type*
- This positions us uniquely among other typesystems
  - This way, existing code is typable and does not break
  - The shortcommings are explicitly marked by ★
- The unknown type is motivated by the *untypability*, similar to typescript
- This is a *natural choice*
- It oftenly uses open record concatenation (unhandled in many cases)
- The motivating example of our work is the `//` operator, it can be used in nix this way (•)
- We need to add the ★ type (because of )
- We can not use *instrumentation* because 


## Declarative
- L2 vorstellen
- Warum brauchen wir T-sel-★ und T-★-intro
- Motivation für qualified types?
- Trace monoid
- The winning example
- Principality

## Algorithmisch
- Unification Regeln
- Sortedness
- Inference rules
- State must be a closure
- Blockers and constraint resolution


## Was muss ich alles fürs algorithmische System zeigen?
- Inference rules


