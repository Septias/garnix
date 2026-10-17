

## Misc
Comment-Pattern:  \[[^\]]*\]\([^)]*\)


## General
The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good.


## Honest Contribution
- Lookup is seperate and unification stays plain first-order
- Deterministic lookup instead of entailment relation
- Trace monoid representation
- Mechanization


## Claude Prompts
- [ ] Go over the comments in <section> of the form […](…) and //…. For each of them, on by one, propose a fix.
- [ ] Go over the references and verify every claim against other papers by dowloading the source into plaintext/ and checking it. 
- [ ] Verify every example given in the thesis by running it in lean. The named rules and sub-goals that are shown  should be checked this way.


## Sessions
- Add a section that admits our eliminator-problem and forces gradual typing
  - this is partly done in the end
- The narrative for T-sel-⊥ has to be made more convincing
- Maths formulism is hand-wavey at times I feel like
- Go over all examples and run them (mechanically?)


## Fragen
- where is [arrow]!* defined?


## Todos
- We mention too often, that we can drop the condition of P&X 1-2 times is enough
- The names of Algorithmic rules A-* should be similar to the declarative
- Better Syntax for key comparison


# Read
- [x] Algorithmisch
  - [x] Soundness / Incompleteness
  - [x] Termination
  - [~] Principality

- [ ] Schreiben
  - [~] Introduction
  - [~] Overview
  - [x] Declarative
    - [x] Syntax
    - [x] Lookup ↓
    - [x] ≈-relation
    - [x] Types
    - [x] Sorts
    - [x] Typing
    - [x] Instantiation
    - [x] Semantics
    - [x] ⊑-relation
  - [x] Metatheory
  - [x] Unification
    - [x] Spines
    - [x] Types ≐
    - [x] Rows ≐ᵣ
  - [x] Inference
    - [x] State
  - [x] Incompleteness
  - [x] Towards Nix
    - [x] FC-Labels
  - [x] Revisited: Introduction
  - [x] Related Work
  - [x] Conclusion

