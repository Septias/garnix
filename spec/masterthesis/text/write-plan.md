

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


## Sessions
- Add a section that admits our eliminator-problem and forces gradual typing
  - this is partly done in the end
- The narrative for T-sel-⊥ has to be made more convincing
- Explain why we force same number of l-labels in both sides

## Todos
- We mention too often, that we can drop the condition of P&X 1-2 times is enough
- Better Syntax for key comparison
- Note somewhere, that we only argue under closed terms. Probably in the metatheory chapter.

# Section-State
- [ ] Introduction
- [ ] Overview
- [ ] Declarative
  - [ ] Syntax
  - [ ] Lookup ↓
  - [ ] ≈-relation
  - [ ] Types
  - [ ] Sorts
  - [ ] Typing
  - [ ] Instantiation
  - [ ] Semantics
  - [ ] ⊑-relation
- [ ] Metatheory
- [ ] Unification
  - [ ] Spines
  - [ ] Types ≐
  - [ ] Rows ≐ᵣ
- [ ] Inference
  - [ ] State
- [ ] Incompleteness
- [ ] Towards Nix
  - [ ] FC-Labels
- [ ] Revisited: Introduction
- [ ] Related Work
- [ ] Conclusion

