

# Style
The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good.

## Rules
- Put the subject in front
  - No extraordinary prose
- Repeat the noune
  - Open with a summary noun
- Short sentences, Theme-rheme order
  - Claum, colon, evidence
- Verbs over nominalization
- "that" takes no comma
- noun-noun is open: noune noune
- No narrative scaffolding
- Fixed focabulary


# Workplace
## Honest Contribution
- Lookup is seperate and unification stays plain first-order
- Deterministic lookup instead of entailment relation
- Trace monoid representation
- Mechanization


## Problems
- T-sel-⊥: remedied by the reason for lazyness
  - removeable?
- Every label must count: Forced by ≈?
- Currently no eliminators: Deffered to further work
- Incompletenss:
  - Wand: unsolvable
  - Let: Not practical to solve
  - Inference: What exactly…?
  - Fc-labels: would neet top-level schemes
- No subtyping: This is anyways hard!


## Claude Prompts
- [ ] Go over the comments in <section> of the form […](…) and //…. For each of them, on by one, propose a fix. Do not edit the text.
- [ ] Go over the references and verify every claim against other papers by dowloading the source into plaintext/ and checking it. 


## Clearup
- We have closed terms
- No top-level schemes
- The use of our trace-monoid


## Sessions
- Add a section that admits our eliminator-problem and forces gradual typing
- Explain why we force same number of l-labels in both sides
- Clearup the usage of warnings


## Todos
- Better Syntax for key comparison
- Align order of rules in text and figures
- Maybe add a section that shows the order (solve, saturate, wake, materialize, finalize)


## Fragen
- Maybe make Precision a headline definition
- Where should I define the spine? Once in the beginning? I now have multiple definitions…
  - also define the window


# Section-State
- [f] 00_Introduction
- [f] 01_Overview
- [x] 02_Declarative (22.10)
  - [f] Syntax
  - [f] Lookup ↓
  - [f] Row equivalence
  - [f] Trace Monoid
  - [f] Sorts
  - [f] Typing
  - [x] ⊑-relation
  - [x] Instantiation
  - [x] Semantics
- [x] Metatheory
- [x] Unification
  - [f] Prelims
  - [f] Types ≐
  - [ ] Rows ≐ᵣ
  - [ ] Examples
  - [f] Metatheory
- [ ] Inference (25.10)
  - [ ] State
- [ ] Incompleteness (26.10)
- [ ] FC-Labels (27.10)
- [ ] Discussion (28.10)
- [ ] Related Work (29.10)
- [ ] Conclusion (29.10)



## Misc
Comment-Pattern:  \[[^\]]*\]\([^)]*\)
