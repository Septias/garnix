

# Style
Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good. Comments are left in the form `//` before a paragraph or inline using this syntax: [text](comment). 


## Rules
- Put the subject in front
  - No extraordinary prose
- Repeat the noune
  - Open with a summary noun
- Short sentences, Theme-rheme order
  - Claim, colon, evidence
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
- Every label must count: Forced by ≈?
- Currently no eliminators: Deffered to further work
- T-sel-⊥: remedied by the reason for lazyness
- Incompleteness:
  - Wand: unsolvable
  - Let: Not practical to solve
  - Inference:
    - Generalization at lets
    - Unrestricted T-★-intro: ?
  - Fc-labels: would need top-level schemes


## Automated checks
- [ ] You are a professor in PL, go over the thesis text and critique it's claims and wording based on your expertise.
- [ ] Go over the thesis text and mark every place where wording is loose in a sense, that the terminalogy is not suited for the claim.


## Admitting
- Gradual typing für usability 
- Use of the wrong semantic for row equality


## Final Cleanup
- Check the syntax whether it is ambiguous
- Check the claims for references


# Section-State
- [x] 00 Introduction
- [x] 01 Overview
- [ ] 02 Declarative
  - [ ] Introduction: Stumps
  - [x] Syntax
  - [x] Typing
  - [x] Lookup ↓
  - [x] Row equivalence
  - [x] Sorts
  - [x] Instantiation
  - [x] Semantics
- [x] 03 Metatheory
- [ ] 04 Unification
  - [x] Types ≐
  - [ ] Trace Monoid
  - [x] Rows ≐ᵣ
  - [ ] Examples
  - [ ] Metatheory
- [ ] 05 Inference
  - [ ] State
  - [ ] Rules
  - [ ] Let-Generalization
  - [ ] Wake, Replay, Saturate
  - [ ] Finalization, Materialization
- [ ] 06 Incompleteness
  - [ ] Unsolvable
  - [ ] Forced Moves
  - [ ] Inference
- [ ] 07 FC-Labels
  - [ ] Keyed Lookup
  - [ ] Row Equivalence & Spines
  - [ ] Typing
  - [ ] Unification
  - [ ] Inference 
- [x] 08 Mechanization
- [ ] 09 Discussion
  - [ ] Idioms
  - [ ] Design Goals
  - [ ] Outlook
- [ ] 10 Related Work
  - [ ] Old Work
  - [ ] FC-Labels
  - [ ] Expressive Row Theories
  - [ ] Subtyping Based
  - [ ] Set Theoretic
  - [ ] Gradual and Soft
  - [ ] NixLang
- [ ] 11 Conclusion
- [ ] 12 Appendix


## Session Times

- [ ] Related Work (1 Day)
- [ ] Integrate Feedback (1 Day)
- [ ] Final Cleanup (Day)
