

# Rules
When writing thesis text, use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good. Comments are left in the form `//` before a paragraph or inline using this syntax: [text](comment). You can fix citation and reference errors without my intervening.

The development of MacroNix is just for test purposes, don't mention it too often.


## Style
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
  - Forced Moves: Good reasons
  - Inference: Good reasons
    - Generalization at lets
    - Unrestricted T-★-intro


## Automated checks
- [ ] You are a professor in PL, go over the thesis text and critique it's claims and wording based on your expertise.
- [ ] Go over the thesis text and mark every place where wording is loose in a sense, that the terminalogy is not suited for the claim.


## Admitting
- Gradual typing for usability 
- Use of the wrong semantic for row equality


## Final Cleanup
- Check the syntax whether it is ambiguous
- Check the claims for references


# Section-State
- [f] 00 Introduction
- [f] 01 Overview
- [f] 02 Declarative
  - [f] Syntax
  - [f] Typing
  - [f] Lookup ↓
  - [f] Row equivalence
  - [f] Sorts
  - [f] Instantiation
  - [ ] Semantics
- [f] 03 Metatheory
- [f] 04 Unification
  - [f] Trace Monoid
  - [f] Types ≐
  - [f] Rows ≐ᵣ
  - [f] Examples
  - [f] Metatheory
- [ ] 05 Inference
  - [ ] State
  - [ ] Rules
  - [ ] Let-Generalization
  - [ ] Wake, Replay, Saturate
  - [ ] Merge

**Done on DI**

- [x] 06 Incompleteness
  - [x] Unsolvable
  - [x] Forced Moves
  - [x] Inference
- [ ] 07 FC-Labels
  - [ ] Keyed Lookup
  - [ ] Row Equivalence & Barriers
  - [ ] Typing and Evaluation
  - [ ] Unification
  - [ ] Inference
  - [ ] Costs
- [x] 08 Mechanization
- [ ] 09 Discussion
  - [x] Idioms
  - [x] Design Goals
  - [~] Outlook
  - [~] 11 Conclusion
- [ ] 10 Related Work
  - [ ] Old Work
  - [ ] FC-Labels
  - [ ] Expressive Row Theories
  - [ ] Subtyping Based
  - [ ] Set Theoretic
  - [x] Gradual and Soft
  - [x] NixLang
- [ ] 12 Appendix


## Zeitplan
- FC-labels morgen lesen, grobe Schitzer finden (So)
- Danach Declarative, Unification und Inferenz finalisieren (So + Mo)
- FC-Labels finalisieren (Di)
- Mit Thiemann quatschen (Mi)
