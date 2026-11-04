

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


## Admitting
- Gradual typing for usability
- Use of the wrong semantic for row equality


## Final Cleanup
- [ ] You are a professor in PL, go over the thesis text and critique it's claims and wording based on your expertise.
- [ ] Check all claims made in related work against the paper sources in plaintext/
- [ ] Check rigidness of presentation


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
  - [f] Semantics
- [f] 03 Metatheory
- [f] 04 Unification
  - [f] Trace Monoid
  - [f] Types ≐
  - [f] Rows ≐ᵣ
  - [f] Examples
  - [f] Metatheory
- [f] 05 Inference
  - [f] State
  - [f] Wakeup and Saturation
  - [f] Inference
  - [f] Let-Generalization
  - [f] Entry Point
  - [f] Metatheory
- [f] 06 Incompleteness
  - [f] Unsolvable
  - [f] Forced Moves
  - [f] Inference
- [f] 07 FC-Labels
  - [f] Introduction
  - [f] Syntax
  - [f] Examples
  - [f] Metatheory
- [f] 08 Mechanization
- [f] 09 Discussion
  - [f] Idioms
  - [f] Design Goals
  - [ ] Outlook
  - [f] Conclusion
- [f] 10 Related Work
  - [f] Old Work
  - [f] FC-Labels
  - [f] Expressive Row Theories
  - [f] Subtyping Based
  - [f] Set Theoretic
  - [f] Gradual and Soft
  - [f] NixLang
- [f] 12 Appendix
  - [f] Row Unification
  - [f] Replay
  - [f] FC Labels
  - [f] MacroNix
