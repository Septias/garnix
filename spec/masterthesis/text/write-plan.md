

# Style
Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good.


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
- T-sel-⊥: remedied by the reason for lazyness
- Every label must count: Forced by ≈?
- Currently no eliminators: Deffered to further work
- Incompleteness:
  - Wand: unsolvable
  - Let: Not practical to solve
  - Inference: What exactly…?
  - Fc-labels: would neet top-level schemes


## Automated checks
- [ ] You are a professor in PL, go over the thesis text and critique it's claims and wording based on your expertise.
- [ ] Go over the thesis text and mark every place where wording is loose in a sense, that the terminalogy is not suited for the claim.
- I have to check whether the syntax is ambiguous or bad in all places (representation is not verified)


## Clearup
- We have closed terms
- No top-level schemes
- The use of the trace-monoid


## Sessions
- Add a section that admits our eliminator-problem and forces gradual typing
- Explain quiesence better


## Todos
- Better Syntax for key comparison
- Align order of rules in text and figures
- Maybe add a section that shows the order (solve, saturate, wake, materialize, finalize)


## Fragen
- Where should I define the spine? Once in the beginning? I now have multiple definitions…
  - Also define the window


# Section-State
- [f] 00_Introduction
  - Fix the claims
- [f] 01_Overview
  - Fix the claims
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
    - Explain the semantic
  - [ ] Examples
  - [f] Metatheory
- [~] Inference
  - Needs introduction to different solver states
  - [~] State
- [f] Mechanization
- [ ] Incompleteness (27.10)
- [x] FC-Labels
  - Good in general
  - Boring sections need 
- [x] Discussion (28.10)
  - I say is good from content
- [x] Related Work (28.10)
  - Needs precision in first part
  - Others are good I beg 
  - More focus on 
- [ ] Conclusion (28.10)


## Misc
Comment-Pattern:  \[[^\]]*\]\([^)]*\)
