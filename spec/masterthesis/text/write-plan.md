

## Misc
Comment-Pattern:  \[[^\]]*\]\([^)]*\)


## General
The style should be similar to the motivation and abstract so I feel comfortable with it. Use academic style. It can sound fancy from time to time, but needs to keep a monotonous tone. We are presenting results and not a sales pitch. Could use some more mathematical slang, even though it should be well understandable. The rule descriptions are generally good.

### Register per section
- Introduction, related work, discussion: Castagna (argumentative, low-key, logically connected).
- Overview: Parreaux's structure (one running example generalized step by step, strawman refuted by counterexample), without his lively register.
- Declarative, explanations of ★ and lookup: HoTT book / Pierce (correct the HM intuition: "not merely ... but rather ...").
- Metatheory, unification, inference: plain definition-theorem prose.
- Avoid funnel openings with general truths and inflated adjectives (compelling, significant, "This is where ...").

### Sentence rules
1. Subject first. Front an element only for a given-new link or a real contrast (German Vorfeld habit).
2. Open paragraphs with a summary noun: "The problem with ...", "The consequence of ...", "The solution we propose ...".
3. Claim, colon, evidence.
4. Only logical connectives (however, hence, in particular, instead, nevertheless). No narrative scaffolding ("To adjust to this situation", "we turn to", "we continue by").
5. Verbs over nominalizations ("for configuring software", not "for the configuration of software").
6. Fixed terminology: define once with "that is,", then never vary. Open compounds (type scheme, row variable); hyphen only when attributive (type-variable substitution).
7. Evaluate through consequences, not adjectives.
8. "This" + noun, never a bare "this".
9. Refer to design goals by name where they justify a decision ("by G3, ...").

### Checks per section
- First-five-words test: the openings of all sentences in a paragraph must tell a coherent story.
- German-interference grep: `, that` `\d\.\d{3}` `Nix'` `can not` `-example|-scheme` `maybe` `has to` `semantic\b` `programme`
- Read aloud.


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
- [x] Introduction
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

