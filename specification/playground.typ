== 1. Structure
- What is nix? Why is it relevant?
  - [x] Package Manager
- Features of nix / inner workings
  - [x] Reproducibility
  - [x] Upgradability
  - [x] Isolation
  - [x] Rollbacks
  - [x] Garbage Collection
- Overcomming Problems
  - [x] Dependency tracking
- Nix features → Language features
  - [x] Special Constructs
  - [ ] Special primitives (path)
  - [ ] Dynamic Access ({})
  - [x] Meta-properties


== Origin of the Nix Language
// moved

== 2. Nix and its Quirks
- Termination
- Recursion
- Shadowing
- Patterns


== 3. Finding a type system

The literatur on type systems is as wide as \_ with many different typesystems studied over the last 70 years of research. Finding a typesystem for a language is thus similar to traversing a jungle with the alluring dangers of getting sidetracked. Since records are such a central aspect of the language, starting from them is not a bad thing.

=== 3.1 Record theory
// moved

#page[
  #bibliography(("bib/misc.bib", "bib/parreaux.bib", "bib/nix.bib"))
]
