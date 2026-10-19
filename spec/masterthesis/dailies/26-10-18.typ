
./26-10-17.typ
./26-10-19.typ


== Fäden
- Thesis auf-und-ab lesen
- Incompleteness reduzieren
- Features ausweiten


== Große Fragen
- Wie machen wir ★?
  - Bleibt für totality
- Sind wir mit T-sel-⊥ zufrieden?
  - Bleibt für lazyness
- Ist counting sinnvoll?
  - Jap


== Todo
- Can we reduce the usage of ★?
  - At the moment it is used to mask certain uncertainty and things we could flag


== Claude Prompts
- [x] The counting arguments for my thesis are dependent on ≈, can you give a better relation, that ignores shadowed labels? Does that decrease or increase the power?
  - Stronger relatation kills trace monoid apparently.
- [x] Explain the rules of T-sel-dyn-𝓫 and T-rcd-dyn-𝓫
  - could 𝓫-labels rejected hard?
  - Same reasoning as T-sel-⊥
- [x] What is the reason we have to admit the ★ with T-sel-⊥. Is it soft-typing?
  - Lazyness, but this is only a weak argument imo
- [x] What exactly is the exercise of ★ in our calculus?
  - Captures a lot of error states
  - Mainly to keep totality
- [x] Should I allow top-level schemes? What are the design implications?
  - Complicates a lot of statements
- [x] Backup the claim in 00_introduction.typ:52
  - 5.600 occurrences of it


== Depth
- [x] Understand the counting argument behind u-host
- [x] Explain the usage of Err and ★
- [x] Was passiert wenn beide Seiten mehrmals das selbe Label haben?
  - Alle müssen matchen :(. Ansonsten bräuchte man negative types für principality
