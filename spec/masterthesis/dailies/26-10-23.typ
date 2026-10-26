

./26-10-25.typ


== Fragen
- Warum ist ≐ wrapped in scripts()
  - Needed because attachements would go next to symbol otherwise
- Was genau ist die shared-tail condition
  - the unification rules that guesses a field into a row
- Warum trace monoid
  - Normale struktur von der ≈ relation, gibt cancellative and … ?
  - Replace shared tail
- What is the (on a) notation for stumps?
  - Just report where it blocked. It should not appear out of the blue
- How are spent stumps actually implemented?
  - The substitution just records it
  - materialize and finalize "consume" it
- Take out T-sel-⊥?
  - Needed for lazyness
- Should we remove the warnings?
  - It mostly is a taste question
  - Remove it from figures to  reduce noise, just mention it
- Why is quiscent important?
  - So the proper variables get generalized and lets stay monomorphic


== Todo
- [x] Use ⊳ instead of ▷
