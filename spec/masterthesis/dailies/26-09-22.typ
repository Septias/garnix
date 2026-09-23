
./26-09-19.typ
./26-09-23.typ

== Fragen
- What ist the projClash test?
  - projection clash case of the algorithm
- What are appDeg and selDeg?
  - Two degrade rules for selection and application
  - Needed for when one part of the expression degrades to ★ (such that we can continue)
- Why is inhabitation such a problem?
- Warum muss der Context acyclic sein?
  - Für termination of lookup
- Ist RowWF eine limitation für den gesamten calculus?
  - Müsste man eigentlich durch rekursive Typen lösen?
- What is B1?
  - Change that introduced depGraph
- What ist he quiscient state?
  - Stumps actually block on their variables
- Was genau macht with; und inherit;
  - With führt dazu, dass closedness nicht gegeben ist
  - Closedness aber auch schon wegen lazyness nicht da

== Claude Prompts
- [x] The U-expand arm has increased the complexity of our system quite a bit since variable invention implies possible circles, needs renaming and a move away from only-local arms. I think this has immediate consequences for termination so I'm thinking is it wort it? We know that our algorithm (if we want to fix it terminating) is incomplete anyways, so we could just draw the line by U-expand. What is your read?
  - Both paths are reasonable, U-expand might pay for itself by allowing ‖

== Architecturally
- Renaming now has DepGraph to keep track of changes. Can this be optimized?
- Spines don't distinguish sorts

== Claude Output
That line is the U-expand staleness problem: the host gets renamed on the spine while a payload mentioning it still reads the old name. DepGraph exists to compensate.
