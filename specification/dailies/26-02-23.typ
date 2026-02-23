
== To: Description
- My problem is, it now sounds to unrelated to nix due to the "academic" tone.
- Previous texts sound to casual
- New text lack a direction and are overly vorbose and to fancy
- The perfect thing would be a mixture of the two actually...
- But it is also not tooooooo important lule (I have the time!!!)


== To: General structure (what do I want to say)
- Intro (General): Nix usage, Nix module system, General direction
- Properties: Features wanted & needed
  - Structural: Lazyness & Graduality
    - Gradual TS: (1-move switch not working)
    - Gradual TS: We need an unknown type
  - Needed: Full Record-calculus
  - Wanted: Occurrence, Type conns, Full type inference
- Occurrence: Case for Gradual, Simplified to Occ (because of applicability)
-

Actually: Do we want to add typeannos?
- We might actually have to put them in (not clear at the moment!)


== Writing Style improvements:
- Sentences don't follow the flow you use when speaking
- No exaggeration, no personal feelings
- "We want this becauso of that"
- "Think of writing a specification"
- Consequence: "X has property P because of mechanism M."
- Contrast: Unlike X, Y exhibits property P.
- Constraint Structure: X is possible only if condition C holds.
- Scope limitation: In this work, X is restricted to Y
- Questions:
  - What property am I claiming?
  - What mechanism explains it?
  - What consequence follows?
  - What constraint limits it?
  - What breaks if this feature did not exist?
  - What invariant would fail?
  - What property does this design enforce?
  - What trade-off is introduced?
- Structure
  - A problem or constraint
  - A mechanism
  - A consequence
- Rheme, Nominalized property, Not actor-centric, clause density, causal encoding

== TS brainstorming
- Functions with recursive patterns:
  - Just refer to type variables
  - A shared rust variable will keep them alive
  - Do you need to copy the variables? (similar technique as in superF)
    - Depends on the
