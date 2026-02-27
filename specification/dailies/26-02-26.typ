

== Places of Interpol

Attr-bindings: `{${t} = l }`
Attr-lookups : `{a = 2}.${t}`
Paths: `a.b.${}`
Strings: `"${}"`

Basically, when and how do I need reduction rules. vs. context?

The context chooses the next reductum in a syntax tree. The tree is already given.
We then need for every piece that can be chosen by the environment a reduction rule.

- How do we (unwantedly) skip computation?
  - Basically by not represeneting the syntax tree correctly
- Do I need reduction rules for paths?
  - No, operators handle this
- When do I actually need context rules?
  - To decide where to reduce, the last case must be handled by a reduction rule
- Basically I need to find for every syntax construct that has multiple sub-terms the order????
  - That sounds fucking right
- If there is no ambiguity, the context does not need to return anything?
  - Yeah, the reduction rules take over

== To: Records
- What is a fuggin open record?
