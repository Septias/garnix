Next: ./26-02-13.typ

../snips/feats.typ
== Reasons for Features:
- FC-Labels: `{a = 2;}.${a}`
- Lazyness: Big structure
- Gradual: Big structure
- Record extension: `a // b`
- Reflection: `isBool`
- Intensional poly: reflection
- Ad-hoc poly: operators, reflection


== Records: History
- A record calculus based on symmetric concatenation (1990)
- Typing record concatenation for free (1992)
- A polymorphic type system for extensible records and variants (1996)
- First class labels for extensible rows (2004)
- Extensible records with scoped labels (2005)
- Infix extensible records for Tubalar data (2023)


== Records: Operations
```
select :: ∀ r a b: (r\l) => { l: a | r }  -> b -> a
select r  l  -> r.l

(_._) :: ∀ rab: (r\l) => { l: a | r} -> Lab l -> a

Function to extract labels
attrKeys: {l : a} ->  [l]
```

== Gradual: No?
- Where do I actually need gradual?
- Basically I want an unkown type for builtins, no?
- Yeah, the `fromJSON` needs it because it returs valid nix
- But: There is no invalid

All (Flow, Gradual, Occurrence) work by refining types.
- Gradual uses casts (implict is also fine no?)


== Typing the Builtins
- Some of them can be implemented in the language?



== Records
Record presentations:
- { a = t₁ } ∧ { b = t₂ } ← Normal subtyping already covers this
- { a = t₁, b = t₂; }
- { a = t₁, b = t₂; | r } ← Row subtyping
- { a = 1; b = false;}    ← Double labels



== Out of scope
- Every number is a type?
- No variants needed
