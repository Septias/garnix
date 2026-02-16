
Needed: Recursive Types, Laziness, Subtype Poly, Parametric Poly, Ad-hoc poly, FC-labels, Record Concat
Wanted: Occurrence Types, Connectives


- Record concatenation
- First class labels
- Recursive Types,
- Laziness
- Efficient computation,
- Subtype Polymorphism
- Parametric Polymorphism
- Adhoc Polymorphism
- Reflection,
- Gradual typing (weak)
- Occurrence typing
- Type connectives


Another important consideration for a typesystem is _expressiveness_. It is obviously possible to type every variable at an _unknown type_ $star.op$ but that would not give meaningful insight for the user. On the other hand, making a typesystem to complex might lead to unwanted properties like undecidability or non-termination @undecidable. Again, the proper path strikes the balance between expressiveness and simplicity.

Most general purpose typesystem come equipped with some form of polymorphism, to abstract over generic program behaviour. Due to its usefulness, polymorphsim is one if not the most researched topic with a myrriad of different kinds:  _parametric polymorphism_, _first-class polymorphism_, _subtyping polymorphism_, _Ad-hoc polymorphsim_, _Presence polymorphism_, _Explicit Polymorphism_, _Implicit Polymorphsim_ just to name a few. It is not immediately obvious which types of polymorphism is the right for your type system but we can conclude from the language.

It is obvious that parametric polymorphism is needed because nix features let-bindings. Last but not least, _subtype polymorphism_ is a common technique that has proven useful especially in conjunction with _type-connectives_. Type connectives are borrowed from logic connect otherwise unrelated types using unions, intersection and negation. They are especially useful in conjunction with _flow-respective typing_, a technique used in flow to narrow types in conditionals.

The nix language furthermore allows to reflect over its types using the builtin (isBool, isAttr, etc.) functions so a reflexive type system is needed. Nix also allows to compute record labels and such labels need to be _first class_ in the language. Last but not least, nix is a lazy and recursive language with hard-to-track shadowing semantics. Due to recursiveness in records, let-bindings, and patterns, recursive types are a must in the language.
