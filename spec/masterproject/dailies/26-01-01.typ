

== To: Rec Annotations
Basically they are not needed, because we can just add a specific rule with a guard for `inhirit`.
That is anyways a bit nicer.

== To: Evaluation
Deep and shallow evaluation is not a topic of our interest, so we can just remove it from our typing rules.

== To: Operators
The evaluation to some set is a bit weird. Shouldn't we be able to just reduce the first operand to a value and only continue if it fits the operator type? Why do we need this weird relation for that? Basically we don't have a type in the evaluation rule... but we can maybe still use str ∨ path?

== To: Matching
Matching just looks at one field after the other, this sub-routine is not especially handy for the usecase and there might be better options to do it. I might have to give it some thought…


