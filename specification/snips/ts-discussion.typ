#import "../functions.typ": *

== Unholy, Untypable
`a: b: with a; with b; x`

An idea I had for this one is, that you keep the constraint lying around, that any of the supplied arguments needs to have the argument. Now the one important thing is to see, if this approach is actually applicable during type inference and whether this information leaks to wide and can not be resolved anymore…

One good thing is, that type inference is always as complete as an evaluator could be. We might get non-termination from it, but that is generally not too bad maybe?


== Inspection
There are a few functions that allow the program logic to take into account the actual value at hand. For example te check-operator. This one is well-behaved, so why am I scared of `attrNames`. This gives back alle the keys defined on a record. Since we anyways track explicit records, that should be no problem?

`(f: attrNames f): {} -> {}`
`(b: with attrNames f; b): {} \ {}`


=== Problematic Children

#bib
\*https://sourcegraph.com/search?q=context:global+%24%7Bbuiltins.currentSystem%7D&patternType=keyword&sm=0
