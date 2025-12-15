

== 25.12.15
- new fresh variables for every record field


== To: Inference
- Fresh variables for all the record fields
- Fresh variable  for optional glob-binding
- Add glob-binding to context in a way that keeps references


== To: Subtyping
- The subtype relation must be extended by patterns
- These patterns have to be related to record fields


== Rewrites
- let-in to let-attribute
- Path accesses to ?
- Path definition to ?


== Aligning Syntax
- Add matching
- Rewrite records to rec/non-rec
- Remove deep/shallow => prabably yes...
- Add delayed substitution?
