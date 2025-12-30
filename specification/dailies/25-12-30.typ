
== Notation
- Records are split into recursive and non-recursive part?
  - Motivated by inherit
  - Problematic example `{inherit x;} -> {x = x;}`

== To: Introduction
This document should lay the foundation of the nix operational semantic. It takes inspiration from @valid but tries to be more thorough, elaborating on the niche features that are usally left out for simplicity. The additional features include all operators and builtins, a full specification of paths and dynamic accesses aswell as function patterns in their full strength.

== Places for dyn
Dyn can be placed wherever a label could be used and also freely in strings and paths.

== Places for paths
Paths can be used in the check operator `? a.b.c` and for record definition `{a.b.c = t}`.

== Patterns
- All Varibles can be referenced in free order so the need to be added together upfront.
