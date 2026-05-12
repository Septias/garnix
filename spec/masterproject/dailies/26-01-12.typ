== Records
Since records are such an integral part of the language, best start with describing them. Records in nix follow the standart scheme, in that they are a collection of key-value bindings. They are immutable by default but there exists a right-prioritizing concatenation operation to "extend" records. The standart lookup operaton `r.l` returns the value associated with the label l and and raises an error otherwise. To circumvent


== Quirks to Discuss
- With
- Inherit
- Lazyness in 4 places
- Recursion in 3 places
- Dynamic lookups
