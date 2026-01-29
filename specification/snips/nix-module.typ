#import "../functions.typ": *


== The Nix Type System
Nix does bring some weird typesystem to the table.

It is a DSL inside the module system, that does type-checking during evaluation. This helps during module system configuration, because on build, you can see what might go wrong instead of in practice due to experiments or no at all. The typesystem provides the basic types and a combination thereof. It does not bring type connectives to the table I can imagine.

