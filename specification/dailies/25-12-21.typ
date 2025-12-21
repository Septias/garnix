== To: Syntax
Nix allows path accesses in plenty situations:


=== Examples
```nix
let rokko = { a.b.c = "hi"; };;
let has_attr = {a.b.c = "hi"} ? a.b.c;; # "hi"
let def = {}.a.b or "hi";               # "hi"

```

Nix also allows to index records by literal strings

```nix
let bruh = {a.b.c = "hi"}."hi";

```

== To: Context Strings
Context strings can be used in plenty of situations:

1. Record access
2. Record definitions (`{ ${null} = true; }`)
3. Inside of strings
4. Inside of paths `~/${"hi"}`


== To: Type Rules
When typing a function, quite a few things have to happen:

1. All record values have to get fresh type variables
2. The global binding has to be a fresh typevariable
  - This typevar has to be "connected" to the other typevars


== To: With Statements
This is a very tricky nix example:

```nix
a: b: (with a; with b; b.a)
```


== To: Deep/Shallow Execution
Using a relation following the operator application is kind of weird. I think it works out, but there should be an easier solution maybe? Kontexs are also very verbose.

Things that actually need to be deep:
1. Equality operations (if not short-circuting)
2. deepSeq
3. ?
