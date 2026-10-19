#import "../functions.typ": *

#let meta = [
  1. Motivate thesis by nix usage
  2. Into: State our goals
  3. Motivation of the ★-type
  4. Second motivation: _asymmetric concat_
  5. Contributions
  6. Overview
]

= Introduction <sec-motivation>
The Nix programming language @nix-language-2-28 @dolstra_phd, we dub NixLang, is a pure, functional, and untyped language for the configuration of software. It is the expression language for the Nix package manager and operating system, where attribute sets, or records in programming language literature, are used to configure packages and programs using key-value pairs. The Nix package manager hosts the most up-to-date package repository @repology, with over 100.000 packages, and the operating system's guarantees of reproducibility and reliability @dolstra_phd @nixos_long are the reason for its [growing user base](source?). All packages, and the entire operating system, live in the public nixpkgs repositories, spanning over four million lines of code#footnote[Counted over the nix-files of nixpkgs 26.05], which sees daily changes to packages and configurations, making it one of the ten busiest repositories on GitHub @octoverse2024. Despite its age and popularity, NixLang has gone without a type system for over twenty years @nix-ts-issue, and its formal treatment is limited to a recent semantic of Krebbers&Broekhoff @verified and work by this author @simplenix. In this thesis, we turn to the issue of type inference again.

The attribute set in Nix, or record in programming language theory, forms the basis for configurations as a natural combination of keys and values that declaratively describe the shape of a package or operating system. For example, the name and version of a package or the allowed list of IPs for a firewall. At build time, the  configuration files constituted of these attribute sets are collected, overwritten, and merged using Nix' language constructs and the built-in functions. Of the features needed to do so, asymmetric concatenation and first-class labels have proven hard to faithfully type on their own and in combination, with only a small body of work on each @concat4multiinher @concat4free @fc_labels, and only [P&X] that combines both in a sound calculus.

The goal of our work is to aid programmers and tools in writing safe and reliable code for the existing body of code today. In scope, we do not try to only infer package information or create a new, simplified configuration language that compiles to nix @nickel. From this real-world requirement, we derive the following four design goals:

/ G1 (No source changes): Type inference should run on today's nixpkgs without source changes because a repository of nixpkgs' size is impractical to annotate or restructure for the sake of an analysis.
/ G2 (No semantic change): Nix depends heavily on laziness to keep its performance. Any change to evaluation order, for instance through casts or dynamic checks, can change the output hashes of packages and their build results, and is therefore costly.
/ G3 (Totality): To run on today's nixpkgs, the analysis must give a type to every valid piece of syntax. In cases where type inference is ambiguous, the algorithm should report uncertainty instead of failing.
/ G4 (Effective inference): All packages and the operating system root in a single file that is the root of type inference. To be applicable, type inference has to be efficient and not explode due to exponential decision procedures or backtracking.

Using only the built-in functions of Nix, one can construct code that admits no static type:

```nix

{ "1759190400" = 1; }.${toString builtins.currentTime}

```

This program uses dynamic lookups and the impure builtins.currentTime function to access a field based on the system time at evaluation. Telling whether this program will error or not predicts when the program will be run and hence the future. Rejecting this program violates goal G3, but assigning it a definite type is unsound. A similar example is the built-in function that converts arbitrary JSON in string form to valid Nix structure. Both of these examples have to be typed at an unknown type ★ that denotes syntax where no precise type can be found. The role is similar to TypeScript any , with the difference that it can not be written and is only produced by the inference itself. Our type system uses it for this kind of uncertainty that can not be solved but also for incompleteness of the unification algorithm. See [](todo: ref).

== The Problem: Asymmetric Concatenation

#figure(
  caption: [Three examples of Nix code that needs asymmetric concatenation.],

  box(width: 100%, align(left, ```nix

  # 1. An overlay extends and overrides existing fields of a package

  self: super: { fields = super.foo // { meta = { broken = false; }; }; }

  # 2. In a derivation, fields are overridden with extra fields

  mkDerivation (args // { buildInputs = args.buildInputs ++ [ extra ]; })

  # 3. A conditional build based on the host-platform

  { pname = "hello"; } // lib.optionalAttrs stdenv.isDarwin { NIX_LDFLAGS = "-liconv"; }

  ```)),
)<nix-idioms>

The override operation `//` is used in Nix code to override existing configuration, mostly to adapt to a particular machine, a new version of a package, or the host environment. @nix-idioms shows three such usages. (1) Shows the usage of `//` to customize the build process of a package to patch dependencies or set meta-fields. (2) Shows a dual case: A package is defined and makes room for extra arguments, and (3) shows its usage to adapt to different machines.

The operator `//` is asymmetric record concatenation, a set-or-replace operation working on two operands. In this thesis we write $‖$ for this operation and $a ‖ b$ is its trivial example. The result of the operation contains every field of b and every field of a that b does not overwrite. Only a few record calculi support this operation (@sec-background).

The principal example that demonstrates the difficulty of typing open record concatenation is a: b: (a ‖ b).l, a concatenation of two unknown arguments a and b of which the result a label l is taken. Whether the field l [comes from](wording) a or b depends on whether b defines it or not. If b has a label l, it shadows the field of a and is taken; otherwise the field of a is returned. If both a and b are unknown (because they are type variables of a function), no amount of positive information can reveal the type of the returned value. We call this the Wand ambiguity since it was first discovered by Wand @cnocat4multiinher. This function is another example of unsolvable uncertainty that must receive a type in a total algorithm. Unlike the time-example, the ambiguity arises naturally in one of the most frequent operations of the language.

MiniNix therefore admits uncertainty in a constrained way: The result of lookups might be typed at ★, denoting that the type system makes no guarantees for this value. It might error, or it might run successfully.  MiniNix rests on two ingredients: Scoped records combine two records without fixing field-precedence on the spot. It loses no information about fields, and a tree-valued lookup relation traverses these total records and returns a type, definite absence, or a don't know value. Such a value is not resolved by a guess, but kept in the type-scheme of a binding and is reevaluated at every instantiation until it is decided by type-variable substitution. If no substitution ever resolves the usage, it is typed at ★, denoting that the algorithm can not decide its lookup.

MiniNix covers functions, records, concatenation, and let-polymorphism. The computed labels of the time example are added in @sec-fc-labels. @sec-background positions both ingredients against existing record calculi, and @sec-overview shows our approach by examples.

*Contributions* <contributions>
Our contributions are fourfold:
+ We separate field lookup from row equality. Field selection is answered by a deterministic lookup relation on rows with three results: a type, definite absence, or _don't know_. Row unification only equates rows and never receives a field demand, so it never guesses a field into a row-variable. We show that rows up to equivalence form a trace monoid, which yields a normal form and cancellativity; together with the separation, this replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ We define MiniNix, a type-safe calculus whose schemes carry pending lookups and mechanize progress up to lookup errors and preservation in Lean. We show that no plain Hindley-Milner scheme is principal for `x: x.l`, while a scheme that carries its pending lookup is (@type-safety, @principality). These results hold for MiniNix extended with first-class labels (@sec-fc-labels).
+ We give a sound and terminating unification algorithim and inference. When row unification succeeds, it returns a most general unifier, and its failure verdicts are sound. We prove inference sound for the declarative system: it defers a lookup that cannot be answered yet until a later substitution decides it, terminates, and every type it returns is a declarative typing (@unification, @inference).
+ We define delimited incompleteness. Where the algorithm stops, we classify it. Some row equations have no most general unifier, so no algorithm that returns a single unifier can solve them. Others have one that the algorithm misses, because it makes only moves that preserve every solution. Inference adds limits of its own (@incompleteness).
+ We test it against some real-word code (…)

*Overview*
@sec-overview positions MiniNix against the record calculi it builds on and introduces it through examples. The calculus itself is the subject of @declarative, which defines its typing rules and semantics, and of @metatheory, which establishes type safety and shows that principality forces qualified schemes. The algorithmic implementation follows: @unification develops row unification on spines, @inference the inference algorithm built on it, and @incompleteness delineates the places where both give up. @sec-mechanization relates the results to the Lean development and we continue by showing one extension to nix: @sec-extensions extends MiniNix with first-class labels and outlines the path towards full NixLang coverage. Finally, @sec-discussion measures our algorithm against the design goals, and @related-work places it in the literature.
