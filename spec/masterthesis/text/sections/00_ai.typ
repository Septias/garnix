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
The Nix programming language @nix-language-2-28 @dolstra_phd, which we call NixLang, is a pure, functional, and untyped language for configuring software. It is the expression language of the Nix package manager and the NixOS operating system. The Nix package manager hosts the most up-to-date package repository @repology, with over 100,000 packages, and the reproducibility and reliability guarantees of the operating system @dolstra_phd @nixos_long account for its [growing user base](source?). All packages and the operating system itself are defined in the public nixpkgs repository, which spans over four million lines of code#footnote[Counted over the nix-files of nixpkgs 26.05], receives daily changes to packages and configurations, and ranks among the ten busiest repositories on GitHub @octoverse2024. Despite its age and popularity, NixLang has lacked a type system for over twenty years @nix-ts-issue, and its formal treatment is limited to a recent semantics by Krebbers and Broekhoff @verified and prior work by the author @simplenix.

Configurations in Nix are attribute sets, the records of programming language theory: each attribute set declaratively describes part of a package or system through key-value pairs, such as the name and version of a package or the permitted IP addresses of a firewall. During evaluation, the attribute sets of many configuration files are collected, merged, and overridden by the operators and built-in functions of the language. Two of the features involved, asymmetric concatenation and first-class labels, are difficult to type faithfully, and more so in combination. Only a small body of work addresses each @concat4multiinher @concat4free @fc_labels, and only [P&X](ref?) combines both in a sound calculus.

The goal of this thesis is to define and study type inference for NixLang as it is written today, and thereby to support programmers and tools in writing safe and reliable Nix code. We therefore neither restrict inference to package metadata nor propose a simplified configuration language that compiles to Nix @nickel. This requirement yields four design goals:

/ G1 (No source changes): Type inference must run on nixpkgs without source changes, since a repository of its size cannot practically be annotated or restructured for the sake of an analysis.
/ G2 (No semantic change): Type inference must not alter evaluation. Nix relies on laziness for its performance, and any change to evaluation order, for instance through casts or dynamic checks, can change the output hashes and build results of packages, and is therefore costly.
/ G3 (Totality): The analysis must assign a type to every syntactically valid expression. Where type inference is ambiguous, it reports uncertainty instead of failing.
/ G4 (Efficient inference): All packages and the operating system are reachable from a single root expression, which is therefore the entry point of type inference. Inference must hence scale to the size of nixpkgs, which excludes exponential decision procedures and backtracking.

The design goals have direct consequences for the type system. Consider the following program, which uses only built-in functions of Nix:

```nix

{ "1759190400" = 1; }.${toString builtins.currentTime}

```

The program selects a field by a label computed from the system time at evaluation. Whether the selection succeeds therefore depends on when the program is run, which no static analysis can know. Rejecting the program violates G3; assigning it a definite type is unsound. The built-in function `builtins.fromJSON`, which converts a JSON string into a Nix value, poses the same problem. Both programs are therefore typed at the _unknown type_ ★, which denotes that no precise type can be determined. ★ resembles the type `any` of TypeScript @typescript, except that it cannot be written by the programmer and arises only from inference. The type system assigns ★ in two situations: to uncertainty that no analysis can resolve, as in the examples above, and to the incompleteness of the unification algorithm (@incompleteness).

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

The operator `//` overrides existing configuration, mostly to adapt a package to a particular machine, a new version, or the host environment. @nix-idioms shows three typical uses: an overlay that patches dependencies or sets meta fields of an existing package (1), a package definition that accepts extra arguments and lets them override its own fields (2), and a configuration that depends on the host platform (3).

In record calculi, `//` is _asymmetric record concatenation_, which we write $‖$: the record $a ‖ b$ contains every field of $b$ and every field of $a$ that $b$ does not define. Only a few record calculi support this operation (@sec-background).

The difficulty of typing open concatenation is well known @concat4multiinher: the type of a field selected from a concatenation depends on fields that the program does not mention. Consider the function `a: b: (a ‖ b).l`, which concatenates its two arguments and selects the label $l$ from the result. If $b$ defines $l$, its field shadows that of $a$ and is returned; otherwise, the field of $a$ is returned. As long as $a$ and $b$ are type variables, that is, as long as the function is typed in isolation, no information about the fields that $a$ and $b$ contain determines which of the two cases applies. We refer to this situation as the _Wand ambiguity_, after Wand, who first identified it @concat4multiinher. Like the time example, the function admits no definite type and, by G3, must nevertheless receive one. Unlike the time example, however, it is not a contrived program: the ambiguity arises in `//`, one of the most frequent operations of the language.

The solution we propose does not resolve this ambiguity but records it. MiniNix, the calculus of this thesis, rests on two ingredients. _Scoped records_ combine two records without fixing field precedence, so that no information about fields is lost. A tree-valued _lookup relation_ traverses these records and answers a field selection with a type, definite absence, or _don't know_. A _don't know_ answer is not resolved by a guess; instead, it is kept in the type scheme of the binding and re-examined at every instantiation, until a type-variable substitution decides it. If no substitution ever decides it, the selection is typed at ★: the type system then makes no guarantee about the value, which may evaluate successfully or fail. Uncertainty is thus admitted in a constrained form, namely only at lookups that remain undecided.

MiniNix covers functions, records, concatenation, and let-polymorphism; @sec-fc-labels adds the computed labels of the time example. @sec-background positions both ingredients against existing record calculi, and @sec-overview introduces them through examples.

*Contributions* <contributions>
Our contributions are the following:
+ We separate field lookup from row equality. Field selection is answered by a deterministic lookup relation on rows with three results: a type, definite absence, or _don't know_. Row unification only equates rows and never receives a field demand, so it never guesses a field into a row variable. We show that rows up to equivalence form a trace monoid, which yields a normal form and cancellativity; together with the separation, this replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ We define MiniNix, a type-safe calculus whose schemes carry pending lookups, and mechanize its progress (up to lookup errors) and preservation in Lean. We show that no plain Hindley-Milner scheme is principal for `x: x.l`, whereas a scheme that carries its pending lookup is (@type-safety, @principality). These results hold for MiniNix extended with first-class labels (@sec-fc-labels).
+ We give a sound and terminating unification algorithm and an inference algorithm built on it. When row unification succeeds, it returns a most general unifier, and its failure verdicts are sound. Inference defers a lookup that cannot be answered yet until a later substitution decides it; we prove that it terminates and that every type it returns is a declarative typing (@unification, @inference).
+ We characterize where both algorithms stop, which we call _delimited incompleteness_. Some row equations have no most general unifier, so no algorithm that returns a single unifier solves them. Others have one that the algorithm misses, because it makes only moves that preserve every solution. Inference adds limits of its own (@incompleteness).
+ [We evaluate the analysis on real-world code.](todo: data)

*Overview*
@sec-overview positions MiniNix against the record calculi it builds on and introduces it through examples. @declarative defines the calculus, its typing rules, and its semantics; @metatheory establishes type safety and shows that principality forces qualified schemes. The algorithmic part follows: @unification develops row unification on spines, @inference builds the inference algorithm on it, and @incompleteness delineates where both give up. @sec-mechanization relates these results to the Lean development. @sec-extensions extends MiniNix with first-class labels and outlines the path towards full NixLang coverage. Finally, @sec-discussion measures the algorithm against the design goals, and @related-work places it in the literature.
