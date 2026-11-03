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

The Nix programming language @nix-language-2-28 @dolstra_phd, which we dub NixLang, is a pure, functional, and untyped language for configuring software. It is the expression language of the Nix package manager and NixOS operating system. The Nix package manager hosts the most up-to-date package repository in existence @repology, with over 140,000 packages, and the operating system's guarantees of reproducibility and reliability @dolstra_phd @nixos_long are the reason for its growing user base @nix-survey-2025. All packages, and the operating system, live in the public nixpkgs repository, spanning over four million lines of code#footnote[Counted over the Nix files of nixpkgs 26.05], which sees daily changes to packages and configurations, making it one of the ten busiest repositories on GitHub @octoverse2024. Despite its age and popularity, NixLang has gone without a type system for over twenty years @nix-ts-issue, and its formal treatment is limited to a recent semantics by Broekhoff and Krebbers @verified and work by this author @simplenix. In this thesis, we turn to the issue of type inference again.

The attribute set in NixLang, or record in programming language theory, forms the basis for configurations as a natural combination of keys and values that declaratively describe the shape of a package or operating system. For example, the name and version of a package or the allowed list of IPs a service can use. At build time, the configuration files constituted of these records are collected, overwritten, and merged using Nix's language constructs and the built-in functions. Of the features needed to do so, asymmetric concatenation and first-class labels have proven hard to faithfully type on their own and in combination, with only a small body of work on each @concat4multiinher @concat4free @fc_labels @extensible_recs, and only Paszke and Xie @extensible_tabular combine both in a sound calculus with unification based  inference.

The goal of our work is to combine _asymmetric record concatenation_ and _first class labels_ in one calculus and aid programmers in writing safe and reliable code for the existing body of NixLang code today. In particular, we do not try to only collect package information or create a new, simplified configuration language that compiles to Nix, like Nickel @nickel. From this real-world requirement, we derive the following four design goals:

/ G1 (No source changes): Type inference should run on today's nixpkgs without source changes because a repository of nixpkgs' size is impractical to annotate or restructure for the sake of analysis.
/ G2 (No semantic change): Nix depends heavily on laziness to keep its performance. Any change to evaluation order, for instance through casts or dynamic checks, can change error, termination and performance behaviour.
/ G3 (Totality): To run on today's nixpkgs, the analysis must give a type to all existing, working code. In cases where type inference is ambiguous, the algorithm should report uncertainty instead of failing.
/ G4 (Efficient inference): All packages and the operating system are reachable from a single file, which is the root of type inference. To be applicable, type inference must be efficient and not explode due to exponential decision procedures or backtracking.

Some programs admit no precise static type at all, even with only the built-in functions of Nix:

```nix

{ "1759190400" = 1; }.${toString builtins.currentTime}

```

This program uses dynamic lookups and the impure `builtins.currentTime` function to access a field based on the system time at evaluation. Telling whether it will error or not predicts when the program will be run and hence the future. Rejecting this program violates goal G3, but assigning it a definite type is unsound. A similar example is the built-in function `builtins.fromJSON`, which converts arbitrary JSON in string form to a Nix value. Both of these examples must be typed at an unknown type ★ that denotes that no precise type can be found. The role is similar to TypeScript's `unknown`, with the difference that it cannot be written and is only created by type inference.

== The Problem: Asymmetric Concatenation

#figure(
  caption: [Three examples of Nix code that needs asymmetric concatenation.],
  box(width: 100%, align(left, ```nix
  # 1. A package is marked broken; the rest of its meta survives.
  drv: drv // { meta = (drv.meta or { }) // { broken = true; }; }

  # 2. In a derivation, fields are overridden with extra fields.
  mkDerivation (args // { buildInputs = (args.buildInputs or [ ]) ++ [ extra ]; })

  # 3. A conditional build based on the host-platform.
  { pname = "hello"; } // lib.optionalAttrs stdenv.isDarwin { NIX_LDFLAGS = "-liconv"; }
  ```)),
)<nix-idioms>

The override operation `//` is used in Nix code to merge existing configuration, mostly to adapt to a particular machine, a new version of a package, or the host environment. @nix-idioms shows three such usages. The operator `//` is asymmetric record concatenation, a set-or-replace operation working on two operands. In this thesis we write $‖$ for this operation and only a few record calculi support this operation (@sec-background, @related-work).

The principal example that demonstrates the difficulty of typing asymmetric record concatenation is $a: b: (a ‖ b).l$, a concatenation of two unknown arguments $a$ and $b$ from whose result a label l is taken. Whether the field $l$ is from $a$ or $b$ depends on whether $b$ defines it or not. If $b$ has the label l, it shadows the field of $a$ and is taken; otherwise the field of $a$ is returned. If both $a$ and $b$ are unknown (because they are variables of a function), the returned type can not be decided. We call this the Wand ambiguity since it was first described by Wand @concat4multiinher. This function is our guiding example of unsolvable uncertainty and motivates our work. Unlike the time example, this ambiguity arises naturally in one of the most frequent operations of the language.

Our calculus, MiniNix, therefore admits uncertainty: The result of lookups might be typed at ★, denoting that the type system makes no guarantees for this value. It might error, or it might run successfully but we cannot decide. To track the unavoidable uncertainty, we use two ingredients: Scoped labels @extensible_recs combine two records without deciding field precedence on the spot. This operation loses no information about fields, and a three-valued lookup relation traverses records and returns a type, definite absence, or _don't know_. A lookup is then not resolved by a guess, but kept in the type scheme of a binding and is reevaluated at every instantiation until it is decided by substitution. If no substitution resolves it during inference, it stays a qualifier of the answer, and a use that leaves it undecided turns it into ★.

*Contributions* <contributions>

// claim efficiency here?
+ We separate field lookup from row equality by admitting a dedicated lookup relation with three possible outcomes. Row unification then only equates rows and never receives a field demand, so it never has to guess a field into a row variable. We show that the row presentation of records forms a trace monoid, which yields a normal form and is cancellative. Together with the separation, this replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ We define MiniNix, a type-safe calculus whose schemes carry pending lookups and mechanize progress up to lookup errors and preservation in Lean (@type-safety). We show that no plain Hindley-Milner scheme is principal for x: x.l in our calculus, while a scheme that carries its pending lookup is (@principality). These results hold for MiniNix extended with first-class labels (@fc-labels).
+ We give a sound and terminating unification algorithm and inference. When row unification succeeds, it returns a most general unifier, and its failure verdicts are sound. It is sound with regards to the declarative calculus and defers a lookup that cannot be answered yet until a later substitution decides it. We prove termination, and show that every instance of the scheme it returns is a declarative typing (@unification, @inference).
+ We show where the algorithm is incomplete. Some row equations have no most general unifier, and no algorithm that returns a single unifier can solve them. Others have one that the algorithm misses, because it makes only moves that preserve every solution and inference adds limits of its own (@incompleteness).
*Overview*

@sec-overview motivates the design decisions of MiniNix by comparing it to previous systems and introduces it through examples. The declarative calculus is discussed in @declarative and @metatheory establishes its metatheory. The algorithmic implementation follows: @unification develops row unification on spines, @inference the inference algorithm built on it, and @incompleteness delineates the places where both give up. @fc-labels extends MiniNix with first-class labels, and @sec-mechanization relates the results to the Lean development. Finally, @sec-discussion measures the algorithm against the design goals and gives an outlook. @related-work places our work in the literature.

