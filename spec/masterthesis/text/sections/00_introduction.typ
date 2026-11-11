#import "../functions.typ": *

#let meta = [
  1. Motivate thesis by nix usage
  2. The problem: asymmetric concatenation and the Wand ambiguity
  3. Design goals, and why they rule out the usual remedies
  4. Our approach: defer the lookup, ★ for what no use decides
  5. Contributions
  6. Overview
]

= Introduction <sec-motivation>

The Nix programming language @nix-language-2-28 @dolstra_phd, which we call NixLang, is a pure, functional, and untyped language for configuring software. It is the expression language of the Nix package manager and the NixOS operating system. The Nix package manager hosts the most up-to-date package repository in existence @repology, with over 140,000 packages, and the operating system's guarantees of reproducibility and reliability @dolstra_phd @nixos_long are the reason for its growing user base @nix-survey-2025. All packages, and the operating system, live in the public nixpkgs repository, spanning over four million lines of code#footnote[Counted over the Nix files of nixpkgs 26.05], which sees daily changes to packages and configurations, making it one of the ten busiest repositories on GitHub @octoverse2024. Despite its age and popularity, NixLang has gone without a type system for over twenty years @nix-ts-issue, and its formal treatment is limited to a recent semantics by Broekhoff et al. @verified and work by this author @simplenix. In this thesis, we turn to the issue of type inference again.

The attribute set in NixLang, or record in programming language theory, forms the basis for configurations as a natural combination of keys and values that declaratively describe the shape of software, for example the name of a package or the allowed list of IPs a service can use. At evaluation time, the configuration files consisting of these records are collected, overwritten, and merged using NixLang's language constructs and the built-in functions. Of the features needed to do so, asymmetric concatenation (`//`) and first-class labels (`x.${e}`) have proven hard to faithfully type on their own and in combination, with only a small body of work on each @concat4multiinher @concat4free @fc_labels @extensible_recs.

#figure(
  caption: [Three examples of Nix code that needs asymmetric concatenation.],
  box(width: 100%, align(left, ```nix
  # 1. A package is marked broken; the rest of its meta survives.
  drv: drv // { meta = (drv.meta or { }) // { broken = true; }; }

  # 2. In a derivation, fields are overridden with extra fields.
  mkDerivation (args // { buildInputs = (args.buildInputs or [ ]) ++ [ extra ]; })

  # 3. A conditional build based on the host platform.
  { pname = "hello"; } // lib.optionalAttrs stdenv.isDarwin { NIX_LDFLAGS = "-liconv"; }
  ```)),
)<nix-idioms>

Asymmetric record concatenation `//` is the operation that merges existing configuration, mostly to adapt to a particular machine, a new version of a package, or the host environment. @nix-idioms shows three common usages. In this thesis, we write $‖$ for this operation. The canonical example that demonstrates the difficulty of typing asymmetric record concatenation is $a: b: (a ‖ b).l$, a concatenation of two arbitrary arguments $a$ and $b$ from whose result the field $l$ is selected. Whether $l$ is taken from $a$ or $b$ depends on whether $b$ defines it or not. If $b$ defines it, it shadows the field of $a$ and is taken; otherwise the field of $a$ is returned. If both $a$ and $b$ are arbitrary (because they are variables of a function), the returned type cannot be decided where the function is defined but only when its arguments are instantiated. Row calculi that must decide where the field comes from on the spot have no principal type for this function. We call this situation the Wand ambiguity since it was first described by Wand @concat4multiinher and it is the motivating example for this thesis because it arises in one of NixLang's most used operations.

*Design goals.* The goal of our work is type inference for _asymmetric record concatenation_ and _first-class labels_ that runs on the existing body of NixLang code today and helps programmers write safe and reliable code. From this real-world requirement, we derive the following four design goals:

/ G1 (No source changes): Type inference should run on today's nixpkgs without source changes because a repository of nixpkgs' size is impractical to annotate or restructure for the sake of analysis.
/ G2 (No semantic change): Nix depends heavily on laziness to keep its performance. Any change to evaluation order, for instance through casts or dynamic checks, can change error, termination and performance behaviour.
/ G3 (Totality): To run on today's nixpkgs, the analysis must give a type to all existing, working code. In cases where type inference is ambiguous, the algorithm should report uncertainty instead of failing.
/ G4 (Efficient inference): All packages and the operating system are reachable from a single file, which is the root of type inference. To be applicable, type inference must be efficient and not explode due to exponential decision procedures or backtracking.

As we discuss in @sec-background, these goals already rule out the known remedies for the Wand ambiguity. Annotations violate G1, casts violate G2, rejecting the program violates G3, and backtracking or unification modulo associativity and commutativity violates G4.

*Our approach.* An annotation or a search attributes the field $l$ of $a: b: (a ‖ b).l$ to one operand, and every use of the function shares this attribution. Our calculus, MiniNix, instead defers the attribution of the field by recording pending lookups in types. This lets each use answer the lookup differently, based on the instantiation of variables. A few ingredients make this possible: Row concatenation joins the rows of both records and loses no information. A novel _three-valued lookup relation_ then answers a selection with a type, definite absence, or _don't know_. A lookup that answers _don't know_ is kept in the type scheme of the binding and re-run at every instantiation until a substitution decides it. With this approach, row unification only ever equates rows and never has to guess a field into a row-variable, so inference stays first-order and never backtracks.

Some uncertainty remains, however, and motivates the addition of an unknown type ★. Using only the built-in functions of NixLang, it is possible to create programs that admit no precise static type:

```nix
{ "1759190400" = 1; }.${toString builtins.currentTime}

```

This program uses a selection under a computed label and the impure `builtins.currentTime` function to access a field based on the system time at evaluation. Deciding whether it errors requires predicting when the program runs, and hence the future. Rejecting this program violates goal G3, but assigning it a definite type is unsound. A similar example is the built-in function `builtins.fromJSON`, which converts arbitrary JSON in string form to a Nix value. Both examples motivate an unknown type ★, which denotes that no precise type can be found. The type system makes no guarantees for a value of this type: It might error, or it might run successfully but we cannot decide. The role of ★ is similar to TypeScript's `unknown`, with the difference that it cannot be written and is only created by type inference. ★ also types two outcomes of lookup. A definite miss is typed at ★, since Nix is lazy in record fields and the error may never be forced. A pending lookup that no substitution decides stays a qualifier of the answer, and a use that leaves it undecided turns into ★.

*Contributions.* <contributions>
+ We define MiniNix, a calculus with scoped labels, an unknown type ★ and a dedicated lookup relation with three possible outcomes. The lookup relation is deterministic, so a pending lookup is decided by running it and not by an entailment relation. Schemes carry pending lookups as qualifiers. MiniNix is type-safe up to lookup errors (@type-safety). We show that no plain Hindley-Milner scheme is principal for $x: x.l$ in our calculus, while a scheme that carries its pending lookup is (@principality).
+ We show that the row presentation of records forms a trace monoid, which yields a normal form and is cancellative. Since lookup is a separate judgement, row unification on this normal form needs no side condition on shared tails, unlike the unification of Paszke and Xie @extensible_tabular. When it succeeds, it returns a most general unifier, and its failure verdicts are sound (@unification).
+ We give a sound and terminating inference algorithm. The algorithm defers a lookup that cannot be answered yet until a later substitution decides it, and keeps a lookup that stays undecided as a qualifier of the scheme. We prove termination, and show that every instance of the scheme it returns is a declarative typing (@inference).
+ We show where the algorithm is incomplete. Some row equations have no most general unifier, and no algorithm that returns a single unifier can solve them. Others have one that row unification misses, because it makes only moves that preserve every solution. Inference adds limits of its own (@incompleteness).
+ We extend MiniNix with first-class labels. Type safety, the unification theorems, and soundness and termination of inference carry over (@fc-labels).
+ We mechanize the metatheory and all examples of this thesis in Lean, sorry-free and without additional axioms. The development covers MiniNix with first-class labels and MacroNix, an extension by booleans, conditionals, lists and some builtins (@sec-mechanization).

*Overview.* @sec-overview motivates the design decisions of MiniNix by comparing it to previous systems and introduces it through examples. @declarative presents the declarative calculus and @metatheory its metatheory. The algorithmic system follows: @unification develops row unification on spines, @inference the inference algorithm built on it, and @incompleteness delineates the places where both give up. @fc-labels extends MiniNix with first-class labels, and @sec-mechanization relates the results to the Lean development. @sec-discussion returns to the idioms of @nix-idioms, measures the algorithm against the design goals and gives an outlook. Finally, @related-work places our work in the literature, and the conclusion summarizes the results.
