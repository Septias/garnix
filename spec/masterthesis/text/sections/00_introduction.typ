#import "../functions.typ": *

#let meta = [
  - This section should motivate the work from a practical perspective
  - Section goal: *The language forces our design decisions*

  1. Motivate thesis by nix usage
  2. Into: State our goals
  3. Motivation of the ★-type
  4. Second motivation: _asymmetric concat_

  == Todo
  - Better theme-rheme pairing
  - The motivation about nix can be slightly informal
  - It is a personal motivation after all and should entice the reader
  - Maybe make it a bit more about "how do we type such languages generally => goals"
  - Directly connect words using "Because, thus, hence, this is why, …"
  - Should the two motivations of ★ be grouped by a sentence?
]

= Introduction <sec-motivation>
The Nix programming language @nix-language-2-28 @dolstra_phd (NixLang) is a lazy, pure and untyped functional language for the configuration of software. It is the expression language of the Nix package manager and NixOS, where it describes how software is built and how systems are configured. Both projects see growing adoption: the package manager hosts the most up-to-date package repository @repology, and NixOS provides strong guarantees such as reproducibility and reliability @dolstra_phd @nixos_long. The largest body of NixLang code is nixpkgs, a single repository of more than four million lines of code#footnote[Counted over the `.nix` files of nixpkgs 26.05, as are all figures about nixpkgs in this chapter.] that defines more than 100,000 packages @repology and is one of the ten GitHub projects with the most contributors @octoverse2024. The same repository carries the Nix standard library, the NixOS module system and the definition of the NixOS distribution itself @nixos_short @nixos_long. Despite its popularity, NixLang has gone more than twenty years without a type system @nix-ts-issue, and its formal treatment is limited to a recent operational semantics @verified. The goal of this thesis is to close this gap.

In NixLang, packages, services and entire operating systems are described by _attribute sets_, sets of key-value bindings that collect, overwrite and merge configuration values. Attribute sets are central to configuration, which is why NixLang provides a rich record calculus with language constructs and builtin functions to create, extend, deconstruct and inspect them. Two of these features, _asymmetric concatenation_ and _first-class labels_, have proven tricky to type on their own, with only a small body of work on each @concat4multiinher @concat4free @fc_labels, and we know of no system that infers types for their combination. Since NixLang code relies on both, this thesis integrates them into a single sound calculus.

Without types, a missing field, or one of unexpected shape, surfaces only when evaluation reaches it, possibly long after the change that introduced it. Providing a type system is difficult for exactly these features: an earlier system by the author @simplenix applies algebraic subtyping @algebraic_subtyping @simplesub to a fragment of NixLang and fails precisely on the operations that combine attribute sets.

Our aim is a type system that applies to this existing body of code as it is and helps its programmers write safer code. We state this aim as four design goals:

/ G1 (No source changes): The analysis should run on today's nixpkgs verbatim. A repository of this size is impractical to annotate or restructure for the sake of an analysis.
/ G2 (No semantic change): Evaluation should produce exactly what it produced before. In particular, no check may force a value that lazy evaluation would leave untouched. The analysis may report on a program, but it may not change its behaviour, so that every package keeps its identifying hash and previously built binaries can still be reused.
/ G3 (Totality): To run on nixpkgs verbatim, every program that runs should receive a type. Where the analysis cannot decide, it should report uncertainty rather than reject.
/ G4 (Effective inference): The analysis should scale to nixpkgs with more than 100,000 packages, evaluated as one program.

To achieve goal G3 we have to admit uncertainty into the calculus: using only the builtin functions, it is possible to write code that no static analysis can type completely:

```nix
{ "1759190400" = 1; }.${toString builtins.currentTime}
```

The program combines a record, an _impure builtin_ and a computed label to select a field by the system time at evaluation. It succeeds in exactly one second and fails in every other, so deciding it statically would mean predicting when it is run. A type system that rejects the program violates G3, and one that assigns it a definite type is unsound [or imprecise…](?). What remains is to admit that the analysis does not know. Our type system therefore admits an _unknown type_ ★, which marks places the analysis cannot decide. ★ is not an error: the program is accepted, and ★ records where its type is unknown. It plays the role of `any` in TypeScript @typescript, with the difference that it is never written by the programmer and is introduced only by the analysis itself. Our type ★ absorbs uncertainty of two kinds: behaviour that is undecidable by nature (see the example), and limitations of our implementation that could be solved in the future.

== The Problem: Asymmetric Concatenation
#figure(
  caption: [The three idioms that make asymmetric concatenation unavoidable.],
  box(width: 100%, align(left, ```nix
  # 1. an overlay: extend or replace fields of a package set given to us
  self: super: { hello = super.hello // { meta = { broken = false; }; }; }

  # 2. an override: replace parts of an argument set we did not build
  mkDerivation (args // { buildInputs = args.buildInputs ++ [ extra ]; })

  # 3. a conditional extension: add fields only on some platforms
  { pname = "hello"; } // lib.optionalAttrs stdenv.isDarwin { NIX_LDFLAGS = "-liconv"; }
  ```)),
)<nix-idioms>

Overwriting fields with `//` is among the most frequent operations on attribute sets. @nix-idioms shows the three idioms that rely on it. An overlay extends or replaces the packages of a package set it is given, an override replaces parts of an argument set it did not build, and a conditional extension adds fields only on some platforms. In all three, the fields of one operand are fixed elsewhere: by the caller for overlays and overrides, by the platform for the conditional extension.

// meta: directly cite or remove the paragraph
The operator `//` is _asymmetric record concatenation_, written ‖ in this thesis: $a ‖ b$ contains every field of b and every field of a that b does not overwrite. This is a total operation, since overwriting is not an error but the intended behaviour. Few record calculi support it on records of unknown shape (@sec-background).

The difficulty already shows in the smallest program that concatenates and then selects, `a: b: (a ‖ b).l`. Whether the field l comes from b or from a depends on whether b has a field l at all. If it has, b's field shadows a's; if it definitely has not, the field is a's. For abstract a and b neither is known, and no amount of positive information about the fields b does have settles which fields it lacks. This is the _wand-ambiguity_, first observed by Wand @concat4multiinher, and it leaves the program without a principal type in any row calculus that must decide where a field comes from. [It is the second source of not knowing. Unlike the time example, it arises inside the type system, and in one of the most frequent operations of the language.](put this into one sentence)

Our answer is to let the type system not know in a disciplined way. Our calculus, MiniNix, rests on two ingredients. _Scoped records_ keep every field of both operands, so that concatenation is a total juxtaposition of rows and never loses information. A three-valued _lookup relation_, separate from the equality of rows, answers a field demand with a type, with definite absence, or with _don't know_. A _don't know_ is not resolved by a guess. It is kept in the type scheme of the binding and asked again at every use, where the arguments may decide it, and it is recorded as ★ only when it is never decided. The ★ of the wand-ambiguity is thus the same marker as the ★ of the time example. MiniNix covers functions, records, concatenation and let-polymorphism. The computed labels of the time example, are added in @sec-fc-labels. @sec-background positions both ingredients against existing record calculi, and @sec-overview shows them at work.

*Contributions* <contributions>

Our contributions are four-fold:

+ We define a separated Lookup relation from row equality. Field selection is answered by a lookup relation on rows with three results: a type, definite absence, or _don't know_. Row unification then only equates rows and never receives a field demand. [Because of this](correct?), Rows up to equivalence then form a trace monoid, which yields a normal form and replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ We define MiniNix, a type-safe calculus whose schemes carry pending lookups and mechanize progress up to lookup errors and preservation in Lean. We show that no plain Hindley-Milner scheme is principal for `x: x.l`, while a scheme that carries its pending lookup and replays it at every use is (@type-safety, @principality). These results hold for MiniNix extended with first-class labels (@sec-fc-labels).
+ We give a sound and terminating unification algorithim and inference. When row unification succeeds, it returns a most general unifier, and its failure verdicts are sound. We prove inference sound for the declarative system: it defers a lookup that cannot be answered yet until a later substitution decides it, terminates, and every type it returns is a declarative typing (@unification, @inference).
+ We define delimited incompleteness. Where the algorithm stops, we classify it. Some row equations have no most general unifier, so no algorithm that returns a single unifier can solve them. Others have one that the algorithm misses, because it makes only moves that preserve every solution. Inference adds limits of its own (@incompleteness).

*Overview*
@sec-background recalls the record calculi MiniNix builds on, and @sec-overview introduces MiniNix through examples. The calculus itself is the subject of @declarative, which defines its typing rules and semantics, and of @metatheory, which establishes type safety and shows that principality forces qualified schemes. The algorithmic implementation follows: @unification develops row unification on spines, @inference the inference algorithm built on it, and @incompleteness delineates the places where both give up. @sec-mechanization relates the results to the Lean development and we continue by showing one extension to nix: @sec-extensions extends MiniNix with first-class labels and sows the path towards full NixLang coverage. Finally, @sec-discussion measures our algorithm against the design goals, and @related-work places it in the literature.
