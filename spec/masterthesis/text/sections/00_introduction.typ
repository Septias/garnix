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
// meta: feels like a wild concatenation of sentences
The Nix programming language @nix-language-2-28 @dolstra_phd (NixLang) is a lazy, untyped functional language for the configuration of software. It is the input language of the Nix package manager and of NixOS, where it describes how software is built and how systems are configured. Its foundational data structure is the _attribute set_, a record of named fields, and the language provides a gamut of language features to create, extend, deconstruct and inspect them. Configuration in NixLang is written as attribute sets: a package, a service and an entire operating system are each described by attribute sets that collect their settings. The largest body of NixLang code is nixpkgs, a single repository of more than four million lines of code#footnote[Counted over the `.nix` files of nixpkgs 26.05, as are all figures about nixpkgs in this chapter.] that defines more than 100,000 packages @repology and is one of the ten GitHub projects with the most contributors @octoverse2024. The same repository carries the Nix standard library, the NixOS module system and the definition of the NixOS distribution itself @nixos_short @nixos_long, and all of them are built from other attribute sets.

NixLang is untyped: a missing field, or one of unexpected shape, surfaces only when evaluation reaches it, possibly long after the change that introduced it. The demand for a type system is long-standing @nix-ts-issue, and so is the difficulty of providing one: an earlier system by the author @simplenix applies off-the-shelf Hindley-Milner inference to a fragment of NixLang and fails precisely on the operations that combine attribute sets. A type system for NixLang therefore has to be built around these operations rather than added to them afterwards. [ ](what is this conclusion lol)

Our aim is a type system that applies to this existing body of code as it is and helps its programmers write safer code. We state this aim as four design goals, and @sec-goals assesses how far our system reaches each of them. [](maybe only note this later?)

/ G1 (No source changes): The analysis should run on today's nixpkgs verbatim. A repository of this size cannot be rewritten to become analysable.
/ G2 (No semantic change): Evaluation should produce exactly what it produced before, [down to derivation hashes](wording). The analysis may report on a program, but it may not instrument it.
/ G3 (Totality): Every program that runs should receive a type. [A tool that rejects working code is not used, so where the analysis cannot decide, it should say so rather than reject.](wording)
/ G4 (Effective inference): The analysis should scale to nixpkgs, more than 100,000 packages evaluated as one program.

G3 cannot be met by a type system that accepts only what it can decide. Using only the builtin functions, it is possible to write code that no static analysis can type completely:

```nix
{ "1759190400" = 1; }.${toString builtins.currentTime}
```

The program combines a record, an impure builtin and a computed label to select a field by the system time at evaluation. [It succeeds in exactly one second](wtf seconds) and fails in every other, so deciding it statically would mean predicting when it is run. A type system that rejects the program violates G3, and one that assigns it a definite type is unsound. What remains is to admit that the analysis does not know. Our type system therefore includes an _unknown type_ ★, which marks a place the analysis cannot decide. ★ is not an error: the program is accepted, and ★ records where its type is unknown. It plays the role of `any` in TypeScript @typescript, with the difference that it is never written by the programmer and is introduced only by the analysis itself. ★ absorbs uncertainty of two kinds: behaviour that is dynamic by nature, as in the example, and constructs that a stronger type system could type in the future.

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

Overwriting fields is among the most frequent operations on attribute sets: nixpkgs applies the operator `//` [13,285 times](this number feels out of context, the claim unneeded and misplaced). @nix-idioms shows the three idioms that rely on it. An overlay extends or replaces the packages of a package set it is given, an override replaces parts of an argument set it did not build, and a conditional extension adds fields only on some platforms. [In all three, `//` is applied to an attribute set whose fields are not known at the place where it is written](why is that not known?).

// meta: directly cite or remove the paragraph
The operator `//` is _asymmetric record concatenation_, written ‖ in this thesis: $a ‖ b$ contains every field of b and every field of a that b does not overwrite. [this total operation…] The operation is total, since overwriting is not an error but the intended behaviour. Record calculi rarely support it on records of unknown shape. Most restrict concatenation to records with disjoint fields or replace it by the extension with a single field, and those that keep it pay with constraints that a solver must discharge (@sec-background, @related-work).

The difficulty already shows in the smallest program that concatenates and then selects, `a: b: (a ‖ b).l`. Whether the field l comes from b or from a depends on whether b has a field l at all. If it has, b's field shadows a's; if it definitely has not, the field is a's. For abstract a and b neither is known, and no amount of positive information about the fields b does have settles which fields it lacks. This is the _wand-ambiguity_, first observed by Wand @concat4multiinher, and it leaves the program without a principal type in any row calculus that must decide where a field comes from. [It is the second source of not knowing](to what…?). [Unlike the time example, it arises inside the type system, and in one of the most frequent operations of the language.](this just feels random)

// meta: this was alreay shown, no?
Our answer is to let the type system not know in a disciplined way. Our calculus, MiniNix, rests on two ingredients. _Scoped records_ keep every field of both operands, so that concatenation is a total juxtaposition of rows and never loses information. A three-valued _lookup relation_, separate from the equality of rows, answers a field demand with a type, with definite absence, or with _don't know_. A _don't know_ is not resolved by a guess. It is kept in the type scheme of the binding and asked again at every use, where the arguments may decide it, and it is recorded as ★ only when it is never decided. The ★ of the wand-ambiguity is thus the same marker as the ★ of the time example. MiniNix covers functions, records, concatenation and let-polymorphism. The computed labels of the time example, which nixpkgs uses for 3,143 dynamic field accesses, are added in @sec-fc-labels. @sec-overview shows both ingredients at work, and @sec-background positions them against existing record calculi.

== Contributions <contributions>

+ Lookup separated from row equality. Field selection is answered by a lookup relation on rows with three results: a type, definite absence, or _don't know_. Row unification only equates rows and never receives a field demand. Rows up to equivalence then form a trace monoid, which yields a normal form and replaces the shared-tail side condition of Paszke and Xie @extensible_tabular (@row-lookup, @trace-monoid).
+ MiniNix, a type-safe calculus whose schemes carry pending lookups. Progress up to lookup errors and preservation are mechanized in Lean. No plain Hindley-Milner scheme is principal for `x: x.l`, while a scheme that carries its pending lookup and replays it at every use is (@type-safety, @principality). These results hold for MiniNix extended with first-class labels (@sec-fc-labels).
+ Sound and terminating unification and inference. When row unification succeeds, it returns a most general unifier, and its failure verdicts are sound. [Inference defers a lookup that cannot be answered yet](what is the relation to the declarative system?) until a later substitution decides it, terminates, and produces declarative typings (@unification, @inference).
+ Delimited incompleteness. Where the algorithm stops, [we show why](to broad). Some row equations have no most general unifier, so no algorithm that returns a single unifier can solve them. Others have one that the algorithm misses, because it makes only moves that preserve every solution. Inference adds limits of its own (@incompleteness).

@sec-overview introduces MiniNix through examples, and @sec-background recalls the record calculi it builds on. The calculus itself is the subject of @declarative, which defines its typing rules and semantics, and of @metatheory, which establishes type safety and shows that principality forces qualified schemes. [The algorithms follow](flow): @unification develops row unification on spines, @inference the inference algorithm built on it, and @incompleteness the places where both give up. @sec-mechanization relates the results to the Lean development, @sec-extensions extends MiniNix towards full NixLang, @sec-discussion measures it against the design goals, and @related-work [places it in the literature](wording).
