#import "functions.typ": *
#import "typesystem.typ": *
#import "./figures/reduction-compare.typ": comparison
#import "./figures/builtin-types.typ": builtin_types
#import "./figures/module-types.typ": module_types
#import "./figures/ts-compare.typ": ts-compare
#import "sections/occurrence.typ"
#import "sections/connectives.typ"
#import "sections/records.typ"
#import "sections/first-class-labels.typ"
#import "sections/modulesystem.typ"

#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

#set document(
  title: "Securing Nix' Foundations",
  description: "Masterproject about nix syntax, operational semantics and type inference approaches",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

= Securing Nix' Foundations
The Nix programming language is used in over 40,000 source files, showing its practical significance and widespread adoption. Despite this prominence has the language not yet received a static type system such that practicioneers need to wait for the evaluator to return a result; A tendious and time intesive process. Independent efforts by S. Klähn and by Breokhoff et al. have begun to address the shortcomming of theoretical treament of the language. Both works provide formalizations of the language’s syntax and operational semantics tailored to their respective objectives. However, neither treatment captures the full expressive range of Nix as it is used in practice. The present work extends this line of research by developing an operational semantics that more comprehensively reflects the language’s behavior, and also systematically examins potential approaches to type inference in this setting.

= Origin of the Nix Language <intro>
The nix package manager distinguishes itself from other package managers because it is founded on a built-in domain-specific programming language. This eponymous language, Nix, is a  major source of the ecosystem’s steep learning curve but is alse the foundation of its core properties — most notably its _reproducability_ guarantees.

The nix package manager was developed to overcome the problem of distributing software (components) between heterogenous environments without breaking them @nixos_short. This problem is deceptively complex and has motivated a wide range of package management strategies. Eelco Dolstra et al. @memory_to_software took an approach that conceptualizes package management as as memory management discipline. In this analogy, files are interpreted as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. This development lead to a _garbage-collector_ inspired technique to consistently track dependencies during package construction which then resemble a closure: a complete dependency graph rooted at the built artifact and containing all recursively required components. Because this closure is self-contained, it can be transferred to another machine and reconstructed without relying on the target system’s ambient state, leading to save and reproducible software.

Nix relies on the _nix store_, a read-only directory that contains the transferred, immutable buildartifacts. Each artifact is identified by a cryptographic hash derived from its inputs, enabling efficient equality checks and facilitating maximal reuse of existing components. It also allows multiple versions or variants of a package to coexist in the store without interference, since each is uniquely addressed and isolated. Because build outputs are pure derivations of their declared inputs, new versions can be added without mutating or invalidating prior ones enabling reliable rollbacks and atomic upgrades @memory_to_software. Moreover, the explicit tracking of dependency references enables a garbage collection mechanism that identifies unreachable store paths by tracing from a set of designated roots, thereby reclaiming disk space while preserving consistency.

These strong guarantees of nix are a direct consequence of the underlying _domain specific language_ (DSL) that makes them an inherent property instead of a retrofitted qualities. First and foremost, one of nix' greatest strength – _reproducibility_ – is a direct consequence of the languages' functional design. When abstracting files and references to memory locations and references, it can be noticed that _pure functionality_ provides the features needed for secure dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

The environment in which Nix operates is costly because a single action – building a package or distributing it over the network – can be very expensive. In such an environment, packages must only be realised when needed. In a lazy language, values of function application are substituted as-is without further reduction i.e computation on them. This saves resources on the lowest level and Nix uses the same mechanism in lazy record fields, patterns and arrays. Since Nix packages are stored in these lazy record fields, a single package will only trigger builds of direct dependencies instead of the whole package tree.


Taken together, these design choices result in a language that combines functional purity, laziness, recursive records, and domain-specific constructs for package composition. While these features enable powerful guarantees—such as reproducibility, composability, and isolation—they also introduce substantial complexity for static analysis.

#subsec[Overview.]
We begin by providing a concise overview of characteristic features of the Nix language that render static typing particularly challenging in @quirks. @syntax presents the formal syntax and operational semantics of the nix language. Finally, @ts-dicsussion analyzes the requirements that a type system must satisfy to adequately capture the full expressiveness of Nix, and evaluates specific calculi with respect to their suitability and limitations.


= Quirks of the Nix Language <quirks>

The nix programming language revolves around records. A primitive record (or `AttrSet` in Nix) is a set of key-value bindings, for example `{a = 2; b = 3;}`. By prefixing a record with the `rec` keyword, its fields are placed in a recursive scope, allowing bindings to reference other attributes of the same record. For instance, `rec {a = b; b = 2;}` evaluates to `{a = 2; b = 2;}`. The combination of lazynes and recursive records enables both structural and non-structural recursion, however not all forms of recursion are well-formed. Direct recursive records such as `rec {x = x;}` or mutual recursive `rec {a = b; b = a;}` records without intervening lazy constructors are ill-formed. The Nix evaluator detects such cases and returns an error instead of recursing forever. Well-behaved recursive definitions must introduce a level of indirection through a _lazy constructor_—such as a nested attribute set, list, pattern-bound field, or let binding—so that recursion is guarded. Consequently, both `rec {x = {x = x;};}` and `let x = {x = y;}; y = x; in x` are well-behaved and can be unrolled indefinitely in their x-field.

Let-bindings also allow multiple, possibly mutual-referential bindings. For example, `let a = b; b = 2; in b` is well-formed and evaluates to 2. In contrast to recursive attribute sets, the let construct does not require an explicit `rec` keyword to enable such mutual references; the recursive scope is implicit. As with recursive records, however, non-constructive recursion is disallowed. Definitions in which bindings immediately force their own evaluation—thereby yielding unguarded cyclic dependencies—lead to infinite unfolding and are rejected at runtime. Productive recursion must therefore be mediated by a lazy constructor, ensuring that self-reference is guarded.

The similarity between both constructs becomes particularly evident in the let-record-form `let {a = 2; b = 3; body = t;} ` that encloses the semicolon-separated key-value bindings of the usual let-binding in braces. The two forms are intertranslatable in a straightforward manner: a conventional let expression can be rewritten as a let-record by collecting its bindings into an attribute set and designating the body explicitly, and conversely, a let-record can be reduced to the standard form by removing the braces and separating the body from the bindings.

The remaining difference between the two is, that a record definition is a terminating expression whilst a let-binding is followed by an arbitrary possibly diverging computation. This subtle difference can be partially bridged by the `with`-construct. The `with`-construct is a nix-specific feature that takes a record as argument and opens it in the following expression, adding all bindings to the scope. It is then possible to compute the sum of two record-fields in the following expression: `with {a = 2; b = 2;}; a + b`. By using the with-construct in conjunction with records, it were in theory possible to form an equivalence between the two constructs: \
`with (rec {a = b; b = 2}); t` ↔︎ `let {a = b; b = 2; body = t;}`

if it were not for with' unexpected shadowing behaviour.

A with statement binds _weakly_, meaning it will not override identifiers that were introduced by enclosing functions or let-bindings. For example `let a = 2; in (with {a = 3;}; a)` evaluates to 2 instead of 3, even though the binding `a = 3;` is evaluated after `a = 2;`. The same behaviour arises with function parameters, inherited bindings, and records. For example, the expression `rec {a = 3; b = (with {a = 4;}; a);}` will evaluate to `{a = 3; b = 3;}`. When stacking with-bindings, shadowing behaves as expected: `with {a = 1;}; with {a = 2;}; a → 2`.

This seemingly unusual shadowing discipline reflects the intended role of `with` as a lightweight module mechanism. An attribute set may be viewed as a module encapsulating a collection of definitions. The with construct “opens” such a module by extending the surrounding scope with its attributes. Because modules may introduce a large number of bindings, weak shadowing prevents accidental overriding of existing identifiers defined by explicit abstractions such as functions or let bindings. This pattern becomes particularly expressive in combination with the built-in import function, which evaluates a file and returns its resulting value. For example, `with (import ./modules.nix); t`, imports the attribute set defined in `modules.nix` and makes all of its bindings available in the evaluation of t. Operationally, this resembles a form of global module inclusion, while retaining the lexical scoping and compositional structure of the language.

The `import` construct may appear at any position in the syntax tree—including within recursive functions, record fields, arrays, and other expressions. Operationally, `import` evaluates the designated file and yields its result, which then composes with the surrounding expression in the usual way. In particular, when the imported file evaluates to a function, a subsequent term is parsed as its argument, yielding an application. For example, `import ./modules.nix { system = "nixos_x86"; }` first evaluates `modules.nix` to a function `f` and then applies it to the record argument, i.e., `import ./modules.nix { system = "nixos_x86"; } → f { system = "nixos_x86"; }`. Although the language does not statically verify the shape of imported files, organizing code across files via `import` is idiomatic and widely used; see @module_example for a complete illustration.

Function application with a record argument admits dedicated syntactic support via _pattern functions_. A pattern function destructures its record argument into fields, thereby providing a convenient notation for functions that conceptually take multiple parameters, possibly equipped with default arguments. In the same way that record construction `{}` serves as an introduction form, pattern functions constitute the corresponding elimination form—hence the shared use of braces. For example, `{a, b}: a + b` expects a single record argument and binds its fields `a` and `b` into the body's scope. Taken together, record construction and pattern-function application behave as logical inverses, e.g., `a: ({a}: a) { a = a; } == a`.

A simple pattern such as `{a, b}: a` requires exactly the fields `a` and `b` in its record argument. Open patterns, written `{a, ...}`, admit additional, unspecified fields. Default arguments are supplied via the question‑mark notation `{ a ? "nikita", ... }`. Crucially, pattern bindings are lazy and recursive: the fields introduced by the pattern are added to the function’s scope as a whole (including the pattern itself) and may be referenced mutually throughout the body.

These scoping and evaluation conventions admit subtle forms of recursion. For example, `{ a ? b, b }: a` appears innocuous yet relies on a self‑referential default. Likewise, `({ a ? (with { a = 3; }; b), b }: a) { b = 2; }` interacts with with‑binding and weak shadowing in ways that complicate termination reasoning. The latter evaluates to 2; however, renaming the with‑bound term to `a` as in `({ a ? (with { a = 3; }; a), b }: a) { b = 2; }` yields divergence: because with‑bindings bind weak, name resolution prefers the pattern‑bound `a` over the with‑bound one, thereby creating a cycle.

#figure(
  ```nix
  # strings.nix
  { pkgs, lib, hasher } : {
    concat = a: b: (/* */);
    captalize = s: (/* */);
    to_camel = s: (/* */);
    hash = s: hasher(s);
  };

  # configuration.nix
  {pkgs, lib, hasher}: {
    f = with (import ./strings.nix {
    inherit pkgs;
    inherit (pkgs) lib hasher;
    }); (name: surname: concat(capitalize(name), capitalize(surname));
  }
  ```,
  caption: [ A module and its application. ],
) <module_example>


== String Interpolation <string_interpolation>

String interpolation provides a uniform mechanism for constructing string and path literals from embedded expressions. In Nix, an occurrence of `${t}` within a string or path evaluates `t` to a string and splices the result verbatim into the surrounding literal. For example, `let name = "john"; in "Hello ${name}"` evaluates to `"Hello john"`. Interpolation also composes with path literals, enabling programmatic construction of file locations:
```
let conf_file_of = (name: readString /home/${name}/.config/nu); in conf_file_of "john" → /home/john/.config/nu
```

The same syntax extends to attribute paths and definitions, thereby supporting dynamic attribute names, dynamic selection, and presence checks. For instance, `{ ${"foo"} = bar; }` is equivalent to `{ foo = bar; }`. Likewise, `{ a = 1; b = 2; c = 3; }.${t}` selects the attribute named by the value of `t`. Interpolation is also admitted in has‑attribute queries:
```
{ players.gaben.health = 10; players.ruben.health = 7; } ? players.${"ruben"}.health → true
```

Consequently, attribute labels are first‑class citizens of the language.


== Dunder Methods <dunder>

The nix programming language provides special dunder variables and record fields that interact with the language and provide information about the current execution environment. Of the six dunders we found during our analysis, only the `__functor` field is described in the nix language manual @nix-language-2-28, the others can only be found in the evaluator tests @nixos_tests or nixpkgs source code @special_args. We will now give a short description of the most important dunder methods.

The special record field `__overrides` is recognized by the language evaluator and can be used to override fields of the record. For example, the expression `rec { a = 1; __overrides = { b = 2; };}` reduces to `{a = 1; b = 2; __overrides = {..};}`. This example only works because the record is declared recursive and if that keyword was to be removed, the whole expression would reduce to `{a = 1; __overrides = {..};}`, ignoring the overridden field. The combination with string interpolation behaves unexpected aswell, because `rec {"${"foo"}" = "bar"; __overrides = { bar = foo; };}` will fail due to foo being undefined. This is not in line with usual records `rec { ${"foo"} = "bar"; bar = foo;}` where name resolution can lookup the value of the dynamic address `foo`. These suprising interactions interactions are not documented and it is not clear what leads to the shown restrictions.

The special `__functor` field can be used to turn a record into a function. For example the expression `({ __functor = self: x: self.foo && x; foo = false; } // { foo = true; }) true` reduces to `true`. The function defined in the `__functor` field will be called with itself and the supplied argument, allowing for recursive function definitions. It is also possible to stack functor special fields `{__functor = {__functor = {…}}}` to form deep and non-trivial definitions.

The remaining special attribute, `__toString`, defines a coercion protocol from records to strings. When an attribute set provides `__toString = self: ...`, a call to `builtins.toString` on that set invokes the function to obtain its textual representation. For example, `builtins.toString { __toString = self: "a record"; }` evaluates to `"a record"`. In the absence of `__toString`, coercion of records to strings is rejected.

Nix additionally provides three impure dunder variables—`__currentSystem`, `__nixPath`, and `__currPos`—which expose, respectively, the target system identifier, the active import search path, and the current source location. By rendering aspects of the execution environment directly observable, these bindings introduce _impurity_ into evaluation. Thesse impurities and their implication for type inference are further discussed in @impurities.

During our analysis we found further occurrences of `__structuredAttrs`, `__splicedPackages`, `__allowFileset`, `__impure`, ... that could have effects on the evaluator, but their usage seems to be mixed with standard variable naming schemes for "hidden" variables such that a clear distinction remains an open question.


== Nix Builtins

The Nix language builtins extends the language' features beyond the simply syntactic ones. They comprehend functions that manipulate the languages datatypes, inspect types, bring the execution environment into scope and infer with the usual program execution flow. We will now give a short overview over the builtin functions relevant to type inference. They will be further discussed in section \@builtin_types_discussion but we want to introduce a few builtins that add hard-to-type features to the laungage upfront.

#[
  #show figure: set block(breakable: true)
  #figure(
    caption: [Selected nix builtins. See @all-builtins for a full list.],
    placement: none,
    table(
      columns: (auto, 1fr),
      table.header([*Builtin*], [*Type*]),
      table.cell(colspan: 2, [ Record-related builtins ]),

      [attrNames `set`             ], $ recordType -> [ str ] $,
      [attrValues `set`            ], $ recordType -> [ or.big oi(τ_i) ] $,
      [catAttrs `attr list`        ],
      $ str -> [oj(recordType)_j] -> [or.big τ_(j i)] "where" l_(j i) = str $,

      table.cell(colspan: 2, [ List-related builtins]),

      [concatLists `lists`         ], $ [[α]] -> [α] $,
      [elem `x xs`                 ], $ [α] -> α -> bool $,
      [elemAt `xs n`               ], $ [α] -> n -> α $,
      [concatMap `f list`          ], $ (α -> β) -> [[α]] -> [β] $,
      [concatStringsSep `sep list` ], $ str -> [str] -> str $,

      table.cell(colspan: 2, [ Impure builtins ]),

      [m currentSystem             ], $ () -> str $,
      [m currentTime               ], $ () -> int $,

      [fromJSON `e`                ], $ str -> star.op $,
      [fromTOML `e`                ], $ str -> star.op $,

      table.cell(colspan: 2, [ Inspection builtins ]),

      [functionArgs `f`            ], $ (openPat -> α) -> {l_i: bool} $,
      [isAttrs `e`                 ], $ τ -> bool $,
      [isBool `e`                  ], $ τ -> bool $,
      [isFloat `e`                 ], $ τ -> bool $,

      table.cell(colspan: 2, [ Control Flow ]),
      [abort `s`                   ], $ τ -> ⊥ $,
    ),
  ) <builtin_excerpt>
]

@builtin_excerpt shows an exerpt of the Nix builtins, the full list can be found in @all-builtins. The record‑ and list‑oriented builtins extend these constructs in predictable ways and motivate the need to closely model record and array values to form precise return types for these functions. Both the `attrValues` and `catAttrs` functions use type connectives in their return type, a feature that is further discussed in @connectives.

The deserializers `fromJson` and `fromToml` pose challenges for static typing: given an arbitrary string, they may produce any Nix value. Absent a faithful model of the decoding process, the most precise static approximation of the return type is the unknown type $star.op$ that directly motivates the need for an _occurence typing_ or _soft_ typing system.

The `is*` predicates can be used to inspect the type of expressions at runtime. This feature enables refinement of types in conditionals and motivates the addition of _occurrence typing_. An example expression is `if isBool(x) then !x else false`. The positive branch of this conditional can be typed under the assumption, that x is a bool because it is only reachable under this condition. Furthermore, it is possible to write functions that act accordung to a supplied type. This motivates the addition of _overloaded_ functions to the typesystem.

A combination of the`toFile` builtin and the import function allow evaluation of arbitrary strings. The following expression:

```nix
let code = "let age = 13 in age;" in import (builtins.toFile "dyn.nix" code) → 13
```
writes an arbitrary string to a file and immediately reads and executes the resulting file. This features is equally strong to the generic `eval` struct ot Javascript of Python.

Finally, `abort` interacts with control flow by terminating evaluation unconditionally; it therefore has return type ⊥, a type that can not be inhabitated by any value.

= Syntax <syntax>
#syntax <syntax-table>

@syntax-table shows the syntax of nix with. The syntax of the literals follows the official regex formulas of the informal nix specification @nix-language-2-28. Records follow a standard notation with multiple `key = value;` assignments. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. Arrays behave similarly and can be  concatenated with the only unintuitive nix-specific distinction that a space is used as separator. Both datatypes are generally _immutable_, but there are concat operations that can be used to create new, bigger datatypes. Furthermore, records can be accessed using labels, strings and a dynamically computed expression. Label access is standard and access by string a common technique in real world programming languages to allow more characters in record keys. Records come equipped with a check operation $t space ? ρ$ that returns a boolean as a result and the or-operator to specify a default value in case the previous check turned out to be negative.

Functions take one argument, a _pattern_. This pattern can be a single label or adhere to a record-structure, allowing multiple fields to be present, possibly with _default arguments_. This way, a function taking multiple arguments can be created without resorting to currying. These functions can be called with a record from which the "single arguments" are taken and form a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.

Patterns can be marked _open_ with the ellipsis (…), and otherwise regarded as _closed_. Arguments can be given a default value using the `?` syntax. The exemplary function pattern `{a, b ? "pratt", …}` is an _open_ pattern with a default value of "pratt" for the label $b$ and a mandatory argument $a$. The whole argument can be referred to in the function body by binding it to a variable using the \@-syntax `contact @ { name, surname, tel, email}: contact.name`. This binding can also occur behind the argument but we use a rewrite to handle both forms at the same time.

Let-expressions can define multiple bindings $a_1 = t_1; … ; a_n = t_n$, possibly referencing each other in a _recursive way_. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. They can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with-statement_ expects an arbitrary expression that reduces to a record. Every field from the given record is added to the scope of the following expression without shadowing variables bound by other means. See @quirks for further details.

=== Paths
There are three different syntactic objects that deserve the name `path` in our formalization. The first one being the syntactic path-object $rho.alt$ that points to a location in a filesystem. A path can be _absolute_, starting with a `/`, _relative_ from the home directory `~/` or relative to the file where it is stated `./`. All these notations are standard in the linux world. The second construct is a search-path $Rho$ of the form `<nixpkgs>`, where the entangled name is looked up in the path returned by `builtins.nixPath`. The usecase of this construct is to easily refer to a package entry that is assumed to be "globally accessible" – a quality of life feature.

The last path-like construct is a sequence of record accesses ρ `r.l.l.l` that "reach" to a field of a deeply nested record like `{a: {b: {c: {}}}}`. We note that even though nix does have a null-type, lookups of fields that do not exist, immediately trigger an error instead of returning null. To mitigate unwanted exceptions, it is possible to check for the presence of fields in an argument using the ?-operator. This operator expects a record as first operand and a path as second `{} ? a.b.c.d`. A subroutine will then iteratively access the fields and short-fuse with false in case any path-element is missing. As mentioned in @string_interpolation, a path can contain string interpolation elements.

== Reduction Rules
#reduction <reduction>
#figure(caption: [Deferred Substitutions.], substitutions) <substitution>

- Same semantic
- abs/with -> with
- rec/nonrec -> inherit
- Deferred substitution (with example)
- unfold/indirects
- elaboration of with/inherit features
- function matching

We follow the semantics of broekhoff and krebbers @verified. We assume prope operational semantics for the primitive Algebraic, Logic, Pipe and Comparison operators and give explicit transition rules for Records and Array operators. We use a call-by-name evaluation order which is operationally equivalent to lazy evaluation but less performant in interpreters. We also use the _deferred substitutions_ introduce by broekhoff and krebbers @verified to properly handle the weaker binding of the with-construct and rec/nonrec annotations to. Because of the problematic `{inherit x;} -> { x = x;}` we need to track for every field, whether it is recursive or not which is done with the recursive kind. $p arrow.squiggly t$ means the file pointed to with p is evaluated and reduces to t.

@substitution shows deferred substitutions. Variables are annotated by the type they should be substituted with. The first two cases handle bindings of different strength with `abs`-bindings taking precedence.

#matching <matching>

@matching is a recursive procedure and ` m ~ p ~ α` read as "Pattern am is matched with record p giving a substitution α". The rules account for open and closed patterns as well as recursiveness because they reduce to recursive records that are re-interpreted as
a

= Finding a Type System <ts-dicsussion>
Nix is a dynamically typed, lazy, and purely functional language. Its core features include extensible records, pattern-based functions with parameter destructuring, first-class labels, overloaded operators, and a reduction semantic with two levels of binding power. In addition, the language provides 78 built-in functions that operate on attribute sets and lists, access the execution environment, and support limited forms of type reflection.

The interaction of these features yields a language of considerable expressive power and to date, no type system is known that can capture the full expressiveness of Nix while preserving soundness and practical tractability. Consequently, any attempt at static typing must restrict itself to a carefully chosen subset of the language in order to obtain a well-behaved and analyzable system.

In practice, most users engage with nix through the module systems of nixos and home-manager to configure their operating system or user environment. A full example of this intersection can be found in @modulesystem. Within this context, autocompletion for option values that consist of a type, default-value, example and description is most valuable. Although both module systems provide online services #footnote(link("https://home-manager-options.extranix.com/")) #footnote(link("https://search.nixos.org/options")) to gain this information, there exists no satisfactory solution that works in IDEs #footnote("Integrated Development Environment").

The module system is a part of the nix standard library and utilizes the languages core features, such that full option type inference is a feature that builds upon nix-language type inference. It is in principle possible to determine types for option values and improve completion, but whether the precision needed to faithfully deduce informative types for options is attainable is not immediately clear. Also, type inference would not provide the accompanying descriptions and examples. It is thus a natural direction for future work to quantify the benefits of Nix type inference for option autocompletion and to evaluate whether leveraging existing web‑portal metadata for completion is preferable to an inference-based approach.

Besides module configuration, developers use Nix as a programming language in the standard library and across the packaging ecosystem. Although this group may be smaller, the benefits of reliable type inference are substantial and have been argued to be necessary to make the language complete @nix-ts-issue and would further lower the barrier to entry for new users.

In the following sections, we survey relevant type-system features and assess their applicability to Nix.

== Wanted Properties

The nixpkgs repository is, with its 40.000 files and 120.000 packages, the biggest and most up-to-date package repository in existence, receiving approx. 80 PRs#footnote("Pull Requests") a day and having close to 100.000 commits and multiple thousand open issues and PRs. This scale has two immediate implications for type‑inference. First, the evaluation root is effectively a single entry point #footnote([Most features of Nixpkgs are reachable from #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether a flake‑based workflow is used.]), so an inference algorithm that tries to fully evaluate the whole tree would need to evaluate all 120.000 packages. Since no typeinference algorithm exists yet, we can only guess, but a _lazy_ type inference algorithm @lazy_inf might be needed to handle a syntax tree of this size.

Second, changes in an ecosystem of this magnitude are expensive and slow. A typesystem must thus force as little as possible changes to the language to retain backwards compatibility. Should changes still be needed, it is not possible to switch the whole ecosystem in one go. A _gradual typing_ discipline @gradual_siek @gradual_tobin, which interposes an unknown type $star.op$ at the boundary between dynamically and statically typed fragments, enables incremental adoption while preserving existing workflows. The same unkown type can be used to type impure builtins like the `fromJSON` and `fromTOML` which can not be given a precise type.

Many typesystems rely on type annotations to reduce the type inference complexity but Nix does not provide the syntax for type annotations. An approach where types hints are added to comments is possible and also gradually changing the syntax to acoomodate type annotions is, but full type inference in the Hindley–Milner tradition, which requires no annotations while recovering principal types is the solution with the least friction on existing code. Regardless of surface design, the inference procedure must be efficient at Nixpkgs scale; in particular, global backtracking is unacceptable, as it would render analysis prohibitively slow.

Besides the properties that are required due to the language' environment, a few properties are directly founded by the features of the language. First and foremost _recursive types_, _first class labels_ and _record concatenation_ immediately follow from the expressive record calculus. As surveyed in @records, these capabilities have been treated in the literature but were only recently combined @extensible_tabular @extensible_rec_funcs @extensible_data_adhoc and have not yet been consolidated in mainstream languages. In addition, Nix requires _parametric polymorphism_ to capture generalization across let‑bindings, and _ad‑hoc polymorphism_ to model overloaded operators and user‑defined dispatch via type inspection.

Last but not least, the combination of _subtype polymorphism_ and _type connectives_ has been shown to be applicable to a lot of programming paradigms @castagna2023programming @mlstruct and their combination leads to very expressive typesystems that can track the flow of programs and naturally implement _occurrence typing_. It has been shown that a full boolean algbra of types can encode overloading, variant types, _bounded polymorphism_, pattern type conditionals and pattern matching @castagna2023programming. Given that such systems already address many of our desiderata, adopting them as a foundation is a natural starting point.

The final list of wanted properties is thus:
- Record concatenation
- First class labels
- Recursive types
- Efficient computation
- Subtype polymorphism
- Parametric polymorphism
- Adhoc polymorphism
- Reflection
- Gradual typing (weak)
- Occurrence typing
- Type connectives


= Types
#figure(
  caption: "Types of nix.",
  types,
  placement: none,
)<types>

Todo: Types are just shown but never actually referenced


The following sections will discuss the list of properties in detail in no particular order. The types build on the calculi of Lionel Parreaux @simplesub @mlstruct @invalml and are summarized in @types. Literal terms inhabit their canonical atomic types (bool, string, path, float, int). We adopt the standard constructors for functions, records, and arrays, with the following refinement for records: a record type denotes a single mapping from a label to a type rather than an explicit enumeration of all fields. This aligns with conjunction-based accumulation of constraints on type variables during inference; see @records for details. For arrays, we provide two descriptions: a homogeneous array type and an accumulated variant that accommodates heterogeneously typed elements. To obtain a Boolean algebra of types, we admit the connectives $¬, ∨, ∧$ together with the top and bottom elements $⊤$ and $⊥$, which are, respectively, the greatest and least elements of the subtyping lattice.

Finally, we introduce a dedicated type of patterns. Although patterns mirror records syntactically, their type is cumulative across fields because introduction and elimination occur atomically at the level of the whole pattern; in contrast, each field selection `record.field` in a record yields independent constraints. We annotate openness with a superscript $b$, indicating whether a pattern is open or closed.

#basic_typing_rules <typingrules>

@typingrules shows the basic typing rules of a mlsub-derived type system. The operator typing rules are relegated to @operator-typingrules. TODO: explain.


// ------------- Longer sections ---------------
#occurrence.export
#connectives.export

// #figure(caption: "Record typing rules. TODO:explain", record_typing_rules)

#records.export
// #first-class-labels.export
#modulesystem.export

== Conclusion & Outlook
To be continued…

- Combined Castagna?
- Parreaux + fc labels?
- With construct typing alg?
- Occurrence typing for ifs?

// -------------- Bibliography ----------------
#pagebreak()
#bib

// -------------- Appendix -------------------

#set figure(placement: none)
#outline(target: heading.where(supplement: [Appendix]), title: [Appendix])
#show: appendix
#show figure: set block(breakable: true)

= List of Nix Features <all-features>
#comparison

= Nix Builtins <all-builtins>
#builtin_types

= Nix module system types <operator-typingrules>
#operator_typing_rules

= Nix module system types <module-types>
#module_types

= Typesystem comparison <ts-comp>
#ts-compare

