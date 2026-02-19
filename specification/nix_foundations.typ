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
The Nix programming language is employed in over 40,000 source files, underscoring its practical significance and widespread adoption. Despite this prominence, it has received comparatively little attention in the theoretical literature until recently. Independent efforts by S. Klähn and by Breokhoff et al. have begun to address this gap. Both works provide formalizations of the language’s syntax and operational semantics tailored to their respective objectives. However, neither treatment captures the full expressive range of Nix as it is used in practice.

The present work extends this line of research by developing an operational semantics that more comprehensively reflects the language’s behavior, and by systematically examining potential approaches to type inference in this setting.

= Origin of the Nix Language <intro>
The nix package manager distinguishes itself from other package managers because it has a built-in domain-specific programming language at its foundation. This eponymous language, Nix, constitutes both a major source of the ecosystem’s steep learning curve and the foundation of its core properties — most notably _purity_ and _functionalilty_.

The nix package manager was developed to overcome the problem of distributing software (components) between heterogenous environments without inflicting breakage @nixos_short. This problem is deceptively complex and has motivated a wide range of package management strategies. Eelco Dolstra et al. conceptualizes package management as as memory management discipline. In this analogy files are interpreted as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. This lead to a _garbage-collector_ inspired technique to consistently track dependencies during package construction. By tracking dependencies, builts resemble a closure: a complete dependency graph rooted at the built artifact and containing all recursively required components. Because this closure is self-contained, it can be transferred to another machine and reconstructed without relying on the target system’s ambient state. Each artifact therefore executes independently of the host environment.

To organize built-artifacts, nix relies on the _nix store_, a read-only directory containing immutable buildartifacts. Each artifact is identified by a cryptographic hash derived from its inputs, enabling efficient equality checks and facilitating maximal reuse of existing components. Multiple versions or variants of a package may coexist in the store without interference, since each is uniquely addressed and isolated. Because build outputs are pure functions of their declared inputs, new versions can be added without mutating or invalidating prior ones. This immutability enables reliable rollbacks and atomic upgrades. @memory_to_software  Moreover, the explicit tracking of dependency references enables a garbage collection mechanism that identifies unreachable store paths by tracing from a set of designated roots, thereby reclaiming disk space while preserving consistency.

The strong properties Nix provides are a direct consequence of the underlying domain specific language Nix.  _reproducibility_ is a direct consequence of the languages' functional design where no sideffects are possible and functions are guaranteed to give the same output if the input doesn't change. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines and enables efficient caching.

Laziness is a further essential design decision. Building software artifacts can be computationally expensive in terms of time and resources. In a lazy language, expressions are evaluated only when their values are required. In Nix, attributes of records are evaluated lazily, which ensures that packages are realized only if they are actually demanded. This evaluation strategy is critical to scaling large package sets efficiently.

When abstracting files and references to memory locations and references, one can notice that _pure functionality_ boasts all the features needed for airtight dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again.

Taken together, these design choices result in a language that combines functional purity, laziness, recursive records, and domain-specific constructs for package composition. While these features enable powerful guarantees—such as reproducibility, composability, and isolation—they also introduce substantial complexity for static analysis. In particular, the interaction between laziness, recursive attribute sets, and dynamic attribute access renders type inference a nontrivial challenge.

#subsec[Overview]
We begin by providing a concise overview of characteristic features of the Nix language that render static typing particularly challenging @quirks. @syntax then presents the formal syntax and operational semantics, establishing a rigorous foundation for understanding the language’s behavior. Finally, Section @ts-dicsussion analyzes the requirements that a type system must satisfy to adequately capture the full expressiveness of Nix, and evaluates specific calculi with respect to their suitability and limitations.


= Quirks of the Nix Language <quirks>

The nix programming language revolves around records. A primitive record (or `AttrSet` in Nix) is a set of key-value bindings, for example `{a = 2; b = 3;}`. By prefixing a record with the `rec` keyword, its fields are placed in a recursive scope, allowing bindings to reference other attributes of the same record. For instanance, `rec {a = b; b = 2;};` evaluates to `{a = 2; b = 2;}`. The combination of lazynes and recursive records enables both structural and non-structural recursion, however not all forms of recursion are well-formed. Direct recursive such as `rec {x = x;}` or mutual recursive `rec {a = b; b = a;}` records without intervening lazy constructors are ill-formed. The Nix evaluator detects such cases and returns an error instead of recursing forever. Well-behaved recursive definitions must introduce a level of indirection through a lazy constructor—such as a nested attribute set, list, pattern-bound field, or let binding—so that recursion is guarded. Consequently, both `rec {x = {x = x;}}` and `let x = {x = y;}; y = x; in x` are well-behaved and can be unrolled indefinitely in their x-field.

Let-bindings are similar to records in thet they allow multiple, possibly mutual-referential bindings. For example, `let a = b; b = 2; in b` is well-formed and evaluates to 2. In contrast to recursive attribute sets, the let construct does not require an explicit `rec` keyword to enable such mutual references; recursive scope is implicit. As with recursive records, however, non-constructive recursion is disallowed. Definitions in which bindings immediately force their own evaluation—thereby yielding unguarded cyclic dependencies—lead to infinite unfolding and are rejected at runtime. Productive recursion must therefore be mediated by a lazy constructor, ensuring that self-reference is guarded.

The similarity between both constructs becomes particularly evident in the let-record-form `let {a = 2; b = 3; body = t;} ` that encloses the semicolon-separated key-value bindings of the usual let-binding in braces. The two forms are intertranslatable in a straightforward manner: a conventional let expression can be rewritten as a let-record by collecting its bindings into an attribute set and designating the body explicitly, and conversely, a let-record can be reduced to the standard form by removing the braces and separating the body from the bindings.

The remaining difference between the two is, that a record definition is a terminating expression whilst a let-binding is followed by an arbitrary possibly diverging computation. This subtle difference can be partially bridged by the `with`-construct. The `with`-construct is a nix-specific feature that takes a record as argument and opens it in the following expression, adding all bindings to the scope. It is thus possible to compute the sum of two record-fields like this: `with {a = 2; b = 2;}; a + b`. By using the with-construct in conjunction with records, it were in theory possible to form an equivalence between the two constructs: \
`with (rec {a = b; b = 2}); t` ↔︎ `let {a = b; b = 2; body = t;}`

if it were not for with' unexpected shadowing behaviour.

A with statement binds _weakly_: it will not override identifiers that were introduced by enclosing functions or let-bindings. For example `let a = 2; in (with {a = 3;}; a)` evaluate to 2 instead of 3, even though the binding `a = 3;` is added after `a = 2;`. The same behaviour arises with function parameters `a: (with {a = 2;}; a) → a`, inherited bindings, and records. The expression `rec {a = 3; b = (with {a = 4;}; a);}` will evaluate to `{a = 3; b = 3;}`. When stacking with-bindings, shadowing behaves as expected: `with {a = 1;}; with {a = 2;}; a → 2`.

This seemingly unusual shadowing discipline reflects the intended role of with as a lightweight module mechanism. An attribute set may be viewed as a module encapsulating a collection of definitions. The with construct “opens” such a module by extending the surrounding scope with its attributes. Because modules may introduce a large number of bindings, weak shadowing prevents accidental overriding of existing identifiers defined by explicit abstractions such as functions or let bindings. This pattern becomes particularly expressive in combination with the built-in import function, which evaluates a file and returns its resulting value. For example, `with (import ./modules.nix); t`, imports the attribute set defined in `modules.nix` and makes all of its bindings available in the evaluation of t. Operationally, this resembles a form of global module inclusion, while retaining the lexical scoping and compositional structure of the language.

The import construct may appear at any position in the syntax tree—including within recursive functions, attribute-set fields, arrays, and other expressions. Operationally, `import` evaluates the designated file and yields its result, which then composes with the surrounding expression in the usual way. In particular, when the imported file evaluates to a function, a subsequent term is parsed as its argument, yielding an application. For example, `import ./modules.nix { system = "nixos_x86"; }` first evaluates `modules.nix` to a function `f` and then applies it to the record argument, i.e., `import ./modules.nix { system = "nixos_x86"; } → f { system = "nixos_x86"; }`. Although the language does not statically verify the shape of imported files, organizing code across files via `import` is idiomatic and widely used; see @module_example for a complete illustration.

Function application with a record argument admits dedicated syntactic support via _pattern functions_. A pattern function destructures its record argument into fields, thereby providing a convenient notation for functions that conceptually take multiple parameters, possibly equipped with default arguments. In the same way that record construction `{}` serves as an introduction form, pattern functions constitute the corresponding elimination form—hence the shared use of braces. For example, `{a, b}: a + b` expects a single record argument and binds its fields `a` and `b` into the body's scope. Taken together, record construction and pattern-function application behave as logical inverses, e.g., `a: ({a}: a) { a = a; } == a`.

A primitive pattern such as `{a, b}: a` requires exactly the fields `a` and `b` in its record argument. Open patterns, written `{a, ...}`, admit additional, unspecified fields. Default arguments are supplied via the question‑mark notation `{ a ? "nikita", ... }`. Crucially, pattern bindings are lazy and recursive: the fields introduced by the pattern are added to the function’s scope as a whole (including the pattern itself) and may be referenced mutually throughout the body.

These scoping and evaluation conventions admit subtle forms of recursion. For example, `{ a ? b, b }: a` appears innocuous yet relies on a self‑referential default. Likewise, `({ a ? (with { a = 3; }; b), b }: a) { b = 2; }` interacts with with‑binding and weak shadowing in ways that complicate termination reasoning. The latter evaluates to 2; however, renaming the with‑bound identifier to `a` as in `({ a ? (with { a = 3; }; a), b }: a) { b = 2; }` yields divergence: because with‑bindings are weak, name resolution prefers the pattern‑bound `a` over the with‑bound one, thereby creating a cycle.

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
  kind: "figure",
  supplement: [Figure],
) <module_example>


== String Interpolation <string_interpolation>

String interpolation is a common feature in modern programming languages because of its convenience to build complex strings from pieces that are string-convertible. When used inside a string or path, the term t of the expression `${t}` will evaluate to a string and will be inserted literally. It is thus possible to easily create complex strings `let name = "john"; in "Hello ${name}"` or programmatically access file locations:

```
let conf_file_of = (name: readString /home/${name}/.config/nu); in conf_file_of "john" → /home/john/.config/nu
```

The same syntax can also be used in _dynamic bindings_, _field-accesses_ and _field-checks_. It is for example possible to define a record with a computed label `{ ${"foo"} = bar;} → {foo = bar;}`. Similarly, it is possible to _access_ records with a dynamically computed label `{ a = 1; b = 2; c = 3;}.${t}`, and check for record fields
```
{ players.gaben.health = 10; players.ruben.health = 7; } ? players.${"ruben"}.health → 7```

Record labels are thus first class inhabitants of the language.


== Dunder Methods <dunder>

The nix programming language provides special dunder variables and record fields that interact with the language and provide information about the current execution environment. Of the six dunders we found during our analysis, only the `__functor` field is described in the nix language manual @nix-language-2-28, the others can only be found in the evaluator tests @nixos_tests or nixpkgs source code @special_args. We will follow this up with a short description of the most important dunder methods.

The special record field `__overrides` is recognized by the language evaluator and can be used to override fields of the record. `rec { a = 1; __overrides = { b = 2; };} → {a = 1; b = 2; __overrides = {..};}`. Notice though, that this example only works because the record is declared recursive. If that keyword was to be removed the whole expression would reduce to `{a = 1; __overrides = {..};}`, ignoring the overridden field. Also, the combination with string interpolation behaves unexpected, because `rec {"${"foo"}" = "bar"; __overrides = { bar = foo; };}` will fail due to foo being undefined. This is in contradiction with usual records ` rec { ${"foo"} = "bar"; bar = foo;}` where name resolution works as expected. We think that these contradictions and unexpected behaviours roots in inconsistent evaluator implementation.

The special `__functor` field can be used to turn a record into a function. `({ __functor = self: x: self.foo && x; foo = false; } // { foo = true; }) true -> true`. The function defined in the `__functor` field will be called with itself and the supplied argument, allowing for recursive function definitions. It is also possible to stack such functors special fields `{__functior = {__functor = {…}}}`.

The last special arguments `__toString` that can be used to generate representation of records as strings. It is thus possible to call the function `builtins.toString {__toString = x: "a record";}` with a record which would otherwise fail.

Nix also provides three impure _dunder variables_ `__currentSystem` ,`__nixPath` and  `__currPos` that give information about the current system, where the nix executable is stored and the current evaluators cursor position in the file. These variables bring the execution environment into scope and are further discussed in @impurities.

During our analysis we also found further occurrences of `__structuredAttrs`, `__splicedPackages`, `__allowFileset`, `__impure`, ... that could have effects on the evaluator, but their use seems to be mixed with standard variable naming schemes for "hidden" variables such that a clear distinction needs further work.



== Nix Builtins

The nix builtins extends the language' features beyond the simply syntactic ones. It comprehends functions that manipulate the languages datatypes, functions that inspect types, impure functions that bring the execution environment into scope and functions that infer with the usual program execution flow. We will now give a short overview over the builtin functions relevant to type inference. They will be further discussed in section \@builtin_types_discussion but we want to make the reader aware of a few of the tricky builtins in @builtin_excerpt upfront.

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
      [elem `x xs`                 ], $ recordType -> l -> bool $,
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

The record and array related builtins extend the construct in expected fashion. The fromJson and fromToml functions are interesting because they return valid nix code given a string. From a typesystem perspective this is the worst case, because no assumptions can be made about the returned value (if one does not simulate this transformation), leading to an unknown type $star.op$ for them. The functions starting with "is" are functions to reflect on types at runtime. Using this type inspection, it is possible to overload functions and apply occurrence typing. The last example we want to give is the abort function that interacts with the program flow by terminating early. This function never returns and has to be given the type ⊥.

Using `builtin.toFile` it is possible to eval any string-code from nix:

```
let code = "let age = 13 in age;" in import (builtins.toFile "dyn.nix" code) → 13
```

= Syntax <syntax>
#syntax <syntax-table>

@syntax-table shows the syntax of nix with the usual literals of fully fledged languages as well as a multi-line string and paths. The syntax is given following the official regex formulas of the informal nix specification @nix-language-2-28. Records follow a standard notation with multiple `key = value;` assignments. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. Arrays are introduced in a similar fashion, where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as separator. Both datatypes are generally _immutable_, but there are concat operations that can be used to create new, bigger datatypes. Furthermore, records can be accessed using labels, strings and a dynamically computed expression. Label access is standard and access by string a common technique in real world programming languages to allow more characters in records keys. Records also have a check operation $t space ? ρ$ that returns a boolean as a result and the or-operator to specify a default value in case the previous check turned out to be negative.

Functions take one argument, a _pattern_. This pattern can be a single label or adhere to a record-structure, allowing multiple fields to be present, possibly with _default arguments_. This way, a function taking multiple arguments can be created without resorting to currying. These functions can be called with a record from which the "single arguments" are taken and form a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.

Patterns can be marked _open_ with the ellipsis (…), and otherwise regarded as _closed_. Arguments can be given a default value using the `?` syntax. The exemplary function pattern `{a, b ? "pratt", …}` is an _open_ pattern with a default value of "pratt" for the label $b$ and a mandatory argument $a$. The whole argument can be referred to in the function body by rebinding it to variable using the \@-syntax `contact @ { name, surname, tel, email}: contact.name`. This binding can also occur behind the argument but we use a rewrite rule to catch this case instead of adding it to the syntax definition.

Let-expressions can define multiple bindings $a_1 = t_1; … ; a_n = t_n$, possibly referencing each other in a _recursive way_. It is also allowed but regarded deprecated to enclose these bindings in braces. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. They can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with-statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing variables bound by other means. See @quirks for further details.

=== Paths
There are three different syntactic objects that deserve the name `path` in our formalization. The first one being the syntactic path-object $rho.alt$ that points to a location in a filesystem. A path can be _absolute_, starting with a `/`, _relative_ from the home directory `~/` or relative to the file where it is stated `./`. All these notations are standard in the linux world. The second construct is a search-path $Rho$ of the form `<nixpkgs>`, where the entangled name is looked up in the path returned by `builtins.nixPath`. The usecase of this construct is to easily refer to a package entry that is assumed to be "globally accessible" – a quality of life feature.

The last path-like construct is a sequence of record accesses ρ `r.l.l.l` that "reach" to a field of a deeply nested record like `{a: {b: {c: {}}}}`. It is to note, that even though nix does have a null-type, lookups of fields that do not exist, immediately trigger an error instead of returning null. To mitigate unwanted exceptions, it is possible to check for the presence of fields in an argument using the ?-operator. This operator expects a record as first operand and a path as second `{} ? a.b.c.d`. A subroutine will then iteratively access the fields and short-fuse with false in case any path-element is missing. As mentioned in @string_interpolation, a path can contain string interpolation elements.

== Reduction Rules
About this section: I adopted my reduction rules for the ones used by broekhoff and krebbers, changing only the initial syntax (minor changes and dynamic lookups and record-setters) and simplifying the record operators to reduction rules. The resulting reduction rules are not lazy in the first argument, but give a better intuitive understanding than the relation-based by broekhoff and krebbers. Since this paper focuses on type inference, the termination analysis is not too important for us. I still think about giving a proper lazy reduction semantic that improves on the call-by-need one by broekhoff and krebbers. Using this it would be possible to remove the (I argue) quite verbose record and variable tagging (with/abs and rec/nonrec), leading to a clearer semantic. Because of that I haven't fully finished the reduction semantic. The general idea is there but for example the distinction between rec/nonrec is done in the reduction semantic but the syntax definition still relies on whole record-tagging (`{} vs. rec {}`). Also, proper descriptions for all the operations and deferred substitutions are missing.

#reduction <reduction>
#figure(caption: [Deferred Substitutions], substitutions) <substitution>

We largely follow the semantics of broekhoff and krebbers @verified. We assume prope operational semantics for the primitive Algebraic, Logic, Pipe and Comparison operators and give explicit transition rules for Records and Array operators. We use a call-by-name evaluation order which is operationally equivalent to lazy evaluation but less performant in interpreters. We also use the _deferred substitutions_ introduce by broekhoff and krebbers @verified to properly handle the weaker binding of the with-construct and rec/nonrec annotations to. Because of the problematic `{inherit x;} -> { x = x;}` we need to track for every field, whether it is recursive or not which is done with the recursive kind. $p arrow.squiggly t$ means the file pointed to with p is evaluated and reduces to t.

@substitution shows deferred substitutions. Variables are annotated by the type they should be substituted with. The first two cases handle bindings of different strength with `abs`-bindings taking precedence.

#matching <matching>

@matching is a recursive procedure and ` m ~ p ~ α` read as "Pattern am is matched with record p giving a substitution α". The rules account for open and closed patterns as well as recursiveness because they reduce to recursive records that are re-interpreted as


= Finding a Type System <ts-dicsussion>
Nix is a dynamically typed, lazy and purely functional language. It features extensible records, functions with patterns, first-class-labels, overloaded operators, subtle shadowing behaviour (with-construct) and also provides 78 builtin function, that manipulate records and arrays, access the execution environment and reflect on the languages types. There exists no type systems expressive enough to handle the full suite of properties needed to statically type such a language yet, such that one can only pick a subset of nix' features to form a well-behaved type system.

A type system is nothing without a usecase and typestems that have an existing language as foundation, _usability_ is the most important metric. In the following section we want to give a broader overview over type system features and their applicability to nix.

The everyday user of nix utilizes the module systems of nixos and home-manager to configure their operating system or user environment. The most beneficial feature is thus autocompletion for option values that consist of a type, default-value, example and description. Both module systems provide online services #footnote(link("https://home-manager-options.extranix.com/")) #footnote(link("https://search.nixos.org/options")) to provide this information, but up to this date, no satisfactory solution exists that works in IDEs #footnote("Integrated Development Environment").

The module system is a part of the nix standard library and utilizes most of the languages core features, such that full option inference is a feature that builds upon nix-language type inference. With nix-type inference it could in theory be possible to infer the exact type for option values and help programmes complete these but descriptions and examples would be missing. It is thus an interesting topic for further research to estimate the gains of type inference in nix in regard to option-autocompletion or whether a system that autocompletes the existing information from the web-portals is preferable.

The second user-class uses nix as a programming language, mostly in the standard library and software packages around the nix ecosystem. Even though they might only represent a fraction of the usage, the benefit of reliable type inference is still huge and "needed to make the language complete" @nix-ts-issue. A good typesystem would also make it easier for new users to get into Nix programming.

Our goals is to derive a typesystem that for the core language instead of a DSL that only handles the module system. In the following section we will discuss.


== Wanted Properties
The nixpkgs repository is, with its 40.000 files and 120.000 packages, the biggest and most up to date package repository in existence, receiving approx. 80 PRs#footnote("Pull Requests") a day and having close to 100.000 commits and multiple thousand open issues and PRs. The sheer size of the ecosystem has two immediate consequences: First of all, type inference needs to take into account that all code (libraries, packages, built-tools, nixos, modulesystem, etc.) root in _a single file_ #footnote([All of nix' features root in a single file at #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether one uses a flake based system or not.]) so an inference algorithm that tries to fully evaluate the whole tree would need to evaluate all 120.000 packages. Since no typeinference algorithm exists yet, we can only guess, but it seems like a _lazy_ type inference algorithm @lazy_inf might be needed to handle such a huge syntax tree.
Secondly, it is impossible to migrate the whole ecosystem to a static typesystem with type annotations at once, meaning other measures have to be taken to add type inference. Gradual type system @gradual_siek @gradual_tobin were created to gradually transform dynamic code into static code by hosting two internal languages, a dynamic one and a static one. The boundary between the two is formed by an _unknown type_ $star.op$ that can be cast between the two worlds and allow for a slow transition into a typesave world.

Type annotations could then be added gracefually (i) as comment or (ii) directly to the language. The first approach has shown fruitful in javascript and typescript @typescript and needs no extra addition to the languages' syntax, but comments have their own usage and should not be polluted with type annotations. (ii) could be possible with serious collaboration with the nix developers, but updating all files would still be a massive undertaking. The third option is _full type inference_ à la ML (cite ml?) which needs no annotations at all while also being able to infer the _principaled_ type of expressions. This would be the approach with the least friction for language users. Other than that, an inference algorithm needs to be _efficient_, meaning we can not allow _backtracking_ during inference, because that would, again, be unusable slow regarding the size of the ecosystem.

Besides the properties that are needed because of the language' environment, a few properties are directly founded by the features of the language. First and foremost, the language needs _recursive types_, _first class labels_ and _record concatenation_ because of its strong record calculus. As we will see in @records all of these features have received some attention but were only combined in recent years @extensible_tabular @extensible_rec_funcs @extensible_data_adhoc and have not yet found their way into fully fledged mainstream languages. Furthermore, our language needs to support _parametric polymorphsim_ because of let-bindings and _ad-hoc polymorphism_ to account for overloaded operators and type-inspection that allows to write overloaded functions yourself.


Last but not least we want to discuss a few properties that are not directly needed but have shown useful in dynamic languages. The combination of _subtype polymorphism_ and _type connectives_ has been shown to be applicable to a lot of programming paradigms @castagna2023programming @mlstruct and their combination leads to very expressive typesystems that can track the flow of programs and easily implement _occurrence typing_. Using a full boolean algbra of types one can easily encode overloading, variant types, _bounded polymorphism_, pattern type conditionals and pattern matching. Since these systems already provide good solutions for most of the properties we need, starting from such a system is not a far fetch.

The final list of wanted properties is thus:
- Record concatenation
- First class labels
- Recursive Types
- Efficient computation
- Subtype Polymorphism
- Parametric Polymorphism
- Adhoc Polymorphism
- Reflection
- Gradual typing (weak)
- Occurrence typing
- Type connectives


= Types
The following sections will further discuss the wanted properties in no particular order. We start with basic types based on the work of Lionell Parreaux @simplesub @mlstruct @invalml with type connectives, top and bottom types, atomic record types and also monomorphic and polymorphic type variables.

#figure(
  caption: "Types of nix.",
  types,
  placement: none,
)<types>

@types shows the initial types we use. We model all literal syntax categories with the respective atom types bool, string, path, float and int. We also add the usual types for functions, records and arrays and note that record types only define a _single_ label to type mapping instead of multiple. This is due to the use of type conjunctions and their accumuation on type variables during type inferene. This mechanism is further discussed in @records. Also, we introduce two types for arrays, one for homogenous arrays of the same type and one accumulative for the case that an array has many distinct elements.
To form a boolean algebra of types we add the expected type connectives $¬, ∨, ∧$ as well as a top and bottom type which represent the least type which is subsumed by every other type and the greatest type which subsumes every other type respectively.
Lastely, we add a single type for patterns. Even thought a pattern is similar in structure to a record, the pattern type is an accumulated type with multiple fields. This distinction is made due to the syntactical difference of the two. Patterns are introduced and eliminated atomically unlike a record where every fild access `record.field` results in new, independent constraints. The superscript b can be true or false, ascribing whether the pattern is _open_ or _closed_.

#basic_typing_rules

@types shows the basic typing rules of a mlsub-derived type system.

All operator typing rules can be found in @operator-typingrules


// ------------- Longer sections ---------------
#occurrence.export
#connectives.export
#record_typing_rules
#records.export
An overview comparison table can be found in @ts-comp.
#first-class-labels.export
#modulesystem.export

== Conclusion & Outlook
To be continued…

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

