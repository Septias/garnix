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
The Nix programming language is used in over 40.000 files, showing its prominence, but was neglected in theoretical work until recently when work was picked up independently by S. Klähn and Breokhoff et. al. @verified @simplenix. Both works give a syntax definition and operational semantics to account for their uses, but did not cover the language in their full expressiveness. This work gives a mixed operational semantic and an overview of possible type inference approaches.


= Origin of the Nix Language <intro>
The nix package manager distinguishes itself from other package managers by one prominent feature: It has a built-in domain-specific programming language at its foundation. The homonymous programming language (nix) is a major reason for the steep learning curve to enter the ecosystem, but it is also the reason nix inhabits two fundamental properties: _purity_ and _functionality_.

The nix package manager was created in an attempt to overcome the problem of distributing software (components) between different environments (machines) without breaking them @nixos_short. This problem is more subtle than one might expect and the reason why so many package managers exist that try to tackle the problem differently. The approach take by Eelco Dolstra et al. to overcome this problem is to »apply a memory management discipline to package management«, effectively interpreting files as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. It's major achievement is a garbage-collector inspired technique to consistently track dependencies during package construction. The final _closure_ that pictorally resembles a tree of (sub-) dependencies with the built package at its root, can then be _extracted_ from the local filesystem and sent to other machines by sending every sub-component and reassembling on the other side. Since every component brings all its dependencies, it can be executed in a foreign environment.

To organize built-artifacts, nix heavily relies on the _nix store_, a read-only location that stores immutable artifacts of builds. It uses hashes to identify components and allows for quick equality checks, and therefore reusability of components. All its components live in the same location, but isolated such that different version of the same package, can be used simultaneously without inferring or overwriting each other. Since every package is a pure derivation of its dependencies, new version can easily be added to the store without having to worry about older versions such that package upgrades become _fearless_. If something should still break, prior versions live perfectly preserved in the store and can be rolled-back to in O(1) at any time @memory_to_software. Tracking component and their dependencies also gives rise to a garbage collection technique that identifies unreachable store locations by tracking "roots" and deleting them to reclaim disk space, resulting in a model that is _reliable_ and _efficient_.

To support this wide palette of features, Eelco Dostra decided to create a _domain specific_ language (DSL) that makes these an inherent property instead of a retrofitted qualities. First and foremost, one of nix' greatest strength – _reproducibility_ – is a direct consequence of the languages' functional design. When abstracting files and references to memory locations and references, one can notice that _pure functionality_ boasts all the features needed for airtight dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

The reason why nix needs to be a _lazy language_ is its costly environment where a single action – building a package – can already be very expensive, possibly taking multiple hours and lots of resources to complete. It is thus of utmost importance, that packages are only realised if actually needed. In a lazy language, values of function application are substituted as-is without further reduction i.e computation on them. In nix, where packages are stored in lazy record fields, laziness of record fields is the essential ingredient to not build packages if not _actually needed_ and saving valuable computation resources.

Nix is used in a declarative configuration system that resolves largely around records (key-value bindings) to describe the goal state of a system. Inherit- and with-statement statements are used to create new records or extract fields from existing ones. Functions can be defined using a pattern that destructures records, and a myriad of operators support record concatenation, dynamic lookups, field checks and more. Using the recursiveness of records it is possible to build self-referntial structures that are used throughout the ecosystem.

Combining all these features, the nix language is a heterogenous mix of constructs, theoretical properties, and language quirks, making type inference a challenging task.

= Overview
We will first give a briev overview of nix language quirks that make typing especially hard in @quirks. @syntax describes the nix syntax and operational semantics which to give the reader a profound understanding of the language. @ts-dicsussion discusses the properties a typesystem needs to handle nix in its entirety and discuss specific calculi and their applicability in detail.

= Quirks of the Nix Language <quirks>

The nix programming language heavily resolves around records. A primitive record (or `AttrSet` as nix calls them) is a set of key-value bindings like `{a = 2; b = 3;}` and by adding the `rec` keyword in front, record fields can reference other fields of the same record like `rec {a = b; b = 2;}; → {a = 2; b = 2;}`. Using self-referential records and the laziness of the language, it is then possible to create structural and non-structural recursion. The directly recursive `rec {x = x;}` or mutual recursive `rec {a = b; b = a;}` records are both ill-formed because they don't go through a lazy structure. The evaluator would thus cycle indefinitely but raises an error due to runtime diagnostics.  To form a well-behaved recursive definitions, one has to go through a lazy constructor like a record, array, pattern-field or let-binding. Consequently, both `rec {x = {x = x;}}` and `let x = {x = y;}; y = x; in x` are well-behaved and can be unrolled indefinitely in their x-field.

The let-binding of the second example is very similar to a recursive record in that it allows multiple, possibly mutual-referential bindings, `let a = b; b = 2; in b` but it does not need a `rec` keyword. Similar to records, it is not permitted to create mutual recursive non-constructive recursion like `let a = b; b = 2; in b` that would lead to infinite recursion. The similarity between both constructs is best shown by the let-record-binding `let {a = 2; b = 3; body = t;} ` that encloses the semicolon-separated key-value bindings of the usual let-binding in braces. It is trivial to see that both let-forms can be rewritten to one another by removing or adding the enclosing braces and explicating the body.

The remaining difference between the two is, that a record definition is a terminating expression whilst a let-binding is followed by an arbitrary possibly diverging computation. This subtle difference can be partially removed using the `with`-construct. The `with`-construct is a nix-specific feature that takes a record as argument and opens it in the following expression, adding all the bindings to the scope. It is thus possible to compute the sum of two record-fields like this: `with {a = 2; b = 2;}; a + b`. By using the with-construct in conjunction with records, it were in theory possible to form an equivalence between the two constructs: \
`with (rec {a = b; b = 2}); t` ↔︎ `let {a = b; b = 2; body = t;}`

if it were not for with' unexpected shadowing behaviour.

A with statement binds _weakly_, meaning it will not shadow any binding that was added by any functions or let-bindings. An expression like `let a = 2; in (with {a = 3;}; a)` will thus evaluate to 2 instead of 3 even though `a = 3;` binds "after" the initial binding of `a = 2;`. Similar is the case for function bindings `a: (with {a = 2;}; a) → a`, inherited bindings, and records `rec {a = 3; b = (with {a = 4;}; a);} → {a = 3; b = 3;}`. When stacking with-bindings, shadowing behaves as expected: `with {a = 1;}; with {a = 2;}; a → 2`.

The reason for this unexpected shadowing behaviour is the intended use to simulate a _module system_. A record viewed as module encapsulates its definitions and by using the width-statement, it can be opened, effectively adding all its definitions to the following expression. Since modules tend to have quite a lot of definitions, the weak binding prevents unwanted shadowing of existing variables. This module system shows its full potential when used together with the built-in `import` function, that takes a file, evaluates the content and returns the computed value. Using these two constructs in unison, it is possible to import a file `./modules.nix` and add all its definitions to the following scope `with (import ./modules.nix); t`, very similar to a global module import.

The import-statement can occur anywhere in the syntax-tree, including recursive functions, struct-fields, arrays and literally every other construct. Since the import statement will "fill in" the files evaluated content literally, it is possible to follow the statement up with an arbitrary value. A statement like `import ./modules.nix {system = "nixos_x86"}` will first evaluate the file `modules.nix` and interpret the following term as an argument to a function, resulting in a function application. `import ./modules.nix {system = "nixos_x86";} → f {system = "nixos_x86"}`. The integrity of functions in files is not checked statically, but it is a frequently used pattern to separate code into different files. To illustrate the point, see @module_example for a full example.

Calling a function with a record argument enjoys its own syntactic sugar. By using a _pattern-functions_, one can destructure a given record-argument into its fields, giving a semantic with functions that can take multiple arguments, possibly with default-arguments. If we think of the record constructor `{}` as introduction-rule, then functions with patterns are the logical equivalent that eliminate them. It is thus a natural choice to encapsulate function patterns in the very same brackets that records use. A function with the signature `{a, b}: a + b` expects one argument of record-type and "extracts" the fields a, b from it, adding them to the function bodies scope. Together, record construction and pattern-function application act as inverses to one another `a: ({a}: a) {a = a;} == a`.

A function pattern in the primitive case `{a, b}: a` will expect exactly the fields a and b in a given record argument. Open patterns are denoted by the ellipsis `{a, ...}` and allow the argument to have arbitrary many other fields. Default arguments can be defined using a question-mark syntax `{a ? "nikita", ...}` followed by an arbitrary expression. An unexpected behaviour of patterns is their laziness and recursiveness. All fields of a pattern are added to the whole function context (including the pattern itself) and can henceforth be referenced.

This is a non-trivial feature, because it allows (arguably unidiomatic) recursion in patterns `{a ? b, b }: a` and functions like `({a ? (with {a = 3;}; b), b }: a){b = 2;}` that make shadowing behaviour and termination qualities hard to assess. For example, the given function application will evaluate to 2 in its current form, but changing the with-bound variables name to `a` as in `({a ? (with {a = 3;}; a), b }: a){b = 2;}` leads to erroneous termination because of infinite recursion. To see why, one has to remember that with-bindings are weakly-binding and hence when looked up, the evaluator will refer to the pattern variable a instead of the with-bound one, forming the cycle.

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

String interpolation is a common feature in modern programming languages because of its convenience to build complex strings from pieces that are string-convertible. When used inside a string or path, the term t of the expression `${t}` will has to evaluate to a string and will be inserted literally. It is thus possible to easily create complex strings `let name = "john"; in "Hello ${name}"` or programmatically access file locations:

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

The nix builtins extends the language' features beyond the simply syntactic ones. It comprehends functions that manipulate the languages datatypes, functions that inspect types, impure functions that bring the execution environment into scope and functions that infer with the usual program execution flow. We will now give a short overview over the builtin functions relevant to type inference. They will be further discussed in section \@builtin_types_discussion but we want to make the reader aware of a few of the tricky builtins upfront.

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
#syntax

Nix supports the usual literals of fully fledged languages as well as a multi-line string and paths. The syntax is given following the official regex formulas of the informal nix specification @nix-language-2-28. Records follow a standard notation with multiple `key = value;` assignments. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. Arrays are introduced in a similar fashion, where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as separator. Both datatypes are generally _immutable_, but there are concat operations that can be used to create new, bigger datatypes. Furthermore, records can be accessed using labels, strings and a dynamically computed expression. Label access is standard and access by string a common technique in real world programming languages to allow more characters in records keys. Records also have a check operation $t space ? ρ$ that returns a boolean as a result and the or-operator to specify a default value in case the previous check turned out to be negative.

Functions take one argument, a _pattern_. This pattern can be a single label or adhere to a record-structure, allowing multiple fields to be present, possibly with _default arguments_. This way, a function taking multiple arguments can be created without resorting to currying. These functions can be called with a record from which the "single arguments" are taken and form a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.

Patterns can be marked _open_ with the ellipsis (…), and otherwise regarded as _closed_. Arguments can be given a default value using the `?` syntax. The exemplary function pattern `{a, b ? "pratt", …}` is an _open_ pattern with a default value of "pratt" for the label $b$ and a mandatory argument $a$. The whole argument can be referred to in the function body by rebinding it to variable using the \@-syntax `contact @ { name, surname, tel, email}: contact.name`. This binding can also occur behind the argument but we use a rewrite rule to catch this case instead of adding it to the syntax definition.

Let-expressions can define multiple bindings $a_1 = t_1; … ; a_n = t_n$, possibly referencing each other in a _recursive way_. It is also allowed but regarded deprecated to enclose these bindings in braces. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. They can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with-statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing variables bound by other means. See @quirks for further details.

=== Paths
There are three different syntactic objects that deserve the name `path` in our formalization. The first one being the syntactic path-object $rho.alt$ that points to a location in a filesystem. A path can be _absolute_, starting with a `/`, _relative_ from the home directory `~/` or relative to the file where it is stated `./`. All these notations are standard in the linux world. The second construct is a search-path $Rho$ of the form `<nixpkgs>`, where the entangled name is looked up in the path returned by `builtins.nixPath`. The usecase of this construct is to easily refer to a package entry that is assumed to be "globally accessible" – a quality of life feature.

The last path-like construct is a sequence of record accesses ρ `r.l.l.l` that "reach" to a field of a deeply nested record like `{a: {b: {c: {}}}}`. It is to note, that even though nix does have a null-type, lookups of fields that do not exist, immediately trigger an error instead of returning null. To mitigate unwanted exceptions, it is possible to check for the presence of fields in an argument using the ?-operator. This operator expects a record as first operand and a path as second `{} ? a.b.c.d`. A subroutine will then iteratively access the fields and short-fuse with false in case any path-element is missing. As mentioned in @string_interpolation, a path can contain string interpolation elements.

== Reduction Rules
About this section: I adopted my reduction rules for the ones used by broekhoff and krebbers, changing only the initial syntax (minor changes and dynamic lookups and record-setters) and simplifying the record operators to reduction rules. The resulting reduction rules are not lazy in the first argument, but give a better intuitive understanding than the relation-based by broekhoff and krebbers. Since this paper focuses on type inference, the termination analysis is not too important for us. I still think about giving a proper lazy reduction semantic that improves on the call-by-need one by broekhoff and krebbers. Using this it would be possible to remove the (I argue) quite verbose record and variable tagging (with/abs and rec/nonrec), leading to a clearer semantic.

#reduction <reduction>
#figure(caption: [Deferred Substitutions], substitutions) <substitution>

We largely follow the semantics of broekhoff and krebbers @verified. We assume prope operational semantics for the primitive Algebraic, Logic, Pipe and Comparison operators and give explicit transition rules for Records and Array operators. We use a call-by-name evaluation order which is operationally equivalent to lazy evaluation but less performant in interpreters. We also use the _deferred substitutions_ introduce by broekhoff and krebbers @verified to properly handle the weaker binding of the with-construct and rec/nonrec annotations to. Because of the problematic `{inherit x;} -> { x = x;}` we need to track for every field, whether it is recursive or not which is done with the recursive kind. $p arrow.squiggly t$ means the file pointed to with p is evaluated and reduces to t.

@substitution shows deferred substitutions. Variables are annotated by the type they should be substituted with. The first two cases handle bindings of different strength with `abs`-bindings taking precedence.

#matching <matching>

@matching is a recursive procedure and ` m ~ p ~ α` read as "Pattern am is matched with record p giving a substitution α". The rules account for open and closed patterns as well as recursiveness because they reduce to recursive records that are re-interpreted as


= Finding a Type System <ts-dicsussion>
Nix is a dynamically typed, lazy and purely functional language. It features extensible records, functions with expressive patterns, first-class-labels, overloaded operators, tricky shadowing behaviour (with-construct) and also boasts 78 builtin function, that manipulate records and arrays, access the execution environment and reflect on the languages types. There exists no type systems powerful enough to handle the full suite of properties needed to statically type such a language yet, leaving us with the only option to pick a subset of nix' features to form a well-behaved type system.

A type system is nothing without a usecase. Research languages might seek theoretical properties to reject less valid programs and giving more static properties in the process but for typestems that have an existing language as foundation, _usability_ is the most important metric. In the following section we want to give a broader overview over type system features and their applicability to nix.

The everyday user of nix utilizes the module systems of nixos and home-manager to configure their operating system or user environment. The most beneficial feature is thus autocompletion for option values that consist of a type, default-value, example and description. Both module systems provide online services #footnote(link("https://home-manager-options.extranix.com/")) #footnote(link("https://search.nixos.org/options")) to provide this information, but up to this date, no satisfactory solution exists that works in IDEs #footnote("Integrated Development Environment").

The module system is a part of the nix standard library and utilizes most of the languages core features, such that full option inference is a feature that builds upon nix-language type inference. With nix-type inference it could in theory be possible to infer the exact type for option values and help programmes complete these but "example" sections would probably still be missing. It is thus an interesting topic for further research to estimate the gains of type inference in nix in regard to option-autocompletion or whether a system that autocompletes the existing information from the web-portals is preferable.

The second user-class uses nix as a programming language, mostly in the standard library and software packages around the nix ecosystem. Even though this might only represent a fraction of the usage, the benefit of good type inference is still huge and "needed to make the language complete" @nix-ts-issue. A good typesystem would also reduce the hurdle to get into nix programming which normal users mostly avoid.

We want to derive a typestem that helps in writing the language itself instead of a DSL that can only handle the module system. To grasp nix in its entirety, a few properties need to be satisfied...


== Wanted Properties
Th nixpkgs repository is with its 40.000 files the biggest and most up to date package repository in existence receiving approx. 80 PRs#footnote("Pull Requests")  a day. Adding mandatory type annotations to an ecosystem of such a size would be an undertaking too big to succeed. Such a situation immediately calls for a gradual type system @gradual_siek @gradual_tobin that allows typed and untyped code to coexist. Type annotations could then be gracefually added (i) as comment or (ii) directly to the language. The first approach has shown fruitful in javascript and typescript, but comments have their own usage and should not be polluted with type annotations. (ii) could be possible, but updating all files would still be a massive undertaking. The way harder but most useful approach is thus  _full type inference_ à la. ML (cite ml?) where no annotations have to be given to begin with. At best it would be _principaled_ type inference with one most general type that can always be inferred without _backtracking_, because that would, again, be unusable slow regarding the size of the ecosystem. #footnote([All of nix' features root in a single file at #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether you use a flake based system or not.]) Since the whole codebase roots in one file, this also calls for a _lazy_ type system.

Another important consideration for a typesystem is expressiveness. It is obviously possible to type every variable at an _unknown type_ $star.op$ but that would not give meaningful insight for the user. On the other hand, making a typesystem to complex might lead to unwanted properties like undecidability or non-termination @undecidable. Again, the proper path strikes the balance between expressiveness and simplicity.

Most general purpose typesystem come equipped with some form of polymorphism, to abstract over generic program behaviour. Due to its usefulness, polymorphsim is one if not the most researched topic with a myrriad of different kinds:  _parametric polymorphism_, _first-class polymorphism_, _subtyping polymorphism_, _Ad-hoc polymorphsim_, _Presence polymorphism_, _Explicit Polymorphism_, _Implicit Polymorphsim_ just to name a few. It is not immediately obvious which types of polymorphism is the right for your type system but we can conclude from the language.

It is obvious that  parametric polymorphism is needed because nix features let-bindings.   Last but not least, _subtype polymorphism_ is a common technique that has proven useful especially in conjunction with _type-connectives_. Type connectives are borrowed from logic connect otherwise unrelated types using unions, intersection and negation. They are especially useful in conjunction with _flow-respective typing_, a technique used in flow to narrow types in conditionals.

The nix language furthermore allows to reflect over its types using the builtin (isBool, isAttr, etc.) functions so a reflexive type system is needed. Nix also allows to compute record labels and such labels need to be _first class_ in the language. Last but not least, nix is a lazy and recursive language with hard-to-track shadowing semantics. Due to recursiveness in records, let-bindings, and patterns, recursive types are a must in the language.

The final list of wanted properties is thus:
- Efficient computation
- Record concatenation
- First class labels
- Recursive Types
- Occurrence typing
- Gradual typing (weak)
- Laziness
- Subtype Polymorphism
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

