#import "functions.typ": *
#import "typesystem.typ": *
#import "./figures/reduction-compare.typ": comparison
#import "./figures/builtin-types.typ": builtin_types
#import "./figures/module-types.typ": module_types
#import "sections/occurrence.typ"
#import "sections/connectives.typ"
#import "sections/records.typ"
#import "sections/first-class-labels.typ"
#import "sections/modulesystem.typ"

#show: template
// #set figure(placement: auto)

#set document(
  title: "Securing Nix' Foundations",
  description: "Masterproject about nix syntax, operational semantics and type inference approaches",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

= Securing Nix' Foundations
The Nix programming language is used in over 40.000 files, showing its prominence, but was neglected in theoretical work until recently when work was picked up independently by S. Klähn and Breokhoff et. al. @verified @simplenix. Both works give a syntax definition and operational semantics to account for their uses, but did not cover the language in their full expressiveness. This work closes the gap by showing nix in its full expressiveness and giving an overview of possible type inference approaches.


= Origin of the Nix Language <intro>
The nix package manager distinguishes itself from other package managers by one prominent feature: It has a built-in domain-specific programming language at its foundation. The homonymous programming language (nix) is a major reason for the steep learning curve to enter the ecosystem, but it is also the reason nix inhabits two fundamental properties: _purity_ and _functionality_.

The nix package manager was created in an attempt to overcome the problem of distributing software (components) between different environments (machines) without breaking them. This problem is more subtle than one might expect and the reason why so many package managers exist that try to tackle the problem differently. The approach take by Eelco Dolstra et al. to overcome this problem is to »apply a memory management discipline to package management«, effectively interpreting files as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. It's major achievement is a garbage-collector inspired technique to consistently track dependencies during package construction. The final _closure_ that pictorally resembles a tree of (sub-) dependencies with the built package at its root, can then be _extracted_ from the local filesystem and sent to other machines by sending every sub-component and reassembling on the other side. Because all dependencies have been transferred to the new machine, the program an run in its new environment.

While this already explains the general approach, one can only understand the entirety of the package manager after taking the _nix store_ into account. The nix store is a read-only location that stores immutable artifacts of builds. It uses hashes to identify components and allows for quick equality checks, and therefore reusability of components. All its components live in the same location, but isolated such that different version of the same package, can be used simultaneously without inferring or overwriting each other. Since every package is a pure derivation of its dependencies, new version can easily be added to the store without having to worry about older versions such that package upgrades become _fearless_. If something should still break, prior versions live perfectly preserved in the store and can be rolled-back to in O(1) at any time @memory_to_software. It is also possible for a garbage collector to identify unreachable store locations by tracking "roots" and deleting them to reclaim disk space, resulting in a model that is reliable and efficient.

To support this colorful palette of features, Eelco Dostra decided to create a _domain specific_ language (DSL) that makes these an inherent property instead of a retrofitted qualities. First and foremost, one of nix' greatest strength – _reproducibility_ – is a direct consequence of the languages' functional design. When abstracting files and references to memory locations and references, one can notice that _pure functionality_ boasts all the features needed for airtight dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

The reason why nix needs to be a _lazy language_ is its costly environment where a single action – building a package – can already be very expensive, possibly taking multiple hours and lots of resources to complete. It is thus of utmost importance, that packages are only realised if actually needed. In a lazy language, values of function application are substituted as-is without further reduction i.e computation on them. In nix, where packages are stored in lazy record fields, laziness of record fields is the essential ingredient to not build packages if not _actually needed_ and saving valuable computation resources.

Nix is used in a declarative configuration system that resolves largely around records (key-value bindings) to describe the goal state of a system. Inherit- and with-statement statements are used to create new records or extract fields from existing ones. Functions can be defined using a pattern that destructures records, and a myriad of operators support _record concatenation_, dynamic lookups, field checks and more. Using the recursiveness of records it is possible to build self-referntial structures that are used throughout the ecosystem.

Combining all these features, the nix language is a heterogenous mix of constructs, theoretical properties, and language quirks, making type inference a challenging task.

== Quirks of the Nix Language <quirks>
#set raw(lang: "nix")
The following section gives a briev overview of nix-specific language features and their surprising interactions.

The nix programming language heavily resolves around records. A primitive record (or AttrSet) is a set of key-value bindings like `{a = 2; b = 3;}` and by adding the `rec` keyword in front, record fields can reference other fields of the same record like `rec {a = b; b = 2;}; → {a = 2; b = 2;}`. Using self-referential records and the laziness of the language, it is then possible to create structural and non-structural recursion. The directly recursive `rec {x = x;}` or mutual recursive `rec {a = b; b = a;}` records are both ill-formed because they don't go through a lazy structure. The evaluator would thus cycle indefinitely but raises an error due to runtime diagnostics.  To form a well-behaved recursive definitions, one has to go through a lazy constructor like a record, array, pattern-field or let-binding. Consequently, both `rec {x = {x = x;}}` and `let x = {x = y;}; y = x; in x` are well-behaved and can be unrolled indefinitely in their x-field.

The let-binding of the second example is very similar to a recursive record in that it allows multiple, possibly mutual-referential bindings, `let a = b; b = 2; in b` but it does not need a `rec` keyword. Similar to records, it is not permitted to create mutual recursive non-constructive recursion like `let a = b; b = 2; in b` that would lead to infinte recursion. The similarity between both constructs is best shown by the let-record-binding `let {a = 2; b = 3;} in t` that encloses the semicolon-separated key-value bindings of the usual let-binding in braces. It is trivial to see that both let-forms can be rewritten to one another by removing or adding the enclosing braces, rendering the latter notation valid, but obsolete.

The remaining difference between the two is, that a record definition is a terminating expression whilst a let-binding is followed by an arbitrary possibly diverging computation. This subtle difference can be partially removed using the `with`-construct. The `with`-construct is a nix-specific feature that takes a record as argument and opens it in the following expression, adding all the bindings to the scope. It is thus possible to compute the sum of two record-fields like this: `with {a = 2; b = 2;}; a + b`. By using the with-construct in conjunction with records, it were in theory possible to form an equivalence between the two constructs: \
`with (rec {a = b; b = 2}); t` ↔︎ `let {a = b; b = 2} in t`

if it were not for with' unexpected shadowing behaviour.

A with statement binds _weakly_, meaning it will not shadow any binding that was added by any other means. An expression like `let a = 2; in (with {a = 3;}; a)` will thus evaluate to 2 instead of 3 even though `a = 3;` binds "after" the initial binding of `a = 2;`. Similar is the case for function bindings `a: (with {a = 2;}; a) → a`, inherited bindings, and records `rec {a = 3; b = (with {a = 4;}; a);} → {a = 3; b = 3;}`. When stacking with-bindings, shadowing behaves as expected: `with {a = 1;}; with {a = 2;}; a → 2`.

The reason for this unexpected shadowing behaviour is the intended use to simulate a _module system_. A record viewed as module encapsulates its definitions and by using the width-statement, it can be opened, effectively adding all its definitions to the following expression. Since modules tend to have quite a lot of definitions, the weak binding prevents unwanted shadowing of existing variables. This module system shows its full potential when used together with the built-in `import` function, that takes a file, evaluates the content and returns the computed value. Using these two constructs in unison, one can import a file `modules.nix` and make all its definitions available in the following scope `with (import ./modules.nix); t`, very similar to a global module import.

The import-statement can occur anywhere in the syntax-tree, including recursive functions, struct-fields, arrays and literally every other construct. Since the import statement will "fill in" the files evaluated content literally, it is possible to follow the statement up with an arbitrary value. A statement like `import ./modules.nix {system = "nixos_x86"}` will first evaluate the file `modules.nix` and interpret the following term as an argument to a function, resulting in a function application. `import ./modules.nix {system = "nixos_x86";} → f {system = "nixos_x86"}`. The integrity of functions in files is not checked statically, but it is a frequently used pattern to separate code into different files. To illustrate the point, see @module_example for a full example.

Calling a function with a record argument enjoys its own syntactic sugar. By using a _pattern-functions_, one can destructure a given record-argument into its fields, giving a semantic with functions that can take multiple arguments, possibly with default-arguments. If we think of the record constructor `{}` as introduction-rule, then functions with patterns are the logical equivalent that eliminate them. It is thus a natural choice to encapsulate function patterns in the very same brackets that records use. A function with the signature `{a, b}: a + b` expects one argument of record-type and "extracts" the fields a, b from it, making them accessible in the function body. Together, record construction and pattern-function application act as inverses to one another `a: ({a}: a) {a = a;} == a`.

A function pattern in the primitive case `{a, b}: a` will expect exactly the fields a and b in a given record argument. Open patterns are denoted by the ellipsis `{a, ...}` and allow the argument to have arbitrary many other fields. Default arguments can be defined using a question-mark syntax `{a ? "nikita", ...}` follwed by an arbitrary expression. An unexpected behaviour of patterns is their laziness and recursiveness. All fields of a pattern are added to the whole function context (including the pattern itself) and can henceforth be referenced.

This is a non-trivial feature, because it allows (arguably unnecessary) recursion in patterns `{a ? b, b }: a` and functions like `({a ? (with {b = 2;}; b), b }: a){b = 2;}` that make shadowing behaviour and termination qualities hard to assess. For example, the given function application will evaluate to 2 in its current form, but changing the with-bound variables  name to b as in `({a ? (with {a = 2;}; a), b }: a){b = 2;}` leads to erroneous termination because of infinite recursion. To see why, one has to remember that with-bindings are weakly-binding and hence when looked up, the evaluator will refer to the pattern variable a instead of the with-bound one, forming the cycle.

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

String interpolation is a common feature in modern programming languages because of its convenience to build complex strings from pieces that are string-convertible. When used inside a string or path, the term t of the  expression `${t}` will be evaluated to a string and inserted literally. It is thus possible to create structural strings `let name = "john"; in "Hello ${name}"` or programmatically access file locations `let conf_file_of = (name: readString /home/${name}/.config/nu); in conf_file_of "john" → /home/john/.config/nu`.

The same syntax can also be used in _dynamic bindings_, _field-accesses_ and _field-checks_. It is for example possible to define a record a record with a computed label `{ ${"foo"} = bar;} → {foo = bar;}`. Similarly, it is possible to _access_ records with a dynamically computed label `{ a = 1;, b = 2; c = 3;}.${t}`, and check for record fields `{ players.gaben.health = 10; players.ruben.health = 7; } ? players.${"ruben"}.health → 7`. Record labels are thus first class inhabitants of the language.


== Dunder Methods <dunder>

The nix programming language provides special dunder variables and record fields that interact with the language and provide information about the current execution environment. Of the six dunders we found during our analysis, only the `__functor` field is described in the @nix-language-2-28, the others can only be found in the tests @nixos_tests or nixpkgs source code @special_args. We will follow this up with a short description of the most important

The special record field `__overrides` is recognized by the language evaluator and can be used to override fields of the record. `rec { a = 1; __overrides = { b = 2; };} -> {a = 1; b = 2; __overrides = {..};}`. Notice though, that this example only works because the record is declared recursive. If that keyword was to be removed the whole expression would reduce to `{a = 1; __overrides = {..};}`, ignoring the overriden field. Also, the combination with string interpolation behaves unexpectedly because `rec {"${"foo"}" = "bar"; __overrides = { bar = foo; };}` will fail due to foo being undefined. This is in contradiction with usual records ` rec { ${"foo"} = "bar"; bar = foo;}` where name resolution works as expected. We think that these contradictions and unexpected behaviours roots in inconsistent evaluator implementation.

The other field record field is the `__functor` field that can be used to turn a record into a function. `({ __functor = self: x: self.foo && x; foo = false; } // { foo = true; }) true -> true`. The functor field can be used to turn a record into an applicable function. The function defined in the `__functor` field will be called with itself and the supplied argument, allowing for recursive function definitions.

The last special arguments `__toString` that can be used to generate representation of records as strings. It is thus possible to call the function `builtins.toString {__toString = x: "a record";}` with a record which would otherwise fail.

Nix also provides three impure _dunder variables_ `__currentSystem` ,`__nixPath` and  `__currPos` that give information about the current system, where the nix executable is stored and the current evaluators cursor position in the file. These variables bring the execution environment into scope and are further discussed in \@impurities.

In the nixpkgs repository, there are further occurences of `__structuredAttrs`, `__splicedPackages`, `__allowFileset`, `__impure`, ... that could also have effects on the evaluator, but their use seems to be mixed with standart variable naming schemes for "hidden" variables such that a clear distinction needs further work.


== The Standart Library

The standart library extends the languag' features beyond the simply syntactic ones. It comprehends functions that manipulate the languages datatypes, functions for type inspections, impure functions that bring the execution environment into scope and functions that infer with the usual program execution flow. We will now give a short overview over the builtin functions relevant to type inference. The full specification with possible types is given in @all-builtins.


like `attrNames` and `attrValues` for records and `elemAt`, `head`, `length` for lists. These give inspection/reflection like features to the language, since they can be used to access fields: `r: map (attrNames rec) (x: r.x)`.

The `getAttr` field is the simple case of the former attrNames and the `hasAttr` can be used to gain flow information. The other two functions `interspectAttr` and `mapAttr` don't add too much. For arrays, we get some general property asserting functions, but since we are not a depent type system, that does not really help.

Inspecting of function args is also a funny feature. `functionArgs {a, b ? 2}: 3  -> { a = false; b = true;}` so this way one can programatically find out which arguments to supply to a function. An llm-call could then actually synthesis some arguments before calling a function xd.

- *Records*: attrNames, attrValues, getAttr, hasAttr, intersectAttrs, mapAttrs
- *Array*: elem, elemAt, head, length, listToAttrs
- *Inspecting*: functionArgs
- *Impure*: currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion
- *flow*: isAttrs, isBool, isFloat, isFunction, isInt, isList, isNull, isPath, isString


= Syntax <syn>

$overline(E)^{i ∈ 𝓘}$ denotes a repetition of a syntax construct indexed by $i ∈ 𝓘$. The index $i ∈ 𝓘$ is omitted if obvious.

#syntax

#set raw(lang: "nix")

Nix supports the usual _literals_ of fully fledged languages as well as a multi-line string and paths. The syntax is given following the official regex formulas of the informal nix specification @nix-language-2-28. _Records_ follow a standart notation where multiple fields can be defined using `key = value;` assignments to define multiple fields. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. _Arrays_ are introduced in a similar fashion, where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as separator. Both datatypes are generally _immutable_, but there are concat operations (Record-Concat and Array-Concat) that can be used to create new, bigger datatypes. Furthermore, records come equipped with the usual lookup, a dynamic label check that returns a boolean as a result, a way to specify a default value in case the previous check turned out to be negative and dynamic lookups.

Functions take one argument, a _pattern_. This pattern can be a single label or adhere to a record-structure, allowing multiple fields to be present, possibly with _default arguments_. This way, a function taking multiple arguments can be created without resorting to currying. These functions can be called with a record from which the "single arguments" are taken and form a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.

Patterns can be marked _open_ with the ellipsis (…), otherwise their are regarded as _closed_. Arguments can be given a default value using the `?` syntax. The exemplary function pattern `{a, b ? "pratt", …}` is an _open_ pattern with a default value of "pratt" for the label $b$ and a mandatory argument $a$. If one wants to refer to the whole argument in the function body, it is possible to create a global-binding for it using the \@-syntax `contact @ { name, surname, tel, email}: contact.name`. The global binding can also occur behind the argument like `{} @ glob: glob` but we use a rewrite rule to catch this case instead of adding it to the syntax definition.

Let-expressions can consist of multiple bindings $a_1 = t_1; … ; a_n = t_n$, possibly referencing each other in a _recursive way_. It is also allowed but regarded deprecated to enclose these bindings in braces. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. They can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing variables bound by other means. See @quirks for further details.

=== Paths
There are three different syntactic objects that deserve the name `path` in our formalization. The first one being the syntactic path-object $rho.alt$ that points to a location in a filesystem. A path can be _absolute_, starting with a `/`, _relative_ from the home directory `~/` or relative to the file where it is stated `./`. All these notations are standart in the linux world. The second construct is a search-path $Rho$ of the form `<nixpkgs>`, where the entangled name is looked up in the path returned by `builtins.nixPath`. The usecase of this construct is to easily refer to a package entry that is assumed to be "globally accessible" – a quality of life feature.

The last path-like construct is a sequence of record accesses ρ `r.l.l.l` that "reach" to a field of a deeply nested record like `{a: {b: {c: {}}}}`. It is to note, that even though nix does have a null-type, lookups of fields that do not exist, immediately trigger an error instead of returning null. To mitigate unwanted exceptions, it is possible to check for the presence of fields in an argument using the ?-operator. This operator expects a record as first operand and a path as second `{} ? a.b.c.d`. A subroutine will then iteratively access the fields and short-fuse with false in case any path-element is missing. As mentioned in @string_interpolation, a path can contain string interpolation elements.


== Reduction Rules

#reduction <reduction>


#basic_typing_rules
#record_typing_rules
#list_typing_rules


= Finding a Type System
Nix is a dynamically typed, lazy and pure language. It features extensible records, functions with expressive patterns, first-class-labels, overloaded operators, dynamic variable binding (with-construct) and also boasts 78 builtin function, that manipulate records and arrays, access the execution environment and reflect on the languages types. There exist no type systems powerfull enough to handle the full suite of properties needed to statically type such a language yet, leaving us with the only option to pick a subset of nix' features to form a well-behaved type system.

A type system is nothing without a usecase. The gain of research languages is advancing the field in general, creating typesystems that infer more properties, reject less valid programs and run faster. For typestems that have an existing language as a foundation, _usability_ is the most important metric. In the following section we want to give a broader overview over type system features and their applicability for nix.

The everyday user of nix utilizes the module systems of nixos and home-manager to configure their operating system or user environment. The most beneficial feature is thus autocompletion for option values that consist of a type, default-value, example and description. Both module systems provide online services @config-search @option-search to provide this information, but up to this date, no satisfactory solution exists that works in IDEs #footnote("Integrated Development Environment").

The module system is a part of the nix standart library and utilizes most of the languages core features, such that full option inference is a feature that builds upon nix-language type inference and even if it existed, is it not clear whether it would bring satisfactory results. There might be ways to add option completion to a languge server without nix type inference, hard-coding the module-system into its foundation and using the existing type hints in conjunction, but this will (for now) not be the topic of this work.

The second class of users use nix as a programming language. The use of nix as a language is mostly in the standart library and software packages around the nix ecosystem. Even though this might only be a fraction of the usage, the benefit of good type inference is still huge and as Eelco Dolstra would say "needed to make the language complete" @nix-ts-issue. A good typesystem would also reduce the hurdle to get into nix programming which normal users mostly avoid.

We want to derive a typestem that helps in writing the language itself instead of a DSL that can only handle the module system. To grasp nix in its entirety, a few properties need to be saftisfied...


== Lets talk properties
The type system should be  _lazy_ because the nix syntax tree is the biggest software package repository in existance and hard to calculate (todo: check). Adding mandatory type annotations to an ecosystem with more than 40.000 files written in it would be an undertaking too big to succeed. It might be possible to add type annotations to comments or try to add a gradual type system to the language itself, but  _full type inference_ à la. ml seems to be the best option. At best it would be _principaled_ type inference with one most general type that can always be inferred without _backtracking_ because that would, again, be unusabl regarding the size of the ecosystem. #footnote([All of nix' features root in a single file at #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether you use a flake based system or not.])

Another important consideration is expressiveness. It is obviously easy to type every variable at an `any` type but that would not give any meaningful insight for the user. On the other hand, making a typesystem to complex might lead to unwanted properties like undecidability or non-termination @undecidable. Again, the proper path strikes the balance between capability and simplicity. No matter the complexity of the typesystem, most general purpose typesystem want to have some form of polymorphism, to abstract over generic program behaviour. A whole suite of polymorphism sorts exist `parametric polymorphism`, `first-class polymorphism`, `subtyping polymorphism`, `Ad-hoc polymorphsim`, `Presence polymorphism`, `Explicit Polymorphism`, `Implicit Polymorphsim` and its hard to judge which one is the right one for your type system. Parametric type inference a la System F is powerful, but needs explicit type application, a feature we can not add because we don't have type annotations. On the other hand, it is obvious that we need ad-hoc polymorphism is needed because nix features let-bindings.

Last but not least, `subtype polymorphism` is a common technique that has proven useful especially in conjunction with _type-connectives_. Type connectives are borrowed from logic and add a way to connect othewise unrelated types with unions, intersection and negation. They are especially handy when used together with _flow-respective typing_, a technique used in flow to narrow types in conditionals. (!check naming)

Now we also have to talk about properties/features needed becaus of the language itself. First of all, the language allows to reflect over its types using the builtin (isBool, isAttr, etc.) functions so we need a type system that is _reflective_. Nix also allows to compute record labels and such labels need to be _first class_ in the language. Last but not least, nix is a lazy and recursive language with hard-to-track shadowing semantics. The language definition thus needs to adapt to the lazy behaviour, _recursive types_ need to be baked into the language and one needs a substitution logic, that respect the different strengths of binding and the hard-to-calculate with-binding.

The final list of wanted properties is thus:
1. Principaled type inference
2. Support for Record concatenation
3. Support for dynamic lookups
4. Recursive Types
5. Occurrence typing
6. Gradual typing
7. Lazyness
8. Subtype Polymorphism
9. Type connectives


== To be continue...
The following sections will further discuss the wanted properties in no particular order.

// ------------- Section
#occurrence.export
#connectives.export
#records.export
#first-class-labels.export
#modulesystem.export


// -------------- Bibliography
#pagebreak()
#bib

// -------------- Appendix

#set figure(placement: none)
#outline(target: heading.where(supplement: [Appendix]), title: [Appendix])
#show: appendix

= List of Nix Features <all-features>
#comparison

= Nix Builtins <all-builtins>
#builtin_types

= Nix module system types <module-types>
#module_types
