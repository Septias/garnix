#import "functions.typ": *
#import "./snips/comparison.typ": comparison
#import "./snips/builtin-types.typ": builtin_types
#import "typesystem.typ": *

#set heading(numbering: "1.")
// #set page(height: auto)
#show figure: set block(breakable: true)
// #set figure(placement: auto)
// #show stack: set block(breakable: true)

#set document(
  title: "Securing Nix' Foundations",
  description: "",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

= Securing Nix' Foundations
The Nix programming language is used in over 100.000 files, showing its prominance, but was neglected in theorectical work until recentently when work was picked up indepentently by S. Klähn and Breokhoff et. al. @verified @simplenix. Both works gave a reduced syntax definition and operational semantic to account for their uses, but not the language in their full expressiveness. This work closes the gap by showing nix in its full expressiveness. We also try to give an overview of possible type inference approaches.


= Origin of the Nix Language <intro>
The nix package manager distinguishes itself from other package managers by one prominent feature: It has a built-in domain-specific programming language at its foundation. The homonymous programming language (nix) is a major reason for the steep learning curve to enter the ecosystem, but it is also the reason nix inhabits two fundamental properties: _purity_ and _functionality_.

The nix package manager was created in an attempt to overcome the problem of distributing software (components) between different environments (machines) without breaking them. This problem is more subtle than one might expect and the reason why so many package managers exist that try to tackle the problem differently. The approach take by Eelco Dolstra et al. to overcome this problem is to »apply a memory management discipline to package management«, effectively interpreting files as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. It's major achievement is a garbage-collector inspired technique to consistently track dependencies during package construction. The final _closure_ that pictorally resembles a tree of (sub-) dependencies with the built package at its root, can then be _extracted_ from the local filesystem and sent to other machines by sending every sub-component and reassembling on the other side. Because all dependencies have been transferred to the new machine, the program is virtually not dependent on the new environment.

While this already explains the general approach, one can only understand the entirety of the package manager after taking the _nix store_ into account. The nix store is a read-only location that stores immutable artifacts of builds. It uses hashes to identify components and allows for quick equality checks, and therefore reusability of components. All its components live in the same location, but isolated such that different version of the same package, can be used simultaneously without infering or overwriting each other. Since every package is a pure derivation of its dependencies, new version can easily be added to the store without having to worry about older versions such that package ugrades become _fearless_. If something should still break, the old version lives perfectly preserved in the store and can be rolled-back to in O(1) at any time @memory_to_software. It is also possible for a garbage collector to identify unreachable store locations by tracking "roots" and deleting them to reclaim disk space.

To support this colorfull palette of features, Eelco Dostra decided to use a _domain specific_ language that makes em an inherent property instead of a retrofitted qualities. First and foremost, one of nix' greatest strength – _reproducibility_ – is a direct consequence of the languages' functional design. When abstracting files and references to memory locations and references, one can notice that _pure functionality_ boasts all the features needed for airtight dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

To understand why the language needs to be lazy, we have to look at the application of it and notice that package management is a costly environment. Even a single action – building a package – can already be very expensive, possibly taking multiple hours and lots of resources to complete. It is thus of utmost importance, that packages are only realised if actually needed. In a lazy language, values of function application are substituted as-is without further reduction i.e computation on them. In nix, where packages are stored in lazy record fields, lazyness of record fields is the essential ingredient to not build packages if not _actually needed_ and saving valuable computation resources.

When using the nix package manager, declarative configuration management is the major activity and the nix language thus optimized to do so. Namely, the inherit- and with-statement are powerful language constructs, that help in creating new records or extracting fields from existing ones. For dependency management, key-value fields strike the balance between verbosity and simplicity. The language thus resolves largely around record fields, taking them as function arguments, in with-statements and for its builtin functions. Using their recursiveness it is possible to build self-referntial structures that enable implementors to overwrite single fields effortlessly.

Combining all these features, the nix language is a wild zoo of constructs, theoretical properties and poses tricky shadowing and termination properties because of the combination thereof.

== Quirks of the Nix Language <quirks>
#set raw(lang: "nix")
The following section gives a briev overview of nix-specific language features and their suprising interactions.

The nix programming language heavily resolves around records. A primitive record is a set of key-value bindings like `{a = 2; b = 3;}` but by adding the `rec` keyword in front, record fields are allows to reference other fields of the same record like `rec {a = b; b = 2;}; → {a = 2; b = 2;}`. Using self-referencial records and the lazyness of the language, it is then possible to create all kinds of infinite recursion, not all of them well-behaved. Bad examples are the simply recursive `rec {x = x;}` or mutual recursive `rec {a = b; b = a;}` records. Both definitions fail because of infinite recursion during evaluation as the evaluator raises an error instead of diverging. To form a well-behaved recursive definitions, one has to utility nix' lazyness and go through a lazy constructor like a record, array, pattern-field or let-binding. Consequently, both `rec {x = {x = x;}}` and `let x = {x = y;}; y = x; in x` don't result in erronous program termination and can be unrolled indefinitely in their x-field. Similar is possible by using function-patterns and arrays which are both lazy aswell.

The let-binding of the second example is very similar to a recursive record in that it allows for _multiple_, possibly mutal-referencial bindings, `let a = b; b = 2 in b`. It does not need a `rec` keyoword but is equally not permitted to create mutual recursive non-constructice recursion like `let a = b; b = 2; in b`, because, again, there is no lazy constructor that remedies infinite computation. Their similarity is best shown by the let-rec-in-binding `let {a = 2; b = 3;} in t` that encloses the semicolon-seperated key-value bindings of the usual let-binding in braces. It is trivial to see that both forms can be rewritten to one another by removing or adding the enclosing braces, rendering the latter notation valid, but obsolete.

The remaining difference between the two is, that a record definition is a terminating expression whilest a let-binding is followed by an arbitrary possibly diverging computation. This subtle difference can be partially removed using the `with`-construct. The with construct is a nix-specific feature that takes a record as first "operand" and "opens" it in the following expression, adding all the bindings to the scope. It is thus possible to compute the sum of two record-fields like this: `with {a = 2; b = 2;}; a + b`.
By using the with-construct in conjunction with records, it were in theory possible to form an equivalence between the two constructs: `with (rec {a = b; b = 2}); t == let {a = b; b = 2} in t` – if it were not for with' unexpected shadowing behaviour.

A with statement binds _weakly_, meaning it will not shadow any binding that was added by any other means. An expression like `let a = 2; in (with {a = 3;}; a)` will thus evaluate to 2 instead of 3 even though `a = 3;` binds "after" the initial binding of `a = 2;`. Similar is the case for function bindings `a: (with {a = 2;}; a) arrow.squiggly a`, inherited bindings, and records `rec {a = 3; b = (with {a=4}; a);}`. When stacking with-bindings, the shadowing behaviour is as expected: `with {a=1;}; with {a=2;}; a -> 2`.

The reason for this unexpected shadowing behaviour is the intended use as a mean to simulate a module system. From a module-perspective, a record encapsulates its definitions and by using the width-statement, one can open a module, effectively adding all its definitions to the following expression. Since modules tend to have quite a lot of definitions, they bind weakly such that they don't overwrite locals. The "module-system" shows its full potential when used together with the built-in `import` function, that takes a file, evaluates the content and returns the computed value. Using these two constructs in unison, one can import a fiel `modules.nix` and make all its definitions available in the following scope `with (import modules.nix); t`, very similar to a global module import.

The import-statement can occurre anywhere in the syntax-tree, including recursive functions, struct-fields, arrays and literally every other construct. Since the import statement will "fill in" the files evaluated content quite literally, it is possible to follow the statement up with an arbitrary value. A statement like `import ./modules.nix {system = "nixos_x86"}` will then first evalute the file `modules.nix` and interprete the following term as an argument to a function, resulting in a function application. `import ./modules.nix {system = "nixos_x86";} -> f {system = ""}`. The integrity of functions in files is not checked statically, but it is a frequently used pattern to seperate code into different files. To illustrate the point, see @module_example for a full example.

Calling a function with a record enjoys its own syntactic sugar. By using a _pattern-function_, one can destructure a given record-argument into its fields, giving a semantic with functions that can take multiple arguments, possibly with default-arguments. If we think of the record constructor `{}` as introduction-rule, then functions with patterns are the logical equivalent that eliminate them. It is thus a natural choice to encapsulate function patterns in the very same brackets that records use. A function with the signature `{a, b}: a + b` expects one argument of record-type and "extracts" the fields a, b from it, making them accessible in the function body. Together, record construction and pattern-function application act as inverses to one another `({a}: a) {a} == a`.

A function pattern in the primitive case `{a, b}: a` will expect exactly the fields a and b in a given record argument. But it is also possible to create open patterns like `{a, ...}` that only require a single field a and allow the argument to have arbitrary many other fields. Likewise, one can give default arguments `{a ? "nikita", ...}` that are supplied in case the argument lacks said field. An unexpected behaviour of patterns is their lazyness and recursiveness, closing the cycle to the introduction. All fields of a pattern are added to the whole function context (including the pattern itself) and can henceforth be referenced.

This is a non-trivial feature, because it allows (arguably unneccessary) recursion in patterns `{a ? b, b }: a` and monstrosities like these `({a ? (with {b = 2;}; b), b }: a){b = 2;}` that make shadowing behaviour and termination qualities hard to assess. For example, the given example will evaluate to 2 in its current form, but changing the with-bound variables (a usual α-conversion) name to b as in `({a ? (with {a = 2;}; a), b }: a){b = 2;}` leads to erronous termination because a of infinite recursion. To see why, one has to remember that with-bindings are weakly-binding and hence when looked up, the evaluator will refer to the pattern variable a instead of the with-bound one.

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
String interpolation is a common feature in mondern programming lanugages because of its handyness to build complex strings from pieces that are string-convertible. In nix, the syntax `${t}` is used to insert the evaluated value of t into a string or path. This can be used to create structural strings `let name = "john"; greeting = "Hello ${name}"; in greeting` or programatically access file locations `let conf_file_of = name: readString /home/${name}/.config/nu in conf_file_of "john"`.

The context string syntax is not limited to strings and paths alone, it can also be used in _dynamic bindings_, field accesses and field-checks. It is for example possibel to define a record like this `{ ${"foo"} = bar;} -> {foo = bar;}` but the interior of `${}` is not restrited to strings but can be any imaginable term. Similarly, it is possible to _access_ records with a dynamically computed key like this `{ a = 1;, b = 2; c = 3;}.${/* some computation */}` effectively lifting labels to first-class inhabitants of the language. There exists some literature on the matter, but none of them work in generalized setting with this expressive strength. @poly_records @verified

To complete this presentation, we want to stress that interpolated strings are also possible in generic path elements ρ like this: `{ inherit ${"bar"};} ? ${aString}.b` or `{ a.${"bar"} = null; }`.


== Dunder Methods
If the unusual language features were placed in an iceberg-chart, we would now be entering deep-blue water, looking at features that are grosely underdocument and can only be found by nix-wizzards and language implementors.

`TODO`
- `__overrides__`
- `__functor__`
- `__currentSystem`
- `__nixPath`
- `__toString`
- `__currPos`


= Syntax <syn>
#set raw(lang: none)
$oi(E)$ denotes $0 … n$ repititions of a syntax construct and the index $i$ is omitted if obvious.

#syntax

Nix supports the usual _literals_ of fully fledged languages as well as a multi-line string and paths. The syntax is given following the official regex formulas of the informal specification @nix-language-2-28. _Records_ follow a standart notation where multiple fields can be defined using `key = value;` assignments to define multiple fields. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. _Arrays_ are introduced in a similar fashion, where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as seperator. Both datatypes are generally _immutable_, but there are concat operations (Record-Concat and Array-Concat) that can be used to create new, bigger datatypes. Other than that, records come equipped with the usual lookup syntax and special operators. A dynamic label check that returns a boolean as a result, a way to specify a default value in case the previous check turned out to be negative and dynamic lookups.

Functions take one argument, a _pattern_. This pattern can be a single label or adher to a record-structure, allowing multiple fields to be present, possibly with _default arguments_. This way, a function taking multiple arguments can be created without resorting to currying. These functions can be called with a record from which the "single arguments" are taken and form a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.

Patterns can be marked _open_ with the ellipsis (…), otherwise their are regarded as _closed_. Arguments can be given a defautl value using the `?` syntax. The examplary function pattern `{a, b ? "pratt", …}` is an _open_ pattern with a default value of "pratt" for the label $b$ and a mandatory argument $a$. If one wants to refer to the whole argument in the function body, it is possible to create a global-binding for it using the \@-syntax `contact @ { name, surname, tel, email}: contact.name`. The global binding can also occur behind the argument like `{} @ glob: glob` but we use a rewrite rule to catch this case instead of adding it to the syntax definition.

Let-expressions can consist of multiple bindings $a_1 = t_1; … ; a_n = t_n$, possibly referencing each other in a _recursive way_. It is also allowed but regarded deprecated to enclose these bindings in braces. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. They can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing variables bound by other means. See @quirks for further details.

=== Paths
There are three different syntactic objects that deserve the name `path` in our formalization. The first one is the syntactic path-object $rho.alt$ that points to a location in a filesystem. A path can be _absolute_, starting with a `/`, _relative_ from the home directory `~/` or relative to the file where it is stated `./`. All tese notations are standart in the linux world. The second construct is a search-path $Rho$ of the form `<nixpkgs>`, where the entangled name is looked up in the path returned by `builtins.nixPath`. The usecase of this construct is to easily refer to a package entry that is assumed to be "globally accessible" – a quality of life feature.

The last path-like construct is a sequence of record accesses ρ `r.l.l.l` that "reach" to a field of a deeply nested record like `{a: {b: {c: {}}}}`. It is to note, that even though nix does have a null-type, lookups of fields that do not exist, immediately trigger an error instead of returning null. To mitigate unwanted exception raising it is possible to check for the presence of fields in an argument using the ?-operator. This operator expects a record as first operand and a path as second `{} ? a.b.c.d`. A subroutine will then iteratively access the fields, short-fusing with false in case any path-element is missing. As mentioned in @string_interpolation, a path can have string interpolation elements.


== Reduction Rules

#reduction <reduction>

Since nix supports patterns with default values and the _open_ modifiers, the function reduction rules become quite verbose. The simplest case is R-Fun which takes an argument t₁ and replaces the occurences of $l$ with said argument in the function body t₂. The next function rules R-Fun-Pat-∗ reduces functions taking patterns, the R-Fun-Pat being the simplest of such. We draw i,j from the index Set ℐ and range them over labels such that if i = j then l_i = l_j.
Since the same index $i$ is used for both the argument and pattern in R-Fun-Pat, they must agree on the same labels which resembles closed-pattern function calls. In the contrary case where the pattern is open, the argument-record can range over arbitray labels (possibly more than in the pattern). In this case, the side-condition enforces that at least the pattern fields are present (R-Fun-Pat-Open).

The R-fun-Pat-Default-∗ rules range over pattern elements $e$ which can be either single labels $l$ or labels with a default values like $l : d$. The former case can be converted to the latter with ε-extension transforming $l$ to $l ? ε$ which is equivalent to $l$ due to the shorthands (TODO: can you do this?). The variables of the body are then substituted twice. First with the argument values and then with the default values to "fill the gaps". The open case needs a side-condition analogous to the former open case.

Since ${oi(e_i)}$ strictly subsumes ${oi(l_i)}$ due to its inner structure, rule 2 and 3 are only stated as a mental stepping stone for the reader but not mentioned further.


= Finding a Type System

The literatur on type systems is as wide as the ocean with many typesystems studied over the last 70 years of research. Finding a typesystem for a language is thus similar to traversing a jungle with the alluring dangers of getting sidetracked behind every corner, but there are a few beacons we that we can use to find a general direction.

Over the years, logical _type connectives_ like union $τ ∨ τ$, intersection $τ ∧ τ$ and negation $¬τ$ have found their way into many mainstream languages @flow @typescript @typed_racket, proving their usefullness by giving an intuitive method to combine otherwise unrelated types. The type systems that epitomize this idea are typesystem called  _semantic subtyping_, mainly developed by Castagna et. al. The principal idea of this approach is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, in this regard giving types a _semantic meaning_. In the set-theoretic model of types, type-union relates to set-unions $⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $𝟙 without ⟦τ⟧$. In these systems, one can encode pattern-matching, function-overloading and subtyping. Furthermore, these type connectives naturally combine with _flow typing_, a technique that is used to give reason to untyped languges such as nix.

_Flow typing_ uses the "flow" of the language to deduce static properties like field-existance and null-safety. For example in typescript one often uses quick field-checks in if-conditions like this ` if (person.age && age > 18) {}`. If it were not for the `person.age` check, this program would produce an error if the person object does not have an age field. With this check though, the conditional short-fuses if the field is not present and continuos normally with the following statements. Nix has the same mechanism with the explicit check operation `{} ? a` that helps on in guarding an otherwise error producing field-access.

When all three type connictives (¬, ∨, ∧) are present in a system, we can laverage the full power of (turing-complete) boolean algbera to build our types. This expressiveness of these typesystem usually come at the cost of complexity with these systems usually not exhibiting principle types and needing backtracking during type inference.


== Record Theory
Records have been studied in a variety of papers [..] and can be partitioned in roughly 3 groups. The first model of records is a syntactic model where the syntax defines what a record is. This approach is conceptually simple but hard to extend because of its verbose nature and exploding rule-complex.
To overcome these shortcommings, \@? studied _row polymorphism_. Row polymorphism extend record with a generic row r, effectively making them polymorphic in their "rest". By extending the row to lacks-predicates not only extension but also restriction of record types can be achieved, giving a lot of flexibility in theory. While strong in theory, their theory gets complex and unwildy fast, making it hard to integrate into fully-fledged type systems. _Semantic subtyping_, developed over multiple years by Castagna et. al. @gentle_intro @poly_records @typing_records_etc to name a few, tries to remedie this by shortcomming by giving records a set-theoretic semantic model.

Since polymorphic type inference is undecidable in general \@?ref, the model has to rely on backtracking and its performance overhead. It also lacks principle types, a strong selling point of ml-like systems. Last but not least, it is possible to model records in constraint based type system. A record field lookup in these systems produces a constrained which is collected and simplified later. Due to the generality, these systems usually don't exhibit good and effective properties.

Only recently in 2017, Stephen Dolan proposed a new family of type systems, named _algebraic type systems_. These systems tackle language construction from a new point of view. Instead of adding types first and then trying to find a semantic model for them, Dolan argues one should pay more attention to finding a semantic model for the types _first_. The types in _algebraic type systems_ form a distributive lattice (thus algebraic) and inherit the lattice' properties. By further restricting the the occurences for union and intersections to positive and negative positions, a distributive lattice can be constructed that allows for lossless reduction of subtyping constraints. In essence, the system is standart ML, with a lattice of types and unification replaced by bi-unification, a subroutine that handles subtyping constraints instead of equality constraints. The final algorithms for subsumption checking and type inference are short as well as simple, all thanks to the initial focus on well-formed types. The final algorithms inherit the standart ML properties, namely _principled type inference_, no need for type annotations and effectiveness i.e no backtracking.

Since batracking in nix' huge syntax tree that roots in a single file and relies heavily on laziness is insufficient, the properties of algebraic subtyping come as a perfect fit. The formalization of algebraic subtyping depends heavily on order-theory and some form of category theory and the proofs are far from simple @simplesub. Thankfully, @simplesub showed how to get from a algebraic domain to a syntactic one by creating an  equivalent using constraint accumulation on type variables and biunification, making algebraic subtyping more accessible. In the seminal Bachelor Thesis from the first author, he showed how to extend the SimpleSub to the more expressive type system features of nix. Even though the work pintpointed a direction, it oversimplified on the operational semantic and derived type rules, leaving lots of room for improvement.

== First class Labels
- TODO

== Flow Typing
Besides the afore-mentioned record-field-check, nix provides function that can dynamically check the type of expressions. It is thus possible to write expressions like `if isStr t then {} else {}` where we can type t as $t ∧ str$ in the positive case and $t ∧ ¬str$ in the negative else-branch.


== Impurities
On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standart library @builtins thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves.



== Related Work
We decided to diverge from the formulation given in @verified by not adding inherit as syntactic sugar but as a inference rule with a premise that ensures that no new recursion was introduced during type inference. This is done by checking the context whether this variable exists.


= Type System
What follows are the the types used to type the nix language.

#types <types>


== The Standart Library
Nix includes a palette of 78 builtin types that are implemented by the evaluator and thus need to be implemented in a complete type inference algorithm.

#figure(
  caption: [The nix language builtins and their respective types given @types],
  builtin_types,
)<builtins>

=== Problematic Children

- *Records*: attrNames, attrValues, getAttr, hasAttr, intersectAttrs, mapAttrs
- *Array*: elem, elemAt, head, length, listToAttrs
- *Inspecting*: functionArgs
- *Too general*:
- *Impure*: currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion
- *flow*: isAttrs, isBool, isFloat, isFunction, isInt, isList, isNull, isPath, isString

#page[
  #bibliography(
    ("bib/misc.bib", "bib/parreaux.bib", "bib/nix.bib", "bib/castagna.bib"),
    style: "association-for-computing-machinery",
  )
]
