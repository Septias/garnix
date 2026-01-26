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
  description: "Masterproject about nix syntax, operational semantic and type inference approaches",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

= Securing Nix' Foundations
The Nix programming language is used in over 100.000 files, showing its prominence, but was neglected in theoretical work until recently when work was picked up independently by S. Klähn and Breokhoff et. al. @verified @simplenix. Both works gave a syntax definition and operational semantic to account for their uses, but did not cover the language in their full expressiveness. This work closes the gap by showing nix in its full expressiveness and giving an overview of possible type inference approaches.


= Origin of the Nix Language <intro>
The nix package manager distinguishes itself from other package managers by one prominent feature: It has a built-in domain-specific programming language at its foundation. The homonymous programming language (nix) is a major reason for the steep learning curve to enter the ecosystem, but it is also the reason nix inhabits two fundamental properties: _purity_ and _functionality_.

The nix package manager was created in an attempt to overcome the problem of distributing software (components) between different environments (machines) without breaking them. This problem is more subtle than one might expect and the reason why so many package managers exist that try to tackle the problem differently. The approach take by Eelco Dolstra et al. to overcome this problem is to »apply a memory management discipline to package management«, effectively interpreting files as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. It's major achievement is a garbage-collector inspired technique to consistently track dependencies during package construction. The final _closure_ that pictorally resembles a tree of (sub-) dependencies with the built package at its root, can then be _extracted_ from the local filesystem and sent to other machines by sending every sub-component and reassembling on the other side. Because all dependencies have been transferred to the new machine, the program is virtually not dependent on the new environment.

While this already explains the general approach, one can only understand the entirety of the package manager after taking the _nix store_ into account. The nix store is a read-only location that stores immutable artifacts of builds. It uses hashes to identify components and allows for quick equality checks, and therefore reusability of components. All its components live in the same location, but isolated such that different version of the same package, can be used simultaneously without inferring or overwriting each other. Since every package is a pure derivation of its dependencies, new version can easily be added to the store without having to worry about older versions such that package upgrades become _fearless_. If something should still break, prior versions live perfectly preserved in the store and can be rolled-back to in O(1) at any time @memory_to_software. It is also possible for a garbage collector to identify unreachable store locations by tracking "roots" and deleting them to reclaim disk space, resulting in a model that is reliable and efficient.

To support this colorful palette of features, Eelco Dostra decided to use a _domain specific_ language (DSL) that makes these an inherent property instead of a retrofitted qualities. First and foremost, one of nix' greatest strength – _reproducibility_ – is a direct consequence of the languages' functional design. When abstracting files and references to memory locations and references, one can notice that _pure functionality_ boasts all the features needed for airtight dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

The reason why nix needs to be a lazy language is its costly environment where a single action – building a package – can already be very expensive, possibly taking multiple hours and lots of resources to complete. It is thus of utmost importance, that packages are only realised if actually needed. In a lazy language, values of function application are substituted as-is without further reduction i.e computation on them. In nix, where packages are stored in lazy record fields, laziness of record fields is the essential ingredient to not build packages if not _actually needed_ and saving valuable computation resources.

Nix is a declarative language that resolves largely around records (key-value bindings) to describe the goal state of a system. Inherit- and with-statement statements are used to create new records or extract fields from existing ones. Functions can be defined using a pattern that destructures records, and a myriad of operators support _record concatenation_, dynamic lookups, field checks and more. Using the recursiveness of records it is possible to build self-referntial structures that are used throughout the ecosystem.

Combining all these features, the nix language is a wild zoo of constructs, theoretical properties and poses tricky shadowing and termination properties because of the combination thereof.

== Quirks of the Nix Language <quirks>
#set raw(lang: "nix")
The following section gives a briev overview of nix-specific language features and their surprising interactions.

The nix programming language heavily resolves around records. A primitive record is a set of key-value bindings like `{a = 2; b = 3;}` but by adding the `rec` keyword in front, record fields are allows to reference other fields of the same record like `rec {a = b; b = 2;}; → {a = 2; b = 2;}`. Using self-referential records and the laziness of the language, it is then possible to create all kinds of infinite recursion, not all of them well-behaved. Bad examples are the simply recursive `rec {x = x;}` or mutual recursive `rec {a = b; b = a;}` records. Both definitions fail because of infinite recursion during evaluation as the evaluator raises an error instead of diverging. To form a well-behaved recursive definitions, one has to utility nix' laiyness and go through a lazy constructor like a record, array, pattern-field or let-binding. Consequently, both `rec {x = {x = x;}}` and `let x = {x = y;}; y = x; in x` don't result in erroneous program termination and can be unrolled indefinitely in their x-field. Similar is possible by using function-patterns and arrays which are both lazy aswell.

The let-binding of the second example is very similar to a recursive record in that it allows for _multiple_, possibly mutual-referential bindings, `let a = b; b = 2; in b`. It does not need a `rec` keyword but is equally not permitted to create mutual recursive non-constructive recursion like `let a = b; b = 2; in b`, because, again, there is no lazy constructor that remedies infinite computation. Their similarity is best shown by the let-rec-in-binding `let {a = 2; b = 3;} in t` that encloses the semicolon-separated key-value bindings of the usual let-binding in braces. It is trivial to see that both let-forms can be rewritten to one another by removing or adding the enclosing braces, rendering the latter notation valid, but obsolete.

The remaining difference between the two is, that a record definition is a terminating expression whilst a let-binding is followed by an arbitrary possibly diverging computation. This subtle difference can be partially removed using the `with`-construct. The with construct is a nix-specific feature that takes a record as first "operand" and "opens" it in the following expression, adding all the bindings to the scope. It is thus possible to compute the sum of two record-fields like this: `with {a = 2; b = 2;}; a + b`.
By using the with-construct in conjunction with records, it were in theory possible to form an equivalence between the two constructs: `with (rec {a = b; b = 2}); t == let {a = b; b = 2} in t` – if it were not for with' unexpected shadowing behaviour.

A with statement binds _weakly_, meaning it will not shadow any binding that was added by any other means. An expression like `let a = 2; in (with {a = 3;}; a)` will thus evaluate to 2 instead of 3 even though `a = 3;` binds "after" the initial binding of `a = 2;`. Similar is the case for function bindings `a: (with {a = 2;}; a) → a`, inherited bindings, and records `rec {a = 3; b = (with {a = 4;}; a);} -> {a = 3; b = 3;}`. When stacking with-bindings, the shadowing behaviour is as expected: `with {a=1;}; with {a=2;}; a -> 2`.

The reason for this unexpected shadowing behaviour is the intended use as a mean to simulate a _module system_. From a module-perspective, a record encapsulates its definitions and by using the width-statement, one can open a module, effectively adding all its definitions to the following expression. Since modules tend to have quite a lot of definitions, they bind weakly to not overwrite locals. The "module-system" shows its full potential when used together with the built-in `import` function, that takes a file, evaluates the content and returns the computed value. Using these two constructs in unison, one can import a file `modules.nix` and make all its definitions available in the following scope `with (import ./modules.nix); t`, very similar to a global module import.

The import-statement can occur anywhere in the syntax-tree, including recursive functions, struct-fields, arrays and literally every other construct. Since the import statement will "fill in" the files evaluated content literally, it is possible to follow the statement up with an arbitrary value. A statement like `import ./modules.nix {system = "nixos_x86"}` will then first evaluate the file `modules.nix` and interpret the following term as an argument to a function, resulting in a function application. `import ./modules.nix {system = "nixos_x86";} -> f {system = ""}`. The integrity of functions in files is not checked statically, but it is a frequently used pattern to separate code into different files. To illustrate the point, see @module_example for a full example.

Calling a function with a record enjoys its own syntactic sugar. By using a _pattern-function_, one can destructure a given record-argument into its fields, giving a semantic with functions that can take multiple arguments, possibly with default-arguments. If we think of the record constructor `{}` as introduction-rule, then functions with patterns are the logical equivalent that eliminate them. It is thus a natural choice to encapsulate function patterns in the very same brackets that records use. A function with the signature `{a, b}: a + b` expects one argument of record-type and "extracts" the fields a, b from it, making them accessible in the function body. Together, record construction and pattern-function application act as inverses to one another `a: ({a}: a) {a = a;} == a`.

A function pattern in the primitive case `{a, b}: a` will expect exactly the fields a and b in a given record argument. But it is also possible to create open patterns like `{a, ...}` that only require a single field a and allow the argument to have arbitrary many other fields. Likewise, one can give default arguments `{a ? "nikita", ...}` that are supplied in case the argument lacks said field. An unexpected behaviour of patterns is their laziness and recursiveness, closing the cycle to the introduction. All fields of a pattern are added to the whole function context (including the pattern itself) and can henceforth be referenced.

This is a non-trivial feature, because it allows (arguably unnecessary) recursion in patterns `{a ? b, b }: a` and monstrosities like these `({a ? (with {b = 2;}; b), b }: a){b = 2;}` that make shadowing behaviour and termination qualities hard to assess. For example, the given example will evaluate to 2 in its current form, but changing the with-bound variables (a usual α-conversion) name to b as in `({a ? (with {a = 2;}; a), b }: a){b = 2;}` leads to erroneous termination because a of infinite recursion. To see why, one has to remember that with-bindings are weakly-binding and hence when looked up, the evaluator will refer to the pattern variable a instead of the with-bound one.

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
String interpolation is a common feature in mondern programming languages because of its handyness to build complex strings from pieces that are string-convertible. In nix, the syntax `${t}` is used to insert the evaluated value of t into a string or path. This can be used to create structural strings `let name = "john"; greeting = "Hello ${name}"; in greeting` or programmatically access file locations `let conf_file_of = (name: readString /home/${name}/.config/nu); in conf_file_of "john" → /home/john/.config/nu`.

The context string syntax is not limited to strings and paths alone, it can also be used in _dynamic bindings_, field accesses and field-checks. It is for example possible to define a record like this `{ ${"foo"} = bar;} → {foo = bar;}`. The interior of `${}` is not restricted to strings but can be any imaginable term. Similarly, it is possible to _access_ records with a dynamically computed key like this `{ a = 1;, b = 2; c = 3;}.${/* some computation */}` effectively lifting labels to first-class inhabitants of the language.

To complete this presentation, we want to stress that interpolated strings are also possible in generic path elements ρ like this: `{ inherit ${"bar"};} ? ${aString}.b` or `{ a.${"bar"} = null; }`.


== Dunder Methods
If the unusual language features were placed in an iceberg-chart, we would now be entering deep-blue water, looking at features that are grosely underdocument and can only be found by nix-wizzards and language implementors.

`__overrides__` is a record-field, that is recognized by the language evaluator and can be used to override fields of the record. `{ a = 1; __overrides = { b = 2; }; -> {a = 1; b = 2;}`. The other field special to nix is the `__functor__` field that can be used to turn a record into a function. `({ __functor = self: x: self.foo && x; foo = false; } // { foo = true; }) true -> true`. Lastly, the `__toString` dunder allows one to define a string representation for a record.

Nix also provides three impure dunder variables `__currentSystem` ,`__nixPath` and  `__currPos` that give information about the current system, where the nix executable is stored and the current evaluators position in the file. These structs bring the execution environment into scope and are further discussed in @impurities.


= Syntax <syn>
#set raw(lang: none)
$overline(E)^{i ∈ 𝓘}$ denotes a repetition of a syntax construct indexed by $i ∈ 𝓘$. The index $i ∈ 𝓘$ is omitted if obvious.

#syntax

Nix supports the usual _literals_ of fully fledged languages as well as a multi-line string and paths. The syntax is given following the official regex formulas of the informal nix specification @nix-language-2-28. _Records_ follow a standard notation where multiple fields can be defined using `key = value;` assignments to define multiple fields. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. _Arrays_ are introduced in a similar fashion, where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as separator. Both datatypes are generally _immutable_, but there are concat operations (Record-Concat and Array-Concat) that can be used to create new, bigger datatypes. Other than that, records come equipped with the usual lookup, a dynamic label check that returns a boolean as a result, a way to specify a default value in case the previous check turned out to be negative and dynamic lookups.

Functions take one argument, a _pattern_. This pattern can be a single label or adher to a record-structure, allowing multiple fields to be present, possibly with _default arguments_. This way, a function taking multiple arguments can be created without resorting to currying. These functions can be called with a record from which the "single arguments" are taken and form a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.

Patterns can be marked _open_ with the ellipsis (…), otherwise their are regarded as _closed_. Arguments can be given a default value using the `?` syntax. The exemplary function pattern `{a, b ? "pratt", …}` is an _open_ pattern with a default value of "pratt" for the label $b$ and a mandatory argument $a$. If one wants to refer to the whole argument in the function body, it is possible to create a global-binding for it using the \@-syntax `contact @ { name, surname, tel, email}: contact.name`. The global binding can also occur behind the argument like `{} @ glob: glob` but we use a rewrite rule to catch this case instead of adding it to the syntax definition.

Let-expressions can consist of multiple bindings $a_1 = t_1; … ; a_n = t_n$, possibly referencing each other in a _recursive way_. It is also allowed but regarded deprecated to enclose these bindings in braces. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. They can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inheri
  [getFlake `args`             ], [: Fetch flake reference and outputs.], $ todo({}) -> path $,
  [groupBy `f list`            ], [: Group elements by key `f(element)`.], $ (α -> bool) -> [α] -> ([α], [: `true` t (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing variables bound by other means. See @quirks for further details.

=== Paths
There are three different syntactic objects that deserve the name `path` in our formalization. The first one being the syntactic path-object $rho.alt$ that points to a location in a filesystem. A path can be _absolute_, starting with a `/`, _relative_ from the home directory `~/` or relative to the file where it is stated `./`. All these notations are standard in the linux world. The second construct is a search-path $Rho$ of the form `<nixpkgs>`, where the entangled name is looked up in the path returned by `builtins.nixPath`. The usecase of this construct is to easily refer to a package entry that is assumed to be "globally accessible" – a quality of life feature.

The last path-like construct is a sequence of record accesses ρ `r.l.l.l` that "reach" to a field of a deeply nested record like `{a: {b: {c: {}}}}`. It is to note, that even though nix does have a null-type, lookups of fields that do not exist, immediately trigger an error instead of returning null. To mitigate unwanted exceptions, it is possible to check for the presence of fields in an argument using the ?-operator. This operator expects a record as first operand and a path as second `{} ? a.b.c.d`. A subroutine will then iteratively access the fields and short-fuse with false in case any path-element is missing. As mentioned in @string_interpolation, a path can have string interpolation elements.


== Reduction Rules

#reduction <reduction>

Since nix supports patterns with default values and the _open_ modifiers, the function reduction rules become quite verbose. The simplest case is R-Fun which takes an argument t₁ and replaces the occurrences of $l$ with said argument in the function body t₂. The next function rules R-Fun-Pat-∗ reduces functions taking patterns, the R-Fun-Pat being the simplest of such. We draw i,j from the index Set ℐ and range them over labels such that if i = j then l_i = l_j.

Since the same index $i$ is used for both the argument and pattern in R-Fun-Pat, they must agree on the same labels which resembles closed-pattern function calls. In the contrary case where the pattern is open, the argument-record can range over arbitrary labels (possibly more than in the pattern). In this case, the side-condition enforces that at least the pattern fields are present (R-Fun-Pat-Open).

The R-fun-Pat-Default-∗ rules range over pattern elements $e$ which can be either single labels $l$ or labels with a default values like $l : d$. The former case can be converted to the latter with ε-extension transforming $l$ to $l ? ε$ which is equivalent to $l$ due to the shorthands (TODO: can you do this?). The variables of the body are then substituted twice. First with the argument values and then with the default values to "fill the gaps". The open case needs a side-condition analogous to the former open case.

Since ${oi(e_i)}$ strictly subsumes ${oi(l_i)}$ due to its inner structure, rule 2 and 3 are only stated as a mental stepping stone for the reader but not mentioned further.


= Finding a Type System
TODO: Make this more about nix

The literatur on type systems is as wide as the ocean with many typesystems studied over the last 70 years of research. Finding a typesystem for a language is thus similar to traversing a jungle with the alluring dangers of getting sidetracked behind every corner, but there are a few beacons we can use to find a general direction.

Over the years, logical _type connectives_ like union $τ ∨ τ$, intersection $τ ∧ τ$ and negation $¬τ$ have found their way into many mainstream languages @flow @typescript @typed_racket @mlstruct, proving their usefulness by giving an intuitive method to combine otherwise unrelated types. For example, a function that uses a conditional `x: y: z: if x then y else z` is a function that that returns either y or z based on whether x is true or false. This function can be typed at $bool -> α -> β -> (α ∨ β)$, intersecting the possible return types. This is a great improvement to previous ml-like systems that would have to _unify_ α and β in this situation which is already not possible for integers and strings @algebraic_subtyping. The intersection type respects the variable _flow_, only merging α and β because they flow together in the output, not merging them in the input.

*overloading*: Using intersection types, one can define functions that have many types. For example, the function `if isBool(x) then !x else x + 1` is a function that either inverts a bool or increments integer. We would like to describe the argument x with an unbound type-variable α, but from the function body it is clear, that this function is only well-behaved on integers and bools. This function can be given two types. The first one $(bool ∨ int) -> (bool ∨ int)$ states that the function accepts argument of either bool or int and will return either an int or bool. But using intersection types, the functiontype can be refined to the more specific type $(int -> int) ∧ (bool -> bool)$, stating that if the function is called with an integer, it will also return one (instead of the union $int ∨ bool$). TODO: reference SystemF?


The nix addition operator can be used on strings and paths likewise. It is such possible to write expressions like `/home/ + "john" -> /home/john` that will return a _path_ and `"/home/" + "john"` which will return a _string_. The most general type is thus `(str -> (str ∨ path) -> string) ∧ (path -> (str ∨ path) -> path)`, needing intersection types.

*pattern-matching and flow typing* _Flow typing_ and the more rigid _occurrence typing_ are techniques used in _gradual type systems_ that narrow a type based on its usage. For example, the function `if isBool(x) then !x else x + 1` checks the runtime value of x to be of type bool. After this conditional check, one can obviously type the positive branch under the assumption, that x is of type bool @typescript. A similar technique can be used to refine the consecutive branches of pattern matching statements.
In an example match-statement `match x with bool(x) -> .. | rec(x) -> .. | _ -> x` one matches the single branches in order. The branches behave like conditionals, forcing a type on the variable, the interesting case is the default-case. This one can be typed under the assumption, that it is not of type bool or record and show why one would like to use negation types.

Using negation types, it is also possibl to add record field removal to a language like `{a: τ} ∧ ¬{b : τ}`.


== A gradual type system
TODO: explain in more detail why a gradual type system is needed.


The type systems that epitomize this idea are typesystem called  _semantic subtyping_, mainly developed by Castagna et. al. The principal idea of this approach is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, in this regard giving types a _semantic meaning_. In the set-theoretic model of types, type-union relates to set-unions $⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $𝟙 without ⟦τ⟧$.  Furthermore, these type connectives naturally combine with _flow typing_, a technique that is used to give reason to untyped languages such as nix.

_Flow typing_ uses the "flow" of the language to deduce static properties like field-existence and null-safety. For example in typescript one often uses quick field-checks in if-conditions like this ` if (person.age && age > 18) {}`. If it were not for the `person.age` check, this program would produce an error if the person object does not have an age field. With this check though, the conditional short-fuses if the field is not present and continuous normally with the following statements. Nix has the same mechanism with the explicit check operation `{} ? a` that helps on in guarding an otherwise error producing field-access.

When all three type connictives (¬, ∨, ∧) are present in a system, we can laverage the full power of (turing-complete) boolean algbera to build our types. This expressiveness of these typesystem usually come at the cost of complexity with these systems usually not exhibiting principle types and needing backtracking during type inference.


== Record Theory
Records have been studied in a variety of papers [..] and can be partitioned in roughly 3 groups. The first model of records is a syntactic model where the syntax defines what a record is. This approach is conceptually simple but hard to extend because of its verbose nature and exploding rule-complex.
To overcome these shortcomings, \@? studied _row polymorphism_. Row polymorphism extend record with a generic row r, effectively making them polymorphic in their "rest". By extending the row to lacks-predicates not only extension but also restriction of record types can be achieved, giving a lot of flexibility in theory. While strong in theory, their theory gets complex and unwildy fast, making it hard to integrate into fully-fledged type systems. _Semantic subtyping_, developed over multiple years by Castagna et. al. @gentle_intro @poly_records @typing_records_etc to name a few, tries to remedie this by shortcoming by giving records a set-theoretic semantic model.

TODO: dolan vs. castagna vs. parreaux


== Impurities <impurities>
On the one hand nix is a pure and function language without sideffects but on the other hand it is one, that tightly integrates with the file-system to properly track built operations, their dependencies and outputs. The standard library thus boasts a few functions that make typing undecidable for systems that don't evaluate the language themselves.

`currentSystem, currentTime, fetch\*, findFile, langVersion, nixVersion` these functions can return arbitrary values. Since nix can use them them in combination with dynamic accesses, the type systems becomes an evaluator. We thus _need_ a gradual type system.


= Type System
What follows are the the types used to type the nix language.

#types <types>


== The Standard Library
Nix includes a palette of 78 builtin types that are implemented by the evaluator and thus need to be implemented in a complete type inference algorithm.

#box(width: 110%, [
  #figure(
    caption: [The nix language builtins and their respective types given @types],
    builtin_types,
  )<builtins>])


TODO: discussion of problematic functions.

#page[
  #bibliography(
    ("bib/misc.bib", "bib/parreaux.bib", "bib/nix.bib", "bib/castagna.bib"),
    style: "association-for-computing-machinery",
  )
]
