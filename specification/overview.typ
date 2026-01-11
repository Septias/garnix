#import "functions.typ": *
#import "comparison.typ": comparison
#import "typesystem.typ": *

#set heading(numbering: "1.")
#set page(height: auto)
#show figure: set block(breakable: true)
// #set figure(placement: auto)
// #show stack: set block(breakable: true)

#set document(
  title: "Improving Nothing",
  description: "Type inference for the Nix Language",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

#text("Improving Nothing", size: 17pt)
#linebreak()
The nix programming language is a pure and lazy programming languge with real-world usage in the nix package manager and NixOs operating system, and even though it has existed for over 20 years and is used close to exclusively in the nix-ecosystem with over 100.000 files written in it, it has not received a proper type system yet. The reason for that is not clear to the authors, but we suspect it roots from the unique features of the language, its unintuitive shadowing behaviour and laziness, that complicate principled type inference in a general sense.
Only the recent works of Lionell Parreaux and Stephen Dolan surrounding _algebraic subtyping_ have opened a new perspective to type inference for such an expressive languagage and motiviated this paper where we try to lay out the current state of type inference in nix, define a comprehensive _operational semantic_ and ultimately a _type system_ for a reduced part of the language. We also provide an implementation of a language server written in rust.

== Preface
I refer to this document as "paper" even though it could be a master-project/thesis etc. and the subject might still change to some extend.

= Origin of the Nix Language

The nix package manager distinguishes itself from other package managers by one prominent feature: It has a built-in domain-specific programming language at its foundation. The homonymous programming language – nix – is a major reason for the steep learning curve to enter the ecosystem, but it is also the reason nix inhabits two fundamental properties: purity and functionality.

The nix package manager was created in an attempt to overcome the problem of distributing software (components) between different environments (machines) without breaking them. This problem is more subtle than one might expect and the reason why so many package managers exist that try to tackle the problem differently. The approach take by Eelco Dolstra et al. to overcome this problem is to »apply a memory management discipline to package management«, effectively interpreting files as memory locations and references as pointers between them @memory_to_software. It's major achievement is a garbage-collector inspired technique to consistently track dependencies during package construction. The final _closure_ that pictorally resembles a tree of (sub-) dependencies with the built package at its root, can then be _extracted_ from the local filesystem and sent to other machines by sending every sub-component and reassembling on the other side. Because all dependencies have been transfered to the new machine, the program is virtually not dependent on the new environment.

While this simplification already explains the key ingredients, one can only understand the entirety of the package manager after beeing introduced to the _nix store_. The nix store is a read-only location that stores the immutable artifacts of builds. It uses hashes to identify components and allows for quick equality checks, and therefore reusability of components. All its components live in the same location, but isolated such that different version of the same package, can be used simultaneously without infering or overwriting each other. Since every package is a pure derivation of its dependencies, new version can easily be added to the store without having to worry about older versions such that package ugrades become _fearless_. If something should still break, the old version lives perfectly preserved in the store and can be rolled-back to in O(1) at any time @memory_to_software. It is also possible for a garbage collector to identify unreachable store locations by tracking "roots" and delete them to reclaim disk space.

To support this colorfull palett of features, Eelco Dostra decidde to implement a _domain specific_ language that makes em a inherent part property instead of a retrofitted qualities. First and foremost, one of nix' greatest strength – _reproducibility_ – is only possible due to the languages' functional design. This is done by taking a concept from language theory and applying it to package management. When abstracting files and references, one can notice that _pure functionality_ boasts all the features needed for airtight dependency management. A pure function computes its output solely given its input fields. The final value can then be memoized and reused should it be needed again. The nix package manager uses _pure functions without side effects_ to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

To understand why the language needs to be lazy, we have to look at the application of it and notice that package management is a costly environment. Even a single action – building a package – can already be very expensive, possibly taking multiple hours to complete. It is thus of utmost importance, that packages are only realised if actually needed. In a lazy language, values of function application are substituted as-is without further reduction i.e computation on them. In nix, where packages are stored in lazy record fields, lazyness of record fields is the essential ingredient to not build packages if not _actually needed_ and saving valuable computation resources.

When using the nix package manager, declarative configuration management is the major activity and the nix language thus optimized to do so. Namely, the inherit- and with-statement are powerful language constructs, that help in creating new records or extracting fields from existing ones. For dependency management, key-value fields strike the balance between verbosity and simplicity. The language thus resolves largely around record fields, taking them as function arguments, in with-statements and for its builtin functions. Using their recursiveness it is possible to build self-referntial structures that enable implementors to overwrite single fields effortlessly. The record concat operator is used to create bigger record fields.

Combining all these features, the nix language is a wild zoo of constructs, theoretical properties and poses tricky shadowing and termination properties because of the combination thereof.

== Quirks of the Nix Language
In this section we look more closely on nix specific features and their suprising interactions.

=== Laziness
Laziness is an ~existential nix feature since without it, the package manager would be unpractically slow. Nix adds lazyiness virtually everywhere: record-fields, functions, let-bindings, arrays, and patterns.

== Context Strings
Context strings and dynamic lookup share the same syntax in that you can insert some arbitrary term `t` into braces with the following syntax `${t}`. For ordinary strings and paths, the value of `t` will be coerced into a string and added literally. From a typing perspective, this is the easy case because inserted values get a constraint of string and that's it. For dynamic lookup it gets trickier though.


== Dynamic Lookup <dynamic_lookup>
Context strings allow lookups of the form `a.${t}` where t is allowed to be any expression that ultimately reduces to a string. The reduced string is then used to index the record which a is supposed to be. Since a type system only computes a type and not the actual value, the only possible approach to handle first-class labels is to evaluate nix expressions to some extent. Writing a full evaluator is probably too much, but there could be heuristics for simple evaluation. One approach would be to work backwards from return statements in functions up until it gets too unwieldy.
This would also mean implementing the standard library functions like map, readToString etc. One ray of hope is that these were probably already implemented in Tvix.


== Dunder Function
- "__overrides__"
- "__functor__"


= Constructs
== With Statements <with>
With statements allow introducing all bindings of a record into the following expression. For this, the first expression (A) in $"with " A"; "B$ has to reduce to a record. If that does not work, typing should raise an _error_. For explicit records, the following typing is straightforward. Just introduce all fields to the scope without shadowing and continue typechecking $B$. For the case that A is a type variable, it gets tricky however because of the generic subsumption rule. When A is subtyped like follows $A: {X: "int"} arrow A: {}$, then the field X would not be accessible in the function body.
The second problem is what I call the _attribution problem_. It occurs when there is a chain of with statements $"with "A; ("with "B;) t$ and A and B are type variables. Now when trying to lookup $x$ in t, it is unclear whether x came from B or A.


== Dunder Methods
There seem to be some special dunder methods for representations which are handled specially by the evaluator. I have not had the chance to look into it further.
An example is the `__functor` field that can be set on a record and lets the function be used as a functor.


= Laziness and Recursiveness
Laziness and recursion occur in two language constructs. The first one being _recursive records_ and the second one being _let bindings_. To evaluate them, a lazy evaluation scheme is needed which is currently implemented as follows:
When typing a let binding or record, the algorithm adds all name bindings to the context up-front. This way, referenced values will not be undefined when looked up, even if their definition was not type checked yet. The typecheck algorithm then starts with some arbitrary first label $A$ which may contain an unchecked expression labeled $B$.
When this undefined label $B$ is found, it is simply used to create upper and lower bounds (constraints). For empty type variables that is fine to do, but when we actually check this $B$, it will unfold and be constrained with upper and lower bounds. These bounds are missing on the typecheck run of $A$ then. An example would be `let f = a: a + 1; x = f b; b = "hi" in {}` In this case b would be constrained to be a number (because of the application and its implication) but afterwards it will get its "real" type which is string. Currently, the constraint error would be placed at the wrong location (that of the true definition).

#figure(
  ```nix
  rec { x = { x = x;};}.x;       # → { x = «repeated»; }
  let x = {x = y;}; y = x; in x  # → { x = «repeated»; }
  ```,
  caption: [Examples of recursive patterns from the nix repl],
)


=== Shadowing
Nix with statement has special shadowing behavior in that it does not shadow let-bount variables. An expression `let a = 1; with {a = 2}; a` will thus reduce to 1 instead of two, because a is "not taken from the record". This is a major source of confusion, also, because it behaves differently for stacked with-statemnts. The expression `with {a = 1;}; with {a=2;}; a` will evaluate to 2, because the latest with-statement shadows outer ones.


== Finding a Type System

The literatur on type systems is as wide as the ocean with many typesystems studied over the last 70 years of research. Finding a typesystem for a language is thus similar to traversing a jungle with the alluring dangers of getting sidetracked behind every corner. Since records are such a central aspect of the language, starting from them is not a bad idea.

=== Record Theory
Records have been studied in a variety of papers [..] and can be partitioned in roughly 3 groups. The first model of records is a syntactic model where the syntax defines what a record is. This approach is simple but hard to extend because everything has to be encoded in its syntax. To overcome its shortcommings, \@? studied _row polymorphism_. Row polymorphism extend record with a generic row r, effectively making them polymorphic in their "rest". By extending the row to lacks-predicates not only extension but also restriction of record types can be achieved, giving a lot of flexibility in theory. While strong in theory, their theory gets complex and unwildy fast, making it hard to integrate into fully-fledged type systems. _Semantic subtyping_, developed over multiple years by \@castagna tries to remedie this by shortcomming by giving records a set-theoretic semantic model. By also adding type connectives (negation, union and intersection), his systems are impressively expressive, especially in combination with _gradual typing_. The strength comes of a cost though, namely _backtracking_. Since polymorphic type inference is undecidable in general \@?ref, the model has to rely on backtracking and its performance overhead. It also lacks principle types, a strong selling point of ml-like systems. Last but not least, it is possible to model records in constraint based type system. A record field lookup in these systems produces a constrained which is collected and simplified later. Due to the generality, these systems usually don't exhibit good and effective properties.

Only recently in 2017, Stephen Dolan proposed a new family of type systems, named _algebraic type systems_. These systems tackle language construction from a new point of view. Instead of adding types first and then trying to find a semantic model for them, Dolan argues one should pay more attention to finding a semantic model for the types _first_. The types in _algebraic type systems_ form a distributive lattice (thus algebraic) and inherit the lattice' properties. By further restricting the the occurences for union and intersections to positive and negative positions, a distributive lattice can be constructed that allows for lossless reduction of subtyping constraints. In essence, the system is standart ML, with a lattice of types and unification replaced by bi-unification, a subroutine that handles subtyping constraints instead of equality constraints. The final algorithms for subsumption checking and type inference are short as well as simple, all thanks to the initial focus on well-formed types. The final algorithms inherit the standart ML properties, namely _principled type inference_, no need for type annotations and effectiveness i.e no backtracking.

Since batracking in nix' huge syntax tree that roots in a single file and relies heavily on laziness is insufficient, the properties of algebraic subtyping come as a perfect fit. The formalization of algebraic subtyping depends heavily on order-theory and some form of category theory and the proofs are far from simple @simplesub. Thankfully, @simplesub showed how to get from a algebraic domain to a syntactic one by creating an  equivalent using constraint accumulation on type variables and biunification, making algebraic subtyping more accessible. In the seminal Bachelor Thesis from the first author, he showed how to extend the SimpleSub to the more expressive type system features of nix. Even though the work pintpointed a direction, it oversimplified on the operational semantic and derived type rules, leaving lots of room for improvement.


== Comparing Nix Features
#comparison <comparison>

The table shows a comparison between garnix and NixLang \@verified. A dotted circle represents feature compatibility in the reduction semantic, where as a full circle tells that type inference was implemented for that feature. The works in \@verified develop an interpreter instead of type inference, so no full circles are expected on that side. To do the paper justic, another circle kind could be added for features that are covered by an interpreter, but that is not the subject of this paper.

== Algebraic Subtying
Algebraic subtypign @dolstra_phd is a technique to get well-behaved types and neat type inference. After @simplesub and @mlstruct we know how to pratically implement it. The first thing one needs to do is to form a boolean algbebra of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually landing at primitive constraints like $"Bool" < top$ that can be solved trivially.


== Things done in this paper
Since nix was built as a domain specfic language with usability as its greatest design goal, the system boasts a lot of features that make retrofitting a type inference hard or even impossible. In traditional language theory, the flow is mostly reversed where one starts from a simple calculus like ML, SystemF, λ and carefully extends it with features to form a wieldy and interesting semantics. When trying to retrofit a type-system onto a language like @nix-language-2-28 \@flow \@castagna_elixier one has to decide which features one can and wants to support.

In this paper we restrict ourselves to:
1. Basetypes (Record, Array, ..Primitive Types)
2. Datatype Operators (Record-extension, Array-concatenation)
3. Special language constructs (with-statements, inherit-statements)

We defer these features to later efforts of research:
1. Dynamic accesses
2. Assert statements?
3. Deprecated let-attrset
4. Deprecated uris

In essence, our contributions are:

1. A comprehensive operational semantic for the nix language
2. A Typesystem based on Mlstruct


= Syntax <syn>
$oi(E)$ denotes $0 … n$ repititions of a syntax construct and the index $i$ is omitted if obvious.

#syntax

Nix supports the usual _literals_ of fully fledged languages as well as a multi-line string and paths. The syntax is given following the official regex formulas to follow the specification @nix-language-2-28. _Records_ follow a standart notation where multiple fields can be defined using `key = value;` assignments to define multiple fields. In addition, records can be marked _recursive_ with the `rec` keyword and are non-recursive otherwise. _Arrays_ are introduced in a similar fashion where multiple values can be concatenated with the only unintuitive nix-specific distinction that a space is used as seperator. Both datatypes are generally _immutable_, but there are concat operations (Record-Concat and Array-Concat) that can be used to create new, bigger datatypes. Other than that, records come equipped with the usual lookup syntax and two specialities. The first being a dynamic label check that returns a boolean as a result and secondly, a way to specify a default value in case the previous check turned out to be negative.

Functions take one argument, a _pattern_. This pattern can be a single label or adher to a _record-like_ structure, allowing multiple fields to be present, possibly with _default arguments_. This way a function taking multiple arguments can be created without resorting to currying. These functions can then be called with a record from which the "single arguments" are taken. This forms a neat syntax ambiguity where function definitions and their supplied arguments can be read as functions taking records or as elaborate functions with multiple arguments and possibly default arguments.
Patterns can alse be marked _open_ with the ellipsis (…), otherwise their are regarded as _closed_. Thye can also be given default arguments with the `?` syntax. An example would be `{a, b ? "pratt", …}` which is an _open_ pattern with a default value of "pratt" for the label $b$.

Let-expressions can have multiple bindings $a_1 = t_1; … ; a_n = t_n$ before the `in` keyword appears, possibly referencing each other in a _recursive way_. Both let-statements and records allow _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let expression. This feature is only syntactic sugar to build records and let-expressions easier and does not complicate the typesystem.
Let statements can also take a root path $p$ which is prefixed to all following labels. This way, a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will desugar to `robert = world.objects.players.robert; anders = world.objects.players.anders;` in the surrounding record or let-expression.

The _with statement_ expects an arbitrary expression that reduces to a record. Every field from this record is then added to the scope of the next expression without shadowing existing variables. This is further discussed in @with.

== Reduction Rules

#reduction <reduction>


Since nix supports patterns with default values and the _open_ modifiers, the function reduction rules become quite verbose. The simplest case is R-Fun which takes an argument t₁ and replaces the occurences of $l$ with said argument in the function body t₂. The next function rules R-Fun-Pat-∗ reduces functions taking patterns, the R-Fun-Pat being the simplest of such. We draw i,j from the index Set ℐ and range them over labels such that if i = j then l_i = l_j.
Since the same index $i$ is used for both the argument and pattern in R-Fun-Pat, they must agree on the same labels which resembles closed-pattern function calls. In the contrary case where the pattern is open, the argument-record can range over arbitray labels (possibly more than in the pattern). In this case, the side-condition enforces that at least the pattern fields are present (R-Fun-Pat-Open).

The R-fun-Pat-Default-∗ rules range over pattern elements $e$ which can be either single labels $l$ or labels with a default values like $l : d$. The former case can be converted to the latter with ε-extension transforming $l$ to $l ? ε$ which is equivalent to $l$ due to the shorthands (TODO: can you do this?). The variables of the body are then substituted twice. First with the argument values and then with the default values to "fill the gaps". The open case needs a side-condition analogous to the former open case.

Since ${oi(e_i)}$ strictly subsumes ${oi(l_i)}$ due to its inner structure, rule 2 and 3 are only stated as a mental stepping stone for the reader but not mentioned further.


= Type System
What follows are the typing and subtyping rules as well as an overview over the constraint subroutine.

#types <types>

Garnix models all literal syntax categories with the respective atom types bool, string, path and num. Notice, that we do not distinguish between float and int as they are coerced during interpretation and thus used interspersely in practice. We also add the usual types for fuctions, records and arrays and note that record  types only define a _single_ label to type mapping instead of multiple. This is due to the use of subtyping constraints and their accumuation on type variables during type inferene. This mechanism is further discussed in \@section_todo. Also, we introduce two types for arrays, one for homogenous arrays of the same type and one accumulative for the case that an array has many distinct elements.
To form a boolean algebra of types we add the expected type connectives $union.sq, inter.sq, ~$ as well as a top and bottom type which represent the least type which is subsumed by every other type and the greatest type which subsumes every other type respectively.
Lastely, we add a single type for patterns. Even thought a pattern is similar in strucuter to a record, the pattern type is an accumulated type with multiple fields. This distinction is made due to the syntactical difference of the two. Patterns are introduced and eliminated atomically unlike a record where every fild acces $"record.field"$ results in new independent constraints. The superscript b can be true or false, ascribing whether the pattern is _open_ or _closed_.


#typing_rules <typing_rules>

#subtyping <subtyping>

- ⊳ and ⊲ are used to add and remove _typing hypotheses_ that are formed during subtyping. Since applying such a hypothesis right after assumption, the later modality ⊳ is added and can only be removed after subtyping passed through a function or record construct. *TODO: check*
- The general idea of the typing algorithm is, that typing progresses and finally constraints are installed on type-variables. The rules need to be chosen in a way, that this general approach is possible.


#constraining <constraining>



#pagebreak()
= Appendix A <prelude>
== List of Builtins
- *abort* `s` : Abort Nix expression evaluation and print the error message `s`.
- *add* `e1 e2` : Return the sum of the numbers `e1` and `e2`.
- *addDrvOutputDependencies* `s` : Copy string `s` while turning constant string context elements into derivation-deep string context.
- *all* `pred list` : Return `true` if `pred` returns `true` for all elements of `list`, else `false`.
- *any* `pred list` : Return `true` if `pred` returns `true` for any element of `list`, else `false`.
- *attrNames* `set` : Return the attribute names of `set`, sorted alphabetically.
- *attrValues* `set` : Return the values of attributes in `set`, ordered by sorted names.
- *baseNameOf* `x` : Return the last component of path or string `x`.
- *bitAnd* `e1 e2` : Bitwise AND of integers `e1` and `e2`.
- *bitOr* `e1 e2` : Bitwise OR of integers `e1` and `e2`.
- *bitXor* `e1 e2` : Bitwise XOR of integers `e1` and `e2`.
- *break* `v` : In debug mode, pause evaluation and enter REPL; otherwise return `v`.
- *builtins* : A set containing all built-in functions and values.
- *catAttrs* `attr list` : Collect the attribute `attr` from each set in `list`, ignoring sets without it.
- *ceil* `double` : Round `double` up to the nearest integer.
- *compareVersions* `s1 s2` : Compare version strings; returns `-1`, `0`, or `1`.
- *concatLists* `lists` : Flatten a list of lists into a single list.
- *concatMap* `f list` : Equivalent to `concatLists (map f list)`.
- *concatStringsSep* `sep list` : Join strings in `list` with separator `sep`.
- *convertHash* `args` : Convert a hash string between formats (base16, sha256, SRI, etc.).
- *currentSystem* : System string like `"x86_64-linux"`.
- *currentTime* : Unix time at moment of evaluation (cached).
- *deepSeq* `e1 e2` : Like `seq`, but fully evaluate nested structures in `e1` first.
- *dirOf* `s` : Directory component of string `s`.
- *div* `e1 e2` : Integer division.
- *elem* `x xs` : `true` if `x` is in list `xs`.
- *elemAt* `xs n` : Return the `n`-th element of `xs`.
- *false* : Boolean literal `false`.
- *fetchClosure* `args` : Fetch a store path closure from a binary cache.
- *fetchGit* `args` : Fetch a Git repo or revision.
- *fetchTarball* `args` : Download and unpack a tarball.
- *fetchTree* `input` : Fetch a tree or file with metadata.
- *fetchurl* `arg` : Download a URL and return store path.
- *filter* `f list` : Return elements where `f` yields `true`.
- *filterSource* `pred path` : Copy sources filtering by `pred`.
- *findFile* `search lookup` : Search `lookup` in `search` path.
- *floor* `double` : Round `double` down to nearest integer.
- *foldl'* `op nul list` : Left fold over `list` with `op`.
- *fromJSON* `e` : Parse JSON string `e` into Nix value.
- *fromTOML* `e` : Parse TOML string `e` into Nix value.
- *functionArgs* `f` : Return formal argument set of function `f`.
- *genList* `generator length` : Generate list of given `length` using `generator`.
- *genericClosure* `attrset` : Compute transitive closure of a relation.
- *getAttr* `s set` : Return attribute `s` from `set`.
- *getContext* `s` : Return derivation context of string `s`.
- *getEnv* `s` : Return environment variable `s`.
- *getFlake* `args` : Fetch flake reference and outputs.
- *groupBy* `f list` : Group elements by key `f(element)`.
- *hasAttr* `s set` : `true` if `set` has attribute `s`.
- *hasContext* `s` : `true` if string `s` has nonempty context.
- *hashFile* `type p` : Compute hash of file at `p`.
- *hashString* `type s` : Compute hash of string `s`.
- *head* `list` : First element of `list`.
- *import* `path` : Load and evaluate Nix file at `path`.
- *intersectAttrs* `e1 e2` : Attributes in `e2` whose names occur in `e1`.
- *isAttrs* `e` : `true` if `e` is an attribute set.
- *isBool* `e` : `true` if `e` is a boolean.
- *isFloat* `e` : `true` if `e` is a float.
- *isFunction* `e` : `true` if `e` is a function.
- *isInt* `e` : `true` if `e` is an integer.
- *isList* `e` : `true` if `e` is a list.
- *isNull* `e` : `true` if `e` is `null`.
- *isPath* `e` : `true` if `e` is a path.
- *isString* `e` : `true` if `e` is a string.
- *langVersion* : Integer of current Nix language version.
- *length* `e` : Length of list `e`.
- *lessThan* `e1 e2` : `true` if `e1 < e2`.
- *listToAttrs* `e` : Convert list of `{name, value}` to attrset.
- *map* `f list` : Apply `f` to each element of `list`.
- *mapAttrs* `f attrset` : Apply `f` to each attribute in `attrset`.
- *match* `regex str` : If `regex` matches `str`, return capture groups, else `null`.
- *mul* `e1 e2` : Multiply integers `e1 * e2`.
- *nixPath* : List of search path entries for lookups.
- *nixVersion* : String version of Nix.
- *null* : Literal `null`.
- *outputOf* `drv out` : Return output path of derivation.
- *parseDrvName* `s` : Parse a derivation name into components.

#page[
  #bibliography(
    ("bib/misc.bib", "bib/parreaux.bib", "bib/nix.bib"),
    style: "iso-690-author-date",
  )
]
