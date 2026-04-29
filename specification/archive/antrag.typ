#import "functions.typ": *
#import "typesystem.typ": *
#import "./figures/reduction-compare.typ": comparison
#import "./figures/builtin-types.typ": builtin_types
#import "./figures/module-types.typ": module_types
#import "./figures/ts-compare.typ": ts-compare
#import "sections/occurrence.typ"
#import "sections/connectives.typ"
#import "sections/records.typ"
#import "sections/modulesystem.typ"
#import "sections/ts-discussion.typ"

#show: template
#set figure(placement: auto)
#set raw(lang: "nix")

#set document(
  title: "LGFG NixOS Sebastina Klähn",
  description: "Nix Proposal for the LGFG grant about Nix type inference.",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)


== Type Inference for the Nix Language
This document lays out the motivation to create a typesystem for the Nix programming language and is meant as a promotion for the LGFG scholarship at the University of Freiburg for doctoral grants.

== Structure
We start by giving a short introduction to Nix and NixOS to motivate the efforts by their real-world significance. We then elaborate the languages design and the difficulties in creating a sound and complete typesystem for a dynamically typed language such as Nix, that was not created with type inference in mind. The discussion includes constraints from the real-world use of the language, approaches to overcome these problems and multiple research directions of interest.

== Nix and NixOS

NixOS is an operating system that is based upon the Nix package manager with the internal, epynomous programming languagage also called Nix. The Nix package manager was developed to overcome the problem of _distributing software between different computer environments_ in a way that the transferred software can be safely executed on without malfunctioning. It has sinced developed into an ecosystem with containing an operating system (NixOs) and home configuration manager (home-manager) and is used by thousands of developers. The Nix 2024 survey reported a 33% surge in participants and also industry players turn to nix for its reproducibility and security guarantees.

Both of these properties root from the underlying design principles of Nix that we will explain shortly. By using an approach that conceptualizes package management as a memory management discipline, files are interpreted as memory locations and references as pointers between them @memory_to_software @dolstra_phd @nixos_long. This resulting technique can be compared to a _garbage-collector_ that consistently tracks dependencies during package construction and resembles them as a closure: a complete dependency graph rooted at the built artifact and containing all recursively required components. This self-contained closure can be transferred to other machines and reconstructed without relying on the target system’s ambient state, leading to save and reproducible software.

// To gain its reproducibilty guarantees, Nix relies on the _nix store_, a read-only directory that contains the transferred, immutable buildartifacts. Each artifact is identified by a cryptographic hash derived from its inputs, enabling efficient equality checks and facilitating _maximal reuse_ of existing components. It also allows multiple versions or variants of a package to coexist in the store without interference, since each is uniquely addressed and isolated. Because build outputs are pure derivations of their declared inputs packages can be updated without mutating or invalidating prior ones, enabling _reliable rollbacks_ and _atomic upgrades_ @memory_to_software. Moreover, the explicit tracking of dependency can be used to find unreachable store paths by tracing from a set of designated roots, thereby reclaiming disk space while preserving consistency.

These strong guarantees of nix are a direct consequence of the underlying _domain specific language_ (DSL) that makes them an inherent property instead of a retrofitted qualities. First and foremost, one of nix' greatest strengths – _reproducibility_ – is a direct consequence of the languages' functional design. When abstracting files and references as memory locations and references, it can be noticed that _pure functionality_ provides the features needed for secure dependency management. A pure function computes its output solely given its input fields and the final value can be memoized and reused, should it be needed again. The nix package manager uses pure functions without side effects to build packages in a clean and sandboxed environment and since no externalities can affect the build, the outcome is guaranteed to be equal if run twice, even on differing machines.

Especially in face of accelerated software development cycles using artifically generated code where thorough human verification might be missing, the need for declarative and effective dependency management is needed to increase security and trust in deployments. The Nix package repository is the biggest and most up to date repository in existance and to the work of over 4.000 unpaid contributors that report and fix security vulnerabilities. Since AIs get increasingly good in exploiting security vulnerabilities, quick fixes and rollout become more important.

Static type systems help in writing secure and reliable software by identifying insecure code statically before running it. Using type inference and language servers, it is both possible to write software _faster_ and _more secure_ at the same time. This is why contemporary languages like Rust, Zig, Kotlin and Z utilize static type inference and the reason that type systems like Typescript and Flow have found great industry adoption. The goal of this work is similar to Typescript in that it tries to retrofit a typesystem for a dynamic language (in this case Nix) to optimistically help programmers to write better software. AI agents like Claude, Cursor and Aider already utilize static type systems to fix minor syntactic errors that are commonly generated by probabilistic language models and can utilize type systems in the same way.


== Finding a Type System <ts-dicsussion>
Nix is a dynamically typed, lazy, and purely functional language. Its core features include extensible records, pattern-based functions with parameter destructuring, first-class labels, overloaded operators, and a reduction semantic with two levels of binding power. In addition, the language provides 78 built-in functions that operate on attribute sets and lists, access the execution environment, and support limited forms of type reflection.

The interaction of these features yields a language of considerable expressive power and to date, no type system is known that can capture the full expressiveness of Nix while preserving soundness and practical tractability. Consequently, any attempt at static typing must restrict itself to a carefully chosen subset of the language in order to obtain a well-behaved and analyzable system.

In practice, most users engage with Nix through the module systems of NixOS and home-manager to configure their operating system or user environment. Within this context, autocompletion for option values that consist of a type, default-value, example and description is most valuable. Although both module systems provide online services #footnote(link("https://home-manager-options.extranix.com/")) #footnote(link("https://search.nixos.org/options")) to gain this information, there exists no satisfactory solution that works in IDEs #footnote("Integrated Development Environment").

The module system is a part of the Nix standard library and utilizes the languages core features, such that full option type inference is a feature that builds upon Nix-language type inference. It is in principle possible to determine types for option values and improve completion, but whether the precision needed to faithfully deduce informative types for options is attainable is not immediately clear.

Besides module configuration, developers use Nix as a programming language in the standard library and across the packaging ecosystem. Although this group may be smaller, the benefits of reliable type inference are substantial and have been argued to be necessary to make the language complete @nix-ts-issue and would further lower the barrier to entry for new users.

In the following sections, we survey relevant type-system features and assess their applicability to Nix.

== Wanted Properties

The Nixpkgs repository is, with its 40.000 files and 120.000 packages, the biggest and most up-to-date package repository in existence, receiving approx. 80 PRs#footnote("Pull Requests") a day and having close to 100.000 commits and multiple thousand open issues and PRs. This scale has two immediate implications for type‑inference. First, the evaluation root is effectively a single entry point #footnote([Most features of Nixpkgs are reachable from #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether a flake‑based workflow is used.]), so an inference algorithm that tries to fully evaluate the whole tree would need to evaluate all 120.000 packages. Since no typeinference algorithm exists yet, we can only guess, but a _lazy_ type inference algorithm @lazy_inf might be needed to handle a syntax tree of this size.

Second, evolution at the scale of Nixpkgs is costly and slow. A type inference approach should therefore minimize required changes to the existing language in order to preserve backward compatibility. When changes are unavoidable, ecosystem‑wide migration cannot occur atomically and a gradual typing discipline @gradual_siek @gradual_tobin is needed for incremental adoption. Gradual type systems have a checked and dynamic portion whichs boundary is mediated by an unknown type $star.op$ and casts between the two systems. The same unknown type can act as a conservative static approximation for impure features exposed by the builtins and dunder-variables.

Many type systems reduce inference complexity by relying on explicit annotations, but Nix offers no surface syntax for types. It is possible to embed hints in comments or extend the language incrementally to admit annotations similar to Typescript, but full Hindley–Milner–style inference—requiring no annotations while recovering principal types—would impose the least friction on existing code. Regardless of surface design, the inference procedure must remain efficient at Nixpkgs scale; in particular, global backtracking is unacceptable, as it would render analysis prohibitively slow.

Besides the properties that are required due to the language' environment, a few properties are directly founded by the features of the language. First and foremost _recursive types_, _first class labels_ and _record concatenation_ immediately follow from the expressive record calculus. As surveyed later, these capabilities have been treated in the literature but were only recently combined @extensible_tabular @extensible_rec_funcs @extensible_data_adhoc and have not yet been consolidated in mainstream languages. In addition, Nix requires _parametric polymorphism_ to capture generalization across let‑bindings, and _ad‑hoc polymorphism_ to model overloaded operators and user‑defined dispatch via type inspection.

Last but not least, the combination of _subtype polymorphism_ and _type connectives_ has been shown to be applicable to a lot of programming paradigms @castagna2023programming @mlstruct and their combination leads to very expressive typesystems that can track the flow of programs and naturally implement _occurrence typing_. It has been shown that a full boolean algbra of types can encode _overloading_, _variant types_, _bounded polymorphism_, _pattern type conditionals_ and _pattern matching_ @castagna2023programming. Given that such systems already address many of our desiderata, adopting them as a foundation is a natural starting point.

This concludes the list of wanted properties:
- Absence of type annotations
- Efficient computation
- Open record concatenation
- First class labels
- Recursive types
- Subtype polymorphism
- Parametric polymorphism
- Adhoc polymorphism
- Reflection
- Gradual typing (weak)
- Occurrence typing
- Type connectives

== Typesystem Discussion
We will now look at a few properties in closer view and discuss their applicability to Nix.


== From Static vs. Dynamic to Gradual to Occurrence Typing <occurrence>
Static type systems analyze programs at the meta‑level to establish properties without executing code. They enforce important safety guarantees such as null‑safety @pearce_flowtyping, panic‑freeness, and the absence of use‑after‑free errors @rust. Inferred annotations further aid reasoning about higher‑order calls, deconstruction of nested data, and interactions with unknown libraries, helping programmers to write safe programs faster. Nevertheless static checking is a conservative, compile‑time abstraction of runtime behavior and thus an inevitably incomplete approximation @coldwar, which may reject valid programs or require auxiliary annotations to satisfy the type checker.

Many scripting languages have thus turned towards dynamic typing to overcome these shortcommings. At this end of the spectrum, no static invariants are imposed a priori; expressiveness is maximized, but runtime failures become possible. The lack of annotation overhead and the availability of reflection make this style attractive during exploration. Only when systems grow and interactions between modules, functions, classes and services become more complex, static type systems and their strong guarantees show their value. The growing popularity of Flow and Typescript @flow @typescript shows the general trend towards type safe programming for inherently dynamic languages like javascript and the need to mix the two approaches.

Gradual type systems @gradual_siek @gradual_tobin aim to reconcile these opposing positions by allowing statically typed and dynamically typed code to coexist. A distinguished unknown type $star.op$ mediates the boundary, together with casts that refine unknown values to more precise types. A rich literature explores the design space @gradual_siek @gradual_tobin @cantblamethis @agt @gradual_extensible_rows @consistent-subtyping @blame_for_all, but many proposals rely on explicit annotations and cast insertion—features we prefer to avoid.

Flow typing @flow, and in particular the more disciplined occurrence typing @revisiting_occurrence @on_occurrence, offers a complementary approach that refines types along control‑flow and don't immediately need casts and annotations. Consider `if isBool(x) then !x else x + 1`: the predicate tests at runtime whether `x` is a boolean. In the positive‑branch, `x` is treated as a bool; in the else‑branch, negative information that `x` is not a bool enables alternative typing. Similar refinements arise across pattern‑matching constructs.

// In a match expression such as `match x with bool(x) -> .. | rec(x) -> .. | _ -> x`, branches are considered in order and behave like conditionals that impose type constraints. The default case (\_) can be typed under the assumption that `x` is neither a bool nor a record because the previous branches did not trigger; moreover, an exhaustiveness check can be implemented by comparing the type of the default branch with ⊥, motivating the inclusion of type‑level negation.


=== Impurities <impurities>
Although Nix is designed as a pure, lazily evaluated functional language, its standard library deliberately exposes aspects of the host environment and file system to support reproducible builds and modular composition. This integration complicates static analysis: certain primitives are observationally impure, and in the absence of evaluation they can undermine purely static reasoning.

A prominent example is dynamic attribute selection via the target system, e.g., `g.${builtins.currentSystem}`. The value of `builtins.currentSystem` is supplied by the evaluator and is used pervasively in real‑world code (observed in 207 public occurrences). #footnote(link("https://sourcegraph.com/search?q=context:global+%24%7Bbuiltins.currentSystem%7D&patternType=keyword&sm=0")) While a type system could, in principle, model this constant in a fixed evaluation context, other primitives expose inherently unpredictable information. In particular, `builtins.currentTime` reflects wall‑clock time and is not amenable to static prediction. Related impure queries include `currentSystem`, `currentTime`, `fetch\*`, `findFile`, `langVersion`, and `nixVersion`; from a typing perspective, these may produce values of arbitrary shape.

Because no static analysis can be both sound and complete in this setting, a degree of _graduality_ or _softness_ is required. We therefore need an unknown type $star.op$ to account for the results of impure or otherwise opaque computations, allowing programs to remain typable without committing to unsound assumptions. This stance mirrors practice in mainstream ecosystems—most notably TypeScript @typescript—which is known to be incomplete with respect to distributivity of intersections and unions, yet remains highly effective in practice.

== Type Connectives <connectives>
Type connectives—union $τ_1 ∨ τ_2$, intersection $τ_1 ∧ τ_2$, and negation $¬τ$—play a central role in contemporary typed languages @flow @typescript @typed_racket @mlstruct @elixir_design_principles @poly_records @typing_records_etc. They provide a principled means of relating otherwise disjoint types and of expressing control‑flow–sensitive specifications. For example, the function `x: y: z: if x then y else z` returns either y or z depending on the boolean guard x and can be assigned the type $bool -> α -> β -> (α ∨ β)$, capturing that the result ranges over the union of the possible outcomes. Importantly, the connective tracks value flow: $α$ and $β$ are combined only at the output, not conflated at the inputs. This strictly improves on ML‑style unification, which would attempt to identify $α$ and $β$ and thereby fail for, say, integers versus strings @algebraic_subtyping.

Intersection types support ad‑hoc overloading by ascribing multiple, behavior‑specific function types simultaneously. Consider `x: if isBool(x) then !x else x + 1`, which either negates a boolean or increments an integer. A coarse typing that only uses type unions is $(bool ∨ int) -> (bool ∨ int)$ stating that the function accepts either a bool or int and returns either a bool or int. By admitting intersection types, this type can be refined to $(int -> int) ∧ (bool -> bool)$, stating that integer inputs yield integers and boolean inputs yield booleans; the strictly more expressive type.

Furthermore, type connectives empower _occurrence typing_ and enable a form of _bounded polymorphsim_ @xie2020row @castagna2023programming.


== Records <records>

Records form natural data structure for declarative configurations, and the Nix language has consequently developed a rich record calculus. The resulting feature combination of _recursive attribute sets_, _dynamic attribute selection_, and _record concatenation_, in interaction with _pattern functions_, _with-construct_ and _inherits_ has not yet been comprehensively addressed by existing type systems. In this section we want to give an overview of the requirements needed to type Nix records and discuss existing typesystems and their advantages and shortcomings in regards to Nix.

Nix records are immutable: updates are realized by construction rather than by in-place mutation, and there are no primitive add or delete operations. To regain expressiveness, the language provides _asymmetric record extension_ via the right-biased concatenation operator (`a // b`). Typing this operator is subtle: safe use requires the ability to express both field extension and field absence (restriction) in the type system.

Record concatenation was first discussed by Harper and Pierce @symm_concat but limmited to _symmetric concatenation_ of records that do not overlap in their defined fields. _Asymmetric concatenation_ @concat4multiinher lifts this restriction so the same fields can appear in both records. In systems, where doubled labels are not allowed, _lacks predicates_ @poly_records regain the needed expressiveness to not only track field presence (due to record fields) but also the absence of fields in generic rows. This allows for _asymmetric concatenation_ to be typed safely without ambiguity due to doubled record labels. Further advancements were made nearly a decade later by Daan Leijen who showed a typesystem with duplicate labels @extensible_recs and a lookup semantic that is right-biased. He also showed how to make labels first class inhabitants of the language in @fc_labels. This work together constitutes most of the features needed to type records in Nix and were already combined by Adam Patzke and Ningning Xi in application to tabular data @extensible_tabular. Other systems that combine these features are @extensible_rec_funcs @extensible_data_adhoc. All of these typesystems use unification, sometimes in combination with qualified types as their workhorse of inference but have shown syntactically heavy and don't include subtyping.

Subtyping is a common form of polymorphism that that forms a type-hierarchy: any value of a subtype may be used wherever a supertype is expected. While subtyping is straightforward for ground types, its interaction with higher‑order functions, variance, other language constructs make type inference hard.

To address these challenges, both Stephen Dolan and Giuseppe Castagna advocate designing type systems around subtyping, which has led to two influential lines of work: _semantic subtyping_ @gentle_intro @frisch_semantic @frisch2002semantic and _algebraic subtyping_ @algebraic_subtyping @simplesub @mlsub. The former adopts a set‑theoretic interpretation of types, whereas the latter utilizes order‑ and lattice‑theoretic algebra. We briefly review these approaches in the following.

The principal idea of _semantic subtyping_ is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, giving types a semantic meaning. In the set-theoretic model of types, type-union relates to set-unions $τ_1 ∨ τ_2 <=> ⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $τ_1 ∧ τ_2 <=> ⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $¬τ <=> 𝟙 without ⟦τ⟧$ and subtyping becomes merely a question of set-inclusion.
Following the seminal work on semantic subtpyping, Castagna has shown how to handle _records_ @poly_records @typing_records_etc, _occurrence typing_ @revisiting_occurrence @on_occurrence, _gradual typing_ @gradual_perspective @gradual_elixir and _first class labels_ @typing_records_etc, showing the expressiveness of the system. But this expressiveness comes at a cost, the cost of complexity. The first calculi surrounding semantic subtyping relied heavily on backtracking and were unreasonably slow for real world applications. Improvements have been made, using, for example, boolean formula abstractions to compute efficient solutions @typing_records_etc, but many techniques used to make CDUCE @CDUCE performant are not even documented (from personal corespondance with Stefan Wehr). Also, semantic subtyping relies on heavy theory to compute subtypes in presence of type connectives, similar to the work of Pearreaux @simplesub @mlstruct @invalml @superF which will be discussed later.

Stephen Dolan develops a related approach named _algebraic type systems_, grounded in order theory. He first specifies a distributive lattice of types and systematically inherits its algebraic properties. By polarizing the occurrences of union and intersection—restricting them to positive and negative positions—a totally ordered distributive lattice can be obtained, enabling lossless simplification of subtyping constraints. Operationally, the system resembles Standard ML: unification is replaced by bi‑unification, a procedure that solves subtyping rather than equality constraints. This design leads to concise and transparent algorithms for subsumption checking and type inference, while preserving the classic ML advantages of principled inference and efficient, backtracking‑free type inference.

Simplesub @simplesub operationalizes this approach by implementing algebraic subtyping with equi‑bounded polymorphism: type variables are constrained by simultaneous lower and upper bounds. The subsequent mlstruct system @mlstruct removes the polarization restriction and attains a _full boolean algebra of types_ by admitting negation, thereby exploring the expressiveness of boolean algebraic subtyping. This line was then extended to effect systems @invalml and System F @superF, employing skolem and rigid variables together with nontrivial constraint‑solving machinery to preserve principled inference. However, the approach is notably verbose. By eschewing an order‑theoretic foundation in favor of a syntactic presentation of the subtyping hierarchy, the metatheory becomes lengthy—the main mlstruct paper spans 146 pages, with only the first thirty devoted to exposition and the remainder to proofs. Moreover, mlstruct models records via tagged classes rather than anonymous record types: a usable record must be tagged with a class label (via an intersection), e.g., $\#T ∧ {a : τ}$, a design choice made to keep the lattice well behaved. The syntactic setting also facilitates the addition of specialized subtyping rules, such as $(τ_1 → τ_2) inter.sq (π_2 → π_3) equiv (τ_1 inter.sq π_2) → (τ_2 union.sq π_3)$ and ${a : τ} union.sq {b: π} "if" a ≠ b$, which support principled and effective type inference but are unsound in the set-theoretic model. The major drawback of these systems is expressiveness; for instance, intersection‑based overloading—e.g., $(bool → bool) ∧ (int → int)$, a technique extensively used by Castagna for fine‑grained inference—is not directly available in mlstruct.

The difference between the two approaches boiles down to expressiveness vs. computability where Castagna leans more towards expressive typesystems that need backtracking, Parreaux systems are slightly weaker but keep efficient and principled type inference. Other than that they are quite similar even under the hood. Both systems heavily rely on type connectives and battle the emerging complexity of type inference by transforming types to normal forms and solving these with either the explicit subtying rules of Parreaux and Chau or boolean formulas that can be derived for the set-represantion of Castagna.

In regards to Nix, type inference both have major drawbacks. For the systems of Parreaux, the unusability of intersections to create overloaded functions is a major drawback because the addition operator needs overloading and using reflection, it is generally possible to write functions that can behave differently depending on the types of supplied arguments. For Castagnas work, the bad computability in face of Nix' huge syntax trees is the biggest drawback. It has to be noted though, positively, that Castagna already carved out approaches for gradual and occurrence typing, even though that is regarded to his continuous endeavor to type another language, elixir @castagna2023elixir.

// #modulesystem.export
// #ts-discussion.export
== Motivation
We have seen the multitude of perspectives in regard to Nix typability. During my PhD., I want to examine the multitude of possibilities to create a sound typesystem for Nix and develop a most comprehensible solution that is applicable to existing code bases and gives meaningful information for programmers. The main challanges I see during this development is the computational complexity of the huge nix syntax tree and the complexity of type inference that can easily lead to undecidable algorithms or overapproximations that do not give meaningful insight. The nix language is a model language but my studies can be extrapolated to other dynamic languages and support the general trend towards type inference for dynamic languages.

#bib
