#import "../functions.typ": *

== TODO
- Are row-variables actually better in gradual systems?

== Records: History

- A Semantics of Multiple Inheritance - Luca Cardelli ← Introduction of records?
- Complete type inference for simple objects (WA 1988)
- A record calculus based on symmetric concatenation (HA&PI 1990)
- Typeinference for records in a natural extension of ML (RE 1901) ← Rows??
- Typing record concatenation for free (RE 1992)
- A polymorphic type system for extensible records and variants (GA & JO 1996) ← lacks
- First class labels for extensible rows (LE 2004)
- Extensible records with scoped labels (LE 2005)
- Infix extensible records for Tubalar data (PA & Xie 2023)


#let export = [
  == Records
  Nix is a language that heavily resolves around records because key-value pairs have proven best for declarative configuration management. It is thus no suprise that nix elaborates a record model with lots of features, leading to a feature-combination that has not yet seen a typesystem that fully covers it. In this section we want to give an overview of the requirements needed to type nix records and discuss existing typesystems and their advantages and shortcommings in regards to nix.


  Nix records are _immutable_, meaning there exist no discrete delete and add operations on them. This simplifies the requirements for typesystems a little bit, but to compensate for this shortcomming, the language adds free record extension with the concat operation (`a // b`). This feature has shown tricky in the past because it eventually needs record extension and restriction to by typable in the first place. Nix also features first class labels (labels that can be computed on). This feature is tricky because complicates the tracking of fiels that exist in a record and which fields are accesses, especially in a polymorphic setting. First class labels have also been understudied in the past, receiving only litte attention @fc_labels @extensible_tabular. Last but not least, nix records are recursive such that a type inference algorithm has to account and detect reference cycles during type inference.


  Record concatenation was first discussed by Harper and Pierce @symm_concat but limmited to _symmetric concatenation_, meaning it was only possible to concatenate records that do not overlap in their defined fields. _asymmetric concatenation_ @concat4multiinher is a generalization of their work by allowing the same fields to appear in both records. In systems, where doubled labels are not allowed _lacks predicates_ @poly_records regain the needed power to not only track field presence (due to record fields) but also the absance of fields in generic rows. This allows for _asymmetric concatenation_ to be typed safely without the danger of doubled record labels. Further advancements were made nearly a decade later by Daan Leijen who showed a typesystem where duplicat labels were allowed @extensible_recs and a lookup semantic that is right-biased. He also showed how to make labels first class inhabitants of the language in @fc_labels. This work together constitutes most of the features needed to type records in nix and were already combined by Adam Patzke and Ningning Xi in application to tabular data. All of these typesystems use unification, sometimes in combination with qualified types as their workhorse of inference. These systems have shown syntactically heavy and also don't benefit from the advantages that strong subtyping systems provide. We will thus look at typesystems that heavily rely on subtyping hirarchies now.

  _Subtyping_ is a common form of polymorphism because it is applicable in so many places. Subtyping basically creates a hirarchy of types with sub- and supertypes. The only property that has to hold for two types in a relation is that a subtype of some supertype has to be applicable at every program point, meaning functions can be called with a dog instead of a generic animal (to take the example seen in virtually every blogpost or paper) or one can substitute a list of pinapples for a list of fruits. This form of polymorphism is easy to understand in the simple case but gets complex quickly when functions are taken into account and lead to complex typesystems.

  To overcome this complexity, both Stephen Dolan and Giuseppe Castagna state quite literally, that they want to >> base their type system around subtyping <<, leading to the development of _semantic subtyping_ @gentle_intro @frisch_semantic @frisch2002semantic and _algebraic subtyping_ @algebraic_subtyping @simplesub @mlsub. One based on sets and one based on algebra. Lets look at them shall we?

  The principal idea of _semantic subtyping_ is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, giving types a semantic meaning. In the set-theoretic model of types, type-union relates to set-unions $τ_1 ∨ t_2 arrow.double ⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $t_1 ∧ t_2 arrow.double ⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $¬τ arrow.long 𝟙 without ⟦τ⟧$ and subtyping merely a question of set-inclusion.
  Following the seminal work on semantic subtpyping, Castagna has shown how to handle records @poly_records @typing_records_etc, occurrence typing @revisiting_occurrence @on_occurence, gradual typing @gradual_perspective @gradual_elixir and even first class labels @typing_records_etc, showing the expressiveness of the system. But this kind of exrpessiveness comes at a cost, the cost of complexity. The first calculi surrounding semantic subtyping relied heavily on backtracking and were thus unreasonably slow for real world applications. Since then advancements have been made, using, for example, boolean formula abstractions to compute efficient solutions (citation needed), but many techniques used to make CDUCE @CDUCE performant are not even documented (from personal corespondance with Stefan Wehr). Also, semantic subtyping relies on some heavy theory to compute subtypes in presence of type connectives, similar to the work of Pearreaux @simplesub @mlstruct @invalml @superF which will be discussed later.

  Stephen dolan empolys a similar approach in _algebraic type systems_ but bases it on order theory. He starts by defining a distributive lattice (thus algebraic) that inherit the lattice' properties. By further restricting the the occurences for union and intersections to positive and negative positions, a distributive lattice with total order can be constructed that allows for lossless reduction of subtyping constraints. In essence, the system is standart ML, with a lattice of types and unification replaced by bi-unification, a subroutine that handles subtyping constraints instead of equality constraints. The final algorithms for subsumption checking and type inference are short as well as simple, all thanks to the initial focus on well-formed types. The final algorithms inherit the standart ML properties, namely _principled type inference_, no need for type annotations and effectiveness i.e no backtracking.

  Parreaux started his line of work with Simplesub @simplesub, showing how algebraic subtyping can be implemented using lower und upper bounds on variables called _equibounded polymorphism_. The former limitations of polarized types and were later lifted in mlstruct, a typesystem that firstly explored the expressiveness of full _boolean algebraic subtyping_ by also adding negation types and thus forming a fool boolean algebra. Afterwards he showed how to extend this approach to effect systems @invalml and SystemF @superF increasing the expressiveness of existing typesystems of the latter using skolem variables, rigid variables and a complicated constraint algorithm. The major drawback of this approach is notably its verbosity. Since they disregard the former formalization in terms of order theory and form their subtyping hirarchy using syntactic rules, the proofs turn out to be very long. The main paper of mlstruct is 120 pages long where only the first 30 explain the system and the rest is only proofs. Other than that, mlstruct uses tagged classes instead of pure record constructs, meaning a record has to be tagged by a classlabel (using a union) to be usable $\#t ∧ {a : τ}$. This decision is delibarte to make the subtyping lattice well behaved. Since they use a syntactic systems, they can easily add subtyping rules like $(τ_1 → τ_2) inter.sq (π_2 → π_3) equiv (τ_1 inter.sq π_2) → (τ_2 union.sq π_2)$ that relate function types and others like ${a : τ} union.sq {b: π} "if" a ≠ b$ to enable principled and effective typinference. This reduction comes at the price of expressiveness, for example overloding with intersections types $(bool → bool) inter (int → int)$, a technique used extensively by castagna to gain expressiveness and fine grained type inference is not applicable in mlstruct.

  The difference between the two approaches finaly boiles down to expressiveness vs. computability where castagna leans more towards expressive typesystems that need backtracking, parreaux systems are slightly weaker but keep efficient and principled type inference. Other than that they are quite similar even under the hood. Both systems heavily rely on type connectives and battle the emerging complexity of type inference by transforming types to normal forms and solving these with either the explicit subtying rules of Parreaux and Chau or boolean formulas that can be derived for the setrepresantion of castagna.

  In regards to nix type inference both have major drawbacks. For the systems of parreaux, the unusability of intersections to create overloaded functions is a major drawback because the addition operator  needs overloading and using reflection, it is generally possible to create functions that can act on different types. For castagnas work, the bad computability in face of nix' huge syntax trees is the biggest drawback. It has to be noted though positively, that castagna already carved out approaches for gradual and occurrence typinge, even though that is regarded of his continuous endeavor to type another language, elixir @castagna2023elixir.
]


== Comparison-draft
- Dolan: Lattice of types, Extensional,
- Castagna: Sets, Denotational, Universes, Occurrence, Function-annotations,
- Parreaux: Boolean Algebraic, Syntactic, Verbose, Levels, Bounds, Skolems, Rigid Vars,

The Workhorse in Parreaux: Carefully crafted inference rules, Normal-Forms
The Workhorse in Castagna: Boolean Formulas, Reduced to Normal-Forms

== Regarding Perreaux
- Since overloading is not possible in boolean algebra systems, we can not fully type nix in them. This is because due to the reflection checks, it is possible to overload functions.
- μ-rule is not possible because recursion might aries during type checking and is not attached to any syntax
- Tagged classes instead of anonymous record types

Unchanged rest: { a: int; | r} -> { a: float; r}
Extension: { .. }

== Parreaux
Singleton record type: { t : τ }

== Things to say
- Castgna combined row poly and subtyping @poly_records.


== Questions
- How does record extension work in parreaux?
  - Unchanged rest: { a: int } ∧ { .. } -> { a: int } ∧ ¬{ a : int} ∧ { .. } ~> { .. }

#bib
