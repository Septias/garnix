#import "../functions.typ": *

== TODO
- Are row-variables actually better in gradual systems?
- Polymorphic records for dynamic languages: no open record extension

== Records: History

- A Semantics of Multiple Inheritance - Luca Cardelli ← Introduction of records?
- Complete type inference for simple objects (WA 1988) ← Rows
- A record calculus based on symmetric concatenation (HA&PI 1990)
- Typeinference for records in a natural extension of ML (RE 1901) ← Rows??
- Typing record concatenation for free (RE 1992)
- A polymorphic type system for extensible records and variants (GA & JO 1996) ← lacks
- First class labels for extensible rows (LE 2004)
- Extensible records with scoped labels (LE 2005)
- Infix extensible records for Tubalar data (PA & Xie 2023)

add: @concat4free
add: Abstracting Extensible Recursive Functions?


#let export = [
  == Records <records>

  Records are the primary structuring mechanism in Nix. Key–value attribute sets provide a natural substrate for declarative configuration, and the language has consequently developed a rich record calculus. The resulting feature combination—recursive attribute sets, dynamic attribute selection, and record concatenation, in interaction with pattern functions—has not yet been comprehensively addressed by existing type systems. This section outlines the requirements for typing Nix records and surveys candidate systems, highlighting their strengths and limitations in this setting.


  Nix records are immutable: updates are realized by construction rather than by in-place mutation, and there are no primitive add or delete operations. To regain expressiveness, the language provides _free record extension_ via the right-biased concatenation operator (`a // b`). Typing this operator is subtle: safe use requires the ability to express both field extension and field absence (restriction) in the type system. Nix further admits first-class labels—attribute names computed by expressions—which complicates static tracking of present and accessed fields, especially under polymorphism; the topic has seen comparatively little attention in prior work @fc_labels @extensible_tabular. Finally, attribute sets are recursive, so an inference procedure must detect and account for reference cycles during typing.


  Record concatenationwas first discussed by Harper and Pierce @symm_concat but limmited to _symmetric concatenation_ of  records that do not overlap in their defined fields. _Asymmetric concatenation_ @concat4multiinher lifts this restriction so the same fields can appear in both records. In systems, where doubled labels are not allowed _lacks predicates_ @poly_records regain the needed expressiveness to not only track field presence (due to record fields) but also the absence of fields in generic rows. This allows for _asymmetric concatenation_ to be typed safely without the danger of doubled record labels. Further advancements were made nearly a decade later by Daan Leijen who showed a typesystem with duplicate labels @extensible_recs and a lookup semantic that is right-biased. He also showed how to make labels first class inhabitants of the language in @fc_labels. This work together constitutes most of the features needed to type records in nix and were already combined by Adam Patzke and Ningning Xi in application to tabular data @extensible_tabular. Other systems that combine these features are @extensible_rec_funcs @extensible_data_adhoc. All of these typesystems use unification, sometimes in combination with qualified types as their workhorse of inference. These systems have shown syntactically heavy and also don't benefit from the advantages that strong subtyping systems provide. We will thus look at typesystems that rely on subtyping.

  Subtyping is a pervasive form of polymorphism that organizes types into a refinement hierarchy: any value of a subtype may be used wherever a supertype is expected. While the substitutability intuition is straightforward for ground types, its interaction with higher‑order functions, variance, and parametric structure is subtle. Designing subtyping relations that are both expressive and tractable has therefore been a central challenge in type theory and programming‑language design.

  To address these challenges, both Stephen Dolan and Giuseppe Castagna advocate designing type systems around subtyping, which has led to two influential lines of work: semantic subtyping @gentle_intro @frisch_semantic @frisch2002semantic and algebraic subtyping @algebraic_subtyping @simplesub @mlsub. The former adopts a set‑theoretic interpretation of types, whereas the latter is grounded in order‑ and lattice‑theoretic algebra. We briefly review these approaches in the following.

  The principal idea of _semantic subtyping_ is to relate types to their set of inhabited types, that is, the set of types that can be given a specific type, giving types a semantic meaning. In the set-theoretic model of types, type-union relates to set-unions $τ_1 ∨ t_2 arrow.double ⟦τ⟧ ∪ ⟦τ⟧$, type-intersection to set-intersections $t_1 ∧ t_2 arrow.double ⟦τ⟧∩⟦τ⟧$ and type negation to set-removal $¬τ arrow.double 𝟙 without ⟦τ⟧$ and subtyping merely a question of set-inclusion.
  Following the seminal work on semantic subtpyping, Castagna has shown how to handle _records_ @poly_records @typing_records_etc, _occurrence typing_ @revisiting_occurrence @on_occurrence, _gradual typing_ @gradual_perspective @gradual_elixir and even _first class labels_ @typing_records_etc, showing the expressiveness of the system. But this kind of expressiveness comes at a cost, the cost of complexity. The first calculi surrounding semantic subtyping relied heavily on backtracking and were unreasonably slow for real world applications. Improvements have been made, using, for example, boolean formula abstractions to compute efficient solutions (citation needed), but many techniques used to make CDUCE @CDUCE performant are not even documented (from personal corespondance with Stefan Wehr). Also, semantic subtyping relies on some heavy theory to compute subtypes in presence of type connectives, similar to the work of Pearreaux @simplesub @mlstruct @invalml @superF which will be discussed later.

  Stephen Dolan develops a related approach under the banner of _algebraic type systems_, grounded in order theory. He first specifies a distributive lattice of types and systematically inherits its algebraic properties. By polarizing the occurrences of union and intersection—restricting them to positive and negative positions—a totally ordered distributive lattice can be obtained, enabling lossless simplification of subtyping constraints. Operationally, the system resembles Standard ML: unification is replaced by bi‑unification, a procedure that solves subtyping rather than equality constraints. This design leads to concise and transparent algorithms for subsumption checking and type inference, while preserving the classic ML advantages of principled inference, little need for type annotations, and efficient, backtracking‑free decision procedures.

  Simplesub @simplesub operationalizes this approach by implementing algebraic subtyping with equi‑bounded polymorphism: type variables are constrained by simultaneous lower and upper bounds. The subsequent mlstruct system @mlstruct removes the polarization restriction and attains a full boolean algebra of types by admitting negation, thereby exploring the expressiveness of boolean algebraic subtyping. This line was then extended to effect systems @invalml and to System F @superF, employing Skolem and rigid variables together with nontrivial constraint‑solving machinery to preserve principled inference. However, the approach is notably verbose. By eschewing an order‑theoretic foundation in favor of a syntactic presentation of the subtyping hierarchy, the metatheory becomes lengthy—the main mlstruct paper spans 146 pages, with only the first thirty devoted to exposition and the remainder to proofs. Moreover, mlstruct models records via tagged classes rather than anonymous record types: a usable record must be tagged with a class label (via a union), e.g., $\#T ∧ {a : τ}$, a design choice made to keep the lattice well behaved. The syntactic setting also facilitates the addition of specialized subtyping principles, such as $(τ_1 → τ_2) inter.sq (π_2 → π_3) equiv (τ_1 inter.sq π_2) → (τ_2 union.sq π_3)$ and ${a : τ} union.sq {b: π} "if" a ≠ b$, which support principled and effective type inference. This convenience, however, comes with trade‑offs in expressiveness; for instance, intersection‑based overloading—e.g., $(bool → bool) inter (int → int)$, a technique extensively used by Castagna for fine‑grained inference—is not directly available in mlstruct.

  The difference between the two approaches boiles down to expressiveness vs. computability where Castagna leans more towards expressive typesystems that need backtracking, Parreaux systems are slightly weaker but keep efficient and principled type inference. Other than that they are quite similar even under the hood. Both systems heavily rely on type connectives and battle the emerging complexity of type inference by transforming types to normal forms and solving these with either the explicit subtying rules of Parreaux and Chau or boolean formulas that can be derived for the set-represantion of Castagna.

  In regards to nix type inference both have major drawbacks. For the systems of Parreaux, the unusability of intersections to create overloaded functions is a major drawback because the addition operator needs overloading and using reflection, it is generally possible to write functions that can act on different types. For Castagnas work, the bad computability in face of nix' huge syntax trees is the biggest drawback. It has to be noted though, positively, that Castagna already carved out approaches for gradual and occurrence typing, even though that is regarded to his continuous endeavor to type another language, elixir @castagna2023elixir.
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
