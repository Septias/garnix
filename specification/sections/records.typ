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



#let export = [
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

  An overview comparison table can be found in @ts-comp.
]

== Comparison-draft
- Dolan: Lattice of types, Extensional,
- Castagna: Sets, Denotational, Universes, Occurrence, Function-annotations,
- Parreaux: Boolean Algebraic, Syntactic, Verbose, Levels, Bounds, Skolems, Rigid Vars,

The Workhorse in Parreaux: Carefully crafted inference rules, Normal-Forms
The Workhorse in Castagna: Boolean Formulas, Reduced to Normal-Forms

== Regarding Perreaux
- Since overloading is not possible in boolean algebra systems, we can not fully type Nix in them. This is because due to the reflection checks, it is possible to overload functions.
- μ-rule is not possible because recursion might aries during type checking and is not attached to any syntax
- Tagged classes instead of anonymous record types

Unchanged rest: { a: int; | r} -> { a: float; r}
Extension: { .. }

== Parreaux
Singleton record type: { t : τ }

== Questions
- How does record extension work in parreaux?
  - Unchanged rest: { a: int } ∧ { .. } -> { a: int } ∧ ¬{ a : int} ∧ { .. } ~> { .. }

#bib
