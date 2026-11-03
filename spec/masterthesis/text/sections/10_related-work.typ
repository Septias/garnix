#import "../functions.typ": *

#let meta = [

  - Don't talk about work that is only slightly related
  - Maybe classify old systems?

  == What do I want to show actually
  - A small overview of old systems
  - A section about P&X
  - The ROSE lineage
  - Gradual systems

  == Things
  - @symm_concat: Not expressive enough
  - @conlat4muiltiinher: No principled types, no inference, shows Wand-ambiguity
  - @ohori: too restricted
  - @xie2020row:
  - @concat4free: removed
]



= Related Work <related-work>
NixLang requires a calculus that concatenates records with unknown fields, lets the right operand override fields of the left one, and infers types without annotations, without changing evaluation, for every program and without search. This already rules out a lot of existing type systems.

_Restricted concatenation._ Record calculi make different tradeoffs between expressiveness and inference. Many calculi operate only on statically known labels @wand_complete @remy_typechecking @remy_natural_ml @gaster_jones @ohori @extensible_recs @simple_boolean @poly_records. This restriction keeps principal types and a simple constraint theory. Other calculi admit the concatenation of unknown records and require disjoint fields @symm_concat @ur @xie2020row @taming_merge. In NixLang, the collision of fields is the purpose of `//`, so MiniNix needs asymmetric concatenation. Wand @concat4multiinher types this concatenation with a disjunction over the operand that provides the field. The resulting calculus has finite complete sets of types instead of principal types.

_Subsumption._ Width subtyping conflicts with asymmetric concatenation, as Cardelli and Mitchell @operations_on_records observe: if subsumption forgets a field of the right operand, the type of `(a ‖ b).l` is taken from a, while evaluation selects the forgotten field of b. A subtyping-based system can therefore type concatenation only where the type records the absence of fields. Algebraic subtyping @algebraic_subtyping @mlsub @simplesub gives principal inference for structural subtyping and does not include concatenation. MLstruct @mlstruct adds negation, which can express absence, and likewise has no general concatenation. Set-theoretic types @frisch_semantic @castagna2023programming express absence exactly. Castagna @typing_records_etc types concatenation by a type operator: the operator takes a field from the right operand where the field is surely defined, and the union of both fields otherwise. This operator works on record types with known fields. The concatenation of polymorphic records remains open @poly_records, and the underlying subtyping relation has a worst-case exponential decision procedure. MiniNix therefore uses row equivalence instead of subsumption.

_Qualified types._ Sulzmann @designing_record_systems states concatenation as a constraint of HM(X) @hmx, which is parametric in the constraint domain. Rose @rose is the most developed system with constraints on rows. Rose types concatenation with qualified types @qualified_types over predicates for the combination and containment of rows, and it types the motivating example as `∀t z1 z2. (x ◃ t) ≼ (z1 ⊙ z2) ⇒ Πz1 → Πz2 → t`. The scoped row theory of Rose makes combination non-commutative and thereby admits shadowing. Inference produces a principal constrained scheme and defers the predicates to an entailment relation, which is a parameter of the framework. The instantiated entailment rules decide ground predicates only. Deciding conjunctions of predicates with row-variables amounts to unification modulo associativity, which is decidable @makanin but has no most general unifiers in general. The successors of Rose move to an explicitly typed System Fω @generic_with_extensible @extensible_data_adhoc, and Morris @another_inference gives inference for this setting with a dedicated predicate solver. These systems represent the Wand ambiguity as a predicate, and the cost of solving such predicates is an open question.

_Scoped labels and unification._ MiniNix builds on the scoped labels of Leijen @extensible_recs: duplicate labels stay in the row, and selection takes the leftmost occurrence. Leijen also introduced first-class labels @fc_labels. Paszke and Xie @extensible_tabular combine both with asymmetric concatenation in a sound calculus with unification-based inference. Their concatenation is plain row concatenation, so it loses no information about fields. Selection needs the field at the head of the row, and a selection past a row-variable is ill-typed. Inference therefore attributes the field to a row-variable on the spot and needs a side condition on shared tails for soundness. MiniNix keeps their rows and replaces the attribution by a three-valued lookup.

MiniNix is closer to Rose than to the classic calculi. Stumps are predicates in the sense of qualified types @qualified_types, and the solver state is a constraint store. The two systems differ in strategy. MiniNix decides a stump only by evaluating its lookup. Row unification makes only forced moves and reports a stuck verdict where a choice would be needed. A use that would need a disjunction types the lookup at ★. MiniNix thus gives up precision in exchange for a solver without search.


_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all. Sekiyama and Igarashi @gradual_extensible_rows instantiate gradual typing for extensible rows with a dynamic row type, the system closest in spirit to our ★-typed rows. The dynamic row type is backed by casts that check at run time, whereas ★ has no runtime counterpart, because casts would change evaluation (G2). To combine theirs and our work with non-forcing cast in the fashion of @[thiemann](cite) is an interesting direction of further work.

Our system is a _soft_ typing system in the tradition of Cartwright and Fagan @soft_typing @practical_soft_typing and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory states the residual risk as the ↯-disjunct of progress (@type-safety). We use the term in a narrower sense. Cartwright and Fagan accept every program and insert run-time checks where the analysis fails. Our system inserts no checks and can reject programs, at a clash or at an elimination of ★ (@sec-goals). Both analyses take the untyped semantics as the reference and report what they cannot decide.

_Typing NixLang._ Little work addresses NixLang itself. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for NixLang and do not consider a type system. An earlier system by the author @simplenix applies HM inference to a NixLang subset and does not cover the record operations this thesis addresses. Nickel @nickel, a configuration language in the Nix ecosystem, uses gradual typing with row polymorphism. Its record merge is symmetric, and the type checker assigns merge the type `Dyn -> Dyn -> Dyn`.
