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
NixLang requires a calculus that concatenates records with unknown fields, lets the right operand win, and infers types without annotations. To reach all goals, changing evaluation is not allowed and every program needs to be typed without search – to stay performative. This already rules out a lot of existing type systems for NixLang.

_Restricted concatenation._ Record calculi make different tradeoffs between expressiveness and inference. Many calculi operate only on statically known labels @wand_complete @remy_typechecking @remy_natural_ml @gaster_jones @ohori @extensible_recs @simple_boolean @poly_records. This restriction keeps principal types and a simple constraint theory. Other calculi admit the concatenation of unknown records and require disjoint fields @symm_concat @ur @xie2020row @taming_merge. All of these typesystems are not faithful to NixLang' record concatenation semantics. Only Wand @concat4multiinher types the concatenation with a disjunction over the operand that provides the field but returns a finite complete sets of types instead of one principal type.

_Subsumption._ Cardelli and Mitchell @operations_on_records observed that width-subtyping conflicts with asymmetric concatenation: if subsumption forgets a field of the right operand, the type of `(a ‖ b).l` is taken from a, while evaluation selects the forgotten field of b. Systems with width-subtyping therefore need absence information to model asymmetric concatenation. The initial systems of algberaic subtyping @algebraic_subtyping @mlsub @simplesub give principaled inference for structural subtyping but do not include concatenation. MLstruct @mlstruct adds negation, which can express absence but only in their newest work @[cite]() do Chau et. al? show how to remove a field from an arbitrary record with a known label. Concatenation of two arbitrary records is not shown. Set-theoretic types @frisch_semantic @castagna2023programming similarly include the full set of type connectives (∧∨¬), negation included. The first record system @typing_records_etc types concatenation for a know field and the concatenation of polymorphic records stays open even in their latest work @poly_records. Their subtype relation also has a worst-case exponential decision so we instead opt for row equivalence.

_Qualified types._ Sulzmann @designing_record_systems states concatenation as a constraint of HM(X) @hmx, which is parametric in the constraint domain. Rose @rose is the most developed system with constraints on rows. Rose types concatenation with qualified types @qualified_types over predicates for the combination and containment of rows, and it types the motivating example as `∀t z1 z2. (x ◃ t) ≼ (z1 ⊙ z2) ⇒ Πz1 → Πz2 → t`. The scoped row theory of Rose makes combination non-commutative and thereby admits shadowing. Inference produces a principal constrained scheme and defers the predicates to an entailment relation, which is a parameter of the framework. The instantiated entailment rules decide ground predicates only.

_Qualified types._ Sulzmann @designing_record_systems states concatenation as a constraint of HM(X) @hmx, which is parametric in the constraint domain. Rose @rose is the most developed system with constraints on rows. Rose types concatenation with qualified types @qualified_types over predicates for the combination and containment of rows, and it types the motivating example as `∀t z1 z2. (x ◃ t) ≼ (z1 ⊙ z2) ⇒ Πz1 → Πz2 → t`. The scoped row theory of Rose makes combination non-commutative and thereby admits shadowing. Inference produces a principal constrained scheme but defers the predicates to an entailment relation, which is a parameter of the framework. Deciding conjunctions of predicates with row-variables amounts to unification modulo associativity, which is decidable @makanin but has no most general unifiers in general. The successors of Rose move to an explicitly typed System Fω @generic_with_extensible @extensible_data_adhoc, and Morris @another_inference gives inference for this setting with a dedicated predicate solver. These systems represent the Wand ambiguity as a predicate, and the cost of solving such predicates is an open question.

_Scoped labels and unification._ MiniNix builds on the scoped labels of Leijen @extensible_recs: duplicate labels stay in the row, and selection takes the leftmost occurrence. Leijen also introduced first-class labels @fc_labels. Paszke and Xie @extensible_tabular combine both with asymmetric concatenation in a sound calculus with unification-based inference. Their concatenation is plain row concatenation, so it loses no information about fields. Selection needs the field at the head of the row, and a selection past a row-variable is ill-typed. Inference therefore attributes the field to a row-variable on the spot and needs a side condition on shared tails for soundness. MiniNix keeps their rows and replaces the attribution by a three-valued lookup.

MiniNix is closer to Rose than to the classic calculi. Stumps are predicates in the sense of qualified types @qualified_types, and the solver state is a constraint store. The two systems differ in strategy. MiniNix decides a stump only by evaluating its lookup. Row unification makes only forced moves and reports a stuck verdict where a choice would be needed.

_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all. Sekiyama and Igarashi @gradual_extensible_rows instantiate gradual typing for extensible rows with a dynamic row type, the system closest in spirit to our ★-typed rows. The dynamic row type is backed by casts that check at run time, whereas ★ has no runtime counterpart, because casts would change evaluation (G2). To combine theirs and our work with non-forcing cast is an interesting direction of further work.

Our system is a _soft_ typing system in the tradition of Cartwright and Fagan @soft_typing @practical_soft_typing and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory states the residual risk as the ↯-disjunct of progress (@type-safety). We use the term in a narrower sense. Cartwright and Fagan accept every program and insert run-time checks where the analysis fails. Our system inserts no checks and can reject programs, at a clash or at an elimination of ★ (@sec-goals). Both analyses take the untyped semantics as the reference and report what they cannot decide. From industry languages, @typescript @flow are the closest systems with `any` taking the role of our unknown type. `any`, however leaves a soundness-gap in the system.

_Typing NixLang._ Little work addresses NixLang itself. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for NixLang and do not consider a type system. An earlier system by the author @simplenix applies HM inference to a NixLang subset and does not cover the record operations this thesis addresses. Nickel @nickel, a configuration language in the Nix ecosystem, uses gradual typing with row polymorphism. Its record merge is symmetric, and the type checker assigns merge the type `Dyn -> Dyn -> Dyn`.
