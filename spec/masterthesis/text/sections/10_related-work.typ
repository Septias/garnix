#import "../functions.typ": *

#let meta = [
  == What do I want to show actually
  - A small overview of old systems
  - A section about P&X
  - The ROSE lineage
  - Gradual systems

  == Things
  - @symm_concat: Not expressive enough
  - @conlat4muiltiinher: No principaled types, no inference, shows Wand-ambiguity
  - @ohori: too restricted
  - @xie2020row:
]



= Related Work <related-work>
_Record concatenation in classic record calculi._ Typing record concatenation is an old and notoriously hard problem. Wand @concat4multiinher studied type inference for concatenation in the context of multiple inheritance, where the set-or-replace semantics of asymmetric concatenation already surfaces: his system case-splits over which side a field comes from, and a term has no principal type but only a finite complete set of types. Harper and Pierce @symm_concat sidestep shadowing by restricting an explicitly typed second-order calculus to _symmetric_ concatenation, which is only defined on records with disjoint fields, tracked by compatibility constraints.


Cardelli and Mitchell @operations_on_records choose extension and restriction as primitives because concatenation is in fundamental conflict with subsumption: if subsumption forgets a field of the right operand, the type of `(l ‖ r).a` is taken from `l`, while evaluation selects the forgotten field of `r`. This conflict steers MiniNix away from subsumption and towards row equivalence. Rémy @concat4free shows that concatenation can be simulated "for free" in a language with polymorphic record extension: a record is translated into a record abstraction over its input row, and concatenation becomes function composition. Every typeable term has a principal type, but the types are those of the encoding, and λ-bound records lose their polymorphism, _so that programs need explicit retyping functions_.


Ohori @ohori obtains efficient compilation for a polymorphic record calculus, but restricts records to selection and functional update. Extension and concatenation are excluded, since their inclusion "significantly complicates both the type theoretical analysis and compilation". In the disjoint-polymorphism line @xie2020row the merge operator subsumes symmetric concatenation, with disjointness playing the role of the compatibility constraints of Harper and Pierce. All of these systems either forbid the colliding case which is forced by NixLang, or pay for it: Wand types the motivating example `a: b: (a ‖ b).l` only with a set of alternatives, and Rémy only through the encoding.

// PROF: "can get stuck" holds for MiniNix too, so it is no differentiator. Say "accepts at star where they reject", not more precision.
_Scoped rows and first-class labels._ Our row theory descends from Leijen's extensible records with scoped labels @extensible_recs, where duplicate labels are kept in the row and selection takes the leftmost occurrence. Leijen's calculus has no concatenation, but this "bag" semantics is what makes asymmetric concatenation a total operation instead of a partially defined one. Leijen later added first-class labels @fc_labels, which NixLang needs for its dynamic field selection `e.${e'}`. Paszke and Xie @extensible_tabular combine both with infix concatenation of rows and a unification-based inference algorithm over row and label variables; their system is the direct basis of ours. Their unification is sound, but it can get stuck where no unique most general unifier exists. Set-or-replace remains out of reach: selection cannot look past a row variable in non-tail position, so `e.l` with `e : {α | l: Bool}` is ill-typed because the shadowing behaviour is unresolved. Our lookup relation instead accepts [such programs at ★ and refines them later](wrong).


// PROF: "strictly more information" is a formal claim; hedge it or prove it.
_Expressive row theories._ Gaster and Jones @gaster_jones type extension in the qualified-types framework with lacks predicates `r \ l`, but leave concatenation open. The line of work started by Morris and McKinna @rose abstracts rows behind an algebra of containment and combination constraints. Its simple row theory requires disjoint operands; asymmetric concatenation is typed by a scoped row theory, which keeps duplicate labels as in Leijen's system. The framework has been extended to generic programming @generic_with_extensible, extensible recursive functions @extensible_rec_funcs and ad-hoc polymorphism @extensible_data_adhoc. These systems track strictly more information than ours.

Rose is a Hindley-Milner language with principal types and decidable type inference, and it types the motivating example as `∀t z1 z2. (x ◃ t) ≼ (z1 ⊙ z2) ⇒ Πz1 → Πz2 → t`. The principality is that of qualified types @qualified_types: inference produces a principal _constrained_ scheme and defers the row predicates to an entailment relation that the framework leaves as a parameter, required only to be invariant under row equivalence, monotone and transitive.

// PROF: MiniNix also unifies modulo associativity (+ partial C); the contrast is the strategy, not the theory. A-unification (Makanin) is more relevant than AC.
The instantiated entailment rules decide only ground predicates and discharge everything else by assumption lookup, so inference is decidable because it does not solve the predicates with row variables that it generates. No procedure and no complexity bound is given for their satisfiability. Since combination is an equation in a partial monoid, deciding conjunctions of such predicates amounts to unification modulo associativity, and modulo commutativity for the simple theory, rather than syntactic unification; AC-unification is NP-complete @ac_unification.

The successor systems move to System Fω and are explicitly typed @generic_with_extensible, with type inference left as future work @extensible_data_adhoc; unrestricted second-order reconstruction is in any case undecidable @undecidable. Sulzmann @designing_record_systems designs record systems in the HM(X) framework, where concatenation becomes a constraint; HM(X) is parametric in the constraint domain and requires each instance to supply principal normal forms. Our position is dual: we keep plain unification-based [inference and instead weaken the types themselves with ★ where the row theory would need a disjunction](we do so, but is that a good thing?).


// PROF: Em dash.
_Subtyping-based systems._ Algebraic subtyping @algebraic_subtyping @mlsub and its simplifications @simplesub give principal inference for structural subtyping, and MLstruct @mlstruct extends this to a Boolean algebra of types with unions, intersections and negations — _negation being one way to express the absence information that shadowing destroys_. None of these systems offers general concatenation of unknown records. The recent extension MLstruct+ @simple_boolean adds record extension with field override, `C{...s, a = t}`, where the extended record carries an explicit tag `C`; plain records carry a default tag. By the argument of Cardelli and Mitchell above, adding `‖` to a subtyping-based system is problematic at the core: a record can always forget the very fields that decide precedence. We deliberately keep our system subtyping-free; the only ordering is the [precision gained by instantiation](true? Note row equivalence here).


_Set-theoretic types and dynamic languages._ Castagna's programme of semantic subtyping @frisch_semantic @castagna2023programming types dynamic languages with unions, intersections and negations, including detailed accounts of records, maps and structs @typing_records_etc and polymorphic records for Elixir @poly_records, together with occurrence typing to refine types along control flow @revisiting_occurrence @on_occurrence and a gradual guard-based system deployed for Elixir @gradual_elixir. This is the most expressive treatment of records for a dynamic language to date, and occurrence typing is a natural future extension of MiniNix (@sec-extensions). The cost is the full set-theoretic machinery and local type inference: functions are explicitly annotated, and types are not reconstructed. Concatenation is typed for records without row variables @typing_records_etc, while the polymorphic system @poly_records supports only strict extension and leaves the concatenation of polymorphic records open. Our ★ plays a role similar to their `dynamic()` @elixir_design_principles, which annotations introduce and which unannotated parameters receive by default; ★ instead arises from the _lookup relation_ when a field cannot be resolved.


_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all, and has been instantiated for extensible rows by Sekiyama and Igarashi @gradual_extensible_rows, the system closest in spirit to our ★-typed rows. We differ in a fundamental way: [NixLang programs cannot be instrumented, so there are no casts, no blame, and no runtime monitoring](check!).

// PROF: Narrower sense than Cartwright-Fagan; define it in the intro or drop "soft" from the title.
Our system is instead a _soft_ typing system in the tradition of Cartwright and Fagan @soft_typing @practical_soft_typing and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory states the residual risk as the ↯-disjunct of progress (@type-safety). The term is used in a narrower sense than theirs.

Cartwright and Fagan accept every program and insert run-time checks where the analysis fails, whereas our system inserts no checks and can reject programs, at a clash or at an elimination of ★ (@sec-goals). What the two share is that the analysis never changes the program and reports what it cannot decide instead of forbidding it.

// PROF: TypeScript has a formal account (Bierman-Abadi-Torgersen, ECOOP 2014). Star also arises from unrestricted T-star-intro.
Industrial gradual systems such as TypeScript @typescript and Flow @flow make the same pragmatic choice of an unsound `any`, but without a formal account of when `any` arises; in our system ★ is introduced only by the lookup relation and T-★-intro, and its origin is therefore always explainable.


_Typing NixLang._ Work on NixLang itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for NixLang, but do not attempt a type system. An earlier system by the author @simplenix applies off-the-shelf HM inference to a NixLang subset and fails exactly on the record operations this thesis addresses. A community issue @nix-ts-issue, open from 2012 until its author closed it in 2018 as not realistically going to happen, documents both the demand for and the difficulty of typing NixLang; Nickel @nickel, a configuration language inspired by NixLang, opts for gradual typing with row polymorphism. Its only record combination is a symmetric merge: colliding fields are merged recursively and fail at runtime if they cannot be combined, unless priorities decide, and the type checker assigns merge the type [`Dyn -> Dyn -> Dyn`](verify).
