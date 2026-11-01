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
  - @conlat4muiltiinher: No principaled types, no inference, shows Wand-ambiguity
  - @ohori: too restricted
  - @xie2020row:
  - @concat4free: removed
]



= Related Work <related-work>
NixLang needs a calculus that concatenates two records whose fields are unknown, lets the right operand override fields of the left one, and infers types without annotations (G1), for every program (G3) and without search (G4). We compare MiniNix againts existing literature under our own stated goals.


_Single-label operations._ Most row calculi type only operations on one statically known label: extension, restriction and selection. This holds for the classic calculi of Wand @wand_complete, Rémy @remy_typechecking @remy_natural_ml, Gaster and Jones @gaster_jones and Ohori @ohori, for Leijen's scoped labels @extensible_recs, [and for recent work](what recent work). MLstruct+ @simple_boolean extends a record by one field with override, `C{...s, a = t}`, and the polymorphic records of Castagna and Peyrot @poly_records support strict extension only. Concatenation is left out on purpose. Ohori, Castagna and Peyrot excludes it since it significantly complicates both the type theoretical analysis and compilation […](cite).

----



_Disjoint concatenation._ A second group admits the concatenation of unknown records but forbids the collision of fields. Harper and Pierce @symm_concat restrict an explicitly typed second-order calculus to _symmetric_ concatenation, which is defined only on records with disjoint fields and tracked by compatibility constraints. Ur @ur guards its concatenation with disjointness constraints in the same way. In the disjoint-polymorphism line @xie2020row @taming_merge, the merge operator subsumes symmetric concatenation, and disjointness plays the role of compatibility. [The simple row theory of Rose @rose defines combination only on disjoint rows, and its successors @generic_with_extensible @extensible_rec_funcs @extensible_data_adhoc are developed for this theory.](is this correct?) Override is recovered by restricting the left operand first: by one label in Harper and Pierce, and by a whole type with type difference @type_difference. The restriction then has to name the overridden fields. In NixLang, the collision is the purpose of `//`: the override of @nix-idioms redefines buildInputs, which args may already define, and a disjointness constraint rejects the program unless it is rewritten (G1).


_Subsumption._ Width subtyping conflicts with asymmetric concatenation, as Cardelli and Mitchell @operations_on_records observe: if subsumption forgets a field of the right operand, the type of `(a ‖ b).l` is taken from a, while evaluation selects the forgotten field of b. A subtyping-based system can therefore type concatenation only where the absence of fields is part of the type. Algebraic subtyping @algebraic_subtyping @mlsub @simplesub gives principal inference for structural subtyping but offers no concatenation. MLstruct @mlstruct adds negation, which is one way to express absence, but no general concatenation either. Set-theoretic types @frisch_semantic @castagna2023programming express absence exactly. Castagna @typing_records_etc types concatenation by a type operator that takes a field from the right operand where it is surely defined and the union of both fields otherwise. The operator works on record types whose fields are known, the concatenation of polymorphic records is still open @poly_records, and the subtyping relation underneath has a worst-case exponential decision procedure (G4). This conflict steers MiniNix away from subsumption and towards row equivalence.

_Asymmetric concatenation in the type language._ The remaining calculi type the asymmetric concatenation of unknown records and keep the Wand ambiguity in the type. Wand @concat4multiinher splits every concatenation per label over the operand that provides the field. A term then has no principal type but a finite complete set of types, which can grow to $2^(n k)$ for n labels and k concatenations (G4). Rémy @concat4free encodes a record as a function on the row of fields it modifies, so that concatenation becomes composition. The encoding keeps the principal types of ML, but the types describe the encoding, and a record bound by a λ is monomorphic in its input row. Pottier @pottier_versatile solves concatenation with conditional constraints on the presence of fields. Inference is effective, but Leijen @extensible_recs judges the resulting types hard to use in practice. Sulzmann @designing_record_systems states concatenation as a constraint of HM(X) @hmx, which is parametric in the constraint domain and asks every instance for principal normal forms.

Rose @rose is the most developed system of this group. It types concatenation with qualified types @qualified_types over predicates for the combination and containment of rows, and it types the motivating example as `∀t z1 z2. (x ◃ t) ≼ (z1 ⊙ z2) ⇒ Πz1 → Πz2 → t`. Its scoped row theory makes combination non-commutative and thereby admits shadowing. Inference produces a principal constrained scheme and defers the predicates to an entailment relation that the framework leaves as a parameter. The instantiated entailment rules decide ground predicates only. Deciding conjunctions of predicates with row-variables amounts to unification modulo associativity, which is decidable @makanin but has no most general unifiers in general. The successors move to an explicitly typed System Fω @generic_with_extensible @extensible_data_adhoc, for which Morris @another_inference has recently given inference with a dedicated predicate solver. Every system of this group thus keeps the Wand ambiguity as a disjunction, an encoding or a predicate, and pays for it in the size of types, in their readability, or with a solver whose cost is open.

_Scoped labels and unification._ MiniNix descends from Leijen's scoped labels @extensible_recs: duplicate labels stay in the row, and selection takes the leftmost occurrence. Leijen also introduced first-class labels @fc_labels, and Paszke and Xie @extensible_tabular combine both with asymmetric concatenation in a sound calculus with unification-based inference. Their concatenation is plain row concatenation, so no information about fields is lost. Selection, however, needs the field at the head of the row, and a selection past a row-variable is ill-typed. Inference therefore attributes the field to a row-variable on the spot and needs a side condition on shared tails to stay sound. The motivating example is typed as if l came from b, and the uses at which b lacks l are rejected (G3). MiniNix keeps their rows and replaces the attribution by a three-valued lookup: an undecided lookup becomes a stump of the scheme, and a use that leaves it undecided reads it as ★.

MiniNix is closer to Rose than to the classic calculi. Stumps are predicates in the sense of qualified types @qualified_types, and the solver state is a constraint store. The difference lies in the strategy. MiniNix decides a stump only by evaluating its lookup, row unification makes only forced moves and reports a stuck verdict where a choice would be needed, and a use that would need a disjunction types the lookup at ★. Where the systems above pay with the size of types or with an open solver, MiniNix pays with precision.


_Gradual and soft typing._ Gradual typing @gradual_siek @gradual_criteria inserts runtime casts at the boundary between typed and untyped code, with blame tracking @cantblamethis @blame_for_all, and has been instantiated for extensible rows by Sekiyama and Igarashi @gradual_extensible_rows, the system closest in spirit to our ★-typed rows. (todo)[how do we differ?]

// Should we note soft-typing here?
Our system is a _soft_ typing system in the tradition of Cartwright and Fagan @soft_typing @practical_soft_typing and the "static where possible, dynamic when needed" school @coldwar: every program keeps its untyped semantics, ★ marks the places the analysis gave up, and the metatheory states the residual risk as the ↯-disjunct of progress (@type-safety). [The term is used in a narrower sense than theirs.](how?) Cartwright and Fagan accept every program and insert run-time checks where the analysis fails, whereas our system inserts no checks and can reject programs, at a clash or at an elimination of ★ (@sec-goals). What the two share is that the analysis never changes the program and reports what it cannot decide instead of forbidding it.

Real-world type systems such as TypeScript @typescript and Flow @flow have an unsound `any`, formalized for TypeScript by Bierman, Abadi and Torgersen @understanding_typescript. A value of type `any` may be used at every type, but ★ differs in two respects: It has no elimination form, so progress up to lookup errors holds (@type-safety), and it is only produced by inference, while `any` can be written by the programmer.


_Typing NixLang._ Work on NixLang itself is scarce. Broekhoff and Krebbers @verified give a verified interpreter and an operational semantics for NixLang, but do not attempt a type system. An earlier system by the author @simplenix applies off-the-shelf HM inference to a NixLang subset and fails exactly on the record operations this thesis addresses. Nickel @nickel, a configuration language that compiles to NixLang, opts for gradual typing with row polymorphism. Their record concatenation is symmetric and the type checker assigns merge the uninformative type `Dyn -> Dyn -> Dyn`.
