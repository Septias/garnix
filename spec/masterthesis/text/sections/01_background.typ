#import "../functions.typ": *

#let meta = [
  - Maybe I can just condense this to one sentence to create the lifting for "we position us here"
  - The "our position" could be the only interesting thing where we *lay out the usage of ★ and asymmetric concat*

]


= Background <sec-background>

// Ur needs disjointness written out at polymorphic functions. I believe this holds for the uses that matter here, but Ur infers some of these constraints.
// Deciding set-theoretic subtyping is exponential in the worst case.
// Gaster-Jones only extends by a fresh field, so it rejects overwriting.
// meta: I want to add some reasoning on why gradual is basically off the table
The goals G1-G4 exclude most existing record calculi as a basis for NixLang, each for a specific reason. Calculi that restrict concatenation to records with disjoint labels @symm_concat @xie2020row, or that offer only extension by a fresh field and functional update @gaster_jones @ohori1995polymorphic, reject the overwriting uses of `//` and thereby violate totality (G3). Calculi that admit overwriting at polymorphic types require the programmer to state disjointness explicitly @ur, which violates G1 (syntax changes). Gradual systems for extensible rows @gradual_siek @gradual_extensible_rows insert casts at the boundary to untyped code; a cast changes evaluation and may force a value that lazy evaluation would leave untouched, which violates G2. Width-subtyping, as in algebraic subtyping @algebraic_subtyping @simplesub @mlstruct, is unsound together with asymmetric concatenation: a field forgotten by subsumption is no longer tracked, yet still overwrites a field of the other operand at run time @symm_concat. @simple_boolean admit record removal but need tagged records for it, a feature we can not add to nix which has anonymous records. The remaining systems are expressive enough but delegate the work to a solver. Wand's typings are disjunctions whose alternatives multiply with every concatenation @concat4multiinher, HM(X) leaves the constraint solver as a parameter @hmx, Rose decides row predicates by an entailment relation @rose, and set-theoretic types rest on a subtyping relation whose decision is exponential in the worst case @frisch_semantic @castagna2023programming. None of these is ruled out in principle, but none offers the structural reasons for scaling that G4 asks for.

Scoped labels @extensible_recs avoid the conflict between overwriting and absence altogether. A row may contain a label several times, and lookup resolves duplicates with left-precedence, so extension is total and demands no information about absent fields. Leijen applies this idea to extension by a single field; it remains to lift it to the concatenation of two rows.

// meta: this is somewhat out of place. Put it in front mayebe?
// meta: Maybe clear up the heritance right in the abstract?
== Paszke and Xie
Paszke and Xie @extensible_tabular combine scoped labels with first-class labels into infix-extensible rows and give a unification-based inference algorithm over row- and label-variables. Their system is the direct basis of ours. Its field selection is handled by a search rule inside unification, and a conditional tail-check rejects programs whose shadowing behaviour is unresolved.


== Our Position: Two Judgements Instead of One <our-position>

Our design rests on a separation between field demands and row equality.

In a row-based system, a field selection is ordinarily _elaborated into a row constraint_ and handed to unification: `e.l` demands that `e`'s row contain $l$, and unification must discharge that demand — which, when the row ends in a variable, means guessing that the variable contains the field. Paszke and Xie's search rule @extensible_tabular does exactly this, and it is the point at which the systems discussed in @related-work either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.

We take selection out of unification altogether. Field demands and row equality are different judgements. Row unification $scripts(≐)_r$ only ever states structural _equality_ of two rows; it never receives a demand of the form "this row must contain $l$", and consequently never has to guess a field into a variable. Field demands are instead answered by a separate _lookup relation_ $ρ.l ↓ r$ whose result is three-valued — a definite type $τ$, definite absence $⊥$, or _don't know_ $?$ — and a demand that cannot be answered yet becomes a _deferred lookup_, asked again once a substitution has solved the row-variable it stopped at.

// meta: add references for the opposing strategies
The consequence is that a field demand is never a constraint to be solved. A demand that cannot be answered yet is either answered later, by running the lookup again on a more instantiated row, or recorded as ★. Schemes do carry such pending lookups as qualifiers (@principality), but their entailment is evaluation: a qualifier is discharged by computing a total and deterministic lookup, never by search. Where the row theory would need a disjunction, we write ★. As a result, inference remains ordinary syntactic first-order unification. There is no constraint solver to supply, no entailment relation left as a parameter, no unification modulo AC, and no backtracking over alternative typings. Paszke and Xie's search rule can be dropped entirely, and their conditional tail-check is replaced by an algebraic property of rows rather than a side condition. Every move of the algorithm is forced; where none applies, the configuration is reported as stuck rather than resolved by a guess.

// meta: maybe not that this is due to efficiency reasons?
