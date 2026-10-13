#import "../functions.typ": *

#let meta = [
  - Maybe I can just condense this to one sentence to create the lifting for "we position us here"
  - The "our position" could be the only interesting thing where we *lay out the usage of ★ and asymmetric concat*

]


= Background <sec-background>

Our goals G1-G4 rule out a few systems by design (…)

// meta: too many small headings
// If I want to add leijen, find a better introduction
// Anyways, this is not really interesting, readers of this section know the state of the art
// maybe motivate again by asymmetric concat?
// Name all record calculi with symmetric concat in a list?
// Maybe just remove this whole section as it is part of related work?

// In Hindley-Milner inference with rows @concat4multiinher @remy_typechecking, a record type ${ρ}$ is built from a row ρ of label-type pairs that may end in a row-variable. A function that selects l from its argument is [polymorphic in the rest of the row](interesting), and unification solves row-variables as it solves type variables. Most such calculi keep labels unique within a row, so that extension must demand the absence of the label it adds.

// == Scoped Labels
// Leijen's scoped labels @extensible_recs drop uniqueness. A row may contain a label several times, and lookup resolves duplicates with left-precedence. Extension is then total, and a shadowed field is kept rather than removed. [This is the row theory of MiniNix, extended from extension by a single field to the concatenation of two rows](this is actually inspired by @extensible_tabular).

// == Why Not Symmetric Concatenation, Subtyping or Lacks-Predicates
// The [ubiquity](wording) of overwriting rules out record calculi that restrict themselves to _symmetric concatenation_ @symm_concat, in which both operands have disjoint labels. It also rules out another feature commonly seen in record calculi. Using _width-subtyping_, one can remove or forget fields ${l: τ} <= {}$ in a record. This, in combination with asymmetric record concatenation, [leads to unsoundness, because previously forgotten and then untracked fields overwrite existing fields semantically but cannot be tracked by the record system](good). Lacks-predicates @gaster_jones @qualified_types can salvage this situation by denoting the absence of a field. They are absent from MiniNix not because they would burden the programmer — in a system without annotations, nobody writes them — [but because nothing in MiniNix generates them](recheck, nix has constructs that generate them). [Gaster and Jones need them because record extension is partial and its typing rule must demand absence. With scoped rows, concatenation is total, no rule demands anything, and a constraint form without an introduction site buys nothing](is this really an argument?). [The NixLang constructs that would generate them — closed function patterns, `removeAttrs` and `?`-guards under `if` — lie outside MiniNix, which is why @sec-extensions returns to them as the most promising extension](reduce this somehow, either drop entirely or…).
// We therefore keep MiniNix subtyping-free; the only ordering it admits is the precision gained by instantiation and row equivalence (≈).

// meta: this is somewhat out of place. Put it in front mayebe?
// meta: Maybe clear up the heritance right in the abstract?
== Paszke and Xie
Paszke and Xie @extensible_tabular combine scoped labels with first-class labels into infix-extensible rows and give a unification-based inference algorithm over row- and label-variables. Their system is the direct basis of ours. Its field selection is handled by a search rule inside unification, and a conditional tail-check rejects programs whose shadowing behaviour is unresolved.

// meta: second (quite verbally) use of the selector example
The difference shows already on the selector `x: x.l`. The search rule turns the selection into the demand that the row of x contain l, and since that row is a variable, unification places the field into it: the selector types at ${(l: δ | ρ)} → δ$. Every use must then supply l, so `(x: x.l) {}` is rejected, and so is the program of @principality that applies one selector to a record with l and to one without. MiniNix answers the lookup with $?$ instead, keeps it as a stump, and types the selector with the qualified scheme selQ, whose instances include ${ε} → ★$. On the motivating example `a: b: (a ‖ b).l` the search must decide whether the row of b contains l, which the tail-check cannot, while MiniNix again keeps the lookup as a stump (@sec-overview).


== Our Position: Two Judgements Instead of One <our-position>

Our design rests on a separation between field demands and row equality.

In a row-based system, a field selection is ordinarily _elaborated into a row constraint_ and handed to unification: `e.l` demands that `e`'s row contain $l$, and unification must discharge that demand — which, when the row ends in a variable, means guessing that the variable contains the field. Paszke and Xie's search rule @extensible_tabular does exactly this, and it is the point at which the systems discussed in @related-work either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.

We take selection out of unification altogether. Field demands and row equality are different judgements. Row unification $scripts(≐)_r$ only ever states structural _equality_ of two rows; it never receives a demand of the form "this row must contain $l$", and consequently never has to guess a field into a variable. Field demands are instead answered by a separate _lookup relation_ $ρ.l ↓ r$ whose result is three-valued — a definite type $τ$, definite absence $⊥$, or _don't know_ $?$ — and a demand that cannot be answered yet becomes a _deferred lookup_, asked again once a substitution has solved the row-variable it stopped at.

// meta: add references for the opposing strategies
The consequence is that a field demand is never a constraint to be solved. A demand that cannot be answered yet is either answered later, by running the lookup again on a more instantiated row, or recorded as ★. Schemes do carry such pending lookups as qualifiers (@principality), but their entailment is evaluation: a qualifier is discharged by computing a total and deterministic lookup, never by search. Where the row theory would need a disjunction, we write ★. As a result, inference remains ordinary syntactic first-order unification. There is no constraint solver to supply, no entailment relation left as a parameter, no unification modulo AC, and no backtracking over alternative typings. Paszke and Xie's search rule can be dropped entirely, and their conditional tail-check is replaced by an algebraic property of rows rather than a side condition. Every move of the algorithm is forced; where none applies, the configuration is reported as stuck rather than resolved by a guess.

// meta: maybe not that this is due to efficiency reasons?
