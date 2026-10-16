#import "../functions.typ": *

= Our Position <sec-overview>
This section positions MiniNix against existing record calculi and then introduces it informally through examples. The formal definitions follow in @declarative.

== Background <sec-background>
The goals G1-G4 exclude most existing record calculi as a basis for NixLang. Calculi that restrict concatenation to records with disjoint labels @symm_concat @xie2020row, or that have no concatenation and offer only extension by a fresh field or functional update @gaster_jones @ohori1995polymorphic, reject the overwriting uses of `//` and thereby violate totality (G3). Ur @ur expresses overwriting as field removal followed by disjoint concatenation, and a polymorphic function must state the disjointness constraints in its signature, which violates G1 (no source changes). Gradual systems for extensible rows @gradual_siek @gradual_extensible_rows insert casts at the boundary to untyped code; a cast changes evaluation and may force a value that lazy evaluation would leave untouched, which violates G2. Without casts, a gradual system loses the safety theorem and the gradual guarantee that rest on them (@sec-discussion). Width-subtyping, as in algebraic subtyping @algebraic_subtyping @simplesub @mlstruct, does not combine with asymmetric concatenation of records of unknown shape: a field forgotten by subsumption is no longer tracked, yet still overwrites a field of the other operand at run time @symm_concat. A sound rule must therefore forget every field of the left operand that the right one might carry, which under width-subtyping is all of them. Chau and Parreaux @simple_boolean encode field removal in Boolean-algebraic subtyping and support extension by a single, statically known field, but not the concatenation of two records. The remaining systems are expressive enough but delegate the work to a solver. Wand's typings are disjunctions whose alternatives multiply with every concatenation @concat4multiinher, HM(X) leaves the constraint solver as a parameter @hmx, Rose decides row predicates by an entailment relation @rose, and set-theoretic types rest on a subtyping relation whose decision is exponential in the worst case @frisch_semantic @castagna2023programming. None of these is ruled out in principle, but none offers the structural reasons for scaling that G4 asks for.

Scoped labels @extensible_recs avoid the conflict between overwriting and absence altogether. A row may contain a label several times, and lookup resolves duplicates with left-precedence, so extension is total and demands no information about absent fields. Leijen applies this idea to extension by a single field; it remains to lift it to the concatenation of two rows.

=== Paszke and Xie
Paszke and Xie @extensible_tabular combine scoped labels with first-class labels into infix-extensible rows and give a unification-based inference algorithm over row- and label-variables. Their system is the direct basis of ours. Its field selection is handled by a search rule inside unification, and a conditional tail-check rejects programs whose shadowing behaviour is unresolved. The search rule is one of the two points where MiniNix departs from their system: it decides how a selection on a row whose tail is a variable is answered, and @sec-overview gives our answer. The second is the admission of the unknown type ★ so that uncertainty is recorded rather than rejected (G3).

== Two Judgements Instead of One <our-position>
In a row-based system, a field selection is ordinarily _elaborated into a row constraint_ and handed to unification: `e.l` demands that `e`'s row contain $l$, and unification must discharge that demand, which, when the row ends in a variable, means guessing that the variable contains the field. Paszke and Xie's search rule @extensible_tabular does exactly this, and it is the point at which the systems discussed in @related-work either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.

We take selection out of unification altogether. Field demands and row equality are different judgements. Row unification $scripts(≐)_r$ only ever states structural _equality_ of two rows; it never receives a demand of the form "this row must contain $l$", and consequently never has to guess a field into a variable. Field demands are instead answered by a separate lookup relation.

== Scoped Rows and Three-Valued Lookup
With _scoped records_, concatenation of record types becomes juxtaposition of rows. MiniNix uses rows ρ that hold label-type bindings and row-variables. A record type ${ρ}$ is formed from a row ρ. For concatenation, the rows of both operands, {ρ} and {ρ'}, are joined to form {ρ | ρ'}, and no information is lost. We combine this with a lookup relation on rows of the form $ρ.l ↓ r$ (@row-lookup) that traverses the natural structure of rows to look up a label l in the row ρ. The result r can be of the following three kinds:

The first two results are a type τ, if the lookup finds the label, and a marker ⊥, if the label is definitely absent. A row can consist of fixed label-type bindings but also row-variables, and our lookup relation stops at these, returning a result ? because shadowing behaviour after this point is not clear. Substitution then solves those row-variables and lets the lookup advance further into the record. After all substitution is done, a  $?$ that no substitution resolves is typed at the unknown type ★.

== Selection
// this section creates more questions than it solves
The smallest program that selects a field the selector, `x: x.l`. Its argument must be a record ${ρ}$, and the lookup $ρ.l$ decides the result. For $ρ = (l: τ₀)$ the lookup hits and the selector types at ${(l: τ₀)} → τ₀$. For $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$. Here ★ marks a selection that fails whenever it is run, not uncertainty; [the program is still accepted](explain why), and the selection is reported. For a row-variable $ρ = β$ the lookup answers $?$, and the result stays pending until a substitution decides it. Only a $?$ that remains at the end of inference becomes ★.

A `let`-bound selector keeps this pending lookup in its scheme and replays it at every use. This lets a single binding serve incompatible uses,
$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$
The first use needs the result of f to be a record, the second applies f to a record without l. No plain scheme covers both, and @principality shows that this forces qualified schemes.

The selector also shows where MiniNix departs from Paszke and Xie @extensible_tabular. Their search rule turns the selection into the demand that the row of x contain l, and since that row is a variable, unification places the field into it: the selector types at ${(l: δ | ρ)} → δ$. Every use must then supply l, so `f {}` is rejected, and with it the program above. MiniNix answers the lookup with $?$ instead and keeps it in the scheme of f, whose instances include ${ε} → ★$.

== The Wand Ambiguity, Resolved
The Wand ambiguity of @sec-motivation is resolved by the same mechanism. The motivating example concatenates two unknown records and selects from the result:
$ #b[let] f = (a: b: (a ‖ b).l) #b[in] … $
With $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$ — the right operand first, since lookup prefers the left of a row. The lookup $(β | α).l$ stops at β with $?$: whether the field comes from b or from a depends on whether β contains l. A plain type can only record this as ${α} → {β} → ★$. MiniNix instead keeps the pending lookup in the scheme of f,
$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $
and replays it at every use: `f {l = c₁} {}` looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$, `f {l = c₁} {l = c₂}` yields $𝓫_(c₂)$ because the right operand shadows, and `f {} {}` yields ★. Each use receives the most precise type its arguments permit. Paszke and Xie's search rule would instead have to decide whether the row of b contains l, which their tail-check cannot.

// meta: this seems out of place, it might be needed soon, but needs a better introduction then
// == A Stump Wakes Up
// [Inference cannot always answer a lookup when it meets it](wording). In `(x: x.l) {l = c}`, the selector is inferred first: x receives a fresh row ρ, the lookup $ρ.l$ answers $?$, and inference returns a fresh δ for the result while _deferring_ the pending lookup as a stump $⟨ρ ▷ ρ.l ↓ δ⟩$, [blocked on ρ](explain the structure of a stump? A general idea about a stump would be helpful). Applying the selector then solves $ρ ≔ (l: 𝓫_c)$. The solution unblocks the stump, the lookup is re-asked and hits, and δ is unified with $𝓫_c$. For `(x: x.l) {}` the same stump wakes to definite absence, δ becomes ★, and a warning records the place. Row unification itself never sees the field demand; @inference develops this mechanism in full.

== Consequences for Inference
// meta: add references for the opposing strategies
The consequence is that a field demand is never a constraint to be solved. A demand that cannot be answered yet is either answered later, by running the lookup again on a more instantiated row, or recorded as ★. Schemes do carry such pending lookups as qualifiers (@principality), but their entailment is evaluation: a qualifier is discharged by computing a total and deterministic lookup, never by search. Where the row theory would need a disjunction, we write ★. As a result, inference remains ordinary syntactic first-order unification. There is no constraint solver to supply, no entailment relation left as a parameter, no unification modulo AC, and no backtracking over alternative typings. Paszke and Xie's search rule can be dropped entirely, and their conditional tail-check is replaced by an algebraic property of rows rather than a side condition. Every move of the algorithm is forced; where none applies, the configuration is reported as stuck rather than resolved by a guess.

// meta: maybe not that this is due to efficiency reasons?
