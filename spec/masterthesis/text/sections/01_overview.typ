#import "../functions.typ": *

= MiniNix <sec-overview>

We motivate the design decisions of MiniNix by comparing it against existing record calculi with regard to the requirements G1-G4. We then give an overview by examples and @declarative treats it formally.

== Background <sec-background>

// todo: verify
// PROF: Remy (Pre/Abs flags) and Wand 1987 support overwriting extension. Width subtyping clashes only without absence info (cf. MLstruct negation). Casts need not force (lazy contracts).
Many existing record calculi are ruled out by the design goals G1-G4. Calculi with concatenation restricted to disjoint labels @symm_concat @xie2020row, or calculi restricted to extension with a single fresh label @wand_complete @remy_typechecking cannot track the overwriting behaviour of Nix's concatenation operation. Gradual systems for extensible rows @gradual_extensible_rows insert casts at the boundary to untyped code which may force values that lazy evaluation would leave untouched otherwise, violating G2. Width-subtyping, a common technique in algebraic and semantic subtyping @algebraic_subtyping @simplesub @mlstruct @frisch_semantic, allows forgetting fields in records and cannot be combined with asymmetric record concatenation. A field removed by typing persists in the semantics and may shadow another field at runtime.

// PROF: Inconsistent with related work, which describes extension with field override. HM is also worst-case exponential. "none of them all our goals": MiniNix doesn't meet them all either (sec-goals).
Chau and Parreaux @simple_boolean allow field removal by a single, statically known field but disallow the concatenation of two arbitrary records. Other type systems like HM(X) @hmx and Rose defer the tricky decision of field attribution and leave it unanswered. HM(X) has principal and first-order type inference, but the constraint solver is a parameter. Rose @rose decides row predicates during unification but leaves the underlying row theory unspecified. New work by the same authors @another_inference describes approaches for type inference but does not give an efficient, applicable algorithm. Set-theoretic types @frisch_semantic @castagna2023programming rest on a subtyping relation with a worst-case exponential decision procedure. All of these systems fulfill some but none of them all our goals.

// PROF: State the P&X tail rule precisely here; contribution 1 rests on it.
Paszke and Xie @extensible_tabular combine scoped and first-class labels in a sound calculus and give a unification-based inference algorithm for it. We base our work on theirs and use the same ingredients: Records {ρ} are formed from rows ρ, and record concatenation is merely the concatenation of them: {ρ | ρ'}. We also admit an equivalence relation but depart from their work in two ways: We admit an unknown type ★ to form a total system (G3) and eliminate the tail-rule of their calculus.

== Our Position <our-position>

A field selection e.l in ordinary type systems is elaborated into a field constraint and given to unification. Unification then solves the row constraint by assigning the label to the row variable, deciding its origin on the spot. This is the point where previously discussed type systems either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.

We take another route and delay the assignment as long as possible by recording field demands on type schemes. This way, row unification and field demands are different judgements and unification never has to guess a label into a row. At the end of unification, all unresolved lookups are typed with the unknown type ★.

== Scoped Rows and Three-Valued Lookup
The concatenation of two records $({r} ‖ {r'})$ described by rows $ρ$ and $ρ'$ results in a simple compound type: ${ρ' | ρ }$. Notice that the order of the row variables is reversed and no information is lost during this process. We combine it with a lookup relation of the form $ρ.l ↓ r$ that traverses the natural structure of a row, looking recursively for a label. The result r can be of the following three kinds:

If the lookup is successful, a definite type τ is returned and if the label is definitely absent the result is ⊥. We add a third verdict:  When a row variable α is encountered, shadowing behaviour from this point on is unclear because α might contain the label l or not. No further positive information about α can resolve this situation, so we park the lookup and continue. When substitution solves row variables, lookup can advance further into the record and resolve it. But substitution might not resolve every type variable, so every $?$ that was not resolved this way is typed as the unknown type ★ in the final pass.

== Selection
// PROF: Here star means certain failure; elsewhere it means "cannot decide". Two meanings for one type; consider a bottom type for definite absence.
The smallest program with an unresolved type variable is `x: x.l`. The variable x has to answer a lookup of label l in the body so it has to be of type {ρ}. For a definite row $ρ = (l: τ)$, the lookup hits and the function types at ${ l: τ } → τ$. For $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$. Here ★ marks a selection that fails whenever it is run, not uncertainty. We still accept this program because it might be in a lazy position and never be evaluated.

// PROF: The example differs from cor-strict (f {l = c}); use one. For x: x.l the P&X type {l: d | r} -> d is principal; the advantage only appears with concatenation.
A let binding records such a pending lookup in the scheme. Using this mechanism, two different uses can be typed:

$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$

The first use forces f to be a record, and the second one applies f to a record without l. No plain scheme covers both these types, and @principality shows that this forces qualified schemes. This also shows the difference to our approach: The search rule of Paszke and Xie forces a label decision, and unification has to attribute it to the row variable. In their type system, the selector types at ${(l: δ | ρ)} → δ$.


== The Wand Ambiguity
The Wand ambiguity of @sec-motivation that concatenates two unknown records and selects from it is handled by the same mechanism:

$ #b[let] f = (a: b: (a ‖ b).l) #b[in] … $

Given $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$. β is placed first since lookup prefers the fields of b, and lookup then stops on this type variable with an unknown result ?. Again, whether the field comes from b or from a depends on whether β contains l. A plain type can only record this as ${α} → {β} → ★$ but MiniNix records it with the following type scheme:

$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $

The row demand is kept in the scheme and replayed at every use: f {l = c₁} {} looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$ whereas f {l = c₁} {l = c₂} yields $𝓫_(c₂)$ because the right operand shadows, and f {} {} yields ★. Remember, we allow definitely missing lookup because in lazy positions, they might never be evaluated.

== Consequences for Inference

// PROF: Rows modulo ~ are A + partial-C unification (word equations; Shift has no finite complete set). Stumps + wake-up are a suspended-constraint solver. Reword "syntactic" and "no constraint solver".
The consequence of our inference approach is that field demands do not have to be resolved by unification. A demand that cannot be answered yet is deferred and either answered later when a lookup on a record with more instantiated type variables reaches further into the record or recorded as ★ otherwise. Where other row theories would need a disjunction, we write ★ and as a result inference remains syntactic first-order unification. No constraint solver needs to run, no entailment relation is left as a parameter, and no unification modulo AC @ac_unification is needed, as well as no backtracking. The tail condition of Paszke and Xie is dropped and replaced by an algebraic property over rows.
