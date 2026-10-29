#import "../functions.typ": *

= MiniNix <sec-overview>

We motivate the design decisions of MiniNix by comparing it against existing record calculi with regard to the requirements G1-G4. We then introduce MiniNix by examples and @declarative gives the formal treatment.

== Background <sec-background>

Many existing record calculi are ruled out by the design goals G1-G4. Calculi with concatenation restricted to disjoint labels @symm_concat @xie2020row cannot track the overwriting behaviour of Nix's concatenation operation. Calculi with record extension @wand_complete @remy_typechecking can overwrite a single, statically known label, but cannot concatenate two records whose fields are unknown. Gradual systems for extensible rows @gradual_extensible_rows insert casts at the boundary to untyped code. Even lazy casts instrument the program, which G2 rules out. Width-subtyping, a common technique in algebraic and semantic subtyping @algebraic_subtyping @simplesub @mlstruct @frisch_semantic, allows forgetting fields in records. Without absence information, it cannot be combined with asymmetric record concatenation: a field removed by typing persists in the semantics and may shadow another field at runtime.

// proper wording for rose (row theory?)
Chau and Parreaux @simple_boolean allow extension by a single, statically known field but disallow the concatenation of two arbitrary records. Other type systems like HM(X) @hmx and Rose defer the tricky decision of field attribution and leave it unanswered. HM(X) has principal and first-order type inference, but the constraint solver is a parameter. Rose @rose decides row predicates during unification but leaves the underlying row theory unspecified. New work by Morris @another_inference describes approaches for type inference but does not give an efficient, applicable algorithm. Set-theoretic types @frisch_semantic @castagna2023programming rest on a subtyping relation with a worst-case exponential decision procedure. None of these systems meet all our goals at once.

Paszke and Xie @extensible_tabular combine scoped and first-class labels in a sound calculus and give a unification-based inference algorithm for it. We base our work on theirs and use the same ingredients: Records {ρ} are formed from rows ρ, and record concatenation is merely the concatenation of them: {ρ | ρ'}. We also admit an equivalence relation but depart from their work in two ways: We admit an unknown type ★ to form a total system (G3) and eliminate the tail-rule of their calculus. Their rule Rfield needs a side condition on the shared tail to reject the unification of $(l: 𝓫 | α)$ with $(m: 𝓫 | α)$, while our row unification cancels the tail (@ex-unif-cancel).

A field selection e.l in ordinary type systems is elaborated into a field constraint and given to unification. Unification then solves the row constraint by assigning the label to the row variable, deciding its origin on the spot. This is the point where previously discussed type systems either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.

We take another route and delay the assignment as long as possible by recording field demands on type schemes. This way, row unification and field demands are different judgements and unification never has to guess a label into a row.

== Scoped Labels and Three-Valued Lookup
The concatenation of two records $({r} ‖ {r'})$ described by rows $ρ$ and $ρ'$ results in a simple compound type: ${ρ' | ρ }$. Notice that the order of the row variables is reversed and no information is lost during this process. We combine it with a lookup relation of the form $ρ.l ↓ r$ that traverses the natural structure of a row, looking recursively for a label. The result r can be of the following three kinds:

If the lookup is successful, a definite type τ is returned and if the label is definitely absent the result is ⊥. We add a third verdict:  When a row variable α is encountered, shadowing behaviour from this point on is unclear because α might contain the label l or not. We return a $?$, park the lookup and continue. When substitution solves row variables, lookup can advance further into the record and resolve it. The following section shows how our approach translates to ordinary selection and the wand-ambiguity.


== Selection
The smallest program with an unresolved type variable is `x: x.l`. The variable x has to answer a lookup of label l in the body so it has to be of type ${ρ}$. For a definite row $ρ = (l: τ)$, the lookup hits and the function types at ${ l: τ } → τ$. For $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$. A definite miss is still not a definite error. Record fields are evaluated only when they are selected, so the program below evaluates to a record although its field b fails. Whether b is ever selected is decided by the consumers of the record, which may sit in another file. Rejecting the program would therefore reject working code (G3). ★ keeps the miss harmless: it has no eliminators, so b can be passed on, but every use of b at a concrete type is rejected.

A let binding records such a pending lookup in the scheme. Using this mechanism, two different uses can be typed:

$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$

The first use forces f to be a record, and the second one applies f to a record without l. No plain scheme covers both these types, and @principality shows that this forces qualified schemes. // This also shows the difference to our approach: The search rule of Paszke and Xie forces a label decision, and unification has to attribute it to the row variable. In their type system, the selector types at ${(l: δ | ρ)} → δ$.


== The Wand Ambiguity
The Wand ambiguity of @sec-motivation concatenates two unknown records and selects from the result. It is handled by the same mechanism:

$ #b[let] f = (a: b: (a ‖ b).l) #b[in] … $

Given $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$ and lookup stops on the row variable $β$ with an unknown result $?$. Again, whether the field comes from b or from a depends on whether β contains l. A plain type can only record this as ${α} → {β} → ★$ but MiniNix records it with the following type scheme:

$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $

The row demand is kept in the scheme and replayed at every use: f {l = c₁} {} looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$ whereas f {l = c₁} {l = c₂} yields $𝓫_(c₂)$ because the right operand shadows, and f {} {} yields ★.

The consequence of our inference approach is that field demands do not have to be resolved by unification. A demand that cannot be answered yet is deferred and either answered later when a lookup on a record with more instantiated type variables reaches further into the record, or kept as a qualifier and read as ★ at a use that leaves it unanswered. Where other row theories would need a disjunction, we write ★ and as a result inference stays first-order and never backtracks: every move is forced.
