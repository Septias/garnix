#import "../functions.typ": *

= MiniNix <sec-overview>

This section motivates the design decisions behind MiniNix by comparing it against existing record calculi with regard to our requirements G1-G4. We then give an overview by examples.

== Background <sec-background>

Many existing record calculi are ruled out by our design Goals G1-G4. For example, calculi with concatenation restricted to disjoint labels @symm_concat @xie2020row, or calculi that only offer extension with a fresh variable. @gaster_jones and @ohori1995polymorphic reject the overwriting uses of `//` and violate totality (G3). [In Ur @ur, overwriting is expressed as field removal followed by disjoint concatenation, and a polymorphic function must state the disjointness constraint in its signature, which violates G1 (no source changes)](verify this).

Gradual systems for extensible rows @gradual_siek @gradual_extensible_rows insert casts at the boundary to untyped code. This may force values that lazy evaluation would leave untouched otherwise and violate G2. Width-subtyping, a common technique in algebraic and semantic subtyping @algebraic_subtyping @simplesub @mlstruct, allows forgetting fields in records and can not be combined with asymmetric record concatenation. This is because a field removed by typing persists in the semantics and may shadow another field at runtime.

Chau and Parreaux @simple_boolean allow field removal by a single, statically known field to MLStruct but disallow the concatenation of two arbitrary records. Other type systems like HM(X) and Rose defer the tricky decision of field attribution and leave it unanswered. HM(X) has principal and first-order type inference, but the constraint solver is a parameter. Rose @rose decides row predicates during unification but leaves the underlying row theory unspecified. New work by the same authors @[cite] describes approaches for type inference but does not give an efficient, applicable algorithm. Set-theoretic types @frisch_semantic @castagna2023programming rest on a subtyping relation with a worst-case exponential decision procedure. All of these systems adher to some of our goals, but none fulfills of them collectively.

Paszke and Xie @extensible_tabular combine scoped and first-class labels in a sound calculus and give a unification-based inference algorithm for it. We base our work on theirs and use the same ingredients: Records {ρ} are formed from rows ρ, and record concatenation is merely the concatenation of them: {ρ | ρ'}, but we depart from their work in two ways: We admit an unknown type ★ to form a total system (G3) and eliminate the tail-rule of their calculus.

== Our Position <our-position>

A field selection e.l in ordinary type systems is elaborated into a field constraint and given to unification. Unification then solves the row constraint by assigning the label to the row variable, deciding its origin on the spot. This is the point where previously discussed type systems either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation.

We take another route and delay the assignment as long as possible by recording field demands on type schemes. This way, row unification and field demands are different judgements and unification never has to guess a label into a row. At the end of unification, all unresolved lookups are then typed with the unknown type ★.

== Scoped Rows and Three-Valued Lookup
The concatenation of two records {r} ‖ {r'} with row types ρ and ρ' is results in a simple compound type: {ρ' | ρ}.Notice that the order of the rows is reversed and no information is lost during this process. We combine it with a lookup relation of the form $ρ.l ↓ r$ that traverses the natural structure of a row, looking recursively for a label. The result r can be of the following three kinds:

The first two results are of type τ, if the lookup is successful, or a marker ⊥, if the label is definitely absent. When a row variable α is encountered, shadowing behaviour is unclear because α might contain the label l or not. No further positive information about α can resolve this situation, so we park this lookup and continue. When substitution solves row variables, lookup can advance further into the record and resolve it. After substitution, every $?$ that was not resolves this way is typed as the unknown type ★.

== Selection
The smallest program that selects a field is x: x.l. Its argument must be a record ${ρ}$, and the lookup $ρ.l$ decides the result. For $ρ = (l: τ₀)$ the lookup hits and the selector types at ${(l: τ₀)} → τ₀$. For $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$. Here ★ marks a selection that fails whenever it is run, not uncertainty. We still accept this program with a warning because it might be in a lazy position and never be evaluated.

A let binding abstracts over its bound variables and also records such a pending lookup in the scheme. Using this mechanism, two different uses can be typed:

$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$

The first use forces f to be a record, and the second one applies f to a record without l. No plain scheme covers both these types, and @principality shows that this forces qualified schemes. This also shows the advancement of our approach: The search rule of Paszeke & Xie forces a label decision, and unification has to attribute it to the row variable. In our type system, the selector types at ${(l: δ | ρ) → δ}$. The constraint records that every argument must supply the l field and f {} is rejected.

== The Wand Ambiguity, Resolved

The Wand ambiguity of @sec-motivation that concatenates two unknown records and selects from it is resolved by the same mechanism:

$ #b[let] f = (a: b: (a ‖ b).l) #b[in] … $

Given $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$. β is placed first since lookup prefers the fields of b, and lookup then stops on this typevar with an unknown result ?. Again, whether the field comes from b or from a depends on whether β contains l. A plain type can only record this as ${α} → {β} → ★$ but Mininix records it with the following type scheme:

$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $

The row demand is kept in the scheme and replayed at every use: f {l = c₁} {} looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$ whereas f {l = c₁} {l = c₂} yields $𝓫_(c₂)$ because the right operand shadows, and f {} {} yields ★. Remember, we allow definitely missing lookup because in lazy positions, they might never be evaluated.

// [Inference cannot always answer a lookup when it meets it](wording). In (x: x.l) {l = c}, the selector is inferred first: x receives a fresh row ρ, the lookup $ρ.l$ answers $?$, and inference returns a fresh δ for the result while deferring the pending lookup as a stump $⟨ρ ▷ ρ.l ↓ δ⟩$, [blocked on ρ](explain the structure of a stump? A general idea about a stump would be helpful). Applying the selector then solves $ρ ≔ (l: 𝓫_c)$. The solution unblocks the stump, the lookup is re-asked and hits, and δ is unified with $𝓫_c$. For (x: x.l) {} the same stump wakes to definite absence, δ becomes ★, and a warning records the place. Row unification itself never sees the field demand; @inference develops this mechanism in full.

== Consequences for Inference

The consequence is that field demands do not have to be resolved by unification. A demand that cannot be answered yet is deferred and either answered later when a lookup on a record with more instantiated type variables reaches further into the record or recorded as ★ otherwise. Where other row theory would need a disjunction, we write ★ and as a result inference remains syntactic first-order unification. No constraint solver needs to run, no entailment relation is left as a parameter, and no [unification modulo AC](cite) is needed, as well as no backtracking. The tail condition of Paszke & Xie is dropped and replaced by an algebraic property over the rows. The result is that every move is forced, and whesn no move applies, stuck is reported, and uncertainty is the result.

