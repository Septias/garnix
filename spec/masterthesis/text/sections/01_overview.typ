#import "../functions.typ": *

= MiniNix <sec-overview>

We motivate the design decisions of MiniNix by comparing it against existing record calculi with regard to the requirements G1-G4. We then introduce MiniNix by examples and @declarative follows with the formal treatment.

== Background <sec-background>
Many existing record calculi are ruled out by the design goals G1-G4. For example, calculi that restrict  concatenation to disjoint labels @symm_concat @xie2020row are not faithful to Nix's overwriting concatenation operation and others are unusable because they require explicit rows or labels @wand_complete @remy_typechecking. Standard gradual systems @gradual_siek @gradual_extensible_rows @gradual_dti insert casts at the boundary to untyped code and violate G2 by forcing lazy values. Systems with width-subtyping @algebraic_subtyping @simplesub @mlstruct @frisch_semantic, allows forgetting fields in records and without absence information, it cannot be combined with asymmetric record concatenation. A field removed by typing persists in the semantics and may shadow another field at runtime. Other type systems like HM(X) @hmx and Rose are theoretically astonishing, but defer the tricky decision of field attribution and leave it unanswered. HM(X) has principal and first-order type inference, but the constraint solver is a parameter. Rose @rose defers row predicates to an entailment relation and leaves the underlying row theory unspecified. Both show no clear path towards full inference. Finally, set-theoretic types @frisch_semantic @castagna2023programming rest on a subtyping relation with a worst-case exponential decision procedure and violate G3. None of these systems meet all our goals at once.

Only Paszke and Xie @extensible_tabular combine first-class labels and asymmetric concatenation in a sound calculus and give a unification-based inference algorithm for it. We base our work on theirs and use the same ingredients: Records ${ ρ }$ are formed from rows $ρ$, and record concatenation is merely the concatenation of them: ${ρ | ρ'}$. We depart from their work in two ways: We admit an unknown type ★ to type what the analysis cannot decide (G3) and eliminate the shared-tail side condition that their unification needs for termination by delaying field lookups.

A field lookup $e.l$ is usually translated into a field constraint and given to unification. Unification then solves the row constraint by assigning the label to the row variable, deciding its origin on the spot. This is the point where previously discussed type systems either backtrack, solve modulo associativity and commutativity, or defer to an unspecified entailment relation. We take another route and delay the assignment as long as possible by recording pending lookups on type schemes. This way, row unification and lookup are different judgements and unification never has to guess a label into a row.

== Scoped Labels and Three-Valued Lookup
The concatenation $e ‖ e′$ of two records of types ${ρ}$ and ${ρ′}$ results in a simple compound type: ${ ρ' | ρ }$. Notice that the order of the rows is reversed to keep the right-precedence of record concatenation and that no information is lost during this process. We combine it with a lookup relation of the form $ρ.l ↓ r$ that traverses the natural structure of a row $ρ$, looking recursively for a label $l$. The result $r$ can be one of the following three kinds:

If the lookup is successful, a definite type τ is returned and if the label is definitely absent the result is ⊥. We add a third verdict:  When a row variable α is encountered, shadowing behaviour from this point on is unclear because α might contain the label l or not. We return a $?$, park the lookup and continue. When substitution solves row variables, lookup can advance further into the record and resolve it. The following section shows how our approach translates to ordinary selection and the Wand ambiguity.


== Selection
// can we say the it types as a function of this signature?
The smallest program with an undecided lookup is `x: x.l`. This function takes an argument $x$ and accesses its field $l$ in the body. To be applicable, $x$ has to be a record of the form ${ρ}$, where ρ is a row. For a definite row $ρ = (l: τ)$ consisting only of the accessed label $l$, the lookup hits and the function types at ${ l: τ } → τ$. For $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$. We accept record accesses that will lead to a definite error when evaluated, because Nix is lazy in record fields and such an error may never be forced. Rejecting the program would reject working code (G3). ★ keeps the miss harmless: it has no eliminators, so it can be passed on, but every use at a concrete type is rejected.

A lookup that cannot yet be answered is recorded as a _pending lookup_ in the scheme of a let-binding. A pending lookup $⟨ρ.l ↓ δ⟩$ records that a lookup of label $l$ in row $ρ$ is blocked and $δ$ names the result of the lookup. Using this mechanism, two different uses can be typed:

$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$

The first use requires the result of f to be a record, and the second one applies f to a record without l. It is applied at two different uses, depending on the instantiation of the row variable.

== The Wand Ambiguity
The Wand ambiguity $a: b: (a ‖ b).l$ is handled by the same mechanism. Given $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$ and lookup stops on the row variable $β$ with an undecided result $?$. Again, whether the field comes from $b$ or $a$ depends on whether $β$ contains $l$. A plain type can only record this as ${α} → {β} → ★$ but MiniNix records it with the following type scheme:

$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $

The type gains a qualifier $⟨(β | α).l ↓ δ⟩$ that describes the undecided lookup. It contains the resulting row of the Wand ambiguity $(β | α)$, the field that is accessed $l$ and a placeholder return type $δ$ that is also the result of the function. On every use, the lookup is replayed and answered based on the instantiation of variables. In this case, f {l = c₁} {} looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$ whereas f {l = c₁} {l = c₂} yields $𝓫_(c₂)$ because the right operand shadows, and f {} {} yields ★.

The consequence of our inference approach is that selections do not have to be resolved by unification. A lookup that cannot be answered yet is deferred and either answered later when a lookup on a record with more instantiated type variables reaches further into the record, or kept as a qualifier and read as ★ at a use that leaves it unanswered. As a result inference stays first-order and never backtracks because every move is forced.
