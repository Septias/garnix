#import "../functions.typ": *

#let meta = [
  - add a proper introduction to the qualified schemes
]

= The Declarative Calculus <declarative>

#let syntax = figure(
  caption: "The syntax of MiniNix.",
  box(width: 100%, stack(
    spacing: 20pt,
    align(center, flexbox(
      $#type_name("Labels") l ∈ 𝓛$,
      $#type_name("Variables") x ∈ 𝓧$,
      $#type_name("Base types") 𝓫 ∈ 𝓑$,
      $#type_name("Constants") c ∈ 𝓒$,
    )),
    subbox(caption: "Terms")[
      $
        #type_name("Term") e & ::= c | x | (x: e) | e₁e₂ | (e₁ ‖ e₂) | e.l | { ξ } | #b[let] x = e₁ #b[in] e₂ \
        #type_name("Record Body") ξ & ::= ε | l = e | (ξ₁ | ξ₂) \
      $
    ],
    subbox(caption: "Types")[
      $
               #type_name("Sort") κ & ::= "Type" | "Row" \
               #type_name("Type") τ & ::= α | 𝓫 | ★ | τ -> τ | { ρ } \
                #type_name("Row") ρ & ::= ε | α | l: τ | (ρ₁ | ρ₂) \
        #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.l ↓ δ⟩ \
        #type_name("Type Scheme") σ & ::= ∀(macron(α): macron(κ)). Q ⇒ τ | τ \
      $
    ],
  )),
)
#syntax <syntax>

@syntax gives the syntax of terms and types. We extend the standard λ-calculus with records, asymmetric concatenation and let-polymorphism. Functions are written $(x: e)$ to mirror the syntax of NixLang and we assume a set 𝓒 of constants $c$ and a finite set 𝓑 of base types $b$, together with a total mapping $c ↦ 𝓫_c$ that assigns each constant its base type. The unknown type ★ types expressions the analysis cannot decide and acts as a fallback for the algorithmic implementation. Record literals ${ξ}$ and record types ${ρ}$ are similar in their structure: Both are formed as inductive trees from an empty base, the binding of a single label and concatenation. Polymorphism is stratified into a scheme σ that quantifies only over monotypes and rows. This is the standard Hindley-Milner @damas_milner technique, keeping instantiation predicative and inference decidable. We depart from standard Hindley-Milner schemes by using constraints: Every scheme is _qualified_ by a list Q of pending lookups ⟨ρ.l ↓ δ⟩ called _stumps_ whose result δ is a type over the scheme's own quantified variables, usually a single one.

== Row Lookup
#let row_lookup = figure(
  caption: "Row lookup.",
  stack(
    spacing: 15pt,
    align(center, $#type_name("Lookup Result") r ::= τ | ⊥ | ?$),
    flexbox(
      derive("L-ε", (), $ε.l ↓ ⊥$),
      derive("L-hit", ($l₁ = l₂$,), $(l₁: τ).l₂ ↓ τ$),
      derive("L-miss", ($l₁ ≠ l₂$,), $(l₁: τ).l₂ ↓ ⊥$),
      derive("L-var", (), $α.l ↓ #h(0.2em) ?$),
      derive("L-conc-hit", ($ρ₁.l ↓ τ$,), $(ρ₁ | ρ₂).l ↓ τ$),
      derive(
        "L-conc-skip",
        ($ρ₁.l ↓ ⊥$, $ρ₂.l ↓ r$),
        $(ρ₁ | ρ₂).l ↓ r$,
      ),
      derive("L-conc-?", ($ρ₁.l ↓ #h(0.2em) ?$,), $(ρ₁ | ρ₂).l ↓ #h(0.2em) ?$),
    ),
  ),
)
#row_lookup <row-lookup>

The structure of row types is traversed by a dedicated lookup judgement ⟨ρ.l ↓ r⟩ read as "the lookup of label $l$ in row $ρ$ has result $r$" with $r := τ | ⊥ | #h(0.2em) ?$. The judgement answers with either a definite type τ, definite absence ⊥ or the unknown marker ? when a row variable renders shadowing unclear. L-ε, L-hit and L-miss form the base cases for row-var free lookups and L-var returns the unknown marker when a row variable is encountered. The concatenation rules L-conc-∗ bubble up a definite result and preserve the left-precedence semantics of asymmetric concatenation.

// todo: a bit of repetition
A lookup can advance further into a row as soon as a blocking row variable α is solved by ρ' during unification $α := ρ'$. An unknown lookup-result $?$ can then become definite if the "unlocked" row behind it contains the searched label or it blocks again on a later row variable. The addition of an unknown result makes our lookup relation total:

=== Properties of the Lookup Relation <lookup-metatheory>

#lemma(name: [Determinism and totality], lean: "lookup_total, LookupQ.det")[
  For every row ρ and label l there is exactly one lookup result r with $ρ.l ↓ r$.
] <lem-lookup-det>

#proof[By induction on ρ: every row shape matches exactly one rule.]

Lookup has left-precedence, so a definite result can never be changed by a substitution; only unknown types ? can be refined:

#lemma(name: [Stability under substitution], lean: "lookup_applySubst")[
  If $ρ.l ↓ r$ and $r ≠ #h(0.2em) ?$, then $(θ ρ).l ↓ θ r$ for every substitution θ.
] <lem-lookup-stable>

#proof[By induction on the derivation of $ρ.l ↓ r$. A definite derivation never uses L-var, so it never reaches a row-variable, and θ acts on no part of the row it inspects.]


== Row Equivalence
#let row_equivalence = figure(
  caption: "Row equivalence.",
  flexbox(
    derive("≈-refl", (), $ρ ≈ ρ$),
    derive("≈-symm", ($ρ₂ ≈ ρ₁$,), $ρ₁ ≈ ρ₂$),
    derive("≈-trans", ($ρ₁ ≈ ρ₂$, $ρ₂ ≈ ρ₃$), $ρ₁ ≈ ρ₃$),
    derive("≈-ext", ($τ₁ ≈ τ₂$,), $(l: τ₁) ≈ (l: τ₂)$),
    derive("≈-conc", ($ρ₁ ≈ ρ₁′$, $ρ₂ ≈ ρ₂′$), $(ρ₁ | ρ₂) ≈ (ρ₁′ | ρ₂′)$),
    derive("≈-assoc", (), $((ρ₁ | ρ₂) | ρ₃) ≈ (ρ₁ | (ρ₂ | ρ₃))$),
    derive("≈-unit-l", (), $(ε | ρ) ≈ ρ$),
    derive("≈-unit-r", (), $(ρ | ε) ≈ ρ$),
    derive("≈-comm", ($l₁ ≠ l₂$,), $(l₁: τ₁ | l₂: τ₂) ≈ (l₂: τ₂ | l₁: τ₁)$),
    derive("≈-rcd", ($ρ₁ ≈ ρ₂$,), ${ρ₁} ≈ {ρ₂}$),
    derive("≈-fn", ($τ₁ ≈ τ₁′$, $τ₂ ≈ τ₂′$), $(τ₁ -> τ₂) ≈ (τ₁′ -> τ₂′)$),
  ),
)
#row_equivalence <row-equivalence>

The order of fields in a record carries no meaning: the records `{ a = 1; b = 2; }` and `{ b = 2; a = 1; }` behave identically, so their rows $(a: τ | b: τ′)$ and $(b: τ′ | a: τ)$ should be equivalent. Row equivalence ≈ permits this by allowing shadowing-preserving reordering of fields using controlled swaps of fields. @row-equivalence gives the rules for the symmetric, transitive and associative relation with the left and right unit ε. ≈-comm forbids the swapping of equal labels as that would change shadowing behaviour, and ≈-rcd and ≈-fn lift ≈ to types. The base types, ★ and type variables are equivalent only to themselves, and ≈ is a congruence below records and arrows. ≈-ext handles singular fields and ≈-conc the concatenation of two rows.


=== Row Equivalence Is a Trace Monoid <trace-monoid>
By ≈-comm, equal labels cannot be swapped and there is no rule to move a label in front of a row variable because that would change shadowing behaviour similarly. Row variables then cut rows into segments where permutation is allowed, delimited by row-variable barriers that cannot be crossed. This opens a new perspective on rows: A row is a word over an alphabet with two components: fields $(l: T)$ and row-variables α. Two letters may swap when they are fields with distinct labels; a row-variable commutes with nothing, and neither do two fields with the same label. Rows modulo ≈ together with the unit ε then form a partially commutative monoid, also called a _trace monoid_.

This has two consequences. First, ≈ is decidable and has a normal form.

#theorem(name: [Characterization of ≈], lean: "rowEquiv_iff_char")[
  $ρ₁ ≈ ρ₂$ if and only if ρ₁ and ρ₂ have the same sequence of row-variables and, for every label l and every var-free segment, the subsequences of fields labelled l agree pointwise up to ≈.
] <thm-char>

Sorting each var-free segment stably by label yields a normal form. For example, $(m: 𝓫 | l: 𝓫 | α | l: 𝓫′ | m: 𝓫)$ splits into the segments $(m: 𝓫 | l: 𝓫)$ and $(l: 𝓫′ | m: 𝓫)$ around α, and its representative is $(l: 𝓫 | m: 𝓫 | α | l: 𝓫′ | m: 𝓫)$: the first segment is reordered, and no field crosses α. Second, the monoid is cancellative.

#lemma(
  name: [Cancellativity],
  lean: "RowEquiv.cancel_cat_left, RowEquiv.cancel_cat_right",
)[
  If $(ρ | ρ₁) ≈ (ρ | ρ₂)$ or $(ρ₁ | ρ) ≈ (ρ₂ | ρ)$, then $ρ₁ ≈ ρ₂$.
] <lem-cancel>

Unification uses both of these properties: The normal form is used to process rows from both sides and cancellativity allows cancelling a common prefix or suffix (@unification).


== Sorts
#let sorting = figure(
  caption: "Sorting.",
  flexbox(
    derive("S-var", ($α: "Type" ∈ Γ$,), $Γ ⊢ α: "Type"$),
    derive("S-base", (), $Γ ⊢ 𝓫: "Type"$),
    derive("S-★", (), $Γ ⊢ ★: "Type"$),
    derive(
      "S-fn",
      ($Γ ⊢ τ₁: "Type"$, $Γ ⊢ τ₂: "Type"$),
      $Γ ⊢ τ₁ -> τ₂: "Type"$,
    ),
    derive("S-rcd", ($Γ ⊢ ρ: "Row"$,), $Γ ⊢ {ρ}: "Type"$),
    derive("S-ρ-var", ($α: "Row" ∈ Γ$,), $Γ ⊢ α: "Row"$),
    derive("S-ε", (), $Γ ⊢ ε: "Row"$),
    derive("S-field", ($Γ ⊢ τ: "Type"$,), $Γ ⊢ (l: τ): "Row"$),
    derive(
      "S-conc",
      ($Γ ⊢ ρ₁: "Row"$, $Γ ⊢ ρ₂: "Row"$),
      $Γ ⊢ (ρ₁ | ρ₂): "Row"$,
    ),
    derive(
      "S-scheme",
      (
        $Γ · (macron(α): macron(κ)) ⊢ τ: "Type"$,
        $∀⟨ρ.l ↓ δ⟩ ∈ Q. #h(0.3em) Γ · (macron(α): macron(κ)) ⊢ ρ: "Row" and (macron(α): macron(κ)) ⊢ δ: "Type"$,
      ),
      $Γ ⊢ (∀(macron(α): macron(κ)). Q ⇒ τ) #h(3pt) "ok"$,
    ),
  ),
)
#sorting <sorting>

@sorting gives the sorting rules of our calculus. We only differentiate between `Type` and `Row` because ρ and τ are already disjoint syntactic categories. The first three rules sort the base types as types and S-fn types functions by recursing into the domain and codomain. S-rcd shows how a row constitutes a type by placing it between braces, S-ρ-var is the equivalent of S-var for rows and S-ε, S-field and S-conc sort rows. S-scheme guarantees well-formedness of type schemes with two premises: The first fixes the parameters' sorts and the second premise fixes the sorts in stumps. We deliberately use fewer kinds than Paszke and Xie @extensible_tabular because MiniNix has no type application, first-class rows or label singletons. First-class labels add a `Label` sort and change nothing else about the discipline (@sec-fc-labels).


== Typing Rules
#let declarative = figure(
  caption: "Declarative typing rules.",
  flexbox(
    derive("T-cons", (), $Γ ⊢ c: 𝓫_c$),
    derive("T-var", ($x: σ ∈ Γ$, $σ ≥ τ$), $Γ ⊢ x: τ$),
    derive("T-eq", ($Γ ⊢ e: τ₁$, $τ₁ ≈ τ₂$), $Γ ⊢ e: τ₂$),
    derive(
      "T-λ-I",
      ($Γ ⊢ τ₁: "Type"$, $Γ · (x: τ₁) ⊢ e: τ₂$),
      $Γ ⊢ (x: e): τ₁ -> τ₂$,
    ),
    derive("T-λ-E", ($Γ ⊢ e₁: τ₁ -> τ₂$, $Γ ⊢ e₂: τ₁$), $Γ ⊢ e₁e₂: τ₂$),
    derive(
      "T-let",
      (
        $Γ ⊢ σ #h(3pt) "ok"$,
        $∀τ₁. #h(0.3em) σ ≥ τ₁ ⟹ Γ ⊢ e₁: τ₁$,
        $∃τ₁. #h(0.3em) σ ≥ τ₁$,
        $Γ · (x: σ) ⊢ e₂: τ₂$,
      ),
      $Γ ⊢ #b[let] x = e₁ #b[in] e₂: τ₂$,
    ),
    derive(
      "T-conc",
      ($Γ ⊢ e₁: {ρ₁}$, $Γ ⊢ e₂: {ρ₂}$),
      $Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }$,
    ),
    derive("T-sel", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ τ$), $Γ ⊢ e.l: τ$),
    derive("T-sel-★", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ #h(0.2em) ?$), $Γ ⊢ e.l: ★$),
    derive("T-sel-⊥", ($Γ ⊢ e: {ρ}$, $ρ.l ↓ ⊥$), $Γ ⊢ e.l: ★$),
    derive("T-★-intro", ($Γ ⊢ e: τ$,), $Γ ⊢ e: ★$),
    derive("T-rec", ($Γ ⊢ ξ: ρ$,), $Γ ⊢ { ξ }: { ρ }$),
    derive("T-ξ-empty", (), $Γ ⊢ ε: ε$),
    derive("T-ξ-field", ($Γ ⊢ e: τ$,), $Γ ⊢ (l = e): (l: τ)$),
    derive(
      "T-ξ-conc",
      ($Γ ⊢ ξ₁: ρ₁$, $Γ ⊢ ξ₂: ρ₂$),
      $Γ ⊢ (ξ₁ | ξ₂): (ρ₁ | ρ₂)$,
    ),
  ),
)
#declarative <declarative-rules>

The typing rules extend the Hindley-Milner system @damas_milner with records, concatenation and the unknown type ★. Most of them are standard. T-cons types each constant at its designated base type $b_c$ and T-var takes a type from the type environment, possibly instantiating type schemes using the instantiation relation from @instantiation. T-eq relates two types under row equivalence @row-equivalence and the two T-λ-∗ rules introduce and eliminate functions. T-rec, T-ξ-empty and T-ξ-field type record literals field by field and T-conc combines them; the concatenation of record bodies is handled equally by T-ξ-conc.

T-let differs from its Hindley-Milner counterpart, because its schemes may carry stumps. Instead of generalizing a type of e₁, it binds x to a scheme σ and constrains σ by three premises. The first premise $Γ ⊢ σ "ok"$ uses the sorting rules to make σ well-sorted. This way, every lookup happens on a row and every result variable δ is a type the scheme quantifies over. The second premise ensures that every instance of σ is a typing of e₁, so x may be used at any of them. We call this _instance-closed_. The third makes σ _inhabited_, meaning it has at least one instance. Without inhabitation, `let x = (3 4) in 5` would get a type while being stuck and it can be uninhabited when two different definitions for δ exist. The last premise types e₂ with x bound to σ.

Our record calculus admits three lookup rules (T-sel, T-sel-★ and T-sel-⊥) for the three possible outcomes of the lookup judgement. T-sel is standard and selects a definite type but T-sel-★ and T-sel-⊥ are new. Nix is a lazy language and a faulty record lookup might never be evaluated. This is why T-sel-⊥ types a definitely failing lookup at ★ which might seem nonsensical at first. The _soft-typing_ approach of MiniNix is permissive in record-selection and we discuss later \@[gradual] in which situations this rule can be removed. T-sel-★ is the sensible version that types an unknown lookup result at the unknown type. Finally, T-★-intro blurs any concrete type into the unknown.

#example(name: [Two ways to type a selection at ★])[
  The selection `{}.l` has type ★ by T-sel-⊥, since $ε.l ↓ ⊥$, and it evaluates to a lookup error by ↯-sel. In a context with $β: "Row"$, the selector `x: x.l` has type ${β} → ★$ by T-sel-★, since $β.l ↓ #h(0.2em) ?$. Applied to `{l = c}` it evaluates to c, applied to `{}` to a lookup error. Both selections receive ★, but T-sel-⊥ marks a selection that fails whenever it is evaluated, while T-sel-★ marks one whose outcome the row does not yet determine.
] <ex-sel-star>

T-sel-★, T-sel-⊥ and T-★-intro place ★ where a more informative type may also be derivable. The following order compares typings by how much of this information they keep.

#definition(name: [Precision])[
  The _precision order_ ⊑ is the least preorder on types that has ★ as top element and is a congruence for → and for the field types of rows. It is covariant in both positions of →: it measures information content and is not a subtyping relation. We write $τ ≼ τ′$ when $τ ≈ τ″ ⊑ τ′$ for some τ″.
] <def-precision>

== Instantiation
#let instantiation = figure(
  caption: "Instantiation.",
  flexbox(
    derive(
      "I-inst",
      (
        $Γ ⊢ θ: (macron(α): macron(κ))$,
        $∀q ∈ Q. #h(0.3em) θ ⊨ q$,
      ),
      $(∀(macron(α): macron(κ)). Q ⇒ τ) ≥ θ τ$,
    ),
    derive("D-hit", ($(θ ρ).l ↓ τ$, $θ δ ≈ τ$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-⊥", ($(θ ρ).l ↓ ⊥$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
    derive("D-?", ($(θ ρ).l ↓ #h(0.2em) ?$, $θ δ = ★$), $θ ⊨ ⟨ρ.l ↓ δ⟩$),
  ),
)
#instantiation <instantiation>

@instantiation defines the instance relation $σ ≥ τ$, used by T-var. The first premise of I-inst makes θ respect the sorts and the second is a _discharge_. A discharge $θ ⊨ q$ replays every stump at the instance: the lookup is performed on θρ, and θδ is pinned to its verdict such that both coincide. The replay allows derivations up to ≈ by D-hit, D-⊥ and D-\?, and requiring ≈ rather than equality in D-hit loses nothing: [T-eq closes every set of instances under ≈, and that is all that inference can guarantee when it discharges the stumps of one instance in turn.](what does this mean? Simplify)

#example(name: [Instances of selQ])[
  Consider $"selQ" = ∀(β: "Row", δ: "Type"). ⟨β.l ↓ δ⟩ ⇒ {β} → δ$.
  - $θ = [β ≔ (l: 𝓫), δ ≔ 𝓫]$ discharges the stump by D-hit, since $(l: 𝓫).l ↓ 𝓫$, so ${(l: 𝓫)} → 𝓫$ is an instance.
  - $θ = [β ≔ ε, δ ≔ ★]$ discharges it by D-⊥, since $ε.l ↓ ⊥$, so ${ε} → ★$ is an instance.
  - $θ = [β ≔ ε, δ ≔ {ε}]$ violates D-⊥, which demands $θ δ = ★$. Hence ${ε} → {ε}$ is not an instance, although it is an instance of the plain scheme $∀(β: "Row", δ: "Type"). {β} → δ$.
] <ex-selq-inst>

Without stumps (Q = ε), our instantiation is plain Hindley-Milner instantiation, but with qualified schemes, a scheme can be uninhabited because two stumps may pin the same δ to different verdicts. This is why T-let demands inhabitation explicitly.

#example(name: [An uninhabited scheme])[
  The scheme $∀(δ: "Type"). ⟨(l: 𝓫).l ↓ δ⟩, ⟨ε.l ↓ δ⟩ ⇒ δ$ is well-sorted but has no instance. The first stump hits, so D-hit demands $θ δ ≈ 𝓫$. The second misses, so D-⊥ demands $θ δ = ★$. Since ★ is equivalent only to itself, no θ satisfies both.
] <ex-uninhabited>


== Operational Semantics
#let semantics = figure(
  caption: "Operational semantics.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Values and Evaluation Contexts")[
      $
        #type_name("Value") v & ::= c | (x: e) | { ξ } \
        #type_name("Context") E & ::= [·] | E e | v E | E.l | (E ‖ e) | (v ‖ E) | #b[let] x = E #b[in] e \
      $
    ],
    subbox(caption: "Reduction")[
      #flexbox(
        derive("E-β", (), $(x: e) v → e[x ≔ v]$),
        derive("E-let", (), $#b[let] x = v #b[in] e → e[x ≔ v]$),
        derive("E-sel", ($ξ(l) = e$,), ${ξ}.l → e$),
        derive("E-conc", (), ${ξ₁} ‖ {ξ₂} → {ξ₂ | ξ₁}$),
        derive("E-ctx", ($e → e′$,), $E[e] → E[e′]$),
      )
    ],
    subbox(caption: "Lookup Error")[
      #flexbox(
        derive("↯-sel", ($ξ(l) "undefined"$,), ${ξ}.l ↯$),
        derive("↯-ctx", ($e ↯$,), $E[e] ↯$),
      )
    ],
  )),
)
#semantics <semantics>

@semantics gives the call-by-value small-step reduction → of the mechanization. The definitions are entirely standard with one subtlety: a record literal is a value and its fields are only evaluated when they are selected. NixLang is a lot more complicated, as Broekhoff and Krebbers @verified show by giving a small-step semantics based on delayed substitution that faithfully tracks the overwriting behaviour of `with e₁; e₂` and laziness in records, let-statements and operators, but in this thesis, we concern ourselves with type inference and limit the operational semantics to a bare minimum. $ξ(l)$ denotes the leftmost binding of l in ξ, so selection agrees with ↓, and E-conc places the right operand first, as T-conc does. The judgement $e ↯$ is a _lookup error_: a selection reaches a record literal that lacks the selected label. @sec-fc-labels extends this with another error case.
