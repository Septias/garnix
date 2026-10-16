#import "../functions.typ": *


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

@syntax gives the syntax of terms and types: a λ-calculus extended with records, record concatenation and let-polymorphism. Functions are written $(x: e)$, as in NixLang. We assume a finite set 𝓒 of constants and a finite set 𝓑 of base types, together with a total map $c ↦ 𝓫_c$ that assigns each constant its base type. The unknown type ★ types the expressions the analysis cannot decide (@sec-motivation). Record literals ${ξ}$ and record types ${ρ}$ share one tree structure of fields and concatenations. As in Hindley-Milner @damas_milner, polymorphism is stratified: a scheme σ quantifies over monotypes and rows only, which keeps instantiation predicative and inference decidable. We depart from standart Hindley-Milner schemes using constraints: our schemes are _qualified_, meaning a scheme carries a list Q of pending lookups ⟨ρ.l ↓ δ⟩, called _stumps_, whose result δ is a type over the scheme's own quantified variables, usually a single one. A plain scheme is the special case Q = ε. // [This is a direct consequence of the constraints our goal language NixLang induces (@principality)](remove this?).

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

@row-lookup gives the derivation rules for record-type lookups. The judgement $ρ.l ↓ r$ is read as "the lookup of label $l$ in row $ρ$ has result $r$" with $r := τ | ⊥ | #h(0.2em) ?$. The lookup succeeds either with a definite type τ due to a successful lookup, ⊥ when the label is definitely absent, or ? if the search reaches a row-variable before it finds the label. Accordingly, L-ε and L-miss return with a negative lookup result, L-hit with a positive result and the rules L-conc-hit and L-conc-skip recurse into the left and right subtrees a row can form, with left precedence. L-var terminates the search at a row-variable α with the unknown result ?, since α may or may not shadow l, and L-conc-? bubbles such a result up.

Solving $α ≔ ρ'$ replaces α by ρ' in the row, and looking the label up again lets the search advance past the former variable. Refinement of a $?$ is thus plain substitution of a row-variable, at instantiation or at application, followed by a fresh lookup, and never a property of the lookup relation itself.


=== Properties of the Lookup Relation <lookup-metatheory>

#lemma(name: [Determinism and totality], lean: "lookup_total, LookupQ.det")[
  For every row ρ and label l there is exactly one lookup result r with $ρ.l ↓ r$.
] <lem-lookup-det>

#proof[By induction on ρ: every row shape matches exactly one rule.]

Since the result is unique, whether a selection is typed at ★ is determined by the row alone, and no typing rule chooses it.


#lemma(name: [Stability under substitution], lean: "lookup_applySubst")[
  If $ρ.l ↓ r$ and $r ≠ #h(0.2em) ?$, then $(θ ρ).l ↓ θ r$ for every substitution θ.
] <lem-lookup-stable>

#proof[By induction on the derivation of $ρ.l ↓ r$. A definite derivation never uses L-var, so it never reaches a row-variable, and θ acts on no part of the row it inspects.]

Only $?$ may change, to whatever the substituted row yields. This makes deferral sound: a deferred lookup can be re-asked after every substitution, and a definite one never needs to be.


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

The order in which a record lists its fields carries no meaning: `{ a = 1; b = 2; }` and `{ b = 2; a = 1; }` are the same attribute set, so the rows $(a: τ | b: τ′)$ and $(b: τ′ | a: τ)$ should describe the same records. Row equivalence ≈ (@row-equivalence) establishes this, but only as far as scoped labels allow. A row may contain a label several times, and lookup answers with the leftmost occurrence, so two fields with the same label keep their order. A row-variable may stand for a row that contains any label, so no field may cross it either: exchanging $(l: τ)$ and α would change which field a lookup of l finds once α is solved to a row with an l-field.

The rules state exactly this. ≈ is an equivalence and a congruence, admits associativity and the units ε, and lets two fields commute by ≈-comm only when their labels are distinct. No rule moves a field past a row-variable. For concrete labels the premise $l₁ ≠ l₂$ is decidable, so ≈ needs no separate constraint on labels. ≈-rcd and ≈-fn lift ≈ to types: base types, ★ and type variables are equivalent only to themselves, and ≈ is a congruence below records and arrows, with fields handled by ≈-ext. Lookup respects ≈: equivalent rows answer every lookup with equivalent results (`lookup_equiv` in Lean). A rule that exchanged two fields with the same label, or a field with a row-variable, would break this property.

≈ is an equivalence, not a subtyping relation. No field is ever dropped, since a field forgotten by width-subtyping still overwrites at run time once its record is concatenated (@sec-background).

=== Row Equivalence Is a Trace Monoid <trace-monoid>
Calculi with unique labels decide row equality by sorting both rows by label. Under scoped labels this is unsound. Sorting identifies $(l: 𝓫 | α)$ with $(α | l: 𝓫)$, but the substitution $α ≔ (l: 𝓫′)$ makes the lookup of l answer 𝓫 in the first row and 𝓫′ in the second. A row-variable therefore cuts a row into segments, and fields may be reordered only within a segment. This is the structure of a trace monoid.

Read a row as a word over an alphabet of fields $(l: τ)$ and row-variables α. Two letters may swap when they are fields with distinct labels; a row-variable commutes with nothing, and neither do two fields with the same label. Rows modulo ≈ are then the free partially commutative monoid, a _trace monoid_, on this independence relation, with ε as unit.

This has two consequences. First, ≈ is decidable and has a normal form.

#theorem(name: [Characterization of ≈], lean: "rowEquiv_iff_char")[
  $ρ₁ ≈ ρ₂$ if and only if ρ₁ and ρ₂ have the same sequence of row-variables and, for every label l and every var-free segment, the subsequences of fields labelled l agree pointwise up to ≈.
] <thm-char>

Equivalently, the projections of the two rows onto every pair of dependent letters coincide, and sorting each var-free segment stably by label yields a representative of the class. For example, $(m: 𝓫 | l: 𝓫 | α | l: 𝓫′ | m: 𝓫)$ splits into the segments $(m: 𝓫 | l: 𝓫)$ and $(l: 𝓫′ | m: 𝓫)$ around α, and its representative is $(l: 𝓫 | m: 𝓫 | α | l: 𝓫′ | m: 𝓫)$: the first segment is reordered, and no field crosses α. Second, the monoid is cancellative.

#lemma(
  name: [Cancellativity],
  lean: "RowEquiv.cancel_cat_left, RowEquiv.cancel_cat_right",
)[
  If $(ρ | ρ₁) ≈ (ρ | ρ₂)$ or $(ρ₁ | ρ) ≈ (ρ₂ | ρ)$, then $ρ₁ ≈ ρ₂$.
] <lem-cancel>

Unification uses both. The normal form lets it process a row as a _spine_ from either end, and cancellativity lets it cancel a common prefix or suffix of two spines without guessing (@unification).


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

@sorting classifies every type-level phrase as a `Type` or a `Row`. Since τ and ρ are disjoint syntactic categories, the only rules with content are S-var and S-ρ-var, which read the sort of a variable off Γ. The sorts are deliberately fewer than the kinds of Paszke and Xie @extensible_tabular, $κ ::= ★ | κ₁ → κ₂ | "Label" | "Row"$: the arrow kind serves type application, first-class rows and label singletons, none of which MiniNix has, and we name the sorts because ★ is the unknown type here. First-class labels add a `Label` sort and change nothing else about the discipline (@sec-fc-labels).

T-λ-I and T-let are the only typing rules of @declarative-rules with a sorting premise, since they are the only ones that choose a type their other premises do not determine. T-λ-I chooses the parameter type and checks $Γ ⊢ τ₁: "Type"$. T-let chooses a scheme and checks $Γ ⊢ σ "ok"$: the body is well-sorted, every stump looks up in a row, and every stump's result is a type over the quantified variables of σ itself. The result is usually a single variable; inference also produces stumps whose result has already been used at a compound type, such as $𝓫 → β$ (@inference). Every other rule's types are fixed by its premises and are well-sorted whenever they are.


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

The typing rules extend the Hindley-Milner system @damas_milner with records, concatenation and the unknown type ★. Most of them are standard. T-cons types each constant c at its base type $𝓫_c$, T-λ-I and T-λ-E introduce and eliminate functions, and T-eq retypes an expression at any type equivalent up to the row equivalence of @row-equivalence. T-rec, T-ξ-empty, T-ξ-field and T-ξ-conc type record literals field by field, so that the row of a literal lists its fields in source order. T-var looks up x in Γ and instantiates its scheme by the instance relation of @instantiation.

T-let differs from its Hindley-Milner counterpart, since its schemes may carry stumps. Instead of generalizing a type of e₁, it binds x to a scheme σ and constrains σ by three premises. The first, $Γ ⊢ σ "ok"$, requires σ to be well-sorted: its body is well-sorted, every stump looks up in a row, and every stump's result is a type over the quantified variables of σ (@sorting). The second makes σ _instance-closed_ for e₁: every instance of σ is a typing of e₁, so x may be used at any of them. The third makes σ _inhabited_: it has at least one instance. A plain scheme always has its own body as an instance, but a qualified one can have none, and then instance-closure says nothing about e₁ at all. Without the third premise, `let x = (3 4) in 5` would type while being stuck. The last premise types e₂ with x bound to σ.

Two rules are specific to records. T-conc concatenates the rows of both operands with the right operand first: lookup prefers the left of a row, so left-precedence on rows realizes the right-precedence of ‖ that NixLang admits. T-sel types a selection by delegating to the row-lookup relation of @row-lookup.

Schemes are qualified because a `let`-bound selector must answer differently at different uses (@sec-overview). The instance relation of @instantiation replays the stumps of a scheme at every use.

T-sel-★ and T-sel-⊥ have no counterpart in standard row calculi. There, a selection whose label is absent is a type error, and a selection on an open row adds a field to the row-variable, so the row type answers every selection with a definite type. MiniNix is a _soft typing_ system that has to allow selection of unknown labels. An absent label is not rejected, [because NixLang programs routinely carry fields that are never selected on every path](reasoning is wrong, no? Explain why it is needed for progress); T-sel-⊥ types such a selection at ★ and leaves the failure to the lookup error of @semantics. A blocked lookup is not resolved by extending the row-variable, because under scoped labels the variable may itself shadow l, and any field placed into it would be a guess about shadowing. T-sel-★ types the selection at ★ instead. Both rules read their verdict off the lookup relation, which is deterministic (@lem-lookup-det), so for a given row exactly one of T-sel, T-sel-★ and T-sel-⊥ applies. T-★-intro, finally, blurs any type into the unknown.

#example(name: [Two ways to type a selection at ★])[
  The selection `{}.l` has type ★ by T-sel-⊥, since $ε.l ↓ ⊥$, and it evaluates to a lookup error by ↯-sel. In a context with $β: "Row"$, the selector `x: x.l` has type ${β} → ★$ by T-sel-★, since $β.l ↓ #h(0.2em) ?$. Applied to `{l = c}` it evaluates to c, applied to `{}` to a lookup error. Both selections receive ★, but T-sel-⊥ marks a selection that fails whenever it is evaluated, while T-sel-★ marks one whose outcome the row does not yet determine.
] <ex-sel-star>

T-sel-⊥ is needed even for programs that never fail. Record fields are evaluated only when selected (@semantics), so `{a = {}.l; b = c}.b` evaluates to c. Without T-sel-⊥ the field a has no type, and neither does the program.

T-sel-★, T-sel-⊥ and T-★-intro place ★ where a more informative type may also be derivable. The following order compares typings by how much of this information they keep.

#definition(name: [Precision])[
  The _precision order_ ⊑ is the least preorder on types that has ★ as top element and is a congruence for → and for the field types of rows. It is covariant in both positions of →: it measures information content and is not a subtyping relation. We write $τ ≼ τ′$ when $τ ≈ τ″ ⊑ τ′$ for some τ″.
] <def-precision>

T-★-intro realizes the top element at the level of whole typings: every typing $Γ ⊢ e : τ$ yields $Γ ⊢ e : ★$. @refinement and @principality use ≼ to compare typings and instances.


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

@instantiation defines the instance relation $σ ≥ τ$, which only T-var uses. The first premise of I-inst requires θ to respect the sorts of the quantified variables. The second, _discharge_ $θ ⊨ q$, replays every stump at the instance: the lookup is performed on θρ, and θδ is pinned to its verdict, to the found type up to ≈ by D-hit and to ★ by D-⊥ and D-?. These are T-sel, T-sel-⊥ and T-sel-★ once more, evaluated at every use instead of once at the binding. Requiring ≈ rather than equality in D-hit loses nothing, since T-eq closes every set of instances under ≈, and it is all that inference can guarantee when it discharges the stumps of one instance in turn.

#example(name: [Instances of selQ])[
  Consider $"selQ" = ∀(β: "Row", δ: "Type"). ⟨β.l ↓ δ⟩ ⇒ {β} → δ$ of @principality.
  - $θ = [β ≔ (l: 𝓫), δ ≔ 𝓫]$ discharges the stump by D-hit, since $(l: 𝓫).l ↓ 𝓫$, so ${(l: 𝓫)} → 𝓫$ is an instance.
  - $θ = [β ≔ ε, δ ≔ ★]$ discharges it by D-⊥, since $ε.l ↓ ⊥$, so ${ε} → ★$ is an instance.
  - $θ = [β ≔ ε, δ ≔ {ε}]$ violates D-⊥, which demands $θ δ = ★$. Hence ${ε} → {ε}$ is not an instance, although it is an instance of the plain scheme $∀(β: "Row", δ: "Type"). {β} → δ$.
] <ex-selq-inst>

With Q = ε the discharge premise is vacuous, so on plain schemes ≥ is the Hindley-Milner instance relation, and a monotype is its own only instance. A qualified scheme need not have any instance, since two stumps may pin the same δ to different verdicts; this is why T-let demands inhabitation explicitly.

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

@semantics gives the small-step reduction → of the mechanization. Application and `let` are call-by-value, but a record literal is a value as it stands: its fields are evaluated only when they are selected, as in NixLang. $ξ(l)$ denotes the leftmost binding of l in ξ, so selection agrees with ↓, and E-conc places the right operand first, as T-conc does. The judgement $e ↯$ is a _lookup error_: a selection reaches a record literal that lacks the selected label. With the first-class labels of @sec-extensions, a dynamic selection whose key does not evaluate to a label is a lookup error as well.

The semantics is deliberately smaller than that of Broekhoff and Krebbers @verified, who give a mechanized semantics of the full Nix expression language, including recursive attribute sets, `with` and the edge cases of evaluation. Their semantics is the reference for what NixLang programs mean, while ours models only what the type system needs. MiniNix has no recursion, and its laziness is restricted to record fields, since application and `let` are call-by-value. Type safety is established for this fragment; recursion and full laziness are left as future work.
