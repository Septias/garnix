#import "../functions.typ": *
#import "04_unification.typ": u_clash
#import "05_inference.typ": st_park, st_solve

= Extensions Towards NixLang <sec-extensions>

== First-Class Labels <sec-fc-labels>
// PROF: String keys give star (T-sel-dyn-b, T-rcd-dyn-b), so r // { ${n} = v; } with a computed string is rejected: the dominant Nix idiom.

NixLang computes labels that can be used to index records or bind variables at runtime. The expression `x.${}` selects an arbitrary string expression, and the expression `{ ${name} = v; }` binds a value to a label that is result of an arbitrary expression. First class labels fit naturally in our system: Shadowing inside a row can not be decided after label variable so it behaves identically to a row-variable. In fact, in the following section we generalize these two types of variables to _barriers_ afer which shadowing is unclear. The comparison of label variables also benefits from our addition of the unknown type: If two variables can not be decided to be equal or not, the unknown type can be used to type the expression.


#let fc_syntax = figure(
  caption: [Syntax of first-class labels, as an extension of @syntax.],
  box(width: 100%, stack(
    spacing: 20pt,
    subbox(caption: "Terms")[
      $
        #type_name("Term") e & ::= … | \"l\" | e₁.\${e₂} | { \${e₁} = e₂ } \
      $
    ],
    subbox(caption: "Types")[
      $
               #type_name("Sort") κ & ::= "Type" | "Row" | "Label" \
                #type_name("Key") k & ::= l | α \
               #type_name("Type") τ & ::= … | ⌊k⌋ \
                #type_name("Row") ρ & ::= … | \${k}: τ \
        #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.k ↓ δ⟩ \
      $
    ],
  )),
)
#fc_syntax <fc-syntax>

@fc-syntax gives the syntax extension. A label can now be a value, written as a string literal $\"l\"$, as in Nixlang and are used in the record syntax. From now on, a record can be indexed by the `${e₁}` syntax of Nix and the dynamic record ${ \${e₁} = e₂ }$ binds the field named by e₁. We abstract over labels and call it key: A key $k$ can either be a discrete label $l$, or a label variable $α$, which has the new sort `Label`. Keys occur in two places: in the singleton type $⌊k⌋$ and in the _keyed field_ $\${k}: τ$ of a row, whose label is not yet known. Stumps are keyed as well, $⟨ρ.k ↓ δ⟩$, so that a scheme can defer a lookup under a key it abstracts over.

=== Keyed Lookup

#let fc_lookup = figure(
  caption: [Key comparison and keyed lookup. The rules L-ε, L-var and the three concatenation rules of @row-lookup carry over with k in place of l.],
  stack(
    spacing: 15pt,
    align(
      center,
      $#type_name("Key comparison") l ≡ l quad α ≡ α quad l class("relation", \#) l′ "if" l ≠ l′ quad k ≟ k′ "otherwise"$,
    ),
    flexbox(
      derive("L-hit", ($k ≡ l$,), $(l: τ).k ↓ τ$),
      derive("L-miss", ($k class("relation", \#) l$,), $(l: τ).k ↓ ⊥$),
      derive("L-key-?", ($k ≟ l$,), $(l: τ).k ↓ #h(0.2em) ?$),
      derive("L-dhit", ($k ≡ k′$,), $(\${k′}: τ).k ↓ τ$),
      derive("L-dmiss", ($k class("relation", \#) k′$,), $(\${k′}: τ).k ↓ ⊥$),
      derive("L-d?", ($k ≟ k′$,), $(\${k′}: τ).k ↓ #h(0.2em) ?$),
    ),
  ),
)
#fc_lookup <fc-lookup>

@fc-lookup generalizes the lookup relation from labels to keys. Two keys are equal when they are the same label literal or variable, distinct when the labels are different and undecided otherways. Just as for rows, the definite answer are stable under substitution so the lookup stays deterministic and total. A new source of $?$ appears for a field whose key cannot yet be compared. The lookup of l in $(\${γ}: τ)$ may hit or miss depending on the label γ becomes, and a lookup under a variable key α may hit any literal field.

The variable that blocks a $?$ is accordingly no longer always a row-variable. It is the row-variable at which the search stopped, the variable key of the lookup, or the variable key of a field, and inference must wake a stump when any of these is solved.



=== Row Equivalence and Spines

#let fc_equiv = figure(
  caption: [Row equivalence for keyed fields, extending @row-equivalence.],
  flexbox(
    derive("≈-dfield", ($τ₁ ≈ τ₂$,), $(\${k}: τ₁) ≈ (\${k}: τ₂)$),
    derive("≈-dlab", (), $(\${l}: τ) ≈ (l: τ)$),
  ),
)
#fc_equiv <fc-equiv>

A keyed field is congruent only under the same key, and a keyed field with a literal key is the literal field (@fc-equiv). ≈-comm is not extended to preserve shadowing. Spines acquire a third atom, $a ::= l: τ | α | \${α}: τ$, where a literal key is normalized to a field, and the monoid remains cancellative on both sides.


=== Typing and Evaluation

#let fc_typing = figure(
  caption: "Typing rules and reduction for first-class labels.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Typing")[
      #flexbox(
        derive("T-lab", (), $Γ ⊢ \"l\": ⌊l⌋$),
        derive(
          "T-sel-dyn",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ τ$),
          $Γ ⊢ e₁.\${e₂}: τ$,
        ),
        derive(
          "T-sel-dyn-★",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ #h(0.2em) ?$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-sel-dyn-⊥",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ ⊥$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-rcd-dyn",
          ($Γ ⊢ e₁: ⌊k⌋$, $Γ ⊢ e₂: τ$),
          $Γ ⊢ { \${e₁} = e₂ }: { \${k}: τ }$,
        ),
        derive(
          "T-sel-dyn-𝓫",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: 𝓫$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-rcd-dyn-𝓫",
          ($Γ ⊢ e₁: 𝓫$, $Γ ⊢ e₂: τ$),
          $Γ ⊢ { \${e₁} = e₂ }: ★$,
        ),
      )
    ],
    subbox(caption: "Reduction")[
      $
          #type_name("Value") v & ::= … | \"l\" \
        #type_name("Context") E & ::= … | E.\${e} | v.\${E} | { \${E} = e } \
      $
      #flexbox(
        derive("E-sel-dyn", ($ξ(l) = e$,), ${ξ}.\${\"l\"} → e$),
        derive("E-rcd-dyn", (), ${ \${\"l\"} = e } → { l = e }$),
        derive("↯-sel-dyn", ($ξ(l) "undefined"$,), ${ξ}.\${\"l\"} ↯$),
        derive(
          "↯-key",
          ($v ≠ \"l\" "for all" l$,),
          ${ξ}.\${v} ↯ quad { \${v} = e } ↯$,
        ),
      )
    ],
  )),
)
#fc_typing <fc-typing>

@fc-typing gives the typing rules and the reduction. The three rules for dynamic selection are those of static selection with the label replaced by the key of₂'s type, and T-rcd-dyn records the key e₁ in a keyed field. T-sel-dyn-𝓫 and T-rcd-dyn-𝓫 answer a key of base type with ★. Discharge (@instantiation) replays a keyed stump with the keyed lookup: θ is applied to both the row and the key, $(θ ρ).(θ k) ↓ r$, and the verdict pins δ exactly as for a static stump.

The evaluation contexts evaluate the record before the key, and E-sel-dyn then selects the leftmost binding of the label, as E-sel of @semantics does for a static one. A dynamic record evaluates its key but leaves the field unevaluated, as a record literal does. A missing label and a key that is not a label are both lookup errors.

Progress up to ↯ and preservation hold for MiniNix with first-class labels. Under T-rcd-dyn, the new case of preservation is E-rcd-dyn. Before the step, T-rcd-dyn types ${ \${\"l\"} = e }$ at ${\${l}: τ}$, since the key $\"l\"$ has type $⌊l⌋$. After the step, the literal record ${ l = e }$ has type ${l: τ}$, and ≈-dlab with T-eq identifies the two types. Under the two 𝓫-rules the redex has type ★, and the field value or the literal record it steps to has some type, which T-★-intro weakens to ★. These two cases rely on T-★-intro being unrestricted.

Qualified schemes abstract over keys as they do over rows. The dynamic selector and the dynamic record constructor have the schemes

$
  #raw("a: x: x.${a}") & quad : quad ∀(α: "Label", β: "Row", δ: "Type"). #h(0.3em) ⟨β.α ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ \
  #raw("a: v: { ${a} = v; }") & quad : quad ∀(α: "Label", δ: "Type"). #h(0.3em) ⌊α⌋ → δ → { \${α}: δ }
$

and both are instance-closed. The first scheme defers the lookup until both the key and the row are known, so that one binding answers a definite type at a use whose record has the field and ★ at a use whose record lacks it.

=== Unification

#let fc_unif = figure(
  caption: [Unification of keys and of keyed fields, extending @unification-ty.],
  flexbox(
    derive("U-key-refl", (), $⌊α⌋ ≐ ⌊α⌋ ⇝ ∅$),
    derive("U-lab", ($l = l′$,), $⌊l⌋ ≐ ⌊l′⌋ ⇝ ∅$),
    derive("U-lab-clash", ($l ≠ l′$,), $⌊l⌋ ≐ ⌊l′⌋ ⇝ #u_clash$),
    derive("U-key-bind", ($k ≠ α$,), $⌊α⌋ ≐ ⌊k⌋ ⇝ [α ≔ k]$),
    derive(
      "U-key-L",
      ($τ ≐ τ′ ⇝ θ$, $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$),
      $(\${α}: τ) · t₁ scripts(≐)_r (\${α}: τ′) · t₂ ⇝ θ′ ∘ θ$,
    ),
    derive(
      "U-key-R",
      ($τ ≐ τ′ ⇝ θ$, $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$),
      $t₁ · (\${α}: τ) scripts(≐)_r t₂ · (\${α}: τ′) ⇝ θ′ ∘ θ$,
    ),
    derive(
      "U-key-pin",
      ($"key"(f) = k ≠ α$, $[α ≔ k](\${α}: τ) scripts(≐)_r [α ≔ k] f ⇝ θ$),
      $(\${α}: τ) scripts(≐)_r f ⇝ θ ∘ [α ≔ k]$,
    ),
  ),
)
#fc_unif <fc-unification>

A singleton type is rigid and nullary, like a base type, and the key pass of @fc-unification is flat: two labels unify when they are equal and clash otherwise, and a label variable is bound to the other key. A key is atomic, so U-key-bind needs no occurs check. $⌊k⌋$ against any other head clashes by U-clash.


On rows, the moves of @unification-cascade treat a keyed field as a barrier. U-ε-clash fires on a keyed field as on any field, since the empty spine has no field of any key. U-row-clash and U-ground count only literal fields and require the other side to be barrier-free, so that $(l: τ) scripts(≐)_r (\${α}: τ′)$ is not a clash: $[α ≔ l]$ solves it. An occurrence of a variable next to a keyed field is reported as stuck rather than as an occurs failure, since the soundness proof of U-var-occurs counts literal fields and does not extend to keys. U-host does not fire on a side with a keyed field, since its key may become the label of the field to be hosted. Two new moves handle keyed fields.


U-key-L and U-key-R cancel two keyed fields under the same variable key at the front or the back of both spines and unify their types. They are restricted to the ends of the spines, since a literal field in front of $\${α}: τ$ may carry the label that α becomes. Both moves are forced. U-key-pin applies when each side consists of a single field, at least one of them keyed, under different keys. A one-field row is equivalent to another only under the same key, so every unifier identifies the two keys, and the move binds the variable key to the other key and runs the problem again, where U-key-L or U-field-L then pairs the fields. Here f is a single field and key(f) is its label or its key. U-key-pin is tried in both directions.


It solves $(l: τ) scripts(≐)_r (\${α}: τ′)$ with $[α ≔ l]$ and $(\${α}: τ) scripts(≐)_r (\${β}: τ′)$ with $[α ≔ β]$, which is most general since $[α ≔ l, β ≔ l] = [β ≔ l] ∘ [α ≔ β]$. For more than one field, the binding is no longer forced. $(\${α}: 𝓫 | m: 𝓫) scripts(≐)_r (m: 𝓫 | \${β}: 𝓫)$ is solved by $[α ≔ l, β ≔ l]$ for every label l, but not by $[α ≔ β]$, since m cannot cross the barrier, and these unifiers have no common generalization. U-key-pin is therefore restricted to single fields, and keyed fields under different unknown keys stay stuck. The metatheory of @unification-metatheory holds as stated: success is most general, clash and occurs are sound, and the algorithm terminates. U-key-pin does not shrink the problem, but it always binds a variable, so the residual has fewer variables.

=== Inference

#let fc_infer = figure(
  caption: [Algorithmic rules for first-class labels, extending @inference-rules. A-sel-dyn-⊥ and A-sel-dyn-★ share the premises of A-sel-dyn.],
  flexbox(
    derive("A-lab", (), $Γ; S ⊢ \"l\" ⇒ ⌊l⌋; S$),
    derive(
      "A-sel-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) "fresh" ρ: "Row" #h(2em) S₁ ⊢ τ₁ ≐ {ρ} #st_solve S₂ #h(2em) Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃$,
        $"fresh" ℓ: "Label" #h(2em) S₃ ⊢ τ₂ ≐ ⌊ℓ⌋ #st_solve S₄ #h(2em) (⟦S₄⟧ρ).(⟦S₄⟧ℓ) ↓ τ′$,
      ),
      $Γ; S ⊢ e₁.\${e₂} ⇒ τ′; S₄$,
    ),
    derive(
      "A-sel-dyn-⊥",
      ($…$, $(⟦S₄⟧ρ).(⟦S₄⟧ℓ) ↓ ⊥$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ ★; S₄$,
    ),
    derive(
      "A-sel-dyn-★",
      ($…$, $(⟦S₄⟧ρ).(⟦S₄⟧ℓ) ↓ #h(0.2em) ? "on" α$, $"fresh" δ: "Type"$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ δ; S₄ #st_park ⟨α ⊳ ρ.ℓ ↓ δ⟩$,
    ),
    derive(
      "A-rcd-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$,
        $"fresh" ℓ: "Label" #h(2em) S₂ ⊢ τ₁ ≐ ⌊ℓ⌋ #st_solve S₃$,
      ),
      $Γ; S ⊢ { \${e₁} = e₂ } ⇒ { \${ℓ}: τ₂ }; S₃$,
    ),
    derive(
      "A-sel-dyn-𝓫",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) "fresh" ρ: "Row" #h(2em) S₁ ⊢ τ₁ ≐ {ρ} #st_solve S₂$,
        $Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃ #h(2em) ⟦S₃⟧τ₂ = 𝓫$,
      ),
      $Γ; S ⊢ e₁.\${e₂} ⇒ ★; S₃$,
    ),
    derive(
      "A-rcd-dyn-𝓫",
      ($Γ; S ⊢ e₁ ⇒ τ₁; S₁$, $Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$, $⟦S₂⟧τ₁ = 𝓫$),
      $Γ; S ⊢ { \${e₁} = e₂ } ⇒ ★; S₂$,
    ),
  ),
)
#fc_infer <fc-inference>

@fc-inference gives the algorithmic rules. Dynamic selection follows A-sel, A-sel-⊥ and A-sel-★ of @inference-rules. The key is unified with the singleton type of a fresh label variable ℓ, and the keyed lookup is asked about the row and the key as solved so far. When it answers $?$, the stump is parked on its blocker, which may now be a label variable: in `a: {l = c; m = {}}.${a}` the stump waits on the key and not on a row. A-rcd-dyn performs no lookup and parks nothing. It is shaped like A-λ-E: the key is unified with $⌊ℓ⌋$ for a fresh ℓ, and the keyed field records ℓ.

Wake-up and saturation are unchanged over keyed lookups, so solving a key wakes the stumps blocked on it: applying the selector above to $\"l\"$ solves ℓ, and the stump hits. A-sel-dyn-𝓫 and A-rcd-dyn-𝓫 apply when the key's type is already a base type under the current solution. They answer ★ instead of unifying 𝓫 with $⌊ℓ⌋$. The test is deliberately restricted to a definite base type. A key whose type is still a variable is unified with $⌊ℓ⌋$ as before, so in `(a: {l = c}.${a}) c` the key becomes a label at the selection and the application clashes. Answering ★ for every lookup keyed by such a variable once the conflict appears would be sound by the same rules, but the result would depend on the order in which the two uses of the key are met.

Finalization needs no new premise. F-★ solves the promised δ with ★ and never binds a label variable, so the blockers of other stumps survive it untouched and no key ever becomes ★. A-let generalizes keyed stumps under the side conditions of @generalization, read over rows, keys and results. A spent promise is materialized only when its key is a literal label and it is blocked on its row. A spent promise blocked on its key has no row to extend, and it remains unanswered (@incompleteness-inference).

Concatenation and computed labels interact as NixLang programmers expect. In `((n: r: r // { ${n} = c; }) "x" { x = {}; }).x`, the keyed field is placed in front of the row of r by T-conc, the application solves the key to x, and the selection yields $𝓫_c$: the computed field shadows the older one.

=== Costs

The extension has three costs. Labels and strings are separate: a label literal has the rigid type $⌊l⌋$ and cannot be used as a string, and a value used both as a key and as a string clashes. A key of base type is typed at ★ only when its type is known at the selection; a key that becomes a string later, as in `(a: {l = c}.${a}) c`, is rejected by U-clash. Keyed fields are barriers, so beyond the single-field case of U-key-pin a keyed field against a literal field and two keyed fields under different unknown keys are stuck. A spent promise blocked on its key is not materialized, which leaves inference incomplete on programs the declarative system types. The first cost follows from giving labels singleton types, the second from the absence of negative information about keys, and the third from the absence of top-level qualified types.

== Lacks-Predicates
todo

== Patterns, `with` and `inherit`
todo

== Occurrence Typing
todo
