#import "../functions.typ": *
#import "04_unification.typ": u_clash
#import "05_inference.typ": st_park, st_solve

= First-Class Labels <fc-labels>

In NixLang, the expression `x.${e}` selects the field named by an arbitrary string expression e, and the expression `{ ${e} = v; }` likewise binds a value to a label that is the result of an expression $e$. Labels are thus first-class inhabitants of the language and they fit naturally in our system: A label can becom a variable in a row and and shadows similarly to a row-variable. In fact, we generalize both into _barriers_ after which shadowing is unclear. The unknown type extends similarly: If two variables cannot be decided to be equal or not, the unknown type can be used to type the expression. Both deferred lookup and qualified schemes naturally translate in this manner.


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
                #type_name("Key") k & ::= l | α | ∗ \
               #type_name("Type") τ & ::= … | ⌊k⌋ \
                #type_name("Row") ρ & ::= … | \${k}: τ \
        #type_name("Constraints") Q & ::= ε | Q, ⟨ρ.k ↓ δ⟩ \
      $
    ],
  )),
)
#fc_syntax <fc-syntax>

@fc-syntax gives the syntax extension for first-class labels. The term $\"l\"$ is a string literal and a value. Records can be indexed and extended by arbitrary expressions, just like the introduction showed. We abstract over labels and call the result a _key_. A key $k$ is a label $l$, a label variable $α$ (of label-sort), or the _unknown key_ ∗. The unknown key stands for a string whose content the analysis does not track, for example the result of a builtin that computes a string.

Keys occur in two places: in the singleton type $⌊k⌋$ that describes the key $k$ and in the _keyed field_ $\${k}: τ$ of a row whose label is not known yet. In nix, every string is a label and we do the same: A trackable string is of type $⌊l⌋$ and an untrackable one of $⌊∗⌋$. The unknown key $⌊∗⌋$ is a label type and can be used as a key [but will be turned into ★]?. Stumps also abstract over keys $⟨ρ.k ↓ δ⟩$, so that a scheme can defer a lookup under a key.

== Keyed Lookup

#let fc_lookup = figure(
  // why is this comment here instead of in the text?
  caption: [Key comparison and keyed lookup. A literal field $l: τ$ is read as $\${l}: τ$.],
  stack(
    spacing: 15pt,
    align(
      center,
      $#type_name("Key comparison") l ≡ l quad l class("relation", \#) l′ "if" l ≠ l′ quad k ≟ k′ "otherwise"$,
    ),
    flexbox(
      derive("L-hit", ($k ≡ k′$,), $(\${k′}: τ).k ↓ τ$),
      derive("L-miss", ($k class("relation", \#) k′$,), $(\${k′}: τ).k ↓ ⊥$),
      derive("L-?", ($k ≟ k′$,), $(\${k′}: τ).k ↓ #h(0.2em) ?$),
    ),
  ),
)
#fc_lookup <fc-lookup>

// three new words for what?
@fc-lookup generalizes the lookup relation from labels to keys. The key comparison has three outcomes. Two keys are _equal_, $k ≡ k′$, when they are the same literal, and _apart_, $k class("relation", \#) k′$, when they are different literals. Every other pair is _undecided_, $k ≟ k′$. On two literals, L-hit and L-miss are the rules of @row-lookup, and L-\? is a new source of ?. A label variable is undecided even against itself. Stability forces this choice: an instance may send α to ∗, and ∗ is undecided against itself, since two strings of unknown content need not be equal. Only two literals decide a comparison, and no substitution changes a literal, so the keyed lookup keeps the properties of @lookup-metatheory. The rules L-ε, L-var and the three concatenation rules of @row-lookup carry over with k in place of l.

#lemma(
  name: [Keyed lookup],
  lean: "LookupQ.det, LookupQ.total, LookupQ.applySubst",
)[
  For every row ρ and key k there is exactly one lookup result r with $ρ.k ↓ r$. If $r ≠ #h(0.2em) ?$, then $(θ ρ).(θ k) ↓ θ r$ for every substitution θ.
] <lem-keyed-lookup>

#proof[As for @lem-lookup-det and @lem-lookup-stable. A definite derivation compares only literals, and θ fixes every literal.]

An undecided comparison blocks the lookup, and the variable that blocks it is no longer always a row-variable. The lookup $(m: 𝓫).α ↓ #h(0.2em) ?$ is blocked on the key α, which may become m. The lookup $(\${γ}: 𝓫 | l: 𝓫′).l ↓ #h(0.2em) ?$ is blocked on γ: the field $l: 𝓫′$ behind the keyed field is out of reach until γ is known, since γ may become l. The _blocker_ of a $?$ is the row-variable at which the search stopped or, at an undecided comparison, the lookup key if it is a variable and the key of the field otherwise.

A comparison of ∗ with a literal or with ∗ is undecided at every instance, since ∗ is no variable and no substitution replaces it. A lookup that stops at such a comparison is _opaque_. The lookups $(l: 𝓫).class("normal", ∗) ↓ #h(0.2em) ?$ and $(\${∗}: 𝓫 | l: 𝓫′).l ↓ #h(0.2em) ?$ are opaque and stay $?$ under every substitution (`LookupOpaqueQ.applySubst` in Lean). An opaque lookup has no blocker, and inference answers it like a miss (@sec-fc-inference).


== Row Equivalence and Barriers

#let fc_equiv = figure(
  caption: [Row equivalence for keyed fields, extending @row-equivalence.],
  flexbox(
    derive("≈-dfield", ($τ₁ ≈ τ₂$,), $(\${k}: τ₁) ≈ (\${k}: τ₂)$),
    derive("≈-dlab", (), $(\${l}: τ) ≈ (l: τ)$),
  ),
)
#fc_equiv <fc-equiv>

Row equivalence extends to keyed fields by two rules (@fc-equiv). ≈-dfield is congruence under one key, and ≈-dlab identifies a keyed field under a literal with the literal field. ≈-comm needs no extension. Its premise $l₁ ≠ l₂$ is apartness, and only literals are apart, so two keyed fields under apart keys swap by ≈-dlab and ≈-comm (`RowEquiv.comm_keys`). A keyed field under a label variable or under ∗ commutes with nothing: a field in front of $\${α}: τ$ may carry the label that α becomes, and swapping the two fields would change which of them a lookup finds. Such a keyed field behaves like a row-variable, and we call both a _barrier_.

The trace monoid of @trace-monoid carries over with barriers in place of row-variables. Spines acquire a third atom, $a ::= l: τ | α | \${k}: τ$ with k not a literal, since a keyed field under a literal is normalized to a literal field. A segment is a barrier-free part of a spine, and @thm-char reads: $ρ₁ ≈ ρ₂$ if and only if both rows have the same sequence of barriers, where two keyed barriers are the same when their keys are equal and their field types are equivalent, and the fields of every label agree pointwise up to ≈ in every segment (`rowEquiv_iff_char`). The monoid remains cancellative on both sides (@lem-cancel).


== Typing and Evaluation

#let fc_typing = figure(
  caption: "Typing rules and reduction for first-class labels.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Typing")[
      #flexbox(
        derive("T-lab", (), $Γ ⊢ \"l\": ⌊l⌋$),
        derive("T-lab-blur", ($Γ ⊢ e: ⌊k⌋$,), $Γ ⊢ e: ⌊∗⌋$),
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
        derive("T-ξ-blur", ($Γ ⊢ e: τ$,), $Γ ⊢ (l = e): (\${∗}: τ)$),
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
      )
    ],
  )),
)
#fc_typing <fc-typing>

@fc-typing gives the typing rules and the reduction. The three rules for dynamic selection are those of static selection with the label replaced by the key of e₂'s type, and static selection is their special case at a literal key. T-lab types $\"l\"$ at $⌊l⌋$, and the keyed lookup under a literal is the lookup of @row-lookup (`LookupQ.lab_iff`). T-sel-dyn, T-sel-dyn-★ and T-sel-dyn-⊥ applied to $e.\${\"l\"}$ thus have the premises and conclusions of T-sel, T-sel-★ and T-sel-⊥ applied to $e.l$. T-rcd-dyn records the key of e₁ in a keyed field.

T-lab-blur and T-ξ-blur forget information, like T-★-intro. T-lab-blur forgets the content of a string, and T-ξ-blur forgets the label of a field. Both conclude at a fixed form, $⌊∗⌋$ and $\${∗}: τ$, and nothing reads through ∗: a lookup that meets $\${∗}: τ$ is opaque, so a blurred field is never hit and never missed. The two rules make strings usable. T-lab-blur lets a literal stand where an arbitrary string is expected, for example as the argument of a function of type $⌊∗⌋ → τ$, and it types the literal that a builtin computes at $⌊∗⌋$ again. T-ξ-blur keeps E-rcd-dyn type-preserving. E-rcd-dyn steps ${ \${\"l\"} = e }$ to the literal record ${ l = e }$, and when the key has type $⌊∗⌋$, the redex has type ${\${∗}: τ}$, which T-ξ-blur gives to the literal record as well. The inference rules of MiniNix never blur.

The evaluation contexts evaluate the record before the key, and E-sel-dyn then selects the leftmost binding of the label, as E-sel of @semantics does for a static one. A dynamic record evaluates its key but leaves the field unevaluated, as a record literal does. Progress up to ↯ and preservation (@thm-progress, @thm-preservation) hold for MiniNix with first-class labels. A key needs no error of its own. A value of type $⌊k⌋$ is a label literal (`qcanonical_lab`), so a well-typed selection never meets a key that is not a label, and the only error of dynamic selection is a missing label, as for static selection. A term such as `{}.${c}` has no typing, since c has a base type and no rule types a key of base type. NixLang reports an error for such a key at run time.

Qualified schemes abstract over keys as they do over rows. Discharge (@instantiation) replays a keyed stump with the keyed lookup: θ is applied to both the row and the key, $(θ ρ).(θ k) ↓ r$, and the verdict pins θδ exactly as for a static stump. An opaque verdict is a $?$, so D-\? sends θδ to ★. The dynamic selector and the dynamic record constructor have the schemes

$
  #raw("a: x: x.${a}") & quad : quad ∀(α: "Label", ρ: "Row", δ: "Type"). #h(0.3em) ⟨ρ.α ↓ δ⟩ ⇒ ⌊α⌋ → {ρ} → δ \
  #raw("a: v: { ${a} = v; }") & quad : quad ∀(α: "Label", δ: "Type"). #h(0.3em) ⌊α⌋ → δ → { \${α}: δ }
$

and both are instance-closed. The first scheme defers the lookup until both the key and the row are known, so that one binding answers a definite type at a use whose record has the field and ★ at a use whose record lacks it.

== Unification

#let fc_unif = figure(
  caption: [Unification of keys and of keyed fields, extending @unification-ty and @unification-cascade.],
  flexbox(
    derive("U-lab", ($l = l′$,), $⌊l⌋ ≐ ⌊l′⌋ ⇝ ∅$),
    derive("U-lab-clash", ($l ≠ l′$,), $⌊l⌋ ≐ ⌊l′⌋ ⇝ #u_clash$),
    derive("U-key-∗", (), $⌊∗⌋ ≐ ⌊∗⌋ ⇝ ∅$),
    derive("U-key-∗-clash", (), $⌊∗⌋ ≐ ⌊l⌋ ⇝ #u_clash$),
    derive("U-key-bind", (), $⌊α⌋ ≐ ⌊k⌋ ⇝ [α ≔ k]$),
    derive(
      "U-key-pin",
      ($"key"(f) = k ≠ α$, $[α ≔ k]⟨\${α}: τ⟩ scripts(≐)_r [α ≔ k]⟨f⟩ ⇝ θ$),
      $⟨\${α}: τ⟩ scripts(≐)_r ⟨f⟩ ⇝ θ ∘ [α ≔ k]$,
    ),
  ),
)
#fc_unif <fc-unification>

Key unification is flat (@fc-unification). Two literals unify when they are equal and clash otherwise, as two base types do. The unknown key behaves as one more constant: it unifies with itself and clashes with every literal. The clash is sound, since no substitution makes ∗ a label. A label variable binds to any key, including ∗ and another label variable. A key has no subterms, so U-key-bind needs no occurs check, and its instance $k = α$ is the identity $[α ≔ α] = ∅$. A singleton type against any other head clashes by U-clash, and so does $⌊∗⌋$ against ★. U-key-∗-clash and U-key-bind are used in both directions.

On rows, every move of @unification-cascade that asks whether a side is free of row-variables asks whether it is free of barriers. U-ground and U-row-clash count literal fields and require the other side to be barrier-free, so $(l: τ) scripts(≐)_r (\${α}: τ′)$ is not a clash: $[α ≔ l]$ solves it. U-ε-clash fires on a keyed field as on any field, since the empty spine has no field under any key. U-host requires the host side to be free of keyed fields, since the key of such a field may become the hosted label. U-field-L and U-field-R pair a leading or trailing field with a field in the window of the other side, and a window ends at the first barrier. A keyed barrier pairs only with a keyed field under the same key at the same end of the other side, since no field may cross it, and the move unifies their types. An occurrence of a variable next to a keyed field is an occurs failure, as next to a literal field: the proof of U-var-occurs counts all field atoms, whatever their key (`occurs_anyField_no_unifier`).

U-key-pin is the one new move. It applies when each side consists of a single field, at least one of them keyed, under different keys. A one-field row is equivalent to another only under the same key, so every unifier identifies the two keys. The move binds the variable key to the other key and runs the problem again, where U-field-L then pairs the fields. Here f is a single field, key(f) is its label or its key, and U-key-pin is tried in both directions. It solves $⟨l: τ⟩ scripts(≐)_r ⟨\${α}: τ′⟩$ with $[α ≔ l]$ and $⟨\${α}: τ⟩ scripts(≐)_r ⟨\${β}: τ′⟩$ with $[α ≔ β]$, which is most general since $[α ≔ l, β ≔ l] = [β ≔ l] ∘ [α ≔ β]$. Against $\${∗}: τ′$, it binds α to ∗.

For more than one field, the binding is no longer forced. $(\${α}: 𝓫 | m: 𝓫) scripts(≐)_r (m: 𝓫 | \${β}: 𝓫)$ is solved by $[α ≔ l, β ≔ l]$ for every label l, but not by $[α ≔ β]$, since m cannot cross the barrier, and these unifiers have no common generalization. U-key-pin is therefore restricted to single fields, and keyed fields under different keys stay stuck. A literal field against a field under ∗, $⟨l: τ⟩ scripts(≐)_r ⟨\${∗}: τ′⟩$, has no unifier, but no move detects this, and the verdict is stuck. The metatheory of @unification-metatheory holds as stated: success is most general, clash and occurs are sound, and the algorithm terminates. U-key-pin does not shrink the problem, but it always binds a variable, so the residual has fewer variables.

== Inference <sec-fc-inference>

#let fc_infer = figure(
  caption: [Algorithmic rules for first-class labels, extending @inference-rules. A-sel-dyn-⊥ and A-sel-dyn-\? share the premises of A-sel-dyn. K-⊥ of @wakeup also fires on an opaque lookup.],
  flexbox(
    derive("A-lab", (), $Γ; S ⊢ \"l\" ⇒ ⌊l⌋; S$),
    derive(
      "A-sel-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) "fresh" ρ: "Row" #h(2em) S₁ ⊢ τ₁ ≐ {ρ} #st_solve S₂ #h(2em) Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃$,
        $"fresh" β: "Label" #h(2em) S₃ ⊢ τ₂ ≐ ⌊β⌋ #st_solve S₄ #h(2em) (⟦S₄⟧ρ).(⟦S₄⟧β) ↓ τ′$,
      ),
      $Γ; S ⊢ e₁.\${e₂} ⇒ τ′; S₄$,
    ),
    derive(
      "A-sel-dyn-⊥",
      ($…$, $(⟦S₄⟧ρ).(⟦S₄⟧β) ↓ ⊥ "or opaque"$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ ★; S₄$,
    ),
    derive(
      "A-sel-dyn-?",
      ($…$, $(⟦S₄⟧ρ).(⟦S₄⟧β) ↓ #h(0.2em) ? "on" α$, $"fresh" δ: "Type"$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ δ; S₄ #st_park ⟨α ⊳ ρ.β ↓ δ⟩$,
    ),
    derive(
      "A-rcd-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$,
        $"fresh" β: "Label" #h(2em) S₂ ⊢ τ₁ ≐ ⌊β⌋ #st_solve S₃$,
      ),
      $Γ; S ⊢ { \${e₁} = e₂ } ⇒ { \${β}: τ₂ }; S₃$,
    ),
  ),
)
#fc_infer <fc-inference>

@fc-inference gives the algorithmic rules. Dynamic selection follows A-sel, A-sel-⊥ and A-sel-\? of @inference-rules. The key is unified with the singleton type of a fresh label variable β, and the keyed lookup is asked about the row and the key as solved so far. An opaque lookup is answered like a miss: no instance decides it, so A-sel-dyn-⊥ answers ★ at once and parks nothing. When the lookup answers a $?$ that is not opaque, the stump is parked on its blocker, which may now be a label variable: in `a: {l = c; m = {}}.${a}` the stump waits on the key and not on a row. A-rcd-dyn performs no lookup and parks nothing. It is shaped like A-λ-E: the key is unified with $⌊β⌋$ for a fresh β, and the keyed field records β.

Wake-up and saturation are unchanged over keyed lookups. An undecided comparison changes only when its blocker is solved, since the blocker is the only variable whose solution can change it. A stump is therefore stale exactly when its blocker is solved, as for rows. Applying the selector above to $\"l\"$ solves β, and the stump hits. When a key variable is bound to ∗ instead, for example by an argument that a builtin has computed, the stump wakes opaque, and K-⊥ answers ★ with a warning.

The entry point needs no new premise. A keyed stump that is still pending stays a qualifier of the answer. A-let generalizes keyed stumps under the side conditions of @generalization, read over rows, keys and results. The fillability test of @fillability reads a label variable α of the binding as a fresh literal, written `${α}`, and a stump under ∗ is never fillable, since no instance makes its lookup hit. The test therefore gains conditions on keys. Every spent stump and every stump on its blocker has a literal label or a label variable of $macron(α)$ other than the blocker, and its row does not use the blocker as a key. Two stumps on one blocker count as asking the same label when their field names agree. No stump of $Δ_q$ waits on a label variable of a filled blocker. At the entry point, the bounded search also tries, for a spent stump whose key is a variable α, the label `${α}` and then the literal labels of the stump's row. A spent stump under a key variable therefore keeps its qualifier. In `r: a: (r.${a}) c`, the stump is blocked on the row of r and spent on a function, and the program answers $∀(ρ: "Row", α: "Label", δ: "Type"). ⟨ρ.α ↓ 𝓫_c → δ⟩ ⇒ {ρ} → ⌊α⌋ → δ$. `let f = r: a: r.${a} c` generalizes f to the same scheme.

Concatenation and computed labels interact as NixLang programmers expect. In `((n: r: r // { ${n} = c; }) "x" { x = {}; }).x`, the keyed field is placed in front of the row of r by T-conc, the application solves the key to x, and the selection yields $𝓫_c$: the computed field shadows the older one. A field under ∗ shadows as well, but no lookup passes it. A selection of x from `r // { ${s} = c; }` with s of type $⌊∗⌋$ meets $\${∗}$ first and is opaque, so it answers ★.

== Costs

The extension has four costs. First, unknown keys have no identity. In `a: {${a} = c}.${a}`, the lookup compares α with itself and stays undecided, so the program answers $∀(α: "Label", δ: "Type"). ⟨(\${α}: 𝓫_c).α ↓ δ⟩ ⇒ ⌊α⌋ → δ$. The stump hits at every literal instance but not at ∗, and a string of unknown content selects nothing, not even from the record it has built. Identity is the price of stability, since $α ≡ α$ would become $∗ ≟ ∗$ at the instance $[α ≔ ∗]$.

Second, two different literals meet only by T-lab-blur, which inference does not apply. `f: { a = f "x"; b = f "y"; }` clashes, although the declarative system types it at $(⌊∗⌋ → 𝓫) → {a: 𝓫 | b: 𝓫}$. This is the incompleteness of T-★-intro (@incompleteness-inference) at the level of keys.

Third, keyed fields are barriers. Beyond the single-field case of U-key-pin, a keyed field against a literal field and two keyed fields under different keys are stuck, and a lookup stops at every keyed barrier. Both follow from the absence of negative information about keys: the knowledge $α ≠ l$ would let $l: τ$ cross $\${α}: τ′$.

Fourth, the unknown key is coarse. A key that is one of several literals, such as the platform in a conditional, is typed at $⌊∗⌋$, and every lookup under it is opaque. An opaque selection answers ★, and ★ has no elimination form, so a selection from its result is rejected. A type for finite sets of labels between $⌊l⌋$ and $⌊∗⌋$ would let such lookups decide when every label of the set agrees.
