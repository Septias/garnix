#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= First-Class Labels <fc-labels>

In NixLang, the expression `x.${e}` selects the field named by an arbitrary string expression `e`, and the expression `{ ${e} = v; }` binds a value to the label that `e` computes. Labels are thus first-class values. This section extends MiniNix with first-class labels and carries over the techniques developed in earlier chapters. A field under a non-literal key behaves just like a row-variable: shadow behaviour is unclear, a lookup cannot pass it and a selection has to park. We generalize both row and label-variables to _barriers_ and the same mechanism of qualified schemes and parking translates.

The extension is only small: Lookup gains three new rules for keyed fields, and row unification gains one new move. The remaining changes are new cases for keys and labels or minor adjustments of existing rules, mostly replacing the explicit label by a _key_.

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

The syntax extension is given in @fc-syntax. Expressions gain a new constructor for string literals and records the two expressions to select a dynamic field or bind a value to a computed name that were shown in the introduction. A key can be one of three things: A concrete label $l$, a label variable $α$ of the now sort `Label` or the _unknown key_ $∗$. The unknown key is needed for strings the analysis can not track, for example the result of a builtin. A key can occur in two places: as the singleton type $⌊k⌋$ that describes a label directly or as part of _keyed field_ $\${k}: τ$ of a row, whose label may not be known. Stumps also abstract over keys, as in $⟨ρ.k ↓ δ⟩$, so that a scheme can defer a lookup under a key.

== Keyed Lookup

#let fc_lookup = figure(
  caption: [Keyed lookup. ≈-dlab identifies a literal field $l: τ$ with $\${l}: τ$.],
  flexbox(
    derive("L-hit", (), $(\${l}: τ).l ↓ τ$),
    derive("L-miss", ($l ≠ l′$,), $(\${l′}: τ).l ↓ ⊥$),
    derive(
      "L-?",
      ($k "or" k′ "not a literal"$,),
      $(\${k′}: τ).k ↓ #h(0.2em) ?$,
    ),
  ),
)
#fc_lookup <fc-lookup>

@fc-lookup gives the lookup under a key. Its three rules correspond to the three verdicts of the lookup relation of @row-lookup. Two concrete literals compare equal or apart but every other pair is undecided. Even a label variable is undecided against itself because it may be sent to $∗$ and two unknown keys can describe completely different strings. Spines and segments of @trace-monoid carry over with barriers in place of row-variables, and so does @thm-char. Only the variable that blocks a lookup changes: the _blocker_ of a $?$ may now be a label variable as well.


== First-Class Labels by Example
We continue introducing the extension through examples. The full set of rules, including row equivalence, the typing and reduction rules, key unification and the algorithmic rules, is given in @app-fc-labels. The following three examples show how first-class labels allow us to type the generic selector, how first-class labels act as barriers in the Wand ambiguity and the effect of untracked strings, typed at ⌊∗⌋.

The dynamic selector is the program `x: x.l` with its label abstracted to a label-variable. It takes a label and a record as arguments and returns the named field from the given record. Our system types it as:

$
  #raw("a: x: x.${a}") quad : quad ∀(α: "Label", β: "Row", δ: "Type"). #h(0.3em) ⟨β.α ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ
$

The scheme is the most general type of the program up to T-★-intro. The label, the row and the result are all variables, and the stump is the only constraint that relates them. We also show an example of how stumps can be chained. The following program selects from the result of a keyed selection:
$
  #raw("a: x: x.${a}.l") quad : quad & ∀(α: "Label", β, γ: "Row", δ: "Type"). \
  & quad ⟨β.α ↓ {γ}⟩, ⟨γ.l ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ
$

For `x.${a}`, the lookup of α in β answers $?$ on β, so A-sel-\? parks $⟨β ⊳ β.α ↓ δ′⟩$ and returns δ′. The selection of l then unifies δ′ with ${γ}$ for a fresh γ, which spends the first stump, and parks $⟨γ ⊳ γ.l ↓ δ⟩$ on the row of its result.

Next, we look at the Wand ambiguity with first-class labels: the function `n: r: (r ‖ {${n} = c}).l` overrides a field under a computed name and then reads l. Given $n: ⌊α⌋$ and $r: {β}$, the concatenation has type ${\${α}: 𝓫_c | β}$. The lookup of l stops at the barrier and answers $?$, since α may become l. The answer is recorded like every other pending lookup:

$
  ∀(α: "Label", β: "Row", δ: "Type"). #h(0.3em) ⟨(\${α}: 𝓫_c | β).l ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ
$

In this example, a label variable becomes the blocker. Wake-up and generalization read a stump through its blocker, whatever its sort.

// todo: reword
A comparison of $∗$ with a literal or with $∗$ is undecided at every instance because $∗$ is no variable and no substitution replaces it. A lookup with such a label is not tracked by our system so the only option is to type it using the unknown type. The time program of the introduction types exactly like this because the `toString` builtin is untrackable:

$
  #raw("{\"1759190400\" = 1}.${toString builtins.currentTime}") quad : quad ★
$

This is the role of ★ in G3: the analysis gives up on the selection, and the program is not rejected. ★ has no eliminators, so a further selection from the result clashes.

== Unification

Row unification needs one new move. A one-field row is equivalent to another one-field row only under the same key, so every unifier identifies the two keys. U-key-pin binds the key variable accordingly:

#align(center, derive(
  "U-key-pin",
  ($k ≠ α$, $[α ≔ k]τ ≐ [α ≔ k]τ′ ⇝ θ$),
  $⟨\${α}: τ⟩ scripts(≐)_r ⟨\${k}: τ′⟩ ⇝ θ ∘ [α ≔ k]$,
))


== Metatheory

Progress up to lookup errors, preservation, the unification theorems of @unification-metatheory, and soundness and termination of inference all hold for MiniNix with first-class labels (`qProgress`, `qPreservation`, `inferSound`, `runF_terminates`). A value of type $⌊k⌋$ is a label literal, so a well-typed selection never meets a key that is not a label, and the only error of dynamic selection is a missing label. @app-fc-labels states the lemmas on keyed lookup and row equivalence and the argument for U-key-pin.
