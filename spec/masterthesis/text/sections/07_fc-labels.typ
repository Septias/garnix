#import "../functions.typ": *
#import "04_unification.typ": u_stuck

= First-Class Labels <fc-labels>

In NixLang, the expression `x.${e}` selects the field named by an arbitrary string expression e, and the expression `{ ${e} = v; }` binds a value to the label that e computes. Labels are thus first-class values. This section extends MiniNix with first-class labels and carries the treatment of row-variables over: A field under an unknown label behaves just like a row-variable. Shadow behaviour is unclear, a lookup cannot pass it and a selection has to park. We put both row and label variables under the term _barrier_ and the same mechanism of qualified schemes and parking translate.

The extension is small: its only new judgement is a key comparison, and row unification gains one move. The remaining rules are new cases for keys and labels or minor adjustments of existing rules, mostly replacing the explicit label by a _key_.

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

@fc-syntax gives the syntax of the extension. The term $\"l\"$ is a string literal and a value. Records can be indexed and extended by arbitrary expressions, as the introduction showed. We abstract over labels by _keys_. A key $k$ is a label $l$, a label variable $α$ of sort Label, or the _unknown key_ ∗. The unknown key stands for a string whose content the analysis does not track, for example the result of a builtin that computes a string.

Keys occur in two places: in the singleton type $⌊k⌋$, which describes the key $k$, and in the _keyed field_ $\${k}: τ$ of a row, whose label may be unknown. In NixLang, every string is a label, and MiniNix follows this: a tracked string has type $⌊l⌋$, and an untracked string has type $⌊∗⌋$. Both can index a record, but only the first can find a field. Stumps also abstract over keys, as in $⟨ρ.k ↓ δ⟩$, so that a scheme can defer a lookup under a key.

We introduce this extension by examples from the introduction and @app-fc-labels gives the full set of rules.

== A Three-Valued Comparison

#let fc_lookup = figure(
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

@fc-lookup gives the only new judgement. The three outcomes of the key comparison are the three verdicts of the lookup relation of @row-lookup: two literals are equal or apart, and every other pair is undecided. A label variable is undecided even against itself. Stability forces this choice: an instance may send α to ∗, and ∗ is undecided against itself, since two strings of unknown content need not be equal.

An undecided comparison answers $?$ exactly as a row-variable does, and a keyed field under a non-literal key behaves like a row-variable. A field in front of $\${α}: τ$ may carry the label that α becomes, so swapping the two fields would change which of them a lookup finds. Spines and segments of @trace-monoid carry over with barriers in place of row-variables, and so does @thm-char. Only the variable that blocks a lookup changes: the _blocker_ of a $?$ may now be a label variable.

== Selection under a Key

The dynamic selector is the program `x: x.l` with its label abstracted:

$
  #raw("a: x: x.${a}") quad : quad ∀(α: "Label", ρ: "Row", δ: "Type"). #h(0.3em) ⟨ρ.α ↓ δ⟩ ⇒ ⌊α⌋ → {ρ} → δ
$

The scheme defers the lookup until both the key and the row are known. A use replays the stump, so one binding answers differently at different uses:

$
  #raw("let g = (a: x: x.${a}) in {p = g \"l\" {l = c} | q = g \"m\" {l = c}}") quad : quad {p: 𝓫 | q: ★}
$

The first use hits and the second one misses, which is the behaviour of the selector in @sec-overview. The verdict now depends on the key as well as on the record.

Stumps also chain under a key. The function `system: pkgs: pkgs.${system}.default` is the common access pattern of a Nix flake and has the scheme

$
  ∀(κ: "Label", ρ₁, ρ₂: "Row", δ: "Type"). #h(0.3em) ⟨ρ₁.κ ↓ {ρ₂}⟩, ⟨ρ₂."default" ↓ δ⟩ ⇒ ⌊κ⌋ → {ρ₁} → δ.
$

The first stump is spent, since its result is selected from. Applied to `"x86"` and `{x86 = {default = c}}`, both stumps hit, and the application has type 𝓫.

== The Wand Ambiguity under a Key

Concatenation with a computed field combines the Wand ambiguity with first-class labels. The function `n: r: (r ‖ {${n} = c}).l` overrides a field under an unknown name and then reads l. Given $n: ⌊κ⌋$ and $r: {ρ}$, the concatenation has type ${\${κ}: 𝓫 | ρ}$. The lookup of l stops at the barrier and answers $?$, since κ may become l. The answer is recorded like every other pending lookup:

$
  ∀(κ: "Label", ρ: "Row", δ: "Type"). #h(0.3em) ⟨(\${κ}: 𝓫 | ρ).l ↓ δ⟩ ⇒ ⌊κ⌋ → {ρ} → δ
$

The structure is the one of the scheme in @sec-overview, with κ in place of the row-variable β. @fc-instances lists its uses.

#figure(
  caption: [Instances of `n: r: (r ‖ {${n} = c}).l`.],
  table(
    columns: (auto, auto, auto),
    align: left,
    stroke: none,
    table.hline(),
    [*Arguments*], [*Type*], [*Lookup*],
    table.hline(),
    [`"l"`], [${ρ} → 𝓫$], [hits the keyed field],
    [`"m"`], [$⟨(m: 𝓫 | ρ).l ↓ δ⟩ ⇒ {ρ} → δ$], [$?$, now blocked on ρ],
    [`"m"`, `{l = {}}`], [${ε}$], [hits the field of r],
    [`"m"`, `{}`], [★], [misses],
    [`toString 1`, `{l = {}}`], [★], [$?$ at every instance],
    table.hline(),
  ),
) <fc-instances>

The instance at `"l"` no longer depends on the row: the keyed field shadows every field of r. The instance at `"m"` shows the one new behaviour. The keyed field becomes a literal field, the comparison with l is apart, and the search continues to ρ. The blocker has migrated from a label variable to a row-variable, and the stump stays pending under the new blocker. Wake-up and generalization read a stump through its blocker, whatever its sort. The two last lines of @fc-instances differ in their reason: the first answers ★ because the field is definitely absent, and the second because the key is unknown.

#let example = [
  == The Unknown Key <sec-fc-unknown>

  A comparison of ∗ with a literal or with ∗ is undecided at every instance, since ∗ is no variable and no substitution replaces it. A lookup that stops at such a comparison is _opaque_. An opaque lookup has no blocker and nothing can answer it later, so inference answers it like a miss. The time program of the introduction is the smallest example:

  $
    #raw("{\"1759190400\" = 1}.${toString builtins.currentTime}") quad : quad ★
  $

  The program types at ★ with a warning and evaluates as in NixLang. This is the role of ★ in G3: the analysis gives up on the selection, and the program is not rejected. ★ has no eliminators, so a further selection from the result clashes.

  A barrier under ∗ hides only what lies behind it. Scoped labels keep the order of the fields, so a lookup that hits in front of the barrier stays definite:

  $
    #raw("({${toString 1} = 1} ‖ {l = \"x\"}).l") & quad : quad ⌊x⌋ \
    #raw("({l = \"x\"} ‖ {${toString 1} = 1}).l") & quad : quad ★
  $

  In the first program the literal field is the right operand and shadows the computed field at every instance. In the second program the computed field may shadow l, and its name is never known. Both answers are faithful to the semantics of `//`.
]

== Unification

Row unification needs one new move. A one-field row is equivalent to another one-field row only under the same key, so every unifier identifies the two keys. U-key-pin binds the key variable accordingly:

$
  #raw("f: a: {p = f {${a} = c} | q = f {l = c}}") \
  : quad ({l: 𝓫} → δ) → ⌊l⌋ → {p: δ | q: δ}
$

The two calls unify $(\${α}: 𝓫)$ with $(l: 𝓫)$, and the key of a is pinned to l. Beyond a single field the binding is no longer forced. The two calls of

$
  #raw("f: a: b: {p = f ({m = c} ‖ {${a} = c}) | q = f ({${b} = c} ‖ {m = c})}")
$

unify $(\${α}: 𝓫 | m: 𝓫)$ with $(m: 𝓫 | \${β}: 𝓫)$. Every label l yields the unifier $[α ≔ l, β ≔ l]$, while $[α ≔ β]$ is no unifier, since m cannot cross the barrier. These unifiers have no common generalization, and the problem is reported as #u_stuck. The problem has the shape of the two-sided problem of @incompleteness-irreducible, with keyed fields as the barriers on opposite ends.

== Metatheory

Progress up to lookup errors, preservation, the unification theorems of @unification-metatheory, and soundness and termination of inference all hold for MiniNix with first-class labels (`qProgress`, `qPreservation`, `inferSound`, `runF_terminates`). A value of type $⌊k⌋$ is a label literal, so a well-typed selection never meets a key that is not a label, and the only error of dynamic selection is a missing label. @app-fc-labels states the lemmas on keyed lookup and row equivalence and the argument for U-key-pin.

== Costs

The extension has costs. First, unknown keys have no identity. In `a: {${a} = c}.${a}`, the lookup compares α with itself and stays undecided, so the program answers $∀(α: "Label", δ: "Type"). ⟨(\${α}: 𝓫_c).α ↓ δ⟩ ⇒ ⌊α⌋ → δ$. The stump hits at every literal instance but not at ∗, and a string of unknown content selects nothing, not even from the record it has built. Identity is the price of stability, since $α ≡ α$ would become $∗ ≟ ∗$ at the instance $[α ≔ ∗]$.

Second, two different literals meet only by T-lab-blur, which inference does not apply. `f: {a = f "x" | b = f "y"}` clashes, although the declarative system types it at $(⌊∗⌋ → 𝓫) → {a: 𝓫 | b: 𝓫}$. This is the incompleteness of T-★-intro (@incompleteness-inference) at the level of keys.

// the following two are too advanced:
// Third, keyed fields are barriers. Beyond the single-field case of U-key-pin, a keyed field against a literal field and two keyed fields under different keys are stuck, and a lookup stops at every keyed barrier. Both follow from the absence of negative information about keys: the knowledge $α ≠ l$ would let $l: τ$ cross $\${α}: τ′$.

// Third, the unknown key is coarse. A key that is one of several literals, such as the platform in a conditional, is typed at $⌊∗⌋$, and every lookup under it is opaque. An opaque selection answers ★, and ★ has no elimination form, so a selection from its result is rejected. A type for finite sets of labels between $⌊l⌋$ and $⌊∗⌋$ would let such lookups decide when every label of the set agrees.
