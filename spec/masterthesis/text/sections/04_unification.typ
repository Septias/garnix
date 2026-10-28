#import "../functions.typ": *

#let u_clash = text(font: "Libertinus Serif", smallcaps("Clash"))
#let u_occurs = text(font: "Libertinus Serif", smallcaps("Occurs"))
#let u_stuck = text(font: "Libertinus Serif", smallcaps("Stuck"))

= Unification <unification>

// The paragraphs of this section are unexpected
Next, we present our unification algorithm, based upon the declarative system. It consists of two judgements, one for types: $τ₁ ≐ τ₂ ⇝ v$ and one for rows: $s₁ scripts(≐)_r s₂ ⇝ v$ that call each other mutually recursive. @unification-ty lists the verdicts of unification and shows how spines are formed. A spine s of a row ρ, denoted $s = ⌈ρ⌉$, represents a row as a list of atoms $a ::= l: τ | α$, obtained by flattening the concatenation tree. For example, $((l: 𝓫 | ε) | (α | m: 𝓫))$ and $(l: 𝓫 | (α | (m: 𝓫 | ε)))$ both have the spine $⟨l: 𝓫, α, m: 𝓫⟩$.

A verdict is either a solution θ or one of three refusals. #u_clash is emitted when two structures clash definitionally and #u_occurs when a recursive type is encountered. NixLang allows recursiveness in records and let-bindings, but to keep the presentation simple, we do not permit it in MiniNix. #u_stuck is emitted when no more moves apply and carries the Wand ambiguity.

#let unification_ty = figure(
  caption: "Spines, verdicts and unification of types.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Spines")[
      $
        #type_name("Atom") a ::= l: τ | α #h(3em) #type_name("Spine") s ::= ⟨⟩ | a · s
      $
    ],
    subbox(caption: "Type Unification")[
      #flexbox(
        derive("U-refl", (), $α ≐ α ⇝ ∅$),
        derive("U-bind", ($τ ≠ α$, $α ∉ "ftv"(τ)$), $α ≐ τ ⇝ [α ≔ τ]$),
        derive("U-occurs", ($τ ≠ α$, $α ∈ "ftv"(τ)$), $α ≐ τ ⇝ #u_occurs$),
        derive("U-base", ($𝓫 = 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ ∅$),
        derive("U-base-clash", ($𝓫 ≠ 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ #u_clash$),
        derive("U-clash", ($"head"(τ₁) ≠ "head"(τ₂)$,), $τ₁ ≐ τ₂ ⇝ #u_clash$),
        derive("U-★", (), $★ ≐ ★ ⇝ ∅$),
        derive(
          "U-fn",
          ($τ₁ ≐ τ₁′ ⇝ θ₁$, $θ₁ τ₂ ≐ θ₁ τ₂′ ⇝ θ₂$),
          $(τ₁ -> τ₂) ≐ (τ₁′ -> τ₂′) ⇝ θ₂ ∘ θ₁$,
        ),
        derive("U-rcd", ($⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ θ$,), ${ρ₁} ≐ {ρ₂} ⇝ θ$),
      )
    ],
    subbox(caption: "Verdicts")[
      $
        #type_name("Verdict") v ::= θ | #u_clash | #u_occurs | #u_stuck
      $
    ],
  )),
)
#unification_ty <unification-ty>

@unification-ty shows the type unification rules of the algorithmic system. Every statement is an equation and unification returns tries to return a substition θ that makes both sides equal. U-refl is the trivial base case and U-bind solves a simple equation by substitution, but only when α is not a free type variable of τ. This case is handled by U-occurs to prevent endless cycles. Unifying the same base-type is a no-op (U-base) and different base-types result in a clash by U-base-clash. Two unequal head-level constructors (functions, records, …) also result in a clash. The unknown type is _rigid_ meaning, it does not spread and get absorbed into any other type; it only unifies with itself by U-★. Finally, U-fn unifies functions and U-rcd hands off unification to row unification on spines.

== Row Unification

#let u_band = (
  exhaust: oklch(95%, 0.025, 250deg),
  cancel: oklch(95%, 0.028, 195deg),
  solve: oklch(95%, 0.03, 155deg),
  matchp: oklch(95%, 0.035, 95deg),
  host: oklch(95%, 0.03, 330deg),
  give: oklch(94%, 0.012, 285deg),
)
#let u_ok = text(fill: oklch(48%, 0.11, 155deg), weight: "bold", "✓")
#let u_no = text(fill: oklch(48%, 0.13, 25deg), weight: "bold", "✗")
#let u_go = text(fill: zink_700, weight: "bold", "↻")
#let u_both = text(fill: zink_700, size: 8pt, "⇄")

#let u_phase(name, color, n) = table.cell(
  rowspan: n,
  fill: color,
  inset: (x: 4pt, y: 6pt),
  align: center + horizon,
  std.rotate(-90deg, reflow: true, text(
    size: 7.5pt,
    fill: zink_900,
    tracking: 0.8pt,
    smallcaps(name),
  )),
)

#let u_step(name, both: false, trigger, effect, mark) = (
  table.cell(align: left + horizon, inset: (left: 10pt, right: 8pt, y: 7pt))[
    #text(size: 9pt, fill: zink_900, smallcaps(name))#if both [ #u_both]
  ],
  table.cell(align: left + horizon)[#text(size: 10pt, trigger)],
  table.cell(align: left + horizon)[#text(size: 10pt, effect)],
  table.cell(align: center + horizon)[#mark],
)

// are the reverse arrows correct everywhere?
// two figures for row unification is too redundant
#let unification_cascade = figure(
  kind: image,
  caption: "The order in which the row-moves are tried.",
  box(width: 100%, stack(
    spacing: 12pt,
    align(center, box(
      inset: (x: 14pt, y: 7pt),
      radius: 4pt,
      fill: luma(96%),
      stroke: 0.5pt + luma(70%),
      $s₁ scripts(≐)_r s₂$,
    )),
    align(center, text(size: 14pt, fill: luma(60%), "↓")),
    table(
      columns: (auto, auto, auto, 1fr, auto),
      stroke: (x, y) => (top: if y > 0 { 0.4pt + luma(88%) }),
      inset: (x: 8pt, y: 7pt),
      row-gutter: 0pt,

      u_phase("exhaust", u_band.exhaust, 2),
      ..u_step(
        "U-ε-var",
        [$⟨⟩ scripts(≐)_r s$ with $s$ field-free],
        $[overline(β) ≔ ε]$,
        u_ok,
      ),
      ..u_step(
        "U-ε-clash",
        [$⟨⟩ scripts(≐)_r s$ with a field in $s$],
        u_clash,
        u_no,
      ),

      u_phase("cancel", u_band.cancel, 2),
      ..u_step(
        "U-cancel-L",
        $α · t₁ scripts(≐)_r α · t₂$,
        [cancel, recurse on $t₁, t₂$],
        u_go,
      ),
      ..u_step(
        "U-cancel-R",
        $t₁ · α scripts(≐)_r t₂ · α$,
        [cancel, recurse on $t₁, t₂$],
        u_go,
      ),

      u_phase("solve", u_band.solve, 3),
      ..u_step(
        "U-var-collapse",
        both: true,
        $α scripts(≐)_r s$ + [, ] + $α ∈ "vars"(s)$ + [, $s$ field-free],
        $[overline(β) ≔ ε]$,
        u_ok,
      ),
      ..u_step(
        "U-var-solve",
        both: true,
        $α scripts(≐)_r s$ + [, ] + $α ∉ "rv"(s)$,
        $[α ≔ s]$,
        u_ok,
      ),
      ..u_step(
        "U-var-occurs",
        both: true,
        $α scripts(≐)_r s$ + [, ] + $α ∈ "rv"(s)$,
        u_occurs,
        u_no,
      ),

      u_phase("match", u_band.matchp, 3),
      ..u_step(
        "U-field-L",
        both: true,
        [leading $l: τ$, $l$ in the other side's leading window],
        [emit $τ ≐ τ′$, apply, recurse],
        u_go,
      ),
      ..u_step(
        "U-field-R",
        both: true,
        [trailing $l: τ$, $l$ in the other side's trailing window],
        [emit $τ ≐ τ′$, apply, recurse],
        u_go,
      ),
      ..u_step(
        "U-ground",
        both: true,
        [other side var-free, $|s₁|_l = |s₂|_l > 0$],
        [pair the first $l$'s, emit $τ ≐ τ′$, recurse],
        u_go,
      ),

      u_phase("clash", u_band.give, 1),
      ..u_step(
        "U-row-clash",
        [some $l$ with $|s₁|_l > |s₂|_l$ and $s₂$ var-free],
        u_clash,
        u_no,
      ),

      u_phase("host", u_band.host, 1),
      ..u_step(
        "U-host",
        both: true,
        [leading $l: τ$, the other side has a sole host β],
        [$[β ≔ (l: τ | β′)]$, recurse],
        u_go,
      ),

      u_phase("stuck", u_band.give, 1),
      ..u_step(
        "U-stuck",
        [no move above fires],
        u_stuck + [ (Wand ambiguity)],
        u_no,
      ),
    ),
    align(center, text(
      size: 8.5pt,
      fill: zink_700,
      [#u_both tried with the sides exchanged #h(12pt) #u_go apply and recurse on the
        residual #h(12pt) #u_ok solution #h(12pt) #u_no verdict],
    )),
  )),
)
#unification_cascade <unification-cascade>

The row pass is a deterministic cascade of forced moves, tried one after another. Its rules are given in @unification-cascade which is read top-to-bottom. If no rule applies, stuck is the verdict.

// Row unification preserves the solution set of the problem and never chooses among alternatives. I.e. it never backtracks and stays first-order.

The first two exhaustion rules, U-ε-var and U-ε-clash, handle a problem with an empty side ⟨⟩. If the other side has only variables, U-ε-var sends each of them to ε; if it has a field, U-ε-clash reports a clash. The following cancel moves use the trace-monoid property of rows: A row variable α can be canceled on either side of the problem and the algorithm recurses on the residual problem.

The solving rules fire when one side is a lone variable α. U-var-collapse fires when the same variable is found on the other side, surrounded only by other type variables. This is best explained by an example: The following row ⟨γ, α, β⟩ is solved by sending every type variable but α to ε [γ → ε, β → ε]. Both sides are then equal. A repeated occurrence forces α to ε as well. U-var-solve simply binds α to the other sides value if α is not contained and can form no cycle. $"rv"(α)$ looks for a variable at any depth and not only the spine. Finally, U-var-occurs reports a cycle if it exists.

// explain the trace monoid structure and adimt that it does not really fit Nix' syntax
[The three matching moves differ in how far they may look for a partner. A _window_ is a maximal var-free segment at one end of a spine: U-field-L and U-field-R pair a field only inside it, because a row-variable in between could be instantiated to a colliding field and the pairing would be a guess about shadowing. In $⟨m: 𝓫, l: 𝓫, α, l: 𝓫′⟩$ the leading window is $⟨m: 𝓫, l: 𝓫⟩$ and the trailing window is $⟨l: 𝓫′⟩$. A leading $l: τ$ on the other side is paired with $l: 𝓫$. Against $⟨m: 𝓫, α, l: 𝓫′⟩$ it has no partner in the leading window, and pairing it with $l: 𝓫′$ would assume that α carries no l. U-ground lifts that restriction by counting. A var-free side fixes the number $|s|_l$ of l-fields. If both sides have the same positive number, the variables of the other side cannot carry an l without breaking the count, so the i-th l-field on one side pairs with the i-th on the other, and the pairing is again forced.](todo)

A field facing only variables on the other side has no partner, but it may have a host. U-host fires on a leading field $l: τ$ when the other side has a _sole host_ β for it: β is the first variable of that side, the side has no l-field and no keyed field, every other variable of the side occurs in τ and so cannot carry the field without containing itself, and β occurs nowhere else in the problem, neither in τ, nor in the rest of the first side, nor in a payload of its own side. Every unifier then places the field at the front of β, so the move is forced. It binds $β ≔ (l: τ | β′)$ for a fresh β′ and continues with the rest of both sides. Since β occurs once, renaming it to β′ on the spine is the same as applying the binding to the whole problem, and the solution stays applied. U-host is stated for the leading end only; its mirror at the trailing end is reported as stuck (@incompleteness-forced). A field demand of a lookup still never enters a row-variable during unification. The only place where it does is the materialization of spent promises at finalization (@inference), after unification has run. Finally U-row-clash is a global projection check rather than a per-window one, and U-stuck reports a terminal configuration.

#let unification_row = figure(
  caption: "Unification of rows, on spines.",
  stack(
    spacing: 15pt,
    align(center, flexbox(
      $#type_name("Leading window") "win"_l (s) = (τ, s′)$,
      $#type_name("Trailing window") "win"^R_l (s) = (τ, s′)$,
      $#type_name("Anywhere") "rem"_l (s) = (τ, s′)$,
      $#type_name("Sole host") "host"_l (τ, t, s) = β$,
    )),
    flexbox(
      derive(
        "U-ε-var",
        ($s$ + [ field-free], $"vars"(s) = overline(β)$),
        $⟨⟩ scripts(≐)_r s ⇝ [overline(β) ≔ ε]$,
      ),
      derive("U-ε-clash", ($(l: τ) ∈ s$,), $⟨⟩ scripts(≐)_r s ⇝ #u_clash$),
      derive(
        "U-cancel-L",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $α · t₁ scripts(≐)_r α · t₂ ⇝ θ$,
      ),
      derive(
        "U-cancel-R",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $t₁ · α scripts(≐)_r t₂ · α ⇝ θ$,
      ),
      derive(
        "U-var-collapse",
        (
          $s$ + [ field-free],
          $α ∈ "vars"(s)$,
          $overline(β) = cases("vars"(s) ∖ α & "if" α "occurs once in" s, "vars"(s) & "otherwise")$,
        ),
        $α scripts(≐)_r s ⇝ [overline(β) ≔ ε]$,
      ),
      derive("U-var-solve", ($α ∉ "rv"(s)$,), $α scripts(≐)_r s ⇝ [α ≔ s]$),
      derive(
        "U-var-occurs",
        ($α ∈ "rv"(s)$,),
        $α scripts(≐)_r s ⇝ #u_occurs$,
      ),
      derive(
        "U-field-L",
        (
          $"win"_l (s₂) = (τ′, t₂)$,
          $τ ≐ τ′ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $(l: τ) · t₁ scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-field-R",
        (
          $"win"^R_l (s₂) = (τ′, t₂)$,
          $τ ≐ τ′ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $t₁ · (l: τ) scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-ground",
        (
          $"vars"(s₂) = ⟨⟩$,
          $|s₁|_l = |s₂|_l > 0$,
          $"rem"_l (s_i) = (τ_i, t_i)$,
          $τ₁ ≐ τ₂ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $s₁ scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-row-clash",
        ($|s₁|_l > |s₂|_l$, $"vars"(s₂) = ⟨⟩$),
        $s₁ scripts(≐)_r s₂ ⇝ #u_clash$,
      ),
      derive(
        "U-host",
        (
          $"host"_l (τ, t₁, s₂) = β$,
          $β′ "fresh"$,
          $t₁ scripts(≐)_r s₂[β′\/β] ⇝ θ$,
        ),
        $(l: τ) · t₁ scripts(≐)_r s₂ ⇝ θ ∘ [β ≔ (l: τ | β′)]$,
      ),
      derive(
        "U-stuck",
        ([no rule above applies],),
        $s₁ scripts(≐)_r s₂ ⇝ #u_stuck$,
      ),
    ),
  ),
)
#unification_row <unification-row>

@unification-row states the moves as rules. The order is fixed by @unification-cascade, and U-var-collapse, U-var-solve, U-var-occurs, U-field-L, U-field-R, U-ground and U-host are tried in both directions. U-ε-var, U-ε-clash and U-row-clash are symmetric as stated.

#example(name: [Matching, then solving])[
  In $⟨l: γ, α⟩ scripts(≐)_r ⟨l: 𝓫, m: 𝓫⟩$ no side is empty, no variable is shared at an end, and neither side is a lone variable. The leading $l: γ$ finds the partner $l: 𝓫$ in the leading window of the right side, so U-field-L emits $γ ≐ 𝓫 ⇝ [γ ≔ 𝓫]$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫⟩$. Now α is a lone variable, and U-var-solve returns $[α ≔ ⟨m: 𝓫⟩]$. The solution is $[γ ≔ 𝓫, α ≔ (m: 𝓫)]$.
] <ex-unif-match>

#example(name: [Pairing by counting])[
  In $⟨α, l: γ, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ both windows of the left side are empty, since the spine begins and ends with a variable, so neither U-field-L nor U-field-R applies. The right side, however, is var-free and $|s₁|_l = |s₂|_l = 1$. Whatever α and β become, they cannot carry an l without making the counts differ, so U-ground pairs the two l-fields and emits $γ ≐ 𝓫$. The residual $⟨α, β⟩ scripts(≐)_r ⟨⟩$ is solved by U-ε-var with $[α ≔ ε, β ≔ ε]$.
] <ex-unif-ground>

#example(name: [Cancelling a shared tail])[
  For $l ≠ m$, the problem $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | α)$ has spines that both end in α. U-cancel-R cancels it and leaves $⟨l: 𝓫⟩ scripts(≐)_r ⟨m: 𝓫⟩$, where no field finds a partner and U-row-clash fires, since $|⟨l: 𝓫⟩|_l = 1 > 0$ and the right side is var-free. By @lem-cancel the cancellation loses no unifier, so the clash is sound. Paszke and Xie @extensible_tabular need a side condition on the shared tail to reach the same verdict.
] <ex-unif-cancel>

#example(name: [A stuck problem])[
  In $⟨α, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ no move fires. The left side has no field and no lone variable, the field $l: 𝓫$ has no partner in the empty windows of the left side, U-ground needs a positive count on both sides, and U-row-clash needs the side with fewer l-fields to be var-free. The verdict is #u_stuck. This is the Wand ambiguity, which has no most general unifier (@prop-irreducible).
] <ex-unif-stuck>

#example(name: [A forced host], lean: "host_forced, crossfield_success")[
  In $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | β)$ with $l ≠ m$, the spines are $⟨l: 𝓫, α⟩$ and $⟨m: 𝓫, β⟩$. Neither leading field has a partner in the other side's leading window, both sides have variables, and neither side is a lone variable. The right side, however, has no l-field and the single variable β, which occurs nowhere else: β is the sole host of $l: 𝓫$. U-host binds $β ≔ (l: 𝓫 | β′)$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫, β′⟩$, which U-var-solve closes with $α ≔ (m: 𝓫 | β′)$. The solution $[β ≔ (l: 𝓫 | β′), α ≔ (m: 𝓫 | β′)]$ is a most general unifier.
] <ex-unif-host>

== Unification Metatheory <unification-metatheory>

#theorem(
  name: [Termination],
  lean: "unifyRowM_terminates, unifyM_fuel_mono, unifyM_draws",
)[
  For all rows ρ₁ and ρ₂, $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉$ terminates with a verdict.
] <thm-unif-term>

The measure is lexicographic in the number of distinct variables of the problem and its size. A cancellation or a pairing returns parts of its input. A move that first solves a type equation and applies the solution to the residual may enlarge the residual, but it removes the variables it bound, and it draws fewer fresh names than it binds variables, so the residual has fewer distinct variables. U-host trades β for β′ and drops a field, which keeps the count and lowers the size. On its own, U-host draws one name for one bound variable; the bound is strict nevertheless, because its residual is never solved without binding a variable, as β′ occurs on one side of it only. The argument requires the drawn names to be new, which holds since the supply starts above the variables of the problem.

A substitution θ _unifies_ ρ₁ and ρ₂ if $θ ρ₁ ≈ θ ρ₂$, and it _satisfies_ a solution Θ if $θ α ≈ θ(Θ α)$ for every binding of Θ.

#theorem(name: [Success is most general], lean: "unifyRowM_success_mgu")[
  Let $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ Θ$ and let V be the variables of ρ₁ and ρ₂. Then Θ unifies ρ₁ and ρ₂, and for every unifier θ of ρ₁ and ρ₂ there is a θ′ that agrees with θ on V and satisfies Θ.
] <thm-unif-mgu>

The restriction to V is necessary: Θ may introduce fresh variables, such as the β′ of U-host, on which an arbitrary unifier θ need not agree with any extension of Θ.

#theorem(
  name: [Failure is sound],
  lean: "unifyRowM_clash_no_unifier, unifyM_occurs_no_unifier",
)[
  If $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ #u_clash$ or $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ #u_occurs$, then no substitution unifies ρ₁ and ρ₂.
] <thm-unif-fail>

Stuck is conservative. Every move preserves the solution set, but not every solvable problem is solved. Where the algorithm stops, and for what reason is the subject of @incompleteness.
