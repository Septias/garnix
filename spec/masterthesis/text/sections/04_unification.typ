#import "../functions.typ": *

#let u_clash = text(font: "Libertinus Serif", smallcaps("Clash"))
#let u_occurs = text(font: "Libertinus Serif", smallcaps("Occurs"))
#let u_stuck = text(font: "Libertinus Serif", smallcaps("Stuck"))
#let u_fuel = text(font: "Libertinus Serif", smallcaps("No fuel"))

= Unification <unification>

We present our unification algorithm, which is sound and terminating but incomplete. It is a _mutual_ pair of judgements: $τ₁ ≐ τ₂ ⇝ v$ solves an equation between types and $s₁ scripts(≐)_r s₂ ⇝ v$ one between rows. @unification-helpers lists the spines and verdicts on which both operate.


#figure(
  caption: "Spines, verdicts and the judgements of the algorithm.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Spines and Verdicts")[
      $
           #type_name("Atom") a & ::= l: τ | α \
          #type_name("Spine") s & ::= ⟨⟩ | a · s \
        #type_name("Verdict") v & ::= θ | #u_clash | #u_occurs | #u_stuck | #u_fuel \
      $
    ],
    subbox(caption: "Notation")[
      #flexbox(
        $#type_name("Spine of a row") ⌈ρ⌉$,
        $#type_name("Field count") |s|_l$,
        $#type_name("Var sequence") "vars"(s)$,
        $#type_name("Row vars, at any depth") "rv"(s)$,
        $#type_name("Type equation") τ₁ ≐ τ₂ ⇝ v$,
        $#type_name("Row equation") s₁ scripts(≐)_r s₂ ⇝ v$,
      )
    ],
  )),
)<unification-helpers>

Rows are not unified as trees but as _spines_ — the lists of atoms $a ::= l: τ | α$ obtained by flattening the concatenation tree — because associativity and the units of @row-equivalence are then quotiented away by construction, and the only residue of ≈ is that distinct labels commute within a var-free segment while nothing crosses a row-variable. For example, $((l: 𝓫 | ε) | (α | m: 𝓫))$ and $(l: 𝓫 | (α | (m: 𝓫 | ε)))$ both have the spine $⟨l: 𝓫, α, m: 𝓫⟩$.

A verdict is either a solution θ or one of four refusals. #u_clash and #u_occurs are the familiar ones, and #u_stuck carries the Wand ambiguity: it is reported when no move applies but nothing is provably wrong. #u_fuel reports an exhausted recursion bound, not a property of the problem: a large enough budget always decides it (@thm-unif-term).

== Type Unification
#let unification_ty = figure(
  caption: "Unification of types.",
  flexbox(
    derive("U-refl", (), $α ≐ α ⇝ ∅$),
    derive("U-bind", ($τ ≠ α$, $α ∉ "ftv"(τ)$), $α ≐ τ ⇝ [α ≔ τ]$),
    derive("U-occurs", ($τ ≠ α$, $α ∈ "ftv"(τ)$), $α ≐ τ ⇝ #u_occurs$),
    derive("U-★", (), $★ ≐ ★ ⇝ ∅$),
    derive("U-base", ($𝓫 = 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ ∅$),
    derive("U-base-clash", ($𝓫 ≠ 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ #u_clash$),
    derive("U-rcd", ($⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ θ$,), ${ρ₁} ≐ {ρ₂} ⇝ θ$),
    derive(
      "U-fn",
      ($τ₁ ≐ τ₁′ ⇝ θ₁$, $θ₁ τ₂ ≐ θ₁ τ₂′ ⇝ θ₂$),
      $(τ₁ -> τ₂) ≐ (τ₁′ -> τ₂′) ⇝ θ₂ ∘ θ₁$,
    ),
    derive("U-clash", ($"head"(τ₁) ≠ "head"(τ₂)$,), $τ₁ ≐ τ₂ ⇝ #u_clash$),
  ),
)
#unification_ty <unification-ty>

@unification-ty is standard first-order unification, read top-to-bottom, with one deviation: ★ is rigid. U-★ unifies it with itself and U-clash rejects it against everything else, so the unknown is never silently absorbed into another type — a ★ in a solution is always one that the lookup relation put there. [The occurs check of U-occurs ensures that a binding _eliminates_ its variable](explain this), and the elimination of variables is what bounds the recursion.

U-bind and U-occurs are stated for a variable on the left and are tried on both sides. U-fn is the only place where the type pass sequences: the solution of the argument equation is applied to the results before they are unified, and the two solutions are composed. A refusal from either premise of U-fn, or from the row problem of U-rcd, is the verdict of the conclusion.

== Row Unification
// meta: the rules are not really tanglible for me
The row pass is a deterministic cascade. Every move it makes is _forced_ — it preserves the solution set of the problem rather than choosing among alternatives. [One move, U-host, introduces a fresh variable, and only where the placement of a field is itself forced.](does the reader understand this yet? or is it an artifact?) @unification-cascade gives the order in which the moves are attempted; the first one whose trigger fires decides the step, and if none fires the configuration is terminal.

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

// todo: are the reverse arrows correct everywhere?
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
        "U-var-refl-L",
        $α · t₁ scripts(≐)_r α · t₂$,
        [cancel, recurse on $t₁, t₂$],
        u_go,
      ),
      ..u_step(
        "U-var-refl-R",
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
        "U-clash",
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
        u_stuck + [ — Wand ambiguity],
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

The two exhaustion rules are checked at every depth and before the budget: once a side is empty, its counterpart's variables are forced to ε and a surviving field has nowhere to come from. The cancellation rules exploit that spines cancel at both ends, which is exactly the trace-monoid property of @trace-monoid. The three solving rules fire only when one side is a lone variable α. If α occurs on the other side s only among variables, U-var-collapse solves the problem without binding α to a row that contains it: the variable sequences of both sides must agree under every unifier, so a single occurrence of α forces every other variable of s to ε and leaves α free, and a repeated occurrence forces α to ε as well. This solution is unique and hence most general (`allvar_occurs_mgu` in Lean). Otherwise U-var-solve binds α if it occurs nowhere in s, at any depth, and U-var-occurs reports the cycle if it does; the occurs check ensures that the binding eliminates the variable.

The three matching moves differ in how far they may look for a partner. A _window_ is a maximal var-free segment at one end of a spine: U-field-L and U-field-R may pair a field only inside it, because a row-variable in between could be instantiated to a colliding field and the pairing would be a guess about shadowing. In $⟨m: 𝓫, l: 𝓫, α, l: 𝓫′⟩$ the leading window is $⟨m: 𝓫, l: 𝓫⟩$ and the trailing window is $⟨l: 𝓫′⟩$. A leading $l: τ$ on the other side is paired with $l: 𝓫$. Against $⟨m: 𝓫, α, l: 𝓫′⟩$ it has no partner in the leading window, and pairing it with $l: 𝓫′$ would assume that α carries no l. U-ground lifts that restriction by counting: if one side [has no variables at all and some label occurs equally often and positively on both sides](explain this), then the other side's variables cannot carry that label, the pairing is positional, and it is again forced.

A field facing only variables on the other side has no partner, but it may have a host. U-host fires on a leading field $l: τ$ when the other side has a _sole host_ β for it: β is the first variable of that side, the side has no l-field and no keyed field, every other variable of the side occurs in τ and so cannot carry the field without containing itself, and β occurs nowhere else in the problem, neither in τ, nor in the rest of the first side, nor in a payload of its own side. Every unifier then places the field at the front of β, so the move is forced. It binds $β ≔ (l: τ | β′)$ for a fresh β′ and continues with the rest of both sides. Since β occurs once, renaming it to β′ on the spine is the same as applying the binding to the whole problem, and the solution stays applied. U-host is stated for the leading end only; its mirror at the trailing end is reported as stuck (@incompleteness-forced). A field demand of a lookup still never enters a row-variable during unification. The only place where it does is the materialization of spent promises at finalization (@inference), after unification has run. Finally U-clash is a global projection check rather than a per-window one, and U-stuck reports a terminal configuration.

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
        "U-var-refl-L",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $α · t₁ scripts(≐)_r α · t₂ ⇝ θ$,
      ),
      derive(
        "U-var-refl-R",
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
        "U-clash",
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

@unification-row states the moves as rules. They are to be read in the order of @unification-cascade, and U-var-collapse, U-var-solve, U-var-occurs, U-field-L, U-field-R, U-ground and U-host are additionally tried with the two sides exchanged; U-ε-var, U-ε-clash and U-clash are symmetric as stated. Field lookups do not travel through $scripts(≐)_r$, they are deferred as stumps, so row unification never guesses a field into a variable: the field U-host places comes from the other side of the equation, and its host is forced.

#example(name: [Matching, then solving])[
  In $⟨l: γ, α⟩ scripts(≐)_r ⟨l: 𝓫, m: 𝓫⟩$ no side is empty, no variable is shared at an end, and neither side is a lone variable. The leading $l: γ$ finds the partner $l: 𝓫$ in the leading window of the right side, so U-field-L emits $γ ≐ 𝓫 ⇝ [γ ≔ 𝓫]$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫⟩$. Now α is a lone variable, and U-var-solve returns $[α ≔ ⟨m: 𝓫⟩]$. The solution is $[γ ≔ 𝓫, α ≔ (m: 𝓫)]$.
] <ex-unif-match>

#example(name: [Pairing by counting])[
  In $⟨α, l: γ, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ both windows of the left side are empty, since the spine begins and ends with a variable, so neither U-field-L nor U-field-R applies. The right side, however, is var-free and $|s₁|_l = |s₂|_l = 1$. Whatever α and β become, they cannot carry an l without making the counts differ, so U-ground pairs the two l-fields and emits $γ ≐ 𝓫$. The residual $⟨α, β⟩ scripts(≐)_r ⟨⟩$ is solved by U-ε-var with $[α ≔ ε, β ≔ ε]$.
] <ex-unif-ground>

// meta: why does this use l₁ an l₂ instead of l and m?
#example(name: [Cancelling a shared tail])[
  For $l₁ ≠ l₂$, the problem $(l₁: 𝓫 | α) scripts(≐)_r (l₂: 𝓫 | α)$ has spines that both end in α. U-var-refl-R cancels it and leaves $⟨l₁: 𝓫⟩ scripts(≐)_r ⟨l₂: 𝓫⟩$, where no field finds a partner and U-clash fires, since $|⟨l₁: 𝓫⟩|_(l₁) = 1 > 0$ and the right side is var-free. By @lem-cancel the cancellation loses no unifier, so the clash is sound. Paszke and Xie @extensible_tabular need a side condition on the shared tail to reach the same verdict.
] <ex-unif-cancel>

#example(name: [A stuck problem])[
  In $⟨α, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ no move fires. The left side has no field and no lone variable, the field $l: 𝓫$ has no partner in the empty windows of the left side, U-ground needs a positive count on both sides, and U-clash needs the side with fewer l-fields to be var-free. The verdict is #u_stuck. This is the Wand ambiguity, which has no most general unifier (@prop-irreducible).
] <ex-unif-stuck>

#example(name: [A forced host], lean: "host_forced, crossfield_success")[
  In $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | β)$ with $l ≠ m$, the spines are $⟨l: 𝓫, α⟩$ and $⟨m: 𝓫, β⟩$. Neither leading field has a partner in the other side's leading window, both sides have variables, and neither side is a lone variable. The right side, however, has no l-field and the single variable β, which occurs nowhere else: β is the sole host of $l: 𝓫$. U-host binds $β ≔ (l: 𝓫 | β′)$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫, β′⟩$, which U-var-solve closes with $α ≔ (m: 𝓫 | β′)$. The solution $[β ≔ (l: 𝓫 | β′), α ≔ (m: 𝓫 | β′)]$ is a most general unifier.
] <ex-unif-host>

The two passes recurse into each other: U-rcd hands a row problem to $scripts(≐)_r$, and U-field-L, U-field-R and U-ground hand a type problem back to $≐$. Each cross-call consumes one unit of the recursion bound behind #u_fuel. Crucially, the type equations a matching move emits are solved on the spot and their solution applied to the residual before the row pass continues. Deferring them instead would make #u_stuck meaningless: an equation must be discharged, or fatal, or itself stuck, never merely postponed.

== Unification Metatheory <unification-metatheory>

#theorem(
  name: [Termination],
  lean: "unifyRowM_terminates, unifyM_fuel_mono, unifyM_draws",
)[
  For all rows ρ₁ and ρ₂ there is a budget at which $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉$ returns a verdict other than #u_fuel, and every larger budget returns the same verdict.
] <thm-unif-term>

The measure is lexicographic in the number of distinct variables of the problem and its size. A cancellation or a pairing returns parts of its input. A move that first solves a type equation and applies the solution to the residual may enlarge the residual, but it removes the variables it bound, and it draws fewer fresh names than it binds variables, so the residual has fewer distinct variables. U-host trades β for β′ and drops a field, which keeps the count and lowers the size. On its own, U-host draws one name for one bound variable; the bound is strict nevertheless, because its residual is never solved without binding a variable, as β′ occurs on one side of it only. The argument requires the drawn names to be new, which holds since [the supply](first introduced here) starts above the variables of the problem.

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

Stuck is conservative. Every move preserves the solution set, but not every solvable problem is solved. Where the algorithm stops, and why it must, is the subject of @incompleteness.
