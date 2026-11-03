#import "../functions.typ": *

#let u_clash = text(font: "Libertinus Serif", smallcaps("Clash"))
#let u_occurs = text(font: "Libertinus Serif", smallcaps("Occurs"))
#let u_stuck = text(font: "Libertinus Serif", smallcaps("Stuck"))

= Unification <unification>
Unification is the core of type inference: given two types, it seeks a substitution that equates them. We split unifcation into two judgements, one for types $τ₁ ≐ τ₂ ⇝ v$, and one for rows $s₁ scripts(≐)_r s₂ ⇝ v$ that solves row-equations up to ≈. Row unification is where we differ from other type systems: a selection never reaches it as a field demand, because the lookup relation answers selections separately. This has immediate consequences for row unification: every move is forced and no backtracking is needed (G4:efficiency). The algorithm never chooses among alternatives, and where no forced move applies, it reports stuck instead of guessing. We prove that a returned solution is most general, and a clash or occurs failure means that no unifier exists in @unification-metatheory.


== Spines and Windows <trace-monoid>
For row unification, we look at the shape of rows and notice a concise representation for them: A lookup searches from left to right and stops on row-variables because shadowing behaviour is unclear after this point. Row-variables thus act as boundaries that no labels can cross. The depth-first reading of a row then flatten a row: A maximal sequence of concutive concrete bindings forms a segment which is delimited by row-variable.

We call this representation a _spine_: the spine $s = ⌈ρ⌉$ of a row ρ is the list of its atoms ($a ::= l: τ | α$ and $s ::= ⟨⟩ | a · s$), obtained by flattening the concatenation tree and dropping ε. For example, $((l: 𝓫 | ε) | (α | m: 𝓫))$ and $(l: 𝓫 | (α | (m: 𝓫 | ε)))$ both have the spine $⟨l: 𝓫, α, m: 𝓫⟩$. A _window_ is the segment either of a spine: in $⟨m: 𝓫, l: 𝓫, α, l: 𝓫′⟩$ the leading window is $⟨m: 𝓫, l: 𝓫⟩$ and the trailing window is $⟨l: 𝓫′⟩$. $|s|_l$ denotes the number of l-fields of s.

A row is thus a word over an alphabet of fields $(l: τ)$ and row-variables α, in which two letters commute exactly when they are fields with distinct labels. Rows modulo ≈ with the unit ε form a partially commutative monoid, also called a _trace monoid_ @book_of_traces. Two standard results of trace theory then carry over to rows and from there into unification. First, ≈ is decidable and has a normal form.

#theorem(name: [Characterization of ≈], lean: "rowEquiv_iff_char")[
  $ρ₁ ≈ ρ₂$ if and only if ρ₁ and ρ₂ have the same sequence of row-variables and, for every label l and every var-free segment, the subsequences of fields labelled l agree pointwise up to ≈.
] <thm-char>

Sorting each var-free segment stably by label yields a normal form, the lexicographic normal form of trace theory. For example, $(m: 𝓫 | l: 𝓫 | α | l: 𝓫′ | m: 𝓫)$ splits into the segments $(m: 𝓫 | l: 𝓫)$ and $(l: 𝓫′ | m: 𝓫)$ around α, and its representative is $(l: 𝓫 | m: 𝓫 | α | l: 𝓫′ | m: 𝓫)$: the first segment is reordered, and no field crosses α. Second, the monoid is cancellative, as every trace monoid is.

#lemma(
  name: [Cancellativity],
  lean: "RowEquiv.cancel_cat_left, RowEquiv.cancel_cat_right",
)[
  If $(ρ | ρ₁) ≈ (ρ | ρ₂)$ or $(ρ₁ | ρ) ≈ (ρ₂ | ρ)$, then $ρ₁ ≈ ρ₂$.
] <lem-cancel>

This reading of rows unveils the benefit that it makes several moves of row unification forced. The outermost l-field of a window can be swapped to the end of the spine, which licenses U-field-L and U-field-R, and cancellativity licenses the cancel moves.

This structure has a cost: ≈ keeps shadowed fields. $(l: 𝓫 | l: 𝓫′) ≉ (l: 𝓫)$, although no program observes the shadowed field. For instance, `{l = c}` and `{l = d} ‖ {l = c}` behave identically, but their rows are not equivalent. NixLang records are finite maps, so the faithful relation identifies a row with its shadow-free part. That relation, however, is not cancellative: it identifies $(l: 𝓫 | l: τ)$ and $(l: 𝓫 | l: τ′)$ for all τ and τ′, while $(l: τ)$ and $(l: τ′)$ stay apart. The cancel moves would then lose unifiers: under it, $α ≔ (l: 𝓫)$ unifies $(α | l: τ)$ and $(α | l: τ′)$ for all τ and τ′, while U-cancel-L reduces the problem to $τ ≐ τ′$. We therefore keep ≈ and accept that it is finer than the records of NixLang.


== Type Unification
We start with presenting type unification. Unification either returns a solution θ or one of three refusals. #u_clash reports that two head constructors differ or that a field has no possible partner. #u_occurs reports that a variable occurs in the type or row it is equated with, so a solution would need a recursive type, which MiniNix does not have. #u_stuck reports that no move applies and the Wand ambiguity is the prime example (@ex-unif-stuck) of this error class. A refusal of a premise is also the verdict of the conclusion.

#let unification_ty = figure(
  caption: "Verdicts and unification of types.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Verdicts")[
      $
        #type_name("Verdict") v ::= θ | #u_clash | #u_occurs | #u_stuck
      $
    ],
    subbox(caption: "Type Unification")[
      #flexbox(
        derive("U-refl", (), $α ≐ α ⇝ ∅$),
        derive("U-bind", ($α ∉ "ftv"(τ)$,), $α ≐ τ ⇝ [α ≔ τ]$),
        derive("U-occurs", ($τ ≠ α$, $α ∈ "ftv"(τ)$), $α ≐ τ ⇝ #u_occurs$),
        derive("U-base", ($𝓫 = 𝓫′$,), $𝓫 ≐ 𝓫′ ⇝ ∅$),
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
  )),
)
#unification_ty <unification-ty>

The rules for types are those of first-order unification (@unification-ty). A variable is bound unless it occurs in the other side (U-bind, U-occurs), and these rules also apply with the sides exchanged. The _head_ of any other type is its outermost constructor: a base type, ★, → or the record braces. Equal heads decompose and different heads clash (U-clash). ★ is a nullary constructor like a base type, so it unifies only with itself (U-★). Unlike the dynamic type of gradual typing, it is not consistent with every type. U-rcd hands records over to row unification on their spines.


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
        both: true,
        "U-ε-var",
        [$⟨⟩ scripts(≐)_r s$ with $s$ field-free],
        $[overline(β) ≔ ε]$,
        u_ok,
      ),
      ..u_step(
        both: true,
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
        both: true,
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
        u_stuck,
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

Row unification is discharged by U-rcd and acts on the spines of records. It is a deterministic cascade of forced moves, tried top to bottom in the order of @unification-cascade. A move is _forced_ if it preserves the set of unifiers on the variables of the problem. If no move applies, the verdict is #u_stuck. @app-row-rules states the moves as inference rules.

The exhaustion moves handle an empty side. If the other side has only variables, U-ε-var sends each of them to ε; if it has a field, U-ε-clash reports a clash. The cancel moves remove a row-variable that both spines share at their front (U-cancel-L) or at their back (U-cancel-R), which loses no unifier by @lem-cancel.

The solving moves fire when one side is a lone variable α. Here $"vars"(s)$ lists the variables on the spine of s, and $"rv"(s)$ the row-variables of s at any depth: for $s = ⟨l: {β}, α⟩$, $"vars"(s) = ⟨α⟩$ and $"rv"(s) = {α, β}$. If s has only variables and contains α, U-var-collapse applies: $α scripts(≐)_r ⟨γ, α, β⟩$ forces $[γ ≔ ε, β ≔ ε]$, and a repeated occurrence of α forces $α ≔ ε$ as well. Otherwise, U-var-solve binds α to s if $α ∉ "rv"(s)$, and U-var-occurs reports that a solution would need a recursive row.

The three matching moves differ in how far they may look for a partner. U-field-L and U-field-R pair a field only inside a window, because a row-variable in between could be instantiated to a colliding field and the pairing would be a guess about shadowing. Against $⟨m: 𝓫, l: 𝓫, α, l: 𝓫′⟩$, a leading $l: τ$ is paired with $l: 𝓫$. Against $⟨m: 𝓫, α, l: 𝓫′⟩$ it has no partner in the leading window, and pairing it with $l: 𝓫′$ would assume that α carries no l. U-ground lifts that restriction by counting. A var-free side fixes $|s|_l$. If both sides have the same positive number, the variables of the other side cannot carry an l without breaking the count, so the i-th l-field on one side pairs with the i-th on the other, and the pairing is again forced.

Counting also detects clashes. If a var-free side has fewer l-fields than the other side, no substitution can balance the counts, and U-row-clash reports a clash. The check counts over the whole spine, not per window.

A field without a partner may still have a host. U-host fires on a leading field $l: τ$ when the other side has a _sole host_ β for it: β is the first variable of that side, the side has no l-field, every other variable of the side occurs in τ and so cannot carry the field without containing itself, and β occurs nowhere else in the problem, neither in τ, nor in the rest of the first side, nor in a payload of its own side. Every unifier then places the field at the front of β, so the move is forced. It binds $β ≔ (l: τ | β′)$ for a fresh β′ and continues with the rest of both sides, where β is renamed to β′. U-host is stated for the leading end only; its mirror at the trailing end is reported as stuck (@incompleteness-forced).

#example(name: [Cancelling a shared tail])[
  For $l ≠ m$, the problem $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | α)$ has spines that both end in α. U-cancel-R cancels it and leaves $⟨l: 𝓫⟩ scripts(≐)_r ⟨m: 𝓫⟩$, where no field finds a partner and U-row-clash fires, since $|⟨l: 𝓫⟩|_l = 1 > 0$ and the right side is var-free. By @lem-cancel the cancellation loses no unifier, so the clash is sound. Paszke and Xie @extensible_tabular need the side condition of their rule Rfield on the shared tail to reach the same verdict.
] <ex-unif-cancel>

#example(name: [Matching, then solving])[
  In $⟨l: γ, α⟩ scripts(≐)_r ⟨l: 𝓫, m: 𝓫⟩$ no side is empty, no variable is shared at an end, and neither side is a lone variable. The leading $l: γ$ finds the partner $l: 𝓫$ in the leading window of the right side, so U-field-L emits $γ ≐ 𝓫 ⇝ [γ ≔ 𝓫]$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫⟩$. Now α is a lone variable, and U-var-solve returns $[α ≔ (m: 𝓫)]$. The solution is $[γ ≔ 𝓫, α ≔ (m: 𝓫)]$.
] <ex-unif-match>

#example(name: [Pairing by counting])[
  In $⟨α, l: γ, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ both windows of the left side are empty, since the spine begins and ends with a variable, so neither U-field-L nor U-field-R applies. The right side, however, is var-free and $|s₁|_l = |s₂|_l = 1$. Whatever α and β become, they cannot carry an l without making the counts differ, so U-ground pairs the two l-fields and emits $γ ≐ 𝓫$. The residual $⟨α, β⟩ scripts(≐)_r ⟨⟩$ is solved by U-ε-var with $[α ≔ ε, β ≔ ε]$.
] <ex-unif-ground>

#example(name: [A forced host], lean: "host_forced, crossfield_success")[
  In $(l: 𝓫 | α) scripts(≐)_r (m: 𝓫 | β)$ with $l ≠ m$, the spines are $⟨l: 𝓫, α⟩$ and $⟨m: 𝓫, β⟩$. Neither leading field has a partner in the other side's leading window, both sides have variables, and neither side is a lone variable. The right side, however, has no l-field and the single variable β, which occurs nowhere else: β is the sole host of $l: 𝓫$. U-host binds $β ≔ (l: 𝓫 | β′)$ and continues with $⟨α⟩ scripts(≐)_r ⟨m: 𝓫, β′⟩$, which U-var-solve closes with $α ≔ (m: 𝓫 | β′)$. The solution $[β ≔ (l: 𝓫 | β′), α ≔ (m: 𝓫 | β′)]$ is a most general unifier.
] <ex-unif-host>

#example(name: [A stuck problem])[
  In $⟨α, β⟩ scripts(≐)_r ⟨l: 𝓫⟩$ no move fires. The left side has no field and no lone variable, the field $l: 𝓫$ has no partner in the empty windows of the left side, U-ground needs a positive count on both sides, U-row-clash needs the side with fewer l-fields to be var-free, and α is no sole host, since β does not occur in 𝓫. The verdict is #u_stuck. This is the Wand equation, which has no most general unifier (@prop-irreducible).
] <ex-unif-stuck>

== Unification Metatheory <unification-metatheory>

#theorem(
  name: [Termination],
  lean: "unifyRowM_terminates, unifyTyM_terminates, unifyM_fuel_mono, unifyM_draws",
)[
  Type and row unification terminate on all inputs with a unique verdict.
] <thm-unif-term>

#proof[By a lexicographic measure: the number of distinct variables of the problem, then its size. A cancellation or a pairing returns parts of its input. A move that first solves a type equation and applies the solution to the residual may enlarge the residual, but it removes the variables it bound, and it draws fewer fresh names than it binds variables, so the residual has fewer distinct variables. U-host trades β for β′ and drops a field, which keeps the count and lowers the size. The argument requires the drawn names to be new, which holds since the supply starts above the variables of the problem.]

A substitution θ _unifies_ ρ₁ and ρ₂ if $θ ρ₁ ≈ θ ρ₂$. It _factors through_ a solution Θ if $θ α ≈ θ(Θ α)$ for every binding of Θ, that is, if θ and $θ ∘ Θ$ agree up to ≈.

#theorem(name: [Success is most general], lean: "unifyRowM_success_mgu")[
  Let $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ Θ$ and let V be the variables of ρ₁ and ρ₂. Then Θ unifies ρ₁ and ρ₂, and for every unifier θ of ρ₁ and ρ₂ there is a θ′ that agrees with θ on V and factors through Θ.
] <thm-unif-mgu>

The restriction to V is necessary: Θ may draw fresh variables, such as the β′ of U-host, and an arbitrary unifier θ knows nothing about them.

#theorem(
  name: [Failure is sound],
  lean: "unifyRowM_clash_no_unifier, unifyRowM_occurs_no_unifier",
)[
  If $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ #u_clash$ or $⌈ρ₁⌉ scripts(≐)_r ⌈ρ₂⌉ ⇝ #u_occurs$, then no substitution unifies ρ₁ and ρ₂.
] <thm-unif-fail>

Stuck is conservative: every move is forced, but not every solvable problem is solved. @incompleteness describes where the algorithm stops and why.
