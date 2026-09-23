// DRAFT — new `== Semantics` subsection for `= Minimal Calculus` in thesis.typ.
// Insert after `== Sorts` (thesis.typ:326–347) and before
// `= Declarative <declarative-system>` (thesis.typ:349). Closes the one
// unchecked box under »Declarative« in meta/zeitplan.md:26.
//
// Fragment: not compilable on its own — it needs `#import "./functions.typ": *`
// and it refers to @syntax, @type-safety and @row-lookup, which live in
// thesis.typ. To render it standalone, see the wrapper at the bottom.
//
// Everything below is read off the mechanization; the concordance at the end
// lists the Lean name for every rule.

== Semantics <semantics>

The calculus of @syntax is given a small-step operational semantics. It is
written down here because @type-safety quantifies over it: progress and
preservation are statements about $→$ and about ↯, and neither judgement has so
far been defined in the text.

Two decisions drive the whole relation, and both are Nix' rather than ours.
*Records are values at the constructor level.* A record body is not evaluated
when the record is built; its fields are unevaluated terms and are forced only
when they are projected out. *Colliding fields resolve to the left.* A body is a
_bag_ of fields in which the leftmost binding for a label wins, and asymmetric
concatenation implements set-or-replace by putting the right operand's fields at
the *front*. This is the term-level counterpart of T-conc's ${ρ₂ | ρ₁}$ and the
reason the operation is total: nothing is discarded, the loser is merely
shadowed.

#let values = figure(
  caption: "Values and field lookup.",
  box(width: 100%, stack(
    spacing: 20pt,
    subbox(caption: "Values")[
      $
        #type_name("Value") v & ::= c | (x: e) | { ξ } \
      $
    ],
    subbox(caption: "Field lookup")[
      #stack(
        spacing: 15pt,
        align(center, $#type_name("Field Result") w ::= e | ⊥$),
        flexbox(
          derive("F-ε", (), $ε.l ⇓ ⊥$),
          derive("F-hit", ($l₁ = l₂$,), $(l₁ = e).l₂ ⇓ e$),
          derive("F-miss", ($l₁ ≠ l₂$,), $(l₁ = e).l₂ ⇓ ⊥$),
          derive("F-conc-hit", ($ξ₁.l ⇓ e$,), $(ξ₁ | ξ₂).l ⇓ e$),
          derive(
            "F-conc-skip",
            ($ξ₁.l ⇓ ⊥$, $ξ₂.l ⇓ w$),
            $(ξ₁ | ξ₂).l ⇓ w$,
          ),
        ),
      )
    ],
  )),
)
#values <values>

@values fixes the two auxiliary notions. A value is a constant, a λ or a record
*literal* — note that $v ::= … | { ξ }$ places no condition on ξ whatsoever,
which is precisely the laziness: `{ l = {}.k }` is a value even though its
single field, once forced, errors. The set 𝓒 of constants is inert; there
are no δ-rules, because the minimal calculus admits constants only to have
something the basetypes 𝓑 can type.

Field lookup $ξ.l ⇓ w$ is the term-level mirror of the row lookup $Γ ⊢ ρ.l ↓ r$
of @row-lookup, and the correspondence is exact rule for rule: F-ε ↔ L-ε,
F-hit ↔ L-hit, F-miss ↔ L-miss, F-conc-hit ↔ L-conc-hit,
F-conc-skip ↔ L-conc-skip. What is *missing* is the interesting part. There is
no F-α, no F-α-free and no F-conc-★, so $w$ has two cases where $r$ has three:
a record literal carries no row-variable in its spine, so ★ can never arise
from a term. The unknown is a phenomenon of *types*, not of evaluation — which
is what makes the soft-typing claim of @our-position meaningful, and the reason
the ↯-disjunct of progress is the only place risk accumulates. Because the
relation is total and deterministic on literals, field lookup is really the
partial function "leftmost binding for l", and we write it as a relation only to
keep the parallel to ↓ visible.

#let reduction = figure(
  caption: "Reduction.",
  stack(
    spacing: 15pt,
    align(center, $#type_name("Judgement") e → e′$),
    flexbox(
      derive("E-β", (), $(x: e) #h(2pt) v → e[x ≔ v]$),
      derive("E-app-fn", ($e₁ → e₁′$,), $e₁ e₂ → e₁′ e₂$),
      derive("E-app-arg", ($e₂ → e₂′$,), $v #h(2pt) e₂ → v #h(2pt) e₂′$),
      derive("E-let-β", (), $#b[let] x = v #b[in] e → e[x ≔ v]$),
      derive(
        "E-let",
        ($e₁ → e₁′$,),
        $#b[let] x = e₁ #b[in] e₂ → #b[let] x = e₁′ #b[in] e₂$,
      ),
      derive("E-conc-l", ($e₁ → e₁′$,), $e₁ ‖ e₂ → e₁′ ‖ e₂$),
      derive("E-conc-r", ($e₂ → e₂′$,), $v ‖ e₂ → v ‖ e₂′$),
      derive("E-conc", (), $\{ ξ₁ \} ‖ \{ ξ₂ \} → \{ ξ₂ | ξ₁ \}$),
      derive("E-sel", ($e → e′$,), $e.l → e′.l$),
      derive("E-sel-hit", ($ξ.l ⇓ e$,), $\{ ξ \}.l → e$),
    ),
  ),
)
#reduction <reduction>

@reduction is call-by-value in its binders and lazy in its record fields. E-β
and E-let-β both wait for a value and then substitute, so the two binding forms
behave alike. Substitution $e[x ≔ v]$ is the *naive* one: it stops at a
shadowing binder and renames nothing. That is sound here only because
@type-safety is stated for closed terms, so the $v$ pushed inwards is closed and
capture cannot arise. The
congruence rules E-app-fn/E-app-arg, E-conc-l/E-conc-r and E-let fix a
left-to-right order and are the only source of nondeterminism-avoidance in the
relation.

Two rules carry the design. E-conc is where set-or-replace happens, and it does
nothing but *rebuild the bag*: the operands' bodies are concatenated with ξ₂ in
front, so a label bound on both sides is thereafter found in ξ₂ by
F-conc-hit and the ξ₁-binding is shadowed rather than deleted. No comparison of
label sets takes place, no side condition constrains the operands, and the rule
therefore never fails — the totality that makes `//` cheap in Nix and expensive
to type. E-sel-hit is the only rule that forces anything: it hands back the
field's term *unevaluated*, so the result of a projection generally steps
further. That is laziness made operational, and it is what lets a well-typed
program contain a field that would not type if it were ever forced.

#let errors = figure(
  caption: [Lookup errors.],
  stack(
    spacing: 15pt,
    align(center, $#type_name("Judgement") e ↯$),
    flexbox(
      derive("↯-sel", ($ξ.l ⇓ ⊥$,), $\{ ξ \}.l ↯$),
      derive("↯-sel-cong", ($e ↯$,), $e.l ↯$),
      derive("↯-app-fn", ($e₁ ↯$,), $e₁ e₂ ↯$),
      derive("↯-app-arg", ($e₂ ↯$,), $v #h(2pt) e₂ ↯$),
      derive("↯-conc-l", ($e₁ ↯$,), $e₁ ‖ e₂ ↯$),
      derive("↯-conc-r", ($e₂ ↯$,), $v ‖ e₂ ↯$),
      derive("↯-let", ($e₁ ↯$,), $#b[let] x = e₁ #b[in] e₂ ↯$),
    ),
  ),
)
#errors <errors>

@errors defines the erroring terms $e ↯$, the third disjunct of progress. Only
one rule *generates* an error: ↯-sel, selecting a label that a record literal
does not bind. The remaining six propagate it through exactly the evaluation
contexts of @reduction — they are the congruence rules of → with the
step-premise replaced by an error-premise — so ↯ is not an open-ended
"something went wrong" but a named, single-origin failure, located at the
one construct the type system admits it cannot decide.

What the judgement does *not* have is as informative. There is no rule under a
λ-body and none under a record field, mirroring the fact that neither position
is evaluated: an erroring field inside a value is not itself an error, only its
projection is. And there is no rule for concatenating non-records or applying a
non-λ; those configurations are stuck, but canonical forms rule them out for
well-typed terms, which is why progress can afford to name ↯ and nothing else.

=== Concordance with the mechanization

Every rule above is a constructor in `lean/minimal.lean`, and the two safety
theorems quantify over these three relations unchanged.

#table(
  columns: (auto, auto),
  stroke: none,
  inset: (x: 8pt, y: 4pt),
  align: left,
  [*Text*], [*Lean*],
  $v$, [`Value` (minimal.lean:1213)],
  $ξ.l ⇓ w$, [`RecBody.lookup` (minimal.lean:24), `some`/`none` for $e$/⊥],
  $e[x ≔ v]$, [`subst` / `substBody` (minimal.lean:1191)],
  $e → e′$, [`Step` (minimal.lean:1264)],
  $e ↯$, [`Err` (minimal.lean:1311)],
  [E-β, E-let-β], [`Step.beta`, `Step.letBeta`],
  [E-app-fn, E-app-arg], [`Step.appFun`, `Step.appArg`],
  [E-conc-l, E-conc-r, E-conc], [`Step.catLeft`, `Step.catRight`, `Step.catVal`],
  [E-sel, E-sel-hit], [`Step.selStep`, `Step.selVal`],
  [↯-sel], [`Err.selAbsent`],
  [Progress], [`progress` (minimal.lean:1333)],
  [Preservation], [`preservation` (minimal.lean:1613)],
  [L2 safety], [`qProgress`, `qPreservation` (Qualified.lean:1230, :1235)],
)

The qualified system of @declarative-system reuses `Value`, `Step` and `Err`
verbatim: there is one semantics, and both type systems are proved safe against
it.

=== Open points

- *Constants are inert.* No δ-rules, so 𝓒 contributes nothing to evaluation.
  Real Nix builtins would add reduction rules and, with them, the first
  opportunity for a type error that is not a lookup error.
- *Binders are by-value, fields are by-name.* Nix is lazy throughout, so E-β
  and E-let-β diverge from the reference semantics of Broekhoff and Krebbers
  @verified in a way the text should own rather than pass over. The divergence
  is harmless for the theorems — laziness only ever *avoids* reaching an error,
  so progress up to ↯ is if anything conservative — but it is a divergence, and
  the honest form of the claim in @related-work is "every program keeps its
  untyped semantics *up to evaluation order*".
- *No sharing.* Substitution duplicates terms; Nix' thunks do not. Sharing is
  invisible to typing and was therefore not modelled, but it is the obvious
  next refinement if the semantics is ever used for anything but safety.

// ---------------------------------------------------------------------------
// Standalone wrapper: uncomment (and comment out the @-references above) to
// render this fragment on its own with
//   typst compile drafts/semantics-draft.typ
//
// #import "../text/functions.typ": *
// #show: template
// ---------------------------------------------------------------------------
