== Semantics <semantics>

nothing is discarded, the loser is merely shadowed.

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
