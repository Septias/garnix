> Analysis of the first-class labels extension (phase B: keys as a sort,
> keyed lookup, keyed fields, dynamic construction). What it buys, where it
> falls short of NixLang, where the theory is imprecise, and what to do next.
> Sources: thesis @sec-fc-labels, `typesystems/algorithmic.typ` (# First-class
> labels), `lean/LabelLookup.lean`, `lean/Regressions.lean` (FC-labels phase B),
> `lean/InferRuns.lean` (FC-labels). Siblings: `incompleteness.md` (kinds 8, 9),
> `let-review.md`.


# 0. Findings, ranked

1. **The motivating example is a type error.** The introduction's
   `{ "1759190400" = 1; }.${toString builtins.currentTime}` is the thesis's
   argument for ★. In phase B the key has a base type (a string), `⌊κ⌋ ≐ 𝓫`
   clashes, and the run fails. Kernel-checked analogue:
   `run (sd (rec1 "foo" c) c) = "fail: clash"` (InferRuns.lean). The
   @sec-goals text ("the dynamic selection yields ★") is wrong for the same
   reason. §3.1.
2. **Labels are not strings.** `"l"` has the rigid type `⌊l⌋`, so a label
   literal cannot be used as a string and a string cannot be used as a key.
   In NixLang they are the same thing, and almost every real key is computed.
   This is the largest gap to R1/R3. §3.1.
3. **The different-keys claim is wrong for singletons.** Thesis
   (@fc-unification, last paragraph), algorithmic.typ:915 and
   Regressions.lean:391 say `${α}:τ ≐ᵣ ${β}:τ′` has no mgu. It has one,
   `[α ≔ β] ∘ mgu(τ, τ′)`. The claim holds only with surrounding fields. §5.
4. **One keyed field is one unknown key.** Attribute sets used as
   dictionaries (`listToAttrs`, `mapAttrs`, `genAttrs`), the dominant use of
   computed keys in nixpkgs, have no type. §3.3.
5. **`{ ${null} = v; }` diverges from Nix.** Nix skips the attribute; the
   calculus steps to ↯. §3.2.
6. **"Two thirds stuck" is not attributable to keys.** The `keyed` fuzz
   universe mixes keys with ordinary row variables; the number includes the
   plain Wand/Levi/crossfield shapes. §5.
7. **Reported top-level types forget the key dependency.** `λa. λx. x.${a}`
   prints `⌊k4⌋ → {r3} → ★`. The scheme `selDynQ` is the real answer;
   reporting it needs qualified top-level types, the same fix as the
   key-blocked spent promise. §6.


# 1. The feature in one paragraph

Labels become values `"l"` with singleton types `⌊l⌋`. A key is a label or a
variable of a third sort, `Label`. Dynamic selection `e₁.${e₂}` looks up the
key of e₂'s type; dynamic construction `{ ${e₁} = e₂ }` produces a keyed field
`${k}: τ`. The lookup compares keys three-way (`=`, `≠`, `?`), keyed fields
are barriers in ≈ (like row variables), unification pairs keyed fields only
under the same variable key at the spine ends, and a stump may now be blocked
on a key. Type safety, unification soundness and termination, and inference
soundness are mechanized; `selDynQ` and `rcdDynQ` are instance-closed.

What works well (all `#guard` in InferRuns.lean):
- one let-bound `get` used at a found and at an absent key: `{p: 𝓫 | q: ★}`
- key refinement: `λa. {foo = c; bar = {}}.${a}` waits on the key and hits
  once it is applied
- the Nix update `r // { ${n} = v }` shadows an older field of the same name
- two records under one unknown key flow into one function (U-key)


# 2. Keys as a sort: the right call, with a price

**Why.** Phase A let a key be any type and answered ★ for a non-label key.
That broke preservation: `{ ${e₁} = e₂ }` with `e₁ : ★` steps to `{l = e₂}`,
which is not ≈ to `{${★}: τ}`; and ★ ≡ ★ made lookup unsound at run time. A
separate sort makes "a key is never ★" a typing invariant instead of a side
condition (phase A's `KeySafe` premise on F-★ is gone).

**Price.** A non-label key is a type error. This is the only place where the
calculus rejects instead of recording uncertainty, and it is exactly where
NixLang code is most dynamic (§3.1).

**What the price is NOT.** The preservation problem is about giving the
*record* a precise type. It does not force rejection. Both dynamic forms can
answer ★ for a non-label key and preserve types through T-★-intro, which is
unrestricted:

    Γ ⊢ e₁: {ρ}   Γ ⊢ e₂: 𝓫_str
    ----------------------------- T-sel-dyn-str
    Γ ⊢ e₁.${e₂}: ★

    Γ ⊢ e₁: 𝓫_str   Γ ⊢ e₂: τ
    ----------------------------- T-rcd-dyn-str
    Γ ⊢ { ${e₁} = e₂ }: ★

Preservation: the selection steps to a field value (typed τ′, hence ★ by
T-★-intro) or to ↯; the construction steps to `{l = e₂}`, typed ★ by
T-★-intro. No keyed field mentions ★, the sort stays clean. **[check]** in
Qualified.lean; it is the same argument as T-sel-★.

The record form is coarse (a ★ record cannot be selected from, ★ has no
eliminator), but it turns a rejection into soft typing, which is what R3
asks for.


# 3. Distance to NixLang

## 3.1 Labels vs strings

In NixLang `"foo"` is a string; it is a label only by virtue of being used as
one. Real keys are computed: `"${prefix}-${name}"`, `toString n`,
`builtins.head names`, `lib.toLower x`. In the calculus:

| Program | Today | With §2's rules |
|---|---|---|
| `x.${"foo"}` | `⌊foo⌋`, static selection | same |
| `x.${toString t}` (the intro example) | **clash** | ★ |
| `"foo" + "bar"` | **clash**, `⌊foo⌋ ≐ 𝓫_str` | still clash |
| `n: x.${n}` | `⌊α⌋`, stump on α | same |
| `n: { a = x.${n}; b = n + "-suffix"; }` | **clash** | still clash |

The last two rows are the hard part: one value used as a key AND as a string.
Options:

- **A. Strings as keys, keys as strings, at the boundary.** Give builtins
  that consume strings a "stringish" parameter (`⌊κ⌋` accepted wherever 𝓫_str
  is). That is subtyping `⌊k⌋ ≤ 𝓫_str`, which the calculus has avoided so far.
- **B. Lose the singleton on conflict.** When `⌊κ⌋ ≐ 𝓫_str` would clash,
  answer ★ for every lookup keyed by κ instead. Sound by §2's rules. This is
  the unify-vs-★ choice again (see `incompleteness.md` kind 11, fix B): it is
  principal only when the conflict is definite.
- **C. Two literals.** Type a string literal at a fresh variable constrained
  to "label or string", resolved by use. A constraint form with its own
  solver; out of scope for the thesis.

Recommendation: §2's two rules now (they fix finding 1; their preservation
argument leans on unrestricted T-★-intro, so they must be revisited if
T-★-intro is ever restricted), B as the documented next step, A/C in
@sec-extensions.

## 3.2 Semantics divergences

- **null keys.** `{ ${null} = v; }` evaluates to `{}` in Nix (the attribute is
  skipped; the `${if c then "x" else null} = …` idiom relies on it). The
  calculus has `↯-key`. Model it as `{ ${null} = e } ⟶ {}` with the typing
  rule giving `{ε}` for a null-typed key, or keep ↯ and say so in @sec-goals.
- **Mixed literals.** `{ a = 1; ${n} = 2; }` is one literal in Nix and fails
  at evaluation if `n = "a"` (duplicate attribute). The calculus only has the
  singleton `{ ${e₁} = e₂ }` and ‖, which *shadows* instead. Desugaring a mixed
  literal into ‖ changes an error into a value. Either desugar with a
  disjointness side condition (a negative key fact, see §4) or state the
  divergence.
- **Strictness.** E-rcd-dyn evaluates the key and keeps the field lazy. That
  matches Nix.

## 3.3 Dictionaries

A keyed field is one field under one unknown key. NixLang uses attribute
sets as homogeneous maps far more often than it computes a single key:
`listToAttrs`, `mapAttrs`, `genAttrs`, `foldl' (acc: x: acc // { ${x} = …; })`.
None of these has a type.

Proposal: a dictionary row `∗: τ` ("every PRESENT key maps to τ").

UNSOUND in the naive form `(∗: τ).k ↓ τ`. A dictionary may lack k, and under
asymmetric concatenation the lookup then falls through to the next segment:
`(∗: τ | l: σ).l` would answer τ via L-conc-hit, while at run time a
dictionary without l yields the l-field of type σ. Preservation breaks. The
"missing key is ↯" argument only holds when nothing follows the dictionary.

A sound version needs a fourth lookup answer, *maybe τ*:

    (∗: τ).k ↓ maybe τ
    ρ₁.k ↓ maybe τ   ρ₂.k ↓ ⊥          ⟹  (ρ₁ | ρ₂).k ↓ τ     (absent is ↯)
    ρ₁.k ↓ maybe τ   ρ₂.k ↓ τ′, τ = τ′  ⟹  (ρ₁ | ρ₂).k ↓ τ
    ρ₁.k ↓ maybe τ   otherwise         ⟹  (ρ₁ | ρ₂).k ↓ ?

and `maybe τ` at the top of a selection reads as τ. Every lookup lemma (det,
total, stability, `LookupQ.equiv`) has to be redone with the new answer, and
typing a literal record at a dictionary type needs a subsumption rule.
≈: `∗: τ` is a barrier. It would give `attrValues` the type `{∗: τ} → [τ]`.
Cost: a fourth atom on spines and a fourth lookup answer, the most invasive
change in this file. **[check]** before writing it into the thesis as more
than an outlook.

## 3.4 Missing forms

- `x ? ${n}` and `builtins.hasAttr n x`: need negative information to be
  useful (the else-branch knows `x ⊬ n`); with a key variable this is the
  "lacks under a label variable" atom `negative-info-plan.md` §11 asks to
  decide on. Without it they can be typed as `𝓫_bool` and learn nothing.
- `builtins.getAttr n x`: a builtin scheme, `selDynQ` with arguments swapped.
  Free.
- Nested dynamic paths `a.${b}.c = v` in literals: desugar to nested
  singletons and ‖; inherits §3.2's shadowing caveat.


# 4. Lookup and ≈

**Lookup** is in good shape: deterministic, total, substitution-stable
(`LookupQ.det`, `LookupQ.total`, `LookupQ.applySubst`), coincides with the
label lookup at literal keys (`LookupQ.lab_iff`), and respects ≈
(`LookupQ.equiv`). The three new sources of ? (variable key against a literal
field, keyed field against anything undecided, variable key against a
variable key) each name their blocker, and wake-up triggers on solving a
label variable.

**≈** treats a keyed field as a barrier: `${α}:τ | m:σ ≉ m:σ | ${α}:τ`, even
though the two agree at every instance with `α ≠ m`. This is the precision
loss behind cost 2 in @sec-fc-labels, and it is *exactly* the absence of the
atom `α ≠ m`. Two observations:
- A key disequality is simpler than a row lacks-fact: it is a relation on a
  flat sort, decidable by comparison, never migrates on substitution (a label
  variable is bound to a key, not expanded). If negative information is taken
  up for rows, key disequalities should come first.
- Unlike row lacks-facts, using it in ≈ would make ≈ context-relative; the
  same objection as in `let-review.md` / `incompleteness.md` kind 2. Use it in
  lookup (`(${α}:τ).m ↓ ⊥` under `α ≠ m`) and in unification detectors, not
  in ≈.

**Two keyed fields under the same key** `${α}:τ | ${α}:τ′`: the first shadows
the second, lookup under α hits τ. Correct, and the spine keeps both.
Nothing to fix; worth one sentence in the thesis, since a reader will ask.


# 5. Unification

Verdicts on the keyed shapes (singletons unless stated):

| Problem | mgu | Verdict | Lean |
|---|---|---|---|
| `⌊α⌋ ≐ ⌊l⌋` | `[α ≔ l]` | success | `unify_lab_var_lit` |
| `⌊α⌋ ≐ 𝓫` | none | clash | `unify_lab_vs_base_clash` |
| `ε ≐ᵣ ${α}:𝓫` | none | clash | `unify_empty_vs_key_clash` |
| `${α}:t ≐ᵣ ${α}:𝓫` | `[t ≔ 𝓫]` | success | `unify_same_key` |
| `(r \| ${α}:t) ≐ᵣ (q \| ${α}:𝓫)` | yes | success | `unify_same_key_tail` |
| `(foo:𝓫) ≐ᵣ (${α}:𝓫)` | `[α ≔ foo]` | **stuck** | `unify_lit_vs_key_stuck` |
| `${α}:𝓫 ≐ᵣ ${β}:𝓫` | `[α ≔ β]` | **stuck** | `unify_diff_key_stuck` |
| `(${α}:𝓫 \| m:𝓫) ≐ᵣ (m:𝓫 \| ${β}:𝓫)` | none (`α, β ≔ x` for every x; `α ≔ β` is not a unifier, barrier) | stuck | not pinned |
| `α ≐ {${α}:𝓫}` | identity on keys, `α ≔ {…}` | success | `unify_key_not_occurs` |
| occurrence next to a keyed field | usually none | stuck (not occurs) | Defs.lean:651 |

**The wrong claim.** For the singleton, every unifier needs `θα = θβ` as
keys (≈-dfield demands the same key, ≈-dlab turns literal keys into literal
fields, which are equal only for equal labels). `[α ≔ β]` covers all of
them: `[α, β ≔ l] = [β ≔ l] ∘ [α ≔ β]`. The thesis's sentence "the unifiers
that identify the keys have no common generalization with those that choose
labels" is false. It needs a context, as in the third stuck row.

**Two cheap forced moves** (no invention, unlike U-expand):

    ${α}:τ ≐ᵣ ${β}:τ′  ⇝  [α ≔ β], then τ ≐ τ′       (both sides a single atom)
    l:τ    ≐ᵣ ${α}:τ′  ⇝  [α ≔ l], then τ ≐ τ′       (both sides a single atom)

Forced exactly when both sides are one atom. Generalizing to "the keyed field
is the only barrier and both windows are otherwise empty" needs a short
cancellation proof. Neither move fires in the third row, which is genuinely
mgu-free.

**Occurs next to a key.** Reported stuck because the counting argument does
not see keys. Both verdicts fail the run, so this is a precision loss of the
verdict only.

**The 67% figure.** The `keyed` universe (Fuzz.lean:667) draws row variables
and keyed fields together. Most of its stuck verdicts are the ordinary row
shapes (`incompleteness.md` kinds 1-5). To attribute anything to keys,
re-run with a universe where keyed fields are the only barriers, and report
the fraction the two moves above would rescue. Until then, the thesis sentence
"about two thirds are stuck" should not be read as a cost of keys.


# 6. Inference

- **Order.** A-sel-dyn infers the record, then the key. The stump's blocker
  is whichever stops the lookup first: the row variable or the key. The `#guard`s
  cover both. Fine.
- **Keys from selections.** `λr. λk. r.${k.name}` runs and forces the `name`
  field to a label (`{name: ⌊k6⌋ | …}`). Under a let it is not generalized,
  because `QScheme.Correctable` forbids a key that is another stump's result.
  That clause disappears under `let-review.md` §4.4 (discharge up to ≈), so
  the fix for nested selection fixes this too.
- **Top-level reports.** F-★ never binds a label variable, so unresolved keys
  remain as free `⌊k⌋` in printed types and the lookup collapses to ★:
  `λa. λx. x.${a}` prints `⌊k4⌋ → {r3} → ★`, while the principal answer is
  `selDynQ`. Same fix as the key-blocked spent promise
  (`incompleteness.md` kind 9, fix B): qualified top-level types. Taking that
  fix would make both FC-label headlines visible in `run`.
- **Key-blocked spent promise.** `λr. λa. r.${a} c` fails with "spent
  promise". See `incompleteness.md` kind 9; at a let, `let-review.md` §4.5
  generalizes it.
- **Finalization.** F-★ leaves keys alone, so no key becomes ★. With §2's
  rules this stays true: they type the *expression* ★, the key is never
  unified with ★.


# 7. Metatheory status

Mechanized: lookup det/total/stability, ≈ characterization with barriers,
qProgress/qPreservation (E-rcd-dyn via ≈-dlab), unification soundness and
termination, inference soundness, `selDynQ_instance_closed`,
`rcdDynQ_instance_closed`. Not covered: any completeness statement for keys
(none exists for rows either), and §2's rules.


# 8. Recommendations

In order of value per effort:

1. Add T-sel-dyn-str / T-rcd-dyn-str and their algorithmic counterparts
   (definite base-typed key ⇒ ★ + W). Fixes the motivating example. Then fix
   the intro and @sec-goals text, which currently claim ★ for it.
2. Correct the different-keys sentence in the thesis, algorithmic.typ:915 and
   Regressions.lean:391; pin the contextual counterexample as a regression.
3. Add the two singleton key moves of §5; re-measure the keyed universe with
   keys as the only barriers.
4. Decide null keys (§3.2): model or state the divergence.
5. Write the dictionary row (§3.3, the `maybe τ` version) into @sec-extensions as the honest answer
   to "how are most dynamic keys typed"; implementation optional.
6. Key disequalities as the first negative fact (§4), before row lacks-facts.
7. Labels vs strings beyond (1) (§3.1 A/B/C): discussion only.
