#import "../functions.typ": *

#let meta = [
  - Show the approach we are taking
  - Motivate our own design decisions
  - Should this be after the background discussion?
]


= Overview <sec-overview>
This section introduces MiniNix informally through examples. The formal definitions follow in @declarative.

== Scoped Rows and Three-Valued Lookup
With _scoped records_, concatenation of record types becomes juxtaposition of rows. MiniNix uses rows ρ that hold label-type bindings and row-variables. [Such a row ρ can be put into curly braces to form a record type {ρ}](too informal). For concatenation, the rows of both operands, {ρ} and {ρ'}, are joined to form {ρ | ρ'}, and no information is lost. We combine this with a lookup relation of the form $ρ.l ↓ r$ (@row-lookup) that traverses the natural structure of rows to look up a label l in the row ρ. The result r can be of the following three kinds:

The first two results are a type τ, if the lookup finds the label, and a marker ⊥, if the label is definitely absent. A row can consist of fixed label-type bindings but also row-variables, and our lookup relation stops at these, returning a result ? because shadowing behaviour after this point is not clear. [](good!) Substitution then solves those row-variables and lets the lookup advance further into the record. After all substitution is done, a  $?$ that no substitution resolves is typed at the unknown type ★.

The smallest [interesting](bad word) program is a selector, `x: x.l`. Its argument must be a record ${ρ}$, and the lookup $ρ.l$ decides the result. For $ρ = (l: τ₀)$ the lookup hits and the selector types at ${(l: τ₀)} → τ₀$; for $ρ = ε$ it reports definite absence, and the selector types at ${ε} → ★$, where ★ marks a selection that fails whenever it is run.[¡](this sounds bad if you don't know that ★ is usually only uncertainty) For a row-variable $ρ = β$ the lookup answers $?$, [and the result is ★ as well](only at finalization!).


== The Wand-Ambiguity
The motivating example concatenates two unknown records and selects from the result:
$ #b[let] f = (a: b: (a ‖ b).l) #b[in] … $
With $a : {α}$ and $b : {β}$, concatenation yields ${β | α}$ — the right operand first, since lookup prefers the left of a row. The lookup $(β | α).l$ stops at β with $?$: whether the field comes from b or from a depends on whether β contains l. A plain type can only record this as ${α} → {β} → ★$. MiniNix instead keeps the pending lookup in the scheme of f,
$ f : ∀(α, β: "Row", δ: "Type"). #h(0.3em) ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ $
and replays it at every use: `f {l = c₁} {}` looks up $(ε | l: 𝓫_(c₁)).l$ and yields $𝓫_(c₁)$, `f {l = c₁} {l = c₂}` yields $𝓫_(c₂)$ because the right operand shadows, and `f {} {}` yields ★. Each use receives the most precise type its arguments permit.

The same mechanism lets a single binding serve incompatible uses,
$
  #b[let] f = (x: x.l) #b[in] { a = (f {l = {m = c}}).m; b = f {} } quad : quad {a: 𝓫_c | b: ★}
$
The first use needs the result of f to be a record, the second applies f to a record without l. No plain scheme covers both, and @principality [shows that this forces qualified schemes](wording).

The selector also shows where MiniNix departs from Paszke and Xie @extensible_tabular. Their search rule turns the selection into the demand that the row of x contain l, and since that row is a variable, unification places the field into it: the selector types at ${(l: δ | ρ)} → δ$. Every use must then supply l, so `f {}` is rejected, and with it the program above. MiniNix answers the lookup with $?$ instead and keeps it in the scheme of f, whose instances include ${ε} → ★$. On `a: b: (a ‖ b).l` the search must decide whether the row of b contains l, which their tail-check cannot, while MiniNix again keeps the lookup pending.


// meta: this seems out of place, it might be needed soon, but needs a better introduction then
// == A Stump Wakes Up
// [Inference cannot always answer a lookup when it meets it](wording). In `(x: x.l) {l = c}`, the selector is inferred first: x receives a fresh row ρ, the lookup $ρ.l$ answers $?$, and inference returns a fresh δ for the result while _deferring_ the pending lookup as a stump $⟨ρ ▷ ρ.l ↓ δ⟩$, [blocked on ρ](explain the structure of a stump? A general idea about a stump would be helpful). Applying the selector then solves $ρ ≔ (l: 𝓫_c)$. The solution unblocks the stump, the lookup is re-asked and hits, and δ is unified with $𝓫_c$. For `(x: x.l) {}` the same stump wakes to definite absence, δ becomes ★, and a warning records the place. Row unification itself never sees the field demand; @inference develops this mechanism in full.
