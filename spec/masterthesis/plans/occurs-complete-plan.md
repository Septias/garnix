# Making `.occurs` a SOUND verdict

Scope: `lean/RowUnify/Defs.lean` (`solveVarM`, `bindTy`) and the two no-unifier
theorems in `NoMgu.lean`. Goal is a *converse-shaped* theorem — the only leg of
the trichotomy where one is reachable.

> ⊢  `unifyRowM fuel ρ₁ ρ₂ = .occurs`  ⟹  `¬ ∃ θ. Unifies θ ρ₁ ρ₂`

Today this is FALSE (`occurs_allVar_unifiable`, NoMgu.lean:1027). The claim of
this plan is that it is false only because the algorithm does not dispatch on a
case analysis that is *already fully mechanized*.

---

## Why it is reachable: the trichotomy already exists

Every configuration `solveVarM` can reach — `α ≐ᵣ s₂` with α occurring in s₂ —
falls into exactly one of three cases, and all three are proved:

| configuration | theorem | says | generality |
|---|---|---|---|
| α under a record constructor (`Row.deepRowVars`) | `deep_occurs_no_unifier` (NoMgu.lean:706) | no unifier | ✔ arbitrary `ρ` |
| α on the spine, some field present | `occurs_field_no_unifier` (NoMgu.lean:1011) | no unifier | ✔ arbitrary `s₂` |
| α on the spine, s₂ all-variable | `occurs_allVar_hasMgu` (NoMgu.lean:1045) | **has an mgu** | ✘ the 3-atom witness only |

The first two are already stated at arbitrary spines — no generalization needed.
Only the third is a witness rather than a rule, and its proof *constructs* the
solution: field counting (`rowEquiv_fieldCount_eq`) kills every field in θβ, θγ;
the var-sequence equation `A = B ++ A ++ C` from `RowEquiv.char` forces
`|B| = |C| = 0` by length; field-free + var-free is ε.

So the missing artefact is an algorithm arm, not a theorem.

## The rule, derived

In case 3, `s₂` is a pure variable spine `[γ₁ … γₙ]` containing α exactly k ≥ 1
times. A unifier satisfies `varseq(θα) = varseq(θγ₁) ++ … ++ varseq(θγₙ)`, hence

    |varseq(θα)|  =  k·|varseq(θα)|  +  Σ_{γᵢ ≠ α} |varseq(θγᵢ)|

* **k = 1** ⟹ every *other* variable has empty var sequence, and field counting
  makes it field-free, hence ε. α itself is unconstrained.
  Solution: `γᵢ ≔ ε` for every γᵢ ≠ α.
* **k ≥ 2** ⟹ `|varseq(θα)| = 0` as well, so *every* variable collapses.
  Solution: `γᵢ ≔ ε` for all i, α included.

Both are forced, so both are mgus. (`α ≐ᵣ (α)` is degenerate and unreachable —
`stripL` consumes a shared leading atom before `solveVarM` is tried — but the
counting above covers it anyway; assert that rather than rely on it.)

## The algorithm change

`Defs.lean:472`, today:

```lean
def solveVarM {B : Type} (S : Supply) : List (Atom B) → List (Atom B) → Option (UResM B)
  | [.var α], s₂ =>
      some (if (Row.allRowVars (ofSpine s₂)).contains α then .occurs
            else .success (Sol.ofRow [(α, ofSpine s₂)]) S)
  | _, _ => none
```

becomes a three-way dispatch: `.occurs` when α is deep or a field is present,
the ε-collapse solution otherwise, and the existing binding when α does not
occur at all.

---

## Stages

### Stage 1 — generalize case 3  (additive, no algorithm change, zero risk)

    allvar_occurs_mgu :
      sFieldCount l s₂ = 0 for every l  →  α ∈ sVarSeq s₂  →
        HasMgu (.var α) (ofSpine s₂)     -- with the explicit ε-collapse witness

Lift the body of `occurs_allVar_hasMgu` from the three-atom instance to an
arbitrary pure-variable spine. Existing tools: `rowEquiv_fieldCount_eq`,
`RowEquiv.char`, `sVarSeq_append`, `sFieldCount_append`, and the "field-free +
var-free ⟹ ε" step already written inside the witness.

Split out the two k-cases; the k ≥ 2 branch is new and is where `α ≔ ε` comes
from.

### Stage 2 — split the guard in `solveVarM`

**This is the first occurs change that is NOT proof-churn-free.** The depth-aware
occurs check and the self-reference filter could only ever turn `.success` into
`.occurs`, so the five proofs touching those branches `split` and never read the
condition. This stage goes the *other* way: it creates new successes, and both
success legs acquire a real arm.

Touch list:

| site | why it moves |
|---|---|
| `Soundness.lean:37` `solveVarM_reflect` | must prove the ε-collapse solution unifies |
| `Completeness.lean:638` `solveVarM_complete` | must prove every unifier meets it |
| `Completeness.lean:111` `solveVarM_bounded` | the new solution mentions only s₂'s own vars, so the bound is easier than the existing one; mechanical |
| `Clash.lean:43` `solveVarM_ne_clash` | one more branch to rule out; mechanical |
| `Driver.lean:197` fuel lemma | unchanged — the arm returns `.inr rfl` whatever the verdict |
| `Terminal` (`Defs.lean:626`) `hsolveL`/`hsolveR` | unchanged — they only read `none`, i.e. the spine shape |

One favourable detail: the completeness call sites (Completeness.lean:935, 941)
discharge this arm with `AgreeOn.refl θ V` — the solve arm invents no variables,
so no unifier has to be *extended*, only shown to meet the solution. The
ε-collapse rule invents nothing either, so that shape survives unchanged. This is
strictly easier than the `expandL` arm, which threads δ and β′.

### Stage 3 — the soundness theorem

    unifyM_occurs_no_unifier :
      unifyRowM fuel ρ₁ ρ₂ = .occurs  →  ¬ ∃ θ. Unifies θ ρ₁ ρ₂

An induction mirroring `unifyM_clash_no_unifier` (Clash.lean), discharging the
`solveVarM` arm with `deep_occurs_no_unifier` / `occurs_field_no_unifier`.

### Stage 4 — sorted ftv at `bindTy`  (REQUIRED, not optional)

`.occurs` has TWO sources. `bindTy` (`Defs.lean:466`) still tests the sort-blind
`τ.ftv`, and `Regressions.tyM_occurs_cross_sort` pins the resulting false
rejection: `x ≐ {x}` is reported occurs although `θ.ty x = {ε}, θ.row x = ε`
solves it. Stage 3's theorem is therefore **unprovable without this stage**.

Replace `τ.ftv.contains α` with the sort-correct test (`Ty.sortedFtv`,
State.lean). Sortedness is already the established principle at the row level —
`a ≐ᵣ (l:{a})` → occurs (row var) while `a ≐ᵣ (l:a)` → success (type var) — and
`Sol.NoCapture`'s refutation is the same observation: cross-sort self-reference
is not a cycle. This stage subsumes the "sorted ftv, second half" open item in
proof-state.md.

### Stage 5 — regressions and sweep

* `occurs_allVar_reported` (Driver.lean:42) asserts `= .occurs` **by `rfl`** and
  its statement becomes FALSE. Replace with the success regression, the way
  `cyclic_occurs_reported` replaced the two buggy-verdict theorems.
* `Fuzz.landmarks` (Fuzz.lean:510) — relabel the all-var entry.
* Re-run all three universes. **Unlike the earlier occurs work, the tripwires
  matter here**: this stage manufactures successes, so `solRankedB` / `solAcyclicB`
  must stay at their current counts. Any new ill-formed solution is a bug in the
  ε-collapse rule, not a pre-existing one.

---

## Documentation drift

Everything below currently asserts that the occurs guard is *deliberately*
conservative. All of it is invalidated by this plan.

### Statements that become FALSE (must change with the code)

| site | today |
|---|---|
| `RowUnify/Driver.lean:42` | `occurs_allVar_reported : … = .occurs := rfl` |
| `Regressions.lean:178` | `tyM_occurs_cross_sort : … = .occurs := rfl` (flips at Stage 4) |

### Prose that drifts

| site | says | becomes |
|---|---|---|
| `RowUnify/Defs.lean:455-458` | bindTy's sort-blind ftv is "the same conservatism the row occurs guard has … Deliberate" | both halves gone — the row guard is sound and bindTy is repaired |
| `RowUnify/Defs.lean:618-621` | "`.stuck` is CONSERVATIVE, exactly as `.occurs` is" | the analogy dies; stuck stands alone |
| `RowUnify/Trichotomy.lean:141` | same analogy | same |
| `RowUnify/Trichotomy.lean:285` | OPEN list: "The occurs guard stays deliberately conservative" | delete — it is closed |
| `RowUnify/NoMgu.lean:978` | "the occurs verdict (`solveVarM` hitting `(sVarSeq s₂).contains α`)" | **doubly stale** — the test became `Row.allRowVars` with the depth-aware check and is now a three-way split |
| `RowUnify/NoMgu.lean:1022-1044` | frames `occurs_allVar_unifiable` / `_hasMgu` as incompleteness witnesses | keep the theorems, reframe: they are now the *correctness proof of the solve rule* |
| `Refutations.lean:7` | header cites occurs conservativity as stuck's sibling | stuck is alone |
| `Refutations.lean:336-338` | "the spine-occurrence conservativity of `occurs_allVar_hasMgu` is untouched by it" | it is now touched |
| `Axioms.lean:28` | "The occurs guard's incompleteness, sharply" + cross-ref to `occurs_allVar_reported` | reframe; add the new theorems to the axiom guard |
| `Fuzz.lean:510` | landmark "occurs, all-var (conservative — HAS an mgu)" | relabel |

### Repo documents

| site | change |
|---|---|
| `typesystems/proof-state.md:53-54` | `[¡] occurs: *incomplete*` + the `α ≐ᵣ (β\|α\|γ)` witness → `[x] occurs: sound` |
| `typesystems/proof-state.md` Open | "sorted ftv, second half" merges into Stage 4 here |
| `plans/inference-gap-analysis.md` §A | the *occurs soundness* row reads `◐ genuine case ✔; occurs_allVar_hasMgu refutes the general form`, and its "what is missing" column asks for a guarded statement that "has never been formulated". This plan formulates it → row becomes ✔ |
| `typesystems/algorithmic.typ` | CHECK — the `S-*` rules' description of the occurs check may describe the sort-blind guard |

### Thesis (draft here, do not edit `thesis.typ` directly)

§Metatheory's "Where unification gives up, and why there is no converse" lists
**three** conservativity witnesses: `occurs_allVar_hasMgu`, `stuck_masks_mgu`,
`terminal_masks_mgu`. After this plan it lists **two**, and the occurs leg moves
from the give-up list to the proved list. The sharper story the section should
then tell:

> The three verdicts are not equally conservative, and the asymmetry is
> structural. For `.occurs` the case analysis closes: an occurrence is either
> under a record constructor, or field-pinned on the spine — both genuinely
> unsolvable — or purely variable, where counting forces a unique solution. So
> `.occurs` is sound. For `.stuck` no such analysis exists at any formulation
> tried, because the give-up set cannot be made to coincide with the no-mgu set
> while the row theory is non-unitary.

Also check the "what is not proved" list for an occurs entry.

---

## Risks

1. **The unsafe direction.** Previous occurs work could only add rejections, so
   every change was conservative by construction. This adds *acceptances*; each
   one rests on Stage 1's mgu proof being right at arbitrary spines.
2. **Not churn-free.** Success soundness and success completeness both gain an
   arm (Stage 2's table).
3. **Two sources of `.occurs`.** Stage 3 is unprovable without Stage 4. Do not
   start Stage 3 first.
4. **Interaction with the open vacuous-success class.** New successes are new
   solutions; `solRankedB` is the detector and must not move.

## Payoff

* `occurs ⟹ no unifier` — the only converse-shaped theorem available anywhere in
  this development.
* Closes the *occurs soundness* row of `inference-gap-analysis.md` §A.
* Subsumes the "sorted ftv, second half" open item.
* Turns "all three verdicts are conservative give-ups" into a *result about the
  asymmetry between them*, which is a better section than the one the thesis
  currently has.
