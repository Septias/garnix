## Why

Three Nix constructs are currently untypable and all three are the same
construct: `e.${e'}`, `builtins.getAttr e' e`, and `e ? ${e'}`. The label is an
ordinary value. A fourth, dynamic construction `{ ${e} = v; }`, is the same
thing on the other side of the record. @nix-features already claims "a label
sort, first-class labels", §Sorts already promises the third sort, and the prose
under @row-lookup already says the lookup returns `?` when it "encounters a row-
**or label** variable" — a rule that does not exist yet.

Scope, decided 2026-09-26: selection AND construction, mechanized so that
`runSound`, `run_typed` and `runF_terminates` stay green. Branch
`worktree-fc-labels`.


## The design insight

P&X have FC-labels and scoped labels together and pay a price for it. Their
§2.3 example:

    f :: ∀(l: Label). ⌊l⌋ → {l: String, foo: Int} → ?
    f l x = x.foo        -- REJECTED

`l` may be instantiated to `foo`, so the selection's result depends on an
instantiation that has not happened. They reject; the programmer writes
`(x\l).foo`.

`(l: String | foo: Int).foo ↓ ?` is a verdict our lookup relation already has:
the FC-label case is *the same uncertainty as the wand ambiguity, entering
through a second door*:

  - row-var blocker:    (α | foo: Int).foo    — is there a shadowing foo in α?
  - label-var blocker:  (α: τ | foo: Int).foo — is this field a shadowing foo?

Both bottom out at `?`, both refine on instantiation, both park as a stump. So
**? absorbs the ambiguity a lacks-predicate would forbid**: no restriction
operator, no lacks-constraints.

What this does NOT buy, since ★ is rigid (decided 2026-09-26): the selection
types at ★ with a flag, and that ★ cannot be consumed. So P&X's rejection
becomes "typed at ★, usable only parametrically", not "a warning". Say it that
way in the thesis.


## The structural finding: selection never puts a label variable in a row

Record literals carry literal labels, and dynamic selection only emits
`τ₁ ≐ {ρ}` with a FRESH row variable. So with selection alone a label variable
occurs only in a lookup QUERY (`ρ.α ↓ r`) and in ⌊α⌋ — never as a field label.
Rows, ≈ and the row unifier are untouched. Only construction
`{ ${e₁} = e₂ }` produces var-labeled fields, and it carries all of the barrier
work. Hence two phases, and phase A is a complete, sound system on its own.


## Shared representation

    Labels      ℓ ::= l | α                  LabelExp := lit Label | var TyVar
    Types       τ ::= … | ⌊ℓ⌋                Ty.lab : LabelExp → Ty B
    Sorts       κ ::= Type | Row | Label     Kind.label
    Contexts    Γ ::= … | Γ·(α = ℓ)          label solutions

Label equality is three-valued, read after resolving ℓ through Γ's label
solutions:

    Γ ⊢ ℓ₁ ≡ ℓ₂       definitely equal    — same literal, or the SAME variable
    Γ ⊢ ℓ₁ # ℓ₂       definitely apart    — two distinct literals
    otherwise         undecided           — distinct vars, or var vs. literal

`α ≡ α` makes ∀(α:Label). ⌊α⌋ → {α: τ | ρ} → τ typable without a side
condition. Apartness is not the negation of equality — the undecided zone is
where ? lives.

Label variables share the TyVar namespace, as rows and types already do.
Solutions get a third component `lab`, and may map var → var (⌊α⌋ ≐ ⌊β⌋ needs
it); `Sol.Good`'s "no binding mentions a key" keeps them acyclic.


## Phase A — dynamic selection

DONE 2026-09-26 (commits 12ea964..fab89c1). Two deviations from the design
below, both simplifications forced by the proofs:
  - no `LabelExp` / third substitution component: label variables ARE type
    variables and ⌊l⌋ is `Ty.lab : Label → Ty B`. The lookup key is a TYPE
    (`LookupQ`), and a stump's label is a `Ty B`.
  - L-junk: a key that is not a label answers ⊥. Without it (or a kind
    discipline) instance-closedness fails, since nothing stops an instance
    from sending a label variable to `int`.

Rules (paper):

    ℓ₂ ≡ l₁                      ℓ₂ # l₁                     ℓ₂ undecided against l₁
    ------------------- L-hit    ------------------- L-miss  ------------------- L-?-lab
    Γ ⊢ (l₁: τ).ℓ₂ ↓ τ          Γ ⊢ (l₁: τ).ℓ₂ ↓ ⊥          Γ ⊢ (l₁: τ).ℓ₂ ↓ ?

    Γ ⊢ e₁: {ρ}   Γ ⊢ e₂: ⌊ℓ⌋   Γ ⊢ ρ.ℓ ↓ τ
    ----------------------------------------- T-sel-dyn   (+ -★ and -⊥ arms)
    Γ ⊢ e₁.(e₂): τ

L-conc-★ already bubbles a `?` out of the left concatenand, so
`(foo: Int | bar: Bool).α` yields `?` by L-?-lab + L-conc-★ with no new
concatenation rule. The ? of L-?-lab is blocked on the LABEL variable, so the
stump `⟨α ▷ ρ.α ↓ δ⟩` wakes when α is solved — which saturation already
notices, since staleness is judged by `LookupBlocked`.

  - [x] A1. `LabelExp`, `labCmp`, `Ty.lab`, `Kind.label`, the `lab` component of
        `TySubst` / `Sol`; extend `applySubst`, `ftv`, `TyEquiv`, `TyPrec`,
        the head- and ⊑-rigidity inversions
  - [x] A2. `Lookup` over a `LabelExp` query + L-?-lab; `LookupBlocked.labFree`;
        re-prove det / mono (also over label-solution extension) / total /
        mono_prec; `Quiescent.blocker_unsolved` checks both sorts
  - [x] A3. `Expr.lab`, `Expr.selDyn`; Step, Err; `qLab`, `qSelDyn*`;
        `qProgress` / `qPreservation` (canonical forms: a value at ⌊l⌋ is `` `l ``)
  - [x] A4. the flat label pass `≐ₗ` inside the type pass; ⌊·⌋ in the clash set;
        re-prove the type-pass success / clash / occurs / termination legs
  - [x] A5. `Stump.label : LabelExp`, Discharge via `labCmp`,
        `QScheme.applySubst` on stump labels, `IsRenaming` / A-let at the label
        sort, rules A-lab and A-sel-dyn
  - [x] A6. `inferSound` cases, `varCase` / `letCase`, `pinv_keeps`,
        `Correctable.correct`, `runSound`, `inferF` / `runF` + termination
  - [x] A7. headline: `(a: x: x.(a)) :: ∀(α:Label)(β:Row)(δ:Type). ⟨β.α ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ`
        instance-closed (twin of `selQ_instance_closed`); runs for the
        label-refinement example, P&X's example at ★, and the spent promise
        through the second door `λr.λa. (r.(a)) 1` (same incompleteness class)

Gate A → B: everything green, `lean_verify` axioms unchanged, proof-state.md
updated.


## Phase B — dynamic construction (var-labeled fields)

DECIDED 2026-09-26: **not mechanized — paper-only.** Phase A is the mechanized
result; this section is the design the thesis presents for construction. With
label variables as type variables (phase A), a field label would also have to
say what it becomes when its variable is sent to a non-label — the natural
answer is a JUNK label that every key misses (L-junk's twin), and constructing
it is a ↯ error at run time.

    Rows        ρ ::= ε | α | ℓ: τ | (ρ₁ | ρ₂)       Row.sing : LabelExp → Ty B → Row B

    Γ ⊢ ℓ₁ # ℓ₂
    ------------------------------------- ≈-comm      only definite apartness commutes
    (ℓ₁: τ₁ | ℓ₂: τ₂) ≈ (ℓ₂: τ₂ | ℓ₁: τ₁)

Two distinct label variables do not commute (they may be instantiated equal,
and then the swap changes shadowing) — P&X's restriction, for the same reason.

  DEFINITION. An atom is a BARRIER if it is a row-var or a var-labeled field.
  A WINDOW is a maximal barrier-free segment of a spine.

The ≈-characterization generalizes with "var sequence" read as "barrier
sequence"; rows stay a trace monoid.

Row unifier — every "is this side var-free?" becomes "is this side rigid
(barrier-free)?":
  - projClash: `!sHasVar` → `!sHasBarrier`. **The one soundness-critical site**:
    without it `(foo: τ) ≐ᵣ (α: τ′)` reports clash though α ≔ foo solves it.
    Write that regression FIRST.
  - sFieldCount / sLabels count only literal fields ≡ l; groundMatch needs a
    rigid other side
  - U-ε-var: a var-labeled field is a field, so `⟨⟩ ≐ᵣ (α: τ)` clashes
  - windows pair only ≡ fields; an undecided pair goes stuck — never commit a
    guess about shadowing
  - optional: var-refl cancellation generalized to a shared barrier

Cost to measure, because **stuck now rejects**: every undecided field pairing
is a rejected program. Extend Fuzz.lean with var-labeled fields and record the
ledger the way drop-expand.md did.

  - [ ] B1. `Row.sing` over `LabelExp` — mechanical, no proof strategy changes
  - [ ] B2. `Expr.rcdDyn` `{ ${e₁} = e₂ }` (RecBody stays literal); Step
        `rcdDyn (lab l) v → rcd (field l v)`; `qRcdDyn`; A-rcd-dyn
  - [ ] B3. lookup against var-labeled fields; det / mono / total
  - [ ] B4. ≈ with barriers; `rowEquiv_iff_char`, cancellativity,
        `TyPrec.comm_equiv`, the `rowEquiv_hasSing` quartet
  - [ ] B5. the row unifier as above; success_iff / mgu, projClash_no_unifier,
        clash, occurs lift, `Sol.Good` / unifyWF, termination
  - [ ] B6. fuzz ledger + `Regressions.nix_*` for `{ ${n} = v; } // rest`
  - [ ] B7. soundness / termination for A-rcd-dyn; runSound green


## OUT, with a sentence each in @sec-extensions

  - first-class *rows* (P&X's projection). What would want them — `attrNames`,
    `removeAttrs` — wants negative label information instead.
  - lacks-predicates / disjointness: ? absorbs what they would forbid.
  - occurrence typing on `e ? l`: `?` stays Bool.
  - computed strings as labels (`"a" + x`): only literals and passed-through
    labels are ⌊ℓ⌋.


## Open questions

  - Can undecided labels ever be resolved by COUNTING (one rigid field left vs.
    one var-labeled field left)? A rule if cheap, skip if it needs its own proof.
  - Negative information (⟨ρ.l ↓ ⊥⟩ as a constraint) wants the same machinery —
    a label-indexed constraint that wakes on a label solution. Sketch both
    before mechanizing phase B.
