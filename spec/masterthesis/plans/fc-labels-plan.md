## Why

Three Nix constructs are currently untypable and all three are the same
construct: `e.${e'}`, `builtins.getAttr e' e`, and `e ? ${e'}`. The label is an
ordinary value. @nix-features already claims "a label sort, first-class
labels" as the forced consequence, §Sorts already promises the third sort, and
the prose under @row-lookup already says the lookup returns `?` when it "encounters
a row- **or label** variable" — a rule that does not exist. FC-labels are the
one extension the thesis has already committed to in text and not delivered.


## The design insight

P&X have FC-labels and scoped labels together and pay a price for it. Their
§2.3 example:

    f :: ∀(l: Label). ⌊l⌋ → {l: String, foo: Int} → ?
    f l x = x.foo        -- REJECTED

`l` may be instantiated to `foo`, so the selection's result depends on an
instantiation that has not happened. They reject, and the programmer must write
`(x\l).foo` to recover.

We do not reject. `(l: String | foo: Int).foo ↓ ?` is exactly the verdict our
lookup relation already has, and T-sel-★ already types it. The FC-label case is
*the same uncertainty as the wand-ambiguity, entering through a second door*:

  - row-var blocker:    (α | foo: Int).foo    — is there a shadowing foo in α?
  - label-var blocker:  (α: τ | foo: Int).foo — is this field a shadowing foo?

Both are "a field that may or may not shadow", both bottom out at `?`, both
refine on instantiation, both park as a stump. The claim to make is that **★ is
what buys FC-labels without a restriction operator**, and that our soft-typing
position converts P&X's rejection into a warning. This is the strongest
argument for ★ in the thesis that is not about `‖`, and it is currently unmade.


## Scope

IN (phase 1):
  - label literals as terms and a label singleton type ⌊ℓ⌋
  - label variables, a `Label` sort, label-polymorphic schemes
  - dynamic *selection* `e₁.(e₂)`
  - label solutions in Γ, hence label *refinement* on application

IN (phase 2, only if phase 1 lands cleanly):
  - dynamic *construction* `{ ${e} = e'; }` — costs a new Err/Step case and
    makes RecBody.lookup three-valued at the term level. Nix has it; it is rare
    and it is separable. Do NOT bundle it into phase 1.

OUT, with a sentence each in @sec-extensions:
  - first-class *rows* ⟨ρ⟩ (P&X's • projection). Nix has no row value; what
    would want them — `attrNames`, `removeAttrs`, `intersectAttrs` — wants
    negative label information instead. Cross-reference the negative-info plan.
  - lacks-predicates / disjointness. The whole point is that we do not need
    them: ? absorbs the ambiguity a lacks-constraint would forbid.
  - occurrence typing on `e ? l`. `?` stays Bool. Separate extension.


## 1. Syntax

    Terms       e ::= … | `l | e₁.(e₂)              -- e.l stays as sugar for e.(`l)
    Labels      ℓ ::= l | α                         -- literal or label-var
    Types       τ ::= … | ⌊ℓ⌋
    Rows        ρ ::= ε | α | ℓ: τ | (ρ₁ | ρ₂)      -- field label is now an ℓ
    Sorts       κ ::= Type | Row | Label
    Contexts    Γ ::= … | Γ·(α = ℓ)                 -- label solutions, cf. (α = ρ)

One grammar change does the work: `l: τ` becomes `ℓ: τ`. Everything downstream
is a consequence of that plus the new sort.


## 2. The label judgement (everything rests on this)

Label equality is three-valued, resolved under Γ's label solutions first:

    Γ ⊢ ℓ ⇓ ℓ′        resolve α through (α = ℓ) ∈ Γ, else itself

    Γ ⊢ ℓ₁ ≡ ℓ₂       definitely equal    — same literal, or the SAME variable
    Γ ⊢ ℓ₁ # ℓ₂       definitely apart    — two distinct literals
    otherwise         undecided           — distinct vars, or var vs. literal

Note `α ≡ α`: a label-var compared against itself is definitely equal, which is
what makes ∀(α:Label). ⌊α⌋ → {α: τ | ρ} → τ typable without a side condition.
Note also that apartness is *not* the negation of equality — the undecided zone
between them is precisely where ? and ★ live.

Every existing side condition of the form `l₁ = l₂` / `l₁ ≠ l₂` becomes `≡` /
`#`, and every rule whose trigger was an exhaustive two-way split acquires a
third arm. There are exactly three such rules: L-hit/L-miss, ≈-comm, and the
field-matching moves of ≐ᵣ.


## 3. Sorting  (@sorting, algorithmic.typ §Sorts)

New:

    α: Label ∈ Γ                                            Γ ⊢ ℓ: Label
    ------------- S-lab-var      ------------ S-lab-lit     ------------- S-lab-ty
    Γ ⊢ α: Label                 Γ ⊢ l: Label               Γ ⊢ ⌊ℓ⌋: Type

Changed:

    Γ ⊢ ℓ: Label   Γ ⊢ τ: Type
    ---------------------------- S-field
    Γ ⊢ (ℓ: τ): Row

I-inst is UNCHANGED — θ is already a sort-respecting map over an annotated
quantifier list, so a Label quantifier costs nothing. This is the concrete
payoff of @sorting and should be said there. But: §Sorts currently claims FC-
labels "add a third sort and change nothing else about the discipline". That
sentence is true of *instantiation* and false of ≈ and of ≐ᵣ. Fix it — the
honest version is "add a third sort and cost nothing at instantiation", with a
forward reference to the barrier discussion below.


## 4. Row lookup  (@row-lookup) — the core change

    ---------- L-ε            unchanged
    Γ ⊢ ε.ℓ ↓ ⊥

    Γ ⊢ ℓ₁ ≡ ℓ₂                     Γ ⊢ ℓ₁ # ℓ₂
    -------------------- L-hit      -------------------- L-miss
    Γ ⊢ (ℓ₁: τ).ℓ₂ ↓ τ              Γ ⊢ (ℓ₁: τ).ℓ₂ ↓ ⊥

    ℓ₁, ℓ₂ undecided
    -------------------- L-?-lab            NEW
    Γ ⊢ (ℓ₁: τ).ℓ₂ ↓ ?

L-α, L-α-free, L-conc-hit, L-conc-skip, L-conc-★ are unchanged *as stated* —
and this is the whole point. L-conc-★ already bubbles a `?` out of the left
concatenand, so `(α: τ | foo: Int).foo` yields `?` by L-?-lab + L-conc-★ with
no new concatenation rule. The soft-typing machinery absorbs FC-labels for
free at exactly the place it was built to absorb them.

Metatheory of ↓ to re-establish (all three already proved, all three should
survive):
  - [ ] determinism (lookup_det): the three label arms are mutually exclusive
        by construction of ≡/#/undecided — trivial, but it is a genuine new
        case split in the proof.
  - [ ] monotonicity (lookup_mono): must now be stated over Ctx.RowExt *and*
        a label-solution extension. An undecided pair can become ≡ or # when a
        label-var is solved — i.e. L-?-lab is a second source of improvable ?,
        which is exactly right and mirrors L-α-free.
  - [ ] totality (lookup_total): needs the same rank argument for label
        solutions. Cheaper than the row case: labels have no structure, so a
        label solution chain is a substitution chain over a flat domain — an
        acyclicity side condition on (α = ℓ) suffices, or forbid label-vars on
        the right of a label solution entirely (recommended: label solutions
        map vars to *literals* only, `α = l`. This is a real simplification and
        costs nothing, since unification only ever binds a label-var to the
        other side, and if the other side is a var, orient the binding).


## 5. Row equivalence  (@row-equivalence) — the barrier

    Γ ⊢ ℓ₁ # ℓ₂
    ------------------------------------- ≈-comm
    (ℓ₁: τ₁ | ℓ₂: τ₂) ≈ (ℓ₂: τ₂ | ℓ₁: τ₁)

Only definite apartness commutes. Two distinct label-*variables* do not commute
(they may be instantiated equal, and then the swap changes shadowing) — this is
P&X's restriction, arrived at for the same reason, and worth citing as such.

Consequence, and the load-bearing new concept for the rest of the plan:

  DEFINITION. An atom is a BARRIER if it is a row-var or a var-labeled field.
  A WINDOW is a maximal barrier-free segment of a spine.

Today barrier = row-var. The spine characterization in algorithmic.typ §Spines
("same var sequence and, per label, the (segment index, type) lists agree")
generalizes verbatim with "var sequence" read as "barrier sequence". Rows stay
a trace monoid, so cancellation at both ends still holds, so U-var-refl-L/R
survive — but they should be generalized to cancel a shared *barrier*, not just
a shared row-var, otherwise `(α: τ)·t₁ ≐ᵣ (α: τ)·t₂` is needlessly stuck.

  - [ ] re-prove the ≈-characterization with barriers
  - [ ] re-prove left/right cancellativity
  - [ ] check ≈ is still decidable (it is: ≡/# are decidable, undecided is
        decidable, and a var-labeled field only ever matches itself)


## 6. Unification

### 6a. The label pass  (new, cheap)

    -------------- U-lab-refl        Γ ⊢ ℓ₁ ≡ ℓ₂ resolves this
    α ≐ₗ α ⇝ ∅

    ℓ ≠ α                            l₁ ≠ l₂
    ------------------ U-lab-bind    ------------------- U-lab-clash
    α ≐ₗ ℓ ⇝ [α ≔ ℓ]                 l₁ ≐ₗ l₂ ⇝ clash

No occurs check: labels are flat, a binding always eliminates its variable,
the pass is non-recursive and consumes no fuel. Lift into the type pass:

    ℓ₁ ≐ₗ ℓ₂ ⇝ v
    ----------------- U-lab
    ⌊ℓ₁⌋ ≐ ⌊ℓ₂⌋ ⇝ v

and ⌊·⌋ joins the head-constructor clash set of U-clash (⌊ℓ⌋ ≐ ★ is a clash —
★ stays rigid).

### 6b. The row pass — one refactor, applied everywhere

Every trigger in the cascade that asks **"is this side var-free?"** must be
re-asked as **"is this side rigid?"** (= barrier-free). A var-labeled field can
become any label, so for every counting, uniqueness or exhaustion argument it
behaves exactly like a row-var. Concretely, against @unification-cascade:

  - U-ε-var   `⟨⟩ ≐ᵣ s`, s field-free. A var-labeled field IS a field, so it
              does not vanish: `⟨⟩ ≐ᵣ (α: τ)` falls to U-ε-clash. This case
              gets *better*, not worse — clash, not stuck.
  - U-ε-clash unchanged (any field, literal- or var-labeled).
  - U-var-refl-L/R  generalize from "shared row-var" to "shared barrier" (§5).
  - U-var-solve / U-var-occurs  `vars(s)` must become `barriers(s)`? NO — the
              occurs check is about the row-var α appearing in s; a var-labeled
              field `β: τ` does not contain α unless τ does. Keep `vars`, but
              check the ftv of field payloads is already covered. FLAG: verify
              against the existing sort-indexed occurs discussion.
  - U-field-L/R  may pair only fields with `Γ ⊢ ℓ₁ ≡ ℓ₂`, and the window must
              end at a barrier. Pairing an undecided pair would COMMIT
              `α ≔ foo` — a guess about shadowing, exactly what the window
              discipline exists to forbid. Keep every move forced; *let the
              cascade die and report stuck instead.*
  - U-ground  precondition becomes "other side is rigid"; `|s|_ℓ` counts only
              fields whose label is ≡ ℓ. The counting argument survives
              verbatim once barriers replace vars.
  - U-expand  `uniqueHost` must count var-labeled fields as candidate hosts
              (a `β: τ` field can become the `l` field). So a side with one
              row-var and one var-labeled field has TWO hosts and U-expand
              refuses. Strictly more refusals, i.e. strictly more stuck — sound,
              conservative, and the right default.
  - U-clash   projClash's `!sHasVar s₂` becomes `!sHasBarrier s₂`. Without
              this the rule is UNSOUND: `(foo: τ) ≐ᵣ (α: τ′)` would be reported
              clash though α ≔ foo solves it. **This is the one place where
              getting the refactor wrong breaks a proved-sound verdict.** Do
              this one first and test it first.
  - U-stuck   unchanged; it now also catches the P&X-rejection shapes.

The invariant to preserve and to state as a lemma: *no move ever binds a label
variable unless U-lab-bind reached through a definitely-forced field pairing.*
The algorithm never guesses a label into a field, exactly as it never guesses a
field into a row-var outside the unique-host case.


## 7. Stumps, solver state, wake-up  (algorithmic.typ)

    q ::= ⟨ρ.ℓ ↓ δ⟩                       label may now be a variable
    Δ ::= ∅ | ⟨b ▷ ρ.ℓ ↓ δ⟩, Δ            b is a row-var OR a label-var

The blocker index is the change with teeth: wake-up currently fires when a row
solution α ≔ ρ is written. It must now also fire when a **label** solution
α ≔ l is written, because L-?-lab's ? is discharged by solving a label-var, not
a row-var. K-hit / K-⊥ / K-repark / F-★ are otherwise unchanged; K-repark's
"progressed to the next var" becomes "progressed to the next barrier".

  - [ ] monotonicity of wake-up still holds (definite results stay definite —
        follows from lookup_mono once that is restated over label solutions)
  - [ ] determinism still holds (lookup is deterministic; unchanged argument)

Inference rules to add/change:

    ------------------- A-lab
    Γ; S ⊢ `l ⇒ ⌊l⌋; S

    Γ; S ⊢ e₁ ⇒ τ₁; S₁   fresh ρ: Row   S₁ ⊢ τ₁ ≐ {ρ} ⇝ S₂
    Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃   fresh α: Label   S₃ ⊢ τ₂ ≐ ⌊α⌋ ⇝ S₄
    ⟦S₄⟧ ⊢ ρ.α ↓ r
    -------------------------------------------------------- A-sel-dyn
    Γ; S ⊢ e₁.(e₂) ⇒ …; S₄        -- three arms on r, exactly as A-sel/⊥/?

The `?`-arm parks `⟨b ▷ ρ.α ↓ δ⟩` where b is whichever barrier produced the ?.
A-sel stays as the derived rule for `e.(`l)`.

Generalization (A-let) needs ftv at three sorts and `κ̄ = Γ(ᾱ)` already handles
Label. The stump-carry fixpoint is unchanged in shape.


## 8. Metatheory

  - Values: `` `l `` is a value. Trivial new case in progress.
  - Step:   selDyn on `(.rcd b).(`l)` reduces exactly like sel; the label
            argument must be evaluated first (new congruence rule).
  - Err:    dynamic selection of a label the record lacks — same shape as
            selAbsent, so the ↯-disjunct absorbs it without a new disjunct.
  - Preservation: unchanged in statement. The ⊑-refinement story gains a second
            source: label solutions sharpen ? just as row solutions do.
  - [ ] The headline scheme to prove and to put in the thesis, the FC-label
        twin of the existing ∀(β:Row)(δ:Type). ⟨β.l ↓ δ⟩ ⇒ {β} → δ:

            (a: x: x.(a))  ::  ∀(α:Label)(β:Row)(δ:Type). ⟨β.α ↓ δ⟩ ⇒ ⌊α⌋ → {β} → δ

        Two blockers, two sorts, one stump. If this typechecks in the
        mechanization, the extension is done.


## 9. Mechanization plan (Lean)

The invasive bit: `Row.sing : Label → Ty B → Row B` becomes
`Row.sing : LabelExp → Ty B → Row B` with

    inductive LabelExp | lit : Label → LabelExp | var : TyVar → LabelExp

and every `if l == l'` site becomes a match on a three-valued comparator. That
touches minimal.lean, RowEquiv.lean and all of RowUnify/. Do it in this order —
each stage must build and keep the axiom count at zero before the next starts:

  - [ ] S0. `LabelExp` + `labCmp : LabelExp → LabelExp → LabCmp` (eq/apart/
        undecided) + decidability. Label solutions restricted to `α = lit l`
        (§4). Standalone, no dependencies.
  - [ ] S1. `Row.sing` generalized. Mechanical breakage everywhere; fix by
        rote, change no proof strategy. Expect this to be a day of nothing but
        renaming.
  - [ ] S2. Lookup: the L-?-lab arm + re-prove det / mono / total.
  - [ ] S3. RowEquiv: ≈-comm side condition + the barrier-based ≈-character-
        ization + cancellativity.
  - [ ] S4. RowUnify/Defs: `sHasBarrier` replacing `sHasVar` in projClash,
        `allVarsEmpty`, the window functions, `uniqueHost`, `sFieldCount`.
        Do projClash FIRST (§6b — it is the one soundness-critical site) and
        write the `(foo: τ) ≐ᵣ (α: τ′)` regression before touching it.
  - [ ] S5. Re-prove the success leg. `unifyRow_success_sound` incl. the
        groundMatch counting is currently the ONE fully-done axiom-clean
        result; U-ground's counting is exactly what var-labeled fields
        perturb. Budget the most time here.
  - [ ] S6. Clash leg (soundness) — projClash's generalization is the content.
  - [ ] S7. The label pass ≐ₗ + its lifting into ≐. Cheap, do it whenever.

RISK. The occurs and stuck legs are already open and already known incomplete
(three refuted formulations — see proof-state.md). FC-labels ADD stuck shapes
(every undecided field pairing) and add no new proof obligation to those legs,
since neither has a converse to lose. So: the extension does not worsen the
open legs, but it does mean the stuck class grows, and the thesis text about
what stuck *means* needs the FC-label shapes listed alongside the wand shape.

DE-RISKING OPTION. If S1 proves too expensive against the deadline, the
fallback is to keep the mechanization at literal labels and present FC-labels
paper-only, with §9's headline scheme as a hand proof. This is defensible —
the contribution is the design argument of §"The design insight", not the
mechanization — but it should be a deliberate decision, not a drift. Decide
after S0+S2, which are cheap and which already tell you whether the lookup
metatheory survives.


## 11. Worked examples to add

  - [ ] `(a: {foo = 1; bar = true;}.(a))` ⇒ ⌊α⌋ → δ with ⟨(foo:Int|bar:Bool).α ↓ δ⟩,
        applied to `` `foo `` refines δ ≔ Int. THE label-refinement example, and
        the one that proves the Δ blocker index needed generalizing.
  - [ ] the P&X rejection `(l: String | foo: Int).foo` ⇒ ★ + W-flag, side by
        side with their `(x\l).foo` workaround. THE motivating contrast.
  - [ ] `(foo: τ) ≐ᵣ (α: τ′)` ⇒ [α ≔ foo] — the projClash regression.
  - [ ] `⟨⟩ ≐ᵣ (α: τ)` ⇒ clash — a var-labeled field cannot vanish.
  - [ ] a two-host stuck: `(l: τ)·⟨⟩ ≐ᵣ (α: τ′)·β` — U-expand refuses on two
        candidate hosts, one of which is a var-labeled field.
  - [ ] the currentTime example from @sec-motivation now actually typechecks
        in the calculus rather than only in prose.


## 12. Open questions

  - Should label solutions be allowed to map var → var? Recommended NO (§4);
    confirm nothing in unification needs it. If U-lab-bind is only ever reached
    with a literal on one side this is automatic — check.
  - Does `α ≡ α` interact badly with generalization? Two occurrences of the
    same label-var in one row (`α: τ₁ | α: τ₂`) is a legitimate shadowing pair
    and must NOT commute; confirm ≈-comm's `#` handles it (it does — α # α is
    false) and that the spine characterization keeps their order.
  - Is there an FC-label analogue of U-ground's counting, i.e. can undecided
    labels ever be resolved BY counting rather than by pairing? Probably: if
    one side is rigid and has exactly one field left and the other has exactly
    one var-labeled field left, the pairing is forced. Worth a rule if it is
    cheap; skip if it needs its own proof.
  - Negative information (⟨ρ.l ↓ ⊥⟩ as a constraint, the other claude prompt)
    and FC-labels want the same machinery — a label-indexed constraint that
    wakes on a label solution. Do NOT design them independently; sketch both
    before mechanizing either.
