# Proof plan: mutually recursive ≐ / ≐ᵣ


## 0. Why: two defects with one shared cause

The row pass `unifySpineF` is a *single* pass over two spines. Type equations it
discovers are EMITTED (`URes.success σ eqs`, `URes.addEq`), never solved. Two
known defects trace back to exactly that:

1. **The stuck leg needs a side condition.** `unify_eq_rescued_stuck`
   (`Regressions.lean:49`): `(k:{β} | β | α) ≐ᵣ (k:{l:𝓫} | l:𝓫)`. `matchL` peels
   the shared `k`, emitting `{β} ≐ {l:𝓫}`; the residual `(β | α) ≐ᵣ (l:𝓫)` is
   Wand, so the pass answers `.stuck` — yet the emitted equation forces
   `β ≈ (l:𝓫)`, hence `α ≈ ε`, and the whole problem has a UNIQUE mgu. So
   `unifySpineF_stuck_no_mgu` (`RowUnify.lean:2722`) can only be stated as a
   REDUCTION to a base hypothesis `hbase`, and `hbase` is false in general.

   The 19921ae attempt (`Indep Q V` + `patchRow` + Q-carrying base techniques)
   made the side condition *checkable* but left it a HYPOTHESIS. It has been
   reverted; this plan makes it an INVARIANT instead.

2. **The stuck class is too big.** `crossfield_stuck_unifiable`
   (`RowUnify.lean:952`): `(l:𝓫 | α) ≐ᵣ (m:𝓫 | β)`, `l ≠ m`, is
   reported `.stuck` yet has (by hand, off `rowEquiv_iff_char`) the mgu
   `α ↦ (m:𝓫 | X)`, `β ↦ (l:𝓫 | X)`. The missing rule is Rémy-style variable
   EXPANSION when the host is unique.

Both are the same shortfall: the algorithm refuses to let information found in
one place act on another place. Fixing (2) alone would not kill the side
condition; fixing (1) alone leaves a stuck class no base technique can kill
(the witnesses all need ≥ 2 candidate hosts, which is precisely the case
expansion does NOT apply to). **Do them in one rebuild.**

## 1. Target shape

### 1.1 Results and solutions

Solutions now exist at both sorts — `TySubst` has `ty` and `row` over one shared
`TyVar` namespace (`minimal.lean:649`, and `Scheme` quantifies both sorts at
once), so a variable bound by the type pass can be read by the row pass.

```lean
structure Sol (B : Type) where
  ty  : List (TyVar × Ty B)
  row : List (TyVar × Row B)

inductive UResM (B : Type) where
  | success : Sol B → UResM B     -- NO eqs component: everything is solved
  | clash | occurs | stuck
```

Dropping `eqs` from `success` is the point of the exercise. `SolSat` generalizes
to both lists; `EqsSat` disappears from the success statement (it survives only
as a proof-internal device).

### 1.2 The mutual definition

```lean
mutual
  def unifyTyF : Nat → Ty B → Ty B → UResM B
    | _, .base b, .base b'   => if b = b' then .success ∅ else .clash
    | _, .var α, τ           => bindTy α τ        -- occurs check at BOTH sorts
    | _, τ, .var α           => bindTy α τ
    | fuel+1, .fn a b, .fn a' b' =>
        seq (unifyTyF fuel a a') (fun θ => unifyTyF fuel (b.applySubst θ) (b'.applySubst θ))
    | fuel+1, .rcd ρ₁, .rcd ρ₂ => unifySpineF fuel ρ₁.toSpine ρ₂.toSpine
    | _, _, _                => .clash            -- ★: see §5, OPEN

  def unifySpineF : Nat → List (Atom B) → List (Atom B) → UResM B
    -- strip / solveVar arms: unchanged
    -- eq-emitting arms (matchL, matchR, groundMatch) become:
    --   some (τ, τ', t₁, t₂) =>
    --     seq (unifyTyF fuel τ τ') (fun θ =>
    --       unifySpineF fuel (t₁.applySubstS θ) (t₂.applySubstS θ))
    -- NEW arm, before projClash: expandVar (§1.4)
end
```

`seq θ k` composes with `TySubst.comp` (`minimal.lean:1669`) and its algebra is
already there: `Ty.applySubst_applySubst` / `Row.applySubst_applySubst`
(`minimal.lean:1673`, `:1684`). This is the single most important pre-existing
asset for the rebuild — no new substitution theory is needed.

### 1.3 Keep fuel; change the bound

Do NOT switch to well-founded recursion. The regressions in `Regressions.lean`
are kernel-checked `rfl` executions and `unify_eq_rescued_stuck` etc. only work
because `unifySpineF` reduces definitionally. Well-founded definitions do not
reduce by `rfl`, and losing that loses the cheapest verification instrument in
the development.

What dies is the *bound*, not the technique: `|s₁| + |s₂|` is no longer valid,
because applying a solution and expanding a variable both GROW the spine. The
replacement measure is lexicographic

    (number of unsolved variables in the problem, number of atoms)

with the strip/match arms shrinking the second component at constant first, and
`solveVar`/`bindTy`/`expandVar` shrinking the first. So:

- keep `unifyF : Nat → …`, define `unifyBound : … → Nat` realizing that measure,
- re-prove the two fuel lemmas in the new setting: `unifySpineF_fuel_irrel`
  (`RowUnify.lean:2470`) and `unifySpineF_fuel_stable` (`:2553`) are the exact
  template — the statement shape ("enough fuel ⟹ result independent of fuel")
  carries over verbatim,
- state every downstream theorem at `unifyBound … ≤ fuel`.

**The occurs guard becomes load-bearing.** The first measure component only
decreases if a binding genuinely eliminates its variable, which is exactly what
`occurs` enforces. That retroactively justifies its conservatism — and it means
`occurs_allVar_hasMgu` (the guard rejects `α ≐ᵣ (β|α|γ)`, which HAS an mgu) must
be accepted as a deliberate price, not repaired. Record it as such; do not
attempt to weaken the guard during this rebuild.

### 1.4 The new move: unique-host variable expansion

Detector shape, to sit between `groundMatch` and `projClash`:

```lean
-- l does not occur in s₂'s window, and s₂ has EXACTLY ONE variable
def expandVar : List (Atom B) → List (Atom B) → Option (TyVar × ... )
```

Rule: `β ≔ (l : δ | β′)` with `δ`, `β′` fresh. Two vars ⟹ do not fire (that is
Wand, and `vars_vs_field_no_mgu` proves it genuinely has no mgu). This shrinks
the stuck class to precisely what the three base techniques can kill, which is
what makes §3 provable.

Freshness discipline for `δ`, `β′` is new machinery this development does not
have yet (`proof-state.md` already lists it as needed for non-vacuity of
qualified schemes). Budget it as real work: a fresh-name supply threaded through
both functions, plus the "fresh variables do not occur in the problem" lemmas
every soundness proof will need.

## 2. What survives, what is re-proved

**Survives untouched** (all stated on spines and moves, not on the driver):

- every detector and its algebra: `stripL/stripR/solveVar/matchL/matchR/
  windowExtract/removeField/groundMatch/allVarsEmpty/projClash`;
- every move-reflection lemma, both directions: `stripL_reflect`,
  `matchL_reflect`, `matchR_reflect` (via `revRow`), `groundMatch_reflect`,
  `allVarsEmpty_sound`, and the `_reflect_fwd` family via
  `field_cancel_left/right`;
- the U-ground core: `field_comm_lfree`, `removeField_equiv_of`,
  `allVars_varFree_of`, `allVars_lfree_of`;
- the ≈-side: `rowEquiv_fieldCount_eq`, `sFieldCount_applySubst_le/_varFree`,
  `instanceOf_fieldCount_mono`, `instanceOf_fieldCount_eq_of_varFree`;
- all three base techniques in their ORIGINAL (Q-free) form:
  `vars_vs_field_no_mgu`, `two_sided_no_mgu`, `allvar_swap_no_mgu`;
- the lift infrastructure: `HasMgu`, `HasMguP`, `hasMgu_congr`,
  `hasMguP_congr`, `hasMgu_rowEquiv`, `hasMgu_symm`, and the per-move
  `stripL/stripR/matchL/matchR/groundMatch_hasMgu_iff`;
- the local no-unifier facts: `projClash_no_unifier`,
  `allVarsEmpty_none_no_unifier`, `occurs_field_no_unifier`.

**Must be re-proved** — all four are fuel inductions over the algorithm, so the
recursion structure changes even where the arm content does not:

| theorem | line | new content |
|---|---|---|
| `unifySpineF_success_sound` | `:1933` | each eq-emitting arm now has a *nested* success to invert; needs a compose lemma "θ meets `σ₂ ∘ σ₁` ⟹ θ meets both" |
| `unifySpineF_success_complete` | `:2259` | mirror: a unifier of the original satisfies the sub-solution, then the residual under it |
| `unifySpineF_clash_no_unifier` | `:2085` | new propagation case: a clash inside a field type is a clash of the whole |
| `unifySpineF_stuck_no_mgu` | `:2722` | see §3 — this is where the payoff is |

**New, small:** an "apply-then-unify" bridge, used in every arm above:

    θ ⊨ (t₁.applySubst σ) ≐ᵣ (t₂.applySubst σ)   ↔   (θ ∘ σ) ⊨ t₁ ≐ᵣ t₂

which is `Row.applySubst_applySubst` plus `RowEquiv.applySubst`. Prove it once,
early; every phase below consumes it.

## 3. The payoff: the side condition becomes an invariant

Target statement, with **no `Q`, no `Indep`, no hypothesis**:

    unifyRow ρ₁ ρ₂ = .stuck  →  ¬ HasMgu ρ₁ ρ₂

The argument: with eager solve-and-apply there is no accumulated `Q` at the
terminal configuration at all. An equation is either

- **discharged** — its solution went into θ and was applied to the residual
  spines, so it constrains nothing that is still free; or
- **fatal** — the sub-unification answered `clash`/`occurs`, and the whole call
  answers that, never `stuck`; or
- **itself stuck** — and then, AFTER §1.4, the sub-problem is genuinely
  ambiguous, so it cannot pin anything either.

The third bullet is the one real proof obligation, and it is where §1.4 pays
for itself: `unify_eq_rescued_stuck` is rescued precisely because
`{β} ≐ {l:𝓫}` *is* solvable, and under mutual recursion it gets solved.
`crossfield_stuck_unifiable` is the shape that would otherwise still park a
solvable-yet-stuck equation. Concretely, prove:

    (INV) if `unifyTyF fuel τ τ' = .stuck` then the sub-problem has no mgu

by the same induction, and then propagate `.stuck` immediately rather than
parking. Propagating stuck is only SOUND once §1.4 has landed — before it,
`crossfield` is a counterexample. This is the formal content of "do them
together".

**Fallback if (INV) resists.** Keep a parked-equation list but prove the parked
equations satisfy the old side condition rather than assuming it: resurrect
`Indep`/`eqsSat_indep` from `git show 19921ae` and derive `Indep Q V` from
"every parked equation is stuck, hence ambiguous". The reverted code is a
usable stepping stone, not a dead end — `git show 19921ae:spec/masterthesis/
lean/RowUnify.lean` has it in full.

## 4. Phase order

- **P0 — done (26-09-03).** Revert the `Indep`/`patchRow`/Q-carrying-technique
  block; keep `occurs_allVar_hasMgu` and `crossfield_stuck_unifiable`. Builds
  clean, `Axioms.lean` guards pass.
- **P1 — done (26-09-05).** Section "P1: SCAFFOLDING FOR THE MUTUAL ≐ / ≐ᵣ
  DRIVER" in `RowUnify.lean`, entirely additive; build and all `rfl`
  regressions green, axiom guards extended in `Axioms.lean`.
  - `SubstEquiv` (`≗`, pointwise ≈-equality of substitutions) and its
    congruence `Ty/Row.applySubst_substEquiv` — the one genuinely new piece of
    theory, and AXIOM-FREE. `minimal.lean`'s `applySubst_congr` (`:1697/:1714`)
    is equality-based and too rigid: a solution is only ever met up to `≈`.
  - `Sol` (ty + row), `Sol.nil/ofRow/toSubst/Sat/comp`, `UResM`, `UResM.seq`
    (+ `seq_success` inversion). `Sol.Sat_ofRow` makes the old `SolSat` the
    row-only case. `tyLookup/rowLookup` with `if β = α` instead of
    `List.lookup`, so `*_spec` is a two-line induction.
  - `Sol.Sat.substEquiv` (`Sat θ s → θ ≗ θ ∘ s.toSubst`) — the reason `Sat` is
    the right notion — and `Sol.Sat.comp_inv`, the compose lemma the §2 table
    demands for P5.
  - The bridge in three shapes: `unifies_applySubst_iff` /
    `tyUnifies_applySubst_iff` / `unifies_sApplySubst_iff` (composition), and
    `*_of_sat` (the form the arms use: under a MET solution, substituting the
    residual does not change the unifier set).
  - `sApplySubst` (spine-level substitution; a var atom expands to a spine, so
    it is not a map) + `sApplySubst_equiv`, written by structural recursion so
    it REDUCES — `Regressions.lean` now kernel-checks that.

  DEVIATION: `unifySpineF` was NOT retargeted onto `UResM` here. Doing so
  breaks every downstream `.success σ eqs` proof, which contradicts "P1 keeps
  the build green"; the retarget belongs to P4, where those proofs are re-done
  anyway. Nothing else in the plan changes.
- **P2 — done (26-09-05).** Section "P2: A FRESH-VARIABLE SUPPLY FOR ≐ / ≐ᵣ"
  in `RowUnify.lean`, again additive; build, regressions and guards green.
  The generator did NOT have to be built: `minimal.lean:1752` already has
  `natName`/`natName_length`/`natName_inj`/`lenBound`/`length_le_lenBound`
  (fresh names by LENGTH, for capture-avoiding scheme renaming). P2 is a
  supply and an invariant on top of it.
  - `Supply` (a Nat), `Supply.fresh`, and `S.Avoids avoid := lenBound avoid <
    S.next`. The avoid-set is PROOF-ONLY — the algorithm never computes it, so
    the arms stay reducible (`Regressions.lean` checks a draw by `rfl`).
  - The spec: `fresh_not_mem`, `Avoids.advance`, `Avoids.cons_fresh` (how the
    invariant survives a move that GROWS the problem — i.e. expandVar),
    `Avoids.mono` (how it survives every move that shrinks it), `fresh_ne`
    (δ ≠ β′), `initSupply`/`initSupply_avoids`.
  - `sFtv` — the avoid-set of a spine, counting field-type variables too since
    the namespace is shared — with `sFtv_ofSpine`, `sFtv_append`, `sFtv_cons`,
    `mem_sFtv_reverse` (the right-end moves work on reversed spines).
  - `TySubst.setTy/setRow` + the four INVISIBILITY lemmas (a value chosen at a
    variable the subject does not mention is unobservable), lifted to
    `unifies_setRow_of_not_mem` / `tyUnifies_setTy_of_not_mem` and to the
    headline `Supply.unifies_setRow_fresh`. Note these use the EQUALITY
    congruence `applySubst_congr`, not P1's `≗` — `set*` perturbs θ pointwise,
    so no `≈` is involved.
  - Residual-freshness for the surviving detectors: `windowExtract_ftv`,
    `removeField_ftv`, `stripL_ftv`, `matchL_ftv`, `groundMatchAux/Match_ftv` —
    every residual and every emitted type lives inside the original problem, so
    `Avoids` transports to the recursive call by `Avoids.mono`.
- **P3a — done (26-09-05): the move's metatheory.** Section "P3: UNIQUE-HOST
  VARIABLE EXPANSION" in `RowUnify.lean`; still additive, build/regressions/
  guards green.
  - `host_forced` — THE theorem: if the other side has exactly one variable and
    NO l-field anywhere, every unifier puts an l-field at the FRONT of that
    variable. Via `host_proj` (all of the l-projection comes from the host, the
    surrounding fields carrying other labels) + `spine_extract`
    (`RowEquiv.lean:243`), which already had the "index 0 ⟹ comm bubbles it
    out" argument. `crossfield_host_forced` instantiates it: this MECHANIZES
    the maximality half that `proof-state.md` carries by hand.
  - `expand_shift` — the algebraic content of the move: under a host expansion
    the side factors as "invented field, then the side with the host renamed".
    A plain induction; `field_comm_lfree` is not even needed.
  - `expand_reflect` (backward) and `expand_reflect_fwd` (forward). The forward
    one EXTENDS the unifier at δ and β′ instead of constraining it — this is
    where P2 is consumed, and why the move does NOT shrink the unifier set, in
    contrast to matchL/groundMatch (§2 CAVEAT).
  - Detectors `uniqueHost`/`expandL`/`expandR` (+ `_spec`), with the side
    condition sharpened: `count_l(s₂) = 0` EVERYWHERE, not just in the window.
    Just "absent from the window" is unsound — `(l:𝓪 | α) ≐ᵣ (β | l:𝓫)` has the
    unifier `β ≔ ε` and must not be expanded. `Regressions.lean` pins all three
    behaviours (fires on crossfield, refuses Wand, refuses an l-field).

  CORRECTION to §1.3: the fuel bound does NOT die here. Fusing the expansion
  with the pairing it enables costs exactly ONE atom (`expandL_len`: the host
  side keeps its length, β ↦ β′, and the invented field is consumed at once),
  so `|s₁| + |s₂|` still bounds the recursion. Only P4's solve-and-apply grows
  a spine, so the lexicographic measure is a P4 requirement, not a P3 one.
- **P3b — next: wire it in.** Add the arm between `groundMatch` and
  `projClash`, re-run the regressions, and turn `crossfield_stuck_unifiable`
  into `crossfield_success`. The dispatch cascade needs a fresh-name source;
  deriving it locally as `⟨lenBound (sFtv s₁ ++ sFtv s₂) + 1⟩` avoids adding a
  `Supply` parameter to `unifySpineF` (which would break every existing
  statement), and stays sound across the recursion because β′ is IN the
  residual, so the next call's bound exceeds it. Each of the four fuel
  inductions then gains one arm, discharged by the P3a reflection lemmas.
- **P4 — mutualize.** Write `unifyTyF`, make the eq-emitting arms solve and
  apply, drop `URes.addEq`. New bound + `fuel_irrel`/`fuel_stable`.
- **P5 — re-prove the legs.** Success sound, success complete, clash — in that
  order; each is a mechanical port plus the P1 bridge.
- **P6 — the stuck leg.** (INV) and the unconditional
  `unifyRow_stuck_no_mgu`. Then, and only then, the trichotomy is a theorem
  rather than a reduction.

P1–P3 are independently valuable and each keeps the build green; P4 is the only
step that briefly breaks everything downstream.

## 5. Decided (26-09-05): ★ is RIGID

```lean
| _, .unk, .unk => .success ∅
| _, .unk, _    => .clash
| _, _,    .unk => .clash
```

`≐` never refines ★; refinement happens only through the lookup relation
`Γ ⊢ ρ.l ↓ r`, never through unification. This keeps mgu-ness intact (the
unifier set stays closed under instantiation), so every P5 statement can be
phrased exactly as its `≐ᵣ`-only ancestor — no ⊑-modulo soundness. The §1.2
sketch already assumes this; nothing in the plan changes.

The alternatives, recorded for the writeup:

**What does `≐` do with ★?** `Ty.unk` is "definite uncertainty, no elimination"
and the development already has a precision order `⊑`. Three candidates:

- ★ is rigid: `★ ≐ ★` succeeds, `★ ≐ τ` clashes. Simplest; but then refinement
  never happens through unification, only through the lookup relation.
- ★ absorbs: `★ ≐ τ` succeeds with no binding. Cheap, but breaks mgu-ness
  (the unifier set is no longer closed under instantiation).
- ★ unifies up to `⊑`: `≐` returns the ⊑-least upper bound. Matches the thesis
  story (types improve under reduction, `qPreservation`), most work.

The first is now the decision (above). The other two are the reason the choice
had to be settled before P4: each changes the statement of every soundness
theorem in P5.
