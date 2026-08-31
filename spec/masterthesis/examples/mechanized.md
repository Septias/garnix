# Mechanized Examples


a: b: (a ‖ b).l
----------------
The typical wand example and motivation for the thesis. l could be in a or b but
we can not know which before getting more information. We explicitly surrender to
uncertainty by admitting the type ★. Only occurrence-typing can refine ★.


x: ({l = c} ‖ x).l : {β} → ★    and later    {β} → 𝓫_c
----------------------------------------------------------
The lambda has type {β} → ★, but when we solve β = ε, we get a more precise
type {β} -> 𝓫_c. The precision relation (⊑) ties the two together
({β} → 𝓫_c ⊑ {β} → ★).


let f = (x: x.l) in f {} :  ★
------------------------------
The initial type {β} → ★ needs a ⊥-lookup because of the reduct `(x: x.l) {}`
`x: {ε} ⊢ x.l`. That is the motivation for T-sel-⊥. 


let w = (f: (y: f (y.l))) in w (z: c) {l = c′}
-----------------------------------------------
the companion counterexample for T-★-intro:
after β ≔ (l: 𝓫′) the lookup y.l refines from ★ to 𝓫′, but the λ-bound
f: ★ → 𝓫 has a FROZEN domain ★. Without a rule to re-blur 𝓫′ back to ★ the
reduct is untypeable. So the two ★ rules are not conveniences: instantiation
can demote AND promote lookup results, and both directions need absorbing.


λx. x.l  :  {(l: τ₀)} → τ₀   (every τ₀)     and     {ε} → ★
------------------------------------------------------------
one term, two typing FAMILIES that no plain scheme can span.
The first fires T-sel (lookup hits), the second T-sel-⊥ (lookup is definitely absent).
Everything in §3–§4 is about finding a scheme form that has
both as instances without also admitting garbage.


¬ ∃θ.  θ({β} → ★)  ⊑  {(l: τ₀)} → τ₀     (τ₀ ≠ ★)
---------------------------------------------------
plain schemes + finalize pending stumps to ★ at the generalization boundary).
L1 infers {β} → δ and freezes δ ≐ ★; but a frozen ★ result sits ⊑-below NOTHING except
★ itself (⊑-rigidity), so the principality factoring "every declarative
typing is substitution-then-blur of the inferred type" fails outright. **Once
★ is written into a scheme's result position, the position is dead.**


{ε} → {ε}   is NOT a typing of λx. x.l
---------------------------------------
the sharper, scheme-independent refutation. Any plain scheme covering both
typings above must have a QUANTIFIED VARIABLE in its result position (it
instantiates to both {ε} and ★, which are rigid constructors). But then
re-pointing that variable at {ε} inside the ⊥-instance's substitution
manufactures the instance {ε} → {ε} — which is not a typing at all, because
on x: {ε} the lookup is ⊥ and the body types only at ★. So NO plain ∀ᾱ.τ
scheme is instance-closed while covering both typings.


∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
---------------------------
✓ (Qualified.lean: selQ, selQ_instance_closed, qualified_principal_scheme) —
the repair. The result position δ stays WRITABLE per instance and discharge
pins it to the lookup's verdict, so all three verdicts produce the right
instance in one statement (selQ_inst_of_lookup):

```
Γ ⊢ ρ.l ↓ τ_r  ⟹  {ρ} → τ_r      (what L1 loses)
Γ ⊢ ρ.l ↓ ⊥    ⟹  {ρ} → ★
Γ ⊢ ρ.l ↓ ?    ⟹  {ρ} → ★
```

and the mixed instance is now correctly EXCLUDED (selQ_no_mixed): θ must send
β to ε, the lookup on ε is definitely ⊥, and discharge pins δ at ★, never at
{ε}. **Discharge is exactly the mechanism that plugs the instance-closedness
leak.** Note the correspondence: the three discharge cases ARE the three-way
case split of the §2 regression proof, now done once per instance instead of
once per derivation.

let f = (x: x.l) in { a = f {l = c} | b = f {} }  :  { a: 𝓫_c | b: ★ }
------------------------------------------------------------------------
✓ (Qualified.lean: qtyped_two_use) — L2's precision demonstrated end to end.
ONE binding, TWO uses at incompatible refined instances: the found-typing and
the ⊥-typing of the same scheme, each discharging its own copy of the stump.
By no_plain_principal_scheme no single plain scheme could serve both uses at
these types, so this program separates L1 from L2 — it is the intended
witness for the (still open) STRICTNESS claim.


## 4. Row equivalence ≈ and the shape of ≐ᵣ

(l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α),  l₁ ≠ l₂     ⟹  no unifier
---------------------------------------------------------
✓ (RowEquiv.lean: shared_tail_no_unifier) ✓rfl (Regressions.lean:
unify_shared_tail = clash) — P&X's shared-tail pitfall, the example
motivating their side condition [Δ₂]ρ₁ = [Δ₁]ρ₁. We need no side condition at
all: rows mod ≈ form a **trace monoid**, which is left- AND right-cancellative,
so U-var-refl right-cancels the shared α on the nose and the l₁-projection
then clashes. **The algebra replaces the side condition** — this is the single
biggest simplification the ≈-characterization buys.

(l: 𝓫) ≐ᵣ (α | l: 𝓫)     ⟺     θα ≈ ε      (unique mgu)
----------------------------------------------------------
⚠ (RowEquiv.lean: lutail_unifier_iff) ✓rfl (unify_lutail = success [α ≔ ε])
— why we DROP P&X's LUtail. Their (Rfield) search hits the row var and
commits it to contain the sought field (α ≔ (l: β | γ), fresh β γ), so it
FAILS here — but the problem has a perfectly good unique mgu α ≔ ε. LUtail is
not forced and demonstrably loses solutions. Two-sided processing finds the
mgu: right-match the field, then U-ε-var. The deeper reason we CAN drop it:
P&X need field demands to flow into rows through unification, ours flow
through the lookup relation and park as stumps, so ≐ᵣ only ever states
structural EQUALITY of two rows and never has to guess a field into a var.
**This is the algorithmic payoff of the T-sel/★ design.**

(β | α) ≐ᵣ (l: 𝓫)     solutions EXIST, but no mgu
--------------------------------------------------
⚠ (RowEquiv.lean: wand_unifiable + wand_no_mgu; RowUnify.lean:
wand_no_mgu_count re-proves it by counting) ✓rfl (unify_wand = stuck) —
Wand's non-principality example in unification clothing, and the definition
of the whole STUCK class. θβ ++ θα = [l: 𝓫] splits two incomparable ways
(field-from-β or field-from-α); each split is a unifier, neither generalizes
the other. Failing here is CORRECT, not a weakness: g: {l: 𝓫} → τ applied to
x ‖ y with both arguments abstract genuinely has no principal typing without
lacks-constraints or unions of typings. **We fail only when two abstract
concatenations must be aligned against each other; selection — the common
case — never asks for alignment, thanks to stumps.**

(β | l: 𝓫 | α) ≐ᵣ (l′: 𝓫),  l ≠ l′     ⟹  CLASH, not stuck
-------------------------------------------------------------
⚠ (RowUnify.lean: projClash_no_unifier) ✓rfl (unify_global_clash = clash) —
this example is why U-clash must be PROJECTION-BASED and checked GLOBALLY.
The leading atoms are var vs. field, so a window-only clash rule would misfile
this as stuck — but the right side is var-free with no l-field, so the
l-projection is unsolvable no matter what the vars do. Getting this wrong
would degrade trichotomy case (c) from "solutions exist but no mgu" to "no
unique mgu OR unsolvable", i.e. destroy its sharpness.

(α | l: 𝓫 | β) ≐ᵣ (l: 𝓫)     ⟹  α ≔ ε, β ≔ ε
------------------------------------------------
✓rfl (unify_ground_collapse) — "worked example 2": the var count must
collapse. Neither window rule can fire (the leading atom is a var, the
trailing atom is a var), yet the answer is forced: the right side is var-free
and both sides have exactly one l-field, so by COUNTING the vars cannot
contribute any l-field and the pairing is positional. **This example is what
forced the U-ground rule into existence — the mechanization surfaced that the
window rules alone do not cover it.** Its soundness is the one genuinely
non-local step in the whole file (groundMatch_reflect reads the counting back
out of the residual solution).

(α | l: 𝓫) ≐ᵣ (l: 𝓫 | β),  α ≠ β     ⟹  STUCK
-------------------------------------------------
✓ (RowUnify.lean: two_sided_no_mgu) ✓rfl (unify_two_sided_stuck) — the OTHER
canonical stuck shape: both windows closed by a var, both sides have vars,
Levi splits two ways. Worth stating separately because the counting argument
CANNOT kill it — the (α,β ↦ ε) unifier has l-count 0 at every variable, so
there is nothing to undercut. The kill is RIGIDITY instead: the empty witness
pins the counts at 0, the unifier equation forces θα var-free, and a var-free
component of a substitution has a FIXED count under every instance, which the
doubling witness (α,β ↦ l:𝓫) then contradicts.

(α | β) ≐ᵣ (β | α)     ⟹  no mgu
-----------------------------------
✓ (RowUnify.lean: allvar_swap_no_mgu) — the third and last base technique,
needed because the all-variable case has NO fields for counting or rigidity to
bite on. The kill is combinatorial: witnesses force θα, θβ field-free, the
unifier equation gives var-sequences with A ++ B = B ++ A, a first-occurrence
index argument (append_comm_subset, the membership fragment of
Lyndon–Schützenberger) forces vars(A) ⊆ vars(B), and emptying B's vars then
collapses θα — contradicting the field it must carry. **Variables do not
commute; that non-commutativity is itself a proof technique.**


## 5. What the occurs check really means

α ≐ᵣ (l: 𝓫 | α)     ⟹  CLASH  (not occurs)
---------------------------------------------
✓rfl (unify_occurs_cancelled) — the shared END-var cancels FIRST, and
cancellation is solution-preserving, leaving ε ≐ᵣ (l: 𝓫): a definite clash,
which is strictly STRONGER information than an occurs-failure. Cancellativity
subsumes all end-aligned occurs cases, so the occurs check only ever sees
genuinely interior recursion.

α ≐ᵣ (l: 𝓫 | α | m: 𝓫)     ⟹  OCCURS
----------------------------------------
✓rfl (unify_occurs) ✓ (RowUnify.lean: occurs_field_no_unifier) — the genuine
recursive row: interior occurrence, no cancellation applies. Here occurs IS a
real no-unifier verdict, and the proof is the counting argument again: θα
would have to carry the l-field both on its own (as the left side) and again
inside the right side — an impossible strict growth.

α ≐ᵣ (β | α | γ)     reported OCCURS, yet UNIFIABLE (β, γ ↦ ε)
----------------------------------------------------------------
⚠ (RowUnify.lean: occurs_allVar_unifiable) — the occurs check is
CONSERVATIVE, and this example proves it. An all-variable interior occurrence
is perfectly unifiable by collapsing the surrounding variables to ε, but the
algorithm rejects it. So `.occurs` does NOT carry a no-unifier guarantee —
only the field-pinned case does. **This is worth saying out loud in the thesis,
since "occurs check" ordinarily connotes definite non-unifiability.** It is a
soundness-preserving incompleteness (we reject some solvable problems), not a
soundness bug.


## 6. The stuck leg: what the trichotomy can and cannot claim

(k: {β} | β | α) ≐ᵣ (k: {l: 𝓫} | l: 𝓫)     STUCK — yet an mgu EXISTS
----------------------------------------------------------------------
⚠⚠ (Regressions.lean: unify_eq_rescued_stuck) — **the sharpest finding in the
development, and the one that reshapes the trichotomy.** Feed a field whose
TYPE embeds the stuck row-var: matchL peels the shared k-field, emitting the
type equation {β} ≐ {l: 𝓫}; the residual (β | α) ≐ᵣ (l: 𝓫) is exactly Wand,
hence stuck. But the emitted equation forces β ≈ (l: 𝓫), which then forces
α ≈ ε — the WHOLE problem has a UNIQUE mgu. Therefore:

> `unifyRow = stuck` does NOT imply "no mgu".

The single row pass does not solve the emitted equations, so its stuck verdict
is incomplete whenever an emitted equation constrains a stuck row-var. The
honest statement of trichotomy case (c) must be RELATIVE to the accumulated
equations Q — which is exactly how unifySpineF_stuck_no_mgu is stated (as a
reduction carrying Q as an arbitrary unifier predicate), and why its base
hypothesis is not universally true.

¬ HasMgu (γ | β | α) (γ | l: 𝓫)
---------------------------------
✓ (RowUnify.lean: wand_under_strip_no_mgu) — the Wand core wrapped in a shared
leading var, obtained MECHANICALLY: stripL peels γ, and because strip moves
preserve the unifier set EXACTLY, mgu-status transports both ways
(stripL_hasMgu_iff) onto the bare Wand config. This is the template for how
the eventual full lift discharges its strip arms.

¬ HasMgu (l: 𝓪 | β | α) (l: 𝓪 | l: 𝓫)
----------------------------------------
✓ (RowUnify.lean: wand_under_match_no_mgu) — the same demo for the harder,
EQ-EMITTING arm. matchL peels the shared leading field, emitting the eq
𝓪 ≐ 𝓪; the residual is Wand. Since match/ground moves SHRINK the unifier set
(they intersect it with the eq-satisfiers) rather than preserving it, they
transport mgu-status only through the predicate-level congruence (HasMguP).
Here the emitted eq happens to be vacuous so it collapses back to plain
HasMgu; **the genuine content — re-running the base witnesses under a
NON-vacuous accumulated equation — is exactly what unify_eq_rescued_stuck
shows is necessary, and is the remaining open work.**

(v₁ | … | vₙ) ≐ᵣ (l: 𝓫),  n ≥ 2 distinct     ⟹  no mgu
---------------------------------------------------------
✓ (RowUnify.lean: vars_vs_field_no_mgu, mirrored by field_vs_vars_no_mgu) —
Wand generalized to the whole family. Same counting kill: the field's counts
across the vars sum to 1, so exactly one var hosts it, and the witness hosting
it in ANY other var undercuts that one's count 1 → 0. This is the general form
the base arm needs for the var-vs-field stuck shapes.


## 7. Degenerate / sanity regressions

ε ≐ᵣ ε   ⟹  success [] []          ·          α ≐ᵣ β   ⟹  success [α ≔ (β | ε)] []
-------------------------------------------------------------------------------------
✓rfl (unify_empty, unify_var_var) — the trivial legs, kept as executions
rather than as prose. The (β | ε) in the var-var solution is a spine-refold
artifact (ofSpine right-nests and terminates in ε); it is ≈-equal to β, and
the fact that the regression pins the raw syntactic form is deliberate — it
would catch a silent change in the normal form.


## 8. What the examples collectively established

- ★ must be a TYPE with introduction rules, not an error state — §1, §2
- lookup verdicts must be THREE-way (τ | ⊥ | ?) and all three must have
  typing rules — §2
- plain schemes cannot be principal; qualified (stump-carrying) schemes are
  FORCED, not chosen — §3
- ≈ is a trace monoid ⟹ cancellativity replaces P&X's shared-tail side
  condition — §4
- LUtail loses solutions and can be dropped because field demands never flow
  through ≐ᵣ — §4
- U-clash must be projection-based and global, else the stuck class is not
  sharp — §4
- U-ground exists because window rules alone are incomplete — §4
- occurs is conservative, not a no-unifier oracle — §5
- **the stuck verdict is only meaningful relative to the accumulated type
  equations** — §6, the finding that currently blocks a clean trichotomy
  statement and is the main open design question
