-- Executable regressions: the worked examples, kernel-checked. Each `:= rfl`
-- makes the Lean kernel RUN the algorithm on a concrete input (B := Unit) and
-- check the result — a regression test baked into the build. If ≐ᵣ's behaviour
-- ever changes, the corresponding rfl stops type-checking and the build breaks.
--
-- Fuel is explicit: `outOfFuel` is its own verdict, so a `.stuck` below always
-- means every move is dead, never "the budget ran out", and 20 is comfortably
-- enough for every example here.

import RowUnify

namespace MinimalCalculus

private def uB : Ty Unit := .base ()

-- U-ε.
-- ⊢  unifyRowM ε ε  =  success ∅
theorem unify_empty :
    unifyRowM (B := Unit) 20 .empty .empty = .success ⟨[], []⟩ ⟨1⟩ := rfl

-- P&X's shared-tail pitfall (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α): U-var-refl
-- right-cancels α, then U-clash — matches shared_tail_no_unifier.
-- ⊢  unifyRowM (l:𝓫 | a) (m:𝓫 | a)  =  clash
theorem unify_shared_tail :
    unifyRowM (B := Unit) 20 (.cat (.sing "l" uB) (.var "a"))
                             (.cat (.sing "m" uB) (.var "a")) = .clash := rfl

-- The LUtail example (l: 𝓫) ≐ᵣ (α | l: 𝓫): right-match the field, then
-- U-ε-var — finds the mgu α ≔ ε that LUtail misses (lutail_unifier_iff). The
-- type equation 𝓫 ≐ 𝓫 is SOLVED (vacuously), so the solution is the whole
-- answer.
-- ⊢  unifyRowM (l:𝓫) (a | l:𝓫)  =  success [a ≔ ε]
theorem unify_lutail :
    unifyRowM (B := Unit) 20 (.sing "l" uB) (.cat (.var "a") (.sing "l" uB)) =
      .success ⟨[], [("a", .empty)]⟩ ⟨2⟩ := rfl

-- Wand's ambiguity (β | α) ≐ᵣ (l: 𝓫): STUCK — solvable but no mgu
-- (wand_unifiable, wand_no_mgu). Solving equations cannot and does not shrink
-- this class; U-expand refuses because TWO variables could host the field.
-- ⊢  unifyRowM (b | a) (l:𝓫)  =  stuck
theorem unify_wand :
    unifyRowM (B := Unit) 20 (.cat (.var "b") (.var "a")) (.sing "l" uB) = .stuck := rfl

-- THE PAYOFF OF MUTUALIZATION. matchL peels k and emits {β} ≐ {l:𝓫}, leaving
-- the Wand residual (β | α) ≐ᵣ (l:𝓫) — which alone is ambiguous, but the
-- equation forces β ≈ (l:𝓫), hence α ≈ ε, so the problem has a UNIQUE mgu.
-- The mutual driver solves that equation, applies it, and finds exactly it.
-- ⊢  unifyRowM (k:{β} | β | α) (k:{l:𝓫} | l:𝓫)
--      =  success [β ≔ (l:𝓫 | ε), α ≔ ε]
theorem eq_rescued_solved :
    unifyRowM (B := Unit) 20
      (.cat (.sing "k" (.rcd (.var "b"))) (.cat (.var "b") (.var "a")))
      (.cat (.sing "k" (.rcd (.sing "l" uB))) (.sing "l" uB))
      = .success ⟨[], [("b", .cat (.sing "l" uB) .empty), ("a", .empty)]⟩ ⟨2⟩ := rfl

-- Worked example 2, (α | l: 𝓫 | β) ≐ᵣ (l: 𝓫): U-ground pairs the l-fields
-- (counting rules the vars out), then U-ε-var forces α ≔ ε, β ≔ ε.
-- ⊢  unifyRowM (a | l:𝓫 | b) (l:𝓫)  =  success [a ≔ ε, b ≔ ε]
theorem unify_ground_collapse :
    unifyRowM (B := Unit) 20 (.cat (.var "a") (.cat (.sing "l" uB) (.var "b")))
                             (.sing "l" uB) =
      .success ⟨[], [("a", .empty), ("b", .empty)]⟩ ⟨2⟩ := rfl

-- (β | l: 𝓫 | α) ≐ᵣ (l′: 𝓫), l ≠ l′: U-clash, NOT stuck — the projection
-- check is global, a window-only rule would misfile this.
-- ⊢  unifyRowM (b | l:𝓫 | a) (m:𝓫)  =  clash
theorem unify_global_clash :
    unifyRowM (B := Unit) 20 (.cat (.var "b") (.cat (.sing "l" uB) (.var "a")))
                             (.sing "m" uB) = .clash := rfl

-- α ≐ᵣ (l: 𝓫 | α): the shared END-var cancels first (solution-preserving!),
-- leaving ε ≐ᵣ (l: 𝓫) — a definite CLASH, strictly stronger than an
-- occurs-failure. Cancellativity subsumes end-aligned occurs cases.
-- ⊢  unifyRowM a (l:𝓫 | a)  =  clash
theorem unify_occurs_cancelled :
    unifyRowM (B := Unit) 20 (.var "a") (.cat (.sing "l" uB) (.var "a")) = .clash := rfl

-- U-var-solve with occurs check: α ≐ᵣ (l: 𝓫 | α | m: 𝓫) — the recursive
-- var is interior, no cancellation applies, genuinely a recursive row.
-- ⊢  unifyRowM a (l:𝓫 | a | m:𝓫)  =  occurs
theorem unify_occurs :
    unifyRowM (B := Unit) 20 (.var "a")
      (.cat (.sing "l" uB) (.cat (.var "a") (.sing "m" uB))) = .occurs := rfl

-- Var-var: solved union-find style.
-- ⊢  unifyRowM a b  =  success [a ≔ (b | ε)]
theorem unify_var_var :
    unifyRowM (B := Unit) 20 (.var "a") (.var "b") =
      .success ⟨[], [("a", .cat (.var "b") .empty)]⟩ ⟨2⟩ := rfl

-- The ambiguous mirror (α | l: 𝓫) ≐ᵣ (l: 𝓫 | β): both windows closed by a
-- var, both sides have vars — correctly stuck (Levi splits two ways).
-- ⊢  unifyRowM (a | l:𝓫) (l:𝓫 | b)  =  stuck
theorem unify_two_sided_stuck :
    unifyRowM (B := Unit) 20 (.cat (.var "a") (.sing "l" uB))
                             (.cat (.sing "l" uB) (.var "b")) = .stuck := rfl

-- THE SHIFT PROBLEM (α | l: 𝓫) ≐ᵣ (l: 𝓫 | α) — the same var on both sides, so
-- unlike unify_two_sided_stuck this is not a Levi ambiguity but a COUNTING one.
-- It is solvable (α ≔ ε) and has one maximal unifier per k — α ≔ (l:𝓫)^k — so
-- no finite answer is complete (RowUnify/UnifType.lean). Stuck here is not a
-- missing driver arm: shift_no_finite_complete_set says no arm can do better.
-- ⊢  unifyRowM (a | l:𝓫) (l:𝓫 | a)  =  stuck
theorem unify_shift_stuck :
    unifyRowM (B := Unit) 20 (.cat (.var "a") (.sing "l" uB))
                             (.cat (.sing "l" uB) (.var "a")) = .stuck := rfl

-- … and its mirror, for the same reason.
-- ⊢  unifyRowM (l:𝓫 | a) (a | l:𝓫)  =  stuck
theorem unify_shift_stuck_mirror :
    unifyRowM (B := Unit) 20 (.cat (.sing "l" uB) (.var "a"))
                             (.cat (.var "a") (.sing "l" uB)) = .stuck := rfl

-- The k = 0 and k = 1 members of that family, instantiated: both succeed, which
-- is what makes the stuck verdict above a genuine incompleteness rather than a
-- clash in disguise.
-- ⊢  unifyRowM (ε | l:𝓫) (l:𝓫 | ε)  =  success ∅
theorem unify_shift_inst_zero :
    unifyRowM (B := Unit) 20 (.cat .empty (.sing "l" uB))
                             (.cat (.sing "l" uB) .empty) = .success ⟨[], []⟩ ⟨1⟩ := rfl

-- ⊢  unifyRowM (l:𝓫 | l:𝓫) (l:𝓫 | l:𝓫)  =  success ∅
theorem unify_shift_inst_one :
    unifyRowM (B := Unit) 20 (.cat (.sing "l" uB) (.sing "l" uB))
                             (.cat (.sing "l" uB) (.sing "l" uB))
      = .success ⟨[], []⟩ ⟨1⟩ := rfl

-- ## P1 scaffolding, kernel-checked
-- The mutual driver applies a solution to the residual spine at every
-- eq-emitting arm, so sApplySubst must REDUCE, not just be provably correct —
-- that is what keeps the regressions above `rfl` once P4 lands.
private def uS : Sol Unit := ⟨[("t", uB)], [("a", .sing "l" uB)]⟩

-- ⊢  (l: t | a | m: 𝓫)[uS]  =  l: 𝓫 | l: 𝓫 | m: 𝓫      (var expands to a spine)
theorem sApplySubst_computes :
    sApplySubst uS.toSubst
      [.field "l" (.var "t"), .var "a", .field "m" uB] =
      [.field "l" uB, .field "l" uB, .field "m" uB] := rfl

-- ⊢  an unbound variable is left alone by a solution's substitution
theorem toSubst_free : uS.toSubst.row "z" = .var "z" := rfl

-- seq composes two successes; the earlier solution is pushed through the later.
-- ⊢  success ⟨[t ≔ 𝓫], []⟩ >>= (fun _ => success ⟨[], [a ≔ ε]⟩)
--      =  success ⟨[t ≔ 𝓫], [a ≔ ε]⟩
theorem seq_composes :
    (UResM.success (B := Unit) ⟨[("t", uB)], []⟩ ⟨7⟩).seq
        (fun _ S => .success ⟨[], [("a", .empty)]⟩ S) =
      .success ⟨[("t", uB)], [("a", .empty)]⟩ ⟨7⟩ := rfl

-- ⊢  a stuck second stage is the verdict of the whole
theorem seq_propagates :
    (UResM.success (B := Unit) ⟨[("t", uB)], []⟩ ⟨7⟩).seq (fun _ _ => .stuck) = .stuck := rfl

-- ## P2 freshness, kernel-checked
-- The supply is a Nat and the avoid-set is proof-only, so drawing a name
-- reduces — expandVar's arm will stay a `rfl` regression.
-- ⊢  two draws from a supply are two DIFFERENT names
theorem fresh_draws :
    ((Supply.mk 2).fresh.1, (Supply.mk 2).fresh.2.fresh.1) = ("aa", "aaa") := rfl

-- ⊢  the initial supply starts strictly above the problem's longest name
theorem initSupply_computes :
    (initSupply (B := Unit) (.var "ab") (.cat (.var "c") (.sing "l" uB))).next = 3 := rfl

-- ⊢  sFtv sees BOTH sorts: the field type's variable counts as used
theorem sFtv_computes :
    sFtv (B := Unit) [.var "a", .field "l" (.var "t")] = ["a", "t"] := rfl

-- ## P3 unique-host expansion — REMOVED
-- The four expansion arms are gone (plans/drop-expand.md). What used to be
-- pinned here — `expandL_crossfield`, `expandL_wand_refuses`,
-- `expandL_lfield_refuses`, `expandR_crossfield_mirror`, and the two driver
-- verdicts they produced — went with them. The COST of that is pinned instead,
-- at `crossfield_stuck` (RowUnify/Driver.lean): the crossfield problem has a
-- unifier, and the driver no longer finds it.
--
-- ⊢  the right-end mirror of crossfield, likewise `.stuck` now. `expandR` was
--    added precisely because this problem was stuck without it; removing the
--    arm returns it to that state, which is the trade taken knowingly.
theorem unify_crossfield_mirror_stuck :
    unifyRowM (B := Unit) 20 (.cat (.var "a") (.sing "l" uB))
                             (.cat (.var "b") (.sing "m" uB)) = .stuck := rfl

-- ⊢  (l:{w}) ≐ᵣ (w | v) — `Refutations.terminal_masks_mgu`'s configuration. It
--    has an mgu (w ≔ ε, v ≔ (ε | l:{ε})), `expandR` used to find it, and the
--    driver is back to `.stuck`. So `TerminalNoMgu` is refuted again: a
--    terminal configuration can still have a unifier. See
--    `Refutations.terminal_masks_mgu`.
theorem unify_terminal_masks_mgu_stuck :
    unifyRowM (B := Unit) 20 (.sing "l" (.rcd (.var "w")))
                             (.cat (.var "w") (.var "v")) = .stuck := rfl

-- ## The accumulated solution, read by the guards
-- U-expand RENAMES its host instead of applying β ≔ (l:δ | β′), and `renameVar`
-- touches spine variables only — so a payload mentioning β still reads β after
-- the move, while β is already bound. A later guard comparing against that
-- stale payload misses a cycle that exists only in the TRANSITIVE CLOSURE of
-- the solution, and the run ends in a VACUOUS success: a solution no θ
-- satisfies. `depReach` is what closes that; these two pin it.

-- ⊢  the tripwire's original witness. The driver used to report SUCCESS here
--    with b ≔ (l:aa | l:aaaa | ε) alongside aaaa ≔ {ε | b} — a cycle through a
--    PAYLOAD, so ⟦S⟧ never terminates and no θ satisfies both. Now `.stuck`:
--    conservative, and it claims nothing.
theorem vacuous_success_payload_cycle :
    unifyRowM (B := Unit) 30 (.cat (.var "b") (.var "a"))
      (.cat (.sing "l" (.rcd (.cat (.sing "l" uB) (.var "a"))))
            (.sing "l" (.rcd (.cat (.var "a") (.var "b"))))) = .stuck := rfl

-- ⊢  … and the SPINE-level one, which the right-end arm exposed. It used to
--    take two expansions in sequence — the first binding a ≔ (m:δ | aaa), the
--    second excused by the self-reference filter reading the stale payload
--    `{a}` — and reached `.occurs`. With no arm to invent `aaa` there is no
--    stale payload and no cycle to catch, and the driver stops at `.stuck`.
--
--    NOTE this is a REAL loss of precision, not just of coverage: the problem
--    has no unifier (count_m forces 0 = 1 + …), so `.occurs` was the sharp
--    answer and `.stuck` is merely a safe one. It is also the last witness that
--    needed `depReach` to be correct — which is why the DepGraph can go.
theorem vacuous_success_spine_cycle :
    unifyRowM (B := Unit) 30 (.cat (.var "a") (.sing "l" (.rcd (.var "a"))))
      (.cat (.sing "m" uB) (.cat (.var "a") (.var "b"))) = .stuck := rfl

-- ## P4: ≐ / ≐ᵣ under the MUTUAL driver
-- (the row verdicts above already run it; these exercise the type pass.)

-- ## ≐ itself, kernel-checked
-- The type pass was FUTURE WORK until P4; these are its first regressions.
-- ⊢  (x → x) ≐ (𝓫 → y)  =  success [x ≔ 𝓫, y ≔ 𝓫]
--    (y ≔ 𝓫, not y ≔ x: the first solution is APPLIED to the second component)
theorem tyM_fn_solve_and_apply :
    unifyTyM (B := Unit) 5 (.fn (.var "x") (.var "x")) (.fn uB (.var "y")) =
      .success ⟨[("x", uB), ("y", uB)], []⟩ ⟨2⟩ := rfl

-- ★ is RIGID: it unifies with itself and nothing else.
-- ⊢  ★ ≐ ★  =  success ∅      ⊢  ★ ≐ 𝓫  =  clash
theorem tyM_unk_refl : unifyTyM (B := Unit) 5 .unk .unk = .success ⟨[], []⟩ ⟨1⟩ := rfl
theorem tyM_unk_rigid : unifyTyM (B := Unit) 5 .unk uB = .clash := rfl

-- The type occurs guard is SORTED. `x ≐ {x}` binds x at the TYPE sort while the
-- inner x occurs at the ROW sort: two different variables sharing one untagged
-- namespace. The sort-blind `ftv` used to reject this as a cycle; `Ty.tyFtv`
-- does not, and the two witnesses below show the old `.occurs` was a FALSE
-- verdict, not conservatism with a reason.
-- ⊢  x ≐ {x}  =  success (x ≔ {x})
theorem tyM_cross_sort_solves :
    unifyTyM (B := Unit) 5 (.var "x") (.rcd (.var "x")) =
      .success ⟨[("x", .rcd (.var "x"))], []⟩ ⟨2⟩ := rfl

-- θ.ty x = {ε}, θ.row x = ε — the unifier the old comment named and the guard
-- denied. It unifies the problem…
private def crossSortSub : TySubst Unit :=
  ⟨fun _ => .rcd .empty, fun _ => .empty⟩

theorem tyM_cross_sort_unifier :
    TyUnifies crossSortSub (.var "x") (.rcd (.var "x")) := TyEquiv.refl _

-- …and it MEETS the emitted solution, so success was the right verdict.
theorem tyM_cross_sort_sat :
    Sol.Sat crossSortSub ⟨[("x", .rcd (.var "x"))], []⟩ := by
  refine ⟨fun p hp => ?_, fun _ hp => nomatch hp⟩
  obtain rfl := List.mem_singleton.mp hp
  exact TyEquiv.refl _

-- The ROW occurs guard keeps its own conservatism — that is a different
-- question (occurs_allVar_hasMgu), untouched by sorting the TYPE guard.

-- …and the guard still catches the genuine cycle, at BOTH the arrow and the
-- field position — there the occurrence really is a TYPE one.
-- ⊢  x ≐ (x → x)  =  occurs      ⊢  x ≐ {l: x}  =  occurs
theorem tyM_occurs_fn :
    unifyTyM (B := Unit) 5 (.var "x") (.fn (.var "x") (.var "x")) = .occurs := rfl
theorem tyM_occurs_field :
    unifyTyM (B := Unit) 5 (.var "x") (.rcd (.sing "l" (.var "x"))) = .occurs := rfl

-- ⊢  fuel exhaustion is its OWN verdict, never mistaken for stuck.
--    The old witness was crossfield at fuel 1, which only ran out because
--    U-expand recursed; it is `.stuck` at every budget now, so it could no
--    longer tell the two apart. This one exhausts fuel in the TYPE pass
--    instead — `matchL` fires, and the nested record eats the budget — and the
--    pair of verdicts below is the actual content of the claim: the same
--    problem is `.outOfFuel` at 2 and `.success` at 3, so `.outOfFuel` is a
--    statement about the budget and never about the problem.
theorem outOfFuel_is_separate :
    unifyRowM (B := Unit) 2 (.sing "k" (.rcd (.sing "l" uB)))
                            (.sing "k" (.rcd (.sing "l" (.var "x")))) = .outOfFuel := rfl

theorem outOfFuel_is_only_the_budget :
    unifyRowM (B := Unit) 3 (.sing "k" (.rcd (.sing "l" uB)))
                            (.sing "k" (.rcd (.sing "l" (.var "x")))) =
      .success ⟨[("x", uB)], []⟩ ⟨2⟩ := rfl

end MinimalCalculus
