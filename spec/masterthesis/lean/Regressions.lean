-- Executable regressions: the worked examples, kernel-checked. Each `:= rfl`
-- makes the Lean kernel RUN the algorithm on a concrete input (B := Unit) and
-- check the result — a regression test baked into the build. If ≐ᵣ's behaviour
-- ever changes, the corresponding rfl stops type-checking and the build breaks.
-- (The `wand_under_*_no_mgu` worked proofs stay in RowUnify: they are proof
-- demos woven into the stuck-leg development, not rfl regressions.)

import RowUnify

namespace MinimalCalculus

private def uB : Ty Unit := .base ()

-- U-ε.
-- ⊢  unifyRow ε ε  =  success [] []
theorem unify_empty : unifyRow (B := Unit) .empty .empty = .success [] [] := rfl

-- P&X's shared-tail pitfall (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α): U-var-refl
-- right-cancels α, then U-clash — matches shared_tail_no_unifier.
-- ⊢  unifyRow (l:𝓫 | a) (m:𝓫 | a)  =  clash
theorem unify_shared_tail :
    unifyRow (B := Unit) (.cat (.sing "l" uB) (.var "a"))
                         (.cat (.sing "m" uB) (.var "a")) = .clash := rfl

-- The LUtail example (l: 𝓫) ≐ᵣ (α | l: 𝓫): right-match the field, then
-- U-ε-var — finds the mgu α ≔ ε that LUtail misses (lutail_unifier_iff).
-- ⊢  unifyRow (l:𝓫) (a | l:𝓫)  =  success [a ≔ ε] [(𝓫, 𝓫)]
theorem unify_lutail :
    unifyRow (B := Unit) (.sing "l" uB) (.cat (.var "a") (.sing "l" uB)) =
      .success [("a", .empty)] [(uB, uB)] := rfl

-- Wand's ambiguity (β | α) ≐ᵣ (l: 𝓫): STUCK — solvable but no mgu
-- (wand_unifiable, wand_no_mgu).
-- ⊢  unifyRow (b | a) (l:𝓫)  =  stuck
theorem unify_wand :
    unifyRow (B := Unit) (.cat (.var "b") (.var "a")) (.sing "l" uB) =
      .stuck := rfl

-- EQ-RESCUED STUCK (the sharp finding behind the stuck leg). Feeding a field
-- whose TYPE embeds the stuck row-var — (k:{β} | β | α) ≐ᵣ (k:{l:𝓫} | l:𝓫) —
-- matchL peels k, emitting the type equation {β} ≐ {l:𝓫}; the residual
-- (β | α) ≐ᵣ (l:𝓫) is Wand, hence STUCK. But the emitted equation forces
-- β ≈ (l:𝓫), which then forces α ≈ ε: the WHOLE problem has a UNIQUE mgu.
-- So `unifyRow = stuck` does NOT imply "no mgu" — the single row pass does not
-- solve the emitted equations, so its stuck verdict is incomplete whenever an
-- emitted equation constrains a stuck row-var. The honest trichotomy (c) must be
-- stated relative to the emitted equations (see unifySpineF_stuck_no_mgu).
-- ⊢  unifyRow (k:{β} | β | α) (k:{l:𝓫} | l:𝓫)  =  stuck   (yet an mgu exists)
theorem unify_eq_rescued_stuck :
    unifyRow (B := Unit)
      (.cat (.sing "k" (.rcd (.var "b"))) (.cat (.var "b") (.var "a")))
      (.cat (.sing "k" (.rcd (.sing "l" uB))) (.sing "l" uB))
      = .stuck := rfl

-- Worked example 2, (α | l: 𝓫 | β) ≐ᵣ (l: 𝓫): U-ground pairs the l-fields
-- (counting rules the vars out), then U-ε-var forces α ≔ ε, β ≔ ε.
-- ⊢  unifyRow (a | l:𝓫 | b) (l:𝓫)  =  success [a ≔ ε, b ≔ ε] [(𝓫, 𝓫)]
theorem unify_ground_collapse :
    unifyRow (B := Unit) (.cat (.var "a") (.cat (.sing "l" uB) (.var "b")))
                         (.sing "l" uB) =
      .success [("a", .empty), ("b", .empty)] [(uB, uB)] := rfl

-- (β | l: 𝓫 | α) ≐ᵣ (l′: 𝓫), l ≠ l′: U-clash, NOT stuck — the projection
-- check is global, a window-only rule would misfile this.
-- ⊢  unifyRow (b | l:𝓫 | a) (m:𝓫)  =  clash
theorem unify_global_clash :
    unifyRow (B := Unit) (.cat (.var "b") (.cat (.sing "l" uB) (.var "a")))
                         (.sing "m" uB) = .clash := rfl

-- α ≐ᵣ (l: 𝓫 | α): the shared END-var cancels first (solution-preserving!),
-- leaving ε ≐ᵣ (l: 𝓫) — a definite CLASH, strictly stronger than an
-- occurs-failure. Cancellativity subsumes end-aligned occurs cases.
-- ⊢  unifyRow a (l:𝓫 | a)  =  clash
theorem unify_occurs_cancelled :
    unifyRow (B := Unit) (.var "a") (.cat (.sing "l" uB) (.var "a")) =
      .clash := rfl

-- U-var-solve with occurs check: α ≐ᵣ (l: 𝓫 | α | m: 𝓫) — the recursive
-- var is interior, no cancellation applies, genuinely a recursive row.
-- ⊢  unifyRow a (l:𝓫 | a | m:𝓫)  =  occurs
theorem unify_occurs :
    unifyRow (B := Unit) (.var "a")
      (.cat (.sing "l" uB) (.cat (.var "a") (.sing "m" uB))) = .occurs := rfl

-- Var-var: solved union-find style.
-- ⊢  unifyRow a b  =  success [a ≔ (b | ε)] []
theorem unify_var_var :
    unifyRow (B := Unit) (.var "a") (.var "b") =
      .success [("a", .cat (.var "b") .empty)] [] := rfl

-- The ambiguous mirror (α | l: 𝓫) ≐ᵣ (l: 𝓫 | β): both windows closed by a
-- var, both sides have vars — correctly stuck (Levi splits two ways).
-- ⊢  unifyRow (a | l:𝓫) (l:𝓫 | b)  =  stuck
theorem unify_two_sided_stuck :
    unifyRow (B := Unit) (.cat (.var "a") (.sing "l" uB))
                         (.cat (.sing "l" uB) (.var "b")) = .stuck := rfl

-- ## P1 scaffolding, kernel-checked (proof-plan.md §1.1)
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
    (UResM.success (B := Unit) ⟨[("t", uB)], []⟩).seq
        (fun _ => .success ⟨[], [("a", .empty)]⟩) =
      .success ⟨[("t", uB)], [("a", .empty)]⟩ := rfl

-- ⊢  a stuck second stage is the verdict of the whole
theorem seq_propagates :
    (UResM.success (B := Unit) ⟨[("t", uB)], []⟩).seq (fun _ => .stuck) = .stuck := rfl

-- ## P2 freshness, kernel-checked (proof-plan.md §1.4)
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

-- ## P3 unique-host expansion, kernel-checked (proof-plan.md §1.4)
-- ⊢  crossfield FIRES, and picks β as the forced host: β ≔ (l:δ | β′), with the
--    host side keeping its length (β renamed to the fresh β′)
theorem expandL_crossfield :
    expandL (B := Unit) ⟨5⟩ [.field "l" uB, .var "a"] [.field "m" uB, .var "b"]
      = some ("b", "l", uB, [.var "a"], [.field "m" uB, .var (natName 6)]) := rfl

-- ⊢  Wand REFUSES: two candidate hosts, and vars_vs_field_no_mgu proves the rule
--    is right to refuse — there is genuinely no mgu
theorem expandL_wand_refuses :
    expandL (B := Unit) ⟨5⟩ [.field "l" uB] [.var "a", .var "b"] = none := rfl

-- ⊢  an l-field on the other side could host the pairing instead, so REFUSE
--    ((l:𝓪 | α) ≐ᵣ (β | l:𝓫) is unifiable with β ≔ ε)
theorem expandL_lfield_refuses :
    expandL (B := Unit) ⟨5⟩ [.field "l" uB, .var "a"] [.var "b", .field "l" uB]
      = none := rfl

end MinimalCalculus
