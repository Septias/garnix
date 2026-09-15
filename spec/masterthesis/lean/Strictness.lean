-- L1 ⊊ L2: the qualified system types strictly more than the plain one.
--
-- Qualified.lean exhibits the two-use program typed at {a: 𝓫_c | b: ★} in L2
-- (qtyped_two_use) and asserts in a COMMENT that no plain scheme could serve
-- both uses. That assertion is the last load-bearing claim of the metatheory
-- that was never mechanized — no_plain_principal_scheme is about λx.x.l at ONE
-- pair of instances, not about this program. This module proves it.
--
-- THE TENSION, in one line: the a-use demands a DEFINITE non-★ result, which
-- forces l into the domain; the b-use passes {}, which forces the domain to be
-- ε. One plain scheme cannot have both, because its result position would have
-- to be a quantified variable — and re-pointing that variable inside the
-- ε-domain instance produces an instance that is not a typing.

import Qualified
import RowEquiv

namespace MinimalCalculus

-- (l: τ) ≉ ε — ≈ preserves the l-projection, and ε has none.
private theorem sing_not_empty {B : Type} {l : Label} {τ : Ty B}
    (h : RowEquiv (.sing l τ) .empty) : False := by
  obtain ⟨-, hpc⟩ := h.char
  have hfalse := hpc l
  simp [Row.toSpine, sProj] at hfalse
  cases hfalse

-- A row whose image is ≈ ε carries no field, and only a field consults the TYPE
-- component of a substitution — so such an image cannot move when the .ty part
-- is changed. This is what lets the mixed instance below keep its domain.
-- ⊢  θ'.row = θ.row,  ρ[θ] ≈ᵣ ε   ⟹   ρ[θ'] = ρ[θ]
private theorem row_image_empty_ty_irrelevant {B : Type} {θ θ' : TySubst B}
    (hrow : ∀ γ, θ'.row γ = θ.row γ) :
    (ρ : Row B) → RowEquiv (ρ.applySubst θ) .empty →
    ρ.applySubst θ' = ρ.applySubst θ
  | .empty, _ => rfl
  | .var γ, _ => hrow γ
  | .sing _ _, h => (sing_not_empty h).elim
  | .cat ρ₁ ρ₂, h => by
      obtain ⟨h₁, h₂⟩ := RowEquiv.cat_empty_split h
      simp only [Row.applySubst,
        row_image_empty_ty_irrelevant hrow ρ₁ h₁,
        row_image_empty_ty_irrelevant hrow ρ₂ h₂]

-- NO PLAIN SCHEME SERVES BOTH USES.
-- ⊢  ¬∃σ. (every instance a typing of λx.x.l)
--          ∧ (an instance A → R with R ≈ 𝓫)
--          ∧ (an instance A' → R' with A' ≈ {ε})
theorem no_plain_scheme_two_use {B C : Type} (constTy : C → B) (b : B) :
    ¬ ∃ σ : Scheme B,
        (∀ τ, σ.Inst τ → Typed constTy Ctx.empty (selEx C) τ) ∧
        (∃ A R, σ.Inst (.fn A R) ∧ TyEquiv R (.base b)) ∧
        (∃ A' R', σ.Inst (.fn A' R') ∧ TyEquiv A' (.rcd .empty)) := by
  rintro ⟨σ, hclosed, ⟨A, R, ⟨θa, hfa, hba⟩, hR⟩, ⟨A', R', ⟨θb, hfb, hbb⟩, hA'⟩⟩
  -- an ε-domain typing of λx.x.l has result ★: the lookup on ε is ⊥
  have hR'unk : R' = .unk := by
    obtain ⟨τ₁, τ₂, heq | hu, hbody⟩ := typed_lam_inv (hclosed _ ⟨θb, hfb, hbb⟩)
    · obtain ⟨s₁, s₂, hs, he₁, he₂⟩ := heq.fn_inv
      cases hs
      have hx : (Ctx.empty.bindTy "x" τ₁).lookup "x" = some ⟨[], τ₁⟩ := by
        simp [Ctx.lookup_bindTy]
      have hτ₂ := sel_var_unk hbody rfl hx (he₁.trans hA')
      exact (hτ₂ ▸ he₂).unk_inv
    · cases hu
  subst hR'unk
  cases hbodyeq : σ.body with
  | base b0 => rw [hbodyeq] at hba; simp only [Ty.applySubst] at hba; cases hba
  | unk => rw [hbodyeq] at hba; simp only [Ty.applySubst] at hba; cases hba
  | rcd ρ => rw [hbodyeq] at hba; simp only [Ty.applySubst] at hba; cases hba
  | var α =>
      by_cases hα : α ∈ σ.vars
      · -- a bare quantified body instantiates to {ε}, which λx.x.l never has
        have hinst : σ.Inst (.rcd .empty : Ty B) := by
          refine ⟨⟨fun δ => if δ = α then .rcd .empty else .var δ, fun δ => .var δ⟩,
            ⟨fun δ hδ => ?_, fun _ _ => rfl⟩, ?_⟩
          · have hne : ¬δ = α := by rintro rfl; exact hδ hα
            simp [hne]
          · rw [hbodyeq]; simp [Ty.applySubst]
        obtain ⟨τ₁, τ₂, heq | hu, -⟩ := typed_lam_inv (hclosed _ hinst)
        · obtain ⟨s₁, s₂, hs, -, -⟩ := heq.fn_inv
          cases hs
        · cases hu
      · rw [hbodyeq] at hba
        simp only [Ty.applySubst] at hba
        rw [hfa.1 α hα] at hba
        cases hba
  | fn dom res =>
      rw [hbodyeq] at hba hbb
      simp only [Ty.applySubst] at hba hbb
      injection hba with hda hra
      injection hbb with hdb hrb
      cases res with
      | base b0 => simp only [Ty.applySubst] at hrb; cases hrb
      | unk =>
          simp only [Ty.applySubst] at hra
          cases (hra ▸ hR : TyEquiv (.unk : Ty B) (.base b)).unk_inv
      | fn _ _ => simp only [Ty.applySubst] at hrb; cases hrb
      | rcd _ => simp only [Ty.applySubst] at hrb; cases hrb
      | var α =>
          simp only [Ty.applySubst] at hra hrb
          -- the result variable is quantified: a free one would image to itself
          have hα : α ∈ σ.vars := by
            by_cases h : α ∈ σ.vars
            · exact h
            · rw [hfb.1 α h] at hrb; cases hrb
          -- θb, with the result variable re-pointed at {ε}
          have hmfix :
              (⟨fun δ => if δ = α then .rcd .empty else θb.ty δ, θb.row⟩ :
                TySubst B).FixedOutside σ.vars := by
            refine ⟨fun δ hδ => ?_, fun δ hδ => hfb.2 δ hδ⟩
            have hne : ¬δ = α := by rintro rfl; exact hδ hα
            simpa [hne] using hfb.1 δ hδ
          -- the domain does not move: its image is ≈ ε, hence field-free
          have hdom : dom.applySubst
              (⟨fun δ => if δ = α then .rcd .empty else θb.ty δ, θb.row⟩ :
                TySubst B) = A' := by
            cases dom with
            | base b0 =>
                simp only [Ty.applySubst] at hdb
                cases (hdb ▸ hA').base_inv
            | unk =>
                simp only [Ty.applySubst] at hdb
                cases (hdb ▸ hA').unk_inv
            | fn x y =>
                simp only [Ty.applySubst] at hdb
                obtain ⟨s₁, s₂, hs, -, -⟩ := (hdb ▸ hA').fn_inv
                cases hs
            | var γ =>
                simp only [Ty.applySubst] at hdb
                have hne : γ ≠ α := by
                  rintro rfl
                  rw [hrb] at hdb
                  cases (hdb ▸ hA').unk_inv
                simp only [Ty.applySubst, if_neg hne]
                exact hdb
            | rcd ρd =>
                simp only [Ty.applySubst] at hdb
                have hemp : RowEquiv (ρd.applySubst θb) .empty := by
                  obtain ⟨ρ', hρ', hre⟩ := (hdb ▸ hA').rcd_inv
                  injection hρ' with hρ'
                  exact hρ' ▸ hre
                have hrw := row_image_empty_ty_irrelevant
                  (θ := θb)
                  (θ' := (⟨fun δ => if δ = α then (.rcd .empty : Ty B)
                                    else θb.ty δ, θb.row⟩ : TySubst B))
                  (fun _ => rfl) ρd hemp
                simp only [Ty.applySubst, hrw]
                exact hdb
          have hinst : σ.Inst (.fn A' (.rcd .empty)) := by
            refine ⟨_, hmfix, ?_⟩
            rw [hbodyeq]
            simp only [Ty.applySubst, hdom]
            simp
          -- ... and that instance is not a typing: an ε-domain forces ★
          obtain ⟨τ₁, τ₂, heq | hu, hbody⟩ := typed_lam_inv (hclosed _ hinst)
          · obtain ⟨s₁, s₂, hs, he₁, he₂⟩ := heq.fn_inv
            cases hs
            have hx : (Ctx.empty.bindTy "x" τ₁).lookup "x" = some ⟨[], τ₁⟩ := by
              simp [Ctx.lookup_bindTy]
            have hτ₂ := sel_var_unk hbody rfl hx (he₁.trans hA')
            rw [hτ₂] at he₂
            cases he₂.unk_inv
          · cases hu

-- The program of qtyped_two_use: one binding, two uses at incompatible
-- refined instances.
def twoUse (C : Type) (c : C) : Expr C :=
  .letE "f" (selEx C)
    (.rcd (.cat
      (.field "a" (.app (.var "f") (.rcd (.field "l" (.con c)))))
      (.field "b" (.app (.var "f") (.rcd .empty)))))

-- L1 CANNOT TYPE IT. Reading the derivation backwards: the record body pins the
-- two field types (≈ cancels leading fields), the a-use hands the let-bound
-- scheme an instance whose result is ≈ 𝓫_c, the b-use hands it one whose domain
-- accepts {} — and no_plain_scheme_two_use says no plain scheme has both.
-- The ★-domain escape (the b-use argument typed at ★) is closed separately:
-- λx.x.l has no typing with domain ★ (selEx_no_unk_domain).
-- ⊢  ¬ ( ∅ ⊢ let f = λx.x.l in {a = f {l=c} | b = f {}} : {a: 𝓫_c | b: ★} )
theorem not_typed_two_use {B C : Type} (constTy : C → B) (c : C) :
    ¬ Typed constTy Ctx.empty (twoUse C c)
        (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" .unk))) := by
  intro h
  rcases typed_let_inv h rfl with hu | ⟨σ, τ', hprem, hbody, hre⟩
  · cases hu
  obtain ⟨ρ, hrow | hu, hb⟩ := typed_rcd_inv hbody
  case inr => rw [hu] at hre; cases hre.unk_inv
  case inl =>
  -- the body's row is ≈ the target row, and TypedBody fixes its shape exactly
  obtain ⟨R, hR, hρ⟩ := (hrow.trans hre).rcd_inv
  injection hR with hR
  subst hR
  cases hb with
  | cat hba hbb =>
      cases hba with
      | field ha =>
          cases hbb with
          | field hbf =>
              -- ≈ cancels the leading a-field, then the b-field
              obtain ⟨hτa, -⟩ := RowEquiv.field_cancel_left hρ
              have hfl : (Ctx.empty.bindScheme "f" σ).lookup "f" = some σ := by
                simp [Ctx.lookup, Ctx.bindScheme]
              refine no_plain_scheme_two_use constTy (constTy c)
                ⟨σ, hprem, ?_, ?_⟩
              · -- the a-use: a DEFINITE result forces an instance A → R, R ≈ 𝓫_c
                rcases typed_app_inv ha rfl with hau | ⟨τ₁, τr, hf, -, hres⟩
                · rw [hau] at hτa; cases hτa.unk_inv
                · rcases var_scheme_inv hf rfl hfl with hvu | ⟨τ₀, hinst, hτ₀⟩
                  · cases hvu
                  · obtain ⟨s₁, s₂, hs, -, he₂⟩ := hτ₀.symm.fn_inv
                    subst hs
                    exact ⟨s₁, s₂, hinst, (he₂.symm.trans hres).trans hτa⟩
              · -- the b-use: the argument {} forces the domain to be ≈ ε
                obtain ⟨τd, τres, hf2, harg2⟩ := typed_app_shape hbf rfl
                rcases var_scheme_inv hf2 rfl hfl with hvu | ⟨τ₀b, hinstb, hτ₀b⟩
                · cases hvu
                · obtain ⟨u₁, u₂, hsu, he₁u, -⟩ := hτ₀b.symm.fn_inv
                  subst hsu
                  obtain ⟨ρ0, hrr | hru, hbe⟩ := typed_rcd_inv harg2
                  · cases hbe
                    exact ⟨u₁, u₂, hinstb, (hrr.trans he₁u).symm⟩
                  · rw [hru] at he₁u
                    have hu₁ : u₁ = .unk := he₁u.unk_inv
                    subst hu₁
                    exact absurd (hprem _ hinstb) selEx_no_unk_domain

-- L1 ⊊ L2, both halves in one statement. The ⊆ direction is Typed.toQ.
-- ⊢  ∅ ⊢_Q twoUse : {a: 𝓫_c | b: ★}   ∧   ¬ (∅ ⊢ twoUse : {a: 𝓫_c | b: ★})
theorem l1_strictly_weaker_than_l2 {B C : Type} (constTy : C → B) (c : C) :
    QTyped constTy (⟨[], []⟩ : QCtx B) (twoUse C c)
        (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" .unk))) ∧
    ¬ Typed constTy Ctx.empty (twoUse C c)
        (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" .unk))) :=
  ⟨qtyped_two_use constTy c, not_typed_two_use constTy c⟩

end MinimalCalculus
