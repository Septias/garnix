-- A-let, AND WHAT THE SOUNDNESS STATEMENTS HAVE TO SAY ABOUT IT.
--
-- The day-6 probe was meant to settle one question — does A-let need the
-- backward half of `QCovers`? — and ran into two facts first, both kept here as
-- theorems:
--
--   1. `A-let` generalizes without a Γ-freshness premise. `λy. let z = y in z`
--      then infers `a → b`, which no declarative derivation gives, so `RunSound`
--      is FALSE against the rule as it stood (`runSound_false_unguarded_let`).
--      The fix is the textbook side condition ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅, now a premise
--      of `Infer.letE` (stated per variable of Γ, at both sorts).
--   2. `InferSoundC` read the CONTEXT under ⟦S′⟧ and the TYPE under an arbitrary
--      σ ⊨ S′. That is false at an open Γ already at A-var
--      (`inferSoundC_false`), and it is not inductive at A-lam either, whose
--      step lemma wants the binder at σ. The context has to be read under the
--      same σ as the type.

import InferSound

namespace MinimalCalculus

--------------------- INVERSION FOR THE TWO FORMS THE WITNESS USES -----------

-- ⊢  ≈ never moves a type variable: only refl/symm/trans cross constructors
theorem TyEquiv.var_inv_both {B : Type} :
    {τ σ : Ty B} → TyEquiv τ σ →
    (∀ {α : TyVar}, τ = .var α → σ = .var α) ∧
    (∀ {α : TyVar}, σ = .var α → τ = .var α)
  | _, _, .refl _  => ⟨fun h => h, fun h => h⟩
  | _, _, .symm h  =>
      have ih := TyEquiv.var_inv_both h
      ⟨fun hτ => ih.2 hτ, fun hσ => ih.1 hσ⟩
  | _, _, .trans h₁ h₂ =>
      have ih₁ := TyEquiv.var_inv_both h₁
      have ih₂ := TyEquiv.var_inv_both h₂
      ⟨fun hτ => ih₂.1 (ih₁.1 hτ), fun hσ => ih₁.2 (ih₂.2 hσ)⟩
  | _, _, .fn _ _  => ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩
  | _, _, .rcd _   => ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩

theorem TyEquiv.var_inv {B : Type} {α : TyVar} {σ : Ty B}
    (h : TyEquiv (.var α) σ) : σ = .var α :=
  (TyEquiv.var_inv_both h).1 rfl

-- ⊢  a use of x reads an instance of x's scheme, up to ≈, or is ★
theorem qtyped_var_inv {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    ∀ {x : Var} {σ : QScheme B}, e = .var x → Γ.lookup x = some σ →
    τ = .unk ∨ ∃ τ', QScheme.Inst Γ.ctx σ τ' ∧ TyEquiv τ' τ
  | _, _, _, .qVar h hi => fun he hx => by
      cases he
      rw [h] at hx
      cases Option.some.inj hx
      exact .inr ⟨_, hi, .refl _⟩
  | _, _, _, .qEq h heq => fun he hx =>
      match qtyped_var_inv h he hx with
      | .inl hu => .inl (hu ▸ heq).unk_inv
      | .inr ⟨τ', hi, ht⟩ => .inr ⟨τ', hi, ht.trans heq⟩
  | _, _, _, .qUnk _ => fun _ _ => .inl rfl
  | _, _, _, .qCon => fun he _ => nomatch he
  | _, _, _, .qLam _ => fun he _ => nomatch he
  | _, _, _, .qApp _ _ => fun he _ => nomatch he
  | _, _, _, .qLet _ _ _ => fun he _ => nomatch he
  | _, _, _, .qCat _ _ => fun he _ => nomatch he
  | _, _, _, .qSel _ _ => fun he _ => nomatch he
  | _, _, _, .qSelUnk _ _ => fun he _ => nomatch he
  | _, _, _, .qSelAbs _ _ => fun he _ => nomatch he
  | _, _, _, .qRcd _ => fun he _ => nomatch he

-- ⊢  …at a MONOTYPE binding that instance is the binding itself
theorem qtyped_var_mono_inv {B C : Type} {constTy : C → B} {Γ : QCtx B}
    {x : Var} {τ₀ τ : Ty B} (h : QTyped constTy Γ (.var x) τ)
    (hx : Γ.lookup x = some ⟨[], [], τ₀⟩) : τ = .unk ∨ TyEquiv τ₀ τ :=
  match qtyped_var_inv h rfl hx with
  | .inl hu => .inl hu
  | .inr ⟨_, hi, ht⟩ => .inr (inst_mono_eq hi ▸ ht)

-- ⊢  `let z = y in z` at a monotype y is typed exactly like y: whatever scheme
--    qLet picks, the body's instance of it is one the premise had to type y at
theorem qtyped_let_alias_inv {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    ∀ {y z : Var} {τ₀ : Ty B}, e = .letE z (.var y) (.var z) →
    Γ.lookup y = some ⟨[], [], τ₀⟩ → τ = .unk ∨ TyEquiv τ₀ τ
  | _, _, _, .qLet (σ := σ₀) hinst _ hbody => fun he hy => by
      cases he
      have hz : ∀ (Γ' : QCtx B) (w : Var), (Γ'.bindScheme w σ₀).lookup w = some σ₀ :=
        fun Γ' w => by rw [QCtx.lookup_bindScheme]; simp
      rcases qtyped_var_inv hbody rfl (hz _ _) with hu | ⟨τ', hi, ht⟩
      · exact .inl hu
      · rw [QCtx.ctx_bindScheme] at hi
        rcases qtyped_var_mono_inv (hinst τ' hi) hy with hu | h₀
        · subst hu; exact .inl ht.unk_inv
        · exact .inr (h₀.trans ht)
  | _, _, _, .qEq h heq => fun he hy =>
      match qtyped_let_alias_inv h he hy with
      | .inl hu => .inl (hu ▸ heq).unk_inv
      | .inr ht => .inr (ht.trans heq)
  | _, _, _, .qUnk _ => fun _ _ => .inl rfl
  | _, _, _, .qCon => fun he _ => nomatch he
  | _, _, _, .qVar _ _ => fun he _ => nomatch he
  | _, _, _, .qLam _ => fun he _ => nomatch he
  | _, _, _, .qApp _ _ => fun he _ => nomatch he
  | _, _, _, .qCat _ _ => fun he _ => nomatch he
  | _, _, _, .qSel _ _ => fun he _ => nomatch he
  | _, _, _, .qSelUnk _ _ => fun he _ => nomatch he
  | _, _, _, .qSelAbs _ _ => fun he _ => nomatch he
  | _, _, _, .qRcd _ => fun he _ => nomatch he


--------------------- 1. GENERALIZING A VARIABLE Γ STILL MENTIONS -------------
--
--     λy. let z = y in z
--
-- A-lam draws `a` for y. A-var at y returns `a`, A-let generalizes it to ∀a. a,
-- and A-var at z instantiates that at a fresh `b`. So the program infers
-- `a → b`: the identity at two unrelated types. Declaratively no scheme for z
-- can do that — every instance qLet admits must type `y`, and `y : a` only
-- (`qtyped_let_alias_inv`).

private def laE : Expr Unit := .lam "y" (.letE "z" (.var "y") (.var "z"))
private def laA : TyVar := natName 1
private def laB : TyVar := natName 2
private def laS : SolverState Unit := ⟨Sol.nil, [], [], ⟨2⟩, [(laA, .ty)]⟩
private def laΓ : QCtx Unit := (⟨[], []⟩ : QCtx Unit).bindTy "y" (.var laA)
private def laSc : QScheme Unit := ⟨[laA], [], .var laA⟩
private def laId : TySubst Unit := ⟨fun x => .var x, fun x => .var x⟩
private def laRen : TySubst Unit :=
  ⟨fun x => if x = laA then .var laB else .var x,
   fun x => if x = laA then .var laB else .var x⟩

private theorem laA_ne_laB : laA ≠ laB := fun h => by
  have := natName_inj h; omega

/-- ⊢  **the declarative side refuses the witness.** No derivation gives
`λy. let z = y in z` two unrelated type variables — nor any ★-free pair. -/
theorem letAlias_not_typed {α β : TyVar} (hne : α ≠ β) :
    ¬ QTyped (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ laE
        (.fn (.var α) (.var β)) := by
  intro h
  obtain ⟨τ₁, τ₂, hfn | hu, hb⟩ := qtyped_lam_inv h
  · obtain ⟨σ₁, σ₂, heq, h₁, h₂⟩ := hfn.fn_inv
    cases heq
    rcases qtyped_let_alias_inv (τ₀ := τ₁) hb rfl
        (by rw [QCtx.bindTy, QCtx.lookup_bindScheme]; simp) with hu | h₀
    · subst hu; exact nomatch h₂.unk_inv
    · -- a ≈ τ₁ ≈ τ₂ ≈ b
      have := (h₁.symm.trans (h₀.trans h₂)).var_inv
      cases this; exact hne rfl
  · exact nomatch hu

/-- A-let as it stood: `Infer.letE` without its generalization premise. Stated as
a closure property of `Infer` rather than as a second inductive, because that is
all the witness uses — one application of the rule. -/
def UnguardedLet (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ {Γ : QCtx B} {S S₁ S₂ : SolverState B} {x : Var} {e₁ e₂ : Expr C}
    {τ₁ τ₂ : Ty B} {Δq Δγ : List (Parked B)} {ᾱ : List TyVar} {κs : List Kind},
    Infer constTy Γ S e₁ τ₁ S₁ →
    S₁.kinds.Assigns ᾱ κs →
    S₁.parked = Δq ++ Δγ →
    (∀ p ∈ Δq, p.blocker ∈ ᾱ) → (∀ p ∈ Δγ, p.blocker ∉ ᾱ) →
    Infer constTy
      (Γ.bindScheme x ⟨ᾱ, Δq.map Parked.stump, τ₁.applySubst S₁.subst⟩)
      { S₁ with parked := Δγ } e₂ τ₂ S₂ →
    Infer constTy Γ S (.letE x e₁ e₂) τ₂ S₂

-- the two A-var steps the witness makes, each an ordinary derivation
private theorem la_var_y :
    Infer (B := Unit) (C := Unit) (fun _ => ()) laΓ laS (.var "y") (.var laA) laS :=
  Infer.var_mono rfl
    ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩

-- b is DRAWN: the supply moves from 2 to 3, and b inherits a's kind
private def laS3 : SolverState Unit :=
  { laS with supply := ⟨3⟩, kinds := [(laB, .ty), (laA, .ty)] }

private theorem la_var_z :
    Infer (B := Unit) (C := Unit) (fun _ => ()) (laΓ.bindScheme "z" laSc)
      { laS with parked := [] } (.var "z") (.var laB) laS3 := by
  have hren : IsRenaming laRen laSc.vars (fun _ => laB) := by
    refine ⟨⟨fun α h => ?_, fun α h => ?_⟩, fun α h => ?_⟩
    · have : α ≠ laA := fun he => h (by simp [laSc, he])
      simp [laRen, this]
    · have : α ≠ laA := fun he => h (by simp [laSc, he])
      simp [laRen, this]
    · simp [laSc] at h; subst h; simp [laRen]
  have hfr : FreshRenaming (fun _ => laB) laSc.vars (laΓ.bindScheme "z" laSc)
      { laS with parked := [] } := by
    refine ⟨?_, ?_, ?_, ?_⟩
    · intro α hα β hβ _
      simp only [laSc, List.mem_singleton] at hα hβ
      rw [hα, hβ]
    · intro _ _ h; exact nomatch h
    · intro _ _ _ h; exact nomatch h
    · intro α _ hm
      have hne := laA_ne_laB
      have hl : (laΓ.bindScheme "z" laSc).ftv = [laA, laA, laA] := rfl
      rw [hl] at hm
      simp only [List.mem_cons, List.mem_nil_iff, or_false, or_self] at hm
      exact hne hm.symm
  exact Infer.var (constTy := fun (_ : Unit) => ()) (Γ := laΓ.bindScheme "z" laSc)
    (S := { laS with parked := [] }) (x := "z") (σ := laSc) (θ := laRen)
    (f := fun _ => laB) (ps := []) (Sup := ⟨3⟩)
    (by rw [QCtx.lookup_bindScheme]; simp) hren hfr
    (fun _ _ => ⟨2, Nat.le_refl _, by decide, rfl⟩) (by decide) (κs := [.ty]) rfl rfl
    ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩

/-- ⊢  **`RunSound` fails against the unguarded A-let.** Any inference relation
closed under A-let without the Γ-freshness premise runs `λy. let z = y in z` to
`a → b`, and `letAlias_not_typed` says no declarative derivation agrees. -/
theorem runSound_false_unguarded_let (hu : UnguardedLet Unit Unit (fun _ => ())) :
    ¬ RunSound Unit Unit (fun _ => ()) := by
  intro h
  have hinf : Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩
      ⟨Sol.nil, [], [], ⟨1⟩, []⟩ laE (.fn (.var laA) (.var laB)) laS3 := by
    refine Infer.lam (S₀ := laS) rfl ?_
    exact hu (Δq := []) (Δγ := []) (ᾱ := [laA]) (κs := [.ty]) la_var_y
      (by show [laA].map laS.kinds.lookup = [some Kind.ty]; simp [KEnv.lookup, laS])
      rfl (fun _ h => nomatch h) (fun _ h => nomatch h) la_var_z
  exact letAlias_not_typed laA_ne_laB (h _ _ _ ⟨laS3, hinf, .nil⟩)

/-- ⊢  **…and the premise is exactly what the witness violates**: the generalized
`a` is y's type, a variable of Γ that ⟦S₁⟧ leaves in place. -/
theorem letAlias_premise_fails :
    ∃ β ∈ laΓ.ftv, laA ∈ (laS.subst.ty β).ftv :=
  ⟨laA, by show laA ∈ [laA]; simp, by show laA ∈ [laA]; simp⟩

/-- ⊢  the guarded rule still runs the program — generalizing nothing, at the
declaratively right `a → a`. -/
theorem letAlias_infers_guarded :
    Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩
      ⟨Sol.nil, [], [], ⟨1⟩, []⟩ laE (.fn (.var laA) (.var laA)) laS := by
  refine Infer.lam (S₀ := laS) rfl ?_
  refine Infer.letE (Δq := []) (Δγ := []) (ᾱ := []) (κs := []) la_var_y rfl (.refl _)
    (fun _ h => nomatch h) (fun _ h => nomatch h) (fun _ h => nomatch h)
    (fun _ h => nomatch h) ⟨fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil⟩ (fun _ h => nomatch h)
    (fun _ h => nomatch h) (fun _ h => nomatch h) ?_
  exact Infer.var_mono
    (by rw [QCtx.lookup_bindScheme]; simp; rfl)
    ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩

--------------------- 2. InferSoundC READ Γ UNDER THE WRONG SUBSTITUTION -------
-- `InferSoundC` concludes at `S′.applyCtx Γ` — Γ under ⟦S′⟧ — and at τ under an
-- ARBITRARY σ ⊨ S′. At an open Γ the two disagree the moment σ refines a
-- variable Γ mentions: with `y : a` and nothing solved, σ := [a ↦ 𝓫] satisfies
-- the state, and the statement then asks for `y : a ⊢ y : 𝓫`.
--
-- The same mismatch makes the statement non-inductive at A-lam: the IH is at
-- ⟦S′⟧(Γ, y:a) = ⟦S′⟧Γ, y:⟦S′⟧a, while `infer_sound_lam_step` wants the binder
-- at σ a. Both go away when the context is read under σ too, with the row
-- environment discharged — the shape every step lemma already assumes
-- (`hrow : Γ'.rowEnv = []`).

private def laSub : TySubst Unit :=
  ⟨fun x => if x = laA then .base () else .var x, fun x => .var x⟩

/-- ⊢  **`InferSoundC` is false as stated.** -/
theorem inferSoundC_false : ¬ InferSoundC Unit Unit (fun _ => ()) := by
  intro h
  have hsat : Sol.Sat laSub laS.sol := ⟨fun _ h => (nomatch h), fun _ h => (nomatch h)⟩
  have ht := (h laΓ laS laS (.var "y") (.var laA) la_var_y laSub hsat
    (fun _ hp => nomatch hp)).toQTyped
  have hl : (laS.applyCtx laΓ).lookup "y" = some ⟨[], [], .var laA⟩ := rfl
  rcases qtyped_var_mono_inv ht hl with hu | he
  · exact nomatch hu
  · exact nomatch he.var_inv

--------------------- 3. GENERALIZING AN OUTER STUMP AWAY ---------------------
--
--     { a = λx. x.l,  b = let y = c in c }
--
-- Field a parks `⟨2 ▷ 2.l ↓ 3⟩` — x's record row is 2, the answer 3. Field b's
-- let may generalize ANY variable ⟦S₁⟧Γ does not mention, and Γ is empty there,
-- so ᾱ = [2] passes the Γ-freshness premise. A-let then files every stump
-- blocked in ᾱ under the scheme of y — including field a's, which has nothing to
-- do with y. y is never used, the stump is gone from the state, nothing ever
-- finalizes it, and the run ends with `a : {2} → 3` and 3 unconstrained.
-- Declaratively x.l at a free row is `?`, so its only typing is ★.

private def lcS1 : SolverState Unit :=
  ⟨⟨[(natName 1, .rcd (.var (natName 2)))], []⟩,
   [⟨natName 2, ⟨.var (natName 2), "l", natName 3⟩⟩], [], ⟨4⟩,
   [(natName 3, .ty), (natName 2, .row), (natName 1, .ty)]⟩

private def lcS2 : SolverState Unit := { lcS1 with parked := [] }

private def lcE : Expr Unit :=
  .rcd (.cat (.field "a" (selEx Unit))
             (.field "b" (.letE "y" (.con ()) (.con ()))))

-- FIXED: `Infer.letE` now requires Δ_q to be e₁'s own stumps (none parked
-- before the let), their answers generalized with them, and Δ_Γ free of ᾱ.
-- The witness below is kept against `UnguardedLet`, which has none of these;
-- `letCapture_premise_fails` is the one it violates.

-- ⊢  a selection at a free row has only the ★ typing, so `λx. x.l` is never
--    `{r} → d` for a variable d
private theorem selEx_not_var_result {r d : TyVar} {τa : Ty Unit}
    (h : QTyped (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ (selEx Unit) τa)
    (he : TyEquiv τa (.fn (.rcd (.var r)) (.var d))) : False := by
  obtain ⟨τ₁, τ₂, hfn | hu, hb⟩ := qtyped_lam_inv h
  · obtain ⟨σ₁, σ₂, heq, h₁, h₂⟩ := (hfn.trans he).fn_inv
    cases heq
    obtain ⟨ρ, res, hρ, hl, hbel⟩ := qsel_var_inv (τx := τ₁) hb rfl
      (by rw [QCtx.bindTy, QCtx.lookup_bindScheme]; simp)
    obtain ⟨ρ', hρ', hre⟩ := (hρ.symm.trans h₁).rcd_inv
    cases hρ'
    obtain ⟨r', hl', hres⟩ := lookup_equiv hre hl
    have hr' : r' = .unknown := by
      cases hl' with
      | var hα _ => simp [QCtx.bindTy, QCtx.bindScheme, QCtx.ctx, Ctx.lookupRow] at hα
      | varFree _ => rfl
    subst hr'
    cases hres
    obtain ⟨τ₀, h₀, hp⟩ := hbel
    have := h₀.unk_inv
    subst this
    have hτ₂ : τ₂ = .unk := by cases hp <;> rfl
    subst hτ₂
    exact nomatch h₂.unk_inv
  · subst hu; exact nomatch he.unk_inv

/-- ⊢  **the declarative side refuses the witness's type.** -/
theorem letCapture_not_typed :
    ¬ QTyped (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ lcE
        (.rcd (.cat (.sing "a" (.fn (.rcd (.var (natName 2))) (.var (natName 3))))
                    (.sing "b" (.base ())))) := by
  intro h
  obtain ⟨ρ, he | hu, hb⟩ := qtyped_rcd_inv h
  · obtain ⟨ρT, hT, hre⟩ := he.rcd_inv
    cases hT
    cases hb with
    | cat ha _ =>
        cases ha with
        | field hea =>
            obtain ⟨r₂, hl₂, hres⟩ :=
              lookup_equiv (Γ := (⟨[], []⟩ : Ctx Unit)) hre (.catHit .hit)
            have := lookup_det hl₂ (.catHit .hit)
            subst this
            cases hres with
            | found ht => exact selEx_not_var_result hea ht
  · exact nomatch hu

/-- ⊢  **…and inference, as A-let stood, runs to it.** -/
theorem letCapture_infers_unguarded (hu : UnguardedLet Unit Unit (fun _ => ())) :
    Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ ⟨Sol.nil, [], [], ⟨1⟩, []⟩
      lcE (.rcd (.cat (.sing "a" (.fn (.var (natName 1)) (.var (natName 3))))
                      (.sing "b" (.base ())))) lcS2 := by
  refine Infer.rcd (.cat (.field selEx_infers) (.field ?_))
  exact hu (S₁ := lcS1) (Δq := lcS1.parked) (Δγ := []) (ᾱ := [natName 2])
    (κs := [.row]) .con rfl (List.append_nil _).symm
    (fun p hp => by
      simp only [lcS1, List.mem_cons, List.not_mem_nil, or_false] at hp
      subst hp; decide)
    (fun _ h => nomatch h) .con

/-- ⊢  **`RunSound` fails against it.** Field a's stump was generalized into a
scheme nobody instantiates, so no finalization ever commits its answer. -/
theorem runSound_false_let_captures (hu : UnguardedLet Unit Unit (fun _ => ())) :
    ¬ RunSound Unit Unit (fun _ => ()) := fun h =>
  letCapture_not_typed (h _ _ _ ⟨lcS2, letCapture_infers_unguarded hu, .nil⟩)

/-- ⊢  the witness violates the new ownership premise: y's let is entered at
`lcS1` (e₁ is a constant, so it is also S₁), and Δ_q = `lcS1.parked` — the stump
field a parked before the let. -/
theorem letCapture_premise_fails :
    ∃ p ∈ lcS1.parked, ∃ q ∈ lcS1.parked, p.stump = q.stump :=
  ⟨_, List.mem_cons_self, _, List.mem_cons_self, rfl⟩

end MinimalCalculus
