-- Lean 4 formalization of masterthesis/minimal.typ
-- Minimal Calculus: functions, scoped records, record concat, row-vars,
-- row equivalence, three-way row lookup (τ | ⊥ | ?), let-polymorphism
-- (instance-closed T-let, T-sel-⊥, T-★-intro)

namespace MinimalCalculus

---------------------------------- TERMS -----------------------------------
--  l ∈ 𝓛   x ∈ 𝓧   𝓫 ∈ 𝓑   c ∈ 𝓒
abbrev Label  := String
abbrev Var    := String
abbrev TyVar  := String

--   e := c | x | (x: e) | e₁e₂ | e₁ ‖ e₂ | e.l | { ξ } | let x = e₁ in e₂
--   ξ := ε | l = e | (ξ₁ | ξ₂)

inductive RecBody (Term : Type) : Type where
  | empty : RecBody Term                             -- ε
  | field : Label → Term → RecBody Term              -- l = e
  | cat   : RecBody Term → RecBody Term → RecBody Term  -- ξ₁ | ξ₂

-- Leftmost occurrence wins (scoped labels)
def RecBody.lookup {α : Type} (l : Label) : RecBody α → Option α
  | .empty      => none
  | .field l' e => if l == l' then some e else none
  | .cat b₁ b₂  =>
      match RecBody.lookup l b₁ with
      | some e => some e
      | none   => RecBody.lookup l b₂

inductive Expr (Const : Type) : Type where
  | con  : Const → Expr Const                            -- c
  | var  : Var → Expr Const                              -- x
  | lam  : Var → Expr Const → Expr Const                 -- (x: e)
  | app  : Expr Const → Expr Const → Expr Const          -- e₁ e₂
  | cat  : Expr Const → Expr Const → Expr Const          -- e₁ ‖ e₂
  | sel  : Expr Const → Label → Expr Const               -- e.l
  | rcd  : RecBody (Expr Const) → Expr Const             -- { ξ }
  | letE : Var → Expr Const → Expr Const → Expr Const    -- let x = e₁ in e₂


---------------------------------- TYPES ------------------------------------------

-- τ := α | 𝓫 | ★ | τ → τ | { ρ }
-- ρ := ε | α | l: τ | (ρ₁ | ρ₂)

mutual
  inductive Ty (B : Type) : Type where
    | var  : TyVar → Ty B                         -- α
    | base : B → Ty B                             -- 𝓫
    | unk  : Ty B                                 -- ★
    | fn   : Ty B → Ty B → Ty B                   -- τ → τ
    | rcd  : Row B → Ty B                         -- { ρ }

  inductive Row (B : Type) : Type where
    | empty : Row B                               -- ε
    | var   : TyVar → Row B                       -- α
    | sing  : Label → Ty B → Row B                -- l: τ
    | cat   : Row B → Row B → Row B               -- ρ₁ | ρ₂
end


-- ## Type schemes  σ := ∀ᾱ. τ
-- ᾱ quantifies over both sorts at once: at instantiation each α ∈ ᾱ may be
-- replaced by a type at Ty-positions (I-ty) and by a row at Row-positions
-- (I-row). Monotypes embed as ⟨[], τ⟩. (Instantiation itself — Scheme.Inst —
-- lives further down, after type substitution is defined.)

structure Scheme (B : Type) where
  vars : List TyVar
  body : Ty B


------------------------------ ROW EQUIVALENCE ---------------------------------
-- ρ₁ ≈ ρ₂  (mutually with τ₁ ≈ τ₂, since types contain rows)
--
-- Rows are equal up to:
--   assoc        reassociating concatenation
--   unitL/unitR  ε is a unit of concatenation
--   comm         swapping two adjacent singletons with *distinct* labels
-- Deliberately NOT derivable:
--   swapping equal labels        (would change shadowing)
--   moving a field past a row-var (its instantiation could shadow it)
-- [Paszke & Xie '23, Fig. 3]

mutual
  inductive TyEquiv {B : Type} : Ty B → Ty B → Prop where
    | refl  (τ : Ty B) : TyEquiv τ τ
    | symm  : TyEquiv τ₁ τ₂ → TyEquiv τ₂ τ₁
    | trans : TyEquiv τ₁ τ₂ → TyEquiv τ₂ τ₃ → TyEquiv τ₁ τ₃
    | fn    : TyEquiv τ₁ τ₁' → TyEquiv τ₂ τ₂' →
              TyEquiv (.fn τ₁ τ₂) (.fn τ₁' τ₂')
    | rcd   : RowEquiv ρ₁ ρ₂ → TyEquiv (.rcd ρ₁) (.rcd ρ₂)

  inductive RowEquiv {B : Type} : Row B → Row B → Prop where
    | refl  (ρ : Row B) : RowEquiv ρ ρ
    | symm  : RowEquiv ρ₁ ρ₂ → RowEquiv ρ₂ ρ₁
    | trans : RowEquiv ρ₁ ρ₂ → RowEquiv ρ₂ ρ₃ → RowEquiv ρ₁ ρ₃
    | sing  : TyEquiv τ₁ τ₂ → RowEquiv (.sing l τ₁) (.sing l τ₂)
    | cat   : RowEquiv ρ₁ ρ₁' → RowEquiv ρ₂ ρ₂' →
              RowEquiv (.cat ρ₁ ρ₂) (.cat ρ₁' ρ₂')
    | assoc : RowEquiv (.cat (.cat ρ₁ ρ₂) ρ₃) (.cat ρ₁ (.cat ρ₂ ρ₃))
    | unitL : RowEquiv (.cat .empty ρ) ρ
    | unitR : RowEquiv (.cat ρ .empty) ρ
    | comm  : l₁ ≠ l₂ →
              RowEquiv (.cat (.sing l₁ τ₁) (.sing l₂ τ₂))
                       (.cat (.sing l₂ τ₂) (.sing l₁ τ₁))
end

infix:50 " ≈ᵣ " => RowEquiv
infix:50 " ≈ₜ " => TyEquiv


------------------------- ≈ PRESERVES HEAD CONSTRUCTORS ------------------------
-- Only refl/symm/trans cross constructor boundaries and the congruence rules
-- keep heads fixed, so ≈ₜ never changes the outermost constructor. Stated as
-- two-directional inversions (symm makes a one-directional IH too weak).

--   If  τ ≈ₜ σ,  then arrow shapes carry over in both directions:
--
--     τ = τ₁ → τ₂   ⟹   ∃ σ₁ σ₂.  σ = σ₁ → σ₂  ∧  τ₁ ≈ₜ σ₁  ∧  τ₂ ≈ₜ σ₂
--     σ = σ₁ → σ₂   ⟹   ∃ τ₁ τ₂.  τ = τ₁ → τ₂  ∧  τ₁ ≈ₜ σ₁  ∧  τ₂ ≈ₜ σ₂
--
--   (Both directions at once because ≈ₜ has symm: a one-directional
--   induction hypothesis would be unusable in the symm case.)


theorem TyEquiv.fn_inv_both {B : Type} :
    {τ σ : Ty B} → TyEquiv τ σ →
    (∀ {τ₁ τ₂ : Ty B}, τ = .fn τ₁ τ₂ →
      ∃ σ₁ σ₂, σ = .fn σ₁ σ₂ ∧ TyEquiv τ₁ σ₁ ∧ TyEquiv τ₂ σ₂) ∧
    (∀ {σ₁ σ₂ : Ty B}, σ = .fn σ₁ σ₂ →
      ∃ τ₁ τ₂, τ = .fn τ₁ τ₂ ∧ TyEquiv τ₁ σ₁ ∧ TyEquiv τ₂ σ₂)
  | _, _, .refl _ =>
      ⟨fun h => ⟨_, _, h, .refl _, .refl _⟩,
       fun h => ⟨_, _, h, .refl _, .refl _⟩⟩
  | _, _, .symm h =>
      have ih := TyEquiv.fn_inv_both h
      ⟨fun hτ => match ih.2 hτ with
        | ⟨_, _, hσ, e₁, e₂⟩ => ⟨_, _, hσ, e₁.symm, e₂.symm⟩,
       fun hσ => match ih.1 hσ with
        | ⟨_, _, hτ, e₁, e₂⟩ => ⟨_, _, hτ, e₁.symm, e₂.symm⟩⟩
  | _, _, .trans h₁ h₂ =>
      have ih₁ := TyEquiv.fn_inv_both h₁
      have ih₂ := TyEquiv.fn_inv_both h₂
      ⟨fun hτ => match ih₁.1 hτ with
        | ⟨_, _, hμ, e₁, e₂⟩ => match ih₂.1 hμ with
          | ⟨_, _, hσ, f₁, f₂⟩ => ⟨_, _, hσ, e₁.trans f₁, e₂.trans f₂⟩,
       fun hσ => match ih₂.2 hσ with
        | ⟨_, _, hμ, f₁, f₂⟩ => match ih₁.2 hμ with
          | ⟨_, _, hτ, e₁, e₂⟩ => ⟨_, _, hτ, e₁.trans f₁, e₂.trans f₂⟩⟩
  | _, _, .fn e₁ e₂ =>
      ⟨fun hτ => (by cases hτ; exact ⟨_, _, rfl, e₁, e₂⟩),
       fun hσ => (by cases hσ; exact ⟨_, _, rfl, e₁, e₂⟩)⟩
  | _, _, .rcd _ =>
      ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩

--   If  τ ≈ₜ σ,  then record shapes carry over in both directions:
--
--     τ = { ρ }    ⟹   ∃ ρ'.  σ = { ρ' }  ∧  ρ ≈ᵣ ρ'
--     σ = { ρ' }   ⟹   ∃ ρ.   τ = { ρ }   ∧  ρ ≈ᵣ ρ'
theorem TyEquiv.rcd_inv_both {B : Type} :
    {τ σ : Ty B} → TyEquiv τ σ →
    (∀ {ρ : Row B}, τ = .rcd ρ → ∃ ρ', σ = .rcd ρ' ∧ RowEquiv ρ ρ') ∧
    (∀ {ρ' : Row B}, σ = .rcd ρ' → ∃ ρ, τ = .rcd ρ ∧ RowEquiv ρ ρ')
  | _, _, .refl _ =>
      ⟨fun h => ⟨_, h, .refl _⟩, fun h => ⟨_, h, .refl _⟩⟩
  | _, _, .symm h =>
      have ih := TyEquiv.rcd_inv_both h
      ⟨fun hτ => match ih.2 hτ with
        | ⟨_, hσ, e⟩ => ⟨_, hσ, e.symm⟩,
       fun hσ => match ih.1 hσ with
        | ⟨_, hτ, e⟩ => ⟨_, hτ, e.symm⟩⟩
  | _, _, .trans h₁ h₂ =>
      have ih₁ := TyEquiv.rcd_inv_both h₁
      have ih₂ := TyEquiv.rcd_inv_both h₂
      ⟨fun hτ => match ih₁.1 hτ with
        | ⟨_, hμ, e⟩ => match ih₂.1 hμ with
          | ⟨_, hσ, f⟩ => ⟨_, hσ, e.trans f⟩,
       fun hσ => match ih₂.2 hσ with
        | ⟨_, hμ, f⟩ => match ih₁.2 hμ with
          | ⟨_, hτ, e⟩ => ⟨_, hτ, e.trans f⟩⟩
  | _, _, .fn _ _ =>
      ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩
  | _, _, .rcd e =>
      ⟨fun hτ => (by cases hτ; exact ⟨_, rfl, e⟩),
       fun hσ => (by cases hσ; exact ⟨_, rfl, e⟩)⟩

--   If  τ ≈ₜ σ,  then base types are rigid in both directions:
--
--     τ = 𝓫   ⟹   σ = 𝓫          σ = 𝓫   ⟹   τ = 𝓫
theorem TyEquiv.base_inv_both {B : Type} :
    {τ σ : Ty B} → TyEquiv τ σ →
    (∀ {b : B}, τ = .base b → σ = .base b) ∧
    (∀ {b : B}, σ = .base b → τ = .base b)
  | _, _, .refl _  => ⟨fun h => h, fun h => h⟩
  | _, _, .symm h  =>
      have ih := TyEquiv.base_inv_both h
      ⟨fun hτ => ih.2 hτ, fun hσ => ih.1 hσ⟩
  | _, _, .trans h₁ h₂ =>
      have ih₁ := TyEquiv.base_inv_both h₁
      have ih₂ := TyEquiv.base_inv_both h₂
      ⟨fun hτ => ih₂.1 (ih₁.1 hτ), fun hσ => ih₁.2 (ih₂.2 hσ)⟩
  | _, _, .fn _ _  => ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩
  | _, _, .rcd _   => ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩

--   If  τ ≈ₜ σ,  then ★ is rigid in both directions (★ has no congruence
--   rule in ≈, deliberately — T-★-intro lives in the typing relation instead):
--
--     τ = ★   ⟹   σ = ★          σ = ★   ⟹   τ = ★
theorem TyEquiv.unk_inv_both {B : Type} :
    {τ σ : Ty B} → TyEquiv τ σ →
    (τ = .unk → σ = .unk) ∧ (σ = .unk → τ = .unk)
  | _, _, .refl _  => ⟨fun h => h, fun h => h⟩
  | _, _, .symm h  =>
      have ih := TyEquiv.unk_inv_both h
      ⟨fun hτ => ih.2 hτ, fun hσ => ih.1 hσ⟩
  | _, _, .trans h₁ h₂ =>
      have ih₁ := TyEquiv.unk_inv_both h₁
      have ih₂ := TyEquiv.unk_inv_both h₂
      ⟨fun hτ => ih₂.1 (ih₁.1 hτ), fun hσ => ih₁.2 (ih₂.2 hσ)⟩
  | _, _, .fn _ _  => ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩
  | _, _, .rcd _   => ⟨(fun hτ => nomatch hτ), (fun hσ => nomatch hσ)⟩

-- The usable corollaries:
theorem TyEquiv.fn_inv {B : Type} {τ₁ τ₂ : Ty B} {σ : Ty B}
    (h : TyEquiv (.fn τ₁ τ₂) σ) :
    ∃ σ₁ σ₂, σ = .fn σ₁ σ₂ ∧ TyEquiv τ₁ σ₁ ∧ TyEquiv τ₂ σ₂ :=
  (TyEquiv.fn_inv_both h).1 rfl

theorem TyEquiv.rcd_inv {B : Type} {ρ : Row B} {σ : Ty B}
    (h : TyEquiv (.rcd ρ) σ) : ∃ ρ', σ = .rcd ρ' ∧ RowEquiv ρ ρ' :=
  (TyEquiv.rcd_inv_both h).1 rfl

theorem TyEquiv.base_inv {B : Type} {b : B} {σ : Ty B}
    (h : TyEquiv (.base b) σ) : σ = .base b :=
  (TyEquiv.base_inv_both h).1 rfl

theorem TyEquiv.unk_inv {B : Type} {σ : Ty B}
    (h : TyEquiv (.unk : Ty B) σ) : σ = .unk :=
  (TyEquiv.unk_inv_both h).1 rfl


------------------------------------ CONTEXT -----------------------------------

structure Ctx (B : Type) where
  tyEnv  : List (Var × Scheme B) -- x: σ  (λ binds monotypes, let binds schemes)
  rowEnv : List (TyVar × Row B)  -- α = ρ  (solved row-vars, consulted by L-α)

namespace Ctx

def empty : Ctx B := ⟨[], []⟩

def lookup (Γ : Ctx B) (x : Var) : Option (Scheme B) :=
  (Γ.tyEnv.find? (·.1 == x)).map (·.2)

def lookupRow (Γ : Ctx B) (α : TyVar) : Option (Row B) :=
  (Γ.rowEnv.find? (·.1 == α)).map (·.2)

-- Γ · (x: σ)
def bindScheme (Γ : Ctx B) (x : Var) (σ : Scheme B) : Ctx B :=
  { Γ with tyEnv := (x, σ) :: Γ.tyEnv }

-- Γ · (x: τ)   (monotype binding, used by T-λ-I)
def bindTy (Γ : Ctx B) (x : Var) (τ : Ty B) : Ctx B :=
  Γ.bindScheme x ⟨[], τ⟩

-- Γ · (α = ρ)
def bindRow (Γ : Ctx B) (α : TyVar) (ρ : Row B) : Ctx B :=
  { Γ with rowEnv := (α, ρ) :: Γ.rowEnv }

-- Γ ⊑ Γ' on row-vars: Γ' keeps every solution of Γ
def RowExt (Γ Γ' : Ctx B) : Prop :=
  ∀ α ρ, Γ.lookupRow α = some ρ → Γ'.lookupRow α = some ρ

end Ctx


---------------------------------- ROW LOOKUP -----------------------------------
--   Γ ⊢ ρ.l ↓ r   with r := τ | ⊥ | ?
--
-- Leftmost occurrence wins; an unknown row in higher-priority position
-- poisons the result, since it could shadow a hit further right.

inductive LookupRes (B : Type) : Type where
  | found   : Ty B → LookupRes B   -- τ
  | absent  : LookupRes B          -- ⊥
  | unknown : LookupRes B          -- ?

inductive Lookup {B : Type} (Γ : Ctx B) : Row B → Label → LookupRes B → Prop where
  -- L-ε
  | emp : Lookup Γ .empty l .absent
  -- L-hit
  | hit : Lookup Γ (.sing l τ) l (.found τ)
  -- L-miss
  | miss : l₁ ≠ l₂ → Lookup Γ (.sing l₁ τ) l₂ .absent
  -- L-α
  | var : Γ.lookupRow α = some ρ → Lookup Γ ρ l r → Lookup Γ (.var α) l r
  -- L-α-free
  | varFree : Γ.lookupRow α = none → Lookup Γ (.var α) l .unknown
  -- L-conc-hit
  | catHit : Lookup Γ ρ₁ l (.found τ) → Lookup Γ (.cat ρ₁ ρ₂) l (.found τ)
  -- L-conc-skip
  | catSkip : Lookup Γ ρ₁ l .absent → Lookup Γ ρ₂ l r → Lookup Γ (.cat ρ₁ ρ₂) l r
  -- L-conc-★
  | catUnk : Lookup Γ ρ₁ l .unknown → Lookup Γ (.cat ρ₁ ρ₂) l .unknown


------------------------------ LOOKUP METATHEORY --------------------------------

-- Lookup is deterministic: every row shape matches exactly one rule.
theorem lookup_det {B : Type} {Γ : Ctx B} {ρ : Row B} {l : Label}
    {r₁ r₂ : LookupRes B}
    (h₁ : Lookup Γ ρ l r₁) (h₂ : Lookup Γ ρ l r₂) : r₁ = r₂ := by
  induction h₁ generalizing r₂ with
  | emp => cases h₂; rfl
  | hit =>
      cases h₂ with
      | hit    => rfl
      | miss h => exact absurd rfl h
  | miss hne =>
      cases h₂ with
      | hit    => exact absurd rfl hne
      | miss _ => rfl
  | var hΓ _ ih =>
      cases h₂ with
      | var hΓ' h' =>
          rw [hΓ] at hΓ'
          injection hΓ' with heq
          exact ih (heq ▸ h')
      | varFree hΓ' => rw [hΓ] at hΓ'; cases hΓ'
  | varFree hΓ =>
      cases h₂ with
      | var hΓ' _  => rw [hΓ] at hΓ'; cases hΓ'
      | varFree _  => rfl
  | catHit _ ih =>
      cases h₂ with
      | catHit h'     => exact ih h'
      | catSkip h' _  => cases ih h'
      | catUnk h'     => cases ih h'
  | catSkip _ _ ih₁ ih₂ =>
      cases h₂ with
      | catHit h'     => cases ih₁ h'
      | catSkip _ h'  => exact ih₂ h'
      | catUnk h'     => cases ih₁ h'
  | catUnk _ ih =>
      cases h₂ with
      | catHit h'    => cases ih h'
      | catSkip h' _ => cases ih h'
      | catUnk _     => rfl

-- Monotonicity: definite results (τ/⊥) survive extending the row-solutions.
-- Only ? can improve under a larger context. This is the lemma that makes
-- deferring lookups (stumps) sound.
theorem lookup_mono {B : Type} {Γ Γ' : Ctx B} {ρ : Row B} {l : Label}
    {r : LookupRes B}
    (hext : Ctx.RowExt Γ Γ') (h : Lookup Γ ρ l r) (hr : r ≠ .unknown) :
    Lookup Γ' ρ l r := by
  induction h with
  | emp        => exact .emp
  | hit        => exact .hit
  | miss hne   => exact .miss hne
  | var hΓ _ ih   => exact .var (hext _ _ hΓ) (ih hr)
  | varFree _     => exact absurd rfl hr
  | catHit _ ih   => exact .catHit (ih (by intro h; cases h))
  | catSkip _ _ ih₁ ih₂ =>
      exact .catSkip (ih₁ (by intro h; cases h)) (ih₂ hr)
  | catUnk _ _    => exact absurd rfl hr

-- Totality. NOT provable by bare structural induction — L-α recurses through
-- Γ's row-solutions, which could be cyclic (α = α | ρ loops forever). The
-- acyclicity condition: some ranking of row-vars strictly drops when stepping
-- into a solution. The telescope discipline (each solution mentions only vars
-- solved strictly later in rowEnv) yields such a ranking; so does an empty or
-- fully-ground rowEnv (rank ≡ 0 vacuously).

-- Largest rank of a row-var in the spine (field types don't matter: lookup
-- never descends into them).
def Row.rankUnder {B : Type} (rank : TyVar → Nat) : Row B → Nat
  | .empty     => 0
  | .var α     => rank α
  | .sing _ _  => 0
  | .cat ρ₁ ρ₂ => max (ρ₁.rankUnder rank) (ρ₂.rankUnder rank)

def Ctx.RowWF {B : Type} (Γ : Ctx B) : Prop :=
  ∃ rank : TyVar → Nat,
    ∀ α ρ, Γ.lookupRow α = some ρ → ρ.rankUnder rank < rank α

-- Recursion on (spine rank, row size) lexicographically: stepping into a
-- solution drops the rank (RowWF), stepping into a concatenand drops the size.
private theorem lookup_total_go {B : Type} {Γ : Ctx B} (rank : TyVar → Nat)
    (hrank : ∀ α ρ, Γ.lookupRow α = some ρ → ρ.rankUnder rank < rank α)
    (ρ : Row B) (l : Label) : ∃ r, Lookup Γ ρ l r :=
  match ρ with
  | .empty => ⟨_, .emp⟩
  | .sing l' _ =>
      if hl : l' = l then by subst hl; exact ⟨_, .hit⟩
      else ⟨_, .miss hl⟩
  | .cat ρ₁ ρ₂ =>
      match lookup_total_go rank hrank ρ₁ l with
      | ⟨.found _, hr₁⟩ => ⟨_, .catHit hr₁⟩
      | ⟨.absent, hr₁⟩ =>
          match lookup_total_go rank hrank ρ₂ l with
          | ⟨_, hr₂⟩ => ⟨_, .catSkip hr₁ hr₂⟩
      | ⟨.unknown, hr₁⟩ => ⟨_, .catUnk hr₁⟩
  | .var α =>
      match hα : Γ.lookupRow α with
      | none => ⟨_, .varFree hα⟩
      | some ρ' =>
          match lookup_total_go rank hrank ρ' l with
          | ⟨_, hr⟩ => ⟨_, .var hα hr⟩
termination_by (ρ.rankUnder rank, sizeOf ρ)
decreasing_by
  · simp only [Prod.lex_def, Row.rankUnder]; simp; omega
  · simp only [Prod.lex_def, Row.rankUnder]; simp; omega
  · have := hrank _ _ hα
    simp only [Prod.lex_def, Row.rankUnder]
    omega

theorem lookup_total {B : Type} {Γ : Ctx B} (hwf : Γ.RowWF) (ρ : Row B)
    (l : Label) : ∃ r, Lookup Γ ρ l r :=
  match hwf with
  | ⟨rank, hrank⟩ => lookup_total_go rank hrank ρ l

-- Result equivalence: found-types match up to ≈, ⊥/? on the nose.
inductive ResEquiv {B : Type} : LookupRes B → LookupRes B → Prop where
  | found   : TyEquiv τ₁ τ₂ → ResEquiv (.found τ₁) (.found τ₂)
  | absent  : ResEquiv .absent .absent
  | unknown : ResEquiv .unknown .unknown

theorem ResEquiv.refl {B : Type} : (r : LookupRes B) → ResEquiv r r
  | .found τ => .found (.refl τ)
  | .absent  => .absent
  | .unknown => .unknown

theorem ResEquiv.symm {B : Type} {r₁ r₂ : LookupRes B} :
    ResEquiv r₁ r₂ → ResEquiv r₂ r₁
  | .found h => .found h.symm
  | .absent  => .absent
  | .unknown => .unknown

theorem ResEquiv.trans {B : Type} {r₁ r₂ r₃ : LookupRes B} :
    ResEquiv r₁ r₂ → ResEquiv r₂ r₃ → ResEquiv r₁ r₃
  | .found h₁, .found h₂ => .found (h₁.trans h₂)
  | .absent,   .absent   => .absent
  | .unknown,  .unknown  => .unknown

-- The ≈-axioms preserve lookup results *on the nose* (only `sing` changes the
-- found type, handled separately). One helper per axiom, both directions.
section EquivHelpers
variable {B : Type} {Γ : Ctx B} {ρ ρ₁ ρ₂ ρ₃ : Row B} {l : Label} {r : LookupRes B}

private theorem lookup_assoc_fwd :
    Lookup Γ (.cat (.cat ρ₁ ρ₂) ρ₃) l r → Lookup Γ (.cat ρ₁ (.cat ρ₂ ρ₃)) l r := by
  intro h
  cases h with
  | catHit h₁₂ =>
      cases h₁₂ with
      | catHit h₁     => exact .catHit h₁
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catHit h₂)
  | catSkip h₁₂ h₃ =>
      cases h₁₂ with
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catSkip h₂ h₃)
  | catUnk h₁₂ =>
      cases h₁₂ with
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catUnk h₂)
      | catUnk h₁     => exact .catUnk h₁

private theorem lookup_assoc_bwd :
    Lookup Γ (.cat ρ₁ (.cat ρ₂ ρ₃)) l r → Lookup Γ (.cat (.cat ρ₁ ρ₂) ρ₃) l r := by
  intro h
  cases h with
  | catHit h₁ => exact .catHit (.catHit h₁)
  | catSkip h₁ h₂₃ =>
      cases h₂₃ with
      | catHit h₂     => exact .catHit (.catSkip h₁ h₂)
      | catSkip h₂ h₃ => exact .catSkip (.catSkip h₁ h₂) h₃
      | catUnk h₂     => exact .catUnk (.catSkip h₁ h₂)
  | catUnk h₁ => exact .catUnk (.catUnk h₁)

private theorem lookup_unitL_fwd :
    Lookup Γ (.cat .empty ρ) l r → Lookup Γ ρ l r := by
  intro h
  cases h with
  | catHit hε     => cases hε
  | catSkip _ h₂  => exact h₂
  | catUnk hε     => cases hε

private theorem lookup_unitL_bwd (h : Lookup Γ ρ l r) :
    Lookup Γ (.cat .empty ρ) l r :=
  .catSkip .emp h

private theorem lookup_unitR_fwd :
    Lookup Γ (.cat ρ .empty) l r → Lookup Γ ρ l r := by
  intro h
  cases h with
  | catHit h₁     => exact h₁
  | catSkip h₁ hε => cases hε; exact h₁
  | catUnk h₁     => exact h₁

private theorem lookup_unitR_bwd (h : Lookup Γ ρ l r) :
    Lookup Γ (.cat ρ .empty) l r := by
  cases r with
  | found τ => exact .catHit h
  | absent  => exact .catSkip h .emp
  | unknown => exact .catUnk h

private theorem lookup_comm_fwd {l₁ l₂ : Label} {τ₁ τ₂ : Ty B} (hne : l₁ ≠ l₂) :
    Lookup Γ (.cat (.sing l₁ τ₁) (.sing l₂ τ₂)) l r →
    Lookup Γ (.cat (.sing l₂ τ₂) (.sing l₁ τ₁)) l r := by
  intro h
  cases h with
  | catHit h₁ =>
      cases h₁
      exact .catSkip (.miss (Ne.symm hne)) .hit
  | catSkip h₁ h₂ =>
      cases h₁ with
      | miss hne₁ =>
          cases h₂ with
          | hit       => exact .catHit .hit
          | miss hne₂ => exact .catSkip (.miss hne₂) (.miss hne₁)
  | catUnk h₁ => cases h₁

end EquivHelpers

-- Lookup respects row equivalence, in both directions at once (≈ is symmetric,
-- so a one-directional induction hypothesis would be too weak).
theorem lookup_equiv_both {B : Type} {Γ : Ctx B} :
    {ρ₁ ρ₂ : Row B} → RowEquiv ρ₁ ρ₂ →
    (∀ {l r}, Lookup Γ ρ₁ l r → ∃ r', Lookup Γ ρ₂ l r' ∧ ResEquiv r r') ∧
    (∀ {l r}, Lookup Γ ρ₂ l r → ∃ r', Lookup Γ ρ₁ l r' ∧ ResEquiv r r')
  | _, _, .refl _ =>
      ⟨fun h => ⟨_, h, .refl _⟩, fun h => ⟨_, h, .refl _⟩⟩
  | _, _, .symm h => (lookup_equiv_both h).symm
  | _, _, .trans h₁ h₂ =>
      have ih₁ := lookup_equiv_both h₁
      have ih₂ := lookup_equiv_both h₂
      ⟨fun hl =>
        match ih₁.1 hl with
        | ⟨_, hm, e₁⟩ => match ih₂.1 hm with
          | ⟨_, hr, e₂⟩ => ⟨_, hr, e₁.trans e₂⟩,
       fun hl =>
        match ih₂.2 hl with
        | ⟨_, hm, e₁⟩ => match ih₁.2 hm with
          | ⟨_, hr, e₂⟩ => ⟨_, hr, e₁.trans e₂⟩⟩
  | _, _, .sing hty =>
      ⟨fun hl => match hl with
        | .hit      => ⟨_, .hit, .found hty⟩
        | .miss hne => ⟨_, .miss hne, .absent⟩,
       fun hl => match hl with
        | .hit      => ⟨_, .hit, .found hty.symm⟩
        | .miss hne => ⟨_, .miss hne, .absent⟩⟩
  | _, _, .cat h₁ h₂ =>
      have ih₁ := lookup_equiv_both h₁
      have ih₂ := lookup_equiv_both h₂
      ⟨fun hl => match hl with
        | .catHit hf => match ih₁.1 hf with
          | ⟨_, h', .found te⟩ => ⟨_, .catHit h', .found te⟩
        | .catSkip ha hr => match ih₁.1 ha with
          | ⟨_, h', .absent⟩ => match ih₂.1 hr with
            | ⟨_, h'', e⟩ => ⟨_, .catSkip h' h'', e⟩
        | .catUnk hu => match ih₁.1 hu with
          | ⟨_, h', .unknown⟩ => ⟨_, .catUnk h', .unknown⟩,
       fun hl => match hl with
        | .catHit hf => match ih₁.2 hf with
          | ⟨_, h', .found te⟩ => ⟨_, .catHit h', .found te⟩
        | .catSkip ha hr => match ih₁.2 ha with
          | ⟨_, h', .absent⟩ => match ih₂.2 hr with
            | ⟨_, h'', e⟩ => ⟨_, .catSkip h' h'', e⟩
        | .catUnk hu => match ih₁.2 hu with
          | ⟨_, h', .unknown⟩ => ⟨_, .catUnk h', .unknown⟩⟩
  | _, _, .assoc =>
      ⟨fun hl => ⟨_, lookup_assoc_fwd hl, .refl _⟩,
       fun hl => ⟨_, lookup_assoc_bwd hl, .refl _⟩⟩
  | _, _, .unitL =>
      ⟨fun hl => ⟨_, lookup_unitL_fwd hl, .refl _⟩,
       fun hl => ⟨_, lookup_unitL_bwd hl, .refl _⟩⟩
  | _, _, .unitR =>
      ⟨fun hl => ⟨_, lookup_unitR_fwd hl, .refl _⟩,
       fun hl => ⟨_, lookup_unitR_bwd hl, .refl _⟩⟩
  | _, _, .comm hne =>
      ⟨fun hl => ⟨_, lookup_comm_fwd hne hl, .refl _⟩,
       fun hl => ⟨_, lookup_comm_fwd (Ne.symm hne) hl, .refl _⟩⟩

-- Lookup respects row equivalence. Calibration check for ≈: any stronger
-- (swapping equal labels) and this lemma breaks, any weaker and T-eq is useless.
theorem lookup_equiv {B : Type} {Γ : Ctx B} {ρ₁ ρ₂ : Row B} {l : Label}
    {r₁ : LookupRes B}
    (heq : RowEquiv ρ₁ ρ₂) (h : Lookup Γ ρ₁ l r₁) :
    ∃ r₂, Lookup Γ ρ₂ l r₂ ∧ ResEquiv r₁ r₂ :=
  (lookup_equiv_both heq).1 h


----------------------------- SPINE-VAR-FREE ROWS ------------------------------
-- Rows of record literals never carry a row-var in their spine (field *types*
-- may — lookup never descends into them). On such rows lookup is decisive:
-- ? is impossible. This keeps T-sel-★ honest for closed programs.

inductive Row.SpineVarFree {B : Type} : Row B → Prop where
  | empty : SpineVarFree .empty
  | sing  : SpineVarFree (.sing l τ)
  | cat   : SpineVarFree ρ₁ → SpineVarFree ρ₂ → SpineVarFree (.cat ρ₁ ρ₂)

-- ≈ preserves the spine: no rule creates or removes a spine variable.
theorem RowEquiv.spineVarFree {B : Type} :
    {ρ₁ ρ₂ : Row B} → RowEquiv ρ₁ ρ₂ →
    (ρ₁.SpineVarFree → ρ₂.SpineVarFree) ∧ (ρ₂.SpineVarFree → ρ₁.SpineVarFree)
  | _, _, .refl _ => ⟨id, id⟩
  | _, _, .symm h => (RowEquiv.spineVarFree h).symm
  | _, _, .trans h₁ h₂ =>
      have ih₁ := RowEquiv.spineVarFree h₁
      have ih₂ := RowEquiv.spineVarFree h₂
      ⟨fun hv => ih₂.1 (ih₁.1 hv), fun hv => ih₁.2 (ih₂.2 hv)⟩
  | _, _, .sing _ => ⟨(fun _ => .sing), (fun _ => .sing)⟩
  | _, _, .cat h₁ h₂ =>
      have ih₁ := RowEquiv.spineVarFree h₁
      have ih₂ := RowEquiv.spineVarFree h₂
      ⟨(fun hv => match hv with | .cat v₁ v₂ => .cat (ih₁.1 v₁) (ih₂.1 v₂)),
       (fun hv => match hv with | .cat v₁ v₂ => .cat (ih₁.2 v₁) (ih₂.2 v₂))⟩
  | _, _, .assoc =>
      ⟨(fun hv => match hv with | .cat (.cat v₁ v₂) v₃ => .cat v₁ (.cat v₂ v₃)),
       (fun hv => match hv with | .cat v₁ (.cat v₂ v₃) => .cat (.cat v₁ v₂) v₃)⟩
  | _, _, .unitL =>
      ⟨(fun hv => match hv with | .cat _ v => v), (fun hv => .cat .empty hv)⟩
  | _, _, .unitR =>
      ⟨(fun hv => match hv with | .cat v _ => v), (fun hv => .cat hv .empty)⟩
  | _, _, .comm _ =>
      ⟨(fun _ => .cat .sing .sing), (fun _ => .cat .sing .sing)⟩

theorem Lookup.not_unknown_of_spineVarFree {B : Type} {Γ : Ctx B} {ρ : Row B}
    {l : Label} (hv : ρ.SpineVarFree) (h : Lookup Γ ρ l .unknown) : False := by
  induction hv with
  | empty => cases h
  | sing  => cases h
  | cat _ _ ih₁ ih₂ =>
      cases h with
      | catSkip _ h₂ => exact ih₂ h₂
      | catUnk h₁    => exact ih₁ h₁


------------------------------ TYPE SUBSTITUTION -------------------------------
-- θ = (θt, θr): a pair of *total* maps (identity outside the finite domain of
-- interest). Ty-positions consult θt, Row-positions θr — no kind system
-- needed; a variable used at both sorts just gets each map at each position.
-- This is the engine of instantiation: σ ≥ τ picks a θ over the quantified
-- variables.

structure TySubst (B : Type) where
  ty  : TyVar → Ty B
  row : TyVar → Row B

mutual
  def Ty.applySubst {B : Type} (θ : TySubst B) : Ty B → Ty B
    | .var α    => θ.ty α
    | .base b   => .base b
    | .unk      => .unk
    | .fn τ₁ τ₂ => .fn (τ₁.applySubst θ) (τ₂.applySubst θ)
    | .rcd ρ    => .rcd (ρ.applySubst θ)

  def Row.applySubst {B : Type} (θ : TySubst B) : Row B → Row B
    | .empty     => .empty
    | .var α     => θ.row α
    | .sing l τ  => .sing l (τ.applySubst θ)
    | .cat ρ₁ ρ₂ => .cat (ρ₁.applySubst θ) (ρ₂.applySubst θ)
end

def TySubst.id (B : Type) : TySubst B := ⟨(.var ·), (.var ·)⟩

mutual
  theorem Ty.applySubst_id {B : Type} :
      (τ : Ty B) → τ.applySubst (TySubst.id B) = τ
    | .var _    => rfl
    | .base _   => rfl
    | .unk      => rfl
    | .fn τ₁ τ₂ => by
        simp only [Ty.applySubst, Ty.applySubst_id τ₁, Ty.applySubst_id τ₂]
    | .rcd ρ    => by simp only [Ty.applySubst, Row.applySubst_id ρ]

  theorem Row.applySubst_id {B : Type} :
      (ρ : Row B) → ρ.applySubst (TySubst.id B) = ρ
    | .empty     => rfl
    | .var _     => rfl
    | .sing _ τ  => by simp only [Row.applySubst, Ty.applySubst_id τ]
    | .cat ρ₁ ρ₂ => by
        simp only [Row.applySubst, Row.applySubst_id ρ₁, Row.applySubst_id ρ₂]
end

-- Free type/row variables, spine and field positions alike.
mutual
  def Ty.ftv {B : Type} : Ty B → List TyVar
    | .var α    => [α]
    | .base _   => []
    | .unk      => []
    | .fn τ₁ τ₂ => τ₁.ftv ++ τ₂.ftv
    | .rcd ρ    => ρ.ftv

  def Row.ftv {B : Type} : Row B → List TyVar
    | .empty     => []
    | .var α     => [α]
    | .sing _ τ  => τ.ftv
    | .cat ρ₁ ρ₂ => ρ₁.ftv ++ ρ₂.ftv
end

-- ≈ is a congruence for substitution: every axiom is label-driven and
-- substitution never touches labels (comm's l₁ ≠ l₂ survives untouched).
mutual
  theorem TyEquiv.applySubst {B : Type} (θ : TySubst B) :
      {τ₁ τ₂ : Ty B} → TyEquiv τ₁ τ₂ →
      TyEquiv (τ₁.applySubst θ) (τ₂.applySubst θ)
    | _, _, .refl _      => .refl _
    | _, _, .symm h      => (TyEquiv.applySubst θ h).symm
    | _, _, .trans h₁ h₂ =>
        (TyEquiv.applySubst θ h₁).trans (TyEquiv.applySubst θ h₂)
    | _, _, .fn h₁ h₂    => by
        simp only [Ty.applySubst]
        exact .fn (TyEquiv.applySubst θ h₁) (TyEquiv.applySubst θ h₂)
    | _, _, .rcd h       => by
        simp only [Ty.applySubst]
        exact .rcd (RowEquiv.applySubst θ h)

  theorem RowEquiv.applySubst {B : Type} (θ : TySubst B) :
      {ρ₁ ρ₂ : Row B} → RowEquiv ρ₁ ρ₂ →
      RowEquiv (ρ₁.applySubst θ) (ρ₂.applySubst θ)
    | _, _, .refl _      => .refl _
    | _, _, .symm h      => (RowEquiv.applySubst θ h).symm
    | _, _, .trans h₁ h₂ =>
        (RowEquiv.applySubst θ h₁).trans (RowEquiv.applySubst θ h₂)
    | _, _, .sing hty    => by
        simp only [Row.applySubst]
        exact .sing (TyEquiv.applySubst θ hty)
    | _, _, .cat h₁ h₂   => by
        simp only [Row.applySubst]
        exact .cat (RowEquiv.applySubst θ h₁) (RowEquiv.applySubst θ h₂)
    | _, _, .assoc       => by simp only [Row.applySubst]; exact .assoc
    | _, _, .unitL       => by simp only [Row.applySubst]; exact .unitL
    | _, _, .unitR       => by simp only [Row.applySubst]; exact .unitR
    | _, _, .comm hne    => by simp only [Row.applySubst]; exact .comm hne
end

-- Substitution acts on lookup results pointwise (definite ones, see below).
def LookupRes.applySubst {B : Type} (θ : TySubst B) : LookupRes B → LookupRes B
  | .found τ => .found (τ.applySubst θ)
  | .absent  => .absent
  | .unknown => .unknown

-- Definite lookups are stable under substitution — the *syntactic* analog of
-- lookup_mono: at an empty rowEnv a definite derivation never consults the
-- context (L-α needs a solution, L-α-free yields ?), so it transports into
-- any context after substituting the row. Only ? can change category — that
-- demotion is exactly what T-sel-⊥ / T-★-intro absorb.
theorem lookup_applySubst {B : Type} {Γ Γ' : Ctx B} {ρ : Row B} {l : Label}
    {r : LookupRes B} (hrow : Γ.rowEnv = []) (θ : TySubst B)
    (h : Lookup Γ ρ l r) (hr : r ≠ .unknown) :
    Lookup Γ' (ρ.applySubst θ) l (r.applySubst θ) := by
  induction h with
  | emp        => exact .emp
  | hit        => exact .hit
  | miss hne   => exact .miss hne
  | var hΓ _ _ =>
      rw [show Γ.lookupRow _ = none by simp [Ctx.lookupRow, hrow]] at hΓ
      cases hΓ
  | varFree _  => exact absurd rfl hr
  | catHit _ ih =>
      simp only [Row.applySubst]
      exact .catHit (ih (by intro h; cases h))
  | catSkip _ _ ih₁ ih₂ =>
      simp only [Row.applySubst]
      exact .catSkip (ih₁ (by intro h; cases h)) (ih₂ hr)
  | catUnk _ _ => exact absurd rfl hr

-- ## Instantiation  σ ≥ τ
-- θ acts only on the quantified variables; everything else stays put.

def TySubst.FixedOutside {B : Type} (θ : TySubst B) (ᾱ : List TyVar) : Prop :=
  (∀ α, α ∉ ᾱ → θ.ty α = .var α) ∧ (∀ α, α ∉ ᾱ → θ.row α = .var α)

def Scheme.Inst {B : Type} (σ : Scheme B) (τ : Ty B) : Prop :=
  ∃ θ : TySubst B, θ.FixedOutside σ.vars ∧ σ.body.applySubst θ = τ

-- A pointwise-identity substitution acts as the identity.
mutual
  theorem Ty.applySubst_fixed {B : Type} {θ : TySubst B}
      (ht : ∀ α, θ.ty α = .var α) (hr : ∀ α, θ.row α = .var α) :
      (τ : Ty B) → τ.applySubst θ = τ
    | .var α    => ht α
    | .base _   => rfl
    | .unk      => rfl
    | .fn τ₁ τ₂ => by
        simp only [Ty.applySubst, Ty.applySubst_fixed ht hr τ₁,
                   Ty.applySubst_fixed ht hr τ₂]
    | .rcd ρ    => by simp only [Ty.applySubst, Row.applySubst_fixed ht hr ρ]

  theorem Row.applySubst_fixed {B : Type} {θ : TySubst B}
      (ht : ∀ α, θ.ty α = .var α) (hr : ∀ α, θ.row α = .var α) :
      (ρ : Row B) → ρ.applySubst θ = ρ
    | .empty     => rfl
    | .var α     => hr α
    | .sing _ τ  => by simp only [Row.applySubst, Ty.applySubst_fixed ht hr τ]
    | .cat ρ₁ ρ₂ => by
        simp only [Row.applySubst, Row.applySubst_fixed ht hr ρ₁,
                   Row.applySubst_fixed ht hr ρ₂]
end

-- Monotype schemes instantiate only to themselves (I-refl is the whole story).
theorem Scheme.Inst.mono {B : Type} {τ₁ τ : Ty B}
    (h : Scheme.Inst ⟨[], τ₁⟩ τ) : τ = τ₁ := by
  obtain ⟨θ, ⟨ht, hr⟩, hτ⟩ := h
  rw [← hτ]
  exact Ty.applySubst_fixed
    (fun α => ht α (List.not_mem_nil))
    (fun α => hr α (List.not_mem_nil)) τ₁

theorem Scheme.Inst.refl {B : Type} (τ : Ty B) : Scheme.Inst ⟨[], τ⟩ τ :=
  ⟨TySubst.id B, ⟨fun _ _ => rfl, fun _ _ => rfl⟩, Ty.applySubst_id τ⟩

-- Every scheme has at least its own body as an instance (θ = id): schemes are
-- never vacuous, which is what lets progress extract *some* typing for a
-- let-bound expression.
theorem Scheme.Inst.self {B : Type} (σ : Scheme B) : σ.Inst σ.body :=
  ⟨TySubst.id B, ⟨fun _ _ => rfl, fun _ _ => rfl⟩, Ty.applySubst_id σ.body⟩


------------------------------- TYPING RELATION --------------------------------
--   Γ ⊢ e : τ
--   `constTy : C → B` assigns each constant its base type.

mutual
  inductive Typed {B C : Type} (constTy : C → B) :
      Ctx B → Expr C → Ty B → Prop where

    -- ----------- T-cons
    -- Γ ⊢ c : 𝓫_c
    | tCon : Typed constTy Γ (.con c) (.base (constTy c))

    -- x : σ ∈ Γ   σ ≥ τ
    -- ------------------- T-var
    -- Γ ⊢ x : τ
    | tVar : Γ.lookup x = some σ → σ.Inst τ → Typed constTy Γ (.var x) τ

    -- Γ ⊢ e : τ₁   τ₁ ≈ τ₂
    -- ---------------------- T-eq
    -- Γ ⊢ e : τ₂
    | tEq : Typed constTy Γ e τ₁ → TyEquiv τ₁ τ₂ → Typed constTy Γ e τ₂

    -- Γ · (x: τ₁) ⊢ e : τ₂
    -- --------------------- T-λ-I
    -- Γ ⊢ (x: e) : τ₁ → τ₂
    | tLam : Typed constTy (Γ.bindTy x τ₁) e τ₂ →
             Typed constTy Γ (.lam x e) (.fn τ₁ τ₂)

    -- Γ ⊢ e₁ : τ₁ → τ₂   Γ ⊢ e₂ : τ₁
    -- -------------------------------- T-λ-E
    -- Γ ⊢ e₁ e₂ : τ₂
    | tApp : Typed constTy Γ e₁ (.fn τ₁ τ₂) → Typed constTy Γ e₂ τ₁ →
             Typed constTy Γ (.app e₁ e₂) τ₂

    -- ∀ τ₁ ≤ σ.  Γ ⊢ e₁ : τ₁     Γ · (x: σ) ⊢ e₂ : τ₂
    -- -------------------------------------------------- T-let
    -- Γ ⊢ let x = e₁ in e₂ : τ₂
    -- *Instance-closed* formulation: e₁ must type at every instance of the
    -- scheme, so generalization is sound by construction — no ᾱ ∩ ftv(Γ) = ∅
    -- side condition, hence no variable-capture/renaming machinery anywhere.
    -- The premise is exactly what the polymorphic substitution lemma consumes
    -- at let-β. The standard syntactic rule (generalize ᾱ = ftv(τ₁) ∖ ftv(Γ))
    -- is admissible via a type-substitution lemma — future work; T-sel-⊥ and
    -- T-★-intro are what make this premise satisfiable when instantiation
    -- demotes/refines a ?-lookup.
    | tLet : (∀ τ₁, σ.Inst τ₁ → Typed constTy Γ e₁ τ₁) →
             Typed constTy (Γ.bindScheme x σ) e₂ τ₂ →
             Typed constTy Γ (.letE x e₁ e₂) τ₂

    -- Γ ⊢ e₁ : {ρ₁}   Γ ⊢ e₂ : {ρ₂}
    -- ------------------------------- T-conc
    -- Γ ⊢ e₁ ‖ e₂ : {ρ₂ | ρ₁}          (right-preference: e₂'s row in front)
    | tCat : Typed constTy Γ e₁ (.rcd ρ₁) → Typed constTy Γ e₂ (.rcd ρ₂) →
             Typed constTy Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))

    -- Γ ⊢ e : {ρ}   Γ ⊢ ρ.l ↓ τ
    -- --------------------------- T-sel
    -- Γ ⊢ e.l : τ
    | tSel : Typed constTy Γ e (.rcd ρ) → Lookup Γ ρ l (.found τ) →
             Typed constTy Γ (.sel e l) τ

    -- Γ ⊢ e : {ρ}   Γ ⊢ ρ.l ↓ ?
    -- --------------------------- T-sel-★
    -- Γ ⊢ e.l : ★
    | tSelUnk : Typed constTy Γ e (.rcd ρ) → Lookup Γ ρ l .unknown →
                Typed constTy Γ (.sel e l) .unk

    -- Γ ⊢ e : {ρ}   Γ ⊢ ρ.l ↓ ⊥
    -- --------------------------- T-sel-⊥
    -- Γ ⊢ e.l : ★
    -- Definitely errs at runtime (soft typing: flagged statically, caught by
    -- the ↯-disjunct of progress). Needed for preservation under let-poly:
    -- instantiation can demote a ?-lookup to ⊥, cf. let f = (x: x.l) in f {}.
    | tSelAbs : Typed constTy Γ e (.rcd ρ) → Lookup Γ ρ l .absent →
                Typed constTy Γ (.sel e l) .unk

    -- Γ ⊢ e : τ
    -- ----------- T-★-intro
    -- Γ ⊢ e : ★
    -- One-step upcast to ★, kept out of ≈ so head rigidity survives.
    -- Non-transitive by construction: nothing sits above ★ and ★ has no
    -- elimination rules. Needed for preservation under let-poly: instantiation
    -- can refine a ?-lookup to a definite τ that a frozen ★-domain expects.
    | tUnk : Typed constTy Γ e τ → Typed constTy Γ e .unk

    -- Γ ⊢ ξ : ρ
    -- ------------------ T-rec
    -- Γ ⊢ { ξ } : { ρ }
    | tRcd : TypedBody constTy Γ b ρ → Typed constTy Γ (.rcd b) (.rcd ρ)

  -- ## Typing relation for record bodies
  --
  --   Γ ⊢ ξ : ρ
  inductive TypedBody {B C : Type} (constTy : C → B) :
      Ctx B → RecBody (Expr C) → Row B → Prop where

    -- -------------------- T-ξ-empty
    -- Γ ⊢ ε : ε
    | empty : TypedBody constTy Γ .empty .empty

    -- Γ ⊢ e : τ
    -- ---------------------- T-ξ-field
    -- Γ ⊢ (l = e) : (l: τ)
    | field : Typed constTy Γ e τ →
              TypedBody constTy Γ (.field l e) (.sing l τ)

    -- Γ ⊢ ξ₁ : ρ₁   Γ ⊢ ξ₂ : ρ₂
    -- ---------------------------- T-ξ-conc
    -- Γ ⊢ (ξ₁ | ξ₂) : (ρ₁ | ρ₂)
    | cat : TypedBody constTy Γ b₁ ρ₁ → TypedBody constTy Γ b₂ ρ₂ →
            TypedBody constTy Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)
end


--------------------------- TYPING INVERSION (mod ≈) ---------------------------
-- T-eq can wrap any derivation, so syntax-directed inversion only holds up to
-- ≈ₜ: peel the tEq layers, collecting them with transitivity. T-★-intro can
-- also wrap any derivation, blurring the type to ★, so each conclusion gains
-- a `∨ τ = ★` escape hatch — kept *inside* the existentials so the underlying
-- typing information survives the upcast. A trailing tEq after tUnk stays at ★
-- by unk-rigidity (unk_inv).

-- Combined so the recursion (peeling tEq/tUnk) runs over variable indices
-- only, which is what Lean's structural recursion over inductive families
-- needs.
private theorem typed_inv_aux {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    (∀ {c : C}, e = .con c →
      TyEquiv (.base (constTy c)) τ ∨ τ = .unk) ∧
    (∀ {x : Var} {e' : Expr C}, e = .lam x e' →
      ∃ τ₁ τ₂, (TyEquiv (.fn τ₁ τ₂) τ ∨ τ = .unk) ∧
        Typed constTy (Γ.bindTy x τ₁) e' τ₂) ∧
    (∀ {b : RecBody (Expr C)}, e = .rcd b →
      ∃ ρ, (TyEquiv (.rcd ρ) τ ∨ τ = .unk) ∧ TypedBody constTy Γ b ρ)
  | _, _, _, .tCon =>
      ⟨(fun h => by cases h; exact .inl (.refl _)),
       (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tVar _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tEq h heq =>
      have ih := typed_inv_aux h
      ⟨(fun hc => match ih.1 hc with
         | .inl he => .inl (he.trans heq)
         | .inr hu => .inr (hu ▸ heq).unk_inv),
       (fun hl => match ih.2.1 hl with
         | ⟨_, _, .inl he, hb⟩ => ⟨_, _, .inl (he.trans heq), hb⟩
         | ⟨_, _, .inr hu, hb⟩ => ⟨_, _, .inr (hu ▸ heq).unk_inv, hb⟩),
       (fun hr => match ih.2.2 hr with
         | ⟨_, .inl he, hb⟩ => ⟨_, .inl (he.trans heq), hb⟩
         | ⟨_, .inr hu, hb⟩ => ⟨_, .inr (hu ▸ heq).unk_inv, hb⟩)⟩
  | _, _, _, .tUnk h =>
      have ih := typed_inv_aux h
      ⟨(fun _ => .inr rfl),
       (fun hl => match ih.2.1 hl with
         | ⟨_, _, _, hb⟩ => ⟨_, _, .inr rfl, hb⟩),
       (fun hr => match ih.2.2 hr with
         | ⟨_, _, hb⟩ => ⟨_, .inr rfl, hb⟩)⟩
  | _, _, _, .tLam h =>
      ⟨(fun hc => nomatch hc),
       (fun hl => by cases hl; exact ⟨_, _, .inl (.refl _), h⟩),
       (fun hr => nomatch hr)⟩
  | _, _, _, .tApp _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tCat _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tSel _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tSelUnk _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tSelAbs _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tLet _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tRcd h =>
      ⟨(fun hc => nomatch hc), (fun hl => nomatch hl),
       (fun hr => by cases hr; exact ⟨_, .inl (.refl _), h⟩)⟩

theorem typed_con_inv {B C : Type} {constTy : C → B} {Γ : Ctx B} {c : C}
    {τ : Ty B}
    (h : Typed constTy Γ (.con c) τ) :
    TyEquiv (.base (constTy c)) τ ∨ τ = .unk :=
  (typed_inv_aux h).1 rfl

theorem typed_lam_inv {B C : Type} {constTy : C → B} {Γ : Ctx B} {x : Var}
    {e : Expr C} {τ : Ty B}
    (h : Typed constTy Γ (.lam x e) τ) :
    ∃ τ₁ τ₂, (TyEquiv (.fn τ₁ τ₂) τ ∨ τ = .unk) ∧
      Typed constTy (Γ.bindTy x τ₁) e τ₂ :=
  (typed_inv_aux h).2.1 rfl

theorem typed_rcd_inv {B C : Type} {constTy : C → B} {Γ : Ctx B}
    {b : RecBody (Expr C)} {τ : Ty B}
    (h : Typed constTy Γ (.rcd b) τ) :
    ∃ ρ, (TyEquiv (.rcd ρ) τ ∨ τ = .unk) ∧ TypedBody constTy Γ b ρ :=
  (typed_inv_aux h).2.2 rfl


------------------------- TERM/TYPE LOOKUP AGREEMENT ---------------------------
-- On a typed record body, RecBody.lookup mirrors the Lookup relation: same
-- labels hit, same labels miss. (Rows of bodies are spine-var-free, so ? can't
-- occur — see TypedBody.spineVarFree.)

theorem TypedBody.spineVarFree {B C : Type} {constTy : C → B} {Γ : Ctx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → TypedBody constTy Γ b ρ →
    ρ.SpineVarFree
  | _, _, .empty     => .empty
  | _, _, .field _   => .sing
  | _, _, .cat h₁ h₂ => .cat (TypedBody.spineVarFree h₁)
                             (TypedBody.spineVarFree h₂)

theorem TypedBody.lookup_absent {B C : Type} {constTy : C → B} {Γ : Ctx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → TypedBody constTy Γ b ρ →
    ∀ {l : Label}, Lookup Γ ρ l .absent → RecBody.lookup l b = none
  | _, _, .empty => fun _ => rfl
  | _, _, .field _ => fun hl => by
      cases hl with
      | miss hne => simp [RecBody.lookup, Ne.symm hne]
  | _, _, .cat h₁ h₂ => fun hl => by
      cases hl with
      | catSkip ha hr =>
          simp [RecBody.lookup, TypedBody.lookup_absent h₁ ha,
                TypedBody.lookup_absent h₂ hr]

theorem TypedBody.lookup_found {B C : Type} {constTy : C → B} {Γ : Ctx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → TypedBody constTy Γ b ρ →
    ∀ {l : Label} {τ : Ty B}, Lookup Γ ρ l (.found τ) →
    ∃ e, RecBody.lookup l b = some e ∧ Typed constTy Γ e τ
  | _, _, .empty => fun hl => nomatch hl
  | _, _, .field ht => fun hl => by
      cases hl
      exact ⟨_, by simp [RecBody.lookup], ht⟩
  | _, _, .cat h₁ h₂ => fun hl => by
      cases hl with
      | catHit hf =>
          obtain ⟨e, hb, hte⟩ := TypedBody.lookup_found h₁ hf
          exact ⟨e, by simp [RecBody.lookup, hb], hte⟩
      | catSkip ha hr =>
          obtain ⟨e, hb, hte⟩ := TypedBody.lookup_found h₂ hr
          exact ⟨e, by simp [RecBody.lookup,
                             TypedBody.lookup_absent h₁ ha, hb], hte⟩


------------------------------ CONTEXT CONVERSION ------------------------------
-- Typing sees the context only through successful tyEnv lookups and the
-- *exact* rowEnv lookup function (L-α-free needs `none` preserved!), so it
-- transports along this preorder. Subsumes weakening, exchange and shadowing —
-- the named-binder plumbing the substitution lemma needs.

def Ctx.Sub {B : Type} (Γ₁ Γ₂ : Ctx B) : Prop :=
  (∀ x σ, Γ₁.lookup x = some σ → Γ₂.lookup x = some σ) ∧
  (∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α)

theorem Ctx.lookup_bindScheme {B : Type} (Γ : Ctx B) (x y : Var)
    (σ : Scheme B) :
    (Γ.bindScheme x σ).lookup y = if x == y then some σ else Γ.lookup y := by
  simp only [Ctx.lookup, Ctx.bindScheme, List.find?_cons]
  cases hxy : (x == y) <;> simp_all

theorem Ctx.lookup_bindTy {B : Type} (Γ : Ctx B) (x y : Var) (τ : Ty B) :
    (Γ.bindTy x τ).lookup y = if x == y then some ⟨[], τ⟩ else Γ.lookup y :=
  Ctx.lookup_bindScheme Γ x y ⟨[], τ⟩

theorem Ctx.Sub.refl {B : Type} (Γ : Ctx B) : Ctx.Sub Γ Γ :=
  ⟨fun _ _ h => h, fun _ => rfl⟩

theorem Ctx.Sub.trans {B : Type} {Γ₁ Γ₂ Γ₃ : Ctx B}
    (h₁ : Ctx.Sub Γ₁ Γ₂) (h₂ : Ctx.Sub Γ₂ Γ₃) : Ctx.Sub Γ₁ Γ₃ :=
  ⟨fun x τ h => h₂.1 x τ (h₁.1 x τ h), fun α => (h₁.2 α).trans (h₂.2 α)⟩

-- Binding respects the preorder.
theorem Ctx.Sub.bindScheme {B : Type} {Γ₁ Γ₂ : Ctx B} (h : Ctx.Sub Γ₁ Γ₂)
    (x : Var) (σ : Scheme B) :
    Ctx.Sub (Γ₁.bindScheme x σ) (Γ₂.bindScheme x σ) := by
  refine ⟨fun y σ' hy => ?_, h.2⟩
  rw [Ctx.lookup_bindScheme] at hy ⊢
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact h.1 y σ' hy
  · simpa [hxy] using hy

theorem Ctx.Sub.bindTy {B : Type} {Γ₁ Γ₂ : Ctx B} (h : Ctx.Sub Γ₁ Γ₂)
    (x : Var) (τ : Ty B) : Ctx.Sub (Γ₁.bindTy x τ) (Γ₂.bindTy x τ) :=
  h.bindScheme x ⟨[], τ⟩

-- Exchange: distinct bindings commute.
theorem Ctx.Sub.exchange {B : Type} (Γ : Ctx B) {x y : Var} (hne : x ≠ y)
    (σ₁ σ₂ : Scheme B) :
    Ctx.Sub ((Γ.bindScheme x σ₁).bindScheme y σ₂)
            ((Γ.bindScheme y σ₂).bindScheme x σ₁) := by
  refine ⟨fun z μ hz => ?_, fun _ => rfl⟩
  simp only [Ctx.lookup_bindScheme] at hz ⊢
  cases hyz : (y == z) <;> cases hxz : (x == z) <;>
    simp only [hyz, hxz, Bool.false_eq_true, if_false, if_true] at hz ⊢ <;>
    try exact hz
  exact absurd ((eq_of_beq hxz).trans (eq_of_beq hyz).symm) hne

-- Shadowing: rebinding x hides whatever σ₁ the lower context knew about x.
theorem Ctx.Sub.shadowed {B : Type} {Δ Γ : Ctx B} {x : Var} {σ₁ : Scheme B}
    (h : Ctx.Sub Δ (Γ.bindScheme x σ₁)) (σ : Scheme B) :
    Ctx.Sub (Δ.bindScheme x σ) (Γ.bindScheme x σ) := by
  refine ⟨fun z μ hz => ?_, fun α => h.2 α⟩
  rw [Ctx.lookup_bindScheme] at hz ⊢
  cases hxz : (x == z)
  · simp only [hxz, Bool.false_eq_true, if_false] at hz ⊢
    have := h.1 z μ hz
    rwa [Ctx.lookup_bindScheme, hxz, if_neg (by simp)] at this
  · simpa [hxz] using hz

-- A closed term types in any context over the same row-solutions.
theorem Ctx.Sub.ofEmptyTyEnv {B : Type} (Γ : Ctx B) :
    Ctx.Sub ⟨[], Γ.rowEnv⟩ Γ :=
  ⟨fun x τ h => by simp [Ctx.lookup] at h, fun _ => rfl⟩

-- Lookup consults only the row-solutions.
theorem Lookup.congr_rowEnv {B : Type} {Γ₁ Γ₂ : Ctx B}
    (hrow : ∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α) :
    {ρ : Row B} → {l : Label} → {r : LookupRes B} →
    Lookup Γ₁ ρ l r → Lookup Γ₂ ρ l r
  | _, _, _, .emp          => .emp
  | _, _, _, .hit          => .hit
  | _, _, _, .miss hne     => .miss hne
  | _, _, _, .var hΓ h     => .var (hrow _ ▸ hΓ) (Lookup.congr_rowEnv hrow h)
  | _, _, _, .varFree hΓ   => .varFree (hrow _ ▸ hΓ)
  | _, _, _, .catHit h     => .catHit (Lookup.congr_rowEnv hrow h)
  | _, _, _, .catSkip h₁ h₂ =>
      .catSkip (Lookup.congr_rowEnv hrow h₁) (Lookup.congr_rowEnv hrow h₂)
  | _, _, _, .catUnk h     => .catUnk (Lookup.congr_rowEnv hrow h)

mutual
theorem typed_sub {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : Ctx B} → {e : Expr C} → {τ : Ty B} → Ctx.Sub Γ₁ Γ₂ →
    Typed constTy Γ₁ e τ → Typed constTy Γ₂ e τ
  | _, _, _, _, _,  .tCon        => .tCon
  | _, _, _, _, hs, .tVar h hi   => .tVar (hs.1 _ _ h) hi
  | _, _, _, _, hs, .tEq h heq   => .tEq (typed_sub hs h) heq
  | _, _, _, _, hs, .tLam h      => .tLam (typed_sub (hs.bindTy _ _) h)
  | _, _, _, _, hs, .tApp h₁ h₂  => .tApp (typed_sub hs h₁) (typed_sub hs h₂)
  | _, _, _, _, hs, .tCat h₁ h₂  => .tCat (typed_sub hs h₁) (typed_sub hs h₂)
  | _, _, _, _, hs, .tSel h hl   =>
      .tSel (typed_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .tSelUnk h hl =>
      .tSelUnk (typed_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .tSelAbs h hl =>
      .tSelAbs (typed_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .tUnk h      => .tUnk (typed_sub hs h)
  | _, _, _, _, hs, .tLet h₁ h₂  =>
      .tLet (fun τ' hi => typed_sub hs (h₁ τ' hi))
            (typed_sub (hs.bindScheme _ _) h₂)
  | _, _, _, _, hs, .tRcd h      => .tRcd (typedBody_sub hs h)

theorem typedBody_sub {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : Ctx B} → {b : RecBody (Expr C)} → {ρ : Row B} → Ctx.Sub Γ₁ Γ₂ →
    TypedBody constTy Γ₁ b ρ → TypedBody constTy Γ₂ b ρ
  | _, _, _, _, _,  .empty     => .empty
  | _, _, _, _, hs, .field h   => .field (typed_sub hs h)
  | _, _, _, _, hs, .cat h₁ h₂ =>
      .cat (typedBody_sub hs h₁) (typedBody_sub hs h₂)
end


-- ## Substitution  e[x := v]

mutual
  def subst {C : Type} (x : Var) (v : Expr C) : Expr C → Expr C
    | .con c      => .con c
    | .var y      => if x == y then v else .var y
    | .lam y e    => if x == y then .lam y e else .lam y (subst x v e)
    | .app e₁ e₂  => .app (subst x v e₁) (subst x v e₂)
    | .cat e₁ e₂  => .cat (subst x v e₁) (subst x v e₂)
    | .sel e l    => .sel (subst x v e) l
    | .rcd b      => .rcd (substBody x v b)
    | .letE y e₁ e₂ =>
        .letE y (subst x v e₁) (if x == y then e₂ else subst x v e₂)

  def substBody {C : Type} (x : Var) (v : Expr C) : RecBody (Expr C) → RecBody (Expr C)
    | .empty      => .empty
    | .field l e  => .field l (subst x v e)
    | .cat b₁ b₂  => .cat (substBody x v b₁) (substBody x v b₂)
end


--------------------------------- VALUES --------------------------------------
-- Lazy semantics: a record is a value at the constructor level (WHNF).
-- Fields are unevaluated thunks and are forced only on projection.

inductive Value {C : Type} : Expr C → Prop where
  | con {c : C}                : Value (.con c)
  | lam {x : Var} {e : Expr C} : Value (.lam x e)
  | rcd {b : RecBody (Expr C)} : Value (.rcd b)


------------------------------- CANONICAL FORMS --------------------------------
-- Since ≈ₜ preserves head constructors, a value's shape is determined by the
-- head of its type even though tEq can rewrite it.

theorem canonical_fn {B C : Type} {constTy : C → B} {Γ : Ctx B} {v : Expr C}
    {τ₁ τ₂ : Ty B}
    (hv : Value v) (ht : Typed constTy Γ v (.fn τ₁ τ₂)) :
    ∃ x e, v = .lam x e := by
  cases hv with
  | con =>
      rcases typed_con_inv ht with he | hu
      · cases he.base_inv
      · cases hu
  | lam => exact ⟨_, _, rfl⟩
  | rcd =>
      obtain ⟨ρ, he | hu, -⟩ := typed_rcd_inv ht
      · obtain ⟨ρ', hσ, -⟩ := he.rcd_inv
        cases hσ
      · cases hu

theorem canonical_rcd {B C : Type} {constTy : C → B} {Γ : Ctx B} {v : Expr C}
    {ρ : Row B}
    (hv : Value v) (ht : Typed constTy Γ v (.rcd ρ)) :
    ∃ b ρ', v = .rcd b ∧ RowEquiv ρ' ρ ∧ TypedBody constTy Γ b ρ' := by
  cases hv with
  | con =>
      rcases typed_con_inv ht with he | hu
      · cases he.base_inv
      · cases hu
  | lam =>
      obtain ⟨τ₁, τ₂, he | hu, -⟩ := typed_lam_inv ht
      · obtain ⟨σ₁, σ₂, hσ, -⟩ := he.fn_inv
        cases hσ
      · cases hu
  | rcd =>
      obtain ⟨ρ', he | hu, hb⟩ := typed_rcd_inv ht
      · obtain ⟨ρ'', hσ, heq⟩ := he.rcd_inv
        cases hσ
        exact ⟨_, _, rfl, heq, hb⟩
      · cases hu


--------------------------------- REDUCTION ------------------------------------
-- e ↦ e'

inductive Step {C : Type} : Expr C → Expr C → Prop where
  | beta {x : Var} {e v : Expr C} :
      Value v →
      Step (.app (.lam x e) v) (subst x v e)
  | appFun {e₁ e₁' e₂ : Expr C} :
      Step e₁ e₁' →
      Step (.app e₁ e₂) (.app e₁' e₂)
  | appArg {e₁ e₂ e₂' : Expr C} :
      Value e₁ →
      Step e₂ e₂' →
      Step (.app e₁ e₂) (.app e₁ e₂')
  | selStep {e e' : Expr C} {l : Label} :
      Step e e' →
      Step (.sel e l) (.sel e' l)
  -- Projection yields the (unevaluated) field term; leftmost occurrence wins.
  | selVal {b : RecBody (Expr C)} {l : Label} {e : Expr C} :
      RecBody.lookup l b = some e →
      Step (.sel (.rcd b) l) e
  | catLeft {a a' b : Expr C} :
      Step a a' →
      Step (.cat a b) (.cat a' b)
  | catRight {a b b' : Expr C} :
      Value a →
      Step b b' →
      Step (.cat a b) (.cat a b')
  -- Right-preference: e₂'s fields go to the front (mirrors T-conc's {ρ₂ | ρ₁}).
  | catVal {b₁ b₂ : RecBody (Expr C)} :
      Step (.cat (.rcd b₁) (.rcd b₂)) (.rcd (.cat b₂ b₁))
  -- CBV let, mirroring beta: evaluate the binding, then substitute.
  | letCong {x : Var} {e₁ e₁' e₂ : Expr C} :
      Step e₁ e₁' →
      Step (.letE x e₁ e₂) (.letE x e₁' e₂)
  | letBeta {x : Var} {v e : Expr C} :
      Value v →
      Step (.letE x v e) (subst x v e)


---------------------------------- PROGRESS ---------------------------------
--   If ⊢ e : τ  then  e ∈ Value  ∨  (∃ e', e → e')  ∨  e ↯
--   Plain progress fails once ★-typed selections can reach records lacking
--   the label (in the full system, via instantiation). Err marks exactly
--   these lookup-errors and their propagation through evaluation contexts,
--   so the honest statement is "progress up to lookup-error" (see plan.typ).

-- e ↯ : a selection reached a record without the label, possibly under an
-- evaluation context (mirrors the congruence rules of Step).
inductive Err {C : Type} : Expr C → Prop where
  | selAbsent {b : RecBody (Expr C)} {l : Label} :
      RecBody.lookup l b = none → Err (.sel (.rcd b) l)
  | appFun   : Err e₁ → Err (.app e₁ e₂)
  | appArg   : Value e₁ → Err e₂ → Err (.app e₁ e₂)
  | sel      : Err e → Err (.sel e l)
  | catLeft  : Err e₁ → Err (.cat e₁ e₂)
  | catRight : Value e₁ → Err e₂ → Err (.cat e₁ e₂)
  | letBind  : Err e₁ → Err (.letE x e₁ e₂)

inductive Progress {C : Type} (e : Expr C) : Prop where
  | step : Step e e' → Progress e
  | done : Value e → Progress e
  | err  : Err e → Progress e

-- Selecting on a record literal always progresses: hit steps, miss errors.
private theorem sel_rcd_progress {C : Type} (b : RecBody (Expr C)) (l : Label) :
    Progress (.sel (.rcd b) l) := by
  cases hbl : RecBody.lookup l b with
  | some e' => exact .step (.selVal hbl)
  | none    => exact .err (.selAbsent hbl)

def progress {B C : Type} {constTy : C → B} {Γ : Ctx B} {e : Expr C} {τ : Ty B}
    (hΓ : Γ = Ctx.empty) (ht : Typed constTy Γ e τ) : Progress e :=
  match ht with
  | .tCon        => .done .con
  | .tVar h _    => by subst hΓ; simp [Ctx.lookup, Ctx.empty] at h
  | .tEq h _     => progress hΓ h
  | .tLam _      => .done .lam
  | .tRcd _      => .done .rcd
  | .tApp h₁ h₂  =>
      match progress hΓ h₁ with
      | .step s  => .step (.appFun s)
      | .err er  => .err (.appFun er)
      | .done v₁ =>
          match progress hΓ h₂ with
          | .step s  => .step (.appArg v₁ s)
          | .err er  => .err (.appArg v₁ er)
          | .done v₂ => by
              obtain ⟨x, e₀, rfl⟩ := canonical_fn v₁ h₁
              exact .step (.beta v₂)
  | .tCat h₁ h₂  =>
      match progress hΓ h₁ with
      | .step s  => .step (.catLeft s)
      | .err er  => .err (.catLeft er)
      | .done v₁ =>
          match progress hΓ h₂ with
          | .step s  => .step (.catRight v₁ s)
          | .err er  => .err (.catRight v₁ er)
          | .done v₂ => by
              obtain ⟨b₁, ρ₁', rfl, -, -⟩ := canonical_rcd v₁ h₁
              obtain ⟨b₂, ρ₂', rfl, -, -⟩ := canonical_rcd v₂ h₂
              exact .step .catVal
  | .tSel he hl  =>
      match progress hΓ he with
      | .step s => .step (.selStep s)
      | .err er => .err (.sel er)
      | .done v => by
          obtain ⟨b, ρ', rfl, heq, hb⟩ := canonical_rcd v he
          -- carry the found-lookup across ≈ᵣ onto the literal's row,
          -- then read the field off the body
          obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm heq) hl
          cases hre
          obtain ⟨e', hbl, -⟩ := TypedBody.lookup_found hb hl'
          exact .step (.selVal hbl)
  | .tSelUnk he _ =>
      match progress hΓ he with
      | .step s => .step (.selStep s)
      | .err er => .err (.sel er)
      | .done v => by
          obtain ⟨b, ρ', rfl, -, -⟩ := canonical_rcd v he
          exact sel_rcd_progress b _
  -- T-sel-⊥: the ↯-disjunct at work — the selection is typed (at ★) and errs.
  | .tSelAbs he _ =>
      match progress hΓ he with
      | .step s => .step (.selStep s)
      | .err er => .err (.sel er)
      | .done v => by
          obtain ⟨b, ρ', rfl, -, -⟩ := canonical_rcd v he
          exact sel_rcd_progress b _
  | .tUnk h => progress hΓ h
  -- The binding is typeable at (at least) the scheme's own body — enough to
  -- drive it to a value, an error, or a step.
  | .tLet h₁ _ =>
      match progress hΓ (h₁ _ (Scheme.Inst.self _)) with
      | .step s  => .step (.letCong s)
      | .err er  => .err (.letBind er)
      | .done v₁ => .step (.letBeta v₁)


---------------------------------- PRESERVATION ---------------------------------
-- ## Polymorphic substitution lemma  (key auxiliary for Preservation)
--
--   If Γ, x:σ ⊢ e : τ₂  and  ⊢ v : τ' for *every* instance τ' of σ
--   (v closed over Γ's row-solutions), then  Γ ⊢ e[x:=v] : τ₂
--
-- v must be *closed*: with named binders, pushing an open v under a λ could
-- capture its free variables. Closed v suffices for preservation of closed
-- programs (β and let-β only fire at ∅), and dodges all renaming machinery.
--
-- The all-instances hypothesis is exactly what T-let's instance-closed
-- premise provides at let-β; each use site of x picks its instance and grabs
-- the matching typing of v. Monotype bindings (β) are the singleton case.
--
-- The recursion is over the derivation of e, but its context changes shape
-- under λ/let (exchange/shadowing), so the statement is generalized to any Δ
-- Sub-below Γ,x:σ — keeping the derivation indices variable, which is what
-- structural recursion over an inductive family needs.

mutual
private theorem subst_aux {B C : Type} {constTy : C → B} :
    {Δ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Δ e τ →
    ∀ {Γ : Ctx B} {x : Var} {v : Expr C} {σ : Scheme B},
      Ctx.Sub Δ (Γ.bindScheme x σ) →
      (∀ τ', σ.Inst τ' → Typed constTy ⟨[], Γ.rowEnv⟩ v τ') →
      Typed constTy Γ (subst x v e) τ
  | _, _, _, .tCon, _, _, _, _, _, _ => .tCon
  | _, .var y, _, .tVar h hi, _, x, _, _, hsub, hv => by
      have hy := hsub.1 _ _ h
      rw [Ctx.lookup_bindScheme] at hy
      simp only [subst]
      cases hxy : (x == y)
      · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
        exact .tVar hy hi
      · simp only [hxy, if_true] at hy ⊢
        -- x = y: the hit binding is the scheme itself; this use site's
        -- instance selects the matching typing of v
        cases Option.some.inj hy
        exact typed_sub (Ctx.Sub.ofEmptyTyEnv _) (hv _ hi)
  | _, _, _, .tEq h heq, _, _, _, _, hsub, hv =>
      .tEq (subst_aux h hsub hv) heq
  | _, .lam y e₀, _, .tLam h, _, x, _, _, hsub, hv => by
      simp only [subst]
      cases hxy : (x == y)
      · -- x ≠ y: substitute in the body, exchanging the two bindings
        simp only [Bool.false_eq_true, if_false]
        exact .tLam (subst_aux h
          ((hsub.bindTy _ _).trans
            (Ctx.Sub.exchange _ (by simpa using hxy) _ _)) hv)
      · -- x = y: the binder shadows x, the body is untouched
        simp only [if_true]
        exact .tLam (typed_sub ((eq_of_beq hxy) ▸ hsub.shadowed _) h)
  | _, _, _, .tApp h₁ h₂, _, _, _, _, hsub, hv =>
      .tApp (subst_aux h₁ hsub hv) (subst_aux h₂ hsub hv)
  | _, _, _, .tCat h₁ h₂, _, _, _, _, hsub, hv =>
      .tCat (subst_aux h₁ hsub hv) (subst_aux h₂ hsub hv)
  | _, _, _, .tSel h hl, Γ, _, _, _, hsub, hv =>
      .tSel (subst_aux h hsub hv)
            (Lookup.congr_rowEnv (Γ₂ := Γ) (fun α => hsub.2 α) hl)
  | _, _, _, .tSelUnk h hl, Γ, _, _, _, hsub, hv =>
      .tSelUnk (subst_aux h hsub hv)
               (Lookup.congr_rowEnv (Γ₂ := Γ) (fun α => hsub.2 α) hl)
  | _, _, _, .tSelAbs h hl, Γ, _, _, _, hsub, hv =>
      .tSelAbs (subst_aux h hsub hv)
               (Lookup.congr_rowEnv (Γ₂ := Γ) (fun α => hsub.2 α) hl)
  | _, _, _, .tUnk h, _, _, _, _, hsub, hv =>
      .tUnk (subst_aux h hsub hv)
  | _, .letE y e₁ e₂, _, .tLet h₁ h₂, _, x, _, _, hsub, hv => by
      simp only [subst]
      cases hxy : (x == y)
      · -- x ≠ y: substitute binding and body, exchanging the two binders
        simp only [Bool.false_eq_true, if_false]
        exact .tLet (fun τ' hi => subst_aux (h₁ τ' hi) hsub hv)
          (subst_aux h₂
            ((hsub.bindScheme _ _).trans
              (Ctx.Sub.exchange _ (by simpa using hxy) _ _)) hv)
      · -- x = y: the let-binder shadows x, the body is untouched
        simp only [if_true]
        exact .tLet (fun τ' hi => subst_aux (h₁ τ' hi) hsub hv)
          (typed_sub ((eq_of_beq hxy) ▸ hsub.shadowed _) h₂)
  | _, _, _, .tRcd h, _, _, _, _, hsub, hv =>
      .tRcd (substBody_aux h hsub hv)

private theorem substBody_aux {B C : Type} {constTy : C → B} :
    {Δ : Ctx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    TypedBody constTy Δ b ρ →
    ∀ {Γ : Ctx B} {x : Var} {v : Expr C} {σ : Scheme B},
      Ctx.Sub Δ (Γ.bindScheme x σ) →
      (∀ τ', σ.Inst τ' → Typed constTy ⟨[], Γ.rowEnv⟩ v τ') →
      TypedBody constTy Γ (substBody x v b) ρ
  | _, _, _, .empty, _, _, _, _, _, _ => .empty
  | _, _, _, .field h, _, _, _, _, hsub, hv => .field (subst_aux h hsub hv)
  | _, _, _, .cat h₁ h₂, _, _, _, _, hsub, hv =>
      .cat (substBody_aux h₁ hsub hv) (substBody_aux h₂ hsub hv)
end

-- Scheme-bound variables: v must be typeable at every instance (provided by
-- T-let's premise at let-β).
theorem subst_scheme_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (x : Var) (v : Expr C) (σ : Scheme B) (τ₂ : Ty B) (e : Expr C)
    (hv : ∀ τ', σ.Inst τ' → Typed constTy { Γ with tyEnv := [] } v τ')
    (he : Typed constTy (Γ.bindScheme x σ) e τ₂) :
    Typed constTy Γ (subst x v e) τ₂ :=
  subst_aux he (Ctx.Sub.refl _) hv

-- Monotype-bound variables (β): the singleton case.
theorem subst_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (x : Var) (v : Expr C) (τ₁ τ₂ : Ty B) (e : Expr C)
    (hv : Typed constTy { Γ with tyEnv := [] } v τ₁)
    (he : Typed constTy (Γ.bindTy x τ₁) e τ₂) :
    Typed constTy Γ (subst x v e) τ₂ :=
  subst_scheme_preserves_typing constTy Γ x v ⟨[], τ₁⟩ τ₂ e
    (fun _ hi => hi.mono.symm ▸ hv) he

--   If ∅ ⊢ e : τ  and  e → e'  then  ∅ ⊢ e' : τ
--   Stated for closed programs (β needs a closed argument, see above).
--   Key steps: selVal needs term/type lookup agreement carried across ≈ᵣ;
--   the tSelUnk/selVal case is vacuous — record literals have spine-var-free
--   rows, so their lookups are never ?.

private theorem preservation_aux {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    Γ = Ctx.empty → ∀ {e' : Expr C}, Step e e' → Typed constTy Γ e' τ
  | _, _, _, .tCon,   _, _ => (nomatch ·)
  | _, _, _, .tVar _ _, _, _ => (nomatch ·)
  | _, _, _, .tLam _, _, _ => (nomatch ·)
  | _, _, _, .tRcd _, _, _ => (nomatch ·)
  | _, _, _, .tEq h heq, hΓ, _ => fun hs =>
      .tEq (preservation_aux h hΓ hs) heq
  | _, _, _, .tApp h₁ h₂, hΓ, _ => fun hs => by
      cases hs with
      | appFun s     => exact .tApp (preservation_aux h₁ hΓ s) h₂
      | appArg v s   => exact .tApp h₁ (preservation_aux h₂ hΓ s)
      | beta hval =>
          subst hΓ
          obtain ⟨σ₁, σ₂, heq | hu, hbody⟩ := typed_lam_inv h₁
          · obtain ⟨τ₁', τ₂', hfn, he₁, he₂⟩ := heq.fn_inv
            cases hfn
            -- retype the argument at the λ's domain, then substitute
            exact .tEq
              (subst_preserves_typing _ _ _ _ _ _ _
                (.tEq h₂ he₁.symm) hbody)
              he₂
          · cases hu
  | _, _, _, .tCat h₁ h₂, hΓ, _ => fun hs => by
      cases hs with
      | catLeft s    => exact .tCat (preservation_aux h₁ hΓ s) h₂
      | catRight v s => exact .tCat h₁ (preservation_aux h₂ hΓ s)
      | catVal =>
          obtain ⟨ρ₁', he₁ | hu₁, hb₁⟩ := typed_rcd_inv h₁
          · obtain ⟨ρ₂', he₂ | hu₂, hb₂⟩ := typed_rcd_inv h₂
            · obtain ⟨_, hσ₁, hr₁⟩ := he₁.rcd_inv
              obtain ⟨_, hσ₂, hr₂⟩ := he₂.rcd_inv
              cases hσ₁; cases hσ₂
              exact .tEq (.tRcd (.cat hb₂ hb₁)) (.rcd (.cat hr₂ hr₁))
            · cases hu₂
          · cases hu₁
  | _, _, _, .tSel h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .tSel (preservation_aux h hΓ s) hl
      | selVal hbl =>
          obtain ⟨ρ', he | hu, hb⟩ := typed_rcd_inv h
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre with
            | found hty =>
                obtain ⟨e'', hbl', hte⟩ := TypedBody.lookup_found hb hl'
                rw [hbl] at hbl'
                exact Option.some.inj hbl' ▸ .tEq hte hty.symm
          · cases hu
  | _, _, _, .tSelUnk h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .tSelUnk (preservation_aux h hΓ s) hl
      | selVal hbl =>
          -- vacuous: the literal's row is spine-var-free, its lookup can't be ?
          obtain ⟨ρ', he | hu, hb⟩ := typed_rcd_inv h
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre
            exact (Lookup.not_unknown_of_spineVarFree hb.spineVarFree hl').elim
          · cases hu
  | _, _, _, .tSelAbs h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .tSelAbs (preservation_aux h hΓ s) hl
      | selVal hbl =>
          -- vacuous: an absent lookup on the literal's row means the body has
          -- no such field, contradicting the successful syntactic lookup
          obtain ⟨ρ', he | hu, hb⟩ := typed_rcd_inv h
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre
            rw [TypedBody.lookup_absent hb hl'] at hbl
            cases hbl
          · cases hu
  | _, _, _, .tUnk h, hΓ, _ => fun hs =>
      .tUnk (preservation_aux h hΓ hs)
  | _, _, _, .tLet h₁ h₂, hΓ, _ => fun hs => by
      cases hs with
      | letCong s =>
          exact .tLet (fun τ' hi => preservation_aux (h₁ τ' hi) hΓ s) h₂
      | letBeta hval =>
          -- let-β: the instance-closed premise is exactly the all-instances
          -- hypothesis of the polymorphic substitution lemma
          subst hΓ
          exact subst_scheme_preserves_typing _ _ _ _ _ _ _
            (fun τ' hi => h₁ τ' hi) h₂

theorem preservation
    {B C : Type} (constTy : C → B)
    (e e' : Expr C) (τ : Ty B)
    (ht : Typed constTy Ctx.empty e τ)
    (hs : Step e e') :
    Typed constTy Ctx.empty e' τ :=
  preservation_aux ht rfl hs


---------------------------------- REGRESSION ----------------------------------
-- The program that breaks preservation without T-sel-⊥/T-★-intro:
--   let f = (x: x.l) in f {}   :   ★
-- f's scheme ∀β. {β} → ★ demands the lambda typed at *every* instance
-- {ρ} → ★; the three-way case split on the (total) lookup of ρ.l hits all
-- three selection rules — with T-sel-⊥ covering exactly the instance ρ = ε
-- that used to make the reduct untypeable.

example {B C : Type} (constTy : C → B) :
    Typed constTy Ctx.empty
      (.letE "f" (.lam "x" (.sel (.var "x") "l"))
        (.app (.var "f") (.rcd .empty)))
      .unk := by
  apply Typed.tLet (σ := ⟨["β"], .fn (.rcd (.var "β")) .unk⟩)
  · -- the binding types at every instance {ρ} → ★
    intro τ' hi
    obtain ⟨θ, -, rfl⟩ := hi
    simp only [Ty.applySubst, Row.applySubst]
    apply Typed.tLam
    have hx : (Ctx.empty.bindTy "x" (.rcd (θ.row "β"))).lookup "x"
        = some ⟨[], .rcd (θ.row "β")⟩ := by
      simp [Ctx.lookup_bindTy]
    have hwf : (Ctx.empty.bindTy "x" (.rcd (θ.row "β")) : Ctx B).RowWF :=
      ⟨fun _ => 0, fun α ρ h => by
        simp [Ctx.lookupRow, Ctx.bindTy, Ctx.bindScheme, Ctx.empty] at h⟩
    obtain ⟨r, hr⟩ := lookup_total hwf (θ.row "β") "l"
    cases r with
    | found τf => exact .tUnk (.tSel (.tVar hx (Scheme.Inst.refl _)) hr)
    | absent   => exact .tSelAbs (.tVar hx (Scheme.Inst.refl _)) hr
    | unknown  => exact .tSelUnk (.tVar hx (Scheme.Inst.refl _)) hr
  · -- the body instantiates f at β ≔ ε and applies it to {}
    have hif : Scheme.Inst (⟨["β"], .fn (.rcd (.var "β")) .unk⟩ : Scheme B)
        (.fn (.rcd .empty) .unk) := by
      refine ⟨⟨fun α => .var α, fun α => if α == "β" then .empty else .var α⟩,
        ⟨fun _ _ => rfl, fun α hα => ?_⟩, ?_⟩
      · simp only [List.mem_singleton] at hα
        simp [hα]
      · simp [Ty.applySubst, Row.applySubst]
    exact .tApp (.tVar (by simp [Ctx.lookup_bindScheme]) hif) (.tRcd .empty)

--------------------- SUBSTITUTION COMPOSITION & AGREEMENT ---------------------
-- Toolkit for the type-substitution lemma below. Two facts about applySubst:
-- sequencing is composition, and the result only depends on θ's action on the
-- free variables of the subject.

-- θ₂.comp θ₁: first θ₁, then θ₂.
def TySubst.comp {B : Type} (θ₂ θ₁ : TySubst B) : TySubst B :=
  ⟨fun α => (θ₁.ty α).applySubst θ₂, fun α => (θ₁.row α).applySubst θ₂⟩

mutual
  theorem Ty.applySubst_applySubst {B : Type} (θ₁ θ₂ : TySubst B) :
      (τ : Ty B) → (τ.applySubst θ₁).applySubst θ₂ = τ.applySubst (θ₂.comp θ₁)
    | .var _    => by simp only [Ty.applySubst, TySubst.comp]
    | .base _   => rfl
    | .unk      => rfl
    | .fn τ₁ τ₂ => by
        simp only [Ty.applySubst, Ty.applySubst_applySubst θ₁ θ₂ τ₁,
                   Ty.applySubst_applySubst θ₁ θ₂ τ₂]
    | .rcd ρ    => by
        simp only [Ty.applySubst, Row.applySubst_applySubst θ₁ θ₂ ρ]

  theorem Row.applySubst_applySubst {B : Type} (θ₁ θ₂ : TySubst B) :
      (ρ : Row B) → (ρ.applySubst θ₁).applySubst θ₂ = ρ.applySubst (θ₂.comp θ₁)
    | .empty     => rfl
    | .var _     => by simp only [Row.applySubst, TySubst.comp]
    | .sing _ τ  => by
        simp only [Row.applySubst, Ty.applySubst_applySubst θ₁ θ₂ τ]
    | .cat ρ₁ ρ₂ => by
        simp only [Row.applySubst, Row.applySubst_applySubst θ₁ θ₂ ρ₁,
                   Row.applySubst_applySubst θ₁ θ₂ ρ₂]
end

-- applySubst only consults θ on the subject's free variables.
mutual
  theorem Ty.applySubst_congr {B : Type} {θ₁ θ₂ : TySubst B} :
      (τ : Ty B) →
      (∀ α ∈ τ.ftv, θ₁.ty α = θ₂.ty α ∧ θ₁.row α = θ₂.row α) →
      τ.applySubst θ₁ = τ.applySubst θ₂
    | .var α, h    => by
        simp only [Ty.applySubst]
        exact (h α (by simp [Ty.ftv])).1
    | .base _, _   => rfl
    | .unk, _      => rfl
    | .fn τ₁ τ₂, h => by
        simp only [Ty.applySubst]
        rw [Ty.applySubst_congr τ₁ (fun α hα => h α (by simp [Ty.ftv, hα])),
            Ty.applySubst_congr τ₂ (fun α hα => h α (by simp [Ty.ftv, hα]))]
    | .rcd ρ, h    => by
        simp only [Ty.applySubst]
        rw [Row.applySubst_congr ρ (fun α hα => h α (by simpa [Ty.ftv] using hα))]

  theorem Row.applySubst_congr {B : Type} {θ₁ θ₂ : TySubst B} :
      (ρ : Row B) →
      (∀ α ∈ ρ.ftv, θ₁.ty α = θ₂.ty α ∧ θ₁.row α = θ₂.row α) →
      ρ.applySubst θ₁ = ρ.applySubst θ₂
    | .empty, _     => rfl
    | .var α, h     => by
        simp only [Row.applySubst]
        exact (h α (by simp [Row.ftv])).2
    | .sing _ τ, h  => by
        simp only [Row.applySubst]
        rw [Ty.applySubst_congr τ (fun α hα => h α (by simpa [Row.ftv] using hα))]
    | .cat ρ₁ ρ₂, h => by
        simp only [Row.applySubst]
        rw [Row.applySubst_congr ρ₁ (fun α hα => h α (by simp [Row.ftv, hα])),
            Row.applySubst_congr ρ₂ (fun α hα => h α (by simp [Row.ftv, hα]))]
end

-- Local version of applySubst_fixed: fixing just the subject's ftv suffices.
theorem Ty.applySubst_fixed_ftv {B : Type} {θ : TySubst B} (τ : Ty B)
    (h : ∀ α ∈ τ.ftv, θ.ty α = .var α ∧ θ.row α = .var α) :
    τ.applySubst θ = τ := by
  rw [Ty.applySubst_congr (θ₂ := TySubst.id B) τ
        (fun α hα => by simpa [TySubst.id] using h α hα),
      Ty.applySubst_id]

theorem Row.applySubst_fixed_ftv {B : Type} {θ : TySubst B} (ρ : Row B)
    (h : ∀ α ∈ ρ.ftv, θ.ty α = .var α ∧ θ.row α = .var α) :
    ρ.applySubst θ = ρ := by
  rw [Row.applySubst_congr (θ₂ := TySubst.id B) ρ
        (fun α hα => by simpa [TySubst.id] using h α hα),
      Row.applySubst_id]

--------------------------- FRESH NAMES & RENAMING -----------------------------
-- To push a substitution under a scheme's binder its bound variables must be
-- renamed away from a finite avoid-set (capture-avoidance). Fresh names are
-- generated by length: natName n has length n, so names strictly longer than
-- everything in the avoid-set are fresh, and distinct lengths never collide.

def natName (n : Nat) : TyVar := String.ofList (List.replicate n 'a')

theorem natName_length (n : Nat) : (natName n).length = n := by simp [natName]

theorem natName_inj {m n : Nat} (h : natName m = natName n) : m = n := by
  have : (natName m).length = (natName n).length := by rw [h]
  simpa [natName_length] using this

def lenBound (l : List TyVar) : Nat := l.foldr (fun s m => max s.length m) 0

theorem length_le_lenBound {s : TyVar} :
    {l : List TyVar} → s ∈ l → s.length ≤ lenBound l
  | a :: l', h => by
      rcases List.mem_cons.mp h with rfl | h'
      · simp only [lenBound, List.foldr]
        exact Nat.le_max_left _ _
      · calc s.length ≤ lenBound l' := length_le_lenBound h'
          _ ≤ _ := by simp only [lenBound, List.foldr]; exact Nat.le_max_right _ _

-- Association list pairing each variable with a fresh name, counting up from n.
def mkRen : List TyVar → Nat → List (TyVar × TyVar)
  | [], _ => []
  | α :: l, n => (α, natName n) :: mkRen l (n + 1)

theorem mkRen_fst {p : TyVar × TyVar} :
    {l : List TyVar} → {n : Nat} → p ∈ mkRen l n → p.1 ∈ l
  | α :: l', _, h => by
      rcases List.mem_cons.mp h with rfl | h'
      · exact List.mem_cons_self ..
      · exact List.mem_cons_of_mem _ (mkRen_fst h')

theorem mkRen_snd {p : TyVar × TyVar} :
    {l : List TyVar} → {n : Nat} → p ∈ mkRen l n → ∃ m, n ≤ m ∧ p.2 = natName m
  | _ :: l', n, h => by
      rcases List.mem_cons.mp h with rfl | h'
      · exact ⟨n, Nat.le_refl n, rfl⟩
      · obtain ⟨m, hm, hp⟩ := mkRen_snd h'
        exact ⟨m, Nat.le_of_succ_le hm, hp⟩

-- Looking up a variable not in the domain fails.
theorem mkRen_fst_none {α : TyVar} {l : List TyVar} {n : Nat} (h : α ∉ l) :
    (mkRen l n).find? (·.1 == α) = none := by
  rw [List.find?_eq_none]
  intro p hp hbeq
  exact h (eq_of_beq hbeq ▸ mkRen_fst hp)

-- Fresh names generated above the avoid-bound never look up backwards.
theorem mkRen_snd_none {γ : TyVar} {avoid l : List TyVar} (h : γ ∈ avoid) :
    (mkRen l (lenBound avoid + 1)).find? (·.2 == γ) = none := by
  rw [List.find?_eq_none]
  intro p hp hbeq
  obtain ⟨m, hm, hp2⟩ := mkRen_snd hp
  have hlen : γ.length ≤ lenBound avoid := length_le_lenBound h
  have : p.2 = γ := eq_of_beq hbeq
  rw [hp2] at this
  rw [← this] at hlen
  rw [natName_length] at hlen
  omega

-- Round trip: every domain variable finds a fresh partner, and that partner
-- finds it back (the fresh names are pairwise distinct by construction).
theorem mkRen_spec {α : TyVar} :
    {l : List TyVar} → {n : Nat} → α ∈ l →
    ∃ β, (mkRen l n).find? (·.1 == α) = some (α, β) ∧
         (mkRen l n).find? (·.2 == β) = some (α, β) ∧
         ∃ m, n ≤ m ∧ β = natName m
  | α' :: l', n, h => by
      by_cases hα : α' = α
      · subst hα
        exact ⟨natName n,
          List.find?_cons_of_pos (by simp),
          List.find?_cons_of_pos (by simp),
          n, Nat.le_refl n, rfl⟩
      · have h' : α ∈ l' := by
          rcases List.mem_cons.mp h with rfl | h'
          · exact absurd rfl hα
          · exact h'
        obtain ⟨β, hfwd, hbwd, m, hm, rfl⟩ := mkRen_spec (n := n + 1) h'
        refine ⟨natName m, ?_, ?_, m, Nat.le_of_succ_le hm, rfl⟩
        · simp only [mkRen]
          rw [List.find?_cons_of_neg (by simp [hα]), hfwd]
        · simp only [mkRen]
          rw [List.find?_cons_of_neg ?_, hbwd]
          intro hbeq
          have := natName_inj (eq_of_beq hbeq)
          omega

------------------------------- SCHEME COVERING --------------------------------
-- σ' covers σ under θ: any χ-image of a σ-instance is a σ'-instance, for any χ
-- that agrees with θ on σ's outward-visible variables. The χ-quantification
-- (rather than plain θ) is exactly what survives the tLet case of the
-- substitution lemma, where θ must be perturbed on freshly chosen binders.

def SchemeAgree {B : Type} (θ χ : TySubst B) (σ : Scheme B) : Prop :=
  ∀ α ∈ σ.body.ftv, α ∉ σ.vars → χ.ty α = θ.ty α ∧ χ.row α = θ.row α

def Covers {B : Type} (θ : TySubst B) (σ σ' : Scheme B) : Prop :=
  ∀ χ, SchemeAgree θ χ σ → ∀ τ, σ.Inst τ → σ'.Inst (τ.applySubst χ)

-- The four substitutions of the renaming construction:
-- renSub: bound variables to their fresh partners (identity elsewhere)
private def renSub {B : Type} (prs : List (TyVar × TyVar)) : TySubst B :=
  ⟨fun α => .var (match prs.find? (·.1 == α) with | some p => p.2 | none => α),
   fun α => .var (match prs.find? (·.1 == α) with | some p => p.2 | none => α)⟩

-- outSub: θ acting through the renamed binder (identity on the fresh names)
private def outSub {B : Type} (θ : TySubst B) (prs : List (TyVar × TyVar)) :
    TySubst B :=
  ⟨fun β => if (prs.find? (·.2 == β)).isSome then .var β else θ.ty β,
   fun β => if (prs.find? (·.2 == β)).isSome then .var β else θ.row β⟩

-- bwdSub: instantiate a fresh binder the way χ ∘ θ₁ instantiates its original
private def bwdSub {B : Type} (prs : List (TyVar × TyVar)) (θ₁ χ : TySubst B) :
    TySubst B :=
  ⟨fun β => match prs.find? (·.2 == β) with
     | some p => (θ₁.ty p.1).applySubst χ
     | none => .var β,
   fun β => match prs.find? (·.2 == β) with
     | some p => (θ₁.row p.1).applySubst χ
     | none => .var β⟩

-- mixSub: θ₂ on the fresh binders, θ elsewhere
private def mixSub {B : Type} (prs : List (TyVar × TyVar)) (θ₂ θ : TySubst B) :
    TySubst B :=
  ⟨fun γ => if (prs.find? (·.2 == γ)).isSome then θ₂.ty γ else θ.ty γ,
   fun γ => if (prs.find? (·.2 == γ)).isSome then θ₂.row γ else θ.row γ⟩

-- Core of the renaming argument, abstracted over the pairing list prs.
private theorem renameScheme_core {B : Type} (θ : TySubst B) (σ : Scheme B)
    (avoid : List TyVar) (prs : List (TyVar × TyVar))
    (hbody : ∀ α ∈ σ.body.ftv, α ∈ avoid)
    (himgTy : ∀ α ∈ σ.body.ftv, ∀ β ∈ (θ.ty α).ftv, β ∈ avoid)
    (himgRow : ∀ α ∈ σ.body.ftv, ∀ β ∈ (θ.row α).ftv, β ∈ avoid)
    (hspec : ∀ α ∈ σ.vars, ∃ β, prs.find? (·.1 == α) = some (α, β) ∧
        prs.find? (·.2 == β) = some (α, β))
    (hfstNone : ∀ α ∉ σ.vars, prs.find? (·.1 == α) = none)
    (hsndNone : ∀ γ ∈ avoid, prs.find? (·.2 == γ) = none) :
    ∃ σ' : Scheme B,
      Covers θ σ σ' ∧
      (∀ τ'', σ'.Inst τ'' → ∃ χ τ₀, σ.Inst τ₀ ∧ τ'' = τ₀.applySubst χ ∧
        ∀ γ ∈ avoid, χ.ty γ = θ.ty γ ∧ χ.row γ = θ.row γ) := by
  -- avoid-members never appear among the fresh binders
  have hnotMemNone : ∀ β, β ∉ prs.map (·.2) → prs.find? (·.2 == β) = none := by
    intro β hβ
    cases hf : prs.find? (·.2 == β) with
    | none => rfl
    | some p =>
        have hp2 := List.find?_some hf
        exact absurd (List.mem_map.mpr ⟨p, List.mem_of_find?_eq_some hf,
          eq_of_beq hp2⟩) hβ
  have hsndNotMem : ∀ γ ∈ avoid, γ ∉ prs.map (·.2) := by
    intro γ hγ hmem
    obtain ⟨p, hp, hpγ⟩ := List.mem_map.mp hmem
    exact List.find?_eq_none.mp (hsndNone γ hγ) p hp (by simp [hpγ])
  refine ⟨⟨prs.map (·.2),
    (σ.body.applySubst (renSub prs)).applySubst (outSub θ prs)⟩, ?_, ?_⟩
  · -- Covers: a χ-image of an instance θ₁ is the bwdSub-instance of σ'
    rintro χ hagree τ ⟨θ₁, ⟨hfixTy, hfixRow⟩, rfl⟩
    refine ⟨bwdSub prs θ₁ χ, ⟨?_, ?_⟩, ?_⟩
    · intro β hβ
      simp [bwdSub, hnotMemNone β hβ]
    · intro β hβ
      simp [bwdSub, hnotMemNone β hβ]
    · rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst,
          Ty.applySubst_applySubst]
      apply Ty.applySubst_congr
      intro α hα
      by_cases hv : α ∈ σ.vars
      · obtain ⟨β, hfwd, hbwd⟩ := hspec α hv
        constructor
        · simp [TySubst.comp, renSub, hfwd, Ty.applySubst, outSub, hbwd, bwdSub]
        · simp [TySubst.comp, renSub, hfwd, Ty.applySubst, Row.applySubst,
                outSub, hbwd, bwdSub]
      · have h1 : (θ.ty α).applySubst (bwdSub prs θ₁ χ) = θ.ty α :=
          Ty.applySubst_fixed_ftv _ (fun γ hγ => by
            simp [bwdSub, hsndNone γ (himgTy α hα γ hγ)])
        have h2 : (θ.row α).applySubst (bwdSub prs θ₁ χ) = θ.row α :=
          Row.applySubst_fixed_ftv _ (fun γ hγ => by
            simp [bwdSub, hsndNone γ (himgRow α hα γ hγ)])
        constructor
        · simp [TySubst.comp, renSub, hfstNone α hv, Ty.applySubst, outSub,
                hsndNone α (hbody α hα), h1, hfixTy α hv, (hagree α hα hv).1]
        · simp [TySubst.comp, renSub, hfstNone α hv, Ty.applySubst,
                Row.applySubst, outSub, hsndNone α (hbody α hα), h2,
                hfixRow α hv, (hagree α hα hv).2]
  · -- every σ'-instance θ₂ is the mixSub-image of the renamed σ-instance
    rintro τ'' ⟨θ₂, ⟨hfx2Ty, hfx2Row⟩, rfl⟩
    refine ⟨mixSub prs θ₂ θ, σ.body.applySubst (renSub prs),
      ⟨renSub prs, ⟨?_, ?_⟩, rfl⟩, ?_, ?_⟩
    · intro α hα; simp [renSub, hfstNone α hα]
    · intro α hα; simp [renSub, hfstNone α hα]
    · rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst,
          Ty.applySubst_applySubst]
      apply Ty.applySubst_congr
      intro α hα
      by_cases hv : α ∈ σ.vars
      · obtain ⟨β, hfwd, hbwd⟩ := hspec α hv
        constructor
        · simp [TySubst.comp, renSub, hfwd, Ty.applySubst, outSub, hbwd, mixSub]
        · simp [TySubst.comp, renSub, hfwd, Ty.applySubst, Row.applySubst,
                outSub, hbwd, mixSub]
      · have h1 : (θ.ty α).applySubst θ₂ = θ.ty α :=
          Ty.applySubst_fixed_ftv _ (fun γ hγ =>
            ⟨hfx2Ty γ (hsndNotMem γ (himgTy α hα γ hγ)),
             hfx2Row γ (hsndNotMem γ (himgTy α hα γ hγ))⟩)
        have h2 : (θ.row α).applySubst θ₂ = θ.row α :=
          Row.applySubst_fixed_ftv _ (fun γ hγ =>
            ⟨hfx2Ty γ (hsndNotMem γ (himgRow α hα γ hγ)),
             hfx2Row γ (hsndNotMem γ (himgRow α hα γ hγ))⟩)
        constructor
        · simp [TySubst.comp, renSub, hfstNone α hv, Ty.applySubst, outSub,
                hsndNone α (hbody α hα), h1, mixSub]
        · simp [TySubst.comp, renSub, hfstNone α hv, Ty.applySubst,
                Row.applySubst, outSub, hsndNone α (hbody α hα), h2, mixSub]
    · intro γ hγ
      simp [mixSub, hsndNone γ hγ]

-- Any scheme can be θ-covered after renaming its binders away from a finite
-- avoid-set that contains its body's ftv and the ftv of θ's relevant images.
theorem renameScheme {B : Type} (θ : TySubst B) (σ : Scheme B)
    (avoid : List TyVar)
    (hbody : ∀ α ∈ σ.body.ftv, α ∈ avoid)
    (himgTy : ∀ α ∈ σ.body.ftv, ∀ β ∈ (θ.ty α).ftv, β ∈ avoid)
    (himgRow : ∀ α ∈ σ.body.ftv, ∀ β ∈ (θ.row α).ftv, β ∈ avoid) :
    ∃ σ' : Scheme B,
      Covers θ σ σ' ∧
      (∀ τ'', σ'.Inst τ'' → ∃ χ τ₀, σ.Inst τ₀ ∧ τ'' = τ₀.applySubst χ ∧
        ∀ γ ∈ avoid, χ.ty γ = θ.ty γ ∧ χ.row γ = θ.row γ) := by
  refine renameScheme_core θ σ avoid (mkRen σ.vars (lenBound avoid + 1))
    hbody himgTy himgRow ?_ ?_ ?_
  · intro α hα
    obtain ⟨β, hfwd, hbwd, -⟩ := mkRen_spec (n := lenBound avoid + 1) hα
    exact ⟨β, hfwd, hbwd⟩
  · intro α hα
    exact mkRen_fst_none hα
  · intro γ hγ
    exact mkRen_snd_none hγ

--------------------------- TYPE-SUBSTITUTION LEMMA ----------------------------
-- Typing is stable under substitution, relative to a context relation that
-- transports each scheme to a covering one. Selection is the interesting case:
-- substitution can refine (? → τ) or demote (? → ⊥) a lookup, and the
-- derivation is rebuilt through T-sel + T-★-intro / T-sel-⊥ / T-sel-★
-- according to the (total) lookup on the substituted row — exactly the job
-- those rules were added for. Stated for empty rowEnv (as progress and
-- preservation are), so lookups never consult row-solutions.

def InstMap {B : Type} (θ : TySubst B) (Γ Γ' : Ctx B) : Prop :=
  Γ.rowEnv = [] ∧ Γ'.rowEnv = [] ∧
  ∀ x σ, Γ.lookup x = some σ → ∃ σ', Γ'.lookup x = some σ' ∧ Covers θ σ σ'

-- All outward-visible variables of Γ's schemes.
def Ctx.schemeFtv {B : Type} (Γ : Ctx B) : List TyVar :=
  Γ.tyEnv.flatMap (fun p => p.2.body.ftv)

theorem Ctx.lookup_ftv_subset {B : Type} {Γ : Ctx B} {x : Var} {σ : Scheme B}
    (h : Γ.lookup x = some σ) : ∀ α ∈ σ.body.ftv, α ∈ Γ.schemeFtv := by
  intro α hα
  unfold Ctx.lookup at h
  cases hf : Γ.tyEnv.find? (·.1 == x) with
  | none => rw [hf] at h; cases h
  | some p =>
      rw [hf] at h
      simp only [Option.map_some] at h
      cases h
      exact List.mem_flatMap.mpr ⟨p, List.mem_of_find?_eq_some hf, hα⟩

-- Perturbing θ outside Γ's outward-visible variables preserves the relation.
theorem InstMap.perturb {B : Type} {θ χ : TySubst B} {Γ Γ' : Ctx B}
    (h : InstMap θ Γ Γ')
    (hagree : ∀ γ ∈ Γ.schemeFtv, χ.ty γ = θ.ty γ ∧ χ.row γ = θ.row γ) :
    InstMap χ Γ Γ' := by
  refine ⟨h.1, h.2.1, fun x σ hx => ?_⟩
  obtain ⟨σ', hx', hcov⟩ := h.2.2 x σ hx
  refine ⟨σ', hx', fun χ₂ hagree₂ τ hinst => ?_⟩
  refine hcov χ₂ (fun α hα hnv => ?_) τ hinst
  obtain ⟨e1, e2⟩ := hagree₂ α hα hnv
  obtain ⟨f1, f2⟩ := hagree α (Ctx.lookup_ftv_subset hx α hα)
  exact ⟨e1.trans f1, e2.trans f2⟩

-- Monotype bindings are covered by their θ-image.
theorem Covers.mono {B : Type} (θ : TySubst B) (τ₁ : Ty B) :
    Covers θ ⟨[], τ₁⟩ ⟨[], τ₁.applySubst θ⟩ := by
  intro χ hagree τ hinst
  cases Scheme.Inst.mono hinst
  rw [Ty.applySubst_congr τ₁ (fun α hα => hagree α hα (List.not_mem_nil))]
  exact Scheme.Inst.refl _

theorem InstMap.bindScheme {B : Type} {θ : TySubst B} {Γ Γ' : Ctx B}
    {σ σ' : Scheme B} (h : InstMap θ Γ Γ') (x : Var) (hcov : Covers θ σ σ') :
    InstMap θ (Γ.bindScheme x σ) (Γ'.bindScheme x σ') := by
  refine ⟨h.1, h.2.1, fun y σy hy => ?_⟩
  rw [Ctx.lookup_bindScheme] at hy
  rw [Ctx.lookup_bindScheme]
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact h.2.2 y σy hy
  · simp only [hxy, if_true] at hy
    cases Option.some.inj hy
    exact ⟨σ', by simp, hcov⟩

theorem InstMap.bindTy {B : Type} {θ : TySubst B} {Γ Γ' : Ctx B}
    (h : InstMap θ Γ Γ') (x : Var) (τ₁ : Ty B) :
    InstMap θ (Γ.bindTy x τ₁) (Γ'.bindTy x (τ₁.applySubst θ)) :=
  h.bindScheme x (Covers.mono θ τ₁)

mutual
theorem typed_applySubst_aux {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    ∀ {θ : TySubst B} {Γ' : Ctx B}, InstMap θ Γ Γ' →
    Typed constTy Γ' e (τ.applySubst θ)
  | _, _, _, .tCon, _, _, _ => by
      simp only [Ty.applySubst]; exact .tCon
  | _, _, _, .tVar h hi, θ, _, hmap => by
      obtain ⟨σ', hlook, hcov⟩ := hmap.2.2 _ _ h
      exact .tVar hlook (hcov θ (fun α _ _ => ⟨rfl, rfl⟩) _ hi)
  | _, _, _, .tEq h heq, θ, _, hmap =>
      .tEq (typed_applySubst_aux h hmap) (TyEquiv.applySubst θ heq)
  | _, _, _, .tLam h, θ, _, hmap => by
      simp only [Ty.applySubst]
      exact .tLam (typed_applySubst_aux h (hmap.bindTy _ _))
  | _, _, _, .tApp h₁ h₂, θ, _, hmap => by
      have ih₁ := typed_applySubst_aux h₁ hmap
      simp only [Ty.applySubst] at ih₁
      exact .tApp ih₁ (typed_applySubst_aux h₂ hmap)
  | _, _, _, .tCat h₁ h₂, θ, _, hmap => by
      have ih₁ := typed_applySubst_aux h₁ hmap
      have ih₂ := typed_applySubst_aux h₂ hmap
      simp only [Ty.applySubst] at ih₁ ih₂
      simp only [Ty.applySubst, Row.applySubst]
      exact .tCat ih₁ ih₂
  | _, _, _, .tSel h hl, θ, Γ', hmap => by
      have ih := typed_applySubst_aux h hmap
      simp only [Ty.applySubst] at ih
      have hlk := lookup_applySubst (Γ' := Γ') hmap.1 θ hl (by intro hh; cases hh)
      simp only [LookupRes.applySubst] at hlk
      exact .tSel ih hlk
  | _, .sel _ l, _, .tSelUnk (ρ := ρ) h hl, θ, Γ', hmap => by
      have ih := typed_applySubst_aux h hmap
      simp only [Ty.applySubst] at ih ⊢
      have hwf : Γ'.RowWF := ⟨fun _ => 0, fun α ρ' hα => by
        simp [Ctx.lookupRow, hmap.2.1] at hα⟩
      obtain ⟨r, hr⟩ := lookup_total hwf (ρ.applySubst θ) l
      cases r with
      | found τf => exact .tUnk (.tSel ih hr)
      | absent   => exact .tSelAbs ih hr
      | unknown  => exact .tSelUnk ih hr
  | _, _, _, .tSelAbs h hl, θ, Γ', hmap => by
      have ih := typed_applySubst_aux h hmap
      simp only [Ty.applySubst] at ih ⊢
      have hlk := lookup_applySubst (Γ' := Γ') hmap.1 θ hl (by intro hh; cases hh)
      simp only [LookupRes.applySubst] at hlk
      exact .tSelAbs ih hlk
  | _, _, _, .tUnk h, θ, _, hmap => by
      simp only [Ty.applySubst]
      exact .tUnk (typed_applySubst_aux h hmap)
  | _, _, _, .tRcd h, θ, _, hmap => by
      simp only [Ty.applySubst]
      exact .tRcd (typedBody_applySubst_aux h hmap)
  | Γ, .letE x e₁ e₂, _, .tLet (σ := σ) h₁ h₂, θ, Γ', hmap => by
      obtain ⟨σ'', hcov, hsrc⟩ := renameScheme θ σ
        (σ.body.ftv ++ (Γ.schemeFtv ++
          σ.body.ftv.flatMap (fun α => (θ.ty α).ftv ++ (θ.row α).ftv)))
        (fun α hα => List.mem_append_left _ hα)
        (fun α hα β hβ => List.mem_append_right _ (List.mem_append_right _
          (List.mem_flatMap.mpr ⟨α, hα, List.mem_append_left _ hβ⟩)))
        (fun α hα β hβ => List.mem_append_right _ (List.mem_append_right _
          (List.mem_flatMap.mpr ⟨α, hα, List.mem_append_right _ hβ⟩)))
      refine .tLet (σ := σ'') (fun τ'' hi => ?_)
        (typed_applySubst_aux h₂ (hmap.bindScheme x hcov))
      obtain ⟨χ, τ₀, hinst₀, rfl, hχ⟩ := hsrc τ'' hi
      exact typed_applySubst_aux (h₁ τ₀ hinst₀)
        (hmap.perturb (fun γ hγ => hχ γ (List.mem_append_right _
          (List.mem_append_left _ hγ))))

theorem typedBody_applySubst_aux {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    TypedBody constTy Γ b ρ →
    ∀ {θ : TySubst B} {Γ' : Ctx B}, InstMap θ Γ Γ' →
    TypedBody constTy Γ' b (ρ.applySubst θ)
  | _, _, _, .empty, _, _, _ => by
      simp only [Row.applySubst]; exact .empty
  | _, _, _, .field h, θ, _, hmap => by
      simp only [Row.applySubst]
      exact .field (typed_applySubst_aux h hmap)
  | _, _, _, .cat h₁ h₂, θ, _, hmap => by
      simp only [Row.applySubst]
      exact .cat (typedBody_applySubst_aux h₁ hmap)
               (typedBody_applySubst_aux h₂ hmap)
end

------------------------- GENERALIZATION COROLLARIES ---------------------------

-- If θ fixes every outward-visible variable of Γ's schemes, typing transports
-- along θ within the *same* context: Γ ⊢ e : τ  ⟹  Γ ⊢ e : θτ.
theorem typed_applySubst {B C : Type} {constTy : C → B} {Γ : Ctx B}
    {e : Expr C} {τ : Ty B} {θ : TySubst B}
    (h : Typed constTy Γ e τ) (hrow : Γ.rowEnv = [])
    (hfix : ∀ x σ, Γ.lookup x = some σ → ∀ α ∈ σ.body.ftv, α ∉ σ.vars →
      θ.ty α = .var α ∧ θ.row α = .var α) :
    Typed constTy Γ e (τ.applySubst θ) := by
  refine typed_applySubst_aux h ⟨hrow, hrow, fun x σ hx => ⟨σ, hx, ?_⟩⟩
  -- every scheme covers itself: χ agrees with θ outside the binders, and θ is
  -- the identity there, so the composite is again an instantiation
  rintro χ hagree τ' ⟨θ₁, ⟨hfixTy, hfixRow⟩, rfl⟩
  refine ⟨⟨fun α => if α ∈ σ.vars then (θ₁.ty α).applySubst χ else .var α,
           fun α => if α ∈ σ.vars then (θ₁.row α).applySubst χ else .var α⟩,
    ⟨fun α hα => by simp [hα], fun α hα => by simp [hα]⟩, ?_⟩
  rw [Ty.applySubst_applySubst]
  apply Ty.applySubst_congr
  intro α hα
  by_cases hv : α ∈ σ.vars
  · constructor <;> simp [TySubst.comp, hv]
  · constructor
    · simp [TySubst.comp, hv, hfixTy α hv, Ty.applySubst,
            (hagree α hα hv).1, (hfix x σ hx α hα hv).1]
    · simp [TySubst.comp, hv, hfixRow α hv, Row.applySubst,
            (hagree α hα hv).2, (hfix x σ hx α hα hv).2]

-- Admissibility of the standard, syntactic HM generalization rule: ONE
-- derivation of the binding, plus ᾱ ∩ ftv(Γ) = ∅ (phrased through lookups),
-- discharges the instance-closed premise of T-let.
theorem tLet_syntactic {B C : Type} {constTy : C → B} {Γ : Ctx B} {x : Var}
    {e₁ e₂ : Expr C} {τ₁ τ₂ : Ty B} {ᾱ : List TyVar}
    (hrow : Γ.rowEnv = [])
    (hfresh : ∀ α ∈ ᾱ, ∀ y σ, Γ.lookup y = some σ → α ∈ σ.body.ftv → α ∈ σ.vars)
    (h₁ : Typed constTy Γ e₁ τ₁)
    (h₂ : Typed constTy (Γ.bindScheme x ⟨ᾱ, τ₁⟩) e₂ τ₂) :
    Typed constTy Γ (.letE x e₁ e₂) τ₂ := by
  refine .tLet (σ := ⟨ᾱ, τ₁⟩) (fun τ' hi => ?_) h₂
  obtain ⟨θ, ⟨hty, hrw⟩, rfl⟩ := hi
  refine typed_applySubst h₁ hrow (fun y σ hy α hα hnv => ?_)
  by_cases hin : α ∈ ᾱ
  · exact absurd (hfresh α hin y σ hy hα) hnv
  · exact ⟨hty α hin, hrw α hin⟩

-- The regression example, rederived through the syntactic rule: the manual
-- three-way case split on the lookup now lives once and for all inside the
-- substitution lemma (tSelUnk case of typed_applySubst_aux).
example {B C : Type} (constTy : C → B) :
    Typed constTy Ctx.empty
      (.letE "f" (.lam "x" (.sel (.var "x") "l"))
        (.app (.var "f") (.rcd .empty)))
      .unk := by
  refine tLet_syntactic (τ₁ := .fn (.rcd (.var "β")) .unk) (ᾱ := ["β"])
    rfl ?_ ?_ ?_
  · intro α hα y σ hy
    simp [Ctx.lookup, Ctx.empty] at hy
  · -- the single symbolic derivation: λx. x.l : {β} → ★  (lookup is ?)
    apply Typed.tLam
    refine .tSelUnk (ρ := .var "β") (.tVar ?_ (Scheme.Inst.refl _)) (.varFree ?_)
    · simp [Ctx.lookup_bindTy]
    · simp [Ctx.lookupRow, Ctx.bindTy, Ctx.bindScheme, Ctx.empty]
  · -- the body instantiates f at β ≔ ε and applies it to {}
    have hif : Scheme.Inst (⟨["β"], .fn (.rcd (.var "β")) .unk⟩ : Scheme B)
        (.fn (.rcd .empty) .unk) := by
      refine ⟨⟨fun α => .var α, fun α => if α == "β" then .empty else .var α⟩,
        ⟨fun _ _ => rfl, fun α hα => ?_⟩, ?_⟩
      · simp only [List.mem_singleton] at hα
        simp [hα]
      · simp [Ty.applySubst, Row.applySubst]
    exact .tApp (.tVar (by simp [Ctx.lookup_bindScheme]) hif) (.tRcd .empty)

end MinimalCalculus
