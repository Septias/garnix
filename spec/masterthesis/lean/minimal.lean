-- Lean 4 formalization of masterthesis/minimal.typ
-- Minimal Calculus: functions, scoped records, record concat, row-vars,
-- row equivalence, three-way row lookup (τ | ⊥ | ?)

namespace MinimalCalculus

---------------------------------- TERMS -----------------------------------
--  l ∈ 𝓛   x ∈ 𝓧   𝓫 ∈ 𝓑   c ∈ 𝓒
abbrev Label  := String
abbrev Var    := String
abbrev TyVar  := String

--   e := c | x | (x: e) | e₁e₂ | e₁ ‖ e₂ | e.l | { ξ }
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


------------------------------------ CONTEXT -----------------------------------

structure Ctx (B : Type) where
  tyEnv  : List (Var × Ty B)     -- x: τ
  rowEnv : List (TyVar × Row B)  -- α = ρ  (solved row-vars, consulted by L-α)

namespace Ctx

def empty : Ctx B := ⟨[], []⟩

def lookup (Γ : Ctx B) (x : Var) : Option (Ty B) :=
  (Γ.tyEnv.find? (·.1 == x)).map (·.2)

def lookupRow (Γ : Ctx B) (α : TyVar) : Option (Row B) :=
  (Γ.rowEnv.find? (·.1 == α)).map (·.2)

-- Γ · (x: τ)
def bindTy (Γ : Ctx B) (x : Var) (τ : Ty B) : Ctx B :=
  { Γ with tyEnv := (x, τ) :: Γ.tyEnv }

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


------------------------------- TYPING RELATION --------------------------------
--   Γ ⊢ e : τ
--   `constTy : C → B` assigns each constant its base type.

mutual
  inductive Typed {B C : Type} (constTy : C → B) :
      Ctx B → Expr C → Ty B → Prop where

    -- ----------- T-cons
    -- Γ ⊢ c : 𝓫_c
    | tCon : Typed constTy Γ (.con c) (.base (constTy c))

    -- x : τ ∈ Γ
    -- ----------- T-var
    -- Γ ⊢ x : τ
    | tVar : Γ.lookup x = some τ → Typed constTy Γ (.var x) τ

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
-- ≈ₜ: peel the tEq layers, collecting them with transitivity.

-- Combined so the recursion (peeling tEq) runs over variable indices only,
-- which is what Lean's structural recursion over inductive families needs.
private theorem typed_inv_aux {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    (∀ {c : C}, e = .con c → TyEquiv (.base (constTy c)) τ) ∧
    (∀ {x : Var} {e' : Expr C}, e = .lam x e' →
      ∃ τ₁ τ₂, TyEquiv (.fn τ₁ τ₂) τ ∧ Typed constTy (Γ.bindTy x τ₁) e' τ₂) ∧
    (∀ {b : RecBody (Expr C)}, e = .rcd b →
      ∃ ρ, TyEquiv (.rcd ρ) τ ∧ TypedBody constTy Γ b ρ)
  | _, _, _, .tCon =>
      ⟨(fun h => by cases h; exact .refl _),
       (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tVar _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tEq h heq =>
      have ih := typed_inv_aux h
      ⟨(fun hc => (ih.1 hc).trans heq),
       (fun hl => match ih.2.1 hl with
         | ⟨_, _, he, hb⟩ => ⟨_, _, he.trans heq, hb⟩),
       (fun hr => match ih.2.2 hr with
         | ⟨_, he, hb⟩ => ⟨_, he.trans heq, hb⟩)⟩
  | _, _, _, .tLam h =>
      ⟨(fun hc => nomatch hc),
       (fun hl => by cases hl; exact ⟨_, _, .refl _, h⟩),
       (fun hr => nomatch hr)⟩
  | _, _, _, .tApp _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tCat _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tSel _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tSelUnk _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .tRcd h =>
      ⟨(fun hc => nomatch hc), (fun hl => nomatch hl),
       (fun hr => by cases hr; exact ⟨_, .refl _, h⟩)⟩

theorem typed_con_inv {B C : Type} {constTy : C → B} {Γ : Ctx B} {c : C}
    {τ : Ty B}
    (h : Typed constTy Γ (.con c) τ) : TyEquiv (.base (constTy c)) τ :=
  (typed_inv_aux h).1 rfl

theorem typed_lam_inv {B C : Type} {constTy : C → B} {Γ : Ctx B} {x : Var}
    {e : Expr C} {τ : Ty B}
    (h : Typed constTy Γ (.lam x e) τ) :
    ∃ τ₁ τ₂, TyEquiv (.fn τ₁ τ₂) τ ∧ Typed constTy (Γ.bindTy x τ₁) e τ₂ :=
  (typed_inv_aux h).2.1 rfl

theorem typed_rcd_inv {B C : Type} {constTy : C → B} {Γ : Ctx B}
    {b : RecBody (Expr C)} {τ : Ty B}
    (h : Typed constTy Γ (.rcd b) τ) :
    ∃ ρ, TyEquiv (.rcd ρ) τ ∧ TypedBody constTy Γ b ρ :=
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
  (∀ x τ, Γ₁.lookup x = some τ → Γ₂.lookup x = some τ) ∧
  (∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α)

theorem Ctx.lookup_bindTy {B : Type} (Γ : Ctx B) (x y : Var) (τ : Ty B) :
    (Γ.bindTy x τ).lookup y = if x == y then some τ else Γ.lookup y := by
  simp only [Ctx.lookup, Ctx.bindTy, List.find?_cons]
  cases hxy : (x == y) <;> simp_all

theorem Ctx.Sub.refl {B : Type} (Γ : Ctx B) : Ctx.Sub Γ Γ :=
  ⟨fun _ _ h => h, fun _ => rfl⟩

theorem Ctx.Sub.trans {B : Type} {Γ₁ Γ₂ Γ₃ : Ctx B}
    (h₁ : Ctx.Sub Γ₁ Γ₂) (h₂ : Ctx.Sub Γ₂ Γ₃) : Ctx.Sub Γ₁ Γ₃ :=
  ⟨fun x τ h => h₂.1 x τ (h₁.1 x τ h), fun α => (h₁.2 α).trans (h₂.2 α)⟩

-- Binding respects the preorder.
theorem Ctx.Sub.bindTy {B : Type} {Γ₁ Γ₂ : Ctx B} (h : Ctx.Sub Γ₁ Γ₂)
    (x : Var) (τ : Ty B) : Ctx.Sub (Γ₁.bindTy x τ) (Γ₂.bindTy x τ) := by
  refine ⟨fun y σ hy => ?_, h.2⟩
  rw [Ctx.lookup_bindTy] at hy ⊢
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact h.1 y σ hy
  · simpa [hxy] using hy

-- Exchange: distinct bindings commute.
theorem Ctx.Sub.exchange {B : Type} (Γ : Ctx B) {x y : Var} (hne : x ≠ y)
    (τ σ : Ty B) :
    Ctx.Sub ((Γ.bindTy x τ).bindTy y σ) ((Γ.bindTy y σ).bindTy x τ) := by
  refine ⟨fun z μ hz => ?_, fun _ => rfl⟩
  simp only [Ctx.lookup_bindTy] at hz ⊢
  cases hyz : (y == z) <;> cases hxz : (x == z) <;>
    simp only [hyz, hxz, Bool.false_eq_true, if_false, if_true] at hz ⊢ <;>
    try exact hz
  exact absurd ((eq_of_beq hxz).trans (eq_of_beq hyz).symm) hne

-- Shadowing: rebinding x hides whatever τ₁ the lower context knew about x.
theorem Ctx.Sub.shadowed {B : Type} {Δ Γ : Ctx B} {x : Var} {τ₁ : Ty B}
    (h : Ctx.Sub Δ (Γ.bindTy x τ₁)) (σ : Ty B) :
    Ctx.Sub (Δ.bindTy x σ) (Γ.bindTy x σ) := by
  refine ⟨fun z μ hz => ?_, fun α => h.2 α⟩
  rw [Ctx.lookup_bindTy] at hz ⊢
  cases hxz : (x == z)
  · simp only [hxz, Bool.false_eq_true, if_false] at hz ⊢
    have := h.1 z μ hz
    rwa [Ctx.lookup_bindTy, hxz, if_neg (by simp)] at this
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
  | _, _, _, _, hs, .tVar h      => .tVar (hs.1 _ _ h)
  | _, _, _, _, hs, .tEq h heq   => .tEq (typed_sub hs h) heq
  | _, _, _, _, hs, .tLam h      => .tLam (typed_sub (hs.bindTy _ _) h)
  | _, _, _, _, hs, .tApp h₁ h₂  => .tApp (typed_sub hs h₁) (typed_sub hs h₂)
  | _, _, _, _, hs, .tCat h₁ h₂  => .tCat (typed_sub hs h₁) (typed_sub hs h₂)
  | _, _, _, _, hs, .tSel h hl   =>
      .tSel (typed_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .tSelUnk h hl =>
      .tSelUnk (typed_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
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
  | con => cases (typed_con_inv ht).base_inv.symm.trans rfl
  | lam => exact ⟨_, _, rfl⟩
  | rcd =>
      obtain ⟨ρ, he, -⟩ := typed_rcd_inv ht
      obtain ⟨ρ', hσ, -⟩ := he.rcd_inv
      cases hσ

theorem canonical_rcd {B C : Type} {constTy : C → B} {Γ : Ctx B} {v : Expr C}
    {ρ : Row B}
    (hv : Value v) (ht : Typed constTy Γ v (.rcd ρ)) :
    ∃ b ρ', v = .rcd b ∧ RowEquiv ρ' ρ ∧ TypedBody constTy Γ b ρ' := by
  cases hv with
  | con => cases typed_con_inv ht |>.base_inv
  | lam =>
      obtain ⟨τ₁, τ₂, he, -⟩ := typed_lam_inv ht
      obtain ⟨σ₁, σ₂, hσ, -⟩ := he.fn_inv
      cases hσ
  | rcd =>
      obtain ⟨ρ', he, hb⟩ := typed_rcd_inv ht
      obtain ⟨ρ'', hσ, heq⟩ := he.rcd_inv
      cases hσ
      exact ⟨_, _, rfl, heq, hb⟩


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
  | .tVar h      => by subst hΓ; simp [Ctx.lookup, Ctx.empty] at h
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


---------------------------------- PRESERVATION ---------------------------------
-- ## Substitution lemma  (key auxiliary for Preservation)
--
--   If Γ, x:τ₁ ⊢ e : τ₂  and  ⊢ v : τ₁ (v closed over Γ's row-solutions)
--   then  Γ ⊢ e[x:=v] : τ₂
--
-- v must be *closed*: with named binders, pushing an open v under a λ could
-- capture its free variables. Closed v suffices for preservation of closed
-- programs (β only fires at ∅), and dodges all renaming machinery.
--
-- The recursion is over the derivation of e, but its context changes shape
-- under λ (exchange/shadowing), so the statement is generalized to any Δ
-- Sub-below Γ,x:τ₁ — keeping the derivation indices variable, which is what
-- structural recursion over an inductive family needs.

mutual
private theorem subst_aux {B C : Type} {constTy : C → B} :
    {Δ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Δ e τ →
    ∀ {Γ : Ctx B} {x : Var} {v : Expr C} {τ₁ : Ty B},
      Ctx.Sub Δ (Γ.bindTy x τ₁) →
      Typed constTy ⟨[], Γ.rowEnv⟩ v τ₁ →
      Typed constTy Γ (subst x v e) τ
  | _, _, _, .tCon, _, _, _, _, _, _ => .tCon
  | _, .var y, _, .tVar h, _, x, _, _, hsub, hv => by
      have hy := hsub.1 _ _ h
      rw [Ctx.lookup_bindTy] at hy
      simp only [subst]
      cases hxy : (x == y)
      · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
        exact .tVar hy
      · simp only [hxy, if_true] at hy ⊢
        exact Option.some.inj hy ▸ typed_sub (Ctx.Sub.ofEmptyTyEnv _) hv
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
  | _, _, _, .tRcd h, _, _, _, _, hsub, hv =>
      .tRcd (substBody_aux h hsub hv)

private theorem substBody_aux {B C : Type} {constTy : C → B} :
    {Δ : Ctx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    TypedBody constTy Δ b ρ →
    ∀ {Γ : Ctx B} {x : Var} {v : Expr C} {τ₁ : Ty B},
      Ctx.Sub Δ (Γ.bindTy x τ₁) →
      Typed constTy ⟨[], Γ.rowEnv⟩ v τ₁ →
      TypedBody constTy Γ (substBody x v b) ρ
  | _, _, _, .empty, _, _, _, _, _, _ => .empty
  | _, _, _, .field h, _, _, _, _, hsub, hv => .field (subst_aux h hsub hv)
  | _, _, _, .cat h₁ h₂, _, _, _, _, hsub, hv =>
      .cat (substBody_aux h₁ hsub hv) (substBody_aux h₂ hsub hv)
end

theorem subst_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (x : Var) (v : Expr C) (τ₁ τ₂ : Ty B) (e : Expr C)
    (hv : Typed constTy { Γ with tyEnv := [] } v τ₁)
    (he : Typed constTy (Γ.bindTy x τ₁) e τ₂) :
    Typed constTy Γ (subst x v e) τ₂ :=
  subst_aux he (Ctx.Sub.refl _) hv

--   If ∅ ⊢ e : τ  and  e → e'  then  ∅ ⊢ e' : τ
--   Stated for closed programs (β needs a closed argument, see above).
--   Key steps: selVal needs term/type lookup agreement carried across ≈ᵣ;
--   the tSelUnk/selVal case is vacuous — record literals have spine-var-free
--   rows, so their lookups are never ?.

private theorem preservation_aux {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    Γ = Ctx.empty → ∀ {e' : Expr C}, Step e e' → Typed constTy Γ e' τ
  | _, _, _, .tCon,   _, _ => (nomatch ·)
  | _, _, _, .tVar _, _, _ => (nomatch ·)
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
          obtain ⟨σ₁, σ₂, heq, hbody⟩ := typed_lam_inv h₁
          obtain ⟨τ₁', τ₂', hfn, he₁, he₂⟩ := heq.fn_inv
          cases hfn
          -- retype the argument at the λ's domain, then substitute
          exact .tEq
            (subst_preserves_typing _ _ _ _ _ _ _
              (.tEq h₂ he₁.symm) hbody)
            he₂
  | _, _, _, .tCat h₁ h₂, hΓ, _ => fun hs => by
      cases hs with
      | catLeft s    => exact .tCat (preservation_aux h₁ hΓ s) h₂
      | catRight v s => exact .tCat h₁ (preservation_aux h₂ hΓ s)
      | catVal =>
          obtain ⟨ρ₁', he₁, hb₁⟩ := typed_rcd_inv h₁
          obtain ⟨ρ₂', he₂, hb₂⟩ := typed_rcd_inv h₂
          obtain ⟨_, hσ₁, hr₁⟩ := he₁.rcd_inv
          obtain ⟨_, hσ₂, hr₂⟩ := he₂.rcd_inv
          cases hσ₁; cases hσ₂
          exact .tEq (.tRcd (.cat hb₂ hb₁)) (.rcd (.cat hr₂ hr₁))
  | _, _, _, .tSel h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .tSel (preservation_aux h hΓ s) hl
      | selVal hbl =>
          obtain ⟨ρ', he, hb⟩ := typed_rcd_inv h
          obtain ⟨_, hσ, hr⟩ := he.rcd_inv
          cases hσ
          obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
          cases hre with
          | found hty =>
              obtain ⟨e'', hbl', hte⟩ := TypedBody.lookup_found hb hl'
              rw [hbl] at hbl'
              exact Option.some.inj hbl' ▸ .tEq hte hty.symm
  | _, _, _, .tSelUnk h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .tSelUnk (preservation_aux h hΓ s) hl
      | selVal hbl =>
          -- vacuous: the literal's row is spine-var-free, its lookup can't be ?
          obtain ⟨ρ', he, hb⟩ := typed_rcd_inv h
          obtain ⟨_, hσ, hr⟩ := he.rcd_inv
          cases hσ
          obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
          cases hre
          exact (Lookup.not_unknown_of_spineVarFree hb.spineVarFree hl').elim

theorem preservation
    {B C : Type} (constTy : C → B)
    (e e' : Expr C) (τ : Ty B)
    (ht : Typed constTy Ctx.empty e τ)
    (hs : Step e e') :
    Typed constTy Ctx.empty e' τ :=
  preservation_aux ht rfl hs

end MinimalCalculus
