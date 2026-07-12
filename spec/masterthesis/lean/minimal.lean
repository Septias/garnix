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

-- Totality: ¿ NOT provable by structural induction — L-α recurses through
-- Γ's row-solutions, so this needs an acyclicity/idempotence condition on
-- rowEnv (rows in the range mention no vars in the domain).
theorem lookup_total {B : Type} (Γ : Ctx B) (ρ : Row B) (l : Label) :
    ∃ r, Lookup Γ ρ l r := by
  sorry

-- Result equivalence: found-types match up to ≈, ⊥/? on the nose.
inductive ResEquiv {B : Type} : LookupRes B → LookupRes B → Prop where
  | found   : TyEquiv τ₁ τ₂ → ResEquiv (.found τ₁) (.found τ₂)
  | absent  : ResEquiv .absent .absent
  | unknown : ResEquiv .unknown .unknown

-- Lookup respects row equivalence. Calibration check for ≈: any stronger
-- (swapping equal labels) and this lemma breaks, any weaker and T-eq is useless.
theorem lookup_equiv {B : Type} {Γ : Ctx B} {ρ₁ ρ₂ : Row B} {l : Label}
    {r₁ : LookupRes B}
    (heq : RowEquiv ρ₁ ρ₂) (h : Lookup Γ ρ₁ l r₁) :
    ∃ r₂, Lookup Γ ρ₂ l r₂ ∧ ResEquiv r₁ r₂ := by
  sorry


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
--   If ⊢ e : τ  then  e ∈ Value  ∨  ∃ e', e → e'
--   ¿ With T-sel-★ plain progress is FALSE: an ★-typed selection can be stuck
--   when the label is absent at runtime. The honest statement is
--   "progress up to lookup-error, errors only at ★-typed redexes" (see plan.typ).

inductive Progress {C : Type} (e : Expr C) : Prop where
  | step : Step e e' → Progress e
  | done : Value e → Progress e

def progress {B C : Type} {constTy : C → B} {Γ : Ctx B} {e : Expr C} {τ : Ty B}
    (hΓ : Γ = Ctx.empty) (ht : Typed constTy Γ e τ) : Progress e :=
  match ht with
  | .tCon        => .done .con
  | .tVar h      => by subst hΓ; simp [Ctx.lookup, Ctx.empty] at h
  | .tEq h _     => progress hΓ h
  | .tLam _      => .done .lam
  | .tRcd _      => .done .rcd
  | .tSel _ _    => sorry   -- needs canonical forms (modulo T-eq) + term/type lookup agreement
  | .tSelUnk _ _ => sorry   -- ¿ stuck when the label is truly absent — see note above
  | .tApp _ _    => sorry
  | .tCat _ _    => sorry


---------------------------------- PRESERVATION ---------------------------------
-- ## Substitution lemma  (key auxiliary for Preservation)
--
--   If Γ, x:τ₁ ⊢ e : τ₂  and  Γ ⊢ v : τ₁  then  Γ ⊢ e[x:=v] : τ₂

theorem subst_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (x : Var) (v : Expr C) (τ₁ τ₂ : Ty B) (e : Expr C)
    (hv : Typed constTy Γ v τ₁)
    (he : Typed constTy (Γ.bindTy x τ₁) e τ₂) :
    Typed constTy Γ (subst x v e) τ₂ := by
  sorry

--   If Γ ⊢ e : τ  and  e → e'  then  Γ ⊢ e' : τ
--   Key steps: catVal needs lookup_equiv (flattening reassociates the row),
--   selVal needs agreement of RecBody.lookup with the Lookup relation.

theorem preservation
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (e e' : Expr C) (τ : Ty B)
    (ht : Typed constTy Γ e τ)
    (hs : Step e e') :
    Typed constTy Γ e' τ := by
  sorry

end MinimalCalculus
