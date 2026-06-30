-- Lean 4 formalization of masterthesis/minimal.typ
-- Minimal Calculus: functions, scoped records, ∈-constraints, record concat, row-vars

namespace MinimalCalculus


--  l ∈ 𝓛   x ∈ 𝓧   𝓫 ∈ 𝓑   c ∈ 𝓒
abbrev Label  := String
abbrev Var    := String
abbrev TyVar  := String

-- A finite set of labels, represented as a duplicate-free list
abbrev LabelSet := List Label

def LabelSet.mem  (l : Label) (S : LabelSet)  : Bool     := S.contains l
def LabelSet.sub  (S : LabelSet) (l : Label)  : LabelSet := S.erase l
def LabelSet.sing (l : Label)                 : LabelSet := [l]

--   a, b, e := c | (x: e) | a ‖ b | e.l | { ξ }
--       ξ   := ε | l = b | (l = b | ξ)

inductive RecBody (Term : Type) : Type where
  | empty  : RecBody Term
  | single : Label → Term → RecBody Term
  | ext    : Label → Term → RecBody Term → RecBody Term

inductive Expr (Const : Type) : Type where
  | con  : Const → Expr Const                            -- c
  | lam  : Var → Expr Const → Expr Const                 -- (x: e)
  | app  : Expr Const → Expr Const → Expr Const          -- e₁ e₂
  | cat  : Expr Const → Expr Const → Expr Const          -- a ‖ b
  | sel  : Expr Const → Label → Expr Const               -- e.l
  | rcd  : RecBody (Expr Const) → Expr Const             -- { ξ }

-- τ := α | 𝓫 | ★ | τ → τ | { ρ }
-- ρ := ε | α | l: τ | (l: τ | ρ)

mutual
  inductive Ty (B : Type) : Type where
    | var  : TyVar → Ty B                         -- α
    | base : B → Ty B                             -- 𝓫
    | err  : Ty B                                 -- ★
    | fn   : Ty B → Ty B → Ty B                   -- τ → τ
    | rcd  : Row B → Ty B                         -- { ρ }

  inductive Row (B : Type) : Type where
    | empty  : Row B                              -- ε
    | var    : TyVar → Row B                      -- α
    | field  : Label → Ty B → Row B               -- l: τ
    | ext    : Label → Ty B → Row B → Row B       -- l: τ | ρ
    | cat    : Row B → Row B → Row B              -- ρ₁ | ρ₂  (for T-conc result)
end

-- ## Context
--
--   π := x ∈ (S₁ ∪ … ∪ Sₙ)
--
--   Simplified: each constraint binds one variable to one allowed label set.
--   The full union-of-sets form from the paper is captured by having multiple
--   constraints per variable; simplification merges / drops them (see §Spec).

structure Constr where
  x       : Var
  allowed : LabelSet   -- treated as a set of allowed labels

structure Ctx (B : Type) where
  tyEnv   : List (Var × Ty B)
  constrs : List Constr

namespace Ctx

def empty : Ctx B := ⟨[], []⟩

def lookup (Γ : Ctx B) (x : Var) : Option (Ty B) :=
  (Γ.tyEnv.find? (·.1 == x)).map (·.2)

-- Γ, x: τ
def bindTy (Γ : Ctx B) (x : Var) (τ : Ty B) : Ctx B :=
  { Γ with tyEnv := (x, τ) :: Γ.tyEnv }

-- Γ · (x ∈ S)
def bindConstr (Γ : Ctx B) (c : Constr) : Ctx B :=
  { Γ with constrs := c :: Γ.constrs }

end Ctx

-- ## Free-variable membership  (x ∈ e)
-- TODO

-- ## Typing relation
--
--   Γ ⊢ e : τ
--
--   `constTy : C → B` assigns each constant its base type.

inductive Typed {B C : Type} (constTy : C → B) :
    Ctx B → Expr C → Ty B → Prop where

  -- ----------- T-cons
  -- Γ ⊢ c : 𝓫_c
  | tCon (Γ : Ctx B) (c : C) :
      Typed constTy Γ (.con c) (.base (constTy c))

  -- Γ x:τ₁ ⊢ e : τ₂
  -- --------------------- T-λ-I
  -- Γ ⊢ (x: e) : τ₁ → τ₂
  | tLam (Γ : Ctx B) (x : Var) (e : Expr C) (τ₁ τ₂ : Ty B) :
      Typed constTy (Γ.bindTy x τ₁) e τ₂ →
      Typed constTy Γ (.lam x e) (.fn τ₁ τ₂)

  -- Γ ⊢ e₁ : τ₁→τ₂   Γ ⊢ e₂ : τ₁
  -- -------------------------------- T-λ-E
  -- Γ ⊢ e₁ e₂ : τ₂
  | tApp (Γ : Ctx B) (e₁ e₂ : Expr C) (τ₁ τ₂ : Ty B) :
      Typed constTy Γ e₁ (.fn τ₁ τ₂) →
      Typed constTy Γ e₂ τ₁ →
      Typed constTy Γ (.app e₁ e₂) τ₂

  -- x ∈ e₂    Γ ⊢ (x: e₁) e₂ : τ
  -- ---------------------------------- T-λ∈-ok
  -- Γ · (x ∈ S) ⊢ (x: e₁) e₂ : τ
  --
  -- The constraint is already satisfied — x is used, so it is "in scope".
  | tInOk (Γ : Ctx B) (x : Var) (e₁ e₂ : Expr C) (τ : Ty B) (S : LabelSet) :
      -- freeIn x e₂ = true →
      Typed constTy Γ (.app (.lam x e₁) e₂) τ →
      Typed constTy (Γ.bindConstr ⟨x, S⟩) (.app (.lam x e₁) e₂) τ

  -- x ∉ e₂    S' = S \ {x}    Γ · (x ∈ S') ⊢ (x: e₁) e₂ : τ
  -- ------------------------------------------------------------- T-λ∈-not-in
  -- Γ · (x ∈ S) ⊢ (x: e₁) e₂ : τ
  --
  -- x is absent from e₂, so we remove x from the allowed set and recurse.
  -- (Label = Var = String, so LabelSet.sub S x is well-typed.)
  | tInNot (Γ : Ctx B) (x : Var) (e₁ e₂ : Expr C) (τ : Ty B) (S : LabelSet) :
      -- freeIn x e₂ = false →
      Typed constTy (Γ.bindConstr ⟨x, S.sub x⟩) (.app (.lam x e₁) e₂) τ →
      Typed constTy (Γ.bindConstr ⟨x, S⟩) (.app (.lam x e₁) e₂) τ

  -- x ∉ e₂
  -- --------------------------------- T-λ∈-err
  -- Γ · (x ∈ {x}) ⊢ (x: e₁) e₂ : ★
  --
  -- S was exactly {x} and x is not used — the only allowed label is exhausted.
  | tInErr (Γ : Ctx B) (x : Var) (e₁ e₂ : Expr C) :
      -- freeIn x e₂ = false →
      Typed constTy (Γ.bindConstr ⟨x, LabelSet.sing x⟩)
                    (.app (.lam x e₁) e₂) .err

  -- Γ ⊢ a : {ρ₁}   Γ ⊢ b : {ρ₂}
  -- ------------------------------- T-conc
  -- Γ ⊢ a ‖ b : {ρ₁ | ρ₂}
  | tCat (Γ : Ctx B) (a b : Expr C) (ρ₁ ρ₂ : Row B) :
      Typed constTy Γ a (.rcd ρ₁) →
      Typed constTy Γ b (.rcd ρ₂) →
      Typed constTy Γ (.cat a b) (.rcd (.cat ρ₁ ρ₂))

  -- Γ ⊢ e : {l: τ | ρ}
  -- -------------------- T-sel
  -- Γ ⊢ e.l : τ
  | tSel (Γ : Ctx B) (e : Expr C) (l : Label) (τ : Ty B) (ρ : Row B) :
      Typed constTy Γ e (.rcd (.ext l τ ρ)) →
      Typed constTy Γ (.sel e l) τ

-- ## Specialization  x ⩪ Γ
--
--   Simplifies constraints when variable x resolves to a known label.
--
--   (x, l ∈ (X ∪ Y))  →  l ∈ Y         when l ∉ X
--   (x, l ∈ (X ∪ Y))  →  ε             when l ∈ X
--   (x, l ∈ (X ∪ Y))  →  l ∈ (X ∪ Y)  otherwise
--
--   Full specialization requires knowing the X / Y decomposition of `allowed`.
--   That structure is not tracked in our `Constr` type as stated — a richer
--   representation (e.g. a list of sets whose union is `allowed`) is needed.
--   We leave this as a placeholder.

end MinimalCalculus
