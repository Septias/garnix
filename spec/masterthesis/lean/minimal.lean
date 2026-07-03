-- Lean 4 formalization of masterthesis/minimal.typ
-- Minimal Calculus: functions, scoped records, ∈-constraints, record concat, row-vars

namespace MinimalCalculus

---------------------------------- SYNTAX-----------------------------------
--  l ∈ 𝓛   x ∈ 𝓧   𝓫 ∈ 𝓑   c ∈ 𝓒
abbrev Label  := String
abbrev Var    := String
abbrev TyVar  := String

-- A finite set of labels, represented as a duplicate-free list
abbrev LabelSet := List Label

def LabelSet.mem  (l : Label) (S : LabelSet)  : Bool     := S.contains l
def LabelSet.sub  (S : LabelSet) (l : Label)  : LabelSet := S.erase l
def LabelSet.sing (l : Label)                 : LabelSet := [l]

--   a, b, e := c | x | (x: e) | a ‖ b | e.l | { ξ }
--       ξ   := ε | (l = b | ξ)

inductive RecBody (Term : Type) : Type where
  | empty  : RecBody Term
  | ext    : Label → Term → RecBody Term → RecBody Term

def RecBody.lookup {α : Type} (l : Label) : RecBody α → Option α
  | .empty      => none
  | .ext l' e b  => if l == l' then some e else RecBody.lookup l b

def RecBody.concat {α : Type} : RecBody α → RecBody α → RecBody α
  | .empty,     b => b
  | .ext l e a, b => .ext l e (RecBody.concat a b)

inductive Expr (Const : Type) : Type where
  | con  : Const → Expr Const                            -- c
  | var  : Var → Expr Const                              -- x
  | lam  : Var → Expr Const → Expr Const                 -- (x: e)
  | app  : Expr Const → Expr Const → Expr Const          -- e₁ e₂
  | cat  : Expr Const → Expr Const → Expr Const          -- a ‖ b
  | sel  : Expr Const → Label → Expr Const               -- e.l
  | rcd  : RecBody (Expr Const) → Expr Const             -- { ξ }



---------------------------------- TYPES-----------------------------------

-- τ := α | 𝓫 | ★ | τ → τ | { ρ }
-- ρ := ε | α | l: τ | (l: τ | ρ)

mutual
  inductive Ty (B : Type) : Type where
    | var  : TyVar → Ty B                         -- α
    | base : B → Ty B                             -- 𝓫
    | unk  : Ty B                                 -- ★
    | fn   : Ty B → Ty B → Ty B                   -- τ → τ
    | rcd  : Row B → Ty B                         -- { ρ }

  inductive Row (B : Type) : Type where
    | empty  : Row B                              -- ε
    | var    : TyVar → Row B                      -- α
    | ext    : Label → Ty B → Row B               -- l: τ
    | cat    : Row B → Row B → Row B              -- ρ₁ | ρ₂  (for T-conc result)
end

----------------------------------- CONTEXT ----------------------------------------
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


--------------------------------- TYPING RELATION ------------------------------
--   Γ ⊢ e : τ
--   `constTy : C → B` assigns each constant its base type.

mutual
  inductive Typed {B C : Type} (constTy : C → B) :
      Ctx B → Expr C → Ty B → Prop where

    -- ----------- T-cons
    -- Γ ⊢ c : 𝓫_c
    | tCon (Γ : Ctx B) (c : C):
        Typed constTy Γ (.con c) (.base (constTy c))

    -- x : τ ∈ Γ
    -- ----------- T-var
    -- Γ ⊢ x : τ
    | tVar (Γ : Ctx B) (x : Var) (τ : Ty B) :
        Γ.lookup x = some τ →
        Typed constTy Γ (.var x) τ

    -- Γ x:τ₁ ⊢ e : τ₂
    -- --------------------- T-λ-I
    -- Γ ⊢ (x: e) : τ₁ → τ₂
    | tLam (Γ : Ctx B) (x : Var) (e : Expr C) (τ₁ τ₂ : Ty B):
        Typed constTy (Γ.bindTy x τ₁) e τ₂ →
        Typed constTy Γ (.lam x e) (.fn τ₁ τ₂)

    -- Γ ⊢ e₁ : τ₁ → τ₂   Γ ⊢ e₂ : τ₁
    -- -------------------------------- T-λ-E
    -- Γ ⊢ e₁ e₂ : τ₂
    | tApp (Γ : Ctx B) (e₁ e₂ : Expr C) (τ₁ τ₂ : Ty B):
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
    | tSel (Γ : Ctx B) (e : Expr C) (l : Label) (τ : Ty B) :
        Typed constTy Γ e (.rcd (.ext l τ)) →
        Typed constTy Γ (.sel e l) τ

    -- TypedBody Γ ξ ρ
    -- -------------------- T-rcd
    -- Γ ⊢ { ξ } : { ρ }
    | tRcd (Γ : Ctx B) (b : RecBody (Expr C)) (ρ : Row B) :
        TypedBody constTy Γ b ρ →
        Typed constTy Γ (.rcd b) (.rcd ρ)

  -- ## Typing relation for record bodies
  --
  --   TypedBody Γ ξ ρ
  inductive TypedBody {B C : Type} (constTy : C → B) :
      Ctx B → RecBody (Expr C) → Row B → Prop where

    -- -------------------- T-rcd-empty
    -- TypedBody Γ ε ε
    | empty (Γ : Ctx B) :
        TypedBody constTy Γ .empty .empty

    -- Γ ⊢ e : τ    TypedBody Γ ξ ρ
    -- ------------------------------------ T-rcd-ext
    -- TypedBody Γ (l = e | ξ) (l: τ | ρ)
    | ext (Γ : Ctx B) (l : Label) (e : Expr C) (b : RecBody (Expr C)) (τ : Ty B) (ρ : Row B) :
        Typed constTy Γ e τ →
        TypedBody constTy Γ b ρ →
        TypedBody constTy Γ (.ext l e b) (.cat (.ext l τ) ρ)
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
    | .empty     => .empty
    | .ext l e b => .ext l (subst x v e) (substBody x v b)
end

--------------------------------- VALUES --------------------------------------
-- Lazy semantics: a record is a value at the constructor level (WHNF).
-- Fields are unevaluated thunks and are forced only on projection.

inductive Value {C : Type} : Expr C → Prop where
  | con {c : C}                : Value (.con c)
  | lam {x : Var} {e : Expr C} : Value (.lam x e)
  | rcd {b : RecBody (Expr C)} : Value (.rcd b)

-- ValueBody (eager variant, kept for reference): all fields are fully forced.
inductive ValueBody {C : Type} : RecBody (Expr C) → Prop where
  | empty  : ValueBody .empty
  | ext    {l : Label} {e : Expr C} {b : RecBody (Expr C)}
           : Value e → ValueBody b → ValueBody (.ext l e b)


--------------------------------- REDUCTION ------------------------------------
-- e ↦ e'

mutual
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
    | selVal {b : RecBody (Expr C)} {l : Label} {v : Expr C} :
        ValueBody b →
        RecBody.lookup l b = some v →
        Step (.sel (.rcd b) l) v
    | rcdStep {b b' : RecBody (Expr C)} :
        StepBody b b' →
        Step (.rcd b) (.rcd b')
    | catLeft {a a' b : Expr C} :
        Step a a' →
        Step (.cat a b) (.cat a' b)
    | catRight {a b b' : Expr C} :
        Value a →
        Step b b' →
        Step (.cat a b) (.cat a b')
    | catVal {b₁ b₂ : RecBody (Expr C)} :
        ValueBody b₁ →
        ValueBody b₂ →
        Step (.cat (.rcd b₁) (.rcd b₂)) (.rcd (b₁.concat b₂))

  inductive StepBody {C : Type} : RecBody (Expr C) → RecBody (Expr C) → Prop where
    | extHead {l : Label} {e e' : Expr C} {b : RecBody (Expr C)} :
        Step e e' →
        StepBody (.ext l e b) (.ext l e' b)
    | extTail {l : Label} {v : Expr C} {b b' : RecBody (Expr C)} :
        Value v →
        StepBody b b' →
        StepBody (.ext l v b) (.ext l v b')
end

---------------------------------- PROGRESS ---------------------------------
--   If ⊢ e : τ  then  e ∈ Value  ∨  ∃ e', e → e'
--   Proof: induction on the typing derivation.


theorem progress
    {B C : Type} (constTy : C → B) (e : Expr C) (τ : Ty B)
    (h : Typed constTy Ctx.empty e τ) :
    Value e ∨ ∃ e', Step e e' := by
  sorry


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
--   Proof: induction on the typing derivation, case analysis on the step.

theorem preservation
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (e e' : Expr C) (τ : Ty B)
    (ht : Typed constTy Γ e τ)
    (hs : Step e e') :
    Typed constTy Γ e' τ := by
  sorry

-- ## Context weakening for constraints
--
--   Adding an irrelevant constraint does not affect typing.

theorem constr_weakening
    {B C : Type} (constTy : C → B)
    (Γ : Ctx B) (e : Expr C) (τ : Ty B) (c : Constr)
    (h : Typed constTy Γ e τ) :
    Typed constTy (Γ.bindConstr c) e τ := by
  sorry

end MinimalCalculus
