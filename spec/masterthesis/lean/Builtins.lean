-- THE MACRONIX SIGNATURE. Constants (ints, strings, null), the builtins with
-- their types and δ-rules, and `macroSig_ok`: δ preserves types, so qProgress
-- and qPreservation hold for MacroNix programs.
--
-- Booleans are the literals `Expr.bool`; `&&`, `||`, `!=`, `>` and `>=` are
-- derived (`if`, `!`, `==`, `<` with swapped arguments). `+` on strings and
-- string interpolation are `strcat`. `==` compares ATOMIC
-- values only (constants, booleans, labels, the empty list) and fails on
-- records, functions and non-empty lists (plans/macronix.md Q3). A builtin's
-- δ fires only at its exact arity; a partial application is an eta-expansion.

import Qualified

namespace MinimalCalculus
namespace MacroNix

inductive Op where
  | add | sub | mul | div | lt | le | eq | not
  | concat | head | tail | elemAt | length | map | filter | foldl
  | toString | isAttrs | strcat
  deriving DecidableEq, Repr

inductive MConst where
  | int (n : Int)
  | str (s : String)
  | null
  | op (o : Op)
  deriving DecidableEq, Repr

abbrev E := Expr MConst
abbrev T := Ty String

def tInt : T := .base "int"
def tStr : T := .base "string"
def tBool : T := .base "bool"
def tA : T := .var "a"
def tB : T := .var "b"

/-- the builtins' types; every variable is generic -/
def Op.ty : Op → T
  | .add | .sub | .mul | .div => .fn tInt (.fn tInt tInt)
  | .lt | .le                 => .fn tInt (.fn tInt tBool)
  | .eq                       => .fn tA (.fn tA tBool)
  | .not                      => .fn tBool tBool
  | .concat                   => .fn (.list tA) (.fn (.list tA) (.list tA))
  | .head                     => .fn (.list tA) tA
  | .tail                     => .fn (.list tA) (.list tA)
  | .elemAt                   => .fn (.list tA) (.fn tInt tA)
  | .length                   => .fn (.list tA) tInt
  | .map                      => .fn (.fn tA tB) (.fn (.list tA) (.list tB))
  | .filter                   => .fn (.fn tA tBool) (.fn (.list tA) (.list tA))
  | .foldl                    => .fn (.fn tB (.fn tA tB)) (.fn tB (.fn (.list tA) tB))
  | .toString                 => .fn tInt tStr
  | .isAttrs                  => .fn tA tBool
  | .strcat                   => .fn tStr (.fn tStr tStr)

def int (n : Int) : E := .con (.int n)
def call (o : Op) (es : List E) : E := .prim (.op o) es

/-- `==` on atomic values -/
def atomEq : E → E → Option Bool
  | .con c, .con d   => some (c == d)
  | .bool a, .bool b => some (a == b)
  | .lab l, .lab m   => some (l == m)
  | .nil, .nil       => some true
  | .nil, .cons _ _  => some false
  | .cons _ _, .nil  => some false
  | _, _             => none

/-- δ, on argument VALUES; `none` is a run-time failure. One match per builtin,
so each case of `delta_pres` splits only its own arms. -/
def Op.delta : Op → List E → Option E
  | .add => fun | [.con (.int a), .con (.int b)] => some (int (a + b)) | _ => none
  | .sub => fun | [.con (.int a), .con (.int b)] => some (int (a - b)) | _ => none
  | .mul => fun | [.con (.int a), .con (.int b)] => some (int (a * b)) | _ => none
  | .div => fun
      | [.con (.int a), .con (.int b)] => if b = 0 then none else some (int (a / b))
      | _ => none
  | .lt => fun | [.con (.int a), .con (.int b)] => some (.bool (decide (a < b))) | _ => none
  | .le => fun | [.con (.int a), .con (.int b)] => some (.bool (decide (a ≤ b))) | _ => none
  | .eq => fun | [v, w] => (atomEq v w).map .bool | _ => none
  | .not => fun | [.bool b] => some (.bool !b) | _ => none
  | .concat => fun
      | [.nil, ys] => some ys
      | [.cons e es, ys] => some (.cons e (call .concat [es, ys]))
      | _ => none
  | .head => fun | [.cons e _] => some e | _ => none
  | .tail => fun | [.cons _ es] => some es | _ => none
  | .elemAt => fun
      | [.cons e es, .con (.int k)] =>
          if k = 0 then some e
          else if 0 < k then some (call .elemAt [es, int (k - 1)]) else none
      | _ => none
  | .length => fun
      | [.nil] => some (int 0)
      | [.cons _ es] => some (call .add [int 1, call .length [es]])
      | _ => none
  | .map => fun
      | [_, .nil] => some .nil
      | [f, .cons e es] => some (.cons (.app f e) (call .map [f, es]))
      | _ => none
  | .filter => fun
      | [_, .nil] => some .nil
      | [p, .cons e es] =>
          some (.ite (.app p e) (.cons e (call .filter [p, es])) (call .filter [p, es]))
      | _ => none
  | .foldl => fun
      | [_, z, .nil] => some z
      | [f, z, .cons e es] => some (call .foldl [f, .app (.app f z) e, es])
      | _ => none
  | .toString => fun | [.con (.int n)] => some (.con (.str (ToString.toString n))) | _ => none
  | .isAttrs => fun
      | [.rcd _] => some (.bool true)
      | [_] => some (.bool false)
      | _ => none
  -- `+` on strings, and string interpolation
  | .strcat => fun
      | [.con (.str a), .con (.str b)] => some (.con (.str (a ++ b)))
      | _ => none

def MConst.ty : MConst → String
  | .int _ => "int"
  | .str _ => "string"
  | .null  => "null"
  | .op _  => "builtin"

def sig : Sig String MConst where
  con   := MConst.ty
  boolB := some "bool"
  prim  := fun | .op o => some o.ty | _ => none
  delta := fun | .op o => o.delta | _ => fun _ => none

--------------------- δ PRESERVES TYPES ----------------------------------------

-- a list literal typed at [τ] has its head at τ and its tail at [τ]
theorem cons_arg {Γ : QCtx String} {e es : E} {τ : T}
    (h : QTyped sig Γ (.cons e es) (.list τ)) :
    QTyped sig Γ e τ ∧ QTyped sig Γ es (.list τ) := by
  obtain ⟨τ₀, he | hu, h₁, h₂⟩ := qtyped_cons_inv h
  · obtain ⟨σ, hσ, hτ⟩ := he.list_inv
    cases hσ
    exact ⟨.qEq h₁ hτ, .qEq h₂ (.list hτ)⟩
  · cases hu

-- the result type of an int-valued builtin at any instance
theorem qint {Γ : QCtx String} (n : Int) : QTyped sig Γ (int n) tInt := .qCon

theorem qbool {Γ : QCtx String} (b : Bool) : QTyped sig Γ (.bool b) tBool := .qBool rfl

theorem args1 {Γ : QCtx String} {v : E} {τs : List T}
    (h : QTypedArgs sig Γ [v] τs) : ∃ τ₁, τs = [τ₁] ∧ QTyped sig Γ v τ₁ := by
  rcases h with _ | ⟨h₁, _ | _⟩; exact ⟨_, rfl, h₁⟩

theorem args2 {Γ : QCtx String} {v w : E} {τs : List T}
    (h : QTypedArgs sig Γ [v, w] τs) :
    ∃ τ₁ τ₂, τs = [τ₁, τ₂] ∧ QTyped sig Γ v τ₁ ∧ QTyped sig Γ w τ₂ := by
  rcases h with _ | ⟨h₁, _ | ⟨h₂, _ | _⟩⟩; exact ⟨_, _, rfl, h₁, h₂⟩

theorem args3 {Γ : QCtx String} {u v w : E} {τs : List T}
    (h : QTypedArgs sig Γ [u, v, w] τs) :
    ∃ τ₁ τ₂ τ₃, τs = [τ₁, τ₂, τ₃] ∧ QTyped sig Γ u τ₁ ∧ QTyped sig Γ v τ₂ ∧
      QTyped sig Γ w τ₃ := by
  rcases h with _ | ⟨h₁, _ | ⟨h₂, _ | ⟨h₃, _ | _⟩⟩⟩; exact ⟨_, _, _, rfl, h₁, h₂, h₃⟩

/-- a builtin used at the instance θ of its type -/
theorem qcall {Γ : QCtx String} {o : Op} {θ : TySubst String} {es : List E}
    {τs : List T} {τ : T} (hi : o.ty.applySubst θ = Ty.arrows τs τ)
    (ha : QTypedArgs sig Γ es τs) : QTyped sig Γ (call o es) τ :=
  .qPrim rfl hi ha

/-- what δ owes typing, one builtin at a time -/
def Pres (o : Op) : Prop :=
  ∀ {θ : TySubst String} {τs : List T} {τ : T} {vs : List E} {e : E},
    o.ty.applySubst θ = Ty.arrows τs τ → QTypedArgs sig QCtx.empty vs τs →
    o.delta vs = some e → QTyped sig QCtx.empty e τ

theorem pres_add : Pres .add := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact qint _
  · cases hδ

theorem pres_sub : Pres .sub := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact qint _
  · cases hδ

theorem pres_mul : Pres .mul := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact qint _
  · cases hδ

theorem pres_div : Pres .div := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    split at hδ
    · cases hδ
    · cases hδ; exact qint _
  · cases hδ

theorem pres_lt : Pres .lt := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tInt, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact qbool _
  · cases hδ

theorem pres_le : Pres .le := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tInt, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact qbool _
  · cases hδ

theorem pres_eq : Pres .eq := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tA, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    obtain ⟨b, -, rfl⟩ := Option.map_eq_some_iff.mp hδ
    exact qbool _
  · cases hδ

theorem pres_not : Pres .not := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, rfl, -⟩ := args1 ha
    simp only [Op.ty, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, rfl⟩ := hi
    cases hδ; exact qbool _
  · cases hδ

theorem pres_concat : Pres .concat := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, h₂⟩ := args2 ha
    simp only [Op.ty, tA, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, rfl, rfl⟩ := hi
    cases hδ; exact h₂
  · obtain ⟨τ₁, τ₂, rfl, h₁, h₂⟩ := args2 ha
    have hi' := hi
    simp only [Op.ty, tA, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl, rfl⟩ := hi
    cases hδ
    obtain ⟨he, hes⟩ := cons_arg h₁
    exact .qCons he (qcall (θ := θ) hi' (.cons hes (.cons h₂ .nil)))
  · cases hδ

theorem pres_head : Pres .head := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, rfl, h₁⟩ := args1 ha
    simp only [Op.ty, tA, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl⟩ := hi
    cases hδ; exact (cons_arg h₁).1
  · cases hδ

theorem pres_tail : Pres .tail := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, rfl, h₁⟩ := args1 ha
    simp only [Op.ty, tA, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl⟩ := hi
    cases hδ; exact (cons_arg h₁).2
  · cases hδ

theorem pres_elemAt : Pres .elemAt := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, h₁, -⟩ := args2 ha
    have hi' := hi
    simp only [Op.ty, tA, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl, rfl⟩ := hi
    obtain ⟨he, hes⟩ := cons_arg h₁
    split at hδ
    · cases hδ; exact he
    · split at hδ
      · cases hδ; exact qcall (θ := θ) hi' (.cons hes (.cons (qint _) .nil))
      · cases hδ
  · cases hδ

theorem pres_length : Pres .length := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, rfl, -⟩ := args1 ha
    simp only [Op.ty, tA, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, rfl⟩ := hi
    cases hδ; exact qint _
  · obtain ⟨τ₁, rfl, h₁⟩ := args1 ha
    have hi' := hi
    simp only [Op.ty, tA, tInt, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl⟩ := hi
    cases hδ
    exact qcall (o := .add) (θ := θ) (τs := [tInt, tInt]) rfl
      (.cons (qint _) (.cons (qcall (θ := θ) hi' (.cons (cons_arg h₁).2 .nil)) .nil))
  · cases hδ

theorem pres_map : Pres .map := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tA, tB, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact .qNil
  · obtain ⟨τ₁, τ₂, rfl, h₁, h₂⟩ := args2 ha
    have hi' := hi
    simp only [Op.ty, tA, tB, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl, rfl⟩ := hi
    cases hδ
    obtain ⟨he, hes⟩ := cons_arg h₂
    exact .qCons (.qApp h₁ he) (qcall (θ := θ) hi' (.cons h₁ (.cons hes .nil)))
  · cases hδ

theorem pres_filter : Pres .filter := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tA, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact .qNil
  · obtain ⟨τ₁, τ₂, rfl, h₁, h₂⟩ := args2 ha
    have hi' := hi
    simp only [Op.ty, tA, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl, rfl⟩ := hi
    cases hδ
    obtain ⟨he, hes⟩ := cons_arg h₂
    have hrec := qcall (θ := θ) hi' (.cons h₁ (.cons hes .nil))
    exact .qIte rfl (.qApp h₁ he) (.qCons he hrec) hrec
  · cases hδ

theorem pres_foldl : Pres .foldl := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, τ₃, rfl, -, h₂, -⟩ := args3 ha
    simp only [Op.ty, tA, tB, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, rfl, -, rfl⟩ := hi
    cases hδ; exact h₂
  · obtain ⟨τ₁, τ₂, τ₃, rfl, h₁, h₂, h₃⟩ := args3 ha
    have hi' := hi
    simp only [Op.ty, tA, tB, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hi
    cases hδ
    obtain ⟨he, hes⟩ := cons_arg h₃
    exact qcall (θ := θ) hi' (.cons h₁ (.cons (.qApp (.qApp h₁ h₂) he) (.cons hes .nil)))
  · cases hδ

theorem pres_toString : Pres .toString := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, rfl, -⟩ := args1 ha
    simp only [Op.ty, tInt, tStr, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, rfl⟩ := hi
    cases hδ; exact .qCon
  · cases hδ

theorem pres_isAttrs : Pres .isAttrs := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ <;> (try cases hδ) <;>
  · obtain ⟨τ₁, rfl, -⟩ := args1 ha
    simp only [Op.ty, tA, tBool, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, rfl⟩ := hi
    exact qbool _

theorem pres_strcat : Pres .strcat := by
  intro θ τs τ vs e hi ha hδ
  simp only [Op.delta] at hδ
  split at hδ
  · obtain ⟨τ₁, τ₂, rfl, -, -⟩ := args2 ha
    simp only [Op.ty, tStr, Ty.applySubst, Ty.arrows, Ty.fn.injEq] at hi
    obtain ⟨-, -, rfl⟩ := hi
    cases hδ; exact .qCon
  · cases hδ

theorem Op.pres : (o : Op) → Pres o
  | .add => pres_add | .sub => pres_sub | .mul => pres_mul | .div => pres_div
  | .lt => pres_lt | .le => pres_le | .eq => pres_eq | .not => pres_not
  | .concat => pres_concat | .head => pres_head | .tail => pres_tail
  | .elemAt => pres_elemAt | .length => pres_length | .map => pres_map
  | .filter => pres_filter | .foldl => pres_foldl | .toString => pres_toString
  | .isAttrs => pres_isAttrs | .strcat => pres_strcat

/-- ⊢  **the MacroNix signature is sound**: δ preserves types, and no constant
inhabits `bool`. Hence `qProgress sig macroSig_ok` and `qPreservation sig
macroSig_ok`: type safety for MacroNix programs. -/
theorem macroSig_ok : SigOK sig where
  con_ne_bool c bb hb := by
    cases hb
    cases c <;> simp [sig, MConst.ty]
  delta_pres {p} {_} {_} {_} {_} {_} {_} hp hi _ ha hδ := by
    cases p with
    | int _ | str _ | null => cases hp
    | op o =>
        simp only [sig, Option.some.injEq] at hp
        subst hp
        exact o.pres hi ha hδ

/-- ⊢  **progress for MacroNix**: a closed well-typed program is a value, steps,
or is an error (a lookup error, a failed `assert`, or a builtin that fails). -/
theorem macro_progress {e : E} {τ : T} (h : QTyped sig QCtx.empty e τ) :
    Progress sig.delta e :=
  qProgress sig macroSig_ok e τ h

/-- ⊢  **preservation for MacroNix**. -/
theorem macro_preservation {e e' : E} {τ : T} (h : QTyped sig QCtx.empty e τ)
    (hs : Step sig.delta e e') : QTyped sig QCtx.empty e' τ :=
  qPreservation sig macroSig_ok e e' τ h hs

end MacroNix
end MinimalCalculus
