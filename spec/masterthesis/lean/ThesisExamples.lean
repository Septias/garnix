-- THE THESIS'S WORKED EXAMPLES, RUN. Every example of `text/sections/*.typ`
-- that names a type, a verdict, a rule or a sub-goal is replayed here, in the
-- order of the thesis. A `rfl`/`decide`/`#guard` that stops checking means the
-- text and the development have drifted apart.
--
-- Unification examples are checked move by move: `traceRow` below is the
-- driver `unifySpineMF` with every arm labelled by the rule name the thesis
-- uses (Figure "Unification of rows, on spines"), and each trace is pinned
-- together with the claim that its verdict IS the driver's verdict.
--
-- Inference examples run `runF` (the Entry rule), and where the text names an
-- intermediate state (a parked stump, its blocker, a generalized scheme) that
-- state is read off `inferF` directly.

import InferRuns
import Regressions
import Refutations
import Builtins

namespace MinimalCalculus
namespace ThesisExamples

open InferRuns (tyS rowS keyS verdict v c rec1 rec2 L sd rd)

------------------------------------------------------------------------------
-- # The labelled driver
------------------------------------------------------------------------------

mutual

def traceTy {B : Type} [DecidableEq B] (S : Supply) (fuel : Nat) :
    Ty B → Ty B → List String × UResM B
  | .var α, τ₂ =>
      (if tyIsVar τ₂ = some α then ["U-refl"]
       else if τ₂.tyFtv.contains α then ["U-occurs"] else ["U-bind"], bindTy S α τ₂)
  | τ₁, .var α =>
      (if tyIsVar τ₁ = some α then ["U-refl"]
       else if τ₁.tyFtv.contains α then ["U-occurs"] else ["U-bind"], bindTy S α τ₁)
  | .unk, .unk => (["U-★"], .success .nil S)
  | .base b, .base b' =>
      if b = b' then (["U-base"], .success .nil S) else (["U-base-clash"], .clash)
  | .lab k₁, .lab k₂ => (["U-lab"], unifyKey S k₁ k₂)
  | .fn a₁ b₁, .fn a₂ b₂ =>
      match fuel with
      | 0 => (["No fuel"], .outOfFuel)
      | f+1 =>
          let r₁ := traceTy S f a₁ a₂
          match r₁.2 with
          | .success s₁ S₁ =>
              let r₂ := traceTy S₁ f (b₁.applySubst s₁.toSubst) (b₂.applySubst s₁.toSubst)
              ("U-fn" :: r₁.1 ++ r₂.1,
               match r₂.2 with
               | .success s₂ S₂ => .success (s₂.comp s₁) S₂
               | r => r)
          | r => ("U-fn" :: r₁.1, r)
  | .rcd ρ₁, .rcd ρ₂ =>
      match fuel with
      | 0 => (["No fuel"], .outOfFuel)
      | f+1 => let r := traceRow S f ρ₁.toSpine ρ₂.toSpine; ("U-rcd" :: r.1, r.2)
  | .list τ₁, .list τ₂ =>
      match fuel with
      | 0 => (["No fuel"], .outOfFuel)
      | f+1 => let r := traceTy S f τ₁ τ₂; ("U-list" :: r.1, r.2)
  | _, _ => (["U-clash"], .clash)

def traceRow {B : Type} [DecidableEq B] :
    Supply → Nat → List (Atom B) → List (Atom B) → List String × UResM B
  | S, _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => (["U-ε-var"], .success (Sol.ofRow σ) S)
      | none   => (["U-ε-clash"], .clash)
  | S, _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => (["U-ε-var"], .success (Sol.ofRow σ) S)
      | none   => (["U-ε-clash"], .clash)
  | _, 0, _, _ => (["No fuel"], .outOfFuel)
  | S, fuel+1, s₁, s₂ =>
      -- an eq-emitting move: name, type sub-goal, then the residual
      let emit (name : String) (τ τ' : Ty B) (t₁ t₂ : List (Atom B)) :
          List String × UResM B :=
        let r₁ := traceTy S fuel τ τ'
        match r₁.2 with
        | .success s₁ S₁ =>
            let r₂ := traceRow S₁ fuel (sApplySubst s₁.toSubst t₁) (sApplySubst s₁.toSubst t₂)
            (name :: r₁.1 ++ r₂.1,
             match r₂.2 with
             | .success s₂ S₂ => .success (s₂.comp s₁) S₂
             | r => r)
        | r => (name :: r₁.1, r)
      match stripL s₁ s₂ with
      | some (t₁, t₂) => let r := traceRow S fuel t₁ t₂; ("U-var-refl-L" :: r.1, r.2)
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => let r := traceRow S fuel t₁ t₂; ("U-var-refl-R" :: r.1, r.2)
      | none =>
      let solveName (s₁ s₂ : List (Atom B)) (r : UResM B) : String :=
        match r, s₁ with
        | .occurs, _ => "U-var-occurs"
        | .stuck, _ => "U-var-stuck"
        | _, [.var α] => if (collapseSol α s₂).isSome then "U-var-collapse" else "U-var-solve"
        | _, _ => "U-var-solve"
      match solveVarM S s₁ s₂ with
      | some r => ([solveName s₁ s₂ r], r)
      | none =>
      match solveVarM S s₂ s₁ with
      | some r => ([solveName s₂ s₁ r ++ " ⇄"], r)
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) => emit "U-field-L" τ τ' t₁ t₂
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) => emit "U-field-L ⇄" τ τ' t₁ t₂
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) => emit "U-field-R" τ τ' t₁ t₂
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) => emit "U-field-R ⇄" τ τ' t₁ t₂
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) => emit "U-ground" τ τ' t₁ t₂
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) => emit "U-ground ⇄" τ τ' t₁ t₂
      | none =>
      if projClash s₁ s₂ then (["U-clash"], .clash) else
      match keyPin s₁ s₂ with
      | some (α, k) =>
          let r₁ := traceTy S fuel (.lab (.var α)) (.lab k)
          match r₁.2 with
          | .success s₁' S₁ =>
              let r₂ := traceRow S₁ fuel (sApplySubst s₁'.toSubst s₁) (sApplySubst s₁'.toSubst s₂)
              ("U-key-pin" :: r₁.1 ++ r₂.1,
               match r₂.2 with
               | .success s₂' S₂ => .success (s₂'.comp s₁') S₂
               | r => r)
          | r => ("U-key-pin" :: r₁.1, r)
      | none =>
      match hostL S s₁ s₂ with
      | some (β, l, τ, t₁, t₂) =>
          let r := traceRow S.fresh.2 fuel t₁ t₂
          ("U-host" :: r.1, hostResM S β l τ r.2)
      | none =>
      match hostL S s₂ s₁ with
      | some (β, l, τ, t₂, t₁) =>
          let r := traceRow S.fresh.2 fuel t₁ t₂
          ("U-host ⇄" :: r.1, hostResM S β l τ r.2)
      | none => (["U-stuck"], .stuck)

end

/-- the rule sequence the driver takes on `ρ₁ ≐ᵣ ρ₂` -/
def rowTrace {B : Type} [DecidableEq B] (ρ₁ ρ₂ : Row B) : List String × UResM B :=
  traceRow (localSupply ρ₁.toSpine ρ₂.toSpine) 20 ρ₁.toSpine ρ₂.toSpine

def tyTrace {B : Type} [DecidableEq B] (τ₁ τ₂ : Ty B) : List String × UResM B :=
  traceTy ⟨lenBound (τ₁.ftv ++ τ₂.ftv) + 1⟩ 20 τ₁ τ₂

------------------------------------------------------------------------------
-- # Printing
------------------------------------------------------------------------------

abbrev T := Ty Unit
abbrev R := Row Unit
def b : T := .base ()
def tv (x : String) : T := .var x
def rv (x : String) : R := .var x
def f (l : String) (τ : T) : R := .sing l τ
/-- a row from a list, right-nested -/
def row : List R → R
  | [] => .empty
  | [ρ] => ρ
  | ρ :: ρs => .cat ρ (row ρs)

-- names are printed as they are, so traces can be read against the thesis
mutual
def tyN : T → String
  | .var α    => α
  | .base _   => "𝓫"
  | .lab k    => "⌊" ++ keyN k ++ "⌋"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyN τ₁ ++ " → " ++ tyN τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowN ρ ++ "}"
  | .list τ   => "[" ++ tyN τ ++ "]"
def rowN : R → String
  | .empty     => "ε"
  | .var α     => α
  | .sing l τ  => l ++ ": " ++ tyN τ
  | .cat ρ₁ ρ₂ => "(" ++ rowN ρ₁ ++ " | " ++ rowN ρ₂ ++ ")"
  | .dsing q τ => "${" ++ keyN q ++ "}: " ++ tyN τ
def keyN : Key → String
  | .lit l => l
  | .var α => α
end

def solN (s : Sol Unit) : String :=
  String.intercalate ", "
    (s.ty.map (fun p => p.1 ++ " ≔ " ++ tyN p.2) ++
     s.row.map (fun p => p.1 ++ " ≔ " ++ rowN p.2) ++
     s.lab.map (fun p => p.1 ++ " ≔ " ++ keyN p.2))

def resN : UResM Unit → String
  | .success s _ => "[" ++ solN s ++ "]"
  | .clash => "Clash" | .occurs => "Occurs" | .stuck => "Stuck" | .outOfFuel => "No fuel"

def tr (ρ₁ ρ₂ : R) : List String × String :=
  let r := rowTrace ρ₁ ρ₂; (r.1, resN r.2)

/-- the trace's verdict is the real driver's -/
def agrees (ρ₁ ρ₂ : R) : Bool := resN (rowTrace ρ₁ ρ₂).2 == resN (unifyRowM 20 ρ₁ ρ₂)

-- Inferred types are printed with their variables renamed in order of first
-- occurrence, by sort: α for types, ρ for rows, κ for label variables. The
-- unifier's trailing `| ε` (spine form, ≈-invisible) is kept.
mutual
def tyC (L : List TyVar) : T → String
  | .var α    => "α" ++ toString (L.idxOf α + 1)
  | .base _   => "𝓫"
  | .lab k    => "⌊" ++ keyC L k ++ "⌋"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyC L τ₁ ++ " → " ++ tyC L τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowC L ρ ++ "}"
  | .list τ   => "[" ++ tyC L τ ++ "]"
def rowC (L : List TyVar) : R → String
  | .empty     => "ε"
  | .var α     => "ρ" ++ toString (L.idxOf α + 1)
  | .sing l τ  => l ++ ": " ++ tyC L τ
  | .cat ρ₁ ρ₂ => rowC L ρ₁ ++ " | " ++ rowC L ρ₂
  | .dsing q τ => "${" ++ keyC L q ++ "}: " ++ tyC L τ
def keyC (L : List TyVar) : Key → String
  | .lit l => l
  | .var α => "κ" ++ toString (L.idxOf α + 1)
end

def σ0 : Sig Unit Unit := Sig.plain fun _ => ()

def stumpC (L : List TyVar) (st : Stump Unit) : String :=
  "⟨" ++ rowC L st.row ++ "." ++ keyC L st.label ++ " ↓ " ++ tyC L st.res ++ "⟩"

/-- the Entry rule: the scheme a run answers, printed `Q ⇒ τ` (every variable is
bound, so the ∀ is left implicit; Q = ∅ prints as the plain type), and the
warnings W -/
def run (e : InferRuns.E) : String × List Label :=
  match runF σ0 50 e with
  | .ok (σ, S) =>
      let L := (σ.body.ftv ++ σ.constraints.flatMap
                  (fun st => st.row.ftv ++ st.label.ftv ++ st.res.ftv)).eraseDups
      ((if σ.constraints.isEmpty then "" else
          String.intercalate ", " (σ.constraints.map (stumpC L)) ++ " ⇒ ") ++ tyC L σ.body,
       S.flags)
  | .fail m => ("fail: " ++ m, [])
  | .oof => ("out of fuel", [])

def schemeC (σ : QScheme Unit) : String :=
  let L := (σ.body.ftv ++ σ.constraints.flatMap
              (fun st => st.row.ftv ++ st.label.ftv ++ st.res.ftv)).eraseDups
  "∀" ++ String.intercalate "," (σ.vars.map (fun α => "#" ++ toString (L.idxOf α + 1))) ++ ". " ++
  (if σ.constraints.isEmpty then "" else
     String.intercalate ", " (σ.constraints.map (stumpC L)) ++ " ⇒ ") ++ tyC L σ.body

/-- A-let at the top level: the scheme `let x = e₁ in …` binds x to, and how
many stumps stay parked in the state (Δ_Γ) -/
def schemeOf (e₁ : InferRuns.E) : String × Nat :=
  match inferF σ0 50 QCtx.empty runStart e₁ with
  | .ok (τ, S₁) =>
      let ᾱ := greatestAlpha QCtx.empty runStart S₁ τ
      (schemeC (letScheme S₁ ᾱ (letQ S₁ ᾱ) τ), (letG S₁ ᾱ).length)
  | _ => ("fail", 0)

/-- inference proper, before the merge: the type and the parked stumps,
each with its blocker, read under the state's substitution -/
def parkedOf (e : InferRuns.E) : String × List String :=
  match inferF σ0 50 QCtx.empty runStart e with
  | .ok (τ, S) =>
      let τ' := τ.applySubst S.subst
      let ps := S.parked.map fun p =>
        (p.blocker, (⟨p.stump.row.applySubst S.subst, p.stump.label.applySubst S.subst,
                      p.stump.res.applySubst S.subst⟩ : Stump Unit))
      let L := (τ'.ftv ++ ps.flatMap (fun q => q.1 :: q.2.row.ftv ++ q.2.label.ftv ++ q.2.res.ftv)).eraseDups
      (tyC L τ', ps.map fun q =>
        (if (Ty.rcd (.var q.1) : T).ftv.isEmpty then "" else
          match q.2.row.ftv.contains q.1, q.2.label.ftv.contains q.1 with
          | _, true => "κ" | _, _ => "ρ") ++ toString (L.idxOf q.1 + 1) ++ " ⊳ " ++ stumpC L q.2)
  | _ => ("fail", [])

------------------------------------------------------------------------------
-- # Deciding ≈ on concrete rows
-- `rowEquiv_iff_char`: same barrier sequence, pointwise-≈ projections. When
-- the projections agree syntactically that is a computation, so `charB`
-- below is a sound (not complete) test, and `charB_sound` turns its `true`
-- into a `RowEquiv`. The negative direction reads one invariant off `char`.
------------------------------------------------------------------------------

deriving instance DecidableEq for Barrier

theorem sProj_nil {B : Type} {l : Label} :
    (s : List (Atom B)) → l ∉ sLabels s → sProj l s = []
  | [], _ => rfl
  | .field l' τ :: s, h => by
      simp only [sLabels, List.mem_cons, not_or] at h
      simp only [sProj, if_neg (Ne.symm h.1)]
      exact sProj_nil s h.2
  | .var _ :: s, h => by simp only [sProj, sProj_nil s h, List.map_nil]
  | .dfield _ _ :: s, h => by simp only [sProj, sProj_nil s h, List.map_nil]

def charB {B : Type} [DecidableEq B] (ρ₁ ρ₂ : Row B) : Bool :=
  decide (sBarSeq ρ₁.toSpine = sBarSeq ρ₂.toSpine) &&
  (sLabels ρ₁.toSpine ++ sLabels ρ₂.toSpine).all
    (fun l => decide (sProj l ρ₁.toSpine = sProj l ρ₂.toSpine)) &&
  decide (sDProj ρ₁.toSpine = sDProj ρ₂.toSpine)

theorem charB_sound {B : Type} [DecidableEq B] {ρ₁ ρ₂ : Row B}
    (h : charB ρ₁ ρ₂ = true) : ρ₁ ≈ᵣ ρ₂ := by
  simp only [charB, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true] at h
  obtain ⟨⟨hb, hp⟩, hd⟩ := h
  refine RowEquiv.ofChar ⟨hb, fun l => ?_, hd ▸ ProjEquiv.refl _⟩
  by_cases hl : l ∈ sLabels ρ₁.toSpine ++ sLabels ρ₂.toSpine
  · rw [show sProj l ρ₁.toSpine = sProj l ρ₂.toSpine from by simpa using hp l hl]
    exact ProjEquiv.refl _
  · simp only [List.mem_append, not_or] at hl
    rw [sProj_nil _ hl.1, sProj_nil _ hl.2]; exact .nil

theorem projFst {B : Type} {ps qs : List (Nat × Ty B)} (h : ProjEquiv ps qs) :
    ps.map Prod.fst = qs.map Prod.fst := by
  induction h with
  | nil => rfl
  | cons hn _ _ ih => simp [hn, ih]

/-- different barrier sequences: not ≈ -/
theorem not_equiv_bars {B : Type} {ρ₁ ρ₂ : Row B}
    (h : sBarSeq ρ₁.toSpine ≠ sBarSeq ρ₂.toSpine) : ¬ ρ₁ ≈ᵣ ρ₂ :=
  fun he => h he.char.1

/-- l-fields in different numbers, or in different segments: not ≈ -/
theorem not_equiv_proj {B : Type} {ρ₁ ρ₂ : Row B} (l : Label)
    (h : (sProj l ρ₁.toSpine).map Prod.fst ≠ (sProj l ρ₂.toSpine).map Prod.fst) :
    ¬ ρ₁ ≈ᵣ ρ₂ :=
  fun he => h (projFst (he.char.2.1 l))

def sub (rows : List (TyVar × R)) (labs : List (TyVar × Key) := []) : TySubst Unit :=
  (⟨[], rows, labs⟩ : Sol Unit).toSubst

/-- θ unifies ρ₁ and ρ₂, decided -/
def unifiesB (θ : TySubst Unit) (ρ₁ ρ₂ : R) : Bool := charB (ρ₁.applySubst θ) (ρ₂.applySubst θ)

theorem unifies_of {θ : TySubst Unit} {ρ₁ ρ₂ : R} (h : unifiesB θ ρ₁ ρ₂ = true) :
    Unifies θ ρ₁ ρ₂ := charB_sound h

/-- a monomorphic binding, as the λ rule puts it into Γ -/
theorem qvar {C : Type} {constTy : Sig Unit C} {Γ : QCtx Unit} {x : Var} {τ : T}
    (h : Γ.lookup x = some ⟨[], [], τ⟩) : QTyped constTy Γ (.var x) τ :=
  .qVar h (QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], _⟩))

open InferRuns (E)
def sel (e : E) (l : String) : E := .sel e l
def lam : Var → E → E := .lam
def app : E → E → E := .app
def cat : E → E → E := .cat
def emp : E := .rcd .empty
def δ0 : Delta Unit := σ0.delta

------------------------------------------------------------------------------
-- # §1 Introduction
------------------------------------------------------------------------------

-- `{ "1759190400" = 1; }.${toString builtins.currentTime}`: a dynamic selection
-- under a key of base type (MacroNix: `toString` yields a string) is typed ★,
-- with a warning, by A-sel-dyn-𝓫. `builtins.currentTime` is an int here.
section intro
open MacroNix
def timeEx : MacroNix.E :=
  .selDyn (.rcd (.field "1759190400" (int 1))) (call .toString [int 1759190400])

#guard (match runF sig 50 timeEx with
        | .ok (σ, S) => σ.body == .unk && σ.constraints.isEmpty && !S.flags.isEmpty
        | _ => false)

-- §9 G3: the same program extended by one selection has no type: ★ cannot be
-- selected from
#guard (match runF sig 50 (.sel timeEx "m") with | .fail "clash" => true | _ => false)
end intro

-- Wand's program `a: b: (a ‖ b).l` (§1, §2): the lookup is undecided, and the
-- answer keeps it as a constraint, in the shape of Rose's principal type (§10).
-- The old `{ρ1} → {ρ2} → ★` with a warning is its D-? instance
#guard run (lam "a" (lam "b" (sel (cat (v "a") (v "b")) "l"))) = ("⟨ρ2 | ρ1 | ε.l ↓ α3⟩ ⇒ ({ρ1} → ({ρ2} → α3))", [])

------------------------------------------------------------------------------
-- # §2 Our Position
------------------------------------------------------------------------------

-- the selector at a hit and at a definite absence (T-sel, T-sel-⊥)
example (τ₀ : T) : Typed (fun (_ : Unit) => ()) Ctx.empty (selEx Unit) (.fn (.rcd (.sing "l" τ₀)) τ₀) :=
  selEx_found _ τ₀
example : Typed (fun (_ : Unit) => ()) Ctx.empty (selEx Unit) (.fn (.rcd .empty) .unk) :=
  selEx_absent _
-- …and at a row variable the lookup answers ?
example : Lookup (.var "β" : R) "l" .unknown := .varFree

-- `let f = (x: x.l) in { a = (f {l = {m = c}}).m; b = f {} } : {a: 𝓫_c | b: ★}`
-- (also Corollary "Qualified schemes type strictly more")
def twoUse : E :=
  .letE "f" (lam "x" (sel (v "x") "l"))
    (rec2 "a" (sel (app (v "f") (rec1 "l" (rec1 "m" c))) "m") "b" (app (v "f") emp))

#guard run twoUse = ("{a: 𝓫 | b: ★}", ["l"])
-- the declarative typing of exactly this program: f at selQ, used at
-- {(l: {(m: 𝓫)})} → {(m: 𝓫)} and at {ε} → ★
theorem twoUse_typed : QTyped σ0 QCtx.empty twoUse (.rcd (.cat (.sing "a" b) (.sing "b" .unk))) := by
  refine .qLet (σ := selQ Unit) (by simp [QScheme.WF, selQ, Ty.ftv]) (fun τ₁ hq => ?_)
    ⟨_, selQ_inst_absent⟩ ?_
  · exact (selQ_instance_closed σ0 Ctx.empty τ₁ hq).toQ
  · refine .qRcd (.cat (.field ?_) (.field ?_))
    · exact .qSel (.qApp (.qVar (by simp [QCtx.lookup_bindScheme])
                    (selQ_inst_found (.rcd (.sing "m" b))))
                  (.qRcd (.field (.qRcd (.field .qCon))))) .hit
    · exact .qApp (.qVar (by simp [QCtx.lookup_bindScheme]) selQ_inst_absent) (.qRcd .empty)

-- The Wand ambiguity, resolved. f's scheme is
--   ∀(α, β: Row, δ: Type). ⟨(β | α).l ↓ δ⟩ ⇒ {α} → {β} → δ
-- (the unifier's spine form appends `| ε`)
def wandF : E := lam "a" (lam "b" (sel (cat (v "a") (v "b")) "l"))
#guard schemeOf wandF = ("∀#1,#2,#3. ⟨ρ2 | ρ1 | ε.l ↓ α3⟩ ⇒ ({ρ1} → ({ρ2} → α3))", 0)
-- …and its three uses. The second operand of the second use is `{l = {}}`, so
-- that "the right operand shadows" is visible in the type.
#guard run (.letE "f" wandF (app (app (v "f") (rec1 "l" c)) emp)) = ("𝓫", [])
#guard run (.letE "f" wandF (app (app (v "f") (rec1 "l" c)) (rec1 "l" emp))) = ("{ε}", [])
#guard run (.letE "f" wandF (app (app (v "f") emp) emp)) = ("★", ["l"])

------------------------------------------------------------------------------
-- # §3 The Declarative Calculus
------------------------------------------------------------------------------

-- Row equivalence: `{ a = 1; b = 2; }` and `{ b = 2; a = 1; }`
example : RowEquiv (row [f "a" b, f "b" (.rcd .empty)]) (row [f "b" (.rcd .empty), f "a" b]) :=
  charB_sound rfl

-- Trace monoid. Sorting would identify (l: 𝓫 | α) and (α | l: 𝓫); they are not ≈
-- (the l-field sits in different segments) …
example : ¬ RowEquiv (row [f "l" b, rv "α"]) (row [rv "α", f "l" b]) :=
  not_equiv_proj "l" (by decide)
-- … and α ≔ (l: 𝓫′) makes the lookup of l answer 𝓫 in the first and 𝓫′ in the
-- second (𝓫′ := {ε}, any type other than 𝓫)
example : Lookup ((row [f "l" b, rv "α"]).applySubst (sub [("α", f "l" (.rcd .empty))])) "l"
    (.found b) := .catHit .hit
example : Lookup ((row [rv "α", f "l" b]).applySubst (sub [("α", f "l" (.rcd .empty))])) "l"
    (.found (.rcd .empty)) := .catHit .hit

-- The normal form: (m: 𝓫 | l: 𝓫 | α | l: 𝓫′ | m: 𝓫) ≈ (l: 𝓫 | m: 𝓫 | α | l: 𝓫′ | m: 𝓫)
def b' : T := .rcd .empty
example : RowEquiv (row [f "m" b, f "l" b, rv "α", f "l" b', f "m" b])
                   (row [f "l" b, f "m" b, rv "α", f "l" b', f "m" b]) := charB_sound rfl
-- …and no field crosses α
example : ¬ RowEquiv (row [f "m" b, f "l" b, rv "α", f "l" b', f "m" b])
                     (row [f "l" b, f "l" b', rv "α", f "m" b, f "m" b]) :=
  not_equiv_proj "l" (by decide)

-- Example "Two ways to type a selection at ★".
-- `{}.l : ★` by T-sel-⊥, since ε.l ↓ ⊥ …
example : QTyped σ0 QCtx.empty (sel emp "l") .unk := .qSelAbs (.qRcd .empty) .emp
-- … and it is a lookup error by ↯-sel
example : Err δ0 (sel emp "l") := .selAbsent rfl
-- `x: x.l : {β} → ★` by T-sel-★, since β.l ↓ ?
example : QTyped σ0 QCtx.empty (lam "x" (sel (v "x") "l")) (.fn (.rcd (.var "β")) .unk) :=
  .qLam (.qSelUnk (qvar (τ := .rcd (.var "β")) (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup])) .varFree)
-- applied to `{l = c}` it evaluates to c …
example : ∃ e', Step δ0 (app (lam "x" (sel (v "x") "l")) (rec1 "l" c)) e' ∧ Step δ0 e' c :=
  ⟨_, .beta .rcd, .selVal rfl⟩
-- … applied to `{}` to a lookup error
example : ∃ e', Step δ0 (app (lam "x" (sel (v "x") "l")) emp) e' ∧ Err δ0 e' :=
  ⟨_, .beta .rcd, .selAbsent rfl⟩

-- T-sel-⊥ is needed for programs that never fail: `{a = {}.l; b = c}.b`
-- evaluates to c, and it is typed only because field a is typed (at ★)
example : Step δ0 (sel (rec2 "a" (sel emp "l") "b" c) "b") c := .selVal rfl
example : QTyped σ0 QCtx.empty (sel (rec2 "a" (sel emp "l") "b" c) "b") b :=
  .qSel (.qRcd (.cat (.field (.qSelAbs (.qRcd .empty) .emp)) (.field .qCon)))
        (.catSkip (.miss (by decide)) .hit)
#guard run (sel (rec2 "a" (sel emp "l") "b" c) "b") = ("𝓫", ["l"])

-- Example "Instances of selQ"
example : QScheme.Inst (selQ Unit) (.fn (.rcd (.sing "l" b)) b) := selQ_inst_found b
example : QScheme.Inst (selQ Unit) (.fn (.rcd .empty) .unk) := selQ_inst_absent
example : ¬ QScheme.Inst (selQ Unit) (.fn (.rcd .empty) (.rcd .empty)) := selQ_no_mixed
-- …while {ε} → {ε} IS an instance of the plain scheme ∀β δ. {β} → δ
example : Scheme.Inst (⟨["β", "δ"], .fn (.rcd (rv "β")) (tv "δ")⟩ : Scheme Unit)
    (.fn (.rcd .empty) (.rcd .empty)) := by
  refine ⟨⟨fun γ => if γ = "δ" then .rcd .empty else .var γ,
           fun γ => if γ = "β" then .empty else .var γ, fun γ => .var γ⟩,
          ⟨fun γ hγ => ?_, fun γ hγ => ?_, fun _ _ => rfl⟩, ?_⟩
  · have : γ ≠ "δ" := by rintro rfl; simp at hγ
    simp [this]
  · have : γ ≠ "β" := by rintro rfl; simp at hγ
    simp [this]
  · simp [Ty.applySubst, Row.applySubst, rv, tv]

-- Example "An uninhabited scheme": ∀δ. ⟨(l: 𝓫).l ↓ δ⟩, ⟨ε.l ↓ δ⟩ ⇒ δ
def uninhabited : QScheme Unit :=
  ⟨["δ"], [⟨f "l" b, .lit "l", tv "δ"⟩, ⟨.empty, .lit "l", tv "δ"⟩], tv "δ"⟩

theorem uninhabited_wf : uninhabited.WF := by
  intro st hst δ hδ
  simp only [uninhabited, List.mem_cons, List.not_mem_nil, or_false] at hst
  rcases hst with rfl | rfl <;> simp_all [uninhabited, tv, Ty.ftv]

theorem uninhabited_no_instance : ¬ ∃ τ, QScheme.Inst uninhabited τ := by
  rintro ⟨τ, θ, -, hQ, -⟩
  have h₁ := hQ ⟨f "l" b, .lit "l", tv "δ"⟩ (by simp [uninhabited])
  have h₂ := hQ ⟨.empty, .lit "l", tv "δ"⟩ (by simp [uninhabited])
  -- the second stump misses, so D-⊥ pins θδ = ★
  have hδ : θ.ty "δ" = .unk := by
    cases h₂ with
    | hit hl _ => simp [Row.applySubst, LookupQ.lab_iff] at hl; cases hl
    | abs _ h => simpa [Ty.applySubst, tv] using h
    | unk hl _ => simp [Row.applySubst, LookupQ.lab_iff] at hl; cases hl
  -- the first hits, so D-hit demands θδ ≈ 𝓫; ★ is ≈ only to itself
  cases h₁ with
  | hit hl he =>
      simp only [f, b, Row.applySubst, Ty.applySubst, Key.applySubst_lit, LookupQ.lab_iff] at hl
      cases hl
      simp only [tv, Ty.applySubst, hδ] at he
      cases he.unk_inv
  | abs hl _ =>
      simp only [f, Row.applySubst, Key.applySubst_lit, LookupQ.lab_iff] at hl
      cases hl with | miss h => exact h rfl
  | unk hl _ =>
      simp only [f, Row.applySubst, Key.applySubst_lit, LookupQ.lab_iff] at hl
      cases hl

-- `let x = (3 4) in 5` (constants for 3, 4, 5): bound to the uninhabited scheme,
-- every premise of T-let but inhabitation holds — instance-closure vacuously —
-- …
def stuckLet : E := .letE "x" (app c c) c

example : uninhabited.WF ∧
    (∀ τ₁, QScheme.Inst uninhabited τ₁ → QTyped σ0 QCtx.empty (app c c) τ₁) ∧
    QTyped σ0 (QCtx.empty.bindScheme "x" uninhabited) c b :=
  ⟨uninhabited_wf, fun τ₁ h => absurd ⟨τ₁, h⟩ uninhabited_no_instance, .qCon⟩

-- … yet the program is stuck: no value, no step, no lookup error
theorem stuckLet_stuck : ¬ Value stuckLet ∧ (¬ ∃ e', Step δ0 stuckLet e') ∧ ¬ Err δ0 stuckLet := by
  refine ⟨?_, ?_, ?_⟩
  · intro h; simp only [stuckLet] at h; cases h
  · rintro ⟨e', h⟩
    simp only [stuckLet, app, InferRuns.c] at h
    cases h with
    | letCong h => cases h with
      | appFun h => cases h
      | appArg _ h => cases h
    | letBeta hv => cases hv
  · intro h
    simp only [stuckLet, app, InferRuns.c] at h
    cases h with
    | letBind h => cases h with
      | appFun h => cases h
      | appArg _ h => cases h

-- … so with the inhabitation premise it has no typing (by progress) …
example : ¬ ∃ τ, QTyped σ0 QCtx.empty stuckLet τ := by
  rintro ⟨τ, h⟩
  obtain ⟨hv, hs, he⟩ := stuckLet_stuck
  cases qProgress σ0 (SigOK.plain _) _ _ h with
  | step h => exact hs ⟨_, h⟩
  | done h => exact hv h
  | err h => exact he h
-- … and inference rejects it
#guard run stuckLet = ("fail: clash", [])

------------------------------------------------------------------------------
-- # §4 Metatheory
------------------------------------------------------------------------------

-- Refinement: `x: ({l = c} ‖ x).l` with x : {β} types at {β} → ★ (the
-- concatenation's row is (β | l: 𝓫), whose lookup stops at β) …
def refineEx : E := lam "x" (sel (cat (rec1 "l" c) (v "x")) "l")
example : QTyped σ0 QCtx.empty refineEx (.fn (.rcd (.var "β")) .unk) :=
  .qLam (.qSelUnk (.qCat (.qRcd (.field .qCon))
    (qvar (τ := .rcd (.var "β")) (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup])))
    (.catUnk .varFree))
-- … carried to {ε} → ★ under [β ≔ ε] (Type substitution) — at ε the lookup
-- hits, so this derivation is T-sel followed by T-★-intro …
example : QTyped σ0 QCtx.empty refineEx (.fn (.rcd .empty) .unk) :=
  .qLam (.qUnk (.qSel (.qCat (.qRcd (.field .qCon))
    (qvar (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup]))) (.catSkip .emp .hit)))
-- … and {ε} → 𝓫_c ⊑ {ε} → ★ becomes derivable
example : QTyped σ0 QCtx.empty refineEx (.fn (.rcd .empty) b) :=
  .qLam (.qSel (.qCat (.qRcd (.field .qCon))
    (qvar (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup]))) (.catSkip .emp .hit))
example : TyPrec (.fn (.rcd .empty) b : T) (.fn (.rcd .empty) .unk) := .fn (.refl _) (.unk _)
-- inference agrees: open, the lookup stays ?; at x = {}, it hits
#guard run refineEx = ("⟨ρ1 | l: 𝓫 | ε.l ↓ α2⟩ ⇒ ({ρ1} → α2)", [])
#guard run (app refineEx emp) = ("𝓫", [])

-- Rigidity of ★, Theorem "No plain scheme is principal", Theorem "Qualified
-- principality" and Corollary "Qualified schemes type strictly more" are the
-- cited theorems; restated here at the thesis's instances.
example (θ : TySubst Unit) : ¬ TyPrec ((Ty.fn (.rcd (rv "β")) .unk).applySubst θ)
    (.fn (.rcd (.sing "l" b)) b) := finalized_no_blur θ (by decide)
example := no_plain_principal_scheme (B := Unit) (C := Unit) (fun _ => ())
example := selQ_principal (B := Unit) σ0
-- The Corollary's program `let f = (x: x.l) in {a = f {l = c}; b = f {}}`
-- is `l1_strictly_weaker`'s witness. (§2's nested variant is `twoUse` above;
-- that no plain scheme covers its two uses is `no_plain_principal_scheme`.)
example := l1_strictly_weaker σ0 ()
#guard run (.letE "f" (lam "x" (sel (v "x") "l")) (rec2 "a" (app (v "f") (rec1 "l" c)) "b" (app (v "f") emp)))
  = ("{a: 𝓫 | b: ★}", ["l"])

------------------------------------------------------------------------------
-- # §5 Unification
------------------------------------------------------------------------------

-- Spines: ((l: 𝓫 | ε) | (α | m: 𝓫)) and (l: 𝓫 | (α | (m: 𝓫 | ε))) are both ⟨l: 𝓫, α, m: 𝓫⟩
example : (Row.cat (.cat (f "l" b) .empty) (.cat (rv "α") (f "m" b))).toSpine =
    [.field "l" b, .var "α", .field "m" b] := rfl
example : (Row.cat (f "l" b) (.cat (rv "α") (.cat (f "m" b) .empty))).toSpine =
    [.field "l" b, .var "α", .field "m" b] := rfl

-- Windows. In ⟨m: 𝓫, l: 𝓫, α, l: 𝓫′⟩ a leading l is paired with l: 𝓫 (the
-- leading window), leaving ⟨m: 𝓫, α, l: 𝓫′⟩ …
example : windowExtract "l" [.field "m" b, .field "l" b, .var "α", .field "l" b'] =
    some (b, [.field "m" b, .var "α", .field "l" b']) := rfl
-- … and in ⟨m: 𝓫, α, l: 𝓫′⟩ it has no partner
example : windowExtract "l" [.field "m" b, .var "α", .field "l" b'] = none := rfl

-- U-var-collapse: α occurs on s only among variables. Once: the others go to
-- ε and α stays free; twice: α goes to ε as well. With a field on s it is
-- U-var-occurs.
#guard tr (rv "α") (row [rv "β", rv "α", rv "γ"]) = (["U-var-collapse"], "[β ≔ ε, γ ≔ ε]")
-- (the driver records α ≔ ε once per occurrence; the duplicates agree)
#guard tr (rv "α") (row [rv "β", rv "α", rv "α", rv "γ"]) =
  (["U-var-collapse"], "[β ≔ ε, α ≔ ε, α ≔ ε, γ ≔ ε]")
#guard tr (rv "α") (row [rv "β", rv "α", f "l" b, rv "γ"]) = (["U-var-occurs"], "Occurs")

-- Example "Matching, then solving": ⟨l: γ, α⟩ ≐ᵣ ⟨l: 𝓫, m: 𝓫⟩
#guard tr (row [f "l" (tv "γ"), rv "α"]) (row [f "l" b, f "m" b]) =
  (["U-field-L", "U-bind", "U-var-solve"], "[γ ≔ 𝓫, α ≔ (m: 𝓫 | ε)]")
#guard agrees (row [f "l" (tv "γ"), rv "α"]) (row [f "l" b, f "m" b])

-- Example "Pairing by counting": ⟨α, l: γ, β⟩ ≐ᵣ ⟨l: 𝓫⟩
#guard tr (row [rv "α", f "l" (tv "γ"), rv "β"]) (f "l" b) =
  (["U-ground", "U-bind", "U-ε-var"], "[γ ≔ 𝓫, α ≔ ε, β ≔ ε]")
#guard agrees (row [rv "α", f "l" (tv "γ"), rv "β"]) (f "l" b)

-- Example "Cancelling a shared tail": (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α)
#guard tr (row [f "l₁" b, rv "α"]) (row [f "l₂" b, rv "α"]) = (["U-var-refl-R", "U-clash"], "Clash")
#guard agrees (row [f "l₁" b, rv "α"]) (row [f "l₂" b, rv "α"])

-- Example "A stuck problem": ⟨α, β⟩ ≐ᵣ ⟨l: 𝓫⟩ — no move fires
#guard tr (row [rv "α", rv "β"]) (f "l" b) = (["U-stuck"], "Stuck")

-- Example "A forced host": (l: 𝓫 | α) ≐ᵣ (m: 𝓫 | β). U-host binds β ≔ (l: 𝓫 | β′)
-- (β′ is the fresh `aa`), then U-var-solve closes ⟨α⟩ ≐ᵣ ⟨m: 𝓫, β′⟩
#guard tr (row [f "l" b, rv "α"]) (row [f "m" b, rv "β"]) =
  (["U-host", "U-var-solve"], "[β ≔ (l: 𝓫 | aa), α ≔ (m: 𝓫 | (aa | ε))]")
#guard agrees (row [f "l" b, rv "α"]) (row [f "m" b, rv "β"])
-- the solution unifies; that it is most general is `host_forced`
example : Unifies (sub [("β", row [f "l" b, rv "β'"]), ("α", row [f "m" b, rv "β'"])])
    (row [f "l" b, rv "α"]) (row [f "m" b, rv "β"]) := unifies_of rfl

------------------------------------------------------------------------------
-- # §6 Inference
------------------------------------------------------------------------------

-- `let f = x: x.l in f {l = c}`: f is bound to selQ, nothing stays parked …
#guard schemeOf (lam "x" (sel (v "x") "l")) = ("∀#1,#2. ⟨ρ1.l ↓ α2⟩ ⇒ ({ρ1} → α2)", 0)
-- … and the use hits (K-hit), without a warning
#guard run (.letE "f" (lam "x" (sel (v "x") "l")) (app (v "f") (rec1 "l" c))) = ("𝓫", [])

-- The side condition Δ_q ∩ Δ = ∅: in `{a = x: x.l; b = let y = c in c}`, the
-- stump of field a is parked when `let y` is reached, and y's scheme does not
-- capture it: ᾱ = ∅, no constraints
#guard (match inferF σ0 50 QCtx.empty runStart (lam "x" (sel (v "x") "l")) with
  | .ok (_, S) =>
      S.parked.length == 1 &&
      (match inferF σ0 50 QCtx.empty S c with
       | .ok (τ, S₁) => let ᾱ := greatestAlpha QCtx.empty S S₁ τ
                        ᾱ.isEmpty && (letQ S₁ ᾱ).isEmpty && (letG S₁ ᾱ).length == 1
       | _ => false)
  | _ => false)

-- Example "Waking, re-parking and finalizing": `r: (x: x.l) (r ‖ {m = c})`.
-- After inference the stump is filed under ρ_r, the row of r (K-repark) …
def reparkEx : E := lam "r" (app (lam "x" (sel (v "x") "l")) (cat (v "r") (rec1 "m" c)))
#guard parkedOf reparkEx = ("({ρ1} → α2)", ["ρ1 ⊳ ⟨m: 𝓫 | ρ1 | ε.l ↓ α2⟩"])
-- … and the answer keeps it; the old `{ρ_r} → ★` is its D-? instance
#guard run reparkEx = ("⟨m: 𝓫 | ρ1 | ε.l ↓ α2⟩ ⇒ ({ρ1} → α2)", [])

-- Example "Why quiescence matters": g generalizes to
--   ∀(ρ_r: Row, δ: Type). ⟨(m: 𝓫_c | ρ_r).l ↓ δ⟩ ⇒ {ρ_r} → δ
#guard schemeOf reparkEx = ("∀#1,#2. ⟨m: 𝓫 | ρ1 | ε.l ↓ α2⟩ ⇒ ({ρ1} → α2)", 0)
#guard run (.letE "g" reparkEx (rec2 "a" (app (v "g") (rec1 "l" c)) "b" (app (v "g") emp))) =
  ("{a: 𝓫 | b: ★}", ["l"])

-- Example "A spent promise": `x: (x.l) c`. A-sel-? parks the stump on x's row
-- and A-app spends its promise on 𝓫_c → γ …
def spentEx' : E := lam "x" (app (sel (v "x") "l") c)
#guard parkedOf spentEx' = ("({ρ1} → α2)", ["ρ1 ⊳ ⟨ρ1.l ↓ (𝓫 → α2)⟩"])
-- … and the answer keeps the constraint; F-hit's {l: 𝓫_c → γ | ρ′} → γ is its
-- fill instance (`InferRuns.spentA_inst_fill`)
#guard run spentEx' = ("⟨ρ1.l ↓ (𝓫 → α2)⟩ ⇒ ({ρ1} → α2)", [])

------------------------------------------------------------------------------
-- # §7 Incompleteness
------------------------------------------------------------------------------

-- Proposition "Irreducible problems": all three are reported stuck
#guard tr (row [rv "α", rv "β"]) (f "l" b) = (["U-stuck"], "Stuck")
#guard tr (row [rv "α", rv "β", rv "γ"]) (f "l" b) = (["U-stuck"], "Stuck")
#guard tr (row [rv "α", f "l" b]) (row [f "l" b, rv "β"]) = (["U-stuck"], "Stuck")
#guard tr (row [rv "α", f "l" b]) (row [f "l" b, rv "α"]) = (["U-stuck"], "Stuck")

-- Example "Unifiers of the irreducible problems".
-- Wand: [α ≔ (l: 𝓫), β ≔ ε] and [α ≔ ε, β ≔ (l: 𝓫)] unify …
example : Unifies (sub [("α", f "l" b), ("β", .empty)]) (row [rv "α", rv "β"]) (f "l" b) :=
  unifies_of rfl
example : Unifies (sub [("α", .empty), ("β", f "l" b)]) (row [rv "α", rv "β"]) (f "l" b) :=
  unifies_of rfl
-- … [α ≔ γ, β ≔ γ′] does not (a variable would face no variable) …
example : ¬ Unifies (sub [("α", rv "γ"), ("β", rv "γ'")]) (row [rv "α", rv "β"]) (f "l" b) :=
  not_equiv_bars (by decide)
-- … nor does [α ≔ ε, β ≔ ε] ((ε | ε) lacks the field)
example : ¬ Unifies (sub [("α", .empty), ("β", .empty)]) (row [rv "α", rv "β"]) (f "l" b) :=
  not_equiv_proj "l" (by decide)
-- Two-sided: [α ≔ ε, β ≔ ε], [α ≔ (l: 𝓫), β ≔ (l: 𝓫)] and
-- [α ≔ (l: 𝓫 | γ), β ≔ (γ | l: 𝓫)] unify
example : Unifies (sub [("α", .empty), ("β", .empty)])
    (row [rv "α", f "l" b]) (row [f "l" b, rv "β"]) := unifies_of rfl
example : Unifies (sub [("α", f "l" b), ("β", f "l" b)])
    (row [rv "α", f "l" b]) (row [f "l" b, rv "β"]) := unifies_of rfl
example : Unifies (sub [("α", row [f "l" b, rv "γ"]), ("β", row [rv "γ", f "l" b])])
    (row [rv "α", f "l" b]) (row [f "l" b, rv "β"]) := unifies_of rfl
-- Shift: [α ≔ (l: 𝓫)ⁿ] unifies, n = 0 … 3
def lpow : Nat → R
  | 0 => .empty
  | n+1 => .cat (f "l" b) (lpow n)
#guard (List.range 4).all fun n =>
  unifiesB (sub [("α", lpow n)]) (row [rv "α", f "l" b]) (row [f "l" b, rv "α"])
example : Unifies (sub [("α", lpow 3)]) (row [rv "α", f "l" b]) (row [f "l" b, rv "α"]) :=
  unifies_of rfl

-- The mirrored crossfield (α | l: 𝓫) ≐ᵣ (β | m: 𝓫) is stuck …
#guard tr (row [rv "α", f "l" b]) (row [rv "β", f "m" b]) = (["U-stuck"], "Stuck")
-- … though [β ≔ (β′ | l: 𝓫), α ≔ (β′ | m: 𝓫)] unifies
example : Unifies (sub [("β", row [rv "β'", f "l" b]), ("α", row [rv "β'", f "m" b])])
    (row [rv "α", f "l" b]) (row [rv "β", f "m" b]) := unifies_of rfl

-- Proposition "Stuck masks an mgu": (k: {β | α} | β) ≐ᵣ (k: {l: 𝓫} | l: 𝓫).
-- U-field-L pairs the k-fields and the emitted {β | α} ≐ {l: 𝓫} is the Wand
-- ambiguity, reported before the residual β ≐ᵣ (l: 𝓫) could solve β
#guard tr (row [f "k" (.rcd (row [rv "β", rv "α"])), rv "β"]) (row [f "k" (.rcd (f "l" b)), f "l" b]) =
  (["U-field-L", "U-rcd", "U-stuck"], "Stuck")
-- …yet [β ≔ (l: 𝓫), α ≔ ε] is a most general unifier
example := stuck_masks_mgu

-- Partially generalized lets. A spent stump blocked on its key is not
-- generalized: `let f = r: a: r.${a} c` binds f monomorphically …
#guard schemeOf (lam "r" (lam "a" (app (sd (v "r") (v "a")) c))) =
  ("∀. ({ρ1} → (⌊κ2⌋ → α3))", 1)
-- … while nested selection `let g = x: (x.l).m` is generalized
#guard schemeOf (lam "x" (sel (sel (v "x") "l") "m")) =
  ("∀#1,#2,#3. ⟨ρ3.m ↓ α2⟩, ⟨ρ1.l ↓ {ρ3}⟩ ⇒ ({ρ1} → α2)", 0)

-- Unrestricted T-★-intro: `g: { a = g {l = c}; b = g {m = c}; }` clashes in
-- inference …
def starIntroEx : E := lam "g" (rec2 "a" (app (v "g") (rec1 "l" c)) "b" (app (v "g") (rec1 "m" c)))
#guard run starIntroEx = ("fail: clash", [])
-- … but types at (★ → 𝓫) → {a: 𝓫 | b: 𝓫}, the parameter type of g sent to ★
example : QTyped σ0 QCtx.empty starIntroEx
    (.fn (.fn .unk b) (.rcd (.cat (.sing "a" b) (.sing "b" b)))) := by
  have hg : QTyped σ0 (QCtx.empty.bindTy "g" (.fn .unk b)) (v "g") (.fn .unk b) :=
    qvar (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup])
  exact .qLam (.qRcd (.cat (.field (.qApp hg (.qUnk (.qRcd (.field .qCon)))))
                           (.field (.qApp hg (.qUnk (.qRcd (.field .qCon)))))))

------------------------------------------------------------------------------
-- # §8 First-Class Labels
------------------------------------------------------------------------------

-- The schemes of the dynamic selector and the dynamic record constructor
#guard schemeOf (lam "a" (lam "x" (sd (v "x") (v "a")))) =
  ("∀#1,#2,#3. ⟨ρ2.κ1 ↓ α3⟩ ⇒ (⌊κ1⌋ → ({ρ2} → α3))", 0)
#guard schemeOf (lam "a" (lam "v" (rd (v "a") (v "v")))) =
  ("∀#1,#2. (⌊κ1⌋ → (α2 → {${κ1}: α2}))", 0)

-- `a: {l = c; m = {}}.${a}`: the stump waits on the key, not on a row …
#guard parkedOf (lam "a" (sd (rec2 "l" c "m" emp) (v "a"))) =
  ("(⌊κ1⌋ → α2)", ["κ1 ⊳ ⟨l: 𝓫 | m: {ε} | ε.κ1 ↓ α2⟩"])
-- … and applying the selector to "l" solves the key, and the stump hits
#guard run (app (lam "a" (sd (rec2 "l" c "m" emp) (v "a"))) (L "l")) = ("𝓫", [])

-- `(a: {l = c}.${a}) c`: the key becomes a label at the selection, the
-- application clashes
#guard run (app (lam "a" (sd (rec1 "l" c) (v "a"))) c) = ("fail: clash", [])

-- `((n: r: r // { ${n} = c; }) "x" { x = {}; }).x`: the computed field shadows
#guard run (sel (app (app (lam "n" (lam "r" (cat (v "r") (rd (v "n") c)))) (L "x")) (rec1 "x" emp)) "x")
  = ("𝓫", [])

-- Unification with keys. U-key-pin solves (l: τ) ≐ᵣ (${α}: τ′) with [α ≔ l] …
#guard tr (f "l" (tv "τ")) (.dsing (.var "α") (tv "τ'")) =
  (["U-key-pin", "U-lab", "U-field-L", "U-bind", "U-ε-var"], "[τ ≔ τ', α ≔ l]")
-- … and (${α}: τ) ≐ᵣ (${β}: τ′) with [α ≔ β]
#guard tr (.dsing (.var "α") (tv "τ")) (.dsing (.var "β") (tv "τ'")) =
  (["U-key-pin", "U-lab", "U-field-L", "U-bind", "U-ε-var"], "[τ ≔ τ', α ≔ β]")
-- U-ε-clash fires on a keyed field
#guard tr .empty (.dsing (.var "α") b) = (["U-ε-clash"], "Clash")
-- (${α}: 𝓫 | m: 𝓫) ≐ᵣ (m: 𝓫 | ${β}: 𝓫) is stuck; [α ≔ l, β ≔ l] solves it …
#guard tr (row [.dsing (.var "α") b, f "m" b]) (row [f "m" b, .dsing (.var "β") b]) =
  (["U-stuck"], "Stuck")
example : Unifies (sub [] [("α", .lit "l"), ("β", .lit "l")])
    (row [.dsing (.var "α") b, f "m" b]) (row [f "m" b, .dsing (.var "β") b]) := unifies_of rfl
-- … and [α ≔ β] does not, since m cannot cross the barrier
example : ¬ Unifies (sub [] [("α", .var "β")])
    (row [.dsing (.var "α") b, f "m" b]) (row [f "m" b, .dsing (.var "β") b]) :=
  not_equiv_proj "m" (by decide)

------------------------------------------------------------------------------
-- # §9 Discussion: the idioms of the introduction
------------------------------------------------------------------------------

-- Overlay: ⟨ρ₂.hello ↓ {ρ₁}⟩ ⇒ α → {ρ₂} → {hello: {meta: {broken: 𝓫} | ρ₁}}; the
-- old α → {hello: {ρ₁} | ρ₂} → … is its fill instance
def overlay : E :=
  lam "self" (lam "super" (rec1 "hello" (cat (sel (v "super") "hello") (rec1 "meta" (rec1 "broken" c)))))
#guard run overlay = ("⟨ρ2.hello ↓ {ρ3}⟩ ⇒ (α1 → ({ρ2} → {hello: {meta: {broken: 𝓫} | ε | ρ3}}))", [])

-- Override: ⟨ρ.buildInputs ↓ δ⟩ ⇒ ({buildInputs: β | ρ} → γ) → (δ → β) → {ρ} → γ;
-- the old (★ → β), with a warning, is its D-? instance
def override : E :=
  lam "mk" (lam "app" (lam "args" (app (v "mk")
    (cat (v "args") (rec1 "buildInputs" (app (v "app") (sel (v "args") "buildInputs")))))))
#guard run override =
  ("⟨ρ2 | ε.buildInputs ↓ α4⟩ ⇒ (({buildInputs: α1 | ε | ρ2} → α3) → ((α4 → α1) → ({ρ2 | ε} → α3)))", [])

-- Conditional extension: (α → {NIX_LDFLAGS: 𝓫} → {ρ}) → α → {ρ | pname: 𝓫}
def condExt : E :=
  lam "opt" (lam "cond" (cat (rec1 "pname" c) (app (app (v "opt") (v "cond")) (rec1 "NIX_LDFLAGS" c))))
#guard run condExt = ("((α1 → ({NIX_LDFLAGS: 𝓫} → {ρ2})) → (α1 → {ρ2 | pname: 𝓫 | ε}))", [])

-- The overlay applied to `{}` and `{ hello = { pname = c; }; }` …
#guard run (app (app overlay emp) (rec1 "hello" (rec1 "pname" c))) =
  ("{hello: {meta: {broken: 𝓫} | ε | pname: 𝓫 | ε}}", [])
-- … and to `{}` and `{}`: a clash
#guard run (app (app overlay emp) emp) = ("fail: clash", [])

------------------------------------------------------------------------------
-- # Appendix: MacroNix
------------------------------------------------------------------------------

-- The conditional extension with `if` instead of a parameter is rejected: the
-- branches {NIX_LDFLAGS: string} and {ε} do not unify
section macronix
open MacroNix
#guard (match runF sig 50
    (.cat (.rcd (.field "pname" (.con (.str "hello"))))
          (.ite (.bool true) (.rcd (.field "NIX_LDFLAGS" (.con (.str "-liconv")))) (.rcd .empty))) with
  | .fail "clash" => true | _ => false)
example := macroSig_ok
end macronix

end ThesisExamples
end MinimalCalculus
