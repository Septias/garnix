-- DEFINITIONS. The row-unification algorithm and the vocabulary its theorems
-- are stated in — detectors, the fresh-name supply, solutions (`Sol`), the
-- result type (`UResM`), the mutual driver `unifyTyF` / `unifySpineMF`, and the
-- entry points. Proof-only gadgets stay with the proofs that need them
-- (`revRow` in Reflection, `TySubst.setTy`/`setRow` in Solutions).
--
-- No proofs live here: everything is structural, so this file is the spec you
-- can read on its own. Part of RowUnify; see RowUnify.lean for the overview.

import minimal
import RowEquiv

namespace MinimalCalculus

------------------------ THE ROW UNIFICATION ALGORITHM ------------------------
-- ≐ᵣ Works on spines; every
-- step is FORCED (solution-set preserving):
--   * strip a shared var off either end            (U-var-refl; cancel_cat_*)
--   * solve a whole-var remainder, occurs-checked  (U-var-solve)
--   * match a leading/trailing field against the other side's window
--     (= leading/trailing segment)                 (U-field, both ends)
--   * one side exhausted: remaining vars ≔ ε,
--     remaining fields clash                       (U-ε-var)
--   * ground-side counting: if one side is var-free and a label has EQUAL
--     positive field-counts on both sides, the other side's vars are l-free
--     by counting and the pairing is positional — match first occurrences.
--     (U-ground: this is worked example 2's "var count must collapse" made
--     into an explicit rule; the window rules alone do not cover it, which
--     the mechanization surfaced.)
--   * global projection clash                      (U-clash)
--   * otherwise stuck                              (U-stuck, Wand ambiguity)
--
--   * unique-host variable EXPANSION, Rémy-style, when exactly one variable on
--     the other side can carry the label and that side has none  (U-expand)
--
-- No LUtail: field demands never flow through ≐ᵣ (they park as stumps), so
-- the algorithm never guesses a field into a var. Type equations are SOLVED on
-- the spot by the type pass and applied to the residual — that mutual recursion
-- is what makes a `.stuck` verdict mean something.
--
-- Fuel is EXPLICIT at the entry points (structural recursion ⟹ the algorithm
-- computes by rfl) and exhaustion is its own verdict, `outOfFuel`.

-- ## Spine measurements
def sHasVar {B : Type} : List (Atom B) → Bool
  | [] => false
  | .var _ :: _ => true
  | .field _ _ :: s => sHasVar s

def sFieldCount {B : Type} (l : Label) : List (Atom B) → Nat
  | [] => 0
  | .field l' _ :: s => (if l' = l then 1 else 0) + sFieldCount l s
  | .var _ :: s => sFieldCount l s

def sLabels {B : Type} : List (Atom B) → List Label
  | [] => []
  | .field l _ :: s => l :: sLabels s
  | .var _ :: s => sLabels s

-- U-clash, projection-based (checked globally, not per-window — cf. the
-- (β | l: Int | α) ≐ᵣ (l′: Bool) example for why).
def projClash {B : Type} (s₁ s₂ : List (Atom B)) : Bool :=
  (sLabels s₁ ++ sLabels s₂).any fun l =>
    (decide (sFieldCount l s₂ < sFieldCount l s₁) && !sHasVar s₂) ||
    (decide (sFieldCount l s₁ < sFieldCount l s₂) && !sHasVar s₁)

-- ## Move detectors
-- U-ε-var: every var of an exhausted-side remainder is forced to ε; a
-- leftover field has nowhere to come from.
def allVarsEmpty {B : Type} : List (Atom B) → Option (List (TyVar × Row B))
  | [] => some []
  | .var α :: s => (allVarsEmpty s).map ((α, Row.empty) :: ·)
  | .field _ _ :: _ => none

-- U-var-refl at the left end.
def stripL {B : Type} : List (Atom B) → List (Atom B) →
    Option (List (Atom B) × List (Atom B))
  | .var α :: t₁, .var β :: t₂ => if α = β then some (t₁, t₂) else none
  | _, _ => none

-- … and at the right end (trace monoids cancel on both sides).
def stripR {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (List (Atom B) × List (Atom B)) :=
  match stripL s₁.reverse s₂.reverse with
  | some (t₁, t₂) => some (t₁.reverse, t₂.reverse)
  | none => none

-- First l-field of the WINDOW (leading segment): the search stops at a var.
def windowExtract {B : Type} (l : Label) :
    List (Atom B) → Option (Ty B × List (Atom B))
  | [] => none
  | .var _ :: _ => none
  | .field l' τ :: s =>
      if l' = l then some (τ, s)
      else match windowExtract l s with
        | some (τ', s') => some (τ', .field l' τ :: s')
        | none => none

-- U-field at the left end: leading field of one side matched against the
-- first same-label occurrence in the other side's window.
def matchL {B : Type} : List (Atom B) → List (Atom B) →
    Option (Ty B × Ty B × List (Atom B) × List (Atom B))
  | .field l τ :: t₁, s₂ =>
      match windowExtract l s₂ with
      | some (τ', s₂') => some (τ, τ', t₁, s₂')
      | none => none
  | _, _ => none

-- … and at the right end.
def matchR {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (Ty B × Ty B × List (Atom B) × List (Atom B)) :=
  match matchL s₁.reverse s₂.reverse with
  | some (τ, τ', t₁, t₂) => some (τ, τ', t₁.reverse, t₂.reverse)
  | none => none

-- First l-field ANYWHERE in the spine (vars skipped), removed — the U-ground
-- pairing is positional among concrete fields once counting rules the vars out.
def removeField {B : Type} (l : Label) :
    List (Atom B) → Option (Ty B × List (Atom B))
  | [] => none
  | .var β :: s =>
      match removeField l s with
      | some (τ, s') => some (τ, .var β :: s')
      | none => none
  | .field l' τ :: s =>
      if l' = l then some (τ, s)
      else match removeField l s with
        | some (τ', s') => some (τ', .field l' τ :: s')
        | none => none

def groundMatchAux {B : Type} (s₁ s₂ : List (Atom B)) :
    List Label → Option (Ty B × Ty B × List (Atom B) × List (Atom B))
  | [] => none
  | l :: ls =>
      if sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ then
        match removeField l s₁, removeField l s₂ with
        | some (τ, t₁), some (τ', t₂) => some (τ, τ', t₁, t₂)
        | _, _ => groundMatchAux s₁ s₂ ls
      else groundMatchAux s₁ s₂ ls

-- U-ground: s₂ var-free, some label with equal positive counts.
def groundMatch {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (Ty B × Ty B × List (Atom B) × List (Atom B)) :=
  if sHasVar s₂ then none else groundMatchAux s₁ s₂ (sLabels s₁)

-- ## U-expand: unique-host variable expansion
-- The DETECTORS only; the metatheory (host_forced, expand_shift, the two
-- reflection lemmas) is in Solutions.lean.
--
-- The fresh names are drawn from a supply derived LOCALLY from the problem
-- (localSupply) and THREADED through the driver: deriving it per call from the
-- current problem is non-monotone, since a move that drops a field drops its
-- type's variables and the bound can fall below a name still in scope
--.


structure Supply where
  next : Nat

/-- Hand out a name and advance. -/
def Supply.fresh (S : Supply) : TyVar × Supply := (natName S.next, ⟨S.next + 1⟩)

def sFtv {B : Type} : List (Atom B) → List TyVar
  | [] => []
  | .field _ τ :: s => τ.ftv ++ sFtv s
  | .var α :: s     => α :: sFtv s

-- The names this problem may invent: strictly longer than everything in it.
def localSupply {B : Type} (s₁ s₂ : List (Atom B)) : Supply :=
  ⟨lenBound (sFtv s₁ ++ sFtv s₂) + 1⟩


def renameVar {B : Type} (β β' : TyVar) : List (Atom B) → List (Atom B)
  | [] => []
  | .var γ :: s => (if γ = β then Atom.var β' else Atom.var γ) :: renameVar β β' s
  | .field l τ :: s => .field l τ :: renameVar β β' s

def uniqueHost {B : Type} (l : Label) (s : List (Atom B)) : Option TyVar :=
  match sVarSeq s with
  | [β] => if sFieldCount l s = 0 then some β else none
  | _   => none

def expandL {B : Type} (S : Supply) :
    List (Atom B) → List (Atom B) →
    Option (TyVar × Label × Ty B × List (Atom B) × List (Atom B))
  | .field l τ :: t₁, s₂ =>
      match uniqueHost l s₂ with
      | some β => some (β, l, τ, t₁, renameVar β S.fresh.2.fresh.1 s₂)
      | none => none
  | _, _ => none

-- … and at the right end (the expansion is then β ≔ (β′ | l:δ)).
def expandR {B : Type} (S : Supply) (s₁ s₂ : List (Atom B)) :
    Option (TyVar × Label × Ty B × List (Atom B) × List (Atom B)) :=
  match expandL S s₁.reverse s₂.reverse with
  | some (β, l, τ, t₁, t₂) => some (β, l, τ, t₁.reverse, t₂.reverse)
  | none => none




------------------------- mgu, AS A PREDICATE --------------------------------

-- ## No-mgu depends only on the unifier SET
-- `HasMgu` packages "a most general unifier exists". Crucially InstanceOf never
-- mentions the rows — only θ's action on variables — so two unification problems
-- with the SAME unifiers (as substitutions) have the SAME mgu-status. This is the
-- vehicle for lifting stuck⟹no-mgu through the unifier-set-PRESERVING moves.
def HasMgu {B : Type} (ρ₁ ρ₂ : Row B) : Prop :=
  ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ ∧
    ∀ θ' : TySubst B, Unifies θ' ρ₁ ρ₂ → InstanceOf θ' θ

-- ## Predicate-based mgu: lifting no-mgu through the eq-EMITTING moves
-- The strip moves preserve the unifier set exactly, so `hasMgu_congr` (stated on
-- two ROW problems) discharges them. matchL/matchR/groundMatch instead emit a
-- type equation: a unifier of the original is EXACTLY a unifier of the residual
-- row problem that ALSO satisfies `τ ≐ τ'`. That is still a set of substitutions,
-- just not the unifier set of a bare row equation. Since `InstanceOf` mentions
-- only the substitutions (never the rows), mgu-status is a property of that SET,
-- whatever cuts it out. So we generalize `HasMgu` to an arbitrary unifier
-- PREDICATE and get a congruence that covers the eq-emitting moves for free.
def HasMguP {B : Type} (P : TySubst B → Prop) : Prop :=
  ∃ θ : TySubst B, P θ ∧ ∀ θ' : TySubst B, P θ' → InstanceOf θ' θ


------------------- SOLUTIONS, SUBSTITUTIONS, THE RESULT TYPE -----------------

def SolSat {B : Type} (θ : TySubst B) (σ : List (TyVar × Row B)) : Prop :=
  ∀ p ∈ σ, RowEquiv (θ.row p.1) (p.2.applySubst θ)

def EqsSat {B : Type} (θ : TySubst B) (eqs : List (Ty B × Ty B)) : Prop :=
  ∀ p ∈ eqs, TyEquiv (p.1.applySubst θ) (p.2.applySubst θ)

-- ## ≗ : pointwise ≈-equality of substitutions
def SubstEquiv {B : Type} (θ₁ θ₂ : TySubst B) : Prop :=
  (∀ α, TyEquiv (θ₁.ty α) (θ₂.ty α)) ∧ (∀ α, RowEquiv (θ₁.row α) (θ₂.row α))

infix:50 " ≗ " => SubstEquiv

-- ## Solutions at both sorts
-- Row bindings, plus the type bindings the driver solves on the spot. One
-- shared TyVar namespace (minimal.lean:649), so a variable bound by the type
-- pass is readable by the row pass.
structure Sol (B : Type) where
  ty  : List (TyVar × Ty B)
  row : List (TyVar × Row B)

def Sol.nil {B : Type} : Sol B := ⟨[], []⟩

-- The old row-only solution, embedded.
def Sol.ofRow {B : Type} (σ : List (TyVar × Row B)) : Sol B := ⟨[], σ⟩

-- Association lists, resolved with `if β = α` rather than List.lookup so the
-- membership spec below is a two-line structural induction.
def tyLookup {B : Type} (α : TyVar) : List (TyVar × Ty B) → Ty B
  | [] => .var α
  | (β, τ) :: t => if β = α then τ else tyLookup α t

def rowLookup {B : Type} (α : TyVar) : List (TyVar × Row B) → Row B
  | [] => .var α
  | (β, ρ) :: t => if β = α then ρ else rowLookup α t

-- The substitution a solution denotes: bound variables go to their binding,
-- every other variable stays free.
def Sol.toSubst {B : Type} (s : Sol B) : TySubst B :=
  ⟨fun α => tyLookup α s.ty, fun α => rowLookup α s.row⟩

-- SolSat at both sorts.
def Sol.Sat {B : Type} (θ : TySubst B) (s : Sol B) : Prop :=
  (∀ p ∈ s.ty, TyEquiv (θ.ty p.1) (p.2.applySubst θ)) ∧
  (∀ p ∈ s.row, RowEquiv (θ.row p.1) (p.2.applySubst θ))

-- ## Composing solutions
-- s₂.comp s₁ — first s₁, then s₂ — the Sol-level image of TySubst.comp
-- (minimal.lean:1669): push s₂ through s₁'s bindings, then keep s₂'s own.
def Sol.comp {B : Type} (s₂ s₁ : Sol B) : Sol B :=
  ⟨s₁.ty.map  (fun p => (p.1, p.2.applySubst s₂.toSubst)) ++ s₂.ty,
   s₁.row.map (fun p => (p.1, p.2.applySubst s₂.toSubst)) ++ s₂.row⟩

-- ## The result type of the mutual driver
-- A success carries the SUPPLY it stopped at, so the fresh names invented by
-- one sub-call are not handed out again by the next (a type equation solved
-- inside a field may expand a row variable, and the invented tail then travels
-- into the residual). `outOfFuel` is the fifth verdict: it separates "the
-- algorithm ran out of budget" from "every move is dead", which is what makes
-- the fuel lemma a structural induction rather than a termination measure.
inductive UResM (B : Type) : Type where
  | success   : Sol B → Supply → UResM B
  | clash     : UResM B
  | occurs    : UResM B
  | stuck     : UResM B
  | outOfFuel : UResM B

-- Sequencing, as used by every eq-emitting arm: run the second stage
-- under the first stage's solution AND its supply, then compose. A non-success
-- in either stage is the verdict of the whole.
def UResM.seq {B : Type} : UResM B → (TySubst B → Supply → UResM B) → UResM B
  | .success s S, k =>
      match k s.toSubst S with
      | .success s' S' => .success (s'.comp s) S'
      | r => r
  | r, _ => r

-- ## Unification at the type sort
-- The ≐ counterpart of Unifies (RowEquiv.lean:543); EqsSat is exactly a list
-- of these.
def TyUnifies {B : Type} (θ : TySubst B) (τ τ' : Ty B) : Prop :=
  TyEquiv (τ.applySubst θ) (τ'.applySubst θ)

-- ## Substitution on spines
-- A var atom expands to a whole spine, so this is not a map: it is the spine
-- image of Row.applySubst. Written by structural recursion (not via
-- ofSpine/toSpine) so the regressions keep reducing by rfl.
def sApplySubst {B : Type} (θ : TySubst B) : List (Atom B) → List (Atom B)
  | [] => []
  | .field l τ :: s => .field l (τ.applySubst θ) :: sApplySubst θ s
  | .var α :: s     => (θ.row α).toSpine ++ sApplySubst θ s

-- ## The supply
/-- The invariant: every name `S` can still produce is longer than everything
in `avoid`, hence fresh for it. -/
def Supply.Avoids (S : Supply) (avoid : List TyVar) : Prop := lenBound avoid < S.next

-- The initial supply of a row problem, and the invariant it establishes.
def initSupply {B : Type} (ρ₁ ρ₂ : Row B) : Supply := ⟨lenBound (ρ₁.ftv ++ ρ₂.ftv) + 1⟩


------------------------------ AGREEMENT -------------------------------------

-- ## Agreement: the completeness statement must allow EXTENSION
-- U-expand invents δ and β′, so a unifier of the problem cannot literally meet
-- the emitted solution — it says nothing about names the problem never had. The
-- honest mgu statement is "every unifier EXTENDS to one that meets σ and eqs,
-- without moving on the problem's own variables". `V` is any variable set the
-- problem lives inside; it is what the extension promises to leave alone.
def AgreeOn {B : Type} (θ θ' : TySubst B) (V : List TyVar) : Prop :=
  ∀ α ∈ V, θ.ty α = θ'.ty α ∧ θ.row α = θ'.row α


--------------------- THE MUTUAL ≐ / ≐ᵣ DRIVER --------------------------------

-- ## Binding a type variable, occurs-checked
-- α ≐ α is vacuous; otherwise α ≔ τ, guarded. ftv spans BOTH sorts
-- (minimal.lean:691), so `α ≐ {… α …}` is rejected even when the inner α is a
-- row variable and the problem is in fact solvable — the same conservatism the
-- row occurs guard has (occurs_allVar_hasMgu). Deliberate: the guard is what
-- makes a binding eliminate its variable.
def tyIsVar {B : Type} : Ty B → Option TyVar
  | .var β => some β
  | _      => none

-- Written as a chain of `if`s rather than a match on τ, so it has ONE
-- unconditional equation and its soundness/completeness proofs never case-split
-- on the shape of τ.
def bindTy {B : Type} (S : Supply) (α : TyVar) (τ : Ty B) : UResM B :=
  if tyIsVar τ = some α then .success .nil S
  else if τ.ftv.contains α then .occurs
  else .success ⟨[(α, τ)], []⟩ S

-- ## U-var-solve, at the mutual driver's result type
def solveVarM {B : Type} (S : Supply) : List (Atom B) → List (Atom B) → Option (UResM B)
  | [.var α], s₂ =>
      some (if (sVarSeq s₂).contains α then .occurs
            else .success (Sol.ofRow [(α, ofSpine s₂)]) S)
  | _, _ => none

-- ## U-expand, at the mutual driver's result type
-- δ is FRESH, so the equation τ ≐ δ has the one solution δ ≔ τ; solving it
-- eagerly is what the driver does everywhere else, and it keeps P3's metatheory
-- (which invents δ) applicable verbatim. The recursive solution is composed ON
-- TOP, so the expansion's own binding sees whatever the residual did to β′.
def expandResM {B : Type} (S : Supply) (β : TyVar) (l : Label) (τ : Ty B) :
    UResM B → UResM B
  | .success s S' =>
      .success (s.comp ⟨[(S.fresh.1, τ)],
                        [(β, .cat (.sing l (.var S.fresh.1)) (.var S.fresh.2.fresh.1))]⟩) S'
  | r => r

-- ## The driver
-- unifyTyF is ≐; unifySpineMF is ≐ᵣ. Both consume one unit of fuel per
-- cross-call, so the block is STRUCTURALLY recursive on fuel — which is what
-- keeps the regressions kernel-checked `rfl` executions.
mutual

def unifyTyF {B : Type} [DecidableEq B] (S : Supply) (fuel : Nat) : Ty B → Ty B → UResM B
  -- The fuel is consumed in the two RECURSIVE arms only, so the match on it
  -- sits inside: every other verdict is reached at any fuel, and the fuel
  -- lemma below then has exactly two interesting cases.
  | .var α, τ₂ => bindTy S α τ₂
  | τ₁, .var α => bindTy S α τ₁
  -- ★ is RIGID: it unifies with itself and clashes with everything else
  | .unk, .unk => .success .nil S
  | .base b, .base b' => if b = b' then .success .nil S else .clash
  | .fn a₁ b₁, .fn a₂ b₂ =>
      match fuel with
      | 0 => .outOfFuel
      | f+1 =>
          (unifyTyF S f a₁ a₂).seq fun θ S' =>
            unifyTyF S' f (b₁.applySubst θ) (b₂.applySubst θ)
  | .rcd ρ₁, .rcd ρ₂ =>
      match fuel with
      | 0 => .outOfFuel
      | f+1 => unifySpineMF S f ρ₁.toSpine ρ₂.toSpine
  | _, _ => .clash

def unifySpineMF {B : Type} [DecidableEq B] :
    Supply → Nat → List (Atom B) → List (Atom B) → UResM B
  | S, _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => .success (Sol.ofRow σ) S
      | none   => .clash
  | S, _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => .success (Sol.ofRow σ) S
      | none   => .clash
  | _, 0, _, _ => .outOfFuel
  | S, fuel+1, s₁, s₂ =>
      match stripL s₁ s₂ with
      | some (t₁, t₂) => unifySpineMF S fuel t₁ t₂
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => unifySpineMF S fuel t₁ t₂
      | none =>
      match solveVarM S s₁ s₂ with
      | some r => r
      | none =>
      match solveVarM S s₂ s₁ with
      | some r => r
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (unifyTyF S fuel τ τ').seq fun θ S' =>
            unifySpineMF S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match expandL S s₁ s₂ with
      | some (β, l, τ, t₁, t₂) =>
          expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂)
      | none =>
      match expandL S s₂ s₁ with
      | some (β, l, τ, t₁, t₂) =>
          expandResM S β l τ (unifySpineMF S.fresh.2.fresh.2 fuel t₁ t₂)
      | none =>
      if projClash s₁ s₂ then .clash else .stuck

end

-- ## Entry points
-- Fuel stays EXPLICIT at the top level. No closed-form bound: solve-and-apply
-- defeats it (see the note below unifyM_fuel_mono), and it is not needed —
-- `outOfFuel` makes every reached verdict fuel-independent.
def unifySpineM {B : Type} [DecidableEq B] (fuel : Nat) (s₁ s₂ : List (Atom B)) : UResM B :=
  unifySpineMF (localSupply s₁ s₂) fuel s₁ s₂

def unifyRowM {B : Type} [DecidableEq B] (fuel : Nat) (ρ₁ ρ₂ : Row B) : UResM B :=
  unifySpineM fuel ρ₁.toSpine ρ₂.toSpine

def unifyTyM {B : Type} [DecidableEq B] (fuel : Nat) (τ τ' : Ty B) : UResM B :=
  unifyTyF ⟨lenBound (τ.ftv ++ τ'.ftv) + 1⟩ fuel τ τ'

/-- `Mono r r'`: `r` is what the algorithm answered on some budget and `r'` on a
larger one — either the smaller run ran out, or the two agree. -/
def UResM.Mono {B : Type} (r r' : UResM B) : Prop := r = .outOfFuel ∨ r' = r


--------------------- WHAT A SOLUTION MENTIONS --------------------------------

-- ## What a solution mentions
-- Keys and ranges, at both sorts. Kept as a PREDICATE rather than a list: it is
-- only ever used inside a `⊆ W`, and a predicate needs no membership algebra.
def SolMentions {B : Type} (s : Sol B) (γ : TyVar) : Prop :=
  (∃ p ∈ s.ty, γ = p.1 ∨ γ ∈ p.2.ftv) ∨ (∃ p ∈ s.row, γ = p.1 ∨ γ ∈ p.2.ftv)

/-- `SolBelow s W`: every name the solution mentions is already in `W`. -/
def SolBelow {B : Type} (s : Sol B) (W : List TyVar) : Prop :=
  ∀ γ, SolMentions s γ → γ ∈ W

end MinimalCalculus
