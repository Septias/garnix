-- The row-unification algorithm: spine measurements, move detectors, and the U-expand supply.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

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
-- is what makes a `.stuck` verdict mean something (proof-plan.md §0).
--
-- Presentation uses fuel (structural recursion ⟹ the algorithm computes by
-- rfl; the regressions are kernel-checked executions). Fuel is EXPLICIT at the
-- entry points and exhaustion is its own verdict, `outOfFuel`; unifyM_fuel_mono
-- says a verdict that was reached never changes when the budget grows.

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

-- ## U-expand: unique-host variable expansion (proof-plan.md §1.4)
-- The DETECTORS only; the metatheory (host_forced, expand_shift, the two
-- reflection lemmas) is in the P3 section at the end of the file. They sit here
-- because the dispatch cascade below needs them.
--
-- The fresh names are drawn from a supply derived LOCALLY from the problem
-- (localSupply) and THREADED through the driver: deriving it per call from the
-- current problem is non-monotone, since a move that drops a field drops its
-- type's variables and the bound can fall below a name still in scope
-- (proof-plan.md §4-P3b(1)).


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


end MinimalCalculus
