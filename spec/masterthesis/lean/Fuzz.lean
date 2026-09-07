-- FUZZ: an executable search for DIVERGENCE in the ≐ᵣ / ≐ driver.
--
-- Not part of the library (`lake build` does not touch it). Run with
--
--     lake build fuzz && lake exe fuzz
--
-- ## Why
-- Phase D of proof-plan.md: there is no closed-form fuel bound, the naive Rémy
-- measure does not close, and the lexicographic measure was retracted because
-- solve-and-apply grows the spine AND the variable count. Before designing a
-- third measure, settle the prior question — is the driver terminating at all?
-- A divergent input is as valuable a result as a bound, and cheaper to find.
--
-- ## What it does, per pair of spines drawn from a small universe
--  1. DIVERGENCE HUNT. `minFuel` = the least budget at which the pair reaches a
--     verdict, by binary search up to `cap`. A pair still `outOfFuel` at `cap`
--     — with `cap` far above its size — is a divergence CANDIDATE and gets
--     printed. Zero survivors is evidence for termination, not a proof.
--  2. FUEL-MONOTONICITY TRIPWIRE. `unifyM_fuel_mono` says a verdict once
--     REACHED never changes as the budget grows. So the full result (verdict
--     AND solution) at `minFuel` must equal the one at `cap`. Any mismatch is a
--     counterexample to a theorem in the build — print it loudly.
--  3. FUEL PROFILE. The max `minFuel` observed per problem size |s₁|+|s₂|.
--     This is the empirical shape of the bound a measure would have to beat: if
--     it is flat or linear the Rémy-style argument is worth the effort, if it
--     climbs fast the bound is not what one would guess.
--
-- The binary search in (1) and (3) is justified by `unifyM_fuel_mono`:
-- "reaches a verdict" is monotone in the budget. (2) is what checks that.

import RowUnify

namespace MinimalCalculus
namespace Fuzz

abbrev Spine := List (Atom Unit)

---------------------------------- PRINTING -----------------------------------

mutual
def tyStr : Ty Unit → String
  | .var α    => α
  | .base _   => "𝓫"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyStr τ₁ ++ "→" ++ tyStr τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowStr ρ ++ "}"

def rowStr : Row Unit → String
  | .empty     => "ε"
  | .var α     => α
  | .sing l τ  => l ++ ":" ++ tyStr τ
  | .cat ρ₁ ρ₂ => rowStr ρ₁ ++ " | " ++ rowStr ρ₂
end

def atomStr : Atom Unit → String
  | .field l τ => l ++ ":" ++ tyStr τ
  | .var α     => α

def spineStr (s : Spine) : String :=
  "(" ++ String.intercalate " | " (s.map atomStr) ++ ")"

def solStr (s : Sol Unit) : String :=
  "[" ++ String.intercalate ", " (s.ty.map  fun p => p.1 ++ "≔" ++ tyStr p.2) ++
  " ; " ++ String.intercalate ", " (s.row.map fun p => p.1 ++ "≔" ++ rowStr p.2) ++ "]"

/-- The FULL result, solution included — the tripwire compares these. -/
def resStr : UResM Unit → String
  | .success s S => "success " ++ solStr s ++ " @supply " ++ toString S.next
  | .clash       => "clash"
  | .occurs      => "occurs"
  | .stuck       => "stuck"
  | .outOfFuel   => "outOfFuel"

def verdictStr : UResM Unit → String
  | .success _ _ => "success"
  | .clash       => "clash"
  | .occurs      => "occurs"
  | .stuck       => "stuck"
  | .outOfFuel   => "outOfFuel"

--------------------------------- GENERATION ----------------------------------
-- Exhaustive, not random: reproducible, and at these sizes complete coverage of
-- the universe is cheaper than a generator plus a seed to remember.

structure Universe where
  name   : String
  vars   : List TyVar          -- row variables usable as spine atoms
  labels : List Label
  tys    : List (Ty Unit)      -- field payloads
  maxLen : Nat                 -- spines of length 0 .. maxLen, both sides

/-- Atom alphabet: every row variable, and every label at every payload. -/
def Universe.atoms (U : Universe) : List (Atom Unit) :=
  U.vars.map .var ++ U.labels.flatMap fun l => U.tys.map (Atom.field l ·)

def spinesOfLen (as : List (Atom Unit)) : Nat → List Spine
  | 0     => [[]]
  | n + 1 => (spinesOfLen as n).flatMap fun s => as.map (· :: s)

def allSpines (as : List (Atom Unit)) (n : Nat) : List Spine :=
  (List.range (n + 1)).flatMap (spinesOfLen as)

---------------------------------- THE SEARCH ---------------------------------

def reached : UResM Unit → Bool
  | .outOfFuel => false
  | _          => true

/-- Least `f ∈ [lo, hi]` with `p f`, assuming `p hi`. `gas` is the structural
    recursion budget; `hi - lo + 1` always suffices. -/
def leastSat (p : Nat → Bool) : Nat → Nat → Nat → Nat
  | 0,     _,  hi => hi
  | g + 1, lo, hi =>
      if hi ≤ lo then lo
      else
        let mid := (lo + hi) / 2
        if p mid then leastSat p g lo mid else leastSat p g (mid + 1) hi

/-- The least budget at which the pair reaches a verdict, or `none` if it is
    still `outOfFuel` at `cap`. Binary search is sound here by
    `unifyM_fuel_mono` — and step (2) of the report checks that. -/
def minFuel (cap : Nat) (s₁ s₂ : Spine) : Option Nat :=
  let p := fun f => reached (unifySpineM f s₁ s₂)
  if p cap then some (leastSat p (cap + 1) 0 cap) else none

/-- Record-nesting depth of a spine, via the ≈-invariant `Row.rcdDepth`
    (NoMgu.lean). Kept because it is the only ≈-INVARIANT size in the tree
    (`RowEquiv.rcdDepth_eq`) — but it is NOT enough for a fuel bound: its `.fn`
    case takes a `max`, so arrow nesting costs it nothing, and the `fn-d` family
    below duly breaks any bound built from it. Reported for contrast. -/
def sDepth (s : Spine) : Nat := Row.rcdDepth (ofSpine s)

mutual
/-- Constructor count of a type. Not ≈-invariant (`comm` reorders, `unitL/R`
    delete), so a bound stated with it would have to be read on the SPINE the
    driver actually holds, not up to ≈ — a real cost to pay later. -/
def tySize : Ty Unit → Nat
  | .var _    => 1
  | .base _   => 1
  | .unk      => 1
  | .fn τ₁ τ₂ => 1 + tySize τ₁ + tySize τ₂
  | .rcd ρ    => 1 + rowSize ρ

def rowSize : Row Unit → Nat
  | .empty     => 1
  | .var _     => 1
  | .sing _ τ  => 1 + tySize τ
  | .cat ρ₁ ρ₂ => 1 + rowSize ρ₁ + rowSize ρ₂
end

def atomSize : Atom Unit → Nat
  | .var _     => 1
  | .field _ τ => 1 + tySize τ

def sSize (s : Spine) : Nat := (s.map atomSize).foldl (· + ·) 0

/-- CANDIDATE BOUND A, tight. Read off the parametric families: `cancel-n` and
    `crossfield-n` demand `n+1`, `nest-d` demands `2d+1`, `fn-d` demands `d+1` —
    and in each case that is at most half the total constructor count. A tight
    conjecture is the useful kind: it gets refuted, and the refutation names the
    ingredient the measure is missing. -/
def boundA (s₁ s₂ : Spine) : Nat := (sSize s₁ + sSize s₂) / 2 + 1

/-- CANDIDATE BOUND B, loose — the same shape with no halving. If A falls and B
    stands, the bound is linear in the problem with a worse constant. -/
def boundB (s₁ s₂ : Spine) : Nat := sSize s₁ + sSize s₂ + 1

structure Stats where
  total     : Nat := 0
  success   : Nat := 0
  clash     : Nat := 0
  occurs    : Nat := 0
  stuck     : Nat := 0
  /-- still `outOfFuel` at `cap` — divergence candidates -/
  survivors : List (Spine × Spine) := []
  nSurv     : Nat := 0
  /-- result at `minFuel` ≠ result at `cap`: refutes `unifyM_fuel_mono` -/
  monoViol  : List (Spine × Spine × String × String) := []
  nViol     : Nat := 0
  /-- (problem size, max minFuel seen at that size) -/
  bySize    : List (Nat × Nat) := []
  /-- the single hungriest pair -/
  worst     : Option (Nat × Spine × Spine) := none
  /-- minFuel > boundA: refutes the tight candidate -/
  boundViol : List (Nat × Nat × Spine × Spine) := []
  nAViol    : Nat := 0
  /-- minFuel > boundB: refutes the loose candidate too -/
  nBViol    : Nat := 0
  /-- the smallest slack `boundA - minFuel` seen — how close A came -/
  tightest  : Option (Nat × Nat × Spine × Spine) := none

def bumpMax : List (Nat × Nat) → Nat → Nat → List (Nat × Nat)
  | [],            k, v => [(k, v)]
  | (k', v') :: t, k, v => if k' = k then (k', max v v') :: t else (k', v') :: bumpMax t k v

def keep : Nat := 12   -- how many witnesses of each kind to retain

def step (cap : Nat) (s₁ s₂ : Spine) (st : Stats) : Stats :=
  let st := { st with total := st.total + 1 }
  match minFuel cap s₁ s₂ with
  | none   =>
      { st with survivors := if st.nSurv < keep then (s₁, s₂) :: st.survivors else st.survivors
                nSurv     := st.nSurv + 1 }
  | some f =>
      let rMin := resStr (unifySpineM f s₁ s₂)
      let rCap := resStr (unifySpineM cap s₁ s₂)
      let st := if rMin == rCap then st else
        { st with monoViol := if st.nViol < keep then (s₁, s₂, rMin, rCap) :: st.monoViol
                              else st.monoViol
                  nViol    := st.nViol + 1 }
      let bnd := boundA s₁ s₂
      let st := if f ≤ bnd then st else
        { st with boundViol := if st.nAViol < keep then (f, bnd, s₁, s₂) :: st.boundViol
                               else st.boundViol
                  nAViol    := st.nAViol + 1 }
      let st := if f ≤ boundB s₁ s₂ then st else { st with nBViol := st.nBViol + 1 }
      let st := if f ≤ bnd then
                  let slack := bnd - f
                  match st.tightest with
                  -- smaller slack wins; on a tie prefer the LARGER fuel, so the
                  -- witness is an interesting pair rather than the empty one
                  | some (sl, f', _, _) =>
                      if slack < sl || (slack == sl && f > f')
                      then { st with tightest := some (slack, f, s₁, s₂) } else st
                  | none               => { st with tightest := some (slack, f, s₁, s₂) }
                else st
      let st := { st with bySize := bumpMax st.bySize (s₁.length + s₂.length) f }
      let st := match st.worst with
                | some (f', _, _) => if f > f' then { st with worst := some (f, s₁, s₂) } else st
                | none            => { st with worst := some (f, s₁, s₂) }
      match unifySpineM cap s₁ s₂ with
      | .success _ _ => { st with success := st.success + 1 }
      | .clash       => { st with clash   := st.clash   + 1 }
      | .occurs      => { st with occurs  := st.occurs  + 1 }
      | .stuck       => { st with stuck   := st.stuck   + 1 }
      | .outOfFuel   => st   -- unreachable: minFuel returned `some`

def sweep (U : Universe) (cap : Nat) : Stats :=
  let ss := allSpines U.atoms U.maxLen
  ss.foldl (fun st s₁ => ss.foldl (fun st s₂ => step cap s₁ s₂ st) st) {}

---------------------------------- REPORTING ----------------------------------

def pairStr (s₁ s₂ : Spine) : String := spineStr s₁ ++ "  ≐ᵣ  " ++ spineStr s₂

def report (U : Universe) (cap : Nat) : IO Unit := do
  let nA := U.atoms.length
  let nS := (allSpines U.atoms U.maxLen).length
  IO.println s!"── universe {U.name} ─ {nA} atoms, spines ≤ {U.maxLen}, {nS} spines, {nS*nS} pairs, cap {cap}"
  let st := sweep U cap
  IO.println s!"   {st.total} pairs: {st.success} success, {st.clash} clash, {st.occurs} occurs, {st.stuck} stuck"

  IO.println s!"   [1] divergence candidates (outOfFuel at cap {cap}): {st.nSurv}"
  for (s₁, s₂) in st.survivors.reverse do
    IO.println s!"        {pairStr s₁ s₂}"

  IO.println s!"   [2] fuel-monotonicity violations: {st.nViol}"
  for (s₁, s₂, a, b) in st.monoViol.reverse do
    IO.println s!"        {pairStr s₁ s₂}\n          at minFuel: {a}\n          at cap:     {b}"

  IO.println s!"   [3] candidate-bound violations — A (tight): {st.nAViol}, B (loose): {st.nBViol}"
  for (f, b, s₁, s₂) in st.boundViol.reverse do
    IO.println s!"        fuel {f} > boundA {b}:  {pairStr s₁ s₂}"
  match st.tightest with
  | some (sl, f, s₁, s₂) => IO.println s!"        A's tightest: slack {sl} (fuel {f}) at {pairStr s₁ s₂}"
  | none                 => pure ()

  IO.println "   [4] fuel profile — max minFuel by problem size |s₁|+|s₂|:"
  for n in List.range (2 * U.maxLen + 1) do
    match st.bySize.find? (fun p => p.1 = n) with
    | some (_, v) => IO.println s!"        size {n}: {v}"
    | none        => pure ()
  match st.worst with
  | some (f, s₁, s₂) => IO.println s!"        hungriest: fuel {f} for {pairStr s₁ s₂}"
  | none             => pure ()
  IO.println ""

---------------------------------- UNIVERSES ----------------------------------
-- Row variables and type variables share ONE namespace (minimal.lean:649), so a
-- payload `.var "a"` alongside a spine atom `.var "a"` is the cross-sort
-- aliasing that makes solve-and-apply feed U-expand — the exact mechanism that
-- killed the lexicographic measure. Both universes below contain it.
--
-- Invented names are `natName n` = n copies of 'a', drawn from `localSupply` =
-- (longest name in the problem) + 1. Keeping every variable one character long
-- means the supply starts at "aa" and can never collide with a name below.

/-- Rich payloads, short spines. Reaches ★, functions, and nested records —
    `{l:{…}}` is the shape behind `terminal_masks_mgu`, whose occurs violation
    passes UNDER a record constructor. -/
def wide : Universe :=
  { name   := "wide"
    vars   := ["a", "b"]
    labels := ["l", "m"]
    tys    := [.base (), .unk, .var "a", .var "b",
               .rcd (.var "a"), .rcd (.sing "l" (.var "a")), .fn (.var "a") (.base ())]
    maxLen := 2 }

/-- Payloads that are themselves multi-atom ROWS. This is the shape the other
    two universes miss and the one the retracted measure died on: a type
    equation solved inside a field can expand a row variable and hand the
    invented tail to the residual, so both the spine length and the variable
    count grow. `{β|α}` is literally the payload in `stuck_masks_mgu`. -/
def nest : Universe :=
  { name   := "nest"
    vars   := ["a", "b"]
    labels := ["l", "k"]
    tys    := [.base (), .var "a",
               .rcd (.cat (.var "a") (.var "b")),
               .rcd (.cat (.sing "l" (.base ())) (.var "a")),
               .rcd (.sing "l" (.var "a"))]
    maxLen := 2 }

/-- Narrow payloads, longer spines. Three variables and length 3 is where the
    Wand shape (a field with several candidate hosts) and repeated expansion
    have room to interact. -/
def deep : Universe :=
  { name   := "deep"
    vars   := ["a", "b", "c"]
    labels := ["l", "m"]
    tys    := [.base (), .var "a", .rcd (.var "a")]
    maxLen := 3 }

--------------------------- PARAMETRIC SCALING --------------------------------
-- Exhaustive search over a small universe can only say "no divergence HERE".
-- The families below say something a measure can be checked against: how the
-- fuel demand GROWS along a parameter. A linear profile is what a bound of the
-- shape `c · (|s₁| + |s₂|)` predicts; anything super-linear tells you which
-- ingredient the measure has to charge for.

def vv (i : Nat) : Atom Unit := .var ("v" ++ toString i)
def ww (i : Nat) : Atom Unit := .var ("w" ++ toString i)
def lf (i : Nat) : Atom Unit := .field ("l" ++ toString i) (.base ())
def mf (i : Nat) : Atom Unit := .field ("m" ++ toString i) (.base ())

def upto (n : Nat) (f : Nat → Atom Unit) : Spine := (List.range n).map f

/-- A record nested `d` deep, ending in `𝓫`. -/
def nestB : Nat → Ty Unit
  | 0     => .base ()
  | d + 1 => .rcd (.sing "l" (nestB d))

/-- Arrows nested `d` deep. `Ty.rcdDepth` charges NOTHING for these — its `.fn`
    case takes a `max`, not a `1 +` — while the type pass still has to walk
    them. If the candidate bound is wrong, this is where it breaks. -/
def arrB : Nat → Ty Unit
  | 0     => .base ()
  | d + 1 => .fn (.base ()) (arrB d)

def arrV : Nat → Ty Unit
  | 0     => .var "x"
  | d + 1 => .fn (.base ()) (arrV d)

/-- The same, ending in a variable — so the pair has a unique mgu at depth `d`. -/
def nestV : Nat → Ty Unit
  | 0     => .var "x"
  | d + 1 => .rcd (.sing "l" (nestV d))

structure Family where
  name : String
  note : String
  gen  : Nat → Spine × Spine

def families : List Family :=
  [ { name := "wand-n",      note := "n variables vs one field — stuck, and must STAY cheap"
      gen := fun n => (upto n vv, [.field "l" (.base ())]) },
    { name := "cancel-n",    note := "n shared vars on both ends — U-var-refl only"
      gen := fun n => (upto n vv ++ [.field "l" (.base ())],
                      upto n vv ++ [.field "l" (.base ())]) },
    { name := "collapse-n",  note := "n vars must all vanish against a var-free side"
      gen := fun n => (upto n vv ++ [.field "l" (.base ())], [.field "l" (.base ())]) },
    { name := "absorb-n",    note := "n distinct fields absorbed into one variable"
      gen := fun n => (upto n lf, [.var "v0"]) },
    { name := "crossfield-n", note := "n disjoint fields each side — the EXPANSION engine"
      gen := fun n => (upto n lf ++ [.var "v0"], upto n mf ++ [.var "w0"]) },
    { name := "interleave-n", note := "expansion with the hosts already carrying fields"
      gen := fun n => (upto n lf ++ [.var "v0"] ++ upto n mf,
                      upto n mf ++ [.var "w0"] ++ upto n lf) },
    { name := "nest-d",      note := "record nesting depth d — the type pass recursing"
      gen := fun d => ([.field "k" (nestB d)], [.field "k" (nestV d)]) },
    { name := "fn-d",        note := "ARROW nesting depth d — rcdDepth cannot see this"
      gen := fun d => ([.field "k" (arrB d)], [.field "k" (arrV d)]) },
    { name := "nestwand-d",  note := "nesting under a Wand payload"
      gen := fun d => ([.field "k" (.rcd (.cat (.var "a") (.var "b")))],
                      [.field "k" (nestB d)]) } ]

def familyLine (F : Family) (cap n : Nat) : IO Bool := do
  let mut out := ""
  let mut blew := false
  for i in List.range (n + 1) do
    let (s₁, s₂) := F.gen i
    let v := verdictStr (unifySpineM cap s₁ s₂)
    let tag := v.take 2
    match minFuel cap s₁ s₂ with
    | some f =>
        let bnd := boundA s₁ s₂
        out := out ++ s!" {i}:{f}{tag}" ++ (if f ≤ bnd then "" else s!"!>{bnd}")
        blew := blew || !(f ≤ bnd)
    | none   => out := out ++ s!" {i}:>{cap}{tag}"; blew := true
  IO.println s!"   {F.name}"
  IO.println s!"      {F.note}"
  IO.println s!"      n:fuel {out}"
  return blew

def familyReport (cap n : Nat) : IO Unit := do
  IO.println s!"── parametric families (n = 0 … {n}, cap {cap}; tag = first 2 letters of the verdict)"
  let mut anyBlew := false
  for F in families do
    let b ← familyLine F cap n
    anyBlew := anyBlew || b
  if anyBlew then
    IO.println "   !! a family did not reach a verdict within cap, or broke the candidate"
    IO.println "      bound (marked !>bound) — DIVERGENCE / BOUND CANDIDATE"
  IO.println ""

------------------------------- KNOWN LANDMARKS -------------------------------
-- Sanity: the harness must reproduce the verdicts the build already pins down.
-- If one of these lines disagrees with Regressions.lean the harness is wrong,
-- not the algorithm.

def landmarks : List (String × Spine × Spine) :=
  [ ("wand (stuck, no mgu)",
       [.var "b", .var "a"], [.field "l" (.base ())]),
    ("lutail (success, α ≔ ε)",
       [.field "l" (.base ())], [.var "a", .field "l" (.base ())]),
    ("shared tail (clash)",
       [.field "l" (.base ()), .var "a"], [.field "m" (.base ()), .var "a"]),
    ("occurs, all-var (conservative — HAS an mgu)",
       [.var "a"], [.var "b", .var "a", .var "c"]),
    ("stuck_masks_mgu (conservative — unique mgu)",
       [.field "k" (.rcd (.cat (.var "b") (.var "a"))), .var "b"],
       [.field "k" (.rcd (.sing "l" (.base ()))), .field "l" (.base ())]),
    ("terminal_masks_mgu (terminal — unique mgu)",
       [.field "l" (.rcd (.var "w"))], [.var "w", .var "v"]) ]

def landmarkReport (cap : Nat) : IO Unit := do
  IO.println "── landmarks (must agree with Regressions.lean / Refutations.lean)"
  for (nm, s₁, s₂) in landmarks do
    let v := verdictStr (unifySpineM cap s₁ s₂)
    let f := match minFuel cap s₁ s₂ with | some f => toString f | none => "—"
    IO.println s!"   {v} (fuel {f})  {nm}\n        {pairStr s₁ s₂}"
  IO.println ""

def main : IO Unit := do
  let cap := 64
  IO.println "≐ᵣ divergence hunt — Phase D triage\n"
  landmarkReport cap
  report wide cap
  report deep cap
  report nest cap
  familyReport 4000 24
  IO.println "A pair still outOfFuel at cap, with a spine of ≤ 6 atoms, is a"
  IO.println "divergence candidate: minimize it by hand and it becomes a theorem."

end Fuzz
end MinimalCalculus

def main : IO Unit := MinimalCalculus.Fuzz.main
