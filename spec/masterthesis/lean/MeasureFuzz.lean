import RowUnify.Applied
namespace MinimalCalculus
namespace MF
abbrev Spine := List (Atom Unit)

def rngVars (s : Sol Unit) : List (Srt × TyVar) :=
  s.ty.flatMap (fun p => Ty.sortedFtv p.2) ++ s.row.flatMap (fun p => Row.sortedFtv p.2) ++
    s.lab.flatMap (fun p => Key.sortedFtv p.2)

structure St where
  total : Nat := 0
  succ : Nat := 0
  withFresh : Nat := 0
  bad : Nat := 0
  eq : Nat := 0
  draws : Nat := 0
  wBad : List String := []

mutual
partial def tyS : Ty Unit → String
  | .var a => a | .base _ => "𝓫" | .unk => "★" | .lab _ => "lab"
  | .fn a b => "(" ++ tyS a ++ "→" ++ tyS b ++ ")"
  | .rcd ρ => "{" ++ spineStr ρ.toSpine ++ "}"
partial def spineStr (s : Spine) : String :=
  ", ".intercalate (s.map fun
    | .var a => a | .field l τ => l ++ ":" ++ tyS τ | .dfield _ τ => "$:" ++ tyS τ)
end

def step (cap : Nat) (st : St) (s₁ s₂ : Spine) : St :=
  let st := { st with total := st.total + 1 }
  match unifySpineM cap s₁ s₂ with
  | .success s S' =>
      let draws := S'.next - (localSupply s₁ s₂).next
      let st := if (draws < s.domS.eraseDups.length) || (draws == 0 && s.domS.isEmpty) then st
        else { st with draws := st.draws + 1 }
      let P := (sSorted s₁ ++ sSorted s₂).eraseDups
      let D := s.domS.eraseDups
      let K := D.filter (P.contains ·)
      let N := (rngVars s).eraseDups.filter (fun x => !P.contains x && !D.contains x)
      let st := { st with succ := st.succ + 1,
                          withFresh := st.withFresh + (if N.isEmpty then 0 else 1) }
      if D.isEmpty then st
      else if N.length < K.length then st
      else { st with bad := st.bad + 1,
                     eq := st.eq + (if N.length == K.length then 1 else 0),
                     wBad := if st.wBad.length < 8 then
                       s!"{spineStr s₁}  ≐  {spineStr s₂}  K={K.length} N={N.length}" :: st.wBad
                       else st.wBad }
  | _ => st

def atomsOf (vars : List TyVar) (labels : List Label) (tys : List (Ty Unit)) : List (Atom Unit) :=
  vars.map .var ++ labels.flatMap (fun l => tys.map (Atom.field l ·))

def spinesOfLen (as : List (Atom Unit)) : Nat → List Spine
  | 0 => [[]]
  | n + 1 => (spinesOfLen as n).flatMap fun s => as.map (· :: s)

def allSpines (as : List (Atom Unit)) (n : Nat) : List Spine :=
  (List.range (n + 1)).flatMap (spinesOfLen as)

def run (name : String) (vars : List TyVar) (labels : List Label) (tys : List (Ty Unit))
    (n : Nat) : IO Unit := do
  let ss := allSpines (atomsOf vars labels tys) n
  let st := ss.foldl (fun st a => ss.foldl (fun st b => step 64 st a b) st) {}
  IO.println s!"{name}: pairs {st.total}, succ {st.succ}, succ w/ fresh {st.withFresh}, VIOLATIONS {st.bad} (of which N=K: {st.eq}), DRAW-VIOLATIONS {st.draws}"
  for w in st.wBad.reverse do IO.println s!"   {w}"

end MF
end MinimalCalculus
open MinimalCalculus MF in
def main : IO Unit := do
  let b : Ty Unit := .base ()
  run "wide" ["a","b"] ["l","m"] [b, .unk, .var "a", .var "b", .rcd (.var "a"),
      .rcd (.sing "l" (.var "a")), .fn (.var "a") b] 2
  run "nest" ["a","b"] ["l","k"] [b, .var "a", .rcd (.cat (.var "a") (.var "b")),
      .rcd (.cat (.sing "l" b) (.var "a")), .rcd (.sing "l" (.var "a"))] 2
  run "deep" ["a","b","c"] ["l","m"] [b, .var "a", .rcd (.var "a")] 3
  run "host" ["a","b"] ["k","l"] [b, .rcd (.var "a"), .rcd (.var "b"),
      .rcd (.cat (.sing "l" b) (.var "a")), .rcd (.cat (.sing "m" b) (.var "b")),
      .rcd (.cat (.sing "m" b) (.var "a"))] 2
