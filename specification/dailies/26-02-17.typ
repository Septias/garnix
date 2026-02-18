
./dailies/26-02-18.typ
== Topics
- Full record calculus with type connectives?
- Record extension + fc labels?
- With construct typing alg?
- Occurrence typing for ifs?
- polymorphic records with fc labels?
- parreaux records w/


== Function typing rules

#derive("", $Γ, x: τ_1 ⊢ e : τ_2$, $Γ ⊢ (x: e) : τ_1 → τ_2$)



#derive("", $Γ, overline(e_i : τ_i) ⊢ e: τ_2$, $Γ ⊢ ({oa(α)}: e) : {α}^- → τ_2$)



#derive(
  "",
  $Γ, overline(e_i : τ_i) ⊢ e: τ_2$,
  $Γ ⊢ ({oa(α),...}: e) : {α}^+ → τ_2$,
)



#derive("", $Γ ⊢ e_1: τ_1 → τ_2   Γ ⊢ e2: τ_3 ≤ τ_1$,
$Γ ⊢ (x: e_1) e_2: τ_2$)



#derive("", $Γ ⊢ e_1: {overline(α)^-} → τ_2   Γ ⊢ e2: τ_3 ≤ τ_1$,
$Γ ⊢ (x: e_1) e_2: τ_2$)



#derive("", $Γ ⊢ e_1: {overline(α)^+} → τ_2 Γ ⊢ e_2: τ_1$,
$Γ ⊢ (x: e_1) e_2: τ_2$)

#let function_typing_rules = flexbox(
  caption: "function",
derive("", $Γ, x: τ_1 ⊢ e : τ_2$, $Γ ⊢ (x: e) : τ_1 → τ_2$)



derive("", $Γ, overline(e_i : τ_i) ⊢ e: τ_2$, $Γ ⊢ ({oa(α)}: e) : {α}^- → τ_2$)



derive(
  "",
  $Γ, overline(e_i : τ_i) ⊢ e: τ_2$,
  $Γ ⊢ ({oa(α),...}: e) : {α}^+ → τ_2$,
)



derive("", $Γ ⊢ e_1: τ_1 → τ_2   Γ ⊢ e2: τ_3 ≤ τ_1$,
$Γ ⊢ (x: e_1) e_2: τ_2$)



derive("", $Γ ⊢ e_1: {overline(α)^-} → τ_2   Γ ⊢ e2: τ_3 ≤ τ_1$,
$Γ ⊢ (x: e_1) e_2: τ_2$)



derive("", $Γ ⊢ e_1: {overline(α)^+} → τ_2 Γ ⊢ e_2: τ_1$,
$Γ ⊢ (x: e_1) e_2: τ_2$)

)
