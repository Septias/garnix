#import "./functions.typ": *
#import "./snips/typesystem.typ": *
#set page(height: auto)
#let op = $overline(α)$

= Die Systeme
- Minimal: Record Concat, Pattern-Functions, FC-Labels
- Mit #b[let]: Bringt let-poly rein
- Mit #b[rec]: Dadurch brauch man spätestens einen rekursiven Typ
- Mit #b[with]: Anderes shadowing (perp)
- Mit #b[inherit]: Wie werden die poly. vars. erweitert?


= Formalisierung
#rect(width: 100%)[
  == Minimal
  - Record Concat
  - Function Destructuring

  #figure(caption: "Terms", box(
    width: 100%,
    [
      #align(center, flexbox(
        $#type_name("Terms") e := { e₁ = e₂ } | {overline(p)} → e | { overline(p), ... } → e | e₁:e₂$,
      ))
    ],
  ))


  #figure(caption: "Types", box(
    width: 100%,
    [
      #align(center, flexbox(
        $#type_name("Basetypes") b ∈ cal(B)$,
        $#type_name("Type Variables") α ∈ cal(A)$,
        $#type_name("Labels") l ∈ cal(L)$,
      ))
      $
        #type_name("Type")&& tau & ::= b | α | τ -> τ | ⦃ overline(p) ⦄^+ -> τ | ⦃ overline(p) ⦄^- -> τ | ⦅l⦆ | {overline(l\: τ)} \
        #type_name("Pattern Field")&& p & := τ | τ^τ \
        #type_name("Rows")&& ρ & := ⟨⟩ | ⟨ l: t | ρ⟩ \
        #type_name("Typing Context")&& Γ & ::= ε | Γ · (x : τ) \
      $
    ],
  ))

  #let record_typing_rules = flexbox(
    derive(
      "T-Rcd",
      ($Γ ⊢ t_1: ⦅l⦆$, $t_2 : τ$),
      $Γ ⊢ {t_1 = t_2}: {l: τ}$,
    ),
    derive(
      "T-Proj",
      ($Γ ⊢ t_1: {l: τ | ρ}$, $Γ ⊢ t_2: ⦅l⦆$),
      $Γ ⊢ t_1.t_2: τ$,
    ),
    derive(
      "T-Rec-Concat",
      ($Γ ⊢ t_1: { ρ }$, $Γ ⊢ t_2: { ρ'}$),
      $Γ ⊢ t_1 "//" t_2: {ρ | ρ'}$,
    ),
  )

  #figure(caption: "Record typing rules.", record_typing_rules)
  #let function_typing_rules = stack(
    spacing: 15pt,
    flexbox(
      derive(
        "T-Abs1",
        $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
        $Γ ⊢ ({overline(e)}: t) : ⦃p⦄^- → τ_2$,
      ),
      derive(
        "T-Abs2",
        $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
        $Γ ⊢ ({overline(e),...}: t) : ⦃p⦄^+ → τ_2$,
      ),
      derive(
        "T-App1",
        (
          $Γ ⊢ t_1: ⦃overline(p)⦄^- → τ_2$,
          $Γ ⊢ t_2: τ_1$,
          $τ_1 ≤ needed(overline(p))$,
          $ceiling(overline(p)) ≤ τ_1$,
        ),
        $Γ ⊢ (x: t_1) t_2: τ_2$,
      ),
      derive(
        "T-App2",
        (
          $Γ ⊢ t_1: ⦃overline(p)⦄^+ → τ_2$,
          $Γ ⊢ t_2: τ_1$,
          $τ_1 ≤ needed(overline(p))$,
        ),
        $Γ ⊢ (x: t_1) t_2: τ_2$,
      ),
    ),
    flexbox(
      $ceiling(op) = { τ | (τ ∈ op) ∨ (τ^τ' ∈ op)}$,
      $floor(op) = { τ | τ ∈ op }$,
    ),
  )
  #figure(caption: "Function typing rules.", function_typing_rules)
]

#rect(width: 100%)[
  == Let-Erweiterung
  - Record Concat
  - Function Destructuring
  - Let-statements

  #figure(caption: "Terms", box(
    width: 100%,
    [
      #align(center, flexbox(
        $#type_name("Terms") e := { e₁ = e₂ } | {overline(p)} → e | { overline(p), ... } → e | #b[let] e₁ = e₂ #b[in] e₃$,
      ))
    ],
  ))

  #figure(caption: "Types", box(
    width: 100%,
    [
      #align(center, flexbox(
        $#type_name("Basetypes") b ∈ cal(B)$,
        $#type_name("Type Variables") α ∈ cal(A)$,
        $#type_name("Labels") l ∈ cal(L)$,
      ))
      $
        #type_name("Type")&& tau & ::= b | α | τ -> τ | ⦃ overline(p) ⦄^+ -> τ | ⦃ overline(p) ⦄^- -> τ | {overline(l\: τ)} \
        #type_name("Pattern Field")&& p & := τ | τ^τ \
        #type_name("Rows")&& ρ & := ⟨⟩ | ⟨ l: t | ρ⟩ \
        #type_name("Polymorphic Type")&& σ & := ∀Xi. τ \
        #type_name("Typing Context")&& Γ & ::= ε | Γ · (x : τ) \
      $
    ],
  ))

  #let record_typing_rules = flexbox(
    derive(
      "T-Rcd",
      ($Γ ⊢ t_1: ⦅l⦆$, $t_2 : τ$),
      $Γ ⊢ {t_1 = t_2}: {l: τ}$,
    ),
    derive(
      "T-Proj",
      ($Γ ⊢ t_1: {l: τ | ρ}$, $Γ ⊢ t_2: ⦅l⦆$),
      $Γ ⊢ t_1.t_2: τ$,
    ),
    derive(
      "T-let",
      ($Γ, x: ∀overline(α). τ_1 ⊢ t_2: τ_2$, $Γ ⊢ t_2: τ_2$, $oa ∉ "FT"(Γ)$),
      $Γ ⊢ #b[let] x = t_1 #b[in] t_2: τ_2$,
    ),
  )

  #figure(caption: "Record typing rules.", record_typing_rules)
  #let function_typing_rules = stack(
    spacing: 15pt,
    flexbox(
      derive(
        "T-Abs1",
        $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
        $Γ ⊢ ({overline(e)}: t) : ⦃p⦄^- → τ_2$,
      ),
      derive(
        "T-Abs2",
        $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
        $Γ ⊢ ({overline(e),...}: t) : ⦃p⦄^+ → τ_2$,
      ),
      derive(
        "T-App1",
        (
          $Γ ⊢ t_1: ⦃overline(p)⦄^- → τ_2$,
          $Γ ⊢ t_2: τ_1$,
          $τ_1 ≤ needed(overline(p))$,
          $ceiling(overline(p)) ≤ τ_1$,
        ),
        $Γ ⊢ (x: t_1) t_2: τ_2$,
      ),
      derive(
        "T-App2",
        (
          $Γ ⊢ t_1: ⦃overline(p)⦄^+ → τ_2$,
          $Γ ⊢ t_2: τ_1$,
          $τ_1 ≤ needed(overline(p))$,
        ),
        $Γ ⊢ (x: t_1) t_2: τ_2$,
      ),
    ),
    flexbox(
      $ceiling(op) = { τ | (τ ∈ op) ∨ (τ^τ' ∈ op)}$,
      $floor(op) = { τ | τ ∈ op }$,
    ),
  )
  #figure(caption: "Function typing rules.", function_typing_rules)
]

#rect(width: 100%)[
  == Constraining

  This one should be for generic accesses:
  #derive("C-with", $$, $ Γ ⊢ (α plus.double β).l => (l ∈ α + β) $)

  Like this:?
  #derive("C-with", $ "routine"(r) $, $ Γ ⊢ r.l => (l ∈ overline(A)) $)
]
