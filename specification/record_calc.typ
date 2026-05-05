#import "functions.typ": *
#import "typesystem.typ": *
#set page(height: auto)
#show: template


= Brainstorming
- Features:
  - Pattern destructuring
  - FC-Labels
  - Record-concat
  - With-construct
  - Inference
- ROSE system ist cool
  - Aber basiert auf System F? ==> Ineffektiv / undecidable?
- What inference engine?
  - subtyping?
  - constraints?
  - unification?
  - bi-unification?
- Relations
  - subtyping: ≤
  - containment: ≤
  - destructuring: ⊙

== Beispiele
```nix
let
 e1 = a: b: (a // b).c;
 e2 = a: b: c: (a // b).${c};
in ()
```


== Fragen
- Kann man destructuring und subtyping vereinen?

= Typen
#let types = box(
  width: 100%,
  [
    #align(center, flexbox(
      $#type_name("Basetypes") b ∈ cal(B)$,
      $#type_name("Type Variables") α ∈ cal(A)$,
      $#type_name("Labels") l ∈ cal(L)$,
    ))
    $
      #type_name("Type")&& tau & ::= b | α | τ -> τ | ⦃ overline(p) ⦄^+ -> τ | ⦃ overline(p) ⦄^- -> τ \
      #type_name("Datatypes")&& &| {overline(l\: τ)} | [τ] | [overline(τ)] \
      #type_name("Connectives")&& & | ⊥ | top | τ ∨ τ | τ ∧ τ | ¬τ \
      #type_name("Pattern Field")&& p & := τ | τ^τ \
      #type_name("Polymorphic type")&& σ & := ∀Xi. τ \
      // #type_name("Mode")&& diamond.small & := + | -\
      #type_name("Typing Context")&& Γ & ::= ε | Γ · (x : τ) \
    $
  ],
)

#types



= Typregeln
#let record_typing_rules = flexbox(
  derive(
    "T-Rcd",
    ($Γ ⊢ t_1: ⦅l⦆$, $t_2 : τ$),
    $Γ ⊢ {t_1 = t_2}: {l: τ}$,
  ),
  derive("T-Proj", ($Γ ⊢ t_1: {l: τ | ρ}$, $Γ ⊢ t_2: ⦅l⦆$), $Γ ⊢ t_1.t_2: τ$),
  derive(
    "T-Or-Pos",
    ($Γ ⊢ t_1: {l: τ | ρ}$, $Γ ⊢ t_2: ⦅l⦆$),
    $Γ ⊢ (t_1).t_2 #b[or] t_3: τ$,
  ),
  derive(
    "T-Or-Neg",
    ($Γ ⊢ t_1: {ρ}$, $l ∉ ρ$, $Γ ⊢ t_2: ⦅l⦆$, $Γ ⊢ t_3: τ$),
    $Γ ⊢ (t_1).t_2 #b[or] t_3: τ$,
  ),
  derive(
    "T-Rec-Concat",
    ($Γ ⊢ t_1: { ρ }$, $Γ ⊢ t_2: { ρ'}$),
    $Γ ⊢ t_1 "//" t_2: {ρ | ρ'}$,
  ),
  derive(
    "T-Check",
    ($Γ ⊢ e: {ρ}$, $t : ⦅l⦆$),
    $Γ ⊢ e #b[?] t: "bool"$,
  ),
)
#figure(caption: "Record typing rules", record_typing_rules)
#let function_typing_rules = stack(
  spacing: 15pt,
  flexbox(
    derive(
      "T-Abs2",
      $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
      $Γ ⊢ ({overline(e)}: t) : ⦃α⦄^- → τ_2$,
    ),
    derive(
      "T-Abs3",
      $Γ, overline(e_i : τ_i) ⊢ t: τ_2$,
      $Γ ⊢ ({overline(e),...}: t) : ⦃α⦄^+ → τ_2$,
    ),
    derive(
      "T-App2",
      (
        $Γ ⊢ t_1: ⦃overline(α)⦄^- → τ_2$,
        $Γ ⊢ t_2: τ_1$,
        $τ_1 ≤ needed(overline(α))$,
        $ceiling(overline(α)) ≤ τ_1$,
      ),
      $Γ ⊢ (x: t_1) t_2: τ_2$,
    ),
    derive(
      "T-App3",
      (
        $Γ ⊢ t_1: ⦃overline(α)⦄^+ → τ_2$,
        $Γ ⊢ t_2: τ_1$,
        $τ_1 ≤ needed(overline(α))$,
      ),
      $Γ ⊢ (x: t_1) t_2: τ_2$,
    ),
  ),
  flexbox(
    $ceiling(oa) = { τ | τ ∈ oa ∨ τ^τ' ∈ oa}$,
    $floor(oa) = { τ | τ ∈ oa }$,
  ),
)
#figure(caption: "Function typing rules", function_typing_rules)

#let other_constructs = figure(
  caption: "Extra constructs typing rules.",
  flexbox(
    derive(
      "T-With",
      ($Γ ⊢ t₁ : {ρ}$, $Γ,Ξ · {ρ} ⊢ t₂ : τ$),
      $Γ ⊢ with t₁; t₂ : τ$,
    ),
    derive("R-Inherit", $x ∈ Γ$, $Γ ⊢ { inherit x; } -> { x = Γ(x);}$),
    derive(
      "R-Inherit-path",
      $x ∈ Γ$,
      $Γ ⊢ { inherit (ρ) space x; } -> { x = "lookup"(ρ, x)}$,
    ),
    derive(
      "T-Import",
      ($𝜚 arrow.squiggly t$, $Γ ⊢ t: τ$),
      $Γ ⊢ #b[import] 𝜚: τ$,
    ),
  ),
)

