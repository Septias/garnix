#import "../functions.typ": *
#import "../typesystem.typ": *
#set page(height: auto)

== Castagna
#figure(caption: "Castagna Record Typesystem.", rect(
  width: 100%,
  [
    #align(left, text(weight: "bold", smallcaps("Semantic Subtyping")))
    $
      ⟦τ_1⟧ ⊆ ⟦τ_2⟧ <=> τ_1 ≤ τ_2 \
      ⟦τ_1 ∧ τ_2⟧ ≡ ⟦τ_1⟧ ∪ ⟦τ_2⟧
    $
    #align(left, text(weight: "bold", smallcaps("Syntax & Types")))
    #flexbox(
      $#type_name("Type variables") α ∈ cal(V)_t$,
      $#type_name("Field Type variables") θ ∈ cal(V)_f$,
      $#type_name("Row Type variables") ρ ∈ cal(V)_r$,
      $#type_name("Labels") l ∈ cal(L)$,
      $#type_name("Rows") r ∈ cal(R)$,
      $#type_name("Basetypes") b ∈ cal(B)$, //#h(1cm) c : b_c #h(1cm) b_c (c) -> cal(T)$,
      $L ∈ cal(P)_"fin" (cal(L))$,
    )
    $
      #type_name("Kinds") && κ & ::= star | star_⊥ | "Row"(L) \
      #type_name("Types") && t & ::= α | b | t → t | { l = τ,…, l = τ | ς} | ¬t | t ∨ t | 𝟘 \
      #type_name("Fieldtypes") && τ & ::= θ | t | ⊥ | τ ∨ τ | ¬τ | \
      #type_name("Tails") && ς & ::= .. | ρ | ε \
      #type_name("Row") && t & ::= ⟨ l : τ ... l: τ | ς⟩^L | r ∨ r | ¬r \
      #type_name("Terms") && e & ::= ? \
    $
    #align(left, text(weight: "bold", smallcaps("Kinding")))
    $ α: ∗ #h(1cm) θ: ∗_⊥ $
    #align(left, text(weight: "bold", smallcaps("Rewrites")))
    $
      T = t | τ | r #h(1cm) T₁ ∧ T₂ = ¬(¬T₁ ∨ ¬T₂) #h(1cm) T₁ without T₂ = T₁ ∧ ¬T₂ #h(1cm) 𝟙 = ¬𝟘
    $
    #align(left, text(weight: "bold", smallcaps("Typing Rules")))
    #align(left, text(weight: "bold", smallcaps("Kinding Rules")))
  ],
))

== Parreaux
#figure(caption: "Record Typesystem.", rect(
  width: 100%,
  stack(),
))

== Full Records
#figure(caption: "Record Typesystem.", rect(
  width: 100%,
  stack(
    spacing: 9pt,
    align(left, text(weight: "bold", smallcaps("Syntax & Types"))),
    flexbox(
      $#type_name("Term variables") x ∈ cal(X)$,
      $#type_name("Type variables") x ∈ cal(A)$,
      $#type_name("Labels") l ∈ cal(L)$,
    ),
    $
      #type_name("Kinds") &&               κ & ::= ∗ | L | κ → κ \
      #type_name("Types") && cal(T) in.rev τ & ::= α | {overline(α)} | ⦅l⦆ | \
        #type_name("Row") && cal(E) in.rev t & ::= { overline(a) } \
      #type_name("Terms") && cal(E) in.rev t & ::= { overline(a) } \
                          &&               a & ::= l = t \
                          &&               α & ::= l : τ \
    $,
    align(left, text(weight: "bold", smallcaps("Typing Rules"))),
    record_typing_rules,
    align(left, text(weight: "bold", smallcaps("Kinding Rules"))),
  ),
))


== Full Functions
#figure(caption: "Function typsystem.", rect(
  width: 100%,
  stack(
    spacing: 9pt,
    flexbox(
      $#type_name("Term variables") x ∈ cal(X)$,
      $#type_name("Type variables") x ∈ cal(A)$,
    ),
    $
      #type_name("Types") && cal(T) in.rev τ & ::= {} → τ ∣ τ → τ | ?t \
      #type_name("Terms") && cal(E) in.rev t & ::= x: t | {α}: t \
                          &&               a & ::= l | l ? t \
                          &&               α & ::= l : τ \
    $,
    align(left, text(weight: "bold", smallcaps("Typing Rules"))),
    function_typing_rules,
    align(left, text(weight: "bold", smallcaps("Subtyping Rules"))),
  ),
))


== Occurrence Typing

#figure(caption: "Occurrence type system.", rect(
  width: 100%,
  stack(
    spacing: 9pt,
    flexbox(
      $#type_name("Term variables") x ∈ cal(X)$,
      $#type_name("Type variables") x ∈ cal(A)$,
    ),
    $
      #type_name("Types")&& cal(T) in.rev τ &::= "true" | "false" \
      #type_name("Terms")&& cal(E) in.rev t &::= #b[if] t_1 #b[then] t_2 #b[else] t_3 \
      &&a &::= l | l ? t \
      &&α &::= l : τ \
    $,
    align(left, text(weight: "bold", smallcaps("Reduction Rules"))),
    $
      #rule_name("R-Cond-True")&& #b[if] "true" #b[ then ] t_1 #b[ else ]t_2 & arrow.long t_1 \
      #rule_name("R-Cond-False")&& #b[if] "false" #b[then ] t_1 #b[ else ]t_2 & arrow.long t_2 \
    $,
    align(left, text(weight: "bold", smallcaps("Typing Rules"))),
  ),
))

== With & Inherit
#figure(caption: "Occurrence type system.", rect(
  width: 100%,
  stack(
    spacing: 9pt,
    flexbox(
      $#type_name("Term variables") x ∈ cal(X)$,
      $#type_name("Type variables") x ∈ cal(A)$,
    ),
    $
      #type_name("Types")&& cal(T) in.rev τ &::= "true" | "false" \
      #type_name("Terms")&& cal(E) in.rev t &::= #b[if] t_1 #b[then] t_2 #b[else] t_3 \
      &&a &::= l | l ? t \
      &&α &::= l : τ \
    $,
    align(left, text(weight: "bold", smallcaps("Reduction Rules"))),
    flexbox(
      derive("T-With", $Γ ⊢ t₂ ≤ {} Γ, Ξ · t₂ ⊢ t₂ : τ$, $Γ ⊢ with t₁; t₂ : τ$),
      derive("R-Inherit1", $x ∈ Γ$, $Γ ⊢ { inherit x; } -> { x = Γ(x);}$),
      derive(
        "R-Inherit2",
        $x ∈ Γ$,
        $Γ ⊢ { inherit (ρ) x; } -> { x = "lookup"(ρ, x)}$,
      ),
    ),
  ),
))



