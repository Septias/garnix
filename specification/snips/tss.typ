#import "../functions.typ": *
#import "../typesystem.typ": *
#set page(height: auto)


== Full Records
#figure(caption: "Record typesystem.", rect(
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



