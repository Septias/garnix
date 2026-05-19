#import "../functions.typ": *
#import "./typesystem.typ": *
#set page(height: auto)

== TODO
- Die Typvariablen und records sind beide α

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
      $#type_name("Type Variables") α ∈ cal(V)_t$,
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
  ],
))

== Parreaux
#figure(
  caption: "Syntax of MLstruct⁺.",
  rect(
    width: 100%,
    [
      #align(left, text(weight: "bold", smallcaps("Syntax")))

      $
        #type_name("Field names")&& a & ∈ cal(F) #v(1cm) #type_name("Tag names") T ∈ cal(T) \
        #type_name("Types")&& τ, π & ::= τ → τ | { a : τ } | \# T | α | T^± | τ ⊔^± τ | ¬π \
        #type_name("Mode")&& ± & ::= + | - \
        #type_name("Shorthands")&& T^+ & ≜ ⊤ #h(1cm) T^- ≜ ⊥ #h(1cm) ⊔^+ ≜ ⊔ \
        && ⊔^- & ≜ ⊓ #h(1cm) ≤^+ ≜ ≤ #h(1cm) ≤^- ≜ ≥ \
        #type_name("Polymorphic types")&& σ &::= ∀𝔅 . τ \
        #type_name("Terms")&&
        s, t &::= x, y, z | t : τ | λ x.t | t space t | T R | t.a | "if" x = t "is" T "then" t "else" t \
        && R &::= {} \;|\; { … , a = t } \
        #type_name("Values")&& v, w &::= λ x.t | T {} | T { … v, a = v } \
        #type_name("Programs")&& P &::= t| "def" x = t; P \
        #type_name("Typing context")&& Γ &::= ε| Γ · (x : τ)| Γ · (x : σ) \
        #type_name("Subtyping context")&& Σ, Δ &::= ε| Σ · H| Σ · ▷ H \
        && H &::= τ ≤ τ \
        #type_name("Bounds context")&& 𝔅, 𝔄 &::= ε| 𝔅 · (α ≤ τ)| 𝔅 · (τ ≤ α)
      $

      #align(left, text(weight: "bold", smallcaps("Typesystem")))
      #flexbox(
        derive(
          "T-Body",
          ($cal(B) #b[cons.]$, $cal(B),Γ ⊢ t: τ$),
          $cal(B), Γ ⊢^star t: τ$,
        ),

        derive(
          "T-Def",
          (
            $cal(B)^prime #b[cons.]$,
            $cal(B)^prime, Γ ⊢ t: τ$,
            $cal(B), Γ · (x : ∀ cal(B)^prime . τ) ⊢^star P : τ_P$,
          ),
          $cal(B), Γ ⊢^star #b[def] x = t ; P : τ_P$,
        ),
        derive(
          "T-Asc",
          (
            $cal(B), Γ ⊢ t : τ$
          ),
          $cal(B), Γ ⊢ (t : τ) : τ$,
        ),

        derive(
          "T-Var1",
          (
            $Γ(x) = τ$
          ),
          $cal(B), Γ ⊢ x : τ$,
        ),

        derive(
          "T-Var2",
          (
            $Γ(x) = ∀ cal(A) . τ$,
            $cal(B) models ρ(cal(A))$,
          ),
          $cal(B), Γ ⊢ x : ρ(τ)$,
        ),

        derive(
          "T-Subs",
          (
            $cal(B), Γ ⊢ t : τ_1$,
            $cal(B) ⊢ τ_1 ≤ τ_2$,
          ),
          $cal(B), Γ ⊢ t : τ_2$,
        ),
        derive(
          "T-Obj",
          (
            $T #b[final]$
          ),
          $cal(B), Γ ⊢ T {} : \#T$,
        ),

        derive(
          "T-Ext",
          (
            $cal(B), Γ ⊢ t_0 : τ_0 inter.sq \#T$,
            $cal(B), Γ ⊢ t_1 : τ_1$,
            $T #b[final]$,
          ),
          $cal(B), Γ ⊢ T { … , t_0, a = t_1 } :
          τ_0 ∧ ¬{ a : τ } inter.sq \#T$,
        ),

        derive(
          "T-Proj",
          (
            $cal(B), Γ ⊢ t : { a : τ }$
          ),
          $cal(B), Γ ⊢ t.a : τ$,
        ),


        derive(
          "T-Abs",
          (
            $cal(B), Γ · (x : τ_1) ⊢ t : τ_2$
          ),
          $cal(B), Γ ⊢ λ x.t : τ_1 → τ_2$,
        ),

        derive(
          "T-App",
          (
            $cal(B), Γ ⊢ t_0 : τ_1 → τ_2$,
            $cal(B), Γ ⊢ t_1 : τ_1$,
          ),
          $cal(B), Γ ⊢ t_0 t_1 : τ_2$,
        ),

        derive(
          "T-If",
          (
            $cal(B), Γ ⊢ t_0 : (τ_1 inter.sq \#T) union.sq (τ_2 without \#T)$,
            $cal(B), Γ · (x : τ_1) ⊢ t_1 : τ$,
            $cal(B), Γ · (x : τ_2) ⊢ t_2 : τ$,
          ),
          $cal(B), Γ ⊢ "if" x = t_0 "is" T "then" t_1 "else" t_2 : τ$,
        ),
      )

      == Subtyping Rules
      #flexwrap(
        main-spacing: 20pt,
        cross-spacing: 10pt,
        derive("S-Refl", (), $τ <= τ$),
        derive("S-ToB", (), $τ rotate(≤) rotate(top)$),
        derive("S-CompL", (), $τ ∨ ¬τ rotate(≥) rotate(top)$),
        derive("S-NegInv", ($Σ tack τ_1 ≤ τ_2$,), $Σ tack ¬τ_1 <= ¬τ_2$),
        derive("S-AndOr11", (), $τ_1 rotate(∨) τ_2 rotate(≥) τ_1$),
        derive("S-AndOr11", (), $τ_1 rotate(∨) τ_2 rotate(≥) τ_2$),
        derive("S-AndOr2", (), $τ_1 rotate(∨) τ_2 rotate(≥) τ_2$),
        derive(
          "S-Distrib",
          (),
          $τ rotate(∧) (τ_1 rotate(∨) τ_2) rotate(≤) (τ rotate(∧) τ_1) rotate(∨)(τ rotate(∧) τ_2)$,
        ),

        derive(
          "S-Trans",
          ($Σ tack τ_0 <= τ_1$, $Σ tack τ_1 <= τ_2$),
          $Σ tack τ_0 <= τ_2$,
        ),
        derive("S-Weaken", ($H$,), $Σ tack H$),
        derive("S-Assume", ($Σ,gt.tri H tack H$,), $Σ tack H$),
        derive("S-Hyp", ($H in Σ$,), $Σ tack H$),
        derive("S-Rec", (), $μ α.τ eq.triple [μ α.τ slash α]τ$),
        derive(
          "S-Or",
          ($∀ i, exists j,Σ tack τ_i <= τ'_j$,),
          $Σ tack union.sq_i τ_i <= union.sq_j τ'_j$,
        ),
        derive(
          "S-And",
          ($∀ i, exists j,Σ tack τ_j <= τ'_i$,),
          $Σ tack inter.sq_j τ_j <= inter.sq_i τ'_i$,
        ),
        derive(
          "S-Fun",
          ($lt.tri Σ tack τ_0 <= τ_1$, $lt.tri Σ tack τ_2 <= τ_3$),
          $Σ tack τ_1 arrow.long τ_2 <= τ_0 arrow.long τ_3$,
        ),
        derive(
          "S-Rcd",
          (),
          ${arrow(t) : arrow(τ)} eq.triple inter.sq_i {l_i : t_i}$,
        ),
        derive(
          "S-Rcd",
          (),
          ${arrow(t) : arrow(τ)} eq.triple inter.sq_i {l_i : t_i}$,
        ),
        derive(
          "S-Rcd",
          (),
          ${arrow(t) : arrow(τ)} eq.triple inter.sq_i {l_i : t_i}$,
        ),
        derive(
          "S-Depth",
          ($lt.tri Σ tack τ_1 <= τ_2$,),
          $Σ tack {l: τ_1} <= { l: τ_2}$,
        ),
        derive("S-Lst", ($ Γ tack τ_1 <= τ_2 $,), $Γ tack [τ_1] <= [τ_2]$),
      )
      $lt.tri(H_0, H_1) = lt.tri H_0, lt.tri H_1$
      $lt.tri(gt.tri H) = H$
      $lt.tri ( τ_0 <= τ_1) = τ_0 <= τ_1$
      )

    ],
  ),
)
== Full Records
#figure(caption: "Record Typesystem.", rect(
  width: 100%,
  stack(
    spacing: 9pt,
    align(left, text(weight: "bold", smallcaps("Syntax & Types"))),
    flexbox(
      $#type_name("Term variables") x ∈ cal(X)$,
      $#type_name("Type variables") α ∈ cal(A)$,
      $#type_name("Labels") l ∈ cal(L)$,
    ),
    $
      #type_name("Kinds") &&               κ & ::= ∗ | L | κ → κ \
      #type_name("Types") && cal(T) in.rev τ & ::= α | {overline(α)} | ⦅l⦆ \
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
      $#type_name("Type variables") α ∈ cal(A)$,
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
    $,
    align(left, text(weight: "bold", smallcaps("Reduction Rules"))),
    $
      #rule_name("R-Cond-True")&& #b[if] "true" #b[ then ] t_1 #b[ else ]t_2 & arrow.long t_1 \
      #rule_name("R-Cond-False")&& #b[if] "false" #b[then ] t_1 #b[ else ]t_2 & arrow.long t_2 \
    $,
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



