#import "functions.typ": *
#set page(paper: "din-d3")

#figure(
  caption: "Functions Rules",
  rect(
    inset: 20pt,
    stack(
      spacing: 3em,
      align(left, sub_typing_rules(caption: "Syntax", [
        $#type_name("Function") & | overline(p) "@ "h : t$

        #text(weight: "bold", smallcaps("Patterns"))
        $
          "e" & ::= l | l space ? space t                        \
            p & ::= { overline(e) } | { overline(e), #b[…] } | l \
            h & ::= ε | l                                        \
        $])),
      sub_typing_rules(
        caption: "Reduction",
        [$
            #rule_name("R-Fun")&& (l: t_2)t_1 & arrow.long t_2[l := t_1] \
            #rule_name("R-Fun-Pat")&& ({oi(l_i)}: t){oi(l_i \= t_i)} & arrow.long t [oi(l_i := t_i)] \
            #rule_name("R-Fun-Pat-Open")&& ({oi(l_i)\, ...}: t) {oj(l_i = t_i)} & arrow.long t [oi(l_i := a_i)] #h(0.5cm) &&&∀i. ∃ j. i eq j \
            #rule_name("R-Fun-Pat-Default")&&{oi(l_i" ? "t_i), overline(l_j)^j}: t_2{overline(l_k = t_k)^k; oj(l_j = t_j)} & arrow.long t_2 [overline(l_m := a_m)^m] [overline(l_n = a_n)^n] [overline(l_j := a_j)^j] \
            &&"Where" &m ∈ {i: ∃k. l_i = l_k}; n ∈ {i: exists.not k. l_i = l_k} \
          $],
      ),
      sub_typing_rules(
        caption: "Typing",
        derive("T-Abs", ($Γ, x: τ_1 tack t: τ_2$,), $Γ tack (x: t): τ_1 → τ_2$),
        derive(
          "T-Abs-Pat",
          ($Γ, oi(x\: τ) tack t: τ_2$,),
          $Γ tack ({oi(x)}: t): oi(τ) → τ_2$,
        ),
        derive("T-Abs-Pat-Opt", ($"TODO"$,), $"TODO"$),
      ),
      sub_typing_rules(caption: "Subtyping"),
    ),
  ),
)

#figure(
  caption: "Record rules",
  rect(
    inset: 20pt,
    stack(
      spacing: 3em,
      align(
        left,
        sub_typing_rules(
          caption: "Syntax",
          [
            $
              #type_name("Record") & | {overline(a)} | #b[rec] {overline(a)}
            $

            #text(weight: "bold", smallcaps("Inherit"))
            $
              "ι" & ::= #text(weight: "bold")[inherit] l_0 " … " l_n; " | " #text(weight: "bold")[inherit] (ρ) " " l_0 " … " l_n; \
              ρ & ::= l | ρ.l \
              a & ::= l = t; " | " ι \
            $],
        ),
      ),
      sub_typing_rules(
        caption: "Reduction",
        [$
            #rule_name("R-Record-Concat")&& t_1 " //" t_2 & arrow.long {…t_2 , …t_1} \
            #rule_name("R-Lookup")&& {oi(l_i\: t_i)}.l & arrow.long t_i #h(0.5cm) &&&"if" ∃i. l_i = l \
            #rule_name("R-Lookup-Null")&& {oi(l_i \= t_i)}.l & arrow.long "null" &&&"if" ∄i. l_i = l \
            #rule_name("R-Lookup-Default-Pos")&& {oi(l_i\: t_i)}.l" or "t & arrow.long t_i &&&"if" ∃i. l_i = l \
            #rule_name("R-Lookup-Default-Neg")&& {oi(l_i\: t_i)}.l" or "t & arrow.long t &&&"if" ∄i. l_i = l \
            #rule_name("R-Has-Pos")&& {oi(l_i\: t_i)}.l" ? "t & arrow.long "true" &&&"if" ∃i. l_i = l \
            #rule_name("R-Has-Neg")&& {oi(l_i\: t_i)}.l" ? "t & arrow.long "false" &&&"if" ∄i. l_i = l \
          $],
      ),
      sub_typing_rules(
        caption: "Typing",
        derive(
          "T-Rcd",
          ($Γ tack t_0: τ_0$, "...", $Γ tack t_n: τ_n$),
          $Γ tack {arrow(l): arrow(t)}: {arrow(l): arrow(τ)}$,
        ),
        derive("T-Proj", ($ Γ tack t: {l: τ} $,), $Γ tack t.l: τ$),
        derive(
          "T-Rec-Concat",
          ($Γ tack a: { oi(l\: τ) }$, $Γ tack b: { l_j: τ_j }$),
          $Γ tack a "//" b: {..b, ..a}$,
        ),
      ),
      sub_typing_rules(caption: "Subtyping"),
    ),
  ),
)
