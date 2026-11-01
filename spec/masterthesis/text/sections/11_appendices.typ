#import "../functions.typ": *
#import "../figures/nix-parity.typ": nix-parity
#import "04_unification.typ": u_clash, u_occurs, u_stuck

= Row Unification Rules <app-row-rules>

@unification-row states the moves as rules. The order is fixed by @unification-cascade. Every move except the two cancel moves and U-stuck is tried in both directions.

#let unification_row = figure(
  caption: "Unification of rows, on spines.",
  stack(
    spacing: 15pt,
    align(center, flexbox(
      $#type_name("Leading window") "win"_l (s) = (τ, s′)$,
      $#type_name("Trailing window") "win"^R_l (s) = (τ, s′)$,
      $#type_name("Anywhere") "rem"_l (s) = (τ, s′)$,
      $#type_name("Sole host") "host"_l (τ, t, s) = β$,
    )),
    flexbox(
      derive(
        "U-ε-var",
        ($s$ + [ field-free], $"vars"(s) = overline(β)$),
        $⟨⟩ scripts(≐)_r s ⇝ [overline(β) ≔ ε]$,
      ),
      derive("U-ε-clash", ($(l: τ) ∈ s$,), $⟨⟩ scripts(≐)_r s ⇝ #u_clash$),
      derive(
        "U-cancel-L",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $α · t₁ scripts(≐)_r α · t₂ ⇝ θ$,
      ),
      derive(
        "U-cancel-R",
        ($t₁ scripts(≐)_r t₂ ⇝ θ$,),
        $t₁ · α scripts(≐)_r t₂ · α ⇝ θ$,
      ),
      derive(
        "U-var-collapse",
        (
          $s$ + [ field-free],
          $α ∈ "vars"(s)$,
          $overline(β) = cases("vars"(s) ∖ α & "if" α "occurs once in" s, "vars"(s) & "otherwise")$,
        ),
        $α scripts(≐)_r s ⇝ [overline(β) ≔ ε]$,
      ),
      derive("U-var-solve", ($α ∉ "rv"(s)$,), $α scripts(≐)_r s ⇝ [α ≔ s]$),
      derive(
        "U-var-occurs",
        ($α ∈ "rv"(s)$,),
        $α scripts(≐)_r s ⇝ #u_occurs$,
      ),
      derive(
        "U-field-L",
        (
          $"win"_l (s₂) = (τ′, t₂)$,
          $τ ≐ τ′ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $(l: τ) · t₁ scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-field-R",
        (
          $"win"^R_l (s₂) = (τ′, t₂)$,
          $τ ≐ τ′ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $t₁ · (l: τ) scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-ground",
        (
          $"vars"(s₂) = ⟨⟩$,
          $|s₁|_l = |s₂|_l > 0$,
          $"rem"_l (s_i) = (τ_i, t_i)$,
          $τ₁ ≐ τ₂ ⇝ θ$,
          $θ t₁ scripts(≐)_r θ t₂ ⇝ θ′$,
        ),
        $s₁ scripts(≐)_r s₂ ⇝ θ′ ∘ θ$,
      ),
      derive(
        "U-row-clash",
        ($|s₁|_l > |s₂|_l$, $"vars"(s₂) = ⟨⟩$),
        $s₁ scripts(≐)_r s₂ ⇝ #u_clash$,
      ),
      derive(
        "U-host",
        (
          $"host"_l (τ, t₁, s₂) = β$,
          $β′ "fresh"$,
          $t₁ scripts(≐)_r s₂[β′\/β] ⇝ θ$,
        ),
        $(l: τ) · t₁ scripts(≐)_r s₂ ⇝ θ ∘ [β ≔ (l: τ | β′)]$,
      ),
      derive(
        "U-stuck",
        ([no rule above applies],),
        $s₁ scripts(≐)_r s₂ ⇝ #u_stuck$,
      ),
    ),
  ),
)
#unification_row <unification-row>


= MacroNix <app-macronix>

The Lean development extends MiniNix beyond the first-class labels of @sec-fc-labels by the constructs of NixLang whose typing is routine: booleans, conditionals, assertions, the attribute test `e ? l`, homogeneous lists and a set of builtins. We call the extension MacroNix. Its purpose is that the mechanized results hold for a language closer to NixLang, and that the test programs of @sec-idioms and of the Nix test suite can be stated without encoding.

$
  e & ::= dots | p #h(0.2em) e_1 dots e_n | #b[true] | #b[false] | #b[if] e #b[then] e_1 #b[else] e_2 | #b[assert] e_1 ; e_2 | [] | e_1 :: e_2 | e #h(0.2em) ? #h(0.2em) l \
  τ & ::= dots | [τ]
$

A builtin p is applied to all of its arguments; a partial application is written as a λ-abstraction. Lists are homogeneous, and the cell $e_1 :: e_2$ is lazy, as a record field is. The derived forms of NixLang, `&&`, `||`, `!=` and `e.l or d`, are abbreviations.

*Signature.* The constants and builtins are given by a signature Σ: the base type of each constant, an optional base type `bool` of the literals, the type of each builtin, whose free variables are all generic, and a reduction δ of each builtin on argument values, which may fail. MiniNix is the signature without booleans and builtins. A signature is _sound_ if no constant has type `bool` and δ preserves types: whenever the arguments $overline(v)$ have the types $overline(τ)$ and an instance of the builtin's type is $overline(τ) -> τ$, the result of $δ(p, overline(v))$ has type τ. Soundness constrains δ only where it succeeds: a builtin may fail on well-typed arguments, as `head []` does, and progress counts such a failure as an error. A signature whose builtins fail on every argument is therefore sound as well.

*Rules.* The typing rules are the standard ones. A builtin is typed at an instance of its type, a conditional and an assertion require a condition of type `bool`, a conditional types both branches at a common type, and `e ? l` requires a record and has type `bool`. It refines nothing; refining by such a test is occurrence typing. Unification decomposes $[τ_1] ≐ [τ_2]$ to $τ_1 ≐ τ_2$, and the inference rules are those of algorithm W, with the condition unified with `bool` and the branches with each other.

#theorem(
  name: [MacroNix metatheory],
  lean: "qProgress, qPreservation, inferSound, runF_terminates",
)[
  For every sound signature, progress up to errors and preservation hold, where an error is now also a failed assertion or a failing builtin. Inference is sound and terminates, and the unification theorems of @unification-metatheory carry the list case.
]

#theorem(name: [Conservativity], lean: "qtyped_mini_iff, runF_mini, Step.mini")[
  On MiniNix terms, typing, reduction and inference do not depend on the booleans or builtins of the signature. The results of this thesis are the instances of the extended theorems at the signature of MiniNix.
]

The concrete signature of the development provides integers, strings and `null` as constants and the builtins `+`, `-`, `*`, `/`, `<`, `<=`, `==`, `!`, `++`, `head`, `tail`, `elemAt`, `length`, `map`, `filter`, `foldl'`, `toString`, `isAttrs` and string concatenation; it is sound (`macroSig_ok`). Equality compares atomic values only. It fails on records, functions and two non-empty lists, and on atoms of different groups, where the groups are constants, booleans, labels and lists: `1 == true` fails, while Nix evaluates it to false. Two constants always compare, so `1 == "a"` is false.

@fig-nix-parity lists which constructs and builtins of NixLang the development covers, which are encoded or restricted, and why the remaining ones are not in place.

#nix-parity <fig-nix-parity>

One limitation is new. Since the branches of a conditional are unified, a conditional whose branches have different record types is rejected, as in the conditional extension of @sec-idioms written with `if` instead of a parameter. The declarative system rejects it as well: the only common type of `{a = 1;}` and `{}` is ★, which cannot be concatenated. Rows cannot express that a field may be present.
