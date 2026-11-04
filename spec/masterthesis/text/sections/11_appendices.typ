#import "../functions.typ": *
#import "../figures/nix-parity.typ": nix-parity
#import "04_unification.typ": u_clash, u_occurs, u_stuck
#import "05_inference.typ": st_park, st_solve, st_wake

= Row Unification Rules <app-row-rules>

@unification-row states the moves as rules. The order is fixed by @unification-cascade. Every move except the two cancel moves and U-stuck is tried in both directions.

U-field-L and U-field-R extract their partner with a window function. $"win"_l (s) = (τ, s′)$ states that τ is the type of the first l-field in the leading window of s and that s′ is s without this field. $"win"_l (s)$ is undefined if the leading window has no l-field. $"win"^R_l$ is the mirror image: it takes the last l-field in the trailing window.

U-ground uses $"rem"_l (s) = (τ, s′)$, which takes the first l-field anywhere in s and may therefore pass variables. U-host uses $"host"_l (τ, t, s) = β$, which states that β is the sole host in s of the field $l: τ$ with the rest t of the first side, as defined in @unification.

#let unification_row = figure(
  caption: "Unification of rows, on spines.",
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
)
#unification_row <unification-row>


= Replay Rules <app-replay>

@replay-rules gives the replay $scripts(↝)^*$ of a list of stumps, which A-var runs on the instantiated stumps of a scheme. K-cons wakes a stump and K-park parks a stump that is still blocked.

#let replay_rules = figure(
  caption: "Replaying a list of stumps.",
  flexbox(
    derive("K-nil", (), $S ⊢ ∅ scripts(↝)^* S$),
    derive(
      "K-cons",
      ($S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_wake S₁$, $S₁ ⊢ Δ′ scripts(↝)^* S′$),
      $S ⊢ (⟨α ⊳ ρ.l ↓ δ⟩, Δ′) scripts(↝)^* S′$,
    ),
    derive(
      "K-park",
      (
        $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
        $(S #st_park ⟨α ⊳ ρ.l ↓ δ⟩) ⊢ Δ′ scripts(↝)^* S′$,
      ),
      $S ⊢ (⟨α ⊳ ρ.l ↓ δ⟩, Δ′) scripts(↝)^* S′$,
    ),
  ),
)
#replay_rules <replay-rules>

= First-Class Labels: Rules <app-fc-labels>

This appendix collects the rules and lemmas of the first-class labels extension (@fc-labels).

== Keyed Lookup

The rules L-ε, L-var and the three concatenation rules of @row-lookup carry over to @fc-lookup with k in place of l. L-hit and L-miss are the rules of @row-lookup under a literal key, and L-\? is a new source of ?.

#lemma(
  name: [Keyed lookup],
  lean: "LookupQ.det, LookupQ.total, LookupQ.applySubst",
)[
  For every row ρ and key k there is exactly one lookup result r with $ρ.k ↓ r$. If $r ≠ #h(0.2em) ?$, then $(θ ρ).(θ k) ↓ θ r$ for every substitution θ.
] <lem-keyed-lookup>

#proof[As for @lem-lookup-det and @lem-lookup-stable. A definite derivation compares only literals, and θ fixes every literal.]

== Row Equivalence and Barriers

#let fc_equiv = figure(
  caption: [Row equivalence for keyed fields, extending @row-equivalence.],
  flexbox(
    derive("≈-dfield", ($τ₁ ≈ τ₂$,), $(\${k}: τ₁) ≈ (\${k}: τ₂)$),
    derive("≈-dlab", (), $(\${l}: τ) ≈ (l: τ)$),
  ),
)
#fc_equiv <fc-equiv>

Row equivalence extends to keyed fields by two rules (@fc-equiv). ≈-dfield is congruence under one key. ≈-dlab identifies a keyed field under a literal with the literal field. ≈-comm needs no extension: its premise $l₁ ≠ l₂$ is apartness, and only literals are apart. A keyed barrier therefore commutes with nothing, not even with a field under the same label variable: both fields carry the same label at every instance, and the left one wins.

The trace monoid of @trace-monoid carries over with barriers in place of row-variables. Spines acquire two atoms, $a ::= l: τ | β | \${α}: τ | \${∗}: τ$. A keyed field under a literal needs no atom, since ≈-dlab normalizes it to a literal field. A segment is a barrier-free part of a spine, and @thm-char reads: $ρ₁ ≈ ρ₂$ if and only if both rows have the same sequence of barriers, where two keyed barriers are the same when their keys are equal and their field types are equivalent, and the fields of every label agree pointwise up to ≈ in every segment (`rowEquiv_iff_char`). The monoid remains cancellative on both sides (@lem-cancel).


== Typing and Evaluation

#let fc_typing = figure(
  caption: "Typing rules and reduction for first-class labels.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Typing")[
      #flexbox(
        derive("T-lab", (), $Γ ⊢ \"l\": ⌊l⌋$),
        derive("T-lab-blur", ($Γ ⊢ e: ⌊k⌋$,), $Γ ⊢ e: ⌊∗⌋$),
        derive(
          "T-sel-dyn",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ τ$),
          $Γ ⊢ e₁.\${e₂}: τ$,
        ),
        derive(
          "T-sel-dyn-★",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ #h(0.2em) ?$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-sel-dyn-⊥",
          ($Γ ⊢ e₁: {ρ}$, $Γ ⊢ e₂: ⌊k⌋$, $ρ.k ↓ ⊥$),
          $Γ ⊢ e₁.\${e₂}: ★$,
        ),
        derive(
          "T-rcd-dyn",
          ($Γ ⊢ e₁: ⌊k⌋$, $Γ ⊢ e₂: τ$),
          $Γ ⊢ { \${e₁} = e₂ }: { \${k}: τ }$,
        ),
        derive("T-ξ-blur", ($Γ ⊢ e: τ$,), $Γ ⊢ (l = e): (\${∗}: τ)$),
      )
    ],
    subbox(caption: "Reduction")[
      $
          #type_name("Value") v & ::= … | \"l\" \
        #type_name("Context") E & ::= … | E.\${e} | v.\${E} | { \${E} = e } \
      $
      #flexbox(
        derive("E-sel-dyn", ($ξ(l) = e$,), ${ξ}.\${\"l\"} → e$),
        derive("E-rcd-dyn", (), ${ \${\"l\"} = e } → { l = e }$),
        derive("↯-sel-dyn", ($ξ(l) "undefined"$,), ${ξ}.\${\"l\"} ↯$),
      )
    ],
  )),
)
#fc_typing <fc-typing>

@fc-typing gives the typing rules and the reduction. T-lab types $\"l\"$ at $⌊l⌋$. The three rules for dynamic selection are the rules of static selection with a key k in place of the label l, where e₂ provides k. T-rcd-dyn records the key of e₁ in a keyed field.

T-lab-blur and T-ξ-blur forget information, like T-★-intro. T-lab-blur lets a literal stand where an arbitrary string is expected, for example as the argument of a function of type $⌊∗⌋ → τ$. T-ξ-blur moves a field under the key ∗. The inference rules never blur.

The evaluation rules reduce the key to a string literal and then proceed as for a static label. ↯-sel-dyn is the lookup error of a selection under an absent label.

== Unification

#let fc_unif = figure(
  caption: [Unification of keys and of keyed fields, extending @unification-ty and @unification-cascade.],
  flexbox(
    derive("U-key-refl", (), $⌊k⌋ ≐ ⌊k⌋ ⇝ ∅$),
    derive("U-key-bind", ($k ≠ α$,), $⌊α⌋ ≐ ⌊k⌋ ⇝ [α ≔ k]$),
    derive(
      "U-key-clash",
      ($k, k′ "not variables"$, $k ≠ k′$),
      $⌊k⌋ ≐ ⌊k′⌋ ⇝ #u_clash$,
    ),
    derive(
      "U-key-pin",
      ($k ≠ α$, $[α ≔ k]τ ≐ [α ≔ k]τ′ ⇝ θ$),
      $⟨\${α}: τ⟩ scripts(≐)_r ⟨\${k}: τ′⟩ ⇝ θ ∘ [α ≔ k]$,
    ),
  ),
)
#fc_unif <fc-unification>

@fc-unification extends unification with the rules for keys. Equal keys unify (U-key-refl), a variable binds to any other key (U-key-bind), also with the sides exchanged, and two different keys that are not variables clash (U-key-clash). The key ∗ is therefore rigid: it unifies with itself and with a variable, and $⌊∗⌋$ clashes with $⌊l⌋$. U-key-pin is the one new move on rows.

U-key-pin applies when each side consists of a single field under different keys and at least one key is a variable. A one-field row is equivalent to another only under the same key, so every unifier identifies the two keys. The move binds the variable to the other key and unifies the field types under this binding, since α may occur in them. U-key-pin is tried in both directions. It solves $⟨l: τ⟩ scripts(≐)_r ⟨\${α}: τ′⟩$ with $[α ≔ l]$ and $⟨\${α}: τ⟩ scripts(≐)_r ⟨\${β}: τ′⟩$ with $[α ≔ β]$. The second solution is most general, since $[α ≔ l, β ≔ l] = [β ≔ l] ∘ [α ≔ β]$. Against $\${∗}: τ′$, the move binds α to ∗.

For more than one field, the binding is not forced (@fc-labels), so keyed fields under different keys stay stuck. A literal field against a field under ∗, $⟨l: τ⟩ scripts(≐)_r ⟨\${∗}: τ′⟩$, has no unifier, but no move detects this, and the verdict is stuck. The metatheory of @unification-metatheory holds as stated: success is most general, clash and occurs are sound, and the algorithm terminates. U-key-pin always binds a variable, so the residual has fewer variables.

== Inference <sec-fc-inference>

#let fc_infer = figure(
  caption: [Algorithmic rules for first-class labels],
  flexbox(
    derive("A-lab", (), $Γ; S ⊢ \"l\" ⇒ ⌊l⌋; S$),
    derive(
      "A-sel-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) "fresh" β: "Row" #h(2em) S₁ ⊢ τ₁ ≐ {β} #st_solve S₂ #h(2em) Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃$,
        $"fresh" α: "Label" #h(2em) S₃ ⊢ τ₂ ≐ ⌊α⌋ #st_solve S₄ #h(2em) (⟦S₄⟧β).(⟦S₄⟧α) ↓ τ′$,
      ),
      $Γ; S ⊢ e₁.\${e₂} ⇒ τ′; S₄$,
    ),
    derive(
      "A-sel-dyn-⊥",
      ($…$, $(⟦S₄⟧β).(⟦S₄⟧α) ↓ ⊥ "or opaque"$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ ★; S₄$,
    ),
    derive(
      "A-sel-dyn-?",
      ($…$, $(⟦S₄⟧β).(⟦S₄⟧α) ↓ #h(0.2em) ? "on" γ$, $"fresh" δ: "Type"$),
      $Γ; S ⊢ e₁.\${e₂} ⇒ δ; S₄ #st_park ⟨γ ⊳ β.α ↓ δ⟩$,
    ),
    derive(
      "A-rcd-dyn",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$,
        $"fresh" α: "Label" #h(2em) S₂ ⊢ τ₁ ≐ ⌊α⌋ #st_solve S₃$,
      ),
      $Γ; S ⊢ { \${e₁} = e₂ } ⇒ { \${α}: τ₂ }; S₃$,
    ),
  ),
)
#fc_infer <fc-inference>

@fc-inference gives the algorithmic rules. Dynamic selection follows A-sel, A-sel-⊥ and A-sel-\? of @inference-rules: the key is unified with $⌊α⌋$ for a fresh label variable α, and the keyed lookup runs on the row and the key as solved so far. A-sel-dyn-⊥ also answers an opaque lookup with ★, since no instance decides it. A-sel-dyn-\? parks the stump on its blocker, which may be a label variable. A-rcd-dyn performs no lookup and parks nothing, and the keyed field records α.

= MacroNix <app-macronix>

The Lean development extends MiniNix beyond the first-class labels of @fc-labels by the constructs of NixLang whose typing is routine: booleans, conditionals, assertions, the attribute test `e ? l`, homogeneous lists and a set of builtins. We call the extension MacroNix. Its purpose is that the mechanized results hold for a language closer to NixLang, and that the test programs of @sec-idioms and of the Nix test suite can be stated without encoding.

$
  e & ::= dots | p #h(0.2em) e_1 dots e_n | #b[true] | #b[false] | #b[if] e #b[then] e_1 #b[else] e_2 | #b[assert] e_1 ; e_2 | [] | e_1 :: e_2 | e #h(0.2em) ? #h(0.2em) l \
  τ & ::= dots | [τ]
$

A builtin p is applied to all of its arguments; a partial application is written as a λ-abstraction. Lists are homogeneous, and the cell $e_1 :: e_2$ is lazy, as a record field is. The derived forms of NixLang, `&&`, `||`, `!=` and `e.l or d`, are abbreviations.

*Signature.* The constants and builtins are given by a signature Σ: the base type of each constant, an optional base type `bool` of the literals, the type of each builtin, whose free variables are all generic, and a reduction δ of each builtin on argument values, which may fail. MiniNix is the signature without booleans and builtins. A signature is _sound_ if no constant has type `bool` and δ preserves types: whenever the arguments $overline(v)$ have the types $overline(τ)$ and an instance of the builtin's type is $overline(τ) -> τ$, the result of $δ(p, overline(v))$ has type τ. Soundness constrains δ only where it succeeds: a builtin may fail on well-typed arguments, as `head []` does, and progress counts such a failure as an error. A signature whose builtins fail on every argument is therefore sound as well.

*Rules.* The typing rules are the standard ones. A builtin is typed at an instance of its type, a conditional and an assertion require a condition of type `bool`, a conditional types both branches at a common type, and `e ? l` requires a record and has type `bool`. Unification decomposes $[τ_1] ≐ [τ_2]$ to $τ_1 ≐ τ_2$, and the inference rules are those of algorithm W, with the condition unified with `bool` and the branches with each other. Strings meet in two places without unification: branches under two different definite keys join at $⌊∗⌋$ (A-if-join), and a list cell whose head is a string literal has type $[⌊∗⌋]$ (A-cons-str).

#theorem(
  name: [MacroNix metatheory],
  lean: "qProgress, qPreservation, inferSound, runF_terminates",
)[
  For every sound signature, progress up to errors and preservation hold, where an error is now also a failed assertion or a failing builtin. Inference is sound and terminates, and the unification theorems of @unification-metatheory carry the list case.
]

#theorem(name: [Conservativity], lean: "qtyped_mini_iff, runF_mini, Step.mini")[
  On MiniNix terms, typing, reduction and inference do not depend on the booleans or builtins of the signature. The results of this thesis are the instances of the extended theorems at the signature of MiniNix.
]

The concrete signature of the development provides integers and `null` as constants, strings as labels, and the builtins `+`, `-`, `*`, `/`, `<`, `<=`, `==`, `!`, `++`, `head`, `tail`, `elemAt`, `length`, `map`, `filter`, `foldl'`, `toString`, `isAttrs` and string concatenation; it is sound (`macroSig_ok`). Equality compares atomic values only: constants, booleans, labels and the empty list. Atoms of different kinds are unequal, as in Nix, so `1 == true` and `1 == "a"` are false. Equality fails on records, functions and non-empty lists, except that the empty list against a non-empty list is false.

@fig-nix-parity lists which constructs and builtins of NixLang the development covers, which are encoded or restricted, and why the remaining ones are not in place.

#nix-parity <fig-nix-parity>
