#import "../functions.typ": *
#import "../figures/nix-parity.typ": nix-parity

= MacroNix <app-macronix>

The Lean development extends MiniNix beyond the first-class labels of @sec-fc-labels by the constructs of NixLang whose typing is routine: booleans, conditionals, assertions, the attribute test `e ? l`, homogeneous lists and a set of builtins. We call the extension MacroNix. It adds no idea to the type system, and the thesis does not present it beyond this appendix; its purpose is that the mechanized results hold for a language closer to NixLang, and that the test programs of @sec-idioms and of the Nix test suite can be stated without encoding.

$
  e & ::= dots | p #h(0.2em) e_1 dots e_n | #b[true] | #b[false] | #b[if] e #b[then] e_1 #b[else] e_2 | #b[assert] e_1 ; e_2 | [] | e_1 :: e_2 | e #h(0.2em) ? #h(0.2em) l \
  τ & ::= dots | [τ]
$

A builtin p is applied to all of its arguments; a partial application is written as a λ-abstraction. Lists are homogeneous, and the cell $e_1 :: e_2$ is lazy, as a record field is. The derived forms of NixLang, `&&`, `||`, `!=` and `e.l or d`, are abbreviations.

*Signature.* The constants and builtins are given by a signature Σ: the base type of each constant, an optional base type `bool` of the literals, the type of each builtin, whose free variables are all generic, and a reduction δ of each builtin on argument values, which may fail. MiniNix is the signature without booleans and builtins. A signature is _sound_ if no constant has type `bool` and δ preserves types: whenever the arguments $overline(v)$ have the types $overline(τ)$ and an instance of the builtin's type is $overline(τ) -> τ$, the result of $δ(p, overline(v))$ has type τ.

*Rules.* The typing rules are the standard ones. A builtin is typed at an instance of its type, a conditional and an assertion require a condition of type `bool`, a conditional types both branches at a common type, and `e ? l` requires a record and has type `bool`. It refines nothing; refining by such a test is occurrence typing. Unification decomposes $[τ_1] ≐ [τ_2]$ to $τ_1 ≐ τ_2$, and the inference rules are those of algorithm W, with the condition unified with `bool` and the branches with each other.

#theorem(name: [MacroNix metatheory], lean: "qProgress, qPreservation, inferSound, runF_terminates")[
  For every sound signature, progress up to errors and preservation hold, where an error is now also a failed assertion or a failing builtin. Inference is sound and terminates, and every leg of the unification trichotomy carries the list case.
]

#theorem(name: [Conservativity], lean: "qtyped_mini_iff, runF_mini, Step.mini")[
  On MiniNix terms, typing, reduction and inference do not depend on the booleans or builtins of the signature. The results of this thesis are the instances of the extended theorems at the signature of MiniNix.
]

The concrete signature of the development provides integers, strings and `null` as constants and the builtins `+`, `-`, `*`, `/`, `<`, `<=`, `==`, `!`, `++`, `head`, `tail`, `elemAt`, `length`, `map`, `filter`, `foldl'`, `toString`, `isAttrs` and string concatenation; it is sound (`macroSig_ok`). Equality compares atomic values only and fails on records, functions and non-empty lists.

@fig-nix-parity lists which constructs and builtins of NixLang the development covers, which are encoded or restricted, and why the remaining ones are not in place.

#nix-parity <fig-nix-parity>

One limitation is new. Since the branches of a conditional are unified, a conditional whose branches have different record types is rejected, as in the conditional extension of @sec-idioms written with `if` instead of a parameter. The declarative system rejects it as well: the only common type of `{a = 1;}` and `{}` is ★, which cannot be concatenated. Rows cannot express that a field may be present.
