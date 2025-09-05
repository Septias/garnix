	-*- mode: agda2;-*-

```
module Lecture8 where
open import Data.Empty using (⊥)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Product using (_×_; proj₁; proj₂; _,_; ∃-syntax)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.String using (String; _≟_)
open import Data.Unit using (⊤; tt)
open import Function using (_∘_)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; cong; sym; trans)
open import Relation.Nullary using (¬_; contradiction)
open import Relation.Nullary.Decidable using (Dec; yes; no; False; toWitnessFalse; ¬?)
```

# Lambda Calculus

## Syntax

```
Id : Set
Id = String

infix  5  ƛ_⇒_
infix  5  μ_⇒_
infixl 7  _·_
infix  8  `suc_
infix  9  `_

data Term : Set where

  -- the pure lambda calculus

  `_                      :  Id → Term
  ƛ_⇒_                    :  Id → Term → Term
  _·_                     :  Term → Term → Term

  -- "applied" lambda calculus with built-in numbers

  `zero                   :  Term
  `suc_                   :  Term → Term
  case_[zero⇒_|suc_⇒_]    :  Term → Term → Id → Term → Term

  -- recursion

  μ_⇒_                    :  Id → Term → Term

variable
  L L′ M M′ N N′ V : Term
```

## Example terms

```
two : Term
two = `suc `suc `zero

plus : Term
plus = μ "plus" ⇒ ƛ "x" ⇒ (ƛ "y" ⇒ case (` "x") [zero⇒ ` "y"
                                                |suc "x'" ⇒ (`suc (` "plus" · ` "x'" · ` "y")) ])
```

## Church numerals

Idea of Church numerals:
Encode _n_ as a function that takes two parameters _f_ and _x_ and it applies _f_ _n_ times to _x_.
$λ f → λ x → f^(n) (x)$

```
zeroᶜ : Term
zeroᶜ = ƛ "f" ⇒ ƛ "x" ⇒ ` "x"

twoᶜ : Term
twoᶜ = ƛ "f" ⇒ ƛ "x" ⇒ ` "f" · (` "f" · ` "x")

plusᶜ : Term
plusᶜ = ƛ "m" ⇒ ƛ "n" ⇒
         ƛ "f" ⇒ ƛ "x" ⇒ ` "m" · ` "f" · (` "n" · ` "f" · ` "x" )
```

## Converting Church numerals to numbers

The following function takes a Church-encoded numeral _n_ and calculates is equivalent in terms of `suc` and `zero`.

(Strange function from  the book.)

```
sucᶜ : Term
sucᶜ =  ƛ "m" ⇒ `suc ` "m"
```

```
c⇒num : Term
c⇒num = ƛ "m" ⇒ ` "m" · sucᶜ · `zero
```

(the backward translation requires a recursive function and an encoding of successor on Church numerals...)

Encoding of successor on Church numerals.
So the argument `"n"` is a Church numeral...

```
succᶜ : Term
succᶜ = ƛ "n" ⇒ (ƛ "f" ⇒ ƛ "x" ⇒ ` "f" · (` "n" · ` "f" · ` "x") )
```

## Bound and free variables

In this term, `"x"` appears free (in front) and bound (under the lambda).

```
annoying : Term
annoying = ` "x" · (ƛ "x" ⇒ ` "x")
```

A term is *closed* if no variables appear free in it.

## Values

1. Functional language usually stop evaluating at lambdas.
2. Numbers are strict, i.e., the successor cannot contain unevaluated material (as its argument must be a value)

```
data Value : Term → Set where

  ƛ_⇒_ : (x : Id) (N : Term)
      ---------------
    → Value (ƛ x ⇒ N)

  `zero :
      -----------
      Value `zero

  `suc_ : ∀ {V}
    → Value V
      --------------
    → Value (`suc V)
```

(Different from book: here value syntax corresponds to term syntax)

## Substitution

First step towards semantics: operational semantics, small step reduction semantics.

In a function application `(ƛ "x" ⇒ M) · V`
we proceed by substituting the free occurrences of "x" in M by the value V.

Go through examples in the book.
Assumption: V is closed!

```
infix 9 _[_:=_]
module original where 
  _[_:=_] : Term → Id → Term → Term
  (` x) [ y := V ] with x ≟ y
  ... | yes _         = V
  ... | no  _         = ` x
  (ƛ x ⇒ N) [ y := V ] with x ≟ y
  ... | yes _         = ƛ x ⇒ N
  ... | no  _         = ƛ x ⇒ N [ y := V ]
  (L · M) [ y := V ]  = L [ y := V ] · M [ y := V ]
  (`zero) [ y := V ]  = `zero
  (`suc M) [ y := V ] = `suc M [ y := V ]
  (case L [zero⇒ M |suc x ⇒ N ]) [ y := V ] with x ≟ y
  ... | yes _         = case L [ y := V ] [zero⇒ M [ y := V ] |suc x ⇒ N ]
  ... | no  _         = case L [ y := V ] [zero⇒ M [ y := V ] |suc x ⇒ N [ y := V ] ]
  (μ x ⇒ N) [ y := V ] with x ≟ y
  ... | yes _         = μ x ⇒ N
  ... | no  _         = μ x ⇒ N [ y := V ]
```

### Exercise `_[_:=_]′ (stretch)`

The definition of substitution above has three clauses (ƛ, case, and μ) that invoke a with clause to deal with bound variables. Rewrite the definition to factor the common part of these three clauses into a single function, defined by mutual recursion with substitution. 

```
do-binder : Id → Term → Id → Term → Term
_[_:=_] : Term → Id → Term → Term

do-binder x N y V with x ≟ y
... | yes _         = N
... | no  _         = N [ y := V ]

(` x) [ y := V ] with x ≟ y
... | yes _         = V
... | no  _         = ` x
(ƛ x ⇒ N) [ y := V ] = ƛ x ⇒ do-binder x N y V
(L · M) [ y := V ]  = L [ y := V ] · M [ y := V ]
(`zero) [ y := V ]  = `zero
(`suc M) [ y := V ] = `suc M [ y := V ]
(case L [zero⇒ M |suc x ⇒ N ]) [ y := V ] = case L [ y := V ] [zero⇒ M [ y := V ] |suc x ⇒ do-binder x N y V ]
(μ x ⇒ N) [ y := V ] = μ x ⇒ do-binder x N y V
```

## Reduction

Operational semantics
Small-step reduction semantics

Informal rules for function application:

>    L —→ L′
>    --------------- ξ-·₁
>    L · M —→ L′ · M
> 
>    M —→ M′
>    --------------- ξ-·₂
>    V · M —→ V · M′
> 
>    ----------------------------- β-ƛ
>    (ƛ x ⇒ N) · V —→ N [ x := V ]

Formally, we define reduction as an inductive relation.
The relation specifies left-to-right, call-by-value reduction.

(For call-by-name, we would omit ξ-·₂ and omit the restriction to a value argument in β-ƛ)

```
infix 4 _—→_

data _—→_ : Term → Term → Set where

  ξ-·₁ : ∀ {L L′ M}
    → L —→ L′
      -----------------
    → L · M —→ L′ · M

  ξ-·₂ : ∀ {V M M′}
    → Value V
    → M —→ M′
      -----------------
    → V · M —→ V · M′

  β-ƛ : ∀ {x N V}
    → Value V
      ------------------------------
    → (ƛ x ⇒ N) · V —→ N [ x := V ]

  ξ-suc : ∀ {M M′}
    → M —→ M′
      ------------------
    → `suc M —→ `suc M′

  ξ-case : ∀ {x L L′ M N}
    → L —→ L′
      -----------------------------------------------------------------
    → case L [zero⇒ M |suc x ⇒ N ] —→ case L′ [zero⇒ M |suc x ⇒ N ]

  β-zero : ∀ {x M N}
      ----------------------------------------
    → case `zero [zero⇒ M |suc x ⇒ N ] —→ M

  β-suc : ∀ {x V M N}
    → Value V
      ---------------------------------------------------
    → case `suc V [zero⇒ M |suc x ⇒ N ] —→ N [ x := V ]

  β-μ : ∀ {x M}
      ------------------------------
    → μ x ⇒ M —→ M [ x := μ x ⇒ M ]
```

Note: you could have a term like this

```
_ : Term
_ = μ "x" ⇒ `suc ` "x"
```


## Reflexive and transitive closure

Rationale: 

```
infix  2 _—↠_
infix  1 begin_
infixr 2 _—→⟨_⟩_
infix  3 _∎

data _—↠_ : Term → Term → Set where
  _∎ : ∀ M
      ---------
    → M —↠ M

  step—→ : ∀ L {M N}
    → M —↠ N
    → L —→ M
      ---------
    → L —↠ N

pattern _—→⟨_⟩_ L L—→M M—↠N = step—→ L M—↠N L—→M

begin_ : ∀ {M N}
  → M —↠ N
    ------
  → M —↠ N
begin M—↠N = M—↠N
```

## Properties of reduction

```
postulate
  confluence : ∀ {L M N}
    → ((L —↠ M) × (L —↠ N))
      --------------------
    → ∃[ P ] ((M —↠ P) × (N —↠ P))

  diamond : ∀ {L M N}
    → ((L —→ M) × (L —→ N))
      --------------------
    → ∃[ P ] ((M —↠ P) × (N —↠ P))
```

```
postulate
  deterministic : ∀ {L M N}
    → L —→ M
    → L —→ N
      ------
    → M ≡ N
```

## Examples

```
_ : twoᶜ · sucᶜ · `zero —↠ `suc `suc `zero
_ = step—→ (twoᶜ · sucᶜ · `zero)
      (step—→ ((ƛ "x" ⇒ ` "f" · (` "f" · ` "x")) [ "f" := sucᶜ ] · `zero)
       (step—→
        (do-binder "x" (` "f" · (` "f" · ` "x")) "f" sucᶜ [ "x" := `zero ])
        (step—→
         (((` "f") [ "f" := sucᶜ ]) [ "x" := `zero ] ·
          do-binder "m" (`suc ` "m") "x" `zero [ "m" :=
          ((` "x") [ "f" := sucᶜ ]) [ "x" := `zero ] ])
         (do-binder "m" (`suc ` "m") "x" `zero [ "m" :=
          do-binder "m" (`suc ` "m") "x" `zero [ "m" :=
          ((` "x") [ "f" := sucᶜ ]) [ "x" := `zero ] ]
          ]
          ∎)
         (β-ƛ (`suc `zero)))
        (ξ-·₂ (ƛ "m" ⇒ do-binder "m" (`suc ` "m") "x" `zero) (β-ƛ `zero)))
       (β-ƛ `zero))
      (ξ-·₁ (β-ƛ (ƛ "m" ⇒ `suc ` "m")))
```

```
_ : plus · two · two —↠ `suc `suc `suc `suc `zero
_ = step—→ (plus · two · two)
      (step—→
       ((ƛ "x" ⇒
         (ƛ "y" ⇒
          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
          `suc (` "plus" · ` "x'" · ` "y") ]))
        [ "plus" :=
        μ "plus" ⇒
        (ƛ "x" ⇒
         (ƛ "y" ⇒
          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
          `suc (` "plus" · ` "x'" · ` "y") ]))
        ]
        · two
        · two)
       (step—→
        (do-binder "x"
         (ƛ "y" ⇒
          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
          `suc (` "plus" · ` "x'" · ` "y") ])
         "plus"
         (μ "plus" ⇒
          (ƛ "x" ⇒
           (ƛ "y" ⇒
            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
            `suc (` "plus" · ` "x'" · ` "y") ])))
         [ "x" := two ]
         · two)
        (step—→
         (do-binder "y"
          (do-binder "y"
           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
           `suc (` "plus" · ` "x'" · ` "y") ]
           "plus"
           (μ "plus" ⇒
            (ƛ "x" ⇒
             (ƛ "y" ⇒
              case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
              `suc (` "plus" · ` "x'" · ` "y") ]))))
          "x" two
          [ "y" := two ])
         (step—→
          (do-binder "x'"
           (do-binder "x'"
            (do-binder "x'" (`suc (` "plus" · ` "x'" · ` "y")) "plus"
             (μ "plus" ⇒
              (ƛ "x" ⇒
               (ƛ "y" ⇒
                case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                `suc (` "plus" · ` "x'" · ` "y") ]))))
            "x" two)
           "y" two
           [ "x'" := (`suc `zero) [ "y" := two ] ])
          (step—→
           (`suc
            (do-binder "plus"
             (do-binder "plus"
              (do-binder "plus"
               (ƛ "x" ⇒
                (ƛ "y" ⇒
                 case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                 `suc (` "plus" · ` "x'" · ` "y") ]))
               "x" two)
              "y" two)
             "x'" ((`suc `zero) [ "y" := two ])
             [ "plus" :=
             μ "plus" ⇒
             do-binder "plus"
             (do-binder "plus"
              (do-binder "plus"
               (ƛ "x" ⇒
                (ƛ "y" ⇒
                 case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                 `suc (` "plus" · ` "x'" · ` "y") ]))
               "x" two)
              "y" two)
             "x'" ((`suc `zero) [ "y" := two ])
             ]
             ·
             ((((` "x'") [ "plus" :=
                μ "plus" ⇒
                (ƛ "x" ⇒
                 (ƛ "y" ⇒
                  case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                  `suc (` "plus" · ` "x'" · ` "y") ]))
                ])
               [ "x" := two ])
              [ "y" := two ])
             [ "x'" := (`suc `zero) [ "y" := two ] ]
             ·
             ((((` "y") [ "plus" :=
                μ "plus" ⇒
                (ƛ "x" ⇒
                 (ƛ "y" ⇒
                  case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                  `suc (` "plus" · ` "x'" · ` "y") ]))
                ])
               [ "x" := two ])
              [ "y" := two ])
             [ "x'" := (`suc `zero) [ "y" := two ] ]))
           (step—→
            (`suc
             (do-binder "x"
              (do-binder "x"
               (do-binder "x"
                (do-binder "x"
                 (ƛ "y" ⇒
                  case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                  `suc (` "plus" · ` "x'" · ` "y") ])
                 "x" two)
                "y" two)
               "x'" ((`suc `zero) [ "y" := two ]))
              "plus"
              (μ "plus" ⇒
               do-binder "plus"
               (do-binder "plus"
                (do-binder "plus"
                 (ƛ "x" ⇒
                  (ƛ "y" ⇒
                   case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                   `suc (` "plus" · ` "x'" · ` "y") ]))
                 "x" two)
                "y" two)
               "x'" ((`suc `zero) [ "y" := two ]))
              [ "x" :=
              ((((` "x'") [ "plus" :=
                 μ "plus" ⇒
                 (ƛ "x" ⇒
                  (ƛ "y" ⇒
                   case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                   `suc (` "plus" · ` "x'" · ` "y") ]))
                 ])
                [ "x" := two ])
               [ "y" := two ])
              [ "x'" := (`suc `zero) [ "y" := two ] ]
              ]
              ·
              ((((` "y") [ "plus" :=
                 μ "plus" ⇒
                 (ƛ "x" ⇒
                  (ƛ "y" ⇒
                   case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                   `suc (` "plus" · ` "x'" · ` "y") ]))
                 ])
                [ "x" := two ])
               [ "y" := two ])
              [ "x'" := (`suc `zero) [ "y" := two ] ]))
            (step—→
             (`suc
              do-binder "y"
              (do-binder "y"
               (do-binder "y"
                (do-binder "y"
                 case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                 `suc (` "plus" · ` "x'" · ` "y") ]
                 "y" two)
                "x'" ((`suc `zero) [ "y" := two ]))
               "plus"
               (μ "plus" ⇒
                do-binder "plus"
                (do-binder "plus"
                 (do-binder "plus"
                  (ƛ "x" ⇒
                   (ƛ "y" ⇒
                    case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                    `suc (` "plus" · ` "x'" · ` "y") ]))
                  "x" two)
                 "y" two)
                "x'" ((`suc `zero) [ "y" := two ])))
              "x"
              (((((` "x'") [ "plus" :=
                  μ "plus" ⇒
                  (ƛ "x" ⇒
                   (ƛ "y" ⇒
                    case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                    `suc (` "plus" · ` "x'" · ` "y") ]))
                  ])
                 [ "x" := two ])
                [ "y" := two ])
               [ "x'" := (`suc `zero) [ "y" := two ] ])
              [ "y" :=
              ((((` "y") [ "plus" :=
                 μ "plus" ⇒
                 (ƛ "x" ⇒
                  (ƛ "y" ⇒
                   case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                   `suc (` "plus" · ` "x'" · ` "y") ]))
                 ])
                [ "x" := two ])
               [ "y" := two ])
              [ "x'" := (`suc `zero) [ "y" := two ] ]
              ])
             (step—→
              (`suc
               do-binder "x'"
               (do-binder "x'"
                (do-binder "x'"
                 (do-binder "x'" (`suc (` "plus" · ` "x'" · ` "y")) "x'"
                  ((`suc `zero) [ "y" := two ]))
                 "plus"
                 (μ "plus" ⇒
                  do-binder "plus"
                  (do-binder "plus"
                   (do-binder "plus"
                    (ƛ "x" ⇒
                     (ƛ "y" ⇒
                      case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                      `suc (` "plus" · ` "x'" · ` "y") ]))
                    "x" two)
                   "y" two)
                  "x'" ((`suc `zero) [ "y" := two ])))
                "x"
                (((((` "x'") [ "plus" :=
                    μ "plus" ⇒
                    (ƛ "x" ⇒
                     (ƛ "y" ⇒
                      case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                      `suc (` "plus" · ` "x'" · ` "y") ]))
                    ])
                   [ "x" := two ])
                  [ "y" := two ])
                 [ "x'" := (`suc `zero) [ "y" := two ] ]))
               "y"
               (((((` "y") [ "plus" :=
                   μ "plus" ⇒
                   (ƛ "x" ⇒
                    (ƛ "y" ⇒
                     case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                     `suc (` "plus" · ` "x'" · ` "y") ]))
                   ])
                  [ "x" := two ])
                 [ "y" := two ])
                [ "x'" := (`suc `zero) [ "y" := two ] ])
               [ "x'" :=
               (`zero [ "y" := two ]) [ "y" :=
               ((((` "y") [ "plus" :=
                  μ "plus" ⇒
                  (ƛ "x" ⇒
                   (ƛ "y" ⇒
                    case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                    `suc (` "plus" · ` "x'" · ` "y") ]))
                  ])
                 [ "x" := two ])
                [ "y" := two ])
               [ "x'" := (`suc `zero) [ "y" := two ] ]
               ]
               ])
              (step—→
               (`suc
                (`suc
                 (do-binder "plus"
                  (do-binder "plus"
                   (do-binder "plus"
                    (do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       "x" two)
                      "y" two)
                     "x'" ((`suc `zero) [ "y" := two ]))
                    "x"
                    (((((` "x'") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]))
                   "y"
                   (((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]))
                  "x'"
                  ((`zero [ "y" := two ]) [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ])
                  [ "plus" :=
                  μ "plus" ⇒
                  do-binder "plus"
                  (do-binder "plus"
                   (do-binder "plus"
                    (do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       "x" two)
                      "y" two)
                     "x'" ((`suc `zero) [ "y" := two ]))
                    "x"
                    (((((` "x'") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]))
                   "y"
                   (((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]))
                  "x'"
                  ((`zero [ "y" := two ]) [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ])
                  ]
                  ·
                  ((((` "x'") [ "plus" :=
                     μ "plus" ⇒
                     do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       "x" two)
                      "y" two)
                     "x'" ((`suc `zero) [ "y" := two ])
                     ])
                    [ "x" :=
                    ((((` "x'") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ])
                   [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ])
                  [ "x'" :=
                  (`zero [ "y" := two ]) [ "y" :=
                  ((((` "y") [ "plus" :=
                     μ "plus" ⇒
                     (ƛ "x" ⇒
                      (ƛ "y" ⇒
                       case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                       `suc (` "plus" · ` "x'" · ` "y") ]))
                     ])
                    [ "x" := two ])
                   [ "y" := two ])
                  [ "x'" := (`suc `zero) [ "y" := two ] ]
                  ]
                  ]
                  ·
                  ((((` "y") [ "plus" :=
                     μ "plus" ⇒
                     do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       "x" two)
                      "y" two)
                     "x'" ((`suc `zero) [ "y" := two ])
                     ])
                    [ "x" :=
                    ((((` "x'") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ])
                   [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ])
                  [ "x'" :=
                  (`zero [ "y" := two ]) [ "y" :=
                  ((((` "y") [ "plus" :=
                     μ "plus" ⇒
                     (ƛ "x" ⇒
                      (ƛ "y" ⇒
                       case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                       `suc (` "plus" · ` "x'" · ` "y") ]))
                     ])
                    [ "x" := two ])
                   [ "y" := two ])
                  [ "x'" := (`suc `zero) [ "y" := two ] ]
                  ]
                  ])))
               (step—→
                (`suc
                 (`suc
                  (do-binder "x"
                   (do-binder "x"
                    (do-binder "x"
                     (do-binder "x"
                      (do-binder "x"
                       (do-binder "x"
                        (do-binder "x"
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ])
                         "x" two)
                        "y" two)
                       "x'" ((`suc `zero) [ "y" := two ]))
                      "x"
                      (((((` "x'") [ "plus" :=
                          μ "plus" ⇒
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          ])
                         [ "x" := two ])
                        [ "y" := two ])
                       [ "x'" := (`suc `zero) [ "y" := two ] ]))
                     "y"
                     (((((` "y") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]))
                    "x'"
                    ((`zero [ "y" := two ]) [ "y" :=
                     ((((` "y") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ]))
                   "plus"
                   (μ "plus" ⇒
                    do-binder "plus"
                    (do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (do-binder "plus"
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         "x" two)
                        "y" two)
                       "x'" ((`suc `zero) [ "y" := two ]))
                      "x"
                      (((((` "x'") [ "plus" :=
                          μ "plus" ⇒
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          ])
                         [ "x" := two ])
                        [ "y" := two ])
                       [ "x'" := (`suc `zero) [ "y" := two ] ]))
                     "y"
                     (((((` "y") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]))
                    "x'"
                    ((`zero [ "y" := two ]) [ "y" :=
                     ((((` "y") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ]))
                   [ "x" :=
                   ((((` "x'") [ "plus" :=
                      μ "plus" ⇒
                      do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        "x" two)
                       "y" two)
                      "x'" ((`suc `zero) [ "y" := two ])
                      ])
                     [ "x" :=
                     ((((` "x'") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ])
                    [ "y" :=
                    ((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ])
                   [ "x'" :=
                   (`zero [ "y" := two ]) [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ]
                   ]
                   ]
                   ·
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        "x" two)
                       "y" two)
                      "x'" ((`suc `zero) [ "y" := two ])
                      ])
                     [ "x" :=
                     ((((` "x'") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ])
                    [ "y" :=
                    ((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ])
                   [ "x'" :=
                   (`zero [ "y" := two ]) [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ]
                   ])))
                (step—→
                 (`suc
                  (`suc
                   do-binder "y"
                   (do-binder "y"
                    (do-binder "y"
                     (do-binder "y"
                      (do-binder "y"
                       (do-binder "y"
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]
                        "y" two)
                       "x'" ((`suc `zero) [ "y" := two ]))
                      "y"
                      (((((` "y") [ "plus" :=
                          μ "plus" ⇒
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          ])
                         [ "x" := two ])
                        [ "y" := two ])
                       [ "x'" := (`suc `zero) [ "y" := two ] ]))
                     "x'"
                     ((`zero [ "y" := two ]) [ "y" :=
                      ((((` "y") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]
                      ]))
                    "plus"
                    (μ "plus" ⇒
                     do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (do-binder "plus"
                         (do-binder "plus"
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          "x" two)
                         "y" two)
                        "x'" ((`suc `zero) [ "y" := two ]))
                       "x"
                       (((((` "x'") [ "plus" :=
                           μ "plus" ⇒
                           (ƛ "x" ⇒
                            (ƛ "y" ⇒
                             case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                             `suc (` "plus" · ` "x'" · ` "y") ]))
                           ])
                          [ "x" := two ])
                         [ "y" := two ])
                        [ "x'" := (`suc `zero) [ "y" := two ] ]))
                      "y"
                      (((((` "y") [ "plus" :=
                          μ "plus" ⇒
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          ])
                         [ "x" := two ])
                        [ "y" := two ])
                       [ "x'" := (`suc `zero) [ "y" := two ] ]))
                     "x'"
                     ((`zero [ "y" := two ]) [ "y" :=
                      ((((` "y") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]
                      ])))
                   "x"
                   (((((` "x'") [ "plus" :=
                       μ "plus" ⇒
                       do-binder "plus"
                       (do-binder "plus"
                        (do-binder "plus"
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         "x" two)
                        "y" two)
                       "x'" ((`suc `zero) [ "y" := two ])
                       ])
                      [ "x" :=
                      ((((` "x'") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]
                      ])
                     [ "y" :=
                     ((((` "y") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ])
                    [ "x'" :=
                    (`zero [ "y" := two ]) [ "y" :=
                    ((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ]
                    ])
                   [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        "x" two)
                       "y" two)
                      "x'" ((`suc `zero) [ "y" := two ])
                      ])
                     [ "x" :=
                     ((((` "x'") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ])
                    [ "y" :=
                    ((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ])
                   [ "x'" :=
                   (`zero [ "y" := two ]) [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ]
                   ]
                   ]))
                 (`suc
                  (`suc
                   (((((` "y") [ "x'" := (`suc `zero) [ "y" := two ] ]) [ "x'" :=
                      (`zero [ "y" := two ]) [ "y" :=
                      ((((` "y") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]
                      ]
                      ])
                     [ "plus" :=
                     μ "plus" ⇒
                     do-binder "plus"
                     (do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (do-binder "plus"
                         (do-binder "plus"
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          "x" two)
                         "y" two)
                        "x'" ((`suc `zero) [ "y" := two ]))
                       "x"
                       (((((` "x'") [ "plus" :=
                           μ "plus" ⇒
                           (ƛ "x" ⇒
                            (ƛ "y" ⇒
                             case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                             `suc (` "plus" · ` "x'" · ` "y") ]))
                           ])
                          [ "x" := two ])
                         [ "y" := two ])
                        [ "x'" := (`suc `zero) [ "y" := two ] ]))
                      "y"
                      (((((` "y") [ "plus" :=
                          μ "plus" ⇒
                          (ƛ "x" ⇒
                           (ƛ "y" ⇒
                            case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                            `suc (` "plus" · ` "x'" · ` "y") ]))
                          ])
                         [ "x" := two ])
                        [ "y" := two ])
                       [ "x'" := (`suc `zero) [ "y" := two ] ]))
                     "x'"
                     ((`zero [ "y" := two ]) [ "y" :=
                      ((((` "y") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]
                      ])
                     ])
                    [ "x" :=
                    ((((` "x'") [ "plus" :=
                       μ "plus" ⇒
                       do-binder "plus"
                       (do-binder "plus"
                        (do-binder "plus"
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         "x" two)
                        "y" two)
                       "x'" ((`suc `zero) [ "y" := two ])
                       ])
                      [ "x" :=
                      ((((` "x'") [ "plus" :=
                         μ "plus" ⇒
                         (ƛ "x" ⇒
                          (ƛ "y" ⇒
                           case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                           `suc (` "plus" · ` "x'" · ` "y") ]))
                         ])
                        [ "x" := two ])
                       [ "y" := two ])
                      [ "x'" := (`suc `zero) [ "y" := two ] ]
                      ])
                     [ "y" :=
                     ((((` "y") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ])
                    [ "x'" :=
                    (`zero [ "y" := two ]) [ "y" :=
                    ((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ]
                    ]
                    ])
                   [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      do-binder "plus"
                      (do-binder "plus"
                       (do-binder "plus"
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        "x" two)
                       "y" two)
                      "x'" ((`suc `zero) [ "y" := two ])
                      ])
                     [ "x" :=
                     ((((` "x'") [ "plus" :=
                        μ "plus" ⇒
                        (ƛ "x" ⇒
                         (ƛ "y" ⇒
                          case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                          `suc (` "plus" · ` "x'" · ` "y") ]))
                        ])
                       [ "x" := two ])
                      [ "y" := two ])
                     [ "x'" := (`suc `zero) [ "y" := two ] ]
                     ])
                    [ "y" :=
                    ((((` "y") [ "plus" :=
                       μ "plus" ⇒
                       (ƛ "x" ⇒
                        (ƛ "y" ⇒
                         case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                         `suc (` "plus" · ` "x'" · ` "y") ]))
                       ])
                      [ "x" := two ])
                     [ "y" := two ])
                    [ "x'" := (`suc `zero) [ "y" := two ] ]
                    ])
                   [ "x'" :=
                   (`zero [ "y" := two ]) [ "y" :=
                   ((((` "y") [ "plus" :=
                      μ "plus" ⇒
                      (ƛ "x" ⇒
                       (ƛ "y" ⇒
                        case ` "x" [zero⇒ ` "y" |suc "x'" ⇒
                        `suc (` "plus" · ` "x'" · ` "y") ]))
                      ])
                     [ "x" := two ])
                    [ "y" := two ])
                   [ "x'" := (`suc `zero) [ "y" := two ] ]
                   ]
                   ]
                   ])
                  ∎)
                 (ξ-suc (ξ-suc β-zero)))
                (ξ-suc (ξ-suc (β-ƛ (`suc (`suc `zero))))))
               (ξ-suc (ξ-suc (ξ-·₁ (β-ƛ `zero)))))
              (ξ-suc (ξ-suc (ξ-·₁ (ξ-·₁ β-μ)))))
             (ξ-suc (β-suc `zero)))
            (ξ-suc (β-ƛ (`suc (`suc `zero)))))
           (ξ-suc (ξ-·₁ (β-ƛ (`suc `zero)))))
          (ξ-suc (ξ-·₁ (ξ-·₁ β-μ))))
         (β-suc (`suc `zero)))
        (β-ƛ (`suc (`suc `zero))))
       (ξ-·₁ (β-ƛ (`suc (`suc `zero)))))
      (ξ-·₁ (ξ-·₁ β-μ))
```

## Typing

### Syntax of Simple Types

```
infixr 7 _⇒_

data Type : Set where
  _⇒_ : Type → Type → Type
  `ℕ : Type

variable
  A B C : Type
```

### Contexts

```
infixl 5  _,_⦂_

data Context : Set where
  ∅     : Context
  _,_⦂_ : Context → Id → Type → Context
```

### Lookup Judgment

```
infix  4  _∋_⦂_

data _∋_⦂_ : Context → Id → Type → Set where

  Z : ∀ {Γ x A}
      ------------------
    → Γ , x ⦂ A ∋ x ⦂ A

  S : ∀ {Γ x y A B}
    → x ≢ y
    → Γ ∋ x ⦂ A
      ------------------
    → Γ , y ⦂ B ∋ x ⦂ A
```

A smart constructor

```
S′ : ∀ {Γ x y A B}
   → {x≢y : False (x ≟ y)}
   → Γ ∋ x ⦂ A
     ------------------
   → Γ , y ⦂ B ∋ x ⦂ A

S′ {x≢y = x≢y} x = S (toWitnessFalse x≢y) x
```

### Typing judgment

```
infix  4  _⊢_⦂_

data _⊢_⦂_ : Context → Term → Type → Set where

  -- Axiom
  ⊢` : ∀ {Γ x A}
    → Γ ∋ x ⦂ A
      -----------
    → Γ ⊢ ` x ⦂ A

  -- ⇒-I
  ⊢ƛ : ∀ {Γ x N A B}
    → Γ , x ⦂ A ⊢ N ⦂ B
      -------------------
    → Γ ⊢ ƛ x ⇒ N ⦂ A ⇒ B

  -- ⇒-E
  _·_ : ∀ {Γ L M A B}
    → Γ ⊢ L ⦂ A ⇒ B
    → Γ ⊢ M ⦂ A
      -------------
    → Γ ⊢ L · M ⦂ B

  -- ℕ-I₁
  ⊢zero : ∀ {Γ}
      --------------
    → Γ ⊢ `zero ⦂ `ℕ

  -- ℕ-I₂
  ⊢suc : ∀ {Γ M}
    → Γ ⊢ M ⦂ `ℕ
      ---------------
    → Γ ⊢ `suc M ⦂ `ℕ

  -- ℕ-E
  ⊢case : ∀ {Γ L M x N A}
    → Γ ⊢ L ⦂ `ℕ
    → Γ ⊢ M ⦂ A
    → Γ , x ⦂ `ℕ ⊢ N ⦂ A
      -------------------------------------
    → Γ ⊢ case L [zero⇒ M |suc x ⇒ N ] ⦂ A

  ⊢μ : ∀ {Γ x M A}
    → Γ , x ⦂ A ⊢ M ⦂ A
      -----------------
    → Γ ⊢ μ x ⇒ M ⦂ A
```

#### Examples

```
Ch : Type → Type
Ch A = (A ⇒ A) ⇒ A ⇒ A

⊢twoᶜ : ∀ {Γ A} → Γ ⊢ twoᶜ ⦂ Ch A
⊢twoᶜ = ⊢ƛ (⊢ƛ ((⊢` (S (λ ()) Z)) · ((⊢` (S (λ ()) Z)) · (⊢` Z))))
```

```
⊢two : ∀ {Γ} → Γ ⊢ two ⦂ `ℕ
⊢two = ⊢suc (⊢suc ⊢zero)

⊢plus : ∀ {Γ} → Γ ⊢ plus ⦂ `ℕ ⇒ `ℕ ⇒ `ℕ
⊢plus = ⊢μ (⊢ƛ (⊢ƛ (⊢case (⊢` (S (λ ()) Z)) (⊢` Z) (⊢suc (((⊢` (S (λ ()) (S (λ ()) (S (λ ()) Z)))) · (⊢` Z)) · (⊢` (S (λ ()) Z)))))))

⊢2+2 : ∅ ⊢ plus · two · two ⦂ `ℕ
⊢2+2 = (⊢plus · ⊢two) · ⊢two
```

Some untypeable terms

```
nope₁ : ∀ {A} → ¬ (∅ ⊢ `zero · `suc `zero ⦂ A)
nope₁ (() · _)

nope₂ : ∀ {A} → ¬ (∅ ⊢ ƛ "x" ⇒ ` "x" · ` "x" ⦂ A)
nope₂ (⊢ƛ (⊢` Z · ⊢` (S x≢x _))) = contradiction refl x≢x
```

### Lookup is functional

```
∋-functional : ∀ {Γ x A B} → Γ ∋ x ⦂ A → Γ ∋ x ⦂ B → A ≡ B
∋-functional Z Z = refl
∋-functional Z (S x≢x _) = contradiction refl x≢x
∋-functional (S x≢x _) Z = contradiction refl x≢x
∋-functional (S x x∈) (S x₁ x∈₁) = ∋-functional x∈ x∈₁
```

