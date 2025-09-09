module main where
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
open import Data.Fin using (Fin)

Id : Set
Id = String

infix  5  ƛ_⇒_
infix  5  μ_⇒_
infixl 7  _·_
infix  8  `suc_
infix  9  `_

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

infixr 5 _∷_


data Expr (n m : ℕ) : Set
data Label : Set

data Record : Set where
  empty     : Record
  _∣_       : ∀ {n m : ℕ} → Expr n m -> Label -> Record

data Kind : Set where
  ★  : Kind

data Type (n : ℕ) : Set where
  `_       : Fin n → Type n 
  ∀[α:_]_  : Type (suc n) → Type n
  _⇒_      : Type n → Type n → Type n

data Expr (n m : ℕ) where
  `_   : Fin m → Expr n m
  λx_  : Expr n (suc m) → Expr n m
  Λα_  : Expr (suc n) m → Expr n m
  _·_  : Expr n m → Expr n m → Expr n m
  _•_  : Expr n m → Type n → Expr n m

  `zero                   : Expr n m
  `suc_                   : Expr n m → Expr n m
  case_[zero⇒_|suc_⇒_]    : Expr n m → Expr n m → Id → Expr n m → Expr n m
  μ_⇒_                    : Id → Expr n m → Expr n m
  withh_⨟_                : Expr n m → Expr n m → Expr n m
  lett_inn_               : Expr n m → Expr n m → Expr n m
  if_then_else_           : Expr n m → Expr n m → Expr n m



-- Global variables
variable
  L L′ M M′ N N′ V : Expr

{- 
-- Definition of values
data Value : Expr → Set where
  λx_⇒_ : (x : Id) (N : Expr)
      ---------------
    → Value (λx x ⇒ N)
  `zero :
      -----------
      Value `zero
  `suc_ : ∀ {V}
    → Value V
      --------------
    → Value (`suc V)


-- Define substitution
_[_:=_] : Expr → Id → Expr → Expr
do-binder : Id → Expr → Id → Expr  → Expr
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



-- Define reduction rules
infix 4 _—→_
data _—→_ : Expr → Expr → Set where

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


-- define steps
infix  2 _—↠_
infix  1 begin_
infixr 2 _—→⟨_⟩_
infix  3 _∎

data _—↠_ : Expr → Expr → Set where
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

-- Safety properties
postulate
  confluence : ∀ {L M N}
    → ((L —↠ M) × (L —↠ N))
      --------------------
    → ∃[ P ] ((M —↠ P) × (N —↠ P))

  diamond : ∀ {L M N}
    → ((L —→ M) × (L —→ N))
      --------------------
    → ∃[ P ] ((M —↠ P) × (N —↠ P))

 -}
