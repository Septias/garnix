
```
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

Label : Set
Label = String

infixr 5 _∷_
infixl 7  _·_
infix  8  `suc_
infix  9  `_



-- Defining the syntax of the language
data Expr (n m : ℕ) : Set


```
Records are defined similarly to lists, but each field has a label and a distinct type.

@Operations:
- Record lookup
- Record concatenation

```

data Record : Set where
  empty     : Record
  _∣_       : ∀ {n m : ℕ} → Label → (Expr n m) → Record
  ‵let_‵in_ : ∀ {n m : ℕ} → Expr n m → Expr n m → Record 


-- Recursive Record
data RecRecord : Set where
  empty     : RecRecord
  _∣_       : ∀ {n m : ℕ} → Label → (Expr n m) → RecRecord
  ‵let_‵in_ : ∀ {n m : ℕ} → Expr n m → Expr n m → RecRecord 



```
Lists are defined inductively with a base case (empty list) and a recursive case (cons cell).

@Operations:
- List concatenation

```

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A


```
We need kinds because of @marius work

``` 

data Kind : Set where
  ★   : Kind



data Type (n : ℕ) : Set where
  `_       : Fin n → Type n
  ∀[α:_]_  : Type (suc n) → Type n
  _⇒_      : Type n → Type n → Type n


```
An attribute path is a sequence of labels leading to a specific attribute within a nested structure.
```

data AttrPath : Set where
  _–_      : Label → AttrPath → AttrPath
  ε        : AttrPath
 


data Expr n m where
  `_                      : Fin m → Expr n m
  λx_                     : Expr n (suc m) → Expr n m
  Λα_                     : Expr (suc n) m → Expr n m
  _·_                     : Expr n m → Expr n m → Expr n m
  _•_                     : Expr n m → Type n → Expr n m

  `zero                   : Expr n m
  `suc_                   : Expr n m → Expr n m
  
  -- Language constructs
  μx_                    :  Expr n m → Expr n m         -- TODO: this should be intrinsic too, no?
  ‵with_⨟_                : Expr n m → Expr n m → Expr n m   -- with Record ; Expr --> Introduce all fields of Record into scope 
  ‵let_inn_               : Expr n m → Expr n m → Expr n m
  if_then_else_           : Expr n m → Expr n m → Expr n m
  assert_                 : Expr n m → Expr n m              -- assert false       --> Program termination 
  _‵?_                    : Expr n m → Expr n m → Expr n m   -- Record ? file      -> boolean
  ‵lambda _⇒_             : Label → Expr n m → Expr n m   

  -- Binary operators
  _+_                     : Expr n m → Expr n m → Expr n m
  _-_                     : Expr n m → Expr n m → Expr n m
  _*_                     : Expr n m → Expr n m → Expr n m
  _/_                     : Expr n m → Expr n m → Expr n m
  _==_                    : Expr n m → Expr n m → Expr n m
  _<_                     : Expr n m → Expr n m → Expr n m
  _≤_                     : Expr n m → Expr n m → Expr n m
  _>_                     : Expr n m → Expr n m → Expr n m
  _≥_                     : Expr n m → Expr n m → Expr n m
  _&&_                    : Expr n m → Expr n m → Expr n m
  _||_                    : Expr n m → Expr n m → Expr n m


-- Definition of values
data Value (n m : ℕ) : Expr n m → Set where
  λx_ : (N : Expr n (suc m)) → Value n m (λx N)
  Λα_ : (N : Expr (suc n) m) → Value n m (Λα N)

  `zero :
      -----------
      Value n m `zero

  `suc_ : ∀ {V}
    → Value n m V
      --------------
    → Value n m (`suc V)


-- Define reduction rules
infix 4 _—→_
data _—→_ : ∀ { n m : ℕ} → Expr n m → Expr n m → Set where

  ξ-·₁ : ∀ {L L′ M}
    → L —→ L′
      -----------------
    → L · M —→ L′ · M

  ξ-·₂ : ∀ {V M M′}
    → Value V
    → M —→ M′
      -----------------
    → V · M —→ V · M′

  β-λx : ∀ {V m n} -- Beta reduction for lambda
    → Value V
      ------------------------------
    → (λx n m) · V —→ (λx n (suc m))

  ξ-suc : ∀ {M M′}
    → M —→ M′
      ------------------
    → `suc M —→ `suc M′


  β-μ : ∀ {x M}
      ------------------------------
    → μx M —→ μx M



data Context : Set where
  ∅     : Context
  _,_⦂_ : Context → Id → Type → Context


data Type : Set where
  _⇒_ : Type → Type → Type
  `ℕ : Type

variable
  A B C : Type



data _∋_⦂_ : Context → Id → Type → Set where

  Z : ∀ {Γ x A}
      ------------------
    → Γ , x ⦂ A ∋ x ⦂ A

  S : ∀ {Γ x y A B}
    → x ≢ y
    → Γ ∋ x ⦂ A
      ------------------
    → Γ , y ⦂ B ∋ x ⦂ A

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
    

{- 
-- Global variables
variable
  L L′ M M′ N N′ V : Expr


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
```
