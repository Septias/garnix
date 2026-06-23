# Formalisierung
> A typesystem with records and function patterns. We use scoped rows with row and label variables which can create ambiguous scenarious. An unkown type ★ is used when the typesystem can not derive sound types. ∈-Solving is a novel algorithm that tries to solve ∈-constraints of the form `a ∈ (A ∪ B)`` where a is a field and A,B are records. A motivating example is: `x: y: (x ‖ y).a` where x and y are variables and ‖ is the concat operator that concatenates two records with right-preference towards fields in y. 

- Only "hard" rules are given with "easy" rules being skipped

## Legend
¿: Meta-symbol to mark something that is not undoubtetly correct or missing
¡: Meta-symbol to mark something that is wrong and needs fixing


## Remarks
- ε verwende ich mehrmals für verschieden syntaktische Objekte
- {} wird auch für Syntax und Typen verwendet

## Terms & Types
Basetypes : 𝓫 ∈ 𝓑
Constants : c ∈ 𝓒
Labels    : ℓ ∈ 𝓛
Variables : x, y ∈ 𝓧
Typevars  : α, β, γ ∈ 𝓿

*Terms*
e := x | c | e₁e₂ | ς: e₂ | { e = e; } | e₁ ‖ e₂ | let e₁ = e₂ in e₃
ς := { ξ } | { ξ, … }
ξ := ε | (x | ξ) | (x ? e | ξ)

||| ¿Sind nur auf Typebene wichtig

*Types*
κ := ∗ | κ₁ -> κ₂ | Row | Lab | Pat | Unknown
σ := ∀α: κ. σ | τ
τ := 𝓫 | ★ | { p }± -> τ | ⦅l⦆ | { ρ }
ρ := ε | α | (l:τ | ρ)
p := ε | (l:τ | p) | (l: τ?τ | p)
l := α | ℓ
± ∈ {+, -}

|| ⦅l⦆ singleton type with element l
|| is it allowed to say ⦅α⦆?
||| was meinst du damit?

*Context*
Γ := • | Γ · (x: σ) | Γ · (α : κ)


## Kinding
*base*
----------- κ-base
Γ ⊢ b ∈ 𝓫: ∗

|| what is b and why is this a kinding rule?
||| In 𝓫 liegen die bastypes, die alle vom type-kind sind

α: κ ∈ Γ
--------- κ-var
Γ ⊢ α: κ

|| does not make sense; should be
|| * (x: σ)  for a typing rule
|| * or (α: κ) for a kinding rule
|| maybe this rule?
|| α: κ ∈ Γ
|| -------------------- κ-var
|| Γ ⊢ α : κ
||| Hab die angepasst

----------- κ-base-lab
Γ ⊢ ℓ: Lab

Γ ⊢ l: Lab
----------- κ-lab
Γ ⊢ ⦅l⦆: ∗

|| ok, this way, labels can only occur in types
||| Ja

*rows & records*
------------ κ-row-empty
ε: Row

Γ ⊢ l: Lab   Γ ⊢ τ: ∗   Γ ⊢ ρ: Row
---------------------------------- κ-row
Γ ⊢ (l: τ | ρ): Row

|| if you don't impose disjointness of labels, then you're going for scoped records, correct?
||| Ja

Γ ⊢ ρ: Row
----------- κ-rec
Γ ⊢ {ρ}: ∗

*patterns*

------------ κ-pat-empty¡
ε: Pat

Γ ⊢ τ: ∗  Γ ⊢ l: Lab Γ ⊢ p: Pat 
-------------------------------- κ-pat
Γ ⊢ (l: τ | p): Pat

Γ ⊢ τ: ∗  Γ ⊢ l: Lab   Γ ⊢ p: Pat
--------------------------------- κ-pat-default
Γ ⊢ (l: τ₁ ? τ₂ | p): Pat

|| this syntax indicates that (literally) the same type τ occurs twice. Intended?
||| An sich sollten default und inferierter Type gleich sein, aber wahrscheinlich ist ein judgement besser
||| Grundsätzlich kann man da ja auch was unsinniges schreiben. Ist dann die Frage, wie man
||| das in nem Typesystem greifen möchte, oder ob man nur von wohlgeformten Typen ausgeht.
||| Dann muss man aber auch nicht unbedingt die Distinction machen.

Γ ⊢ p: Pat
--------------------- κ-λ
Γ ⊢ {p}± -> τ: ∗


*misc* 
----------- κ-unknown
Γ ⊢ ★: Unknown

|| this might be too broad.
|| there could be unknown types, rows, labels, patterns,...


## Rewriting
{l₁ = a; {l₂ = b;}} ≙ {l₁ = a; l₂ = b;}
{ε} = {}       (syntax-records & type-records)
{ε}: e = {}: e

|| in a calculus, do you need the shorthand for {} etc?
||| Im Moment noch nicht

## Inference
*Basics*
x: σ ∈ Γ   Γ ⊢ σ ⊑ τ
--------------------- Var
Γ ⊢ x: τ

--------
Γ ⊢ ℓ: ⦅ℓ⦆

*Equivalences*
- TODO: choose one

Γ ⊢ e₁: τ₁  τ₂ ≤ τ₁ 
--------------------- Sub
Γ ⊢ e₁: τ₂


Γ ⊢ e₁: τ₁  τ₂ ≡ τ₁ 
-------------------- Eq
Γ ⊢ e₁: τ₂

|| suggestion: try with Eq and polymorphism (to avoid the extra complexity of Sub)
||| Okay

*Records*
Γ ⊢ e₁: ⦅l⦆ e₂: τ  
----------------- Rec-I
Γ ⊢ {e₁ = e₂}: {l: τ}

|| I don't think there is a rule for a: ⦅l⦆ nor a rule for putting `a` in an expression


Γ ⊢ e₁: {ρ₁}   Γ ⊢ e₂: {ρ₂}
--------------------- Rec-Concat
Γ ⊢ e₁ ‖ e₂: {ρ₁ | ρ₂}

|| b overrides a in a ∥ b?
||| Ja
|| metavariables should be used consistently. Best you declare:
|| what do a and b range over? why do you start with e₁ and e₂? what's the difference?
||| Ich finde a,b ein bisschen schöner als e₁,e₂, aber sehe den Punkt mit Consistency


Γ ⊢ˡ a: τ ↝ Γ'
--------------------- Rec-Acc
Γ ⊢ e₁.e₂: τ

|| see comments to ∈-solving below
|| I'm surprised that this operates on the structure of a rather than a's type
|| why is `a` repeated?
||| Habs statement geändert



*Functions*
Γ,Δ ⊢ e: τ   p ↦ Δ
--------------------------- λ-I-open
Γ ⊢ ({ ξ, … }: e): {p}⁺ -> τ

|| connection between p and Δ?
||| ξ war hier falsch, wir gehen für den Typen

Γ,Δ ⊢ e: τ   ξ ↦ Δ
--------------------------- λ-I-close
Γ ⊢ ({ ξ }: e): {p}⁻ -> τ

|| explain ⁺ (closed) vs ⁻ (open)
|| intuitively, I'd expect ⁺ to be open...
||| Typregel-name und ⁻ sind konsistent, die Expression habe ich jetzt angepasst


e ⧀ τ ≙ (Γ ⊢ e: τ' and τ' ⧀ τ)
τ ⧀ e ≙ (Γ ⊢ e: τ' and τ ⧀ τ')

|| no! this implies that Γ is invented (i.e., ∃ Γ, such that ...)
||| TODO: »invented« verstehe ich nicht
|| what's the definition of ⧀ on types?
||| Gibt es (noch) nicht, müsste man, wenn man sich für Subtyping entscheidet, noch von rows auf Types ausweiten


Γ ⊢ e₁: { p }⁻ -> τ₂    e₂ ⧀ ⌊p⌋   ⌈p⌉ ⧀ e₂
------------------------------------------- λ-E-1
Γ ⊢ e₁e₂: τ₂

|| what is ⌊p⌋ and ⌈p⌉?
|| IIRC, ⁻ is open, why does the rule fix e₂ at p?
||| Andersherum ist wie gesagt richti, daher die Konfusion


Γ ⊢ e₁: { p }⁺ -> τ₂    e₂ ⧀ ⌊p⌋
------------------------------------------- λ-E-2
Γ ⊢ e₁e₂: τ₂

*Auxiliary pattern approximation*
- Only retain non-optional fields
⌊p⌋ :: Pat -> ∗
ε              = {}
(l: τ | p)     = { l: τ | ⌊p⌋ }
(l: τ ? τ | p) = ⌊p⌋

- Retain all fields
⌈p⌉ :: Pat -> ∗
ε              = {}
(l: τ | p)     = { l: τ | ⌈p⌉ }
(l: τ ? τ | p) = { l: τ | ⌈p⌉ }

|| remind me of the meaning of τ ? τ 
||| Default-argument (siehe oben)


*Let-Poly*
Γ x: ∀ᾱ: overline(κ). τ₁ ⊢ e₂ : τ₂     Γ ⊢ e₁: τ₁    ᾱ ∉ ftv(Γ)
--------------------------------------------------------------- Let
Γ ⊢ let x = e₁ in e₂: τ₂

|| side condition should be α ∈ ftv(τ₁) ∖ ftv(Γ)
||| warum ist das unbedingt besser?


## Matching
> An auxiliary judgement that creates a new context Δ with all pattern variables

------ m-empty
ε ↦ •

Γ ⊢ x: τ   ξ ↦ Δ'
-------------------------- m-pat
(x | ξ) ↦ Δ, x: τ, Δ'

|| where does Δ come from? does sequence matter?
||| Reihenfolge ist eigentlich egal, die uniqueness sollte anderweitig sichergestellt werden

- Using the default expressions type here is a deliberate decision
Γ ⊢ e: τ  ξ ↦ Δ'
-------------------------- m-default
(x ? e | ξ)  ↦ Δ, x: τ, Δ'

|| Δ?  d is an expression?
||| Ja, der Default. Metavariable ist jetzt dur `e` eindeutig

## Instantation
> Instantiate type schemes ∀ᾱ: overline(κ). σ when taking them out of the context

------ Inst-Refl
τ ⊑ τ


Γ ⊢ τ: ∗   Γ ⊢ σ[τ/α] ⊑  σ'
--------------------------- Inst-∗
Γ ⊢ ∀α: ∗. σ ⊑ σ'


(α ∉ ftv(Γ) or  α ⊳ˡ σ)  Γ ⊢ σ[τ/α] ⊑ σ'
-------------------------------------- Inst-Lab
Γ ⊢  ∀α: Lab. σ ⊑ σ'


(α ∉ ftv(Γ) or  α ⊳ʳ σ)  Γ ⊢ σ[τ/α] ⊑ σ'
-------------------------------------- Inst-Row
Γ ⊢  ∀α: Row. σ ⊑ σ'

|| is there really a difference in how each of these rules instantiates a variable?
||| Die verwenden unterschiedliche _tail-checks_ (definiert in »Infix-Extensible Record Types for Tabular Data«)
|| one should be able to insert any suitable "thing" of the correct kind.
||| Verstehe ich nicht


## Tailcheck
> Make sure not to instantiate row and label variables when they could shadow existing fields
- TODO: we should use the subtype relation here

|| so you must insist that all row and label variables in a row are distinct?
|| Otherwise, duplicate variables cannot be instantiated. I think
|| not sure what the intention of this judgment is
||| Der Sinn ist eigentlich (siehe line 303), dass instantiierte labels oder rows nicht existierende
||| Felder shadowen. Wir verwenden ja scoped records mit rechts oder links präzedenz (steht noch nicht fest)

α ⊳ τ₁
--------- go-l
α ⊳ τ₁ τ₂


α ⊳ τ₂
--------- go-r
α ⊳ τ₁ τ₂


α ⊳ σ
------------ go-in
α ⊳ ∀β: κ. σ


------------ ⊳-Lab
α ⊳ˡ ⦅α⦆


ρ ≡ {ρ | α: τ}
--------------- ⊳-Lab-Rec
α ⊳ˡ {ρ}


ρ ≡ {ρ' | α}
------------- ⊳-Row
α ⊳ʳ {ρ}



## Subtyping

|| Do you really want that? Do you have an example where subtyping is truly needed?
||| Wahrscheinlich reicht row-equivalence vorerst, kann man ja sonst auch noch später ausweiten

- TODO: das müsste von hinten sein?
- TODO: ≤ nicht definiert

l₁ = l₂    τ₁ ≤ τ₂    ρ₁ ⧀ ρ₂
-------------------------------- - 
{l₁: τ₁ | ρ₁} ⧀ { l₂: τ₂ | ρ₂}

|| for this rule to work, you need an equivalence on rows: swap assumptions with different labels
|| or you are even more restrictive and ask that labels match in the sequence they are written
||| J

- Für label variablen:
[α = l]¡   τ₁ ≤ τ₂    ρ₁ ⧀ ρ₂
--------------------------------- -
{α: τ₁ | ρ₁} ⧀ { l: τ₂ | ρ₂}

|| if you have a swap rule, then this is almost certainly wrong.
|| the way to check the rule has two steps:
|| 1. define subtyping (semantics) for closed types, rows, etc
|| 2. extend this semantics to open types by quantification over all closing substitutions
|| Now every proposed rule can be checked formally against this semantics.

¿
--------------------
{ l₁: τ₁ | ρ₁} ⧀ α


¿
--------------------
α ⧀ { l₁: τ₁ | ρ₁}

|| these rules also depend on the chosen semantics, so that should come first!

## ∈-Solving
> Tries to solve element constraints for rows that can have (multiple) row and label variables

|| start with a declarative specification, rather than an algorithm!
|| what is the intention?

Discharged by Γ ⊨ˡ a: τ ⊣ Γ'

|| what's the connection to the stuff below?

A => B
where A is a tuple of (row, query-label (l)) and B can be one of:
- (Γ, ρ, b) : To recurse
- τ         : The type of of query-label
- ★         : An unkown type due to missing information

- ------------------------------------------------ -
(recurse) – (Γ, { l: τ | ρ}, q) => (Γ, ρ, q)          if  q ≠ l
(solved ) – (Γ, { l: τ | ρ}, q) => τ                  if  q = l

(var-lab) – (Γ, { α: τ | ε }, q) => (Γ · (α = q), ε, q)
(–––––––) – (Γ, { α: τ | ρ }, q) => ¿ Hier constraint, dass es in allen restlichen variablen drin ist?

(var-row) – (Γ, { α | ε },    q) => (Γ · (q ∈ α), ε, q)
(–––––––) – (Γ, { α | ρ },    q) => ¿ Hier constraint, dass es in allen restlichen variablen drin ist?

(default) –  (_, _, _)           => ★
- ------------------------------------------------ -
