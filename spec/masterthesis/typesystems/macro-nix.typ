== MacroNix
> L2 plus the cheap NixLang constructs: booleans, `if`, `assert`, `e ? l`, homogeneous lists, builtins with δ-rules

l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒  p ∈ 𝓟 ⊆ 𝓒

e := c | x | (x: e) | e₁e₂ | (e₁ ‖ e₂) | e.l | { ξ } | let x = e₁ in e₂
| 'l | e₁.(e₂) | { \${e₁} = e₂ }
| p e₁ … eₙ | true | false | if e then e₁ else e₂ | assert e₁; e₂
| [] | e₁ :: e₂ | e ? l
ξ := ε | l = e | (ξ₁ | ξ₂)

τ := α | 𝓫 | ★ | τ -> τ | { ρ } | ⌊k⌋ | [τ]
ρ := ε | α | l: τ | \${k}: τ | (ρ₁ | ρ₂)


== Desugaring
- `p e₁ … eₙ` is SATURATED: a builtin applied to all its arguments. A partial
  application is an eta-expansion in the elaboration, so a value never carries
  an arity
- Lists are HOMOGENEOUS: `[τ]` has one element type

`[e₁ … eₙ]`     ≔ `e₁ :: … :: eₙ :: []`
`e.l or d`      ≔ `(e ‖ {l = d}).l`   (derived, lazy in d)
`a && b`        ≔ `if a then b else false`
`a || b`        ≔ `if a then true else b`
`!a`            ≔ `not a`
`a != b`        ≔ `!(a == b)`
`a > b`         ≔ `b < a`
`+` on strings, `"${…}"`  ≔ `strcat`


== Signature
Σ := (con, bool, prim, δ)

con : 𝓒 → 𝓑              the type of a constant (MiniNix's 𝓫_c)
bool ∈ 𝓑 ∪ {none}        the type of `true`/`false`
prim : 𝓟 → τ             a builtin's type; every variable of it is generic (∀ftv. τ)
δ : 𝓟 × v̄ → e ∪ {fail}   a builtin's reduction on argument values

- MiniNix is Σ₀ = (con, none, ∅, ∅) (`Sig.plain`); Σ₀ ok vacuously

=== Σ ok (`SigOK`)

(∀ c. con(c) ≠ bool)   (∀ p v̄ θ. ⊢ v̄: τ̄  ∧  prim(p)θ = τ̄ -> τ  ∧  δ(p, v̄) = e  ⟹  ⊢ e: τ)
------------------------------------------------------------------------------------------ Σ-ok
Σ ok
// No constant inhabits bool, and δ preserves types.


== Sorts
- One new type former; bool is a base type, so S-base covers it

Γ ⊢ τ: Type
-------------- S-list
Γ ⊢ [τ]: Type


== Declarative

prim(p) = τₚ   τₚθ = τ₁ -> … -> τₙ -> τ   (∀ i. Γ ⊢ eᵢ: τᵢ)
-------------------------------------------------------------- T-prim
Γ ⊢ p e₁ … eₙ: τ


------------------ T-true
Γ ⊢ true: bool


------------------ T-false
Γ ⊢ false: bool


Γ ⊢ e: bool   Γ ⊢ e₁: τ   Γ ⊢ e₂: τ
------------------------------------- T-if
Γ ⊢ if e then e₁ else e₂: τ


Γ ⊢ e₁: bool   Γ ⊢ e₂: τ
-------------------------- T-assert
Γ ⊢ assert e₁; e₂: τ


------------ T-nil
Γ ⊢ []: [τ]


Γ ⊢ e₁: τ   Γ ⊢ e₂: [τ]
------------------------- T-cons
Γ ⊢ e₁ :: e₂: [τ]


Γ ⊢ e: {ρ}
----------------- T-has
Γ ⊢ e ? l: bool
// Refines nothing: occurrence typing is Negative Information S3.


=== Equivalence and precision
- ★ stays rigid, so a value at bool is a boolean literal and a value at [τ] a list

τ₁ ≈ τ₂
------------- ≈-list
[τ₁] ≈ [τ₂]


τ₁ ⊑ τ₂
------------- ⊑-list
[τ₁] ⊑ [τ₂]


== Runtime
v := c | (x: e) | { b } | 'l | true | false | [] | e₁ :: e₂
E := … | p v̄ E ē | if E then e₁ else e₂ | assert E; e | E ? l

- `::` is lazy, like a record field: `e₁ :: e₂` is a value for any e₁, e₂


------------------------------- R-if-t
if true then e₁ else e₂ ⟶ e₁


------------------------------- R-if-f
if false then e₁ else e₂ ⟶ e₂


--------------------- R-assert
assert true; e ⟶ e


------------------- R-assert-↯
assert false; e ↯


l ∈ b
------------------- R-has-t
{ b } ? l ⟶ true


l ∉ b
------------------- R-has-f
{ b } ? l ⟶ false


δ(p, v̄) = e
------------- R-δ
p v̄ ⟶ e


δ(p, v̄) = fail
--------------- R-δ-↯
p v̄ ↯


== Unification
- [τ] against any other head clashes (U-clash); α ≐ [… α …] is an occurs
  violation (U-occurs)
- Every leg of the trichotomy carries the case: success is an mgu, clash and
  occurs have no unifier, termination by size([τ]) = 1 + size(τ)

τ₁ ≐ τ₂ ⇝ θ
------------------ U-list
[τ₁] ≐ [τ₂] ⇝ θ


== Inference (`Infer`)

θ renames ftv(τₚ) fresh at their sorts   Γ; S ⊢ e₁ … eₙ ⇒ τ₁ … τₙ; S₁   fresh β: Type
S₁ ⊢ τₚθ ≐ (τ₁ -> … -> τₙ -> β) ⇝! S₂
-------------------------------------------------------------------------------------- A-prim
Γ; S ⊢ p e₁ … eₙ ⇒ β; S₂
// prim(p) = τₚ. Arguments are inferred left to right, threading the state.


b ∈ {true, false}
------------------- A-bool
Γ; S ⊢ b ⇒ bool; S


Γ; S ⊢ e ⇒ τ; S₁   S₁ ⊢ τ ≐ bool ⇝! S₂   Γ; S₂ ⊢ e₁ ⇒ τ₁; S₃
Γ; S₃ ⊢ e₂ ⇒ τ₂; S₄   S₄ ⊢ τ₁ ≐ τ₂ ⇝! S₅
------------------------------------------------------------- A-if
Γ; S ⊢ if e then e₁ else e₂ ⇒ τ₁; S₅


Γ; S ⊢ e₁ ⇒ τ₁; S₁   S₁ ⊢ τ₁ ≐ bool ⇝! S₂   Γ; S₂ ⊢ e₂ ⇒ τ₂; S₃
-------------------------------------------------------------- A-assert
Γ; S ⊢ assert e₁; e₂ ⇒ τ₂; S₃


fresh α: Type
-------------------- A-nil
Γ; S ⊢ [] ⇒ [α]; S


Γ; S ⊢ e₁ ⇒ τ₁; S₁   Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂   S₂ ⊢ τ₂ ≐ [τ₁] ⇝! S₃
---------------------------------------------------------------- A-cons
Γ; S ⊢ e₁ :: e₂ ⇒ [τ₁]; S₃


Γ; S ⊢ e ⇒ τ; S₁   fresh r: Row   S₁ ⊢ τ ≐ {r} ⇝! S₂
------------------------------------------------------ A-has
Γ; S ⊢ e ? l ⇒ bool; S₂


== The concrete signature (Builtins.lean)
- 𝓑 = {int, string, null, bool}; constants: integers, strings, null

`+ - * /`    : int -> int -> int             (/ fails on 0)
`< <=`       : int -> int -> bool
`==`         : α -> α -> bool                (atomic values only: constants, booleans, labels, []; fails otherwise)
`not`        : bool -> bool
`++`         : [α] -> [α] -> [α]
`head`       : [α] -> α                      (fails on [])
`tail`       : [α] -> [α]                    (fails on [])
`elemAt`     : [α] -> int -> α               (fails out of range)
`length`     : [α] -> int
`map`        : (α -> β) -> [α] -> [β]
`filter`     : (α -> bool) -> [α] -> [α]
`foldl'`     : (β -> α -> β) -> β -> [α] -> β
`toString`   : int -> string
`isAttrs`    : α -> bool
`strcat`     : string -> string -> string


== Headlines
- NixSuite.lean: 28 guards; the tuple encoding is gone except for `? \${dynamic}.path`
- Agree with Nix: attrs3, attrs5 e1-e6, dynamic-attrs-2, dynamic-attrs-bare, scope-1/2/3/7,
  let, new-let, patterns, empty-args, if, concat, logic, map, filter,
  foldlStrict, elemAt
- Disagree: attrs (whole), inherit-from, attrs5 e7 (unchanged, `or` on a non-record)

=== Idioms of the introduction (thesis §10), on real Nix
- overlay   `self: super: { hello = super.hello // { meta = { broken = false; }; }; }`
  :: α -> {hello: {ρ₁} | ρ₂} -> {hello: {ρ₁ | meta: {broken: bool}}}
- override   `mk: extra: args: mk (args // { buildInputs = args.buildInputs ++ [ extra ]; })`
  :: ({buildInputs: [β] | ρ} -> γ) -> β -> {buildInputs: [β] | ρ} -> γ
- conditional extension   `cond: { pname = "p"; } // (if cond then { NIX_LDFLAGS = "l"; } else { })`
  :: clash (declaratively rejected as well, see Costs)


== Costs
- *Joins at ★*: `if` and list literals unify their parts. `if as ? a then as.a else as.z`
  (★ against int) and `[1 {}]` clash, though each is declaratively typable via
  T-★-intro (at ★, at [★]). Same root as Unrestricted T-★-intro (thesis §7): ★ is the
  join of any two types, ≐ only finds common instances
- *Optional fields*: `if c then {a = 1;} else {}` has no common record type, only ★,
  and ★ cannot be concatenated. So the conditional-extension idiom
  `{pname = "p";} // (if c then {…} else {})` is rejected DECLARATIVELY too: a row cannot
  say "a may be there". Only a dynamic row ★ᵣ (Gradual) recovers it, as {★ᵣ | pname: string}
- Q4 options: (a) keep the clash [current]; (b) join at ★ in A-if/lists: completes the
  first item, not the idiom; (c) ★ᵣ in Gradual: the only one that recovers the idiom;
  (d) occurrence typing on `e ? l` (Negative Information S3): the `as ? a` case only
- *Dynamic `?`*: `e ? \${k}` is not in the syntax


== Open
- Q1 decided: merged into main's lean/ (692e664); thesis gets an appendix and a one-line note
- Q4 `if` with differing record branches: accept the clash (current), or hand to Gradual
