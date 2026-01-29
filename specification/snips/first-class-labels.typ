#import "../functions.typ": *
== First Class Labels
First class labels allow computation on record labels. It is thus possible to access record fields with a computed label. In general this lookis like `r.t` where r is a record and t is an arbitrary expression. Since nix' string interpolatio is allowed in paths and as record accesses, we need to add it to the language.

In research there exist different approaches to the problem of first-class labels.

It might be the only reasonable approach to apply something like @verified and put substitions into the syntactic domain. Another possible approach is to actually evaluate stuff, but it is hard to see whether we find any end to it or not. For example, strings could be generated in loop `n: x: if n>0 then x + "x"`. Maybe it is actually a good thing to use semantic subtyping, because we anyways want to use set-theoretic notation for _with_.

The other idea is from @poly_records. This one implements first-class labels for maps. Is it because they are constant in the default value? It might actually be that. The difference between the two approaches (see [simple essence of boolean algebraic subtyping] aswell) is, that one uses a row variables `{ a = b; | ξ}` while the other uses negation types and boolean reduction `{a: τ₁} ∧ {a: ¬T} ∧ {a: τ₂} -> {a : t₂}`. This allows them to address records. Otherwise, one could just build the conjunction and and give priority to later elements. It is not clear though, this affects the type inference algorithm.


== Can we check whether a variable is shadowed?
- For with statements no
- For let-staments yes?
- Ways to shadow a variable:


The nix programming language is _lexically scoped_, meaning functions capture the surrounding environment and use these functions for computations. Actually, they are executed in their lexical scope instead of the (dynamic) environment. The only distinction is the with construct: `with g {}; a + b` where it is unclear whether a and b are even defined @verified.

The prime example of verified @verified is . This is becaus they classify g as an arbitrary function that can return an arbitrary set which is consequently opened in the following expression. The statement, that this forces dynamic variable bindings is only justified, as long as we can actually not find out more about the value produced by `g {}`. In a language that supports full record type inference this is not the case, because all bindings can be determined _statically_.

But as soon as the language is enabled to be _gradual_ and
*closed*: A term is closed iff all variables are bound


Their proposed solution are _deferred substitutions_ that

- Problematic example `{inherit x;} -> {x = x;}`


[$ Ξ_{(x = y)}(t) $] -> [$$] |
[$ Ξ_(t) $] -> [$$] |
[$ Ξ(t) $] -> [$$] |

Dynamic bindig: `${e} = x` \<- this one is more tricky, because it changes the context
Dynamic vars: `t = ${e}` \<- this one is easier because its just a lookup

`with {${e} = d}; e`


`Since Nix does not feature a construct to refer to dynamically computed variables`
Yeah, but it is possible to refer to _dynamically computed labels_.

#bib
