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

Dynamic bindig: `${e} = x` <- this one is more tricky, because it changes the context
Dynamic vars: `t = ${e}` <- this one is easier because its just a lookup

`with {${e} = d}; e`


`Since Nix does not feature a construct to refer to dynamically computed variables`
Yeah, but it is possible to refer to _dynamically computed labels_.
