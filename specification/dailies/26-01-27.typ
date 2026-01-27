`let x = 2; f = { ${"x"} = 2;}.x; g = (let x = 3; in f); in g`
upward-funarg: `(z: let x = 2; f = y: (x + y + z); in f) 2 2`
downward-funarg: `let f = (x: x 2); y = (y: y + 1); in f y`


== Binding type of nix

The nix programming language is _lexically scoped_, meaning functions capture the surrounding environment and use these functions for computations. Actually, they are executed in their lexical scope instead of the (dynamic) environment.

The prime example of verified @verified is `with g {}; a + b` where it is unclear whether a and b are even defined. This is becaus they classify g as an arbitrary function that can return an arbitrary set which is consequently opened in the following expression. The statement, that this forces dynamic variable bindings is only justified, as long as we can actually not find out more about the value produced by `g {}`. In a language that supports full record type inference this is not the case, because all bindings can be determined _statically_.

But as soon as the language is enabled to be _gradual_ and


Their proposed solution are _deferred substitutions_ that



[$ Ξ_{(x = y)}(t) $] -> [$$] |
[$ Ξ_(t) $] -> [$$] |
[$ Ξ(t) $] -> [$$] |

Dynamic bindig: `${e} = x` <- this one is more tricky, because it changes the context
Dynamic vars: `t = ${e}` <- this one is easier because its just a lookup

`with {${e} = d}; e`


`Since Nix does not feature a construct to refer to dynamically computed variables`
Yeah, but it is possible to refer to _dynamically computed labels_.


== To: Records
Actually, a row variable for records can be advantageous in occurence typing systems because


== To: Occurence Typing

