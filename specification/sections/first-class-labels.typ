#import "../functions.typ": *

Dynamic bindig: `${e} = x` \<- this one is more tricky, because it changes the context
Dynamic vars: `t = ${e}` \<- this one is easier because its just a lookup

#let export = [

  == First Class Labels
  First class labels allow computation on record labels. It is thus possible to access record fields with a computed label. In general this lookis like `r.t` where r is a record and t is an arbitrary expression. Since nix' string interpolatio is allowed in paths and as record accesses, we need to add it to the language.

  In research there exist different approaches to the problem of first-class labels @verified @typing_records_etc.

  Broekhoff et al. @verified put substitions into the syntactic domain in a technique called _deferred substitutions_. Their prime example is `with g {}; a + b` for why we don't even know whether a term is closed or not. In this case, variables can be added to the scope dynamically, meaning for `with {x = 2;}; with g; x` we don't know whether x's value is 2 or overwritten with any other value from g. An interpreter will thus store the binding x := 2; on the variable x, until it actually has to reduce x. For the example program, this means x will stay variable until g is fully resolved giving it the chance to overwrite the value of x. It is also possible to extend their approach to dynamic variables `${e}`. Where the lookup-name is not known until e is fully resolved. By adding the possible bindings to such expressions as in ${e}_(overline(d))$ will first resolve the variable and then look it up in the deferred substitution. For the nix language, this same substitution mechanism is handy because it can be used to implement the weak binding of width-constructs.

  Since nix is lexically scoped, it is would also be possible to track the opened record withs in a seperate context, lookup variables normally (as they are stronger bounding anyways) and only if a variable is undefined, we check the contexts.

  Frow the presentation follow two questions:
  1. Can the same mechanism be used to implement first-class-labels?

  Basically we can add this rules to get computation inside the construct:
  Actually, we don't even need it, because we have the context lule?

  #derive("T-lab", ($e arrow.long e'$,), $t.\${e} arrow.long p.\${e'}$)


  - are default values important for castagna to work out?


  == Can we check whether a variable is shadowed?
  - For with statements no
  - For let-staments yes?
  - Ways to shadow a variable:

  The nix programming language is _lexically scoped_, meaning functions capture the surrounding environment and use these functions for computations. Actually, they are executed in their lexical scope instead of the (dynamic) environment. The only distinction is the with construct: `with g {}; a + b` where it is unclear whether a and b are even bound @verified.

  The prime example of verified @verified is `with g {}; a + b`. This is becaus they classify `g` as an arbitrary function that can return an arbitrary set which is consequently opened in the following expression. The statement, that this forces dynamic variable bindings is only justified, as long as we can actually not find out more about the value produced by `g {}`. In a language that supports full record type inference this is not the case, because all bindings can be determined _statically_.

  But as soon as the language allowes _gradual_ code, we might not know a lot about the record values anymore.
]

#bib
