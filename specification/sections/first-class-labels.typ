#import "../functions.typ": *

Dynamic bindig: `${e} = x` \<- this one is more tricky, because it changes the context
Dynamic vars: `t = ${e}` \<- this one is easier because its just a lookup

== First class labels

Kinds: ∗ | ∗ → ∗ | Lab | Row
Types: ⦅ℓ ⦆|

- are default values important for castagna to work out?

#let export = [

  == First Class Labels
  First class labels allow computation on record labels. It is thus possible to access record fields with a computed label. In general this lookis like `r.t` where r is a record and t is an arbitrary expression. Since nix' string interpolation is allowed in paths and as record accesses, we need to add it to the language.

  In research there exist different approaches to the problem of first-class labels @verified @typing_records_etc @extensible_rec_funcs @extensible_data_adhoc

  Todo: this part is not really applicable to a discussion (there are only so many papers) so I think an explicit view of semantic subtyping approach vs. Daan Leijen is good... :thinking:

]

== Can we check whether a variable is shadowed?
- For with statements no
- For let-staments yes?
- Ways to shadow a variable:


Broekhoff et al. @verified put substitutions into the syntactic domain in a technique called _deferred substitutions_. Their prime example is `with g {}; a + b` for why we don't even know whether a term is closed or not. In this case, variables can be added to the scope dynamically, meaning for `with {x = 2;}; with g; x` we don't know whether x's value is 2 or overwritten with any other value from g. An interpreter will thus store the binding x := 2; on the variable x, until it actually has to reduce x. For the example program, this means x will stay variable until g is fully resolved giving it the chance to overwrite the value of x. It is also possible to extend their approach to dynamic variables `${e}`. Where the lookup-name is not known until e is fully resolved. By adding the possible bindings to such expressions as in ${e}_(overline(d))$ will first resolve the variable and then look it up in the deferred substitution. For the nix language, this same substitution mechanism is handy because it can be used to implement the weak binding of width-constructs.

Since nix is lexically scoped, it would also be possible to track the opened record with in a separate context, lookup variables normally (as they are stronger binding anyways) and only if a variable is undefined, we check the contexts.

Frow the presentation follow two questions:
1. Can the same mechanism be used to implement first-class-labels?

Basically we can add this rules to get computation inside the construct:
Actually, we don't even need it, because we have the context lule?

#derive("T-lab", ($e arrow.long e'$,), $t.\${e} arrow.long p.\${e'}$)

#bib
