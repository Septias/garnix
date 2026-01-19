== Inference
Garnix models all literal syntax categories with the respective atom types bool, string, path and num. Notice, that we do not distinguish between float and int as they are coerced during interpretation and thus used interspersely in practice. We also add the usual types for fuctions, records and arrays and note that record  types only define a _single_ label to type mapping instead of multiple. This is due to the use of subtyping constraints and their accumuation on type variables during type inferene. This mechanism is further discussed in \@section_todo. Also, we introduce two types for arrays, one for homogenous arrays of the same type and one accumulative for the case that an array has many distinct elements.
To form a boolean algebra of types we add the expected type connectives $union.sq, inter.sq, ~$ as well as a top and bottom type which represent the least type which is subsumed by every other type and the greatest type which subsumes every other type respectively.
Lastely, we add a single type for patterns. Even thought a pattern is similar in structere to a record, the pattern type is an accumulated type with multiple fields. This distinction is made due to the syntactical difference of the two. Patterns are introduced and eliminated atomically unlike a record where every fild acces `record.field` results in new independent constraints. The superscript b can be true or false, ascribing whether the pattern is _open_ or _closed_.

== From interpreter to theory
When trying to retrofit a typesystem onto a language that is effectively defined by an interpreter, the natural question arises, how close one should model the interpreters behaviour. In many cases the interpreter needs to be followed closely, but in cases of "higher theoretical properties" like infinite-recursion it might be benefical to extrapolate or simplify the intepreter quirks, effectiely deviating from the instantiated operational semantic.


== Algebraic Subtying
Algebraic subtyping \@dolstra_phd is a technique to get well-behaved types and neat type inference. After \@simplesub and \@mlstruct we know how to pratically implement it. The first thing one needs to do is to form a boolean algbebra of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually landing at primitive constraints like $"Bool" < top$ that can be solved trivially.

== Things done in this paper
Since nix was built as a domain specific language with usability as its greatest design goal, the system boasts a lot of features that make retrofitting a type inference hard or even impossible. In traditional language theory, the flow is mostly reversed where one starts from a simple calculus like ML, SystemF, λ and carefully extends it with features to form a wieldy and interesting semantics. When trying to retrofit a type-system onto a language like @nix-language-2-28 @flow @typescript @elixir_design_principles @typed_racket one has to decide which features one can and wants to support.

#page[
  #bibliography(
    (
      "../bib/misc.bib",
      "../bib/parreaux.bib",
      "../bib/nix.bib",
      "../bib/castagna.bib",
    ),
    style: "association-for-computing-machinery",
  )
]
