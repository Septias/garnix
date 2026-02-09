#import "../functions.typ": *


== Inference
Garnix models all literal syntax categories with the respective atom types bool, string, path and num. Notice, that we do not distinguish between float and int as they are coerced during interpretation and thus used interspersely in practice. We also add the usual types for fuctions, records and arrays and note that record  types only define a _single_ label to type mapping instead of multiple. This is due to the use of subtyping constraints and their accumuation on type variables during type inferene. This mechanism is further discussed in \@section_todo. Also, we introduce two types for arrays, one for homogenous arrays of the same type and one accumulative for the case that an array has many distinct elements.
To form a boolean algebra of types we add the expected type connectives $union.sq, inter.sq, ~$ as well as a top and bottom type which represent the least type which is subsumed by every other type and the greatest type which subsumes every other type respectively.
Lastely, we add a single type for patterns. Even thought a pattern is similar in structere to a record, the pattern type is an accumulated type with multiple fields. This distinction is made due to the syntactical difference of the two. Patterns are introduced and eliminated atomically unlike a record where every fild acces `record.field` results in new independent constraints. The superscript b can be true or false, ascribing whether the pattern is _open_ or _closed_.

== From interpreter to theory
When trying to retrofit a typesystem onto a language that is effectively defined by an interpreter, the natural question arises, how close one should model the interpreters behaviour. In many cases the interpreter needs to be followed closely, but in cases of "higher theoretical properties" like infinite-recursion it might be benefical to extrapolate or simplify the intepreter quirks, effectiely deviating from the instantiated operational semantic.


== Things done in this paper
Since nix was built as a domain specific language with usability as its greatest design goal, the system boasts a lot of features that make retrofitting a type inference hard or even impossible. In traditional language theory, the flow is mostly reversed where one starts from a simple calculus like ML, SystemF, λ and carefully extends it with features to form a wieldy and interesting semantics. When trying to retrofit a type-system onto a language like @nix-language-2-28 @flow @typescript @elixir_design_principles @typed_racket one has to decide which features one can and wants to support.



= Archive
== Algebraic Subtying
Algebraic subtyping \@dolstra_phd is a technique to get well-behaved types and neat type inference. After \@simplesub and \@mlstruct we know how to pratically implement it. The first thing one needs to do is to form a boolean algbebra of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually landing at primitive constraints like $"Bool" < top$ that can be solved trivially.

Since batracking in nix' huge syntax tree that roots in a single file and relies heavily on laziness is insufficient, the properties of algebraic subtyping come as a perfect fit. The formalization of algebraic subtyping depends heavily on order-theory and some form of category theory and the proofs are far from simple @simplesub. Thankfully, @simplesub showed how to get from a algebraic domain to a syntactic one by creating an  equivalent using constraint accumulation on type variables and biunification, making algebraic subtyping more accessible. In the seminal Bachelor Thesis from the first author, he showed how to extend the SimpleSub to the more expressive type system features of nix. Even though the work pintpointed a direction, it oversimplified on the operational semantic and derived type rules, leaving lots of room for improvement.


Properties of a typesystem describe what guarantees and benefits a typesystem brings to the table. For example, a typesystem that is null-save will ensure at compile-time that when the program is executed, values of null will never be dereferenced and thus eliminates a whole class of problems (todo:check). The first decision is whether one wants to limit the features of the typesystem and making it incomplete silently,

== Bad function reduction

$ #rule_name("R-Fun")&& (l: t_2)t_1 & arrow.long t_2[l := t_1] \
#rule_name("R-Fun-Pat")&& ({oi(l_i)}: t){oi(l_i \= t_i)} & arrow.long
t [oi(l_i := t_i)] \
#rule_name("R-Fun-Pat-Open")&& ({oi(l_i)\, ...}: t) {oj(l_i = t_i)} & arrow.long
t [oi(l_i := t_i)] #h(0.5cm) &&&∀i. ∃ j. i eq j \
#rule_name("R-Fun-Pat-Default")&&({oi(e_i)}: t){oj(l_j = t_j)} & arrow.long
t [oj(l_j = t_j)][oi(l_i := d_i)] \
#rule_name("R-Fun-Pat-Default-Open")&&({oi(e_i), …}: t){oj(l_j = t_j), …} & arrow.long
t [oj(l_j = t_j)][oi(l_i := d_i)] &&&∀i. ∃ j. i eq j\ $,

Since nix supports patterns with default values and the _open_ modifiers, the function reduction rules become quite verbose. The simplest case is R-Fun which takes an argument t₁ and replaces the occurrences of $l$ with said argument in the function body t₂. The next function rules R-Fun-Pat-∗ reduces functions taking patterns, the R-Fun-Pat being the simplest of such. We draw i,j from the index Set ℐ and range them over labels such that if i = j then l_i = l_j.

Since the same index $i$ is used for both the argument and pattern in R-Fun-Pat, they must agree on the same labels which resembles closed-pattern function calls. In the contrary case where the pattern is open, the argument-record can range over arbitrary labels (possibly more than in the pattern). In this case, the side-condition enforces that at least the pattern fields are present (R-Fun-Pat-Open).

The R-fun-Pat-Default-∗ rules range over pattern elements $e$ which can be either single labels $l$ or labels with a default values like $l : d$. The former case can be converted to the latter with ε-extension transforming $l$ to $l ? ε$ which is equivalent to $l$ due to the shorthands (TODO: can you do this?). The variables of the body are then substituted twice. First with the argument values and then with the default values to "fill the gaps". The open case needs a side-condition analogous to the former open case.

Since ${oi(e_i)}$ strictly subsumes ${oi(l_i)}$ due to its inner structure, rule 2 and 3 are only stated as a mental stepping stone for the reader but not mentioned further.


== Inherit subtetly
We diverge from this representation quite a bit. First and foremost, NixLang @verified follows the first semantic of Dolan @memory_to_software @dolstra_phd and annotates every record field as recursive or not. The reason being a subtlety of the inherit statement. Both systems handle inherit by adding rewriting rules, that turn expressions of the form `inherit (a) x;` into something like `x = a.x;` in records or let-bindings. When used in conjunction with recursive records, this leads to unwanted recursion. The statement `inherit x;` will be desugared into `x = x;`



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
