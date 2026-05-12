#import "../functions.typ": *

== From interpreter to theory
When trying to retrofit a typesystem onto a language that is effectively defined by an interpreter, the natural question arises, how close one should model the interpreters behaviour. In many cases the interpreter needs to be followed closely, but in cases of "higher theoretical properties" like infinite-recursion it might be benefical to extrapolate or simplify the intepreter quirks, effectively deviating from the instantiated operational semantic.


= Archive

$overline(E)^{i ∈ 𝓘}$ denotes a repetition of a syntax construct indexed by $i ∈ 𝓘$. The index $i ∈ 𝓘$ is omitted if obvious.


== Records
Algebraic subtyping \@dolstra_phd is a technique to get well-behaved types and neat type inference. After \@simplesub and \@mlstruct we know how to pratically implement it. The first thing one needs to do is to form a boolean algbebra of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually landing at primitive constraints like $"Bool" < top$ that can be solved trivially.

Since batracking in nix' huge syntax tree that roots in a single file and relies heavily on laziness is insufficient, the properties of algebraic subtyping come as a perfect fit. The formalization of algebraic subtyping depends heavily on order-theory and some form of category theory and the proofs are far from simple @simplesub. Thankfully, @simplesub showed how to get from a algebraic domain to a syntactic one by creating an  equivalent using constraint accumulation on type variables and biunification, making algebraic subtyping more accessible. In the seminal Bachelor Thesis from the first author, he showed how to extend the SimpleSub to the more expressive type system features of nix. Even though the work pintpointed a direction, it oversimplified on the operational semantic and derived type rules, leaving lots of room for improvement.

Properties of a typesystem describe what guarantees and benefits a typesystem brings to the table. For example, a typesystem that is null-save will ensure at compile-time that when the program is executed, values of null will never be dereferenced and thus eliminates a whole class of problems (todo:check). The first decision is whether one wants to limit the features of the typesystem and making it incomplete silently,

Records come as a natural extension of tuples to named tuples to make fields accessible by names. Records have been studied in variety of papers @symm_concat @concat4free @fc_labels @extensible_recs @extensible_tabular, each with their own quirk to it.

The first model of records is a syntactic model where the syntax defines what a record is. This approach is conceptually simple but hard to extend because of its verbose nature and exploding rule-complexity. To overcome these shortcomings, \@wand studied _row polymorphism_. Row polymorphism extend record with a generic row r, effectively making them polymorphic in their "rest". By extending the row to lacks-predicates not only extension, but also restriction of record types can be achieved, giving a lot of flexibility in theory. While strong in theory, the theory gets complex and unwildy fast, making it hard to integrate into fully-fledged type systems. _Semantic subtyping_, developed over multiple years by Castagna et. al. @gentle_intro @poly_records @typing_records_etc to name a few, tries to remedie this by shortcoming by giving records a set-theoretic semantic model.

Stephen Dolan proposed a new family of type systems, named _algebraic type systems_. These systems tackle language construction from a new point of view. Instead of adding types first and then trying to find a semantic model for them, Dolan argues one should pay more attention to finding a semantic model for the types _first_.


== Old property talk
Another important consideration for a typesystem is _expressiveness_. It is obviously possible to type every variable at an _unknown type_ $star.op$ but that would not give meaningful insight for the user. On the other hand, making a typesystem to complex might lead to unwanted properties like undecidability or non-termination @undecidable. Again, the proper path strikes the balance between expressiveness and simplicity.

Most general purpose typesystem come equipped with some form of polymorphism, to abstract over generic program behaviour. Due to its usefulness, polymorphsim is one if not the most researched topic with a myrriad of different kinds:  _parametric polymorphism_, _first-class polymorphism_, _subtyping polymorphism_, _Ad-hoc polymorphsim_, _Presence polymorphism_, _Explicit Polymorphism_, _Implicit Polymorphsim_ just to name a few. It is not immediately obvious which types of polymorphism is the right for your type system but we can conclude from the language.

It is obvious that parametric polymorphism is needed because nix features let-bindings. Last but not least, _subtype polymorphism_ is a common technique that has proven useful especially in conjunction with _type-connectives_. Type connectives are borrowed from logic connect otherwise unrelated types using unions, intersection and negation. They are especially useful in conjunction with _flow-respective typing_, a technique used in flow to narrow types in conditionals.

The nix language furthermore allows to reflect over its types using the builtin (isBool, isAttr, etc.) functions so a reflexive type system is needed. Nix also allows to compute record labels and such labels need to be _first class_ in the language. Last but not least, nix is a lazy and recursive language with hard-to-track shadowing semantics. Due to recursiveness in records, let-bindings, and patterns, recursive types are a must in the language.

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


== Can we check whether a variable is shadowed?
- For with statements no
- For let-staments yes?
- Ways to shadow a variable:


Broekhoff et al. @verified put substitutions into the syntactic domain in a technique called _deferred substitutions_. Their prime example is `with g {}; a + b` for why we don't even know whether a term is closed or not. In this case, variables can be added to the scope dynamically, meaning for `with {x = 2;}; with g; x` we don't know whether x's value is 2 or overwritten with any other value from g. An interpreter will thus store the binding x := 2; on the variable x, until it actually has to reduce x. For the example program, this means x will stay variable until g is fully resolved giving it the chance to overwrite the value of x. It is also possible to extend their approach to dynamic variables `${e}`. Where the lookup-name is not known until e is fully resolved. By adding the possible bindings to such expressions as in ${e}_(overline(d))$ will first resolve the variable and then look it up in the deferred substitution. For the nix language, this same substitution mechanism is handy because it can be used to implement the weak binding of width-constructs.

Since nix is lexically scoped, it would also be possible to track the opened record with in a separate context, lookup variables normally (as they are stronger binding anyways) and only if a variable is undefined, we check the contexts.




#bib
