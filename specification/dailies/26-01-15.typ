
- We might want to talk about null-safety


== Algebraic Subtying
Algebraic subtyping \@dolstra_phd is a technique to get well-behaved types and neat type inference. After \@simplesub and \@mlstruct we know how to pratically implement it. The first thing one needs to do is to form a boolean algbebra of types that is well behaved. If given, constraints of the form τ₁ <= τ₂ can be "grained down" into sub-constraints, eventually landing at primitive constraints like $"Bool" < top$ that can be solved trivially.

== Things done in this paper
Since nix was built as a domain specific language with usability as its greatest design goal, the system boasts a lot of features that make retrofitting a type inference hard or even impossible. In traditional language theory, the flow is mostly reversed where one starts from a simple calculus like ML, SystemF, λ and carefully extends it with features to form a wieldy and interesting semantics. When trying to retrofit a type-system onto a language like @nix-language-2-28 @flow @typescript @elixir_design_principles @typed_racket one has to decide which features one can and wants to support.


== To: Comparison
@verified has made three interesting decisions in their formalization:

1. rec/nonrec attribution
2. Deep/shallow evaluation
3. Operators as relations
4. Matching

We diverge from this representation quite a bit. First and foremost, NixLang @verified follows the first semantic of Dolan @memory_to_software @dolan_phd and annotates every record field as recursive or not. The reason being a subtlety of the inherit statement. Both systems handle inherit by adding rewriting rules, that turn expressions of the form `inherit (a) x;` into something like `x = a.x;` in records or let-bindings. When used in conjunction with recursive records, this leads to unwanted recursion. The statement `inherit x;` will be desugared into `x = x;` and in a record `rec {x = x;}` this will lead to infinite recursion. That is why





== First class values
Commonly used representations include De Bruijn indices [15], nominal syntax [21], higher-orderabstract syntax [48] and locally nameless [13]. The strength of these representations is that they


== To: Impurities
- import function
- eval function?


== Limitations


== Weirdness
- appendContext is not in builtin-list
- builtins.elemAt
- \@ args does not include any default values specified with ? in the function's set pattern.
- Is nix really statically scoped?
- __toString
- An attribute set also interpolates to the value of its outPath attribute.
- Attribute sets and lists are compared recursively, and therefore are fully evaluated.
