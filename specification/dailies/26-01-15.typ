
- [x] We might want to talk about null-safety


== To: Comparison
@verified has made three interesting decisions in their formalization:

1. Rec/nonrec attribution
2. Deep/shallow evaluation
3. Operators as relations
4. Matching

We diverge from this representation quite a bit. First and foremost, NixLang @verified follows the first semantic of Dolan @memory_to_software @dolan_phd and annotates every record field as recursive or not. The reason being a subtlety of the inherit statement. Both systems handle inherit by adding rewriting rules, that turn expressions of the form `inherit (a) x;` into something like `x = a.x;` in records or let-bindings. When used in conjunction with recursive records, this leads to unwanted recursion. The statement `inherit x;` will be desugared into `x = x;` and in a record `rec {x = x;}` this will lead to infinite recursion. That is why there needs to be distinction between the two and annotating every field is one way to solve it – albeit a very noisy one. We choose to handle inherit with a reduction rule that makes sure in the side-codition, that make sure x exists in the context.

@verified also makes a distinction between deep and shallow evaluation. In a lazy equi-recursive language, recursive definitions have to be unrolled one at a time, usually only on the surface level. It might be of interest though to



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
