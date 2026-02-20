#import "../functions.typ": *

#let export = [
  == From Static vs. Dynamic to Gradual to Occurrence
  Static type systems analyze programs at the meta‑level to establish properties without executing code. They enforce important safety guarantees such as null‑safety @pearce_flowtyping, panic‑freeness, and the absence of use‑after‑free errors @rust. Inferred annotations further aid reasoning about higher‑order calls, deconstruction of nested data, and interactions with unknown libraries. Nevertheless, static checking is a conservative, compile‑time abstraction of runtime behavior and thus an inevitably incomplete approximation @coldwar, which may reject valid programs or require auxiliary annotations to satisfy the type checker.

  For scripting and rapid prototyping, many languages adopt dynamic typing. At this end of the spectrum, no static invariants are imposed a priori; expressiveness is maximized, but runtime failures become possible. The lack of annotation overhead and the availability of reflection make this style attractive during exploration. As systems grow and module, function, class, and service boundaries proliferate, however, the guarantees afforded by static typing become increasingly valuable. The widespread adoption of Flow and TypeScript for JavaScript @flow @typescript show a general trend toward integrating stronger typing into inherently dynamic ecosystems.

  Gradual type systems @gradual_siek @gradual_tobin aim to reconcile these positions by allowing statically typed and dynamically typed code to coexist. A distinguished unknown type $star.op$ mediates the boundary, together with casts that refine unknown values to more precise types. A rich literature explores the design space @gradual_siek @gradual_tobin @cantblamethis @agt @gradual_extensible_rows @consistent-subtyping @blame_for_all, but many proposals rely on explicit annotations and cast insertion—features we prefer to avoid in the present work.

  Flow typing @flow, and in particular the more disciplined occurrence typing @revisiting_occurrence @on_occurrence, offers a complementary approach that refines types along control‑flow. Consider `if isBool(x) then !x else x + 1`: the predicate tests at runtime whether `x` is a boolean. In the then‑branch, `x` is treated as a bool; in the else‑branch, negative information that `x` is not a bool enables alternative typing. Similar refinements arise across pattern‑matching constructs.

  In a match expression such as `match x with bool(x) -> .. | rec(x) -> .. | _ -> x`, branches are considered in order and behave like conditionals that impose type constraints. The default case can be typed under the assumption that `x` is neither a bool nor a record; moreover, an exhaustiveness check can be phrased by comparing the type of the default branch with ⊥, motivating the inclusion of type‑level negation.


  == Impurities <impurities>
  Although Nix is designed as a pure, lazily evaluated functional language, its standard library deliberately exposes aspects of the host environment and file system to support reproducible builds and modular composition. This integration complicates static analysis: certain primitives are observationally impure, and in the absence of evaluation they can undermine purely static reasoning.

  A prominent example is dynamic attribute selection via the target system, e.g., `g.${builtins.currentSystem}`. The value of `builtins.currentSystem` is supplied by the evaluator and is used pervasively in real‑world code (observed in 207 public occurrences). #footnote(link("https://sourcegraph.com/search?q=context:global+%24%7Bbuiltins.currentSystem%7D&patternType=keyword&sm=0")) While a type system could, in principle, model this constant in a fixed evaluation context, other primitives expose inherently unpredictable information. In particular, `builtins.currentTime` reflects wall‑clock time and is not amenable to static prediction. Related impure queries include `currentSystem`, `currentTime`, `fetch\*`, `findFile`, `langVersion`, and `nixVersion`; from a typing perspective, these may produce values of arbitrary shape.

  Because no static analysis can be both sound and complete in this setting, a degree of graduality is required. We therefore admit an unknown type $star.op$ to account for the results of impure or otherwise opaque computations, allowing programs to remain typable without committing to unsound assumptions. This stance mirrors practice in mainstream ecosystems—most notably TypeScript @typescript—which is known to be incomplete with respect to distributivity of intersections and unions, yet remains highly effective in practice.

]

== Graduality
- We need a type system that can approximate (check builtins, untypable)
- If I have an any type it should be applicable to any function, because we can not make assumptions about it.
- It should thus be the subtype of every possible type.
- Should it be a supertype aswell?



