#import "../functions.typ": *
#import "../typesystem.typ": *


== TODO
- Scoped Record with lacks or duplicates
- Mixed use of subsumption
- No clear usage of polymorphic variables
- Missing: With
- Missing: Occurrence Typing
- Missing: Gradual Typing


== Typing approach
#figure(
  caption: "Types of Nix.",
  types,
  placement: none,
)<types>


The types build on the calculi of Lionel Parreaux @simplesub @mlstruct @invalml and are summarized in @types. Literal terms inhabit their canonical atomic types (bool, string, path, float, int). We adopt the standard constructors for functions, records, and arrays, with the following refinement for records: a record type denotes a single mapping from a label to a type rather than an explicit enumeration of all fields. This aligns with conjunction-based accumulation of constraints on type variables during inference; see \@records for details. For arrays, we provide two descriptions: a homogeneous array type and an accumulated variant that accommodates heterogeneously typed elements. To obtain a Boolean algebra of types, we admit the connectives $¬, ∨, ∧$ together with the top and bottom elements $⊤$ and $⊥$, which are, respectively, the greatest and least elements of the subtyping lattice.

Finally, we introduce a dedicated type of patterns. Although patterns mirror records syntactically, their type is cumulative across fields because introduction and elimination occur atomically at the level of the whole pattern; in contrast, each field selection `record.field` in a record yields independent constraints. We annotate openness with a superscript $b$, indicating whether a pattern is open or closed.

#basic_typing_rules <typingrules>
@typingrules shows the basic typing rules for Nix. They are mostly standart but two var rules are needed to account for monomorphic and polymorphic types. Four list typing rules  account for \_ and \_ array types and their respective concatenation. The operator typing rules are relegated to \@operator-typingrules.

#figure(caption: "Record typing rules", record_typing_rules) <recordrules>

The record typing rules are given in @recordrules. T-Rcd and T-Proj are entirely standart and the T-Or-Neg and T-or-Pos handle the or-operator if the lookup succeedes or not. This operator works on explicit records but already fails on typevariables because of the generic subsumption rule. It is then possible to "add"? fields using subsumption and false trigger the wrong branch?. THe concat operation usese the same right-biased union as in the reduction semantic on types. A lacks check should be added? T-check trivially returns a bool but if we have singleton types for true and false, this could even be used in branches. The final T-Acc-dyn rule lookups a label value dynamicall and uses first-class-labels.

#figure(caption: "Function typing rules", function_typing_rules) <functionrules>

@functionrules shows the typing rules for functions. T-Abs1 is the standartfunction typing rule. The following two typing rules handle ope and closed patterns respectively. The three application rules are straigt forward, but we want to note that T-App3 does not allow subsumption because open patterns... Actually this need to be domain checks.

#with_inherit
#occurrence_typing

#let f = [
  == Unholy, Untypable
  `a: b: with a; with b; x`

  An idea I had for this one is, that you keep the constraint lying around, that any of the supplied arguments needs to have the argument. Now the one important thing is to see, if this approach is actually applicable during type inference and whether this information leaks to wide and can not be resolved anymore…

  One good thing is, that type inference is always as complete as an evaluator could be. We might get non-termination from it, but that is generally not too bad maybe?



  == Builtin types <builtin_types_discussion>

  == Recursiveness
  You can decide between equi-recursive and iso-recursive when modeling a typesystem.
  Equi-recursive has the benefit that you can just declare the infinite rollout of recursive type _euqal_ to their un-rolled out form whereas in iso-recursive type systems, you need explicit rollout operations. The main drawback of equi-recursiveness is is that it comes with high algorithmic complexity and heavy metatheory due to its co-recursive definition. Zhou et. al showed how to create an efficient algrothim for iso-recursive type systems in their develpment of Quicksub @quicksub. In addition, despite being less convenient, iso-recursive types are known to have the same expressive power as equi-recursive types @quicksub.

]


#bib
