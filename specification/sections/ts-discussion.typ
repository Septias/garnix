#import "../functions.typ": *
#import "../typesystem.typ": *


== TODO
- Scoped Record with lacks or duplicates
- Mixed use of subsumption
- No clear usage of polymorphic variables
- Missing: Gradual Typing


== Typing Approach
#figure(
  caption: "Types of Nix.",
  types,
)<types>


@types summarizes our initial set of types. We use b to range over groundtypes for language primitives like bool, int, str and define a function $b_c$ that maps every constant to its type $b_c (c) = b$. We adopt the standard constructor for functions and extend it with patterns. These patterns can either be open (+) or closed (-). The pattern elements can either be ordinary types for required arguments or a type with its default type in the superscript $τ^τ$.

Records types are defined by a finite mapping of labels to types. We also provide an array type for heteogenous arrays $[overline(τ)]$ that behave as tuples and a homogenous array type $[τ]$ for arrays of the same type. We want to note that types can be composed with type connectives. The list `[true 1]` can thus be given the type $[ int ∨ bool]$ or $[ int bool]$. To obtain a Boolean algebra of types, we admit the connectives $¬, ∨, ∧$ together with the top and bottom elements $⊤$ and $⊥$, which are, respectively, the greatest and least elements of the subtyping hirarchy.


#basic_typing_rules <typingrules>
@typingrules shows the basic typing rules for Nix. They are mostly standard but four list typing rules account for homogenous and heterogenous array types and their respective concatenations. The operator typing rules are relegated to \@operator-typingrules.

#figure(caption: "Record typing rules", record_typing_rules) <recordrules>

The record typing rules are given in @recordrules. T-Rcd and T-Proj are entirely standart and the T-Or-Neg and T-or-Pos handle the or-operator if the lookup succeedes or not. This operator works on explicit records but already fails on typevariables because of the generic subsumption rule. It is then possible to "add"? fields using subsumption and false trigger the wrong branch?. THe concat operation usese the same right-biased union as in the reduction semantic on types. A lacks check should be added? T-check trivially returns a bool but if we have singleton types for true and false, this could even be used in branches. The final T-Acc-dyn rule lookups a label value dynamicall and uses first-class-labels.

#figure(
  caption: "Function typing rules.",
  function_typing_rules,
) <functionrules>

@functionrules shows the typing rules for functions. T-Abs1 is the standartfunction typing rule. The following two typing rules handle ope and closed patterns respectively. The three application rules are straigt forward, but we want to note that T-App3 does not allow subsumption because open patterns... Actually this need to be domain checks.

#with_inherit
The with construct binds variables from a record. The first premise ensures this condition. The second premise extends an new context Ξ with the given record type. This context stores the list of with-bindings that can be used as fall-backs. The lookup operation of an environment based interpreter can then be extended to lookup expressions that are not bound in Γ in Ξ. Since with bindings can be stacked with the latest taking precedence, the lookup has to start at the end of the new context.
This works for the simple case of explicit record types but becomes more involved for type, and row-variables. A record type can have the form ${ overline(l\: t) | ξ }$ where ξ denotes the rest of the record. Should ξ be a row variable ρ, then .. ?

The other two derivations can be used in environment interpreters or with subtle changes for type inference. Both rules use the context Γ to check whether the inherited variable is in the context and desugare to an binding using the value taken from the context. This is a neccessary step to circumvent the accidental introduction of unprotected recursion as in
```
rec { inherit x; } → { x = x; }
```
We don't give a syntactic definition of the lookup routine since an implementation that iteratively looks up a nested record is straight forward.


#occurrence_typing

We finally want to determine a system to use the function based bool-checks for guard analysis. Occurrence typing has been studied for Erlang @schimpf2022set Elixir @gradual_elixir in where-clauses of case-branches. Both languages allow pattern guards on destructured case-branches. Elixir has a stronger version of type checks of the form $e ? τ$ that checks an expression against an arbitrary type than Erlang. The type checks employed by Schimpf et. al @schimpf2022set use function predicates of the form $"is"_b(e)$ where b ranges over ground types bool, int, str,… .


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
