#import "../functions.typ": *
#import "../typesystem.typ": *


== TODO
- Scoped Record with lacks or duplicates
- Mixed use of subsumption
- No clear usage of polymorphic variables
- Missing: Gradual Typing

#let export = [
  #set figure(placement: none)

  = Typing Approach
  #figure(
    caption: "Types of Nix.",
    types,
  )<types>


  @types summarizes our initial set of types. We use b to range over groundtypes for language primitives like bool, int, str and define a function $b_c$ that maps every constant to its type $b_c (c) = b$. We adopt the standard constructor for functions and extend it with patterns. These patterns can either be open (+) or closed (-). The pattern elements can either be ordinary types for required arguments or a type with its default type in the superscript $τ^τ$.

  Records types are defined by a finite mapping of labels to types. We also provide an array type for heteogenous arrays $[overline(τ)]$ that behave as tuples and a homogenous array type $[τ]$ for arrays of the same type. We want to note that types can be composed with type connectives. The list `[true 1]` can thus be given the type $[ int ∨ bool]$ or $[ int bool]$. To obtain a Boolean algebra of types, we admit the connectives $¬, ∨, ∧$ together with the top and bottom elements $⊤$ and $⊥$, which are, respectively, the greatest and least elements of the subtyping hirarchy.


  #basic_typing_rules <typingrules>
  @typingrules shows the basic typing rules for Nix. They are mostly standard but four list typing rules account for homogenous and heterogenous array types and their respective concatenations. The operator typing rules are relegated to \@operator-typingrules.

  == Record Typing
  #figure(caption: "Record typing rules and type syntax.", [
    #subbox(caption: "Syntax", flexbox(
      $#type_name("Type Variables") α ∈ cal(V)_t$,
      $#type_name("Labels") l_c ∈ cal(L)$,
    ))
    $
           #type_name("Kinds") && κ & ::= ∗ | κ_1 → κ_2 | "Row" | "Label" \
      #type_name("Poly types") && σ & ::= ∀a: κ. σ | τ \
            #type_name("Type") && τ & ::= a | "Int" | → | τ_1 τ_2 | {ρ} | ⟨ρ⟩ | ⦅l⦆ \
             #type_name("Row") && τ & ::= a | "Empty" | l: τ | (ρ_1 | ρ_2) \
           #type_name("Label") && l & ::= a | l_c \
         #type_name("Context") && Γ & ::= • | Γ, a: κ | Γ, x: τ \
    $

    #subbox(caption: "Typingrules", record_typing_rules)

  ]) <recordrules>

  The record typing rules are given in @recordrules. We adopt the formalization of Adam Paszke and Ningning Xie with first-class labels, row polymorphism, open record extension. T-Rcd and T-Proj are entirely standart and the T-Or-Neg and T-or-Pos handle the or-operator if the lookup succeedes or not. This operator works on explicit records but already fails on typevariables because of the generic subsumption rule. It is then possible to "add"? fields using subsumption and false trigger the wrong branch?. THe concat operation usese the same right-biased union as in the reduction semantic on types. A lacks check should be added? T-check trivially returns a bool but if we have singleton types for true and false, this could even be used in branches. The final T-Acc-dyn rule lookups a label value dynamicall and uses first-class-labels.



  In research there exist different approaches to the problem of first-class labels @fc_labels @typing_records_etc @extensible_rec_funcs @extensible_data_adhoc. The semantic approach by Castagna uses the semantic intepretation of types to describe the set of labels. Since every literal obtains a singleton type, this addition is straight forward. The second line of research extends the work of Daan Leijin, using a special label kind and type constructors that describe for every label its singleton type $⦅l⦆$ @fc_labels @extensible_rec_funcs @extensible_data_adhoc. Both approaches seem to be possible for nix such that a decision for either of them relies on the decision between semantic subtyping or unfication and quilified types based systems.

  == Function Typing
  #figure(
    caption: "Function typing rules.",
    function_typing_rules,
  ) <functionrules>

  @functionrules shows the typing rules for functions. T-Abs1 is the standart function typing rule. The following two typing rules handle ope and closed patterns respectively. The three application rules are straigt forward, but we want to note that T-App3 does not allow subsumption because open patterns... Actually this need to be domain checks.

  #with_inherit
  The with construct binds variables from a record in the following expression. The first premise ensures this condition and the second premise extends an new context Ξ with the given record type. This context stores the list of with-bindings that can be used as fall-backs. The lookup operation of an environment based interpreter can then be extended to lookup expressions that are not bound in Γ in Ξ. We want to note that this mechanism has the same power as _deferred substitutions_. Since with bindings can be stacked with the latest taking precedence, the lookup has to start at the end of the new context.
  This works for the simple case of explicit record types but becomes more involved for type-, and row-variables. A record type can have the form ${ overline(l\: t) | ξ }$ where ξ denotes the rest of the record. Should ξ be a row variable ρ, then .. ?

  The other two derivations can be used in environment interpreters or with subtle changes for type inference. Both rules use the context Γ to check whether the inherited variable is in the context and desugare to an binding using the value taken from the context. This is a neccessary step to circumvent the accidental introduction of unprotected recursion as in
  ```
  rec { inherit x; } → { x = x; }
  ```
  We don't give a syntactic definition of the lookup routine since an implementation that iteratively looks up a nested record is straight forward.


  == Occurrence Typing
  #occurrence_typing

  We finally want to determine a system to use the function based bool-checks for guard analysis. Occurrence typing has been studied for Erlang @schimpf2022set Elixir @gradual_elixir in where-clauses of case-branches. Both languages allow pattern guards on destructured case-branches. Elixir has a stronger version of type checks of the form $e ? τ$ that checks an expression against an arbitrary type than Erlang. The type checks employed by Schimpf et. al @schimpf2022set use function predicates of the form $"is"_b(e)$ where b ranges over ground types bool, int, str,… . Nix only provides the weaker if-statement to do type-checks in comparison to case-analysis that can destructure nested data strucuters and binds new variables. Our theory is consequently does not contain substitutions and only needs to apply typing hypotheses for environment terms that were applied during the bool condition checking. A simple example would be:

  #align(center, derive(
    "T-bool-simple",
    ($"isBool"(c) arrow.squiggly "true"$, $ Γ · (Γ(c) ∧ bool) ⊢ t_1: τ $),
    $Γ ⊢ #b[if] "isBool"(c) #b[then] t_1 #b[else] t_2: τ$,
  ))

  The condition can be an arbitrary term as long at it reduces to a boolean. This includes functions that can internally use arbitrary computations to return a boolean. To show the general approach, we restrict conditionals to basic checks like `isBool`, `isAttr` and field checkes `{a = t;} ? a` and show inferrence for boolean connectives. To support more complex features, an oracle type inspired by @schimpf2022set could be used that can be given to undecidable checks. Occurrence typing is then a technique that only works for simplified checks and doesn't interfer with type inference otherwise.

  We admit boolean type connectives (and, or, negation, implication?) to our language and apply their assumptions based on the outcome of the conditional. For this we need to track, which checks lead to the satisfaction of the condition.


  == Recursiveness
  Typesystems can be modeled equi-recursive or iso-recursive. This distinction is important because Nix provides recursiveness in so many places ()


  Equi-recursive has the benefit that you can just declare the infinite rollout of recursive type _euqal_ to their un-rolled out form whereas in iso-recursive type systems, you need explicit rollout operations. The main drawback of equi-recursiveness is is that it comes with high algorithmic complexity and heavy metatheory due to its co-recursive definition. Zhou et. al showed how to create an efficient algrothim for iso-recursive type systems in their develpment of Quicksub @quicksub. In addition, despite being less convenient, iso-recursive types are known to have the same expressive power as equi-recursive types @quicksub.
]

#bib
