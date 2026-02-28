#import "../functions.typ": *
#import "../typesystem.typ": *
#set page(height: auto)
#set heading(numbering: "1.")

== TODO
- How does record concat exactly work?
- Do Abs2 & Abs3 account for recursivto typeness?
- Let rule missing xD


#let export = [
  // #set figure(placement: none)

  = Typing Approach <section-ts-discussion>
  This section surveys typing rule candidates that could be used to type Nix. The systems are only shown in brevity to give a general idea but might show some unsoundness or be unclear without reading the papers they are based on.

  #figure(
    caption: "Types of Nix.",
    types,
  )<types>


  @types summarizes our initial set of types. We use b to range over groundtypes for language primitives like bool, int, str and define a map $b_c$ that maps every constant c to its type. We adopt the standard constructor for functions and extend it with patterns. These patterns can either be open (+) or closed (-). The pattern fields can either be ordinary types for required arguments or a type with its default type in the superscript $τ^τ$.

  Records types are defined by a finite mapping of labels to types. We also provide an array type for heteogenous arrays $[overline(τ)]$ that behave as tuples and a homogenous array type $[τ]$ for arrays of the same type. We want to note that types can be composed with type connectives. The list `[true 1]` can thus be given the type $[ int ∨ bool]$ or $[ int bool]$. To obtain a Boolean algebra of types, we admit the connectives $¬, ∨, ∧$ together with the top and bottom elements $⊤$ and $⊥$, which are, respectively, the greatest and least elements of the subtyping hirarchy.


  #basic_typing_rules <typingrules>
  @typingrules shows the basic typing rules for Nix. They are mostly standard but four list typing rules account for homogenous and heterogenous array types and their respective concatenations. The missing operator typing rules are relegated to @operator-typingrules.

  == Record Typing
  #figure(caption: "Record typing rules and type syntax.", [
    #subbox(caption: "Types", flexbox(
      $#type_name("Type Variables") a ∈ cal(A)$,
      $#type_name("Labels") l_c ∈ cal(L)$,
    ))
    $
           #type_name("Kinds") && κ & ::= ∗ | κ_1 → κ_2 | "Row" | "Label" \
      #type_name("Poly Types") && σ & ::= ∀a: κ. σ | τ \
            #type_name("Type") && τ & ::= a | "Int" | → | τ_1 τ_2 | {ρ} | ⟨ρ⟩ | ⦅l⦆ \
             #type_name("Row") && ρ & ::= a | "Empty" | l: τ | (ρ_1 | ρ_2) \
           #type_name("Label") && l & ::= a | l_c \
         #type_name("Context") && Γ & ::= ε | Γ · a: κ | Γ · x: τ \
    $

    #subbox(caption: "Typingrules", record_typing_rules)

  ]) <recordrules>

  A set of record typing rules are given in @recordrules. We adopt the formalization of Adam Paszke and Ningning Xie with first-class labels, row polymorphism, open record extension @extensible_tabular. We use a kinding system to distinguish between ordinary types, Rows and Labels and type constructors ${ρ}$, $⟨ρ⟩$, and $⦅l⦆$ to create record-, row- and label-types. Notice that types, rows and labels admit type variables and that $⟨ρ_1 | ρ_2⟩$ is the concatenation operation on two rows.

  T-Rcd and T-Proj are the natural extension to first class labels. The T-Or-Neg and T-or-Pos handle the or-operator if the lookup succeeds or not. The check ∈ operation is new. This operator works on explicit records but fails in presence of the generic subsumption rule and with-subtypng. Using both rules it is possible to "forget" record fields such that the or-rule dedudces the wrong type.
  The concat operation usese simple row concatenation since the semantics using multiple labels already handes the case for duplicate labels @extensible_tabular. T-check trivially returns a bool but if we have singleton types for true and false, this could be refined.

  In research there exist different approaches to the problem of first-class labels @fc_labels @typing_records_etc @extensible_rec_funcs @extensible_data_adhoc. The semantic approach by Castagna uses the semantic interpretation of types to describe the set of labels. Since every literal obtains a singleton type, this addition is straight forward. The second line of research extends the work of Daan Leijin, using a special label kind and type constructors that describe for every label its singleton type $⦅l⦆$ @fc_labels @extensible_rec_funcs @extensible_data_adhoc. Both approaches seem to be applicable to Nix such that a decision for either of them relies on the decision between semantic subtyping or unfication and quilified types based systems.

  == Function Typing
  #figure(
    caption: "Function typing rules.",
    function_typing_rules,
  ) <functionrules>

  @functionrules shows the typing rules for functions. T-Abs1 and T-Abs2 handle open and closed patterns respectively and T-App2 as well as T-App3 are the respective elimination rules that use auxiliary judgements $needed(oa)$ and $ceiling(oa)$. The judgement $needed(oa)$ throws away optional pattern fields such that a record argument admits the freedom to lack these fields. The other judgement $ceiling(oa)$ requires all arguments and is used in T-App3 to ensure that the argument type does not provide more fields. Again, generic record subtyping and width subtyping allows to forget a field that was previously in a record argument such that a match might falsy fail because a field is missing (that is actually there).

  == Other Constructs
  #other_constructs
  The with construct binds variables from a record in the following expression. The first premise of T-With ensures this condition and the second premise extends an new context Ξ with the given record type. This context stores the list of with-bindings that can be used as fall-backs. The lookup operation of an environment based interpreter can then be extended to lookup expressions that are not bound in Γ in Ξ. We want to note that this mechanism has the same power as _deferred substitutions_. Since with-bindings can be stacked with the latest taking precedence, the lookup has to start at the end of the new context.
  This works for the simple case of explicit record types but becomes more involved for type-, and row-variables. A currently untypable expression is:
  ```nix
  a: b: with a; with b; x
  ```
  This expression uses two typevariables a and b, and extends the scope of the term x with the bindings from a and b. We know that x is from either a or b, with b taking precedence but it can not be deduced where exactly to take x from. To circumvent this we can maybe add a constraint $x ∈ a ∪ b$ and delaying its resolution until function application. The function application of this example:

  ```nix
  let f = (a: b: with a; with b; x) in f y {}
  ```
  can then reason as follows: The second argument b is an explicit record that does not have any bindings, such that $x ∈ a ∪ ∅ => x ∈ a$ can be applied to the type variable $y$. If, again, both arguments were type-variables, no further information can be concluded and the constraints has to be persistet.

  Whether this reasoning is applicable, inferable and scalabale is not clear but it shows an interesting direction for further research that could well combine with semantic subtyping.

  The two inherit-derivations R-Inherit and R-Inherit-path can be used in environment based interpreters or with subtle changes for type inference. Both rules use the context Γ to check whether the inherited variable is in the context and desugares to a binding using the value taken from the context. This is a necessary step to circumvent the accidental introduction of unprotected recursion as in the following example.

  ```nix
  rec { inherit x; } → rec { x = x; }
  ```
  We don't give a syntactic definition of the lookup routine since an implementation that iteratively looks up a nested record is straight forward but note that row variables complicate the matter and that inherit can use with-inherited variables.

  Furthermore note, that the explicit context check is, similarly to the with-rule, equally powerful as _deferred substitutions_. We conjecture that the combinitation of both enables us to remove the verbose abs/with and rec/nonrec annotations in the reduction semantic and form a simpler system.

  The T-import typing derivation calls the typing routine on the file at location 𝜚 and expects it to evaluate to a term t with type τ. This type is the final type of the import expression.


  == Occurrence Typing
  #occurrence_typing <occ>

  @occ shows a system to use the function based bool-checks for occurrence typing. Occurrence typing has been studied for Erlang @schimpf2022set and Elixir @gradual_elixir in where-clauses of case-branches. Both languages allow pattern guards on destructuring case-branches. For example the pattern guard `(a, b) when isBool(a)` destructures a tuple into its components a an b. Following this destructuring, the variable $a$ is checked to be of type bool.

  Elixir has a stronger version of type checks of the form $e #b[?] τ$ that checks an expression against an arbitrary type than Erlang that uses function predicates of the form $"is"_b (e)$ where b ranges over ground types bool, int, str,… . Nix only provides primitive conditionals and not the more expressive case-expression that can  destructure nested data structure and bind new variables. Our theory consequently does not contain substitutions and only needs to apply typing hypotheses for terms that were applied during condition-checking. A simple example is the following typing rule:

  #align(center, derive(
    "T-Bool-Simple",
    ($"isBool"(c) arrow.squiggly "true"$, $ Γ · (Γ(c) ∧ bool) ⊢ t_1: τ $),
    $Γ ⊢ #b[if] "isBool"(c) #b[then] t_1 #b[else] t_2: τ$,
  ))

  This exemplary type derivation uses the fact that the condition returns true, such that the positive branch of the conditional can be typed under the assumption, that the type of c is a boolean. Notice that we use type connectives to retain the information we might possess over the type c. This example is very simple, but in general, condition can be an arbitrary term as long as they reduces to a boolean. This includes functions that can internally use arbitrary computations to return a boolean.

  To show the general approach, we restrict the syntax of the condition to basic checks like `isBool`, `isAttr`, ..., field checks `{a = t;} ? a` and show inference in presence of boolean connectives. To support the conditions full expressiveness, an oracle type inspired by @schimpf2022set can be used to type undecidable checks. Occurrence typing is then a technique that only works for simplified checks and doesn't interfere with type inference otherwise.

  We admit boolean type connectives (and, or, negation) to our language and apply their assumptions based on the outcome of the conditional. For this we need to track, which checks lead to the satisfaction of the condition.

  The rules T-Cond-\* type the conditional based on the boolean state of the condition. Both use an auxiliary judgement $Γ ⊢ t: τ => Ξ$, that can be read as "In context Γ, the term t is typed as τ, producing typing hypotheses Ξ". The auxiliary Context Ξ is used to store typing hypotheses and permeates through the derivation tree. The rule T-Ground adds new typing hypotheses of the form t: τ that assert t has type τ and b ranges over the groundtypes of our language bool, str, int, … . T-Has creates a constraint of the form t: {l : τ} that asserts the existence of a label.

  The last three rules propagate constraints through logical connectives. The T-And rule types both operators and combines their constraints. The T-Or rule does not take any constraints because it is not clear which branch lead to the satisfaction. (If we model true and false, can we actually do this?). The last rule T-Neg negates all hypotheses that were formed during the inference of t_2.

  == Recursiveness
  Typesystems can be modeled equi-recursive or iso-recursive. This distinction is important because Nix provides recursiveness in so many places (patterns, records, array-fields and operators).

  Equi-recursive has the benefit that you can just declare the infinite rollout of recursive type _equal_ to their un-rolled out form whereas in iso-recursive type systems, you need explicit rollout operations. The main drawback of equi-recursiveness is is that it comes with high algorithmic complexity and heavy metatheory due to its co-recursive definition. Zhou et. al showed how to create an efficient algrothim for iso-recursive type systems in their development of Quicksub @quicksub. In addition, despite being less convenient, iso-recursive types are known to have the same expressive power as equi-recursive types @quicksub.

]
#export

== Dummy Section <operator-typingrules>

#bib
