./26-05-13.typ

== Vorgehen: Typ-Algorithmus
1. Wir lesen den Code und erstellen ein Parsetree
  - Debjrujin?
2. Jede Funktion bekommt für ihre Argumente _Typvariablen_
3. Die Nutzung dieser Typvariablen wird im Funktionskörper analysiert
4. Anhand der Nutzung stellen wir dann fest:
  - Welche Felder hat diese Variable?
  - Andere Typinformationen
5. Diese Constraints werden gesammelt
  - Auf den Variablen?
  - In einem Context?
6. Und dann gelöst durch
  - Unification?
  - Bi-unification?


== Misc
- What inference engine?
  - subtyping?
  - constraints?
  - unification?
  - bi-unification?


== Records mit vielen Rows
Rows mit typvariablen:
⟨ρ₁⟩ | ⟨ρ₂⟩ -> ⟨ρ₁ ρ₂⟩
⟨ρ₁⟩ | ⟨α⟩  -> ⟨ρ₁ α⟩
⟨α⟩  | ⟨ρ₁⟩ -> ⟨α ρ₁⟩
⟨α⟩  | ⟨β⟩  -> ⟨α β⟩

Indexing mit typvariablen:

⟨ρ₁ ρ₂⟩.x ->
- ρ₁.x if x ∈ ρ₁
- ρ₂.x if x ∈ ρ₂
- e else

⟨ρ₁ α⟩.x  ->
- ρ₁.x if x ∈ ρ₁
- ?    if x ∈ ρ₂

⟨α ρ₁⟩.x  ->
- ρ₁.x if x ∈ ρ₁
- ?    if x ∈ ρ₂

⟨α β⟩.x   ->
- ?    if x ∈ ρ₁
- ?    if x ∈ ρ₂

Das Problem ist, dass ich nicht weiß, was in den Typvariablen drin steckt. Die einzige Lösung ist dann, die Auflösung zu _verzögern_ oder eine _proof-obligation_ zu erstellen. D.h entweder man versucht im Nachhinein (z.B) beim Function Call zu klären, ob alles funktioniert. Oder man erweitert die Sprache so, dass schon beim Call "bewiesen" werden muss, dass die gefragten Felder in dem Record existieren.

== Wie genau macht ROSE das?
{ l: τ} <= ζ₃ , ζ₁ ⊙ ζ₂ ~ ζ₃ => l -> τ


== Beispiel: Funktion
```nix
let
  arg = {a = 2; c = 3;};
  fun =  {a, ..}: a;
in (fun arg)
```

== Relations
- Subtyping: ≤
- Containment: ⧀
- Destructuring: ⊙


== Tabular Data
⊕ Unification
⊕ First class labels
⊕ Polymorphic Rows
⊖ Kein Subtyping


== Abstracting Extensible Data Types
⊕ Variants & Records
⊕ Asymmetric Concat 
⊕ Expressive constraints: $_ ⊙ _ ~ \_$(containment) and $ζ ⧀ ζ$(subtype)
⊖ System F (Explizite type-application)
  ⊖ Keine Inferenz für Funktionen
