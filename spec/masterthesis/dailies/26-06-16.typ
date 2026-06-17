> Die Grundidee ist also, ein unification-algorithmus zu erstellen, der nahe an der Nix Sprache liegt und der _extensible-rows_ verwendet. Damit liegt der ziemlich nah an _Infix extensible tabular data_, aber das ist glaube ich okay? Auf jeden Fall füge ich dann die ∈-constraint hinzu, versuche das zu lösen und gebe ansonsten ★ zurück. Damit kann ich glaube ich einen guten Kalkulus erstellen, den eventuell auch durchbeweisen und meine Masterarbeit schreiben. Mögliche Erweiterungen sind dann für recursiveness, inherits und withs. Wobei letzteres glaube ich relativ orthogonal und einfach ist.

./26-06-17.typ
== Fragen
- Ist es problematisch, dass ich constraints »mitten drin« löse?

== Todo
- ⌊p⌋ und ⌈p⌉ als Funktionen definieren
- Row syntax unifien

== Fäden
- Wie genau ⧀ auf rows / records?
- Unification rules aufschreiben
- Instantation Rules aufschreiben
- Wie mache ich den Übergang ins ∈-System?


== Syntax
- records: {l = t}
- types: {l: t}
- Oder beides als row: ⟨l: τ⟩
- Finde ich die Doppeldeutigkeit okay?
- Eigentlich nein, aber ich will halt auch nicht alle Zeichen aufbrauchen

Zuweisung:
- Record: {e₁ = e₂}
- Record-type: { ρ }
- Row: α | ε | l: τ | (ρ₁ | ρ₂)
- Pattern: { p, ...}
- Pattern elemen p: (l: τ) | (l :: τ)
- Pattern-type: {p}±


== Inferenz
Γ · (b ∈ a) ⊨ τ
---------------
Γ ⊢ a.b: τ

== Subtype Hirarchy
l₁ = l₂    τ₁ ≤ τ₂   l ∉ ρ₁   l ∉ ρ₂
-----------------------------------
{ρ₁ | l₁: τ₁} ≤ { ρ₂ | l₂: τ₂ }


== Pattern-Matching Rules

ρ₁ ⧀  ρ₂
---------------------------------
{ρ₁} ⧀  {ρ₂}


l₁ = l₂    τ₁ < τ₂    ρ₁ ⧀ ρ₂
---------------------------------
⟨l₁: τ₁ | ρ₁⟩ ⧀ ⟨ l₂: τ₂ | ρ₂⟩


- Für label variablen:
[α = l]¡   τ₁ < τ₂    ρ₁ ⧀ ρ₂
---------------------------------
⟨α: τ₁ | ρ₁⟩ ⧀ ⟨ l: τ₂ | ρ₂⟩


??    hier einfach constraint?
--------------------
⟨ l₁: τ₁ | ρ₁⟩ ⧀ α


??    hier einfach constraint?
--------------------
α ⧀ ⟨ l₁: τ₁ | ρ₁⟩


== Unification

(Γ, α, τ) => (Γ', )
(Γ, τ, τ) => ⊤

(Tapp)   τ₁τ₂, τ₃τ₄         => τ₁ ~ τ₃ ∧ τ₂ ~ τ₄
(Tsolve) (Γ, α, τ)          => Γ · α: κ = [Γ]τ ⊢ ⊤    | if (α : κ ∈ Γ) ∧ (Γ ⊢ τ: κ)         ∧ (α ∉ ftv([Γ]τ))
(Rfield) Γ₁ ⊢ ℓ: τ, ρ₁ ~ ρ₂ => Γ₂ ⊢ τ ~ τ' ∧ ρ₁ ~ ρ'₂ | if (Γ₁ ⊢ [Γ₁]ρ₂ ℓ↪ ℓ: τ., ρ'₂ ⊣ Γ₂) ∧ ([Γ₂]ρ₁ = [Γ₁]ρ₁)
