
./26-07-10.typ
./26-07-12.typ
== Fragen
- Soll ich die constraints an Funktionen binden?
  - Für abstraction könnte das interessant sein
  - Hier werden die ja auch verwendet
- Soll ich ganze Records speichern?
  - Yees
- Kann ich ein lookup von Typevars im Kontext machen?
  - Nein, weil ich derzeit nur term-type bindings hinzufüge
  - Ich brauche dann aber type-type bindings
  - Wo könnte ich die hinzufügen?
    - Eigentlich ja auch bei Application einfach?
    - Kann halt transitiv werden dann (α: β)
  - Eigentlich reicht es, die expressions zu filtern und dann die Rows zu extrahieren
    - Also ich habe eigentlich alles im Kontex, ich muss es nur neu verpacken?


== Application
- lule, lookup kann ja einfach den Kontext verwenden?
- und dann dort nachschauen, ob die fehlenden Typvars schon gesetzt sind?
- Dann läuft refinement auch einfach über die normalen Typregeln ab
- Mehrere Applications (let-poly) geht dann auch, weil immer neue (x: {ρ}) verwendet werden

Old:
Γe₁ ~ ((ρ₁, l) ↦ β)   Γ ⊢ e₂: { ρ₂ }
ρ₁↓ₗρ₂    Γ · (x: ρ₃) ⊢ e₁: τ
--------------------------------------- T-App
Γ ⊢ (x: e₁)e₂: τ

`((Γ,e₁) ~ (r, l) => β)`


Beispiel: Refinement
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
----------------------- T-Rec   -------- T-look
Γ ⊢ (α | l₁ = 4).l₁: {ρ}         ρ.l₁: τ
----------------------------------------- T-sel
(l₂ = 3 | l₁ = 4).l₁ : τ
-------------------------- [Subst]             ------------
Γ (x: {l₂: int}) ⊢ (x || l₁ : τ).l₁: τ' -> τ   {l₂ = 2}: τ'    [τ' = { l₂: int }]
--------------------------------------------------------------------------------- T-Lam
(x: (x || (l₁ = 4)).l₁) {l₂ = 2} : τ
–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––


== Row-Lookup
- Γ ⊢ ρ.l
- This statement recursively searches rows for a label l
- If no derivation can be found, the label does not exist in ρ
- If a typevar (or la)


l₁ = l₂
----------------------------- T-look-hit
Γ ⊢ (l₁: τ | ρ).l₂ : τ


l₁ ≠ l₂      Γ ⊢ ρ.l₂: τ₂
----------------------------- T-look-rec
Γ ⊢ (l₁: τ₁ | ρ).l₂: τ₂


l₁ ≠ l₂
----------------------------- ↯
Γ ⊢ ( l₁: τ | ε ).l₂: τ


Γ ⊢ α: {ρ}  Γ ⊢ ρ.l: τ
------------------------------ T-look-Γ-rec
Γ ⊢ (α | ρ).l: τ
- If α in bound in Γ, try to look up the label there
- This also handles the case when we get ★ in (ρ.l)


Γ ⊢ α: {ρ}   Γ ⊢̷ ρ.l: τ₁   ρ.l: τ₂
----------------------------------- T-look-Γ-cont
Γ ⊢ (α | ρ).l: τ₂
- If we can not find the label in α, recurse into the row


α ∉ Γ
----------------------------- T-look-Γ-neg
Γ ⊢ (α | ρ).l: ★


== FC-Labels
- Not addded, but for completeness reasons

α ∉ Γ
----------------------------- T-look-FC-neg
Γ ⊢ (α: τ | ρ).l: ★


Γ ⊢ α: ⦅l⦆
----------------------------- T-look-FC-pos
Γ ⊢ (α: τ | ρ).l: τ


Γ ⊢ α: ⦅l₁⦆   l₁ ≠ l₂   Γ ⊢ ρ.l: τ
------------------------------------ T-look-FC-cont
Γ ⊢ (α: τ | ρ).l₂: τ
