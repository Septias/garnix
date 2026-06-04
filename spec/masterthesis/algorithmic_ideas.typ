== Subtyping für Rows mit Vars
- Wir haben die Row x: { a: int, b: {c : str}} und y: { a: α, β@{γ} }
- Hier muss jetzt einfach α ≤ int oder α ≡ int

== Beispiel: Row introduction
```nix
let
  f = a: a.b;
in ()
```

1. Hier wird eine typvariable für a eingeführt
2. Durch den Zugriff wissen wir, dass es ein record sein muss
3. Theoretisch würden wir dann { b: τ } als  _lower bound_ hinzufügen
4. Was wir aber in row system machen ist: α bekommt einen constraint ⟨b | ρ⟩?


== Beispiel: Nominale Tags funktionieren nicht
```nix
let
 a = { b = 2; };
 c = { b = 2; };
in ()
```
- Das man records in Nix schreiben kann reicht schon?


== Beispiel: Asymmetric Concat
```nix
let
 e1 = a: b: (a // b).c;
 e2 = a: b: c: (a // b).${c};
in ()
```

1. Die beiden Argumente werden als Variablen in den Context aufgenommen
2. Danach müssen beide durch das Concat zumindest mal ein Record sein
3. Und in diesem gemeinsamen Record muss das Feld `c` enthalten sein

- Nun wissen wir nur leider nicht, aus welchem es stammt (beide sind ja abstrakt)
- Wir können uns aber für die beiden Argumente merken, dass es in der Verbindung stecken _muss_
- Wir geben also die _proof-obligation_ an den Caller weiter. Dieser muss beweisen können, dass das Feld c tatsächlich existiert
- Um diese Art von Model zu haben, bringen uns polynomische Rows nicht so viel, weil die Ungewissenheit zu groß ist
- Nun könnte man versuchen, diese Beweise zu generieren, das stinkt aber nach untractability.
- Auf der anderen Seite kann man das Typsystem schwächen: Nur Inferenz, wenn keine zwei Unbekannten
- Oder man modelliert die Auswertung? Stinkt aber auch nach untractability
- Oder man darf gar nicht zwei Unbekannte zusammenführen?

== Beispiel: Intersection
```nix
let
  f = a: b: c: if a then b else c;
in f
```
- Hier laufen b und c zusammen. Dh. f hat Typ `bool -> b -> c -> b ∨ c`


== Beispiel: Union
```nix
let
  f = a: a;
  x = f 1;
  y = f "str";
in f
```
- Hier muss die Funktion für int und str funktionieren. => f: int ∨ str -> ?


== Beispiel: Intersection an positiver Stelle
```
builtins.Removeattrs :: r ≤ {} => r -> [overline(str_i)] -> r ∧ ¬{overline(str_i)}
```

```nix
let
   x = builtins.removeAttrs {a = 2; b = 2;} ["b"];
in x
```
- This has type: {a = 2;} ∧ ¬{ b: ? }
- We can not find a nice type for this!
- The strs have to be labels!
  - Maybe just prefill this function?

== Scoped Rows
Rows mit Typvariablen:
⟨ρ₁⟩ | ⟨ρ₂⟩ -> ⟨ρ₁ ρ₂⟩
⟨ρ₁⟩ | ⟨α⟩  -> ⟨ρ₁ α⟩
⟨α⟩  | ⟨ρ₁⟩ -> ⟨α ρ₁⟩
⟨α⟩  | ⟨β⟩  -> ⟨α β⟩

Indexing mit Typvariablen:

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

== Algorithmus für Funktionen
- Funktionen: Monomorph
- Wie modellieren wir record-referenzen (enstehen durch Destructuring)?
  - Theorie: Matching?
    - { α, l: τ } fügt dann Typvariablen und Bindings (wieder?) in den Kontext
- Können wir _inferieren_, wann etwas ein Label sein muss? Ja?


== Subtyping für Rows mit FC-Labels
- Wir haben die Row x: { a: int, b: {c : str}} und y: { α: int, β\@{γ} }
- Jetzt müssen wir entscheiden, ob die subtypen sind
- Damit wir sagen können y ≤ x, muss α = a
- Wie kann man das als Constraint machen?
- Wir haben halt noch kein _generelles_ Model an constraints, die wir zulassen wollen
- Hier liegt ein stückweit auch der _Knackpunkt_

