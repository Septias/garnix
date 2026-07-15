
./26-07-14.typ

== Fragen
- Was macht `by …`?
  - Wechselt von Term zu _tactic mode_
- Was genau macht `by cases h2; rfl`?
  - Schreibt die branches aus, zero constructors löst oft schon alles
- Was machen die Klammern ⟨⟩
  - Constructor sugar
- Warum ResEquiv?
  - Types can change syntactically ({ε} vs {ε | ε}) but they can not change category (τ, ⊥, ?)
- What is _head rigidy_?
  - The row type can change slightly (in an equivalence class)
- What is _nose_?
  - Mathematic slang

== Todo
- [x] Pen & Paper proof-structur aufschreiben (was wir haben)
- Can be typed at `r <= {l: τ} => r -> τ` (if there is subtyping in the system) because that is the only way our function does not get stuck. The function `a: b: (a || b).l` can not be given a type though.


== Progress
- Wir haben jetzt den Typesafety proof für den minimalen calculus
- Der ist extrinsisch
- Kein Debrujin
- Hat viele gute sublemmas
- Die Row-equivalence hat großen Einfluss auf die Proofs
- _Let-poly fehlt noch_
- Nur über _closed terms_
