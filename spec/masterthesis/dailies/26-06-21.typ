
./26-06-22.typ
./26-06-20.typ
== Misc
- Ich habe halt Angst, dass das alles zu wild wird (viele Konzepte und nichts richig anstatt kleinen, aber harten calculus)
- Junge, ich werde von Thiemann so aufs Maul bekommen lol
- Ist für ihn okay, dass wir einfach was soundes mit Abstrichen erstellen und das dann irgendwie beweisen?


== Fragen
- Wenn wir überall direkt row-variablen einbauen, können wir dann überhaupt irgendwas sagen?
  - Ist es möglich, Rows wieder zu eliminieren?
  - Eigentlich nicht, es gibt kein Construct, was einen Record "reduziert"
- Kann ich noch negative Rec-fields hinzufügen?
  - Anscheinend ist das halt gefährlich wegen _boolean algebra_.
  - Aber die wird halt nur übermaßig komplex, wenn man das für alle Typen erlaubt.
- Wie genau gehen wir bei Funktionsaufrufen und ∈-constraints vor?
  - Ich will halt eigentlich Regeln, die Typvariablen/constraints aufschieben
  - Also muss man beim Aufrufen checken, ob die Constraints aus dem Körper teilweise angewandt werden können
- Wie genau gehen wir bei Funktionsaufrufen und Subtyping-Ungenauigkeit vor?
  - Wir wissen dann halt nicht, ob der Funktionsaufruf dann stattfinden kann
  - D.h geben wir in dem Fall einfach ★ zurück?
  - Oder machen wir so, also ob es funktioniert hätte.
    - Das ist halt grob unsound
    - Also eher ★ zurück geben, weil wir keine Ahnung haben
- Findet Thiemann das geil, dass wir so ins Ungenaue gehen?
  - Ist halt nicht anders möglich lule
- Wie genau werden die Rows eingeführt?
  - Sollen wir einfach jeden Record mit Row spawnen?
  - Eigentlich sollten wir das glaube ich nur machen, wenn wir typvariablen haben
  - Die werden dann bei record-access zu einer record-typevar
  - Oder ist das Record-sein dann nur ein Constraint? (a.k.a Parreaux)


== Subtyping

ρ₁ ⧀ ρ₂
-------------------------------- ⧀-width
{ l₁: τ₁ | ρ₁ } ⧀ { ρ₂ }


l₁ = l₂    τ₁ ≤ τ₂    ρ₁ ⧀ ρ₂
-------------------------------- ⧀-depth
{ l₁: τ₁ | ρ₁ } ⧀ { l₂: τ₂ | ρ₂ }


- Für label variablen:
[α = l]¡   τ₁ ≤ τ₂    ρ₁ ⧀ ρ₂
--------------------------------- ⧀-depth-var-lab
{ α: τ₁ | ρ₁ } ⧀ { l: τ₂ | ρ₂ }


- Now we can
a != b
---------------------------------------------  ⧀-comm
{α: τ₁ | b : τ₂ | ρ₁} ⧀ { b: τ₂ | a: τ₁ | ρ₂}


- Mit ner Row-Variable ist es halt vorbei
- Außer wir können irgendwie einen Constraint behalten

¿
-------------------- ⧀-row-r
{ l₁: τ₁ | ρ₁} ⧀ { α }


¿
-------------------- ⧀-row-l
{ α } ⧀ { l₁: τ₁ | ρ₁}

