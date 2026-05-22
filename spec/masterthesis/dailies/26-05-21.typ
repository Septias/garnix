
== Fragen
- Warum genau funktioniert die Trennung von polaren typen nicht für mich?
  - Das lag an den _negativen typen_
- Ich könnte nochmal die Trennung überprüfen:
  - Unions nur in positiven positionen?
  - bricht bei _negativen typen_
- Wann intersection in neg. position?
  - Durch case? nein
  - Wieso brechen negative typen das?
  - Kann ich einfach lacks constraints inferieren?

== Todo
- Rows weiter behandeln und checken, wie das mit removal interagiert

== Beispiel: Unions
```nix
let
  f = a: b: c: if a then b else c;
in f
```
- Hier laufen b und c zusammen. Dh. sie müssen durch die union `b ∧ c` dargestellt werden


== Beispiel: Intersection
```nix
let
  f = a: a;
  x = f 1;
  y = f "str";
in f
```
- Hier muss die Funktion für int und str funktionieren. => f: int ∧ str -> ?


== Beispiel: Intersection an positiver Stelle
```
builtins.Removeattrs :: r ≤ {} -> [overline(str_i)] -> {} \ overline(str_i)
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
