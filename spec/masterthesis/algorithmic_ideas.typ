


== Beispiel: Asymmetric Concat
```nix
let
 e1 = a: b: (a // b).c;
 e2 = a: b: c: (a // b).${c};
in ()
```

1. Die beiden Argumente werden als Variablen in den Context aufgenommen
2. Danach müssen beide durch das concat zumindest mal ein Record sein
3. Und in diesem gemeinsamen Record muss das Feld `c` enthalten sein

- Nun wissen wir nur leider nicht, aus welchem es stammt (beide sind ja abstrakt)
- Wir können uns aber für die beiden Argumente merken, dass es in der Verbindung stecken _muss_
- Wir geben also die _proof-obligation_ an den Caller weiter. Dieser muss beweisen können, dass das Feld c tatsächlich existiert
- Um diese Art von Model zu haben, bringen uns polynomische Rows nicht so viel, weil die Unwissenheit zu groß ist
- Nun könnte man versuchen, diese Beweise zu generieren, das stinkt aber nach Runberechenbarkeit.
- Auf der anderen Seite kann man das Typsystem schwächen: Nur Inferenz, wenn keine zwei Unbekannten
- Oder man modelliert die Auswertung? Stinkt aber auch nach Runberechenbarkeit.
- Oder man darf gar nicht zwei Unbekannte zusammenführen?

