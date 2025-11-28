Viko: könnte ich Montag 1.12. oder Freitag 5.12. jeweils um 9 anbieten.
Schickst du mir bitte mal das Rocq paper (oder Link) von 2025?

Mir ist gerade noch was zu t.l or t eingefallen. Semantisch reduziert ja t.l -> null, falls t das Feld l nicht besitzt. Aus deiner overview geht nicht hervor, was null für einen Typ hat. Aber die Reduktion besagt ja, dass man jeden Recordtyp mit dem Typ von null "auffüllen" darf. Ob's hilft kann ich gerade nicht sagen.

- [ ] give null a type

Vielleicht hilft folgende Idee: man unterscheidet zwischen exact record types !{l_i:t_i}, die genau die vorhandenen Felder beschreiben, und inexact record types {l_i:t_i}. Ein record startet mit einem exact type, aber sobald eine (nicht triviale) subsumption passiert, wird es inexact.
Wenn nun beim t.l or t' das t einen exact type hat, dann kann man den Typ ohne Probleme exakt bestimmen. Andernfalls bleibt nur \top als Ergebnis. 
Noch anders sieht es aus, wenn es Typen gibt, deren Wert auch null sein darf: Wenn t.l einen "nullbaren" Typ TL hat, dann ist das Ergebnis tatsächlich TL \/ T, wenn T der Type von t' ist.

Die Zeile Operator in Fig. 1 ist eine Dublette, oder?

R-Fun-Pat-Open: ich verstehe, was gemeint ist, aber die Verwendung von dem \forall\exists würde ich als sloppy und ungenau bewerten.
Bei den -Default Regeln fragt man sich, wo die d_i herkommen. Mit dem "substitute twice" ist es so eine Sache.  Was wenn die ursprünglichen t_j selbst wieder l_is enthalten?
R-Lookup-Default-Neg: was wenn \exists i. l_i = l, aber t_i = null?
Die Values in Fig. 3 verstehe ich nicht: ok p:t ist ein Lambda, aber was ist x;? und was wird durch {..} abgekürzt? Es fehlt wohl v ::= ...
Letzte Zeile Evaluationkontext muss enden mit v \bullet \Box
Regeln für die Pattern Types habe ich nicht gefunden und mir fehlt auch die Intuition dafür??
