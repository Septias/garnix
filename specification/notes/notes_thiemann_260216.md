Kommentare zum Dokument:
* [ ] (insgesamt klingt es etwas pathetisch für ein wissenschaftliches Paper; ein paar Grammatikfehler fallen auf)
* [ ] mixed operation semantics - was ist das?
* [x] without inferring -> without interfering ?
* [x] Overview ist keine eigene Section
* [x] heavily resolves ?
* [x] wo wird Figure 1 referenziert? Dito für Table 1 und Listing 1
* [x] "will has to evaluate"
* [ ] (Table 1) was soll der Kreis bei functionArgs?
* [ ] (Listing 1) terminale und nichtterminale kann man nicht gut auseinander halten
* [x] (die Semantik würde ich nicht call-by-need machen, weil das technisch aufwändig ist. Stattdessen call-by-name)
* [ ] Figure 1 gibt es mehrmals (S.10); die wird auch nicht im Text erwähnt
* [x] Beim Evaluation Context musst du dich entscheiden, ob er rekursiv sein soll oder nicht. Wenn nicht, muss die erste Alternative \Box weg. Ansonsten musst du alle anderen Boxen durch E ersetzen.
* [ ] R-Final verstehe ich nicht. Woher kommt das Some (k e)?
* [ ] was machen unfold und indirects? —> Hab's gefunden! Beispiele wären hilfreich.
* [ ] wieso explizite Indizes in R-Let-In und sonst nicht?
* [ ] woher das t und das i in R-Lookup?
* [ ] die Beschreibung der Auswertung von ${t} ist komisch. Laut Syntax in Listing 1 ist zulässig
  t.l, t.i, und t.s  (also Label, Interpolation, Stringkonstante)
  Dann hätte ich gedacht, dass E ::= ... | {\alpha}.${ E } ein evaluation context ist, der verlangt, dass
  erst links vom Punkt das Record ausgewertet sein muss. Dadurch wird Lookup-DYn-Step überflüssig.
  Andererseits kann ja auch in s interpoliert werden,
  d.h. dafür muss es auch Regeln geben. Die sehe ich nicht und die Semantik macht keinen Unterschied
  zwischen s mit bzw ohne Interpolanten.
* [ ] bei Has-Path-Pos/Neg fehlt was. Soll das t_i sein?
* [ ] R-Import p verstehe ich nicht. p ist doch ein Pattern?!
