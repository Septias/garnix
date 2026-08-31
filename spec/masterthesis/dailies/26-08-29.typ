
./26-08-28.typ
./26-08-30.typ

== Misc
Im Endefekt hängen wir wieder an dem Wand-Beispiel fest. Ich glaube meine delayed stumps haben einen leichten Effekt darauf, lösen das Problem aber nicht maßgeblich. Das war eigentlich auch eine Contribution, auf die ich gehofft hatte. Also, dass man da zumindest einen kleinen Fortschritt in die Richtung macht. Es ist aber glaube ich noch nicht bewiese, dass das wirklich unlösbar ist…

== Claude Prompts
- Go over RowUnify.lean and explain what is done in the file by explaining what the current proofs and upcoming continuations are and what problems we are facing. There is quite a bit of boilerplate, explain why all this machinery is needed. Try to create a bottom to top reasoning by showing which lemmas feed into which proofs that finally want to show principality and correctness. You can be thorough and critique the current design.
- Okay, to get a clearer picture: The current developments are driven by examples. Go through the *.lean files and collect the mechanized examples and put them into a new file in a style similar to algorithmic.typ. A unicode version underlined by simple dashes and followed by an explanation on why this example is important. You can also collect examples from the files minimal.typ and algorithmic.typ.
