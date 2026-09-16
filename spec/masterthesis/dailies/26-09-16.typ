
== Claude Prompts
- [ ] The success leg can be vacuous, tell me how "bad" that is and propose a fix.
- [~] What is the unification type of row equivalence under asymmetric concatenation? Unitary, finitary, infinitary, or nullary?
- [x] What is the expandR driver arm?
- [~] Try to prove `occurs => ¬no unifier` (use the plan for it)
- [ ] What is the best naming-strateg for real? Should we make architectural changes to switch to deBrujin for example?


== Occurs ⟹ ¬mgu — what came out
Stages 1-4 of plans/occurs-complete-plan.md are done, Stage 5 (docs, fuzz) with
them. The plan held up: the missing artefact really was an algorithm arm, not a
theorem.

- The all-variable occurrence is NOT a failure. `allvar_occurs_mgu`: field-free
  spine, α on it ⟹ there is an mgu, at any spine and any multiplicity. The two
  readings of the ≈-characterization turn out to be the SAME arithmetic
  `m(θα) = k·m(θα) + Σ_{γ≠α} m(θγ)` — once at |varseq|, once at every l-count.
  Needed the exact field-count law; the old one was only ≤.
- k = 1 leaves α genuinely free (ε | α | ε ≈ α), k ≥ 2 collapses α too. Both
  forced, so both mgus. U-var-collapse is now a rule in algorithmic.typ.
- bindTy's guard is sorted at last (`Ty.tyVars`). Cost: nothing — the five
  proofs touching that branch split and never read the condition.
- occurs is SOUND where the guard is local, at both sorts. The case analysis
  closes: deep (depth) or field-pinned (counting), and the third shape is
  solved, not rejected.
- `rcdDepth` could not carry the type sort — an arrow does not raise it, so
  x ≐ (x→x) is invisible to it. `tyDepth` counts the arrow too; same
  ≈-invariance argument (max on cat).


== Was noch offen ist
- Der Driver-Level-Satz `unifyRowM … = .occurs ⟹ ¬∃θ` geht NICHT durch, und
  zwar aus einem Grund, den der Plan nicht kannte: seit dem 14. liest der Guard
  `depReach Θ`, feuert also auch, wenn α nur über die akkumulierten Expansionen
  erreichbar ist. Das ist eine Aussage über den Solver-Zustand, nicht über das
  Problem — daraus folgt kein No-Unifier-Satz. Blockiert genau dort und sonst
  nirgends; bei Θ = [] (`depReach [] V = V`) gilt der Satz unbedingt.
- Ich habe die ~280-Zeilen-Induktion à la unifyM_clash_no_unifier deshalb NICHT
  geschrieben. Sie könnte nicht schließen.
- Entweder: den Guard in zwei Verdicts aufspalten (echter Zyklus vs. stale
  binding), dann ist der lokale Teil liftbar. Oder: den Satz relativ zu einem
  wohlgeformten Θ formulieren und die Solver-Zustands-Aussage separat führen.
  Das ist die nächste Entscheidung.


== Fallout
- Zwei `:= rfl`-Regressionen sind umgekippt und ersetzt: occurs_allVar_reported
  → allVar_collapse_reported (+ k≥2), tyM_occurs_cross_sort →
  tyM_cross_sort_success (+ tyM_occurs_fn/_field).
- Jede "der occurs-Guard ist bewusst konservativ"-Notiz war falsch und ist raus.
  `.stuck` steht damit allein — das ist die bessere Geschichte für den
  Metatheorie-Abschnitt: die drei Verdicts sind NICHT gleich konservativ, und
  die Asymmetrie ist strukturell.
- Fuzz, alle drei Universen: ill-formed 0, spine-cyclic 0, unrankable 0.
  occurs geht 8688→6396 (wide), 119176→25192 (deep).


== Stand der anderen Tasks (Branch-Rundgang)
Alle fünf Prompts sind angefasst worden — aber auf vier verschiedenen Branches.

- UNIFICATION TYPE — BEANTWORTET. worktree-row-unification-type, 55e03cf, neue
  Datei RowUnify/UnifType.lean. Ergebnis: MINDESTENS INFINITÄR. Nicht unitär
  (schon durch wand_no_mgu), und jetzt auch nicht finitär — das Shift-Problem
  (α | l:𝓫) ≐ᵣ (l:𝓫 | α) ist lösbar, zwingt jeden Unifier dazu α
  spine-var-frei zu machen, und die Familie α ≔ (l:𝓫)^k ist paarweise
  unvergleichbar ⟹ keine endliche complete set.
  NICHT entschieden: infinitär vs. nullär. Die Antikette ist selbst nicht
  complete (α ≔ (m:𝓫), m ≠ l, wird von keinem Mitglied gedeckt). Dafür müsste
  die trace-indizierte Kandidatenmenge als complete gezeigt werden. Die Datei
  warnt ausdrücklich davor, sie für diese Hälfte zu zitieren.
  → Für die Thesis ist das eine UNTERE SCHRANKE AUF DEN ALGORITHMUS: jedes ≐ᵣ,
    das eine oder endlich viele Lösungen zurückgibt, ist auf einem lösbaren
    Problem unvollständig. Die Unvollständigkeit des stuck-Legs ist damit keine
    Lücke, die man schließen könnte.

- EXPANDR DRIVER ARM — war schon am 14. fertig (506bf95, auf main). Stand
  heute früh nur noch als Karteileiche auf der Liste.

- OCCURS ⟹ ¬MGU — heute, dieser Branch. Siehe oben.

- SUCCESS LEG VACUOUS — stark eingegrenzt, NICHT beantwortet. Das ist die
  UnifyWF-Frage. main hat seit dem 14. zweimal nachgelegt: 3b36ee1 (die beiden
  algebraischen Schritte) und 278e947, das DREI weitere Invarianten-Kandidaten
  per Sweep widerlegt (40/2336/24, 40/2160/24, 0/176/0) — zusätzlich zu den drei
  Rank-Kandidaten davor. Fazit dort: der Rank ist echt TOPOLOGISCH. Listen-
  ordnung scheitert, weil Sol.comp frühe Bindings durch spätere Lösungen
  durchschiebt, ein früh erzeugtes Binding also von einer später erzeugten
  Variable abhängen kann. Empirisch weiter 0/0/0, weiter unbewiesen.
  Die eigentlich gefragte Antwort ("wie schlimm ist es, Fix vorschlagen") hat
  bisher niemand geschrieben.

- NAMING / DEBRUJIN — die FALSCHE Hälfte ist beantwortet. b47a059 ist ein
  mechanisches Umbenennen der LEAN-BEZEICHNER (unifyRowM → unifyRow,
  solveVarM → solveVar, UResM → URes, unifyM_* → unifyStep_*). Die Frage im
  Daily zielt auf die VARIABLENREPRÄSENTATION. Dazu nichts Neues; es gilt
  weiter "Kein Debrujin" vom 26-07-13, begründet damit, dass der
  Type-Safety-Beweis extrinsisch ist.


== Kollisionen — vor dem nächsten Merge lesen
- DER RENAME vs. HEUTE. b47a059 benennt solveVarM → solveVar und UResM → URes.
  Beide Commits von heute hängen an solveVarM (neuer Arm in der Definition,
  solveVarM_occurs_no_unifier, solveVarM_occurs_inv). Mechanisch auflösbar,
  aber die Richtung muss bewusst gewählt werden: den Rename NACH dieser Arbeit
  zu mergen ist deutlich billiger als umgekehrt.
- State.lean:895 ist doppelt Konfliktzone. Auf diesem Branch zählt die
  arm-by-arm-Liste für UnifyAcyclic solveVarM noch mit ZWEI Zweigen auf ("die
  occurs-Guard gibt schon α ∉ allRowVars") — der ε-collapse-Arm fehlt darin.
  Sachlich unkritisch (er bindet auf ε, sVarSeq ist leer, also trivial
  acyclic), aber die Liste ist unvollständig. Auf main hat 278e947 genau diese
  Passage neu geschrieben und die Liste ganz entfernt.
- Dieser Branch sitzt auf a605042; main ist weiter (27f8ad6, fdbc8df, 278e947).
  Mein proof-state.md-Umbau am occurs-Outcome kollidiert mit mains
  "[¡] occurs: *incomplete*".
- worktree-infer-sound läuft parallel an InferSound (bf258b2, 365d2a4, 9e85aa0,
  f3f593f) und hat dabei zwei Defekte gefunden, die uns angehen: F-★ hat keine
  Lookup-Prämisse (finalize_star_no_discharge), und QScheme trägt keine
  Wohlgeformtheit auf seinen Constraints (QScheme.ResWF, gestellt, nicht
  bewiesen — dieselbe Familie wie UnifyWF).
