    THE DEPGRAPH ROUTE IS CLOSED (2026-09-19), and so is the whole rank-by-order
    family. Measured with a TRACED CLONE of the driver in Fuzz.lean ([6]/[7]) —
    `Θ` is an input to `unifyTyF`/`unifySpineMF` and never returned, so it had to
    be cloned to be observed; the clone's verdict is compared against the real
    driver on every pair (`nDis` = 0 / 0 / 0, so the numbers below stand).
      * Θ AS THE CARRIER: ✘ twice over.
        - Θ is itself CYCLIC on 316 / 3232 / 88 successes. The cycle is spurious
          and cross-sort: `expandDeps` emits `(δ, τ.ftv)` and `DepGraph` is
          UNTAGGED, so on (l:a | a) ≐ᵣ (a | m:𝓫) it records a→[aa aaa], aa→[a]
          where the first `a` is a ROW variable and the second a TYPE variable.
        - Θ is INCOMPLETE: it records expansions only, so U-var-solve's bindings
          have no edges at all and Θ-reachability MISSES 40 / 2736 / 24 real
          solution edges — first at (l:𝓫 | b) ≐ᵣ (m:{a} | a), which needs ᵣb→ᵣa.
        - consequently Θ-depth as the rank fails 332 / 4700 / 96.
        Tagging Θ and recording U-var-solve in it would make Θ *equal* to the
        solution's dependency graph — at which point "Θ is acyclic" IS
        `Sol.Ranked`, so that repair renames the problem rather than reducing it.
      * CREATION ORDER: ✘, and this kills the family. The clone also records a
        LEDGER — every key bound, tagged, in the order bound — which is what
        `domS` is not (`domS` = ty ++ row scrambles two sorts the driver
        interleaves, so the earlier index candidates were confounded). Both
        directions, and both intra-expansion orders, fail:
          payload δ before host β:  decreasing  16 / 15036 /  16   ✘
          host β before payload δ:  increasing  40 /  2720 /  24   ✘
        because the two edge sources point OPPOSITE WAYS in time. An expansion's
        host binding mentions the variables it just invented (forward); a
        U-var-solve binding mentions variables already there (backward); and
        `comp`'s push makes an old binding acquire a new dependency. All three
        occur in one solution at (b | a) ≐ᵣ (l:{a} | l:{a}):
          [aa≔{ε}, aaaa≔{ε} ; b≔l:aa | l:aaaa | ε, aaa≔l:aaaa | ε, aaaaa≔ε, a≔ε]
          ledger ₜaa ᵣb ₜaaaa ᵣaaa ᵣaaaaa ᵣa — ᵣb→ₜaa points back, ᵣb→ₜaaaa forward.
        So no rank read off any timeline can work. Do not retry one.
      * THE `Sol.comp` PRESERVATION STEP IS FALSE AS STATED. The lemma the entry
        above asks for — the later stage mentions nothing of the earlier stage's
        domain, so the two ranks stack — was measured at every `.seq` and every
        expansion: 388 / 11756 / 132 crossing mentions. And the cause is not
        capture, it is that THE DRIVER RE-BINDS A KEY IT ALREADY BOUND.
