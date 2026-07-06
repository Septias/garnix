./26-07-06.typ


== Fragen
- Wie transportiere ich die Constraints nach oben?
  - Durch extended context in constraints
  - => deshalb auch context-weakening
  - Oder sollte ich ein neues Statement hinzufügen, dass hinten die constraints sammelt? (Γ ⊢ e: τ -> Ξ)
    - Das würde halt auch
- Wo spezialisiere ich Typen?
  - Bsp: records
    - (e: e.l): ★
    - (e: e.l){l: τ} -> {l: τ}.l -> τ
  - Bsp: row-var (durch concat)
    - (e: ({l: τ} ‖ e).l): ★
    - (e: { α | l: τ }.l): ★
    - (e: e.l){} -> {l: τ}.l -> τ
- Warum ist der preservation proof noop?
