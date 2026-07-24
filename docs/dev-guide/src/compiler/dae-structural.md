# DAE Lowering and Structural Analysis

## DAE Lowering (`rumoca-phase-dae`)

DAE lowering eliminates the Modelica-specific surface and produces the MLS
Appendix B canonical system (see [The IR Graph](./irs.md)). The
transformations with the most moving parts:

- **`pre()` elimination.** Every `pre(x)` becomes an explicit `__pre__.*`
  parameter slot; the runtime writes those slots at event entry. This must
  hold in *all* partitions (`f_x`, `f_z`, `f_m`, `f_c`) — `pre` exists only
  in AST and Flat.
- **`when`/`reinit` lowering.** When-clauses become event conditions plus
  guarded discrete update equations; `reinit(x, e)` becomes a guarded state
  update over current/pre slots.
- **Relation extraction.** Every relational expression that can generate an
  event gets a relation/condition variable; `conditions.relations` is the
  single owner of that surface. Synthetic numeric roots (from `abs`,
  `sign`) live separately in `events.synthetic_root_conditions`.
- **`sample`/clocks.** Scheduled events are explicit DAE metadata, not
  expressions the runtime has to discover.
- **`assert`/`terminate`.** Lowered to guarded `events.event_actions` with
  source spans, keeping the compute graphs pure.

A positive validation gate (`appendix_b_validation`) rejects any surviving
source temporal operator — the contract is enforced, not assumed.

## Structural Analysis (`rumoca-phase-structural`)

Between the DAE and a runnable system sits structural preparation:

1. **Matching.** Assign each equation to the unknown it will compute
   (maximum bipartite matching). Failure here is the *structurally
   singular system* diagnostic users see, which names unmatched equations
   and unknowns.
2. **Sorting (BLT).** Order the matched system into block lower-triangular
   form: a sequence of scalar assignments and strongly connected components
   (simultaneous blocks).
3. **Tearing.** Within coupled blocks, choose tearing variables so a small
   nonlinear core is iterated while the rest is evaluated explicitly.
4. **Index reduction.** Higher-index DAEs are reduced with the
   Mattsson–Söderlind dummy-derivative method: constraint equations are
   differentiated and some derivative-defined states are demoted to dummy
   variables. State selection decides which candidates remain integrator
   states.
5. **Scalarization** (when a backend requires it) expands symbolic arrays
   using shape metadata — never by parsing display strings.

The analysis products (matching, BLT blocks, tearing choices, state
selection reports) are returned as separate artifacts — they are inputs to
Solve lowering and to `--inspect structure`, and deliberately *not* stored
in the DAE.

## Watching It Work

```bash
rumoca compile Model.mo --inspect structure   # matching, BLT, SCCs, tearing
rumoca sim Model.mo --inspect eval            # values + derivatives at a point
rumoca sim Model.mo --inspect jacobian --at "x=1@0"
```

These are the same tools the user guide teaches for debugging models — as a
compiler developer you will mostly use them to verify that a lowering or
matching change did what you intended on a specific model.
