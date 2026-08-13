# Tier 1 ledger — structured state-event roots

Branch working tree based on `a166e83d`.

## Capability and root cause

Relations inside compact structured equation owners were not lowered to state
event roots.  In particular, each landing-gear contact relation in an arrayed
RDD2 equation loop disappeared from the DAE event contract, even though the
same scalar relation produced a root outside a loop.  That was a false-success
counterexample: the compiler accepted a model while omitting behavior required
by its source semantics.

The DAE IR now owns compact structured-root families with a checked domain and
relational expression.  Solve lowering derives scalar roots in canonical domain
order.  Compile-time relations in parameter comprehensions remain compile-time
expressions and do not become event roots; continuous relations derive their
owner from the expression's checked binder domain rather than map iteration
order.

## Focused evidence

`structured_event_roots` checks that a four-element loop remains one compact
DAE root family and derives four ordered scalar Solve roots.  The RDD2 plant
produces three compact families and twelve touchdown roots, and its two-second
simulation completes.  `NeuralODETensor` retains relational parameter
comprehensions without leaking their binder or manufacturing runtime roots.

The focused DAE-IR, DAE-construction, structured-event, and tensor solve tests
all pass.  Both Rumoca and OMC also compile the RDD2 and CubS2 downstream model
suites.

## Tier 1 canary delta

The fixed `dev/msl-canary-20.json` tripwire was rerun with the accepted ten-
second per-phase budget.  The before artifact is
`target/msl/canary-trace-evidence-10s`; the after artifact is
`target/msl/canary-structured-event-roots`.  This focused partial run is not a
cohort parity claim.

| Metric | Before (`a166e83d`, dirty) | After (`a166e83d`, dirty) |
|---|---:|---:|
| parsed / flattened | 20 / 20 | 20 / 20 |
| reached DAE / solve | 11 / 11 | 11 / 11 |
| `sim_ok` completion | 8 / 20 | 8 / 20 |
| OMC traces compared | 8 | 8 |
| strict-high | 8 | 8 |
| near / deviation | 0 / 0 | 0 / 0 |
| missing / skipped | 0 / 0 | 0 / 0 |

The canary has no actionable trace counterexample.  Nine models stop at typed
DAE-construction failures and three stop at typed Solve-IR
structural/computability failures; none is represented as a successful
simulation.  The unchanged canary outcome is evidence against a regression,
not evidence that this canary exercises the newly supported RDD2 contact loop.
