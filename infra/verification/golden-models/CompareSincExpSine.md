# Golden candidate: `Modelica.Blocks.Examples.CompareSincExpSine`

## Status

**Candidate, verdict REJECT.** One blocker of three is closed. `registry.toml`
remains authoritative and does not yet contain an entry for this model.

This is the first candidate drawn from the Modelica Standard Library rather
than written for the purpose, so unlike `UnitDerivative` it contributes to MSL
trace parity. It is one of the models currently passing at strict-high on this
worktree.

## Source

Defined inside
`target/msl/ModelicaStandardLibrary-4.1.0/Modelica 4.1.0/Blocks/package.mo`:

```modelica
model CompareSincExpSine "Compare sinc and exponential sine signal"
  extends Modelica.Icons.Example;
  Sources.Sinc sinc(f=5);
  Sources.ExpSine expSine1(f=5, damping=5);
  Sources.ExpSine expSine2(f=5, phase=Modelica.Constants.pi/2, damping=5);
  annotation (experiment(StopTime=1.0, Interval=0.0001));
end CompareSincExpSine;
```

## The measured cone

Pinned by `crates/rumoca-test-msl/tests/compare_sinc_exp_sine_cone/mod.rs`,
which passes. These are **measured**, not inferred:

| Quantity | Measured |
|---|---|
| DAE state count | 0 |
| `relation_count` | 3 |
| `root_count` | 3 |
| `condition_count` | 3 |
| `time_event_count` | 0 |
| Declaration inventory | 21 (17 Parameter, 3 Output, 1 Algebraic) |
| The three `y` | role `Output`, causality **`Local`** |
| Solve `state_scalar_count` | 0 |
| Solve `y_scalars` | 4 |
| Y layout | algebraic `sinc.x` at Y0; outputs at Y1, Y2, Y3 |
| `continuous_equations` | 4 |

**This model is not event-free.** Its three `Sources.*` components each carry
`if time < startTime`, and because the crossing instant is rejected when
`startTime = 0`, each becomes a state-relation zero-crossing root. It was
originally selected as the cohort's smallest cone on the basis of its source
text: three components, no `connect` equations, no states. That reasoning was
wrong, and the cone it actually reaches is not a subset of `UnitDerivative`'s
but a different one. Selection by source-level feature count does not predict
the compiler cone, because the library components a trivial example
instantiates need not be trivial.

## What it reaches, and what it does not

Reached: strict parse; resolve, where `extends` targets and imports bind by
`DefId` rather than by name; instantiate, including the `Icons.Example` mixin
and multi-level inheritance merge; flatten, including constant evaluation of
`Modelica.Constants.pi/2`; DAE construction including the expression-event
walk; structural sort; Solve lowering; and a zero-state execution path.

Not reached: connection-set flattening, since the model has no `connect`
equations. Continuous integration, since there are no states.

Entered but degenerate: initialization, which short-circuits on empty blocks
rather than being skipped. The earlier claim that this model "skips the
initialization machinery" is false as stated.

## Open blockers

**B2, zero-state models carry roots nothing locates.** The three state-relation
roots are constructed and lowered into Solve IR, but the zero-state executor
owns no root or event policy, so they are never located. The `if time <
startTime` branch is correct **only** because `startTime = 0` places the
crossing outside the simulated interval, and because the relation is
re-evaluated inline at each sample. Nothing pins that. A zero-state model whose
relation crossed inside the interval would be sampled rather than root-found,
and could silently produce a wrong trace. This is a latent correctness defect,
not a documentation gap, and the repair is a fail-closed construction check
that a zero-state model's crossings are provably degenerate.

**B3, a capability granted over an empty set.** `assess_bdf_capability`
(`crates/rumoca-sim/src/prepared_simulation.rs`) returns
`BdfCapability::Eligible` for a zero-state model without probing the
linearization that does not exist. It names "eligible" where "not applicable"
is the truth. **Deliberately deferred**: the variant is consumed to select a
solver, so introducing a distinct disposition changes a selection path while
MSL parity is fragile, and that trade belongs to the orchestrator rather than
to a low-severity naming fix.

## Closed blockers

**B1, the cone was assumed rather than pinned.** Closed by the measured test
above. Its load-bearing assertion is `root_count == 3` pinned **alongside**
`time_event_count == 0`: a lone zero on time events would pass on a default,
but paired with a nonzero root count, any reclassification of these guards into
time events moves both numbers and fails. Every other zero in that test rides
beside a nonzero that anchors it, so a failed or empty construction cannot
pass.

## Shared cone

Certifying this cone certifies the Blocks signal-source subfamily: the
`Icons.Example` mixin, deep `Blocks.Interfaces` inheritance, constant
evaluation of `Modelica.Constants`, the `if time < startTime` state-relation
pattern, and the zero-state execution path. Cohort siblings sharing it include
`Blocks.Examples.BusUsage`, `DemoSignalCharacteristic`,
`DemonstrateSignalExtrema`, `Modulation`, `SlewRateLimiter` and
`Noise.Densities`.

It does **not** transfer to the `Clocked.Examples.*` models, which are the
majority of the currently-passing cohort and reach clocked, `sample` and `when`
machinery this model never enters. The cohort therefore has at least two
disjoint certification spines, and estimates that assumed one are optimistic.

## Review criteria still to satisfy

Beyond closing B2 and B3: an execution-layer measurement of the zero-state
session path and the withheld backends, which the current test deliberately
does not cover; and confirmation that the strict-high trace parity this model
already achieves rests on reviewed code rather than on the sampling coincidence
described in B2.
