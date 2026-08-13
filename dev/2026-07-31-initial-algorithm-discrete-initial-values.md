# Tier 1 ledger — MLS §8.6 discrete initial values from `initial algorithm`

Branch `msl-trace-parity-50`, baseline commit `cdff8c39` (uncommitted working tree,
shared with concurrent agents).

## Capability

`ED013` previously rejected every initial-algorithm target whose planned role was
not `Parameter`. A scalar `DiscreteReal`/`DiscreteValue` target now becomes an
initialization-partition definition of the coordinate's initialization-instant
value, owned by `rumoca-ir-dae`
`InitializationEquations::discrete_{real,value}_initial_value` and lowered by
`rumoca-phase-solve` to `InitializationSolveSystem::update_rhs/update_targets`
for the coordinate *and* its `pre` slot (MLS §8.6 `pre(v) = v` at that instant).

Unblocks the `T_start`/`count` shape shared by
`Modelica.Blocks.Sources.Pulse`, `SawTooth`, and `Trapezoid`.

## Acceptance contract (SPEC_0008 §Acceptance Contract Before Rejection)

Legal, and the owner that handles each:

| Shape | Owner | Evidence |
|---|---|---|
| `parameter` `fixed = false`, unbound, scalar | calculated-parameter binding (unchanged) | `rumoca/tests/initial_algorithm_test.rs::deferred_parameters_are_determined_by_their_initial_algorithm` |
| `assert` statement, guards folded | assertion owner (unchanged) | same file |
| scalar `DiscreteReal`/`DiscreteValue` target, value reads only `time`/parameters/constants | `dae::InitializationEquations::discrete_*_initial_value` → `solve::InitializationSolveSystem::update_*` | `initial_algorithm_test::a_discrete_target_is_determined_by_its_initial_algorithm`, `::a_determined_discrete_value_is_the_pre_value_the_first_event_is_scheduled_from`, `rumoca-phase-solve::tests::discrete_initial_value_becomes_an_initialization_update_of_the_coordinate_and_its_pre_slot` |
| the coordinate's `pre` slot at that instant | same owner, second update row | same phase-solve test |
| GALEC block whose discrete coordinates start from `start` attributes only | GALEC Startup (unchanged) | `rumoca-phase-galec::admissibility::tests::checked_discrete_initial_value_is_never_ignored` (negative side) |

Rejected, each naming the absent owner: state/algebraic/output/input targets
(solved from residual rows), array-valued discrete targets, a discrete target
that also has a declaration binding, two initial algorithms determining one
coordinate, a value reading anything unsettled at the initialization instant,
a value built from an MLS §12.3 `impure` call (the runtime applies the update
until it stops changing, which such a call never does), and — new code
`EGT022` — a GALEC projection of a model carrying such a definition.

Consumers that had to learn the owner rather than drop it silently:
`rumoca-phase-structural::dae_transform` replays every definition through the
same checked constructor (a reduction would otherwise leave the coordinate at
its declared `start`), and the `rumoca-phase-codegen` DAE template context
exposes them as their own `initialization.discrete_values` column.

## Tier 1 canary delta (`dev/msl-canary-20.json`, one 10 s attempt per model)

| Metric | Before (certified baseline) | After |
|---|---|---|
| canary `sim_ok` | 6 / 20 | 6 / 20 |
| canary compiled | — | 8 / 20 |
| parity | unmeasured (comparator did not run) | unmeasured (comparator did not run) |

No canary regression. The canary member `Modelica.Electrical.Analog.Examples.OpAmps.Comparator`
moved off the `T_start` rejection and now stops at a different owner
(`ED020` expression shape mismatch on `min(1, max(0, rInt))` in
`Electrical/Analog/Basic/Potentiometer.mo:47`), so it is still `not-simulated`.

## Named cohort models, CLI before/after (`--t-end 0.1`)

Before: every one stopped at
`ED013 … initial algorithm target 'vIn.signalSource.T_start' has role DiscreteReal`
(directly observed on `Comparator`; the remaining models instantiate the same
`Pulse`/`SawTooth`/`Trapezoid` blocks and each now emits exactly two
`initial_discrete_values` in its DAE, which is the previously rejected shape).

| Model | After |
|---|---|
| `Modelica.Electrical.Analog.Examples.OpAmps.LowPass` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.OpAmps.HighPass` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.OpAmps.Integrator` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.OpAmps.VoltageFollower` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.OpAmps.InvertingAmplifier` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.OpAmps.NonInvertingAmplifier` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.InvertingAmp` | `SIM_OK` |
| `Modelica.Electrical.Analog.Examples.OpAmps.Comparator` | `ED020` (different owner, see above) |
| `Modelica.Electrical.Analog.Examples.OpAmps.Differentiator` | `EX001` (solver) |

`sim_ok` is completion, never parity — no parity number is claimed here.

## SPEC_0021 file size

`rumoca-phase-solve/src/lower.rs` and `rumoca-phase-dae/src/construction.rs`
were within 50 lines of the 2,000-line hard threshold, so the new lowering and
its tests live in `lower/initial_discrete.rs`,
`construction/initial_discrete_values.rs`, and
`phase-solve/src/tests/initial_discrete_values.rs`. Both files end this change
below the threshold (1,958 and 1,967 lines, from 1,950 and 1,960).

## Pre-existing defect found while verifying (not caused by this change)

A `when time >= (pre(count) + 1)*period + startTime` counter stops advancing
once the model also carries a root over a discrete coordinate
(`y = if time < T_start + T_width then …`). Reproduced with **declared `start`
attributes and no initial algorithm at all**:

```modelica
model P
  parameter Real period = 0.1;
  parameter Real startTime = 0;
  Real y;
protected
  Real T_start(start = 0);
  Integer count(start = 0);
equation
  when time >= (pre(count) + 1)*period + startTime then
    count = pre(count) + 1;
    T_start = time;
  end when;
  y = if time < T_start + 0.05 then 1 else 0;
end P;
```

`count` advances 0 → 1 → 2 and then freezes. Without the root over `T_start`
the same counter advances for the whole run. This is the dynamic-time-event /
state-root interaction, in solver/driver territory, and it bounds the trace
quality of the whole `Pulse`/`SawTooth`/`Trapezoid` cohort.
