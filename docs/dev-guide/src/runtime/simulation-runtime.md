# Simulation Runtime

The simulation runtime turns a compiled model into trajectories. It is
organized around shared solver interfaces: generic simulation policy lives
in the shared runtime layer, and solver backends are thin adapters.

## Layering

| Crate | Role |
|---|---|
| `rumoca-sim` | Simulation orchestration over the compiled model |
| `rumoca-solver` | Shared solver API, result types, report payloads |
| `rumoca-solver-rk45` | Explicit Runge–Kutta-style backend (`rk-like`) |
| `rumoca-solver-diffsol` | Implicit backend via diffsol (`bdf`) |
| `rumoca-input`, `rumoca-input-keyboard`, `rumoca-input-gamepad` | Interactive input devices |
| `rumoca-signal-frame` | Signal payload types |
| `rumoca-transport-udp`, `rumoca-transport-websocket` | External coupling and viewer transport |
| `rumoca-web` | Browser viewer assets |

## Shared Responsibilities

These belong in the shared runtime/solver API layer, never duplicated in a
backend:

- event schedules and root handling (zero-crossing location, event
  iteration, `__pre__.*` slot updates at event entry),
- input routing and zero-order-hold behavior,
- result collection and the report payload,
- termination (`terminate`) and assertion handling,
- pacing: `as_fast_as_possible`, `realtime`, and `lockstep` modes.

If two solvers need the same code, it moves into the shared layer. Solver
backends implement the integration method and consume resolved input
values — they must not know about keyboards, gamepads, or transports.

## Numerical Backend Options

`SimOptions::backend_options` carries an optional owned `BackendOptions`
value. The concrete configuration belongs to the selected backend crate:

| `SimSolverMode` | `NumericalBackend` | Concrete configuration |
|---|---|---|
| `Bdf` | `Bdf` | `rumoca_solver_diffsol::DiffsolOptions` |
| `RkLike` | `Rk45` | `rumoca_solver_rk45::Rk45Options` |
| `VelImplicit` | `VelImplicit` | `rumoca_solver_velimplicit::VelImplicitConfig` |
| `FixedStep` | `FixedStep` | `rumoca_solver_rk45::FixedStepConfig` |

Leave the payload absent to use the selected backend's defaults, including
normal `Auto` selection. An explicit payload requires an explicit solver;
`Auto` does not distribute custom options among candidate backends. Backend
identity, concrete type, build availability and configuration values are
validated before model work or session-cache reuse. A rejected request
returns a typed `BackendOptionsError` rather than silently choosing another
configuration.

Cloning `BackendOptions` shares ownership of its captured value. The runtime
accepts the backend-owned concrete configuration and derives its cache
identity from that validated value. Absence and an explicitly supplied
default are distinct requests; callers do not supply cache keys.

## Passive Numerical Observation

`simulate_with_numerical_observation` selects the same numerical path as
`simulate_with_diagnostics`. The optional `SettingsCollector` records host
lifecycle facts and settings reported by the selected adapter. Observation
does not choose a solver or replace a failed integration with a different
method.

Requested settings and retained settings are separate records. In
particular, the scalar absolute tolerance and state nominals supplied to a
backend are different facts from its retained per-state tolerance vector.
An initial-step setting describes a configured hint or starting value, not
the first step an adaptive method actually accepts.

Readback distinguishes `Known`, `NotApplicable` and `Unsupported`.
Unsupported readback or exhausted diagnostic storage makes the snapshot
incomplete; it does not change the numerical result or manufacture an
effective setting. Auto capability probes, preparation checks and the
selected run also have distinct construction labels.

Both facades return `SimulationDiagnosticError`. Backend-option admission
errors remain typed, and simulation failures retain their `SimError` and
its existing typed cause when present. Use the error source chain when the
cause or ME stage matters; formatted text alone is not that contract.

## Events at Runtime

The DAE hands the runtime explicit metadata: relations (event condition
surfaces), scheduled events (`sample`), event actions (`assert`,
`terminate`), and the `pre`-slot bindings. The runtime's job is mechanical:
detect or schedule the event, advance to the event instant, write pre
slots, apply discrete updates, and restart integration. No backend
rediscovers event structure from expressions.

## Failure Diagnostics

When a simulation produces a non-finite value, the runtime re-runs with NaN
tracing to name the offending variables — the user-facing behavior
documented in the handbook's troubleshooting chapter. Keep that path
working when touching solver internals; it is the difference between a
useful bug report and "solver error".
