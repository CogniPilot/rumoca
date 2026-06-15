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
| `rumoca-solver-diffsol` | Implicit backends via diffsol (`bdf`, `esdirk34`, `trbdf2`) |
| `rumoca-input`, `rumoca-input-keyboard`, `rumoca-input-gamepad` | Interactive input devices |
| `rumoca-signal-frame` | Signal payload types |
| `rumoca-transport-udp`, `rumoca-transport-websocket` | External coupling and viewer transport |
| `rumoca-viz-web` | Browser viewer assets |

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
