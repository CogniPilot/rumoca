# rumoca-sim-diffsol

`rumoca-sim-diffsol` is the Diffsol-backed simulator for Rumoca.

It executes lowered `rumoca-ir-dae::Dae` models and returns sampled traces for
states/algebraics/outputs.

## Public API

Main entrypoint:

- `simulate(dae: &Dae, opts: &SimOptions) -> Result<SimResult, SimError>`

Core API types:

- `SimOptions`
  - `t_start`, `t_end`
  - `rtol`, `atol`
  - `dt` (optional output/sample interval)
  - `scalarize` (flatten array equations for solving)
  - `max_wall_seconds` (per-model timeout budget)
  - `solver_mode` (`Auto`, `Bdf`, `RkLike`)
- `SimResult`
  - `times: Vec<f64>`
  - `names: Vec<String>`
  - `data: Vec<Vec<f64>>`
  - `n_states: usize`
- `SimError`
  - includes model-shape errors, unsupported-function errors, solver failures,
    and timeout errors.

## Architecture

This crate is a backend adapter layer:

- `rumoca-ir-dae`: canonical simulation IR
- `rumoca-sim-core`: solver-agnostic runtime semantics and utilities
- `rumoca-sim-diffsol`: Diffsol problem construction and integration loop

Rule of thumb: if logic is not Diffsol-specific, it should live in
`rumoca-sim-core`.

## Module Layout

- `src/lib.rs`: API, orchestration, option/error/result types
- `src/prepare.rs`: DAE preparation pipeline
- `src/problem/`
  - `mod.rs`: Diffsol problem wrapper
  - `core.rs`: variable/state mapping and equation evaluation bridge
  - `init.rs`: initialization and seed logic
- `src/integration/`
  - `mod.rs`: integration driver
  - `solver_state.rs`, `event_settle.rs`, `esdirk34.rs`, `fallback.rs`

## Typical Flow

1. Validate and prepare `Dae`.
2. Build Diffsol problem callbacks.
3. Integrate from `t_start` to `t_end`.
4. Handle events/clock updates through shared runtime semantics.
5. Return sampled traces in `SimResult`.

## Debugging

Useful environment flags while debugging local simulations:

- `RUMOCA_DEBUG=1`
- `RUMOCA_SIM_INTROSPECT=1`
- `RUMOCA_SIM_TRACE=1`

## Development Checks

```bash
cargo test -p rumoca-sim-core
cargo test -p rumoca-sim-diffsol --lib
```
