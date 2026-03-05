# rumoca-sim-diffsol

Diffsol-backed DAE simulation backend for Rumoca.

## Role in Rumoca
Thin adapter from `rumoca-sim-core` runtime contracts to Diffsol solver APIs.

## Public Surface
- Main backend API: `simulate(&Dae, &SimOptions) -> Result<SimResult, SimError>`.
- Re-exported shared simulation types for consumers: `SimOptions`, `SimResult`, `SimError`, `SimSolverMode`, `SimVariableMeta`.
- Public `problem` module for test/integration helpers that diffsol-specific tests import.

## Inputs
- Prepared DAE model and options from simulation core.
- Mass-matrix-compatible system form accepted by this backend.

## Outputs
- Time-series simulation results.
- Backend-specific solver failures mapped to shared `SimError`.

## Design Constraints
- Keep shared runtime semantics in `rumoca-sim-core`.
- Reject unsupported mass-matrix forms instead of attempting incorrect solves.
- Keep adapter implementation compact (`lib.rs`, `integration.rs`).
