# rumoca-sim-core

Solver-agnostic simulation runtime core and backend adapter contract.

## Role in Rumoca
Owns shared simulation behavior: DAE preparation, IC solve orchestration, runtime event scheduling, timeout/diagnostic handling, and result assembly.

## Public Surface
- Backend contract: `SimulationBackend` (runtime orchestration interface).
- Runtime driver: `run_with_runtime_schedule`.
- Shared preparation/runtime APIs used by backends: `prepare_dae`, `solve_initial_conditions`, `finalize_dynamic_result`, event/startup helpers.
- Shared option/result/error types: `SimOptions`, `SimResult`, `SimError`, `SimSolverMode`.

## Inputs
- Canonical DAE model, simulation options, and a backend implementation.

## Outputs
- Prepared backend inputs and unified simulation results/errors.

## Design Constraints
- Keep solver-specific loops out of core.
- Maintain one clear backend contract for new solver adapters.
- Centralize runtime semantics so backends stay behaviorally aligned.
