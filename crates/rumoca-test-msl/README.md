# rumoca-test-msl

MSL regression and parity harness for Rumoca compile/balance/simulation validation.

## Role in Rumoca
Runs large-scale Modelica Standard Library checks and parity comparisons used to track compiler/simulator quality.

## Public Surface
- Developer binary: `rumoca-sim-worker` (parallel simulation worker used by the harness).
- Test harness entry points via crate tests (compile/balance/sim target sweeps and parity reporting).

## Inputs
- MSL target sets, optional filters, and optional reference outputs.

## Outputs
- Regression pass/fail status and per-model parity artifacts.

## Design Constraints
- Keep target selection reproducible for CI/local runs.
- Keep harness logic separate from compiler/simulator implementation crates.
- Support both quick subsets and full sweeps.
