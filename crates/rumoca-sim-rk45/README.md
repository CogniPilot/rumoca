# rumoca-sim-rk45

Explicit RK45 simulation backend for causal ODE subsets.

## Role in Rumoca
Minimal exemplar backend showing how to plug a new solver into `rumoca-sim-core`.

## Public Surface
- Main backend API: `simulate(&Dae, &SimOptions) -> Result<SimResult, SimError>`.
- Re-exported shared simulation types: `SimOptions`, `SimResult`, `SimError`, `SimSolverMode`, `SimVariableMeta`.

## Inputs
- Prepared model extractable to causal form `x_dot = f(x, u, p, t)`.
- Simulation options and runtime schedule callbacks.

## Outputs
- Time-series simulation results for accepted causal models.
- Explicit rejection errors for non-causal/non-explicit models.

## Design Constraints
- Stay thin and contract-driven.
- No dependency on Diffsol.
- Fail fast when causal extraction is not possible.
