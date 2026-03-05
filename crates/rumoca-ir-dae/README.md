# rumoca-ir-dae

Canonical hybrid DAE IR for Rumoca solver and codegen consumers.

## Role in Rumoca
Defines the solver-facing DAE contract (continuous/discrete equations and variable partitions) produced by DAE lowering and consumed by simulation/codegen crates.

## Public Surface
- Primary model type: `Dae`.
- Core equation/variable types: `Equation`, `Variable`, `Algorithm`, `WhenClause`, `VariableKind`.
- Re-exported expression surface used by downstream crates (via `flat` namespace and top-level re-exports).

## Inputs
- Flat-model lowering output from `rumoca-phase-dae`.

## Outputs
- Canonical DAE structures for simulation and target generation.

## Design Constraints
- Keep IR lean and solver-agnostic.
- Preserve explicit variable classification and equation partition semantics.
- Avoid embedding runtime/solver algorithms in the IR crate.
