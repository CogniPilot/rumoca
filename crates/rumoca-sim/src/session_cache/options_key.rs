//! Exact preparation inputs, without source/dependency invalidation policy.

use crate::{DiffsolMethod, SimExecutionPolicy, SimOptions, SimPacingMode, SimSolverMode};

#[derive(PartialEq, Eq)]
pub(super) struct OptionsKey {
    time_and_tolerances: [u64; 4],
    dt: Option<u64>,
    max_wall_seconds: Option<u64>,
    scalarize: bool,
    solver_mode: SimSolverMode,
    diffsol_method: DiffsolMethod,
    pacing_mode: SimPacingMode,
    parameters: Vec<(String, u64)>,
    starts: Vec<(String, u64)>,
    inputs: Vec<(String, u64)>,
    execution_policy: SimExecutionPolicy,
    checkpoint_budget_bytes: u64,
}

impl OptionsKey {
    pub(super) fn new(options: &SimOptions) -> Self {
        // Exhaustive: adding a new simulation option requires deciding how it
        // participates, rather than silently reusing an incompatible instance.
        let SimOptions {
            t_start,
            t_end,
            rtol,
            atol,
            dt,
            scalarize,
            max_wall_seconds,
            solver_mode,
            diffsol_method,
            pacing_mode,
            param_overrides,
            start_overrides,
            initial_inputs,
            execution_policy,
            checkpoint_budget_bytes,
        } = options;
        Self {
            time_and_tolerances: [
                t_start.to_bits(),
                t_end.to_bits(),
                rtol.to_bits(),
                atol.to_bits(),
            ],
            dt: dt.map(f64::to_bits),
            max_wall_seconds: max_wall_seconds.map(f64::to_bits),
            scalarize: *scalarize,
            solver_mode: *solver_mode,
            diffsol_method: *diffsol_method,
            pacing_mode: *pacing_mode,
            parameters: value_bits(param_overrides),
            starts: value_bits(start_overrides),
            inputs: value_bits(initial_inputs),
            execution_policy: *execution_policy,
            checkpoint_budget_bytes: *checkpoint_budget_bytes,
        }
    }
}

fn value_bits(values: &[(String, f64)]) -> Vec<(String, u64)> {
    values
        .iter()
        .map(|(name, value)| (name.clone(), value.to_bits()))
        .collect()
}
