//! Conservative direct-read Cartesian partition issuance at the phase boundary.

use rumoca_ir_solve::{CartesianPairPlan, ContinuousSolveSystem, direct_state_derivative_reads};

pub(super) fn issue(system: &ContinuousSolveSystem, states: usize) -> Option<CartesianPairPlan> {
    if !system.manifold_residual.is_empty() || !system.reduced_chart_set.charts.is_empty() {
        return None;
    }
    let reads = direct_state_derivative_reads(&system.derivative_rhs, states)?;
    let pairs = reads
        .iter()
        .enumerate()
        .filter_map(|(position, &velocity)| {
            let velocity = velocity?;
            // A cycle/chain of direct reads has no unique second-order partition.
            (reads[velocity].is_none()).then_some([position, velocity])
        })
        .collect();
    CartesianPairPlan::checked(states, pairs, &system.derivative_rhs).ok()
}
