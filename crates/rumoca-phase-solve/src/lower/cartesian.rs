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

pub(super) fn mechanical(
    system: &ContinuousSolveSystem,
) -> Option<rumoca_ir_solve::MechanicalStepPlan> {
    let cartesian = system.cartesian_pairs.as_ref()?;
    let mut nodes = system
        .derivative_rhs
        .nodes
        .iter()
        .enumerate()
        .filter_map(|(index, node)| {
            matches!(node, rumoca_ir_solve::ComputeNode::LinSolve { .. }).then_some(index)
        });
    let index = nodes.next()?;
    if nodes.next().is_some() {
        return None;
    }
    rumoca_ir_solve::MechanicalStepPlan::checked(cartesian.clone(), index, &system.derivative_rhs)
        .ok()
}
