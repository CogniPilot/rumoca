use rumoca_ir_solve as solve;

use super::continuous::{self, ContinuousFixtureParts};
use super::fixture::{ContinuousInventory, ContinuousProblem};

pub(crate) fn derivative_problem(
    inventory: ContinuousInventory,
    derivative_rhs: solve::ComputeBlock,
) -> ContinuousProblem {
    derivative_problem_with_partitions(
        inventory,
        derivative_rhs,
        solve::InitializationSolveSystem::empty(),
        solve::DiscreteSolveSystem::default(),
        solve::SolveEventPartition::default(),
        solve::SolveClockPartition::default(),
    )
}

pub(crate) fn derivative_problem_with_partitions(
    inventory: ContinuousInventory,
    derivative_rhs: solve::ComputeBlock,
    initialization: solve::InitializationSolveSystem,
    discrete: solve::DiscreteSolveSystem,
    events: solve::SolveEventPartition,
    clocks: solve::SolveClockPartition,
) -> ContinuousProblem {
    let continuous = continuous::checked_continuous_system(
        inventory.solve_layout(),
        ContinuousFixtureParts {
            implicit_rhs: solve::ComputeBlock::default(),
            implicit_row_targets: Vec::new(),
            algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
            residual: solve::ComputeBlock::default(),
            manifold_residual: solve::ComputeBlock::default(),
            manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
            derivative_rhs,
        },
        &discrete,
        &events,
        &clocks,
    );
    inventory.seal(continuous, initialization, discrete, events, clocks)
}
