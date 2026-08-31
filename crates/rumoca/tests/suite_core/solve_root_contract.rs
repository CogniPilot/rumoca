//! Re-sealing witness shared by the suite's lowered-Solve-root fixtures.
//!
//! [`rumoca_ir_solve::SolveProblem::construct`] is the sole issuer of a Solve
//! root, so a fixture states that root's shape contract by handing the root's
//! own seven checked children back to the constructor. The serialization
//! equality additionally fixes that the seven getters expose the whole root:
//! a sealed field the children do not carry would make the re-sealed root
//! differ from the lowered one.

use rumoca_ir_solve::{SolveProblem, SolveProblemShapeContractError};

/// Re-seal `problem` through the public checked constructor and require the
/// result to reproduce the lowered root exactly.
pub(crate) fn reseal_solve_problem(
    problem: &SolveProblem,
) -> Result<SolveProblem, SolveProblemShapeContractError> {
    let resealed = SolveProblem::construct(
        problem.layout().clone(),
        problem.solve_layout().clone(),
        problem.continuous().clone(),
        problem.initialization().clone(),
        problem.discrete().clone(),
        problem.events().clone(),
        problem.clocks().clone(),
    )?;
    assert_eq!(
        serde_json::to_value(&resealed).expect("a re-sealed Solve root serializes"),
        serde_json::to_value(problem).expect("a lowered Solve root serializes"),
        "re-sealing a lowered root from its seven checked children reproduces it exactly"
    );
    Ok(resealed)
}
