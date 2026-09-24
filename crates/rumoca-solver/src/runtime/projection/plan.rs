use rumoca_ir_solve as solve;

use super::RuntimeSolveError;

#[cfg(test)]
thread_local! {
    static ALGEBRAIC_PLAN_VALIDATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
pub(crate) fn algebraic_plan_validation_count() -> usize {
    ALGEBRAIC_PLAN_VALIDATIONS.get()
}

pub(crate) fn validate_algebraic_projection_plan(
    plan: &solve::AlgebraicProjectionPlan,
    state_count: usize,
    solver_count: usize,
) -> Result<(), RuntimeSolveError> {
    #[cfg(test)]
    ALGEBRAIC_PLAN_VALIDATIONS.set(ALGEBRAIC_PLAN_VALIDATIONS.get() + 1);
    let algebraic_count =
        algebraic_tail_len(solver_count, state_count, "algebraic projection plan")?;
    let mut row_seen = vec![false; solver_count];
    let mut y_seen = vec![false; algebraic_count];
    for block in &plan.blocks {
        if !block.has_valid_tearing_partitions() {
            return Err(RuntimeSolveError::solve_ir(
                "invalid algebraic tearing partition",
            ));
        }
        require_square_projection_block(block.rows.len(), block.y_indices.len(), "algebraic")?;
        mark_projection_indices(
            &block.rows,
            0,
            solver_count,
            &mut row_seen,
            "algebraic projection",
            "residual row",
        )?;
        mark_projection_indices(
            &block.y_indices,
            state_count,
            solver_count,
            &mut y_seen,
            "algebraic projection",
            "unknown",
        )?;
    }
    Ok(())
}

pub(super) fn validate_initial_projection_plan(
    plan: &solve::AlgebraicProjectionPlan,
    residual_count: usize,
    solver_count: usize,
) -> Result<(), RuntimeSolveError> {
    let mut row_seen = vec![false; residual_count];
    let mut y_seen = vec![false; solver_count];
    for block in &plan.blocks {
        require_square_projection_block(block.rows.len(), block.y_indices.len(), "initial")?;
        mark_projection_indices(
            &block.rows,
            0,
            residual_count,
            &mut row_seen,
            "initial projection",
            "residual row",
        )?;
        mark_projection_indices(
            &block.y_indices,
            0,
            solver_count,
            &mut y_seen,
            "initial projection",
            "unknown",
        )?;
    }
    Ok(())
}

pub(super) fn require_square_projection_block(
    row_count: usize,
    unknown_count: usize,
    kind: &str,
) -> Result<(), RuntimeSolveError> {
    if row_count == unknown_count {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "{kind} projection block has {row_count} residual rows but {unknown_count} unknowns"
    )))
}

fn mark_projection_indices(
    indices: &[usize],
    lower_bound: usize,
    upper_bound: usize,
    seen: &mut [bool],
    context: &str,
    role: &str,
) -> Result<(), RuntimeSolveError> {
    for &index in indices {
        if index < lower_bound || index >= upper_bound {
            return Err(RuntimeSolveError::solve_ir(format!(
                "{context} {role} {index} is outside {lower_bound}..{upper_bound}"
            )));
        }
        let slot = &mut seen[index - lower_bound];
        if *slot {
            return Err(RuntimeSolveError::solve_ir(format!(
                "{context} {role} {index} appears more than once"
            )));
        }
        *slot = true;
    }
    Ok(())
}

pub(super) fn algebraic_tail_len(
    total: usize,
    state_count: usize,
    context: &'static str,
) -> Result<usize, RuntimeSolveError> {
    total.checked_sub(state_count).ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "{context} state count {state_count} exceeds vector length {total}"
        ))
    })
}
