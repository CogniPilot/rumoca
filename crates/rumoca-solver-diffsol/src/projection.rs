use rumoca_ir_solve as solve;

use crate::{
    EVENT_UPDATE_MAX_ITERS, OdeModel, SimError, apply_runtime_assignments,
    update_relation_memory_values,
};

pub(crate) fn settle_initial_projection_context(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        let mut changed = apply_runtime_assignments(ode_model, y, p, t, tol)?;
        changed |= update_relation_memory_values(
            ode_model,
            y,
            p,
            t,
            &model.problem.solve_layout.relation_memory_parameter_indices,
        )?;
        changed |= apply_runtime_assignments(ode_model, y, p, t, tol)?;
        if !changed {
            return Ok(());
        }
    }
    Err(SimError::SolveIr(format!(
        "initial projection context did not converge at t={t}"
    )))
}
