//! Re-prepare a lowered model's initial vectors for new parameter values.
//!
//! Changing a tunable parameter (e.g. a geometry angle baked into algebraic
//! masks) does not require re-lowering: override the parameter slots, then
//! re-run the same settle sequence the runtime performs at simulation start
//! (initialization updates, relation memory, algebraic refresh, pre-param
//! commit). Event iteration is *not* performed here — callers on paths that
//! do not handle runtime events (the WebGPU v1 driver) keep their existing
//! frozen-events contract.

use rumoca_eval_solve::SolveRuntime;
use rumoca_ir_solve as solve;
use rumoca_solver::commit_pre_params_after_event;

const SETTLE_TOL: f64 = 1e-9;
const SETTLE_MAX_ITERS: usize = 64;

#[derive(Debug, thiserror::Error)]
pub enum PreparedVectorError {
    #[error("`{name}` is not a tunable scalar parameter of this model")]
    NotAParameter { name: String },
    #[error("parameter settle failed: {message}")]
    Settle { message: String },
}

/// Override named parameters and return freshly settled `(y0, p0)`.
pub fn refresh_prepared_vectors(
    model: &solve::SolveModel,
    t_start: f64,
    overrides: &[(String, f64)],
) -> Result<(Vec<f64>, Vec<f64>), PreparedVectorError> {
    let mut params = model.parameters.clone();
    for (name, value) in overrides {
        let Some(solve::ScalarSlot::P { index, .. }) = model.problem.layout.binding(name) else {
            return Err(PreparedVectorError::NotAParameter { name: name.clone() });
        };
        params[index] = *value;
    }

    let mut y = model.initial_y.clone();
    let runtime = SolveRuntime::new(model);
    let settle = |message: String| PreparedVectorError::Settle { message };
    runtime
        .apply_initialization_updates(&mut y, &mut params, t_start, SETTLE_TOL, SETTLE_MAX_ITERS)
        .map_err(|e| settle(e.to_string()))?;
    let state_count = model.state_scalar_count();
    let state = y[..state_count.min(y.len())].to_vec();
    runtime
        .update_relation_memory_from_state(
            t_start,
            &state,
            &mut params,
            SETTLE_TOL,
            SETTLE_MAX_ITERS,
        )
        .map_err(|e| settle(e.to_string()))?;
    runtime
        .refresh_algebraic_and_output_slots(t_start, &mut y, &params, SETTLE_TOL, SETTLE_MAX_ITERS)
        .map_err(|e| settle(e.to_string()))?;
    commit_pre_params_after_event(model, &y, &mut params, SETTLE_TOL);
    Ok((y, params))
}
