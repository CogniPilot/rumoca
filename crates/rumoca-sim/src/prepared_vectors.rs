//! Re-prepare a lowered model's initial vectors for new parameter values.
//!
//! Changing a tunable parameter (e.g. a geometry angle baked into algebraic
//! masks) does not require re-lowering: override the parameter slots, then
//! re-run the same settle sequence the runtime performs at simulation start
//! (initialization updates, relation memory, algebraic refresh, pre-param
//! commit). Event iteration is *not* performed here — callers on paths that
//! do not handle runtime events (the WebGPU v1 driver) keep their existing
//! frozen-events contract.

use rumoca_ir_solve as solve;
use rumoca_solver::SolveRuntime;
use rumoca_solver::commit_pre_params_after_event;

const SETTLE_TOL: f64 = 1e-9;
const SETTLE_MAX_ITERS: usize = 64;

#[derive(Debug, thiserror::Error)]
pub enum PreparedVectorError {
    #[error("`{name}` is not a tunable scalar parameter of this model")]
    NotAParameter { name: String },
    #[error("parameter settle failed: {message}")]
    Settle { message: String },
    #[error("runtime preparation failed: {message}")]
    Runtime {
        message: String,
        span: Option<rumoca_core::Span>,
    },
}

impl PreparedVectorError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::Runtime { span, .. } => *span,
            _ => None,
        }
    }
}

/// Keep each prepared-vector failure's own identity when it is reported through
/// the shared simulation diagnostic (SPEC_0008: a code identifies the defect,
/// not the reporting surface).
///
/// - `NotAParameter` is the same user mistake `apply_simulation_overrides`
///   rejects, so it stays an override rejection (`EX003`) instead of being
///   relabeled as a solver failure.
/// - `Settle` and `Runtime` happen before integration starts, so they are
///   runtime-preparation failures (`EX002`); `Runtime` keeps its span so the
///   CLI/LSP can still point at the offending source.
impl From<PreparedVectorError> for crate::SimulationDiagnosticError {
    fn from(value: PreparedVectorError) -> Self {
        let message = value.to_string();
        match value {
            PreparedVectorError::NotAParameter { .. } => Self::InvalidOverride { message },
            PreparedVectorError::Settle { .. } => Self::RuntimePreparation {
                message,
                span: None,
            },
            PreparedVectorError::Runtime { span, .. } => Self::RuntimePreparation { message, span },
        }
    }
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
    let runtime = SolveRuntime::new(model).map_err(|error| PreparedVectorError::Runtime {
        message: error.to_string(),
        span: error.source_span(),
    })?;
    let settle = |message: String| PreparedVectorError::Settle { message };
    runtime
        .settle_initialization_system(&mut y, &mut params, t_start, SETTLE_TOL, SETTLE_MAX_ITERS)
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

#[cfg(test)]
mod tests {
    use super::PreparedVectorError;
    use crate::SimulationDiagnosticError;

    #[test]
    fn unknown_override_name_keeps_the_override_rejection_code() {
        let error = SimulationDiagnosticError::from(PreparedVectorError::NotAParameter {
            name: "notAParam".to_string(),
        });

        // EX003 (override rejected), not EX001 (numeric solver failed): the
        // user named a parameter the model does not have, exactly as the
        // in-crate `apply_simulation_overrides` path reports it.
        assert_eq!(error.diagnostic_code(), "EX003");
        assert!(
            error.to_string().contains("notAParam"),
            "unexpected message: {error}"
        );
    }

    #[test]
    fn settle_failure_is_reported_as_runtime_preparation() {
        let error = SimulationDiagnosticError::from(PreparedVectorError::Settle {
            message: "algebraic refresh did not converge".to_string(),
        });

        assert_eq!(error.diagnostic_code(), "EX002");
        assert_eq!(error.source_span(), None);
        assert_eq!(
            error.to_string(),
            "parameter settle failed: algebraic refresh did not converge"
        );
    }

    #[test]
    fn runtime_failure_keeps_its_span_and_preparation_code() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("prepared_vectors_source_1.mo"),
            12,
            18,
        );
        let error = SimulationDiagnosticError::from(PreparedVectorError::Runtime {
            message: "invalid native map metadata".to_string(),
            span: Some(span),
        });

        assert_eq!(error.diagnostic_code(), "EX002");
        assert_eq!(error.source_span(), Some(span));
    }
}
