//! Numeric inspection of a checked DAE at the DAE phase boundary.
//!
//! Backend and runtime crates consume this narrow context instead of coupling
//! directly to the DAE evaluator. Evaluation semantics remain single-source in
//! `rumoca-eval-dae`; this module owns the phase-facing capability and error
//! envelope.

use rumoca_ir_dae as dae;

#[derive(Debug, thiserror::Error)]
#[error("{source}")]
pub struct NumericDaeError {
    source: rumoca_eval_dae::NumericEvaluationError,
}

impl NumericDaeError {
    #[must_use]
    pub const fn span(&self) -> rumoca_core::Span {
        self.source.span()
    }

    #[must_use]
    pub fn is_invalid_override(&self) -> bool {
        self.source.kind() == rumoca_eval_dae::NumericEvaluationErrorKind::InvalidOverride
    }
}

impl From<rumoca_eval_dae::NumericEvaluationError> for NumericDaeError {
    fn from(source: rumoca_eval_dae::NumericEvaluationError) -> Self {
        Self { source }
    }
}

/// Phase-owned capability for numeric inspection of one branded checked DAE.
pub struct NumericDaeContext<'dae, F = fn(dae::VariableView<'dae>, usize) -> Option<f64>> {
    evaluator: rumoca_eval_dae::NumericEvaluator<'dae, F>,
}

impl<'dae> NumericDaeContext<'dae> {
    #[must_use]
    pub fn new(view: dae::DaeView<'dae>) -> Self {
        Self {
            evaluator: rumoca_eval_dae::NumericEvaluator::new(view),
        }
    }
}

impl<'dae, F> NumericDaeContext<'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    #[must_use]
    pub fn with_overrides(view: dae::DaeView<'dae>, override_value: F) -> Self {
        Self {
            evaluator: rumoca_eval_dae::NumericEvaluator::with_overrides(view, override_value),
        }
    }

    pub fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<Vec<f64>, NumericDaeError> {
        self.evaluator.expression(expression).map_err(Into::into)
    }

    pub fn initial_value(
        &mut self,
        variable: dae::VariableId<'dae>,
    ) -> Result<Vec<f64>, NumericDaeError> {
        self.evaluator.initial_value(variable).map_err(Into::into)
    }
}
