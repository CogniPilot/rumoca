//! Use the complete source-bound callback without a second refresh/value pass.
use super::*;
impl ScalarExecutionView<'_> {
    pub(super) fn try_complete_derivative(
        &self,
        time: f64,
        y: &mut [f64],
        parameters: &[f64],
        output: &mut [f64],
    ) -> Option<Result<(), RuntimeSolveError>> {
        let operation = self.scalar.compiled_derivative_operation.as_ref()?;
        let result = {
            let mut cache = match self.scalar.static_refresh_cache.try_borrow_mut() {
                Ok(cache) => cache,
                Err(_) => {
                    return Some(Err(RuntimeSolveError::solve_ir(
                        "complete derivative cache is reentrant",
                    )));
                }
            };
            operation.call(
                y,
                parameters,
                time,
                self.model.external_tables.as_slice(),
                output,
                cache.operation_view(),
            )
        };
        if result.as_ref().err().is_none_or(|failure| {
            failure
                .phase
                .is_some_and(solve::fmi::FmiDerivativeStatus::has_evaluated_values)
        }) {
            solve_eval::nan_trace::report_state_derivative(&self.model, time, y, output);
        }
        Some(result.map_err(|failure| failure.error))
    }
}

impl ScalarExecutionView<'_> {
    /// The issued kinematic entry of the complete operation, when it serves
    /// exactly `rows`.
    pub(super) fn try_complete_kinematic(
        &self,
        time: f64,
        y: &[f64],
        parameters: &[f64],
        rows: &[usize],
        output: &mut [f64],
    ) -> Option<Result<(), RuntimeSolveError>> {
        let operation = self.scalar.compiled_derivative_operation.as_ref()?;
        let result = operation.call_kinematic(
            y,
            parameters,
            time,
            self.model.external_tables.as_slice(),
            rows,
            output,
        )?;
        Some(result.map_err(|failure| failure.error))
    }
}
