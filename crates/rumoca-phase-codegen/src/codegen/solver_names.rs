//! An explicit template call materializes a requested named state report.

use super::solve_lazy::SolveRenderHandle;
use minijinja::{Error, ErrorKind, State, Value, value::Object};
use std::{fmt, sync::Arc};

pub(super) struct SolverStateReport(pub(super) SolveRenderHandle);

impl fmt::Debug for SolverStateReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SolverStateReport")
            .field(
                "state_scalar_count",
                &self.0.problem().solve_layout.state_scalar_count(),
            )
            .finish_non_exhaustive()
    }
}

impl Object for SolverStateReport {
    fn call(self: &Arc<Self>, _state: &State<'_, '_>, args: &[Value]) -> Result<Value, Error> {
        if !args.is_empty() {
            return Err(Error::new(
                ErrorKind::InvalidOperation,
                "state_names takes no arguments",
            ));
        }
        let problem = self.0.problem();
        problem
            .solver_named_report(0..problem.solve_layout.state_scalar_count())
            .map(Value::from_serialize)
            .map_err(|error| Error::new(ErrorKind::InvalidOperation, error.to_string()))
    }
}
