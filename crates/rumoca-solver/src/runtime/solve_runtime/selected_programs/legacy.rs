//! Temporary trait borrows for numeric fields whose source owner has not yet
//! migrated. Complete products retain mandatory execution; this bridge cannot
//! turn an admitted call into a decline or expose an owner engine.
use super::*;

impl CompiledSolveExpression for CompleteExpression {
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        CompleteExpression::call_program_outputs(self, program, y, p, t, tables, out)?;
        Ok(true)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        CompleteExpression::call_program_output(self, coordinate, y, p, t, tables).map(Some)
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: (&[f64], &[f64], f64),
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        CompleteExpression::call_program_outputs_at(self, coordinates, inputs, tables, out)?;
        Ok(true)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        CompleteExpression::call(self, y, p, t, tables, out)
    }
}

impl CompiledSolveJacobianExpression for CompleteJacobian {
    fn prepare_projections(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        CompleteJacobian::prepare_projections(self, applications)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        CompleteJacobian::call_program_outputs(self, program, inputs, tables, out)?;
        Ok(true)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        CompleteJacobian::call_program_output(self, coordinate, y, p, t, seed, tables).map(Some)
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        CompleteJacobian::call_program_outputs_at(self, coordinates, inputs, tables, out)?;
        Ok(true)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        CompleteJacobian::call(self, y, p, t, seed, tables, out)
    }
}
