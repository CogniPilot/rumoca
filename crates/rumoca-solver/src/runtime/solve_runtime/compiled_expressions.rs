//! Backend-neutral contracts for compiled Solve-IR expression blocks.

use std::rc::Rc;

use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use super::CompiledSolveProjectionJacobian;

/// Backend-neutral callable produced from one checked Solve-IR expression
/// block. Native execution adapters implement this contract; the runtime
/// retains the prepared evaluator as the correctness fallback.
pub trait CompiledSolveExpression {
    /// Execute all local outputs of one retained source program. A decline
    /// occurs before execution; admitted execution errors must propagate.
    fn call_program_outputs(
        &self,
        _program: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _external_tables: &[rumoca_core::ExternalTableData],
        _out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        Ok(false)
    }

    /// Evaluate one source program output at `(program index, output offset)`.
    /// Other programs must not execute. `None` declines this optional entry
    /// point; an admitted execution error must propagate to the caller.
    fn call_program_output(
        &self,
        _coordinate: (usize, usize),
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, crate::RuntimeSolveError> {
        Ok(None)
    }

    /// Optional complete bulk entry point. The default declines before any
    /// source operation executes; implementations preflight complete coverage
    /// and publish all outputs only after successful admitted execution.
    fn call_program_outputs_at(
        &self,
        _coordinates: &[(usize, usize)],
        _inputs: (&[f64], &[f64], f64),
        _external_tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<bool, crate::RuntimeSolveError> {
        Ok(false)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError>;
}

/// Backend-neutral callable for a checked forward-mode Solve-IR expression.
pub trait CompiledSolveJacobianExpression {
    /// Prepare every application at once, one entry per application in
    /// order; `None` leaves an application to the generic evaluator.
    fn prepare_projections(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        Ok(vec![None; applications.len()])
    }
    /// Execute one already compiled program and return all of its local
    /// outputs. `false` declines this optional entry point before execution.
    fn call_program_outputs(
        &self,
        _program: usize,
        _inputs: solve_eval::JacobianEvalInputs<'_>,
        _external_tables: &[rumoca_core::ExternalTableData],
        _out: &mut Vec<f64>,
    ) -> Result<bool, crate::RuntimeSolveError> {
        Ok(false)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError>;

    /// Evaluate one output of one already compiled program. `coordinate` is
    /// `(program index, output offset)`, as issued by the prepared scalar view,
    /// not a visible-output index. Other programs must not execute. `None`
    /// declines this optional entry point; an execution error is not a decline.
    fn call_program_output(
        &self,
        _coordinate: (usize, usize),
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &[f64],
        _external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, crate::RuntimeSolveError> {
        Ok(None)
    }

    /// Optional complete bulk entry point. The default declines before any
    /// source operation executes; implementations preflight complete coverage
    /// and publish all outputs only after successful admitted execution.
    fn call_program_outputs_at(
        &self,
        _coordinates: &[(usize, usize)],
        _inputs: solve_eval::JacobianEvalInputs<'_>,
        _external_tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<bool, crate::RuntimeSolveError> {
        Ok(false)
    }
}
