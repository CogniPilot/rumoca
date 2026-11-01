//! Backend-neutral contracts for compiled Solve-IR expression blocks.

use std::rc::Rc;

use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use super::CompiledSolveProjectionJacobian;

/// A backend's refusal to compile a program block. When the backend can name
/// the refused source program, it carries that program's span, so a block of
/// programs with distinct spans still reports the one that was refused.
#[derive(Clone, Debug, PartialEq)]
pub struct CompileRefusal {
    message: String,
    span: Option<rumoca_core::Span>,
}

impl CompileRefusal {
    pub fn new(message: impl Into<String>, span: Option<rumoca_core::Span>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub const fn span(&self) -> Option<rumoca_core::Span> {
        self.span
    }
}

impl From<String> for CompileRefusal {
    fn from(message: String) -> Self {
        Self::new(message, None)
    }
}

impl From<&str> for CompileRefusal {
    fn from(message: &str) -> Self {
        Self::new(message, None)
    }
}

impl std::fmt::Display for CompileRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

/// Receives one program's local outputs after it succeeds and may write the
/// seed later programs read; see
/// [`CompiledProgramsCommitting::call_selected_programs_committing`].
pub type ProgramOutputCommit<'a> =
    dyn FnMut(usize, &[f64], &mut [f64]) -> Result<(), crate::RuntimeSolveError> + 'a;

/// Backend-neutral callable produced from one checked Solve-IR expression
/// block. Native execution adapters implement this contract; a compiled runtime
/// requires its whole-block implementation before construction succeeds.
pub trait CompiledSolveExpression {
    /// Bind an actual independent native entry of the exact source owner.
    /// `None` requires preparation of a separate mandatory compiled program.
    fn bind_program(
        &self,
        _source: &solve::ScalarProgramBlock,
        _program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        Ok(None)
    }

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
    /// Issue the committing-sweep capability during preparation. Absence is
    /// fixed for this compiled owner; callers bind their prepared program
    /// executor instead of probing for support during execution.
    fn programs_committing(self: Rc<Self>) -> Option<Rc<dyn CompiledProgramsCommitting>>;
    /// Bind an existing directional entry without executing or recompiling it.
    fn bind_program(
        &self,
        _source: &solve::ScalarProgramBlock,
        _program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        Ok(None)
    }
    /// Prepare every application at once, one entry per application in
    /// order; `None` leaves an application to its required whole compiled kernel.
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

/// A construction-issued native sweep over an ordered list of source
/// programs. Every successful program commits before the next reads its seed.
/// Errors stop at that program; an admitted entry cannot decline execution.
pub trait CompiledProgramsCommitting {
    fn call_selected_programs_committing(
        &self,
        programs: &[usize],
        inputs: (&[f64], &[f64], f64),
        seed: &mut [f64],
        external_tables: &[rumoca_core::ExternalTableData],
        commit: &mut ProgramOutputCommit<'_>,
    ) -> Result<(), crate::RuntimeSolveError>;
}
