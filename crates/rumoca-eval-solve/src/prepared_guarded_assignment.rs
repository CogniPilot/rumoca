use std::{cell::RefCell, sync::Arc};

use rumoca_ir_solve as solve;

use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    RowInputRequirements, eval_row_prepared_maybe_fast, record_solve_block_eval,
    row_input_requirements, validate_input_requirements_with_span,
};

/// Prepared evaluator for one compact guarded-assignment owner.
///
/// Unlike [`crate::PreparedScalarProgramBlock`], this owner has no scalar
/// output-index catalog. Its result cardinality comes from the checked target
/// ranges and coordinates are exposed only to the caller's final write
/// boundary.
pub struct PreparedGuardedAssignmentProgram {
    program: Arc<[solve::LinearOp]>,
    span: rumoca_core::Span,
    output_count: usize,
    register_count: usize,
    requirements: RowInputRequirements,
    scratch: RefCell<RowEvalScratch>,
}

impl Clone for PreparedGuardedAssignmentProgram {
    fn clone(&self) -> Self {
        Self {
            program: self.program.clone(),
            span: self.span,
            output_count: self.output_count,
            register_count: self.register_count,
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }
}

impl PreparedGuardedAssignmentProgram {
    pub fn new(owner: &solve::GuardedAssignmentProgram) -> Result<Self, EvalSolveError> {
        let program = owner.shared_program();
        let span = owner.span();
        let register_count = owner.register_count();
        let requirements =
            row_input_requirements(&program).map_err(|error| error.with_source_span(Some(span)))?;
        Ok(Self {
            program,
            span,
            output_count: owner.output_count(),
            register_count,
            requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        })
    }

    pub fn program(&self) -> &[solve::LinearOp] {
        &self.program
    }

    pub const fn span(&self) -> rumoca_core::Span {
        self.span
    }

    pub const fn output_count(&self) -> usize {
        self.output_count
    }

    pub fn eval_outputs_with_context(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        validate_input_requirements_with_span(
            self.requirements,
            y,
            p,
            context.seed,
            Some(self.span),
        )?;
        out.resize(self.output_count, 0.0);
        out.fill(0.0);
        record_solve_block_eval(
            "guarded_assignment_program",
            self.output_count,
            self.output_count,
        );
        let mut scratch = self.scratch.borrow_mut();
        let mut sink = OutputCursor::new(out.as_mut_slice());
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(&self.program, self.register_count, y, p, t, context)
                .with_source_span(Some(self.span)),
            true,
            &mut scratch,
            &mut sink,
        )
        .map_err(|error| error.with_source_span(Some(self.span)))
    }
}
