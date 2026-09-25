//! Evaluation of checked multi-lane tangent programs.

use std::cell::RefCell;

use rumoca_ir_solve::TangentLaneProgram;

use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    SimulationRuntimeState, eval_row_prepared_maybe_fast, row_input_requirements,
    validate_input_requirements, validate_output_len,
};

/// A [`TangentLaneProgram`] prepared for repeated evaluation.
///
/// Seeds are element-major (`seed[i * lanes + l]` is lane `l` of seed index
/// `i`) and outputs lane-major (`out[l * m + o]` is lane `l` of output `o`).
pub struct PreparedTangentLaneProgram {
    program: TangentLaneProgram,
    scratch: RefCell<RowEvalScratch>,
}

impl PreparedTangentLaneProgram {
    #[must_use]
    pub fn new(program: TangentLaneProgram) -> Self {
        Self {
            program,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }

    #[must_use]
    pub const fn program(&self) -> &TangentLaneProgram {
        &self.program
    }

    /// Evaluate every lane of every output into `out`.
    pub fn eval(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
        let ops = self.program.ops();
        validate_output_len(out, self.program.lanes() * self.program.lane_outputs())?;
        validate_input_requirements(row_input_requirements(ops)?, y, p, context.seed)?;
        let mut scratch = self.scratch.borrow_mut();
        let mut sink = OutputCursor::new(out);
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(ops, self.program.register_count(), y, p, t, context),
            true,
            &mut scratch,
            &mut sink,
        )
    }
}
