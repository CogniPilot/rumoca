//! Evaluation of a checked block residual split (SPEC_0043 §6a).

use std::cell::RefCell;

use rumoca_ir_solve::BlockResidualSplit;

use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    RowInputRequirements, SimulationRuntimeState, eval_row_prepared_maybe_fast,
    row_input_requirements, validate_input_requirements,
};

/// A [`BlockResidualSplit`] prepared for one block projection call's
/// invariant evaluation and each pass's dependent evaluation.
pub struct PreparedBlockResidualSplit {
    split: BlockResidualSplit,
    output_count: usize,
    span: Option<rumoca_core::Span>,
    invariant_requirements: Result<RowInputRequirements, EvalSolveError>,
    dependent_requirements: Result<RowInputRequirements, EvalSolveError>,
    scratch: RefCell<RowEvalScratch>,
}

impl PreparedBlockResidualSplit {
    /// Prepare a checked `split` of a program storing `output_count` outputs.
    #[must_use]
    pub fn new(
        split: BlockResidualSplit,
        output_count: usize,
        span: Option<rumoca_core::Span>,
    ) -> Self {
        Self {
            invariant_requirements: row_input_requirements(split.invariant()),
            dependent_requirements: row_input_requirements(split.dependent()),
            split,
            output_count,
            span,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }

    #[must_use]
    pub const fn split(&self) -> &BlockResidualSplit {
        &self.split
    }

    /// Evaluate the invariant part and write its live-out registers to
    /// `values`, in [`BlockResidualSplit::live_out`] order.
    pub fn eval_invariant(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        context: RowEvalContext<'_>,
        values: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        let local_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_state)
            }
        };
        validate_input_requirements(self.invariant_requirements.clone()?, y, p, context.seed)?;
        let mut scratch = self.scratch.borrow_mut();
        let mut none: [f64; 0] = [];
        let mut sink = OutputCursor::new(&mut none);
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(
                self.split.invariant(),
                self.split.register_count(),
                y,
                p,
                t,
                context,
            )
            .with_source_span(self.span),
            true,
            &mut scratch,
            &mut sink,
        )
        .map_err(|error| error.with_source_span(self.span))?;
        values.clear();
        values.extend(
            self.split
                .live_out()
                .iter()
                .map(|&register| scratch.regs[register as usize]),
        );
        Ok(())
    }

    /// Evaluate the dependent part over the invariant `values` and write the
    /// program's outputs to `out`, as a whole-program evaluation would.
    pub fn eval_dependent(
        &self,
        values: &[f64],
        (y, p, t): (&[f64], &[f64], f64),
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        let local_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_state)
            }
        };
        validate_input_requirements(self.dependent_requirements.clone()?, y, p, context.seed)?;
        if values.len() != self.split.live_out().len() {
            return Err(EvalSolveError::InvalidRow {
                message: "block residual split received a different live-out count".to_string(),
                span: self.span,
            });
        }
        out.resize(self.output_count, 0.0);
        out.fill(0.0);
        let mut scratch = self.scratch.borrow_mut();
        scratch.regs.resize(self.split.register_count(), 0.0);
        for (&register, &value) in self.split.live_out().iter().zip(values) {
            scratch.regs[register as usize] = value;
        }
        let mut sink = OutputCursor::new(out.as_mut_slice());
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(
                self.split.dependent(),
                self.split.register_count(),
                y,
                p,
                t,
                context,
            )
            .with_source_span(self.span),
            true,
            &mut scratch,
            &mut sink,
        )
        .map_err(|error| error.with_source_span(self.span))
    }
}

#[cfg(test)]
mod tests;
