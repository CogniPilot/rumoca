//! Evaluation of a checked block residual split (SPEC_0043 §6a).

use rumoca_ir_solve::BlockResidualSplit;

use crate::{EvalSolveError, RowEvalContext};
#[cfg(feature = "reference-eval")]
use crate::{
    OutputCursor, PreparedRowEval, RowEvalScratch, RowInputRequirements, SimulationRuntimeState,
    eval_row_prepared_maybe_fast, validate_input_requirements,
};
#[cfg(feature = "reference-eval")]
use std::cell::RefCell;

/// A [`BlockResidualSplit`] prepared for one block projection call's
/// invariant evaluation and each pass's dependent evaluation.
pub struct PreparedBlockResidualSplit {
    split: BlockResidualSplit,
    #[cfg(feature = "reference-eval")]
    output_count: usize,
    #[cfg(feature = "reference-eval")]
    span: Option<rumoca_core::Span>,
    #[cfg(feature = "reference-eval")]
    invariant_requirements: Result<RowInputRequirements, EvalSolveError>,
    #[cfg(feature = "reference-eval")]
    dependent_requirements: Result<RowInputRequirements, EvalSolveError>,
    #[cfg(feature = "reference-eval")]
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
        let invariant_requirements = crate::row_input_requirements(split.invariant());
        let dependent_requirements = crate::row_input_requirements(split.dependent());
        #[cfg(not(feature = "reference-eval"))]
        let _ = (
            output_count,
            span,
            invariant_requirements,
            dependent_requirements,
        );
        Self {
            #[cfg(feature = "reference-eval")]
            invariant_requirements,
            #[cfg(feature = "reference-eval")]
            dependent_requirements,
            split,
            #[cfg(feature = "reference-eval")]
            output_count,
            #[cfg(feature = "reference-eval")]
            span,
            #[cfg(feature = "reference-eval")]
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }

    #[must_use]
    pub const fn split(&self) -> &BlockResidualSplit {
        &self.split
    }

    /// Refused in the compiled-only product, which excludes this evaluator body.
    #[cfg(not(feature = "reference-eval"))]
    #[allow(unused_variables, unused_mut)] // the refusal reads none of its arguments
    pub fn eval_invariant(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        context: RowEvalContext<'_>,
        values: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        Err(crate::interpreter_excluded("eval_invariant"))
    }
    /// Evaluate the invariant part and write its live-out registers to
    /// `values`, in [`BlockResidualSplit::live_out`] order.
    #[cfg(feature = "reference-eval")]
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

    /// Refused in the compiled-only product, which excludes this evaluator body.
    #[cfg(not(feature = "reference-eval"))]
    #[allow(unused_variables, unused_mut)] // the refusal reads none of its arguments
    pub fn eval_dependent(
        &self,
        values: &[f64],
        (y, p, t): (&[f64], &[f64], f64),
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        Err(crate::interpreter_excluded("eval_dependent"))
    }
    /// Evaluate the dependent part over the invariant `values` and write the
    /// program's outputs to `out`, as a whole-program evaluation would.
    #[cfg(feature = "reference-eval")]
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
                #[cfg(feature = "reference-eval")]
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

#[cfg(all(test, feature = "reference-eval"))]
mod tests;
