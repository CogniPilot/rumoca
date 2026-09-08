//! Prepared batched sweep for one torn coupled algebraic block.
//!
//! The torn projection evaluates the same short row sequence many times per
//! solve: every causal isolator in dependency order, then the reduced residual
//! rows. Issued as separate prepared-block calls, each row pays per-call
//! dispatch (row lookup, shape search, scratch borrows) on top of its
//! interpreter time, roughly ten times per sweep on the MSL thyristor loops.
//! The prepared sweep resolves each row to its program and certified
//! assignment shape once, so the hot solve makes one call per sweep.
//!
//! Strict refinement: the sweep is built from the same certified isolators and
//! program outputs the per-row path resolves on every call, and evaluation
//! reuses the same row cores (`eval_target_assignment_row_with_scratch` and
//! `eval_row_output_with_scratch`), so the two paths cannot diverge in values,
//! evaluation order, or singular/non-finite decline decisions.

use rumoca_ir_solve::TargetAssignmentShape;

use super::{PreparedScalarProgramBlock, RowOutputRequest, TargetAssignmentScratchRequest};
use crate::{EvalSolveError, RowEvalContext};

/// One causal back-substitution step resolved at prepare time.
struct PreparedTornStep {
    program_row: usize,
    shape: TargetAssignmentShape,
    target_y_index: usize,
}

/// Prepared batched sweep for one torn coupled block: the ordered causal
/// steps plus the reduced residual rows, resolved once against the scalar
/// program block.
pub struct PreparedTornSweep {
    steps: Vec<PreparedTornStep>,
    /// Residual rows resolved to (program, output offset); `None` marks a row
    /// with no scalar view, which the sweep reports as unevaluable exactly as
    /// the per-row path does.
    residuals: Vec<Option<(usize, usize)>>,
}

/// Whether one batched sweep completed or declined at a causal step.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TornSweepStatus {
    /// Every causal step produced a finite value; the residuals are populated.
    Completed,
    /// A causal step was singular or non-finite at this iterate; the caller
    /// restores `y` and declines the torn solve.
    Declined,
}

impl PreparedScalarProgramBlock {
    /// Resolve a torn block's causal steps `(row, target solver-Y index)` and
    /// residual rows once.
    ///
    /// Returns `None` when any causal step is not a certified exact target
    /// assignment with a recognized shape; the caller then keeps the per-row
    /// path, which reaches the same decline decision one row at a time.
    pub fn prepare_torn_sweep(
        &self,
        causal_steps: &[(usize, usize)],
        residual_rows: &[usize],
    ) -> Option<PreparedTornSweep> {
        let steps = causal_steps
            .iter()
            .map(|&(row, target_y_index)| {
                let (program_row, output_offset) = self.row_output_position(row)?;
                if !self.certifies_exact_target_assignment_output(
                    program_row,
                    output_offset,
                    target_y_index,
                ) {
                    return None;
                }
                let shape =
                    self.assignment_shape_for_output(program_row, output_offset, target_y_index)?;
                Some(PreparedTornStep {
                    program_row,
                    shape,
                    target_y_index,
                })
            })
            .collect::<Option<Vec<_>>>()?;
        let residuals = residual_rows
            .iter()
            .map(|&row| self.row_output_position(row))
            .collect();
        Some(PreparedTornSweep { steps, residuals })
    }

    /// Execute one prepared sweep: every causal isolator in order (each
    /// writes its recovered unknown into `y`), then the reduced residual
    /// rows. `residual_out` receives the raw row values; `None` marks a row
    /// with no scalar view. The caller applies its own finiteness policy to
    /// the residual values, matching its per-row path.
    pub fn eval_torn_sweep_unchecked_with_context(
        &self,
        sweep: &PreparedTornSweep,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        residual_out: &mut Vec<Option<f64>>,
    ) -> Result<TornSweepStatus, EvalSolveError> {
        residual_out.clear();
        let mut out = self.row_output_scratch.borrow_mut();
        let mut scratch = self.scratch.borrow_mut();
        for step in &sweep.steps {
            let evaluated =
                self.eval_target_assignment_row_with_scratch(TargetAssignmentScratchRequest {
                    row_idx: step.program_row,
                    shape: step.shape,
                    y,
                    p,
                    t,
                    context,
                    scratch: &mut scratch,
                });
            let value = match evaluated {
                Ok(value) => value,
                // The per-row path declines the torn solve on a singular
                // isolator instead of failing the projection; the batched
                // sweep makes the same decision at the same step.
                Err(EvalSolveError::SingularTargetAssignment { .. }) => {
                    return Ok(TornSweepStatus::Declined);
                }
                Err(error) => return Err(error),
            };
            if !value.is_finite() {
                return Ok(TornSweepStatus::Declined);
            }
            let Some(slot) = y.get_mut(step.target_y_index) else {
                return Ok(TornSweepStatus::Declined);
            };
            *slot = value;
        }
        for position in &sweep.residuals {
            let Some((program_row, output_offset)) = *position else {
                residual_out.push(None);
                continue;
            };
            let value = self.eval_row_output_with_scratch(
                RowOutputRequest {
                    row_idx: program_row,
                    output_offset,
                    y,
                    p,
                    t,
                    context,
                    validate_inputs: false,
                    label: "torn_sweep_residual",
                },
                &mut scratch,
                &mut out,
            )?;
            residual_out.push(Some(value));
        }
        Ok(TornSweepStatus::Completed)
    }
}
