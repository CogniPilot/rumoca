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

use rumoca_ir_solve::{LinearOp, ScalarProgramBlock, TargetAssignmentShape};

use super::{
    AssignmentProgramBuilder, PreparedScalarProgramBlock, RowOutputRequest,
    TargetAssignmentScratchRequest,
};
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
    /// Runs of several consecutive steps recovered from one residual program
    /// ([`PreparedScalarProgramBlock::torn_sweep_runs`]), each evaluated as
    /// one chain program of `chains`, in step order.
    runs: Vec<PreparedTornRun>,
    chains: Option<Box<PreparedScalarProgramBlock>>,
    /// Residual rows resolved to (program, output offset); `None` marks a row
    /// with no scalar view, which the sweep reports as unevaluable exactly as
    /// the per-row path does.
    residuals: Vec<Option<(usize, usize)>>,
}

/// One chain run: `count` steps from `first_step`, answered by chain program
/// `chain`.
struct PreparedTornRun {
    first_step: usize,
    count: usize,
    chain: usize,
}

/// Backend-compilable form of one prepared torn sweep. The causal chain
/// becomes an ordered assignment-schedule row list (each row's single output
/// writes its solver-Y target, so later rows observe earlier writes exactly
/// as back-substitution does) and the reduced residual rows become one
/// expression block evaluated at the substituted point.
pub struct TornSweepComposite {
    /// One single-output isolator program per causal step, in order.
    pub assignment_rows: Vec<Vec<LinearOp>>,
    /// The solver-Y slot each assignment row writes.
    pub assignment_targets: Vec<usize>,
    /// The reduced residual rows as one expression block.
    pub residual_block: ScalarProgramBlock,
    /// Flat output index of each residual row in `residual_block`'s output
    /// order; `None` marks a row with no scalar view.
    pub residual_outputs: Vec<Option<usize>>,
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
                    shape: shape.clone(),
                    target_y_index,
                })
            })
            .collect::<Option<Vec<_>>>()?;
        let residuals = residual_rows
            .iter()
            .map(|&row| self.row_output_position(row))
            .collect();
        // Without chain runs the sweep declines grouping and evaluates every
        // step on its own.
        let Some((runs, chains)) = self.prepare_torn_chains(causal_steps) else {
            return Some(PreparedTornSweep {
                steps,
                runs: Vec::new(),
                chains: None,
                residuals,
            });
        };
        Some(PreparedTornSweep {
            steps,
            runs,
            chains,
            residuals,
        })
    }

    /// The chain runs of a sweep and their prepared chain programs, or `None`
    /// when the runs or a chain program do not construct; the sweep then
    /// evaluates every step on its own.
    fn prepare_torn_chains(
        &self,
        causal_steps: &[(usize, usize)],
    ) -> Option<(
        Vec<PreparedTornRun>,
        Option<Box<PreparedScalarProgramBlock>>,
    )> {
        let mut runs = Vec::new();
        let mut programs = Vec::new();
        let mut spans = Vec::new();
        for run in self.torn_sweep_runs(causal_steps)? {
            if run.pairs.len() < 2 {
                continue;
            }
            programs.push(self.target_isolation_chain_program(run.program_row, &run.pairs)?);
            spans.push(self.block.program_span(run.program_row)?);
            runs.push(PreparedTornRun {
                first_step: run.first_step,
                count: run.pairs.len(),
                chain: runs.len(),
            });
        }
        if runs.is_empty() {
            return Some((runs, None));
        }
        let block = ScalarProgramBlock::with_program_spans(programs, spans).ok()?;
        let prepared = PreparedScalarProgramBlock::new(block).ok()?;
        Some((runs, Some(Box::new(prepared))))
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
        let mut runs = sweep.runs.iter().peekable();
        let mut index = 0;
        while index < sweep.steps.len() {
            let run = runs.next_if(|run| run.first_step == index);
            let count = run.map_or(1, |run| run.count);
            let chained = match run {
                Some(run) => self.eval_torn_chain(sweep, run, y, (p, t), context)?,
                None => None,
            };
            let status = match chained {
                Some(status) => status,
                None => {
                    let steps = &sweep.steps[index..index + count];
                    self.eval_torn_steps(steps, y, (p, t), context, &mut scratch)?
                }
            };
            if status == TornSweepStatus::Declined {
                return Ok(status);
            }
            index += count;
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

    /// One causal step through its certified isolator: writes the recovered
    /// unknown, or declines on a singular or non-finite isolation.
    fn eval_torn_step(
        &self,
        step: &PreparedTornStep,
        y: &mut [f64],
        (p, t): (&[f64], f64),
        context: RowEvalContext<'_>,
        scratch: &mut super::super::RowEvalScratch,
    ) -> Result<TornSweepStatus, EvalSolveError> {
        let evaluated =
            self.eval_target_assignment_row_with_scratch(TargetAssignmentScratchRequest {
                row_idx: step.program_row,
                shape: &step.shape,
                y,
                p,
                t,
                context,
                scratch,
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
        let Some(slot) = y.get_mut(step.target_y_index).filter(|_| value.is_finite()) else {
            return Ok(TornSweepStatus::Declined);
        };
        *slot = value;
        Ok(TornSweepStatus::Completed)
    }

    /// Consecutive causal steps, each through its own isolator.
    fn eval_torn_steps(
        &self,
        steps: &[PreparedTornStep],
        y: &mut [f64],
        (p, t): (&[f64], f64),
        context: RowEvalContext<'_>,
        scratch: &mut super::super::RowEvalScratch,
    ) -> Result<TornSweepStatus, EvalSolveError> {
        for step in steps {
            if self.eval_torn_step(step, y, (p, t), context, scratch)? == TornSweepStatus::Declined
            {
                return Ok(TornSweepStatus::Declined);
            }
        }
        Ok(TornSweepStatus::Completed)
    }

    /// One chain run: its isolated values from one evaluation of the chain
    /// program, written in step order; declines at the first non-finite value
    /// as the per-step isolators would. `None` when the chain evaluation
    /// fails, so the caller answers the run step by step and reaches the
    /// per-step outcome, including an earlier decline, exactly.
    fn eval_torn_chain(
        &self,
        sweep: &PreparedTornSweep,
        run: &PreparedTornRun,
        y: &mut [f64],
        (p, t): (&[f64], f64),
        context: RowEvalContext<'_>,
    ) -> Result<Option<TornSweepStatus>, EvalSolveError> {
        let Some(chains) = sweep.chains.as_deref() else {
            return Ok(None);
        };
        let mut values = Vec::with_capacity(run.count);
        if chains
            .eval_row_outputs_unchecked_with_context(run.chain, y, p, t, context, &mut values)
            .is_err()
            || values.len() != run.count
        {
            return Ok(None);
        }
        let steps = &sweep.steps[run.first_step..run.first_step + run.count];
        for (step, value) in steps.iter().zip(values) {
            let Some(slot) = y.get_mut(step.target_y_index).filter(|_| value.is_finite()) else {
                return Ok(Some(TornSweepStatus::Declined));
            };
            *slot = value;
        }
        Ok(Some(TornSweepStatus::Completed))
    }

    /// Backend-compilable composite of one prepared sweep, or `None` when a
    /// step cannot be expressed with the per-row path's exact decline
    /// semantics (a constant singular coefficient declines on every call) or
    /// a program is missing; the caller then keeps the interpreted sweep.
    pub fn torn_sweep_composite(&self, sweep: &PreparedTornSweep) -> Option<TornSweepComposite> {
        let mut assignment_rows = Vec::with_capacity(sweep.steps.len());
        let mut assignment_targets = Vec::with_capacity(sweep.steps.len());
        for step in &sweep.steps {
            assignment_rows.push(self.torn_step_isolator_program(step)?);
            assignment_targets.push(step.target_y_index);
        }
        let mut residual_programs = Vec::new();
        let mut residual_spans = Vec::new();
        let mut residual_outputs = Vec::with_capacity(sweep.residuals.len());
        let mut next_output = 0usize;
        for position in &sweep.residuals {
            let Some((program_row, output_offset)) = *position else {
                residual_outputs.push(None);
                continue;
            };
            let program = self.block.programs().get(program_row)?.clone();
            let span = self.block.program_span(program_row)?;
            let output_count = ScalarProgramBlock::program_output_count(&program);
            if output_offset >= output_count {
                return None;
            }
            residual_outputs.push(Some(next_output.checked_add(output_offset)?));
            next_output = next_output.checked_add(output_count)?;
            residual_programs.push(program);
            residual_spans.push(span);
        }
        let residual_block =
            ScalarProgramBlock::with_program_spans(residual_programs, residual_spans).ok()?;
        Some(TornSweepComposite {
            assignment_rows,
            assignment_targets,
            residual_block,
            residual_outputs,
        })
    }

    /// Single-output isolator program for one causal step, poisoned so a
    /// compiled schedule that cannot raise the per-row singular-coefficient
    /// error still declines at exactly the same iterates.
    fn torn_step_isolator_program(&self, step: &PreparedTornStep) -> Option<Vec<LinearOp>> {
        let row = self.block.programs().get(step.program_row)?;
        if !self.is_causal_row(step.program_row) {
            return None;
        }
        let mut program = row
            .get(..step.shape.expr_eval_len())?
            .iter()
            .filter(|op| {
                !matches!(
                    op,
                    LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        let result = AssignmentProgramBuilder::new(&mut program)?
            .materialize_poisoning_singular(&step.shape)?;
        program.push(LinearOp::StoreOutput { src: result });
        Some(program)
    }
}
