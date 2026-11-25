//! Pullbacks of structured control flow, on the discrete execution path taken
//! by the primal. Each loop transition owns its intermediate register tape.
use super::*;
use crate::{OutputCursor, PreparedRowEval, RowEvalScratch};
use rumoca_ir_solve::{FunctionConditionalProgram, FunctionFoldProgram};
use std::ops::Range;

pub(super) mod conditional;
mod frame;
use frame::Frame;

mod admission;
pub(super) use admission::Admission;

// A zero weight on a skipped branch must not multiply an infinite partial.
// Carry the branch mark across every frame, independently for each result.
#[derive(Clone, Copy)]
struct Weight {
    value: f64,
    untaken: bool,
}

impl Weight {
    fn add_to(self, adj: &mut [f64], untaken: &mut [bool], slot: usize) {
        adj[slot] += self.value;
        if self.untaken && adj[slot] == 0. {
            untaken[slot] = true;
        }
    }
}

#[derive(Clone)]
pub(super) struct Invocation {
    initial: Range<usize>,
    captures: Range<usize>,
    result: Range<usize>,
    frames: Vec<Frame>,
}

fn range(start: Reg, count: usize) -> Range<usize> {
    start as usize..start as usize + count
}

impl Invocation {
    pub(super) fn execute(
        op: &LinearOp,
        inputs: &ReverseInputs<'_>,
        regs: &mut [f64],
    ) -> Result<Self, EvalSolveError> {
        use LinearOp::*;
        match op {
            FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            }
            | GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
                ..
            } => {
                let enabled =
                    !matches!(op,GuardedFunctionFold {activation,..} if reg(regs,*activation)==0.);
                let initial = range(*initial_start, program.carried_count);
                let captures = range(*capture_start, program.capture_count);
                let result = range(*dst_start, program.carried_count);
                let (frames, values) = fold(
                    inputs,
                    program,
                    &regs[initial.clone()],
                    &regs[captures.clone()],
                    enabled,
                )?;
                regs[result.clone()].copy_from_slice(&values);
                Ok(Self {
                    initial,
                    captures,
                    result,
                    frames,
                })
            }
            FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => {
                let captures = range(*capture_start, program.capture_count);
                let result = range(*dst_start, program.result_count);
                let (frame, values) =
                    conditional::selected(inputs, program, &regs[captures.clone()])?;
                regs[result.clone()].copy_from_slice(&values);
                Ok(Self {
                    initial: 0..0,
                    captures,
                    result,
                    frames: vec![frame],
                })
            }
            scalar_ops!()
            | tensor_ops!()
            | region_local_ops!()
            | outside_first_order!()
            | StoreOutput { .. }
            | StoreOutputRange { .. }
            | PureCall { .. } => unreachable!("non-region passed to structured invocation"),
        }
    }

    pub(super) fn reverse(
        self,
        context: RowEvalContext<'_>,
        adj: &mut [f64],
        untaken: &mut [bool],
        cot: &mut ReverseCotangents<'_>,
    ) -> Result<(), EvalSolveError> {
        let mut weights: Vec<_> = self
            .result
            .map(|i| Weight {
                value: std::mem::take(&mut adj[i]),
                untaken: std::mem::take(&mut untaken[i]),
            })
            .collect();
        for frame in self.frames.into_iter().rev() {
            let (previous, captures) = frame.reverse(context, &weights, cot)?;
            for (slot, w) in self.captures.clone().zip(captures) {
                w.add_to(adj, untaken, slot);
            }
            weights = previous;
        }
        for (slot, w) in self.initial.zip(weights) {
            w.add_to(adj, untaken, slot);
        }
        Ok(())
    }
}

fn fold(
    inputs: &ReverseInputs<'_>,
    program: &FunctionFoldProgram,
    initial: &[f64],
    captures: &[f64],
    enabled: bool,
) -> Result<(Vec<Frame>, Vec<f64>), EvalSolveError> {
    let mut carried = initial.to_vec();
    let mut frames = Vec::new();
    if !enabled {
        return Ok((frames, carried));
    }
    let mut scratch = RowEvalScratch::default();
    for indices in program
        .domain
        .index_tuple_iter()
        .map_err(|e| unsupported(&e.to_string()))?
    {
        if let Some(test) = &program.continuation {
            let point = PreparedRowEval::new(
                &test.ops,
                test.register_count,
                inputs.y,
                inputs.p,
                inputs.t,
                inputs.context,
            )
            .with_fold_context(&carried, &[], captures);
            let mut predicate = [0.];
            crate::eval_row_prepared_fast(
                point,
                &mut scratch,
                &mut OutputCursor::new(&mut predicate),
            )?;
            if predicate[0] == 0. {
                break;
            }
        }
        let mut frame = Frame::new(program.register_count, &carried, captures)?;
        carried = frame.run(inputs, &program.update, &indices, None)?;
        frames.push(frame);
    }
    Ok((frames, carried))
}

#[cfg(test)]
mod tests;
