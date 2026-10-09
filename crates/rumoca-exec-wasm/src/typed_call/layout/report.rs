//! Per-frame, per-region and per-call view of a checked scratch layout.
use super::FramePlan;
use crate::scratch_report::{ScratchCall, ScratchFrame, ScratchRegion};
use rumoca_ir_solve as solve;

impl FramePlan {
    pub(in crate::typed_call) fn report(&self, program: &solve::TypedProgram) -> ScratchFrame {
        let mut regions = Vec::new();
        let mut calls = Vec::new();
        for (index, operation) in program.operations().iter().enumerate() {
            let roles: &[&str] = match operation.operation() {
                solve::SolveOperation::Conditional { .. } => &["then", "else"],
                solve::SolveOperation::Map { .. } => &["body"],
                solve::SolveOperation::Fold { .. } => &["transition", "predicate"],
                _ => &[],
            };
            let bodies = region_bodies(operation.operation());
            for ((role, body), plan) in roles.iter().zip(bodies).zip(&self.regions[index]) {
                regions.push(ScratchRegion {
                    operation: index,
                    role,
                    frame: plan.report(body),
                });
            }
            if let (Some(frame), solve::SolveOperation::Call { owner, .. }) =
                (&self.calls[index], operation.operation())
            {
                calls.push(ScratchCall {
                    operation: index,
                    owner: owner.index() as usize,
                    input_bytes: frame.input.bytes,
                    output_bytes: frame.output.bytes,
                    scratch_bytes: frame.scratch.bytes,
                    offset_bytes: frame.input.offset,
                });
            }
        }
        ScratchFrame {
            base_bytes: self.base,
            input_bytes: self.input_bytes,
            output_bytes: self.output_bytes,
            high_water_bytes: self.scratch_bytes,
            unshared_bytes: self.unshared_bytes,
            slot_bytes: self.slot_bytes,
            register_bytes: self.register_bytes,
            register_count: self.register_count,
            largest_register_bytes: self.largest_register,
            regions,
            calls,
        }
    }
}

fn region_bodies(operation: &solve::SolveOperation) -> Vec<&solve::TypedProgram> {
    match operation {
        solve::SolveOperation::Conditional {
            if_true, if_false, ..
        } => vec![if_true.body(), if_false.body()],
        solve::SolveOperation::Map { body, .. } => vec![body.body()],
        solve::SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => std::iter::once(transition.body())
            .chain(continuation.as_deref().map(|c| c.body()))
            .collect(),
        _ => Vec::new(),
    }
}
