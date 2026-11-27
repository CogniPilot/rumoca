//! Result leaves read by a scalar caller of a typed function.
use super::RegisterVersionProgram;
use crate::{LinearOp, SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwnerId};
use std::collections::BTreeMap;
use std::ops::Range;

/// A scalar call with at least one unread result leaf. Register reuse is
/// resolved at the original use, before associating a read with its producer.
pub struct ScalarCallDemand {
    pub owner: SolvePureCallOwnerId,
    pub directional: bool,
    pub reads: Vec<bool>,
}

impl ScalarCallDemand {
    pub fn in_program(ops: &[LinearOp]) -> BTreeMap<usize, Self> {
        let mut calls = BTreeMap::new();
        let mut leaves = BTreeMap::new();
        for (index, op) in ops.iter().enumerate() {
            let (owner, directional, start, outputs) = match op {
                LinearOp::PureCall {
                    dst_start, site, ..
                } => (site.owner(), false, *dst_start, site.outputs()),
                LinearOp::PureCallDirectional {
                    dst_start, site, ..
                } => (site.owner(), true, *dst_start, site.outputs()),
                _ => continue,
            };
            if outputs.len() < 2 {
                continue;
            }
            leaves.insert(index, output_ranges(start as usize, outputs));
            let reads = outputs
                .iter()
                .map(|result| result.kind() != SolvePureCallOutputKind::Result)
                .collect();
            calls.insert(
                index,
                Self {
                    owner,
                    directional,
                    reads,
                },
            );
        }
        if calls.is_empty() {
            return calls;
        }
        let Some(program) = RegisterVersionProgram::derive(ops) else {
            return BTreeMap::new();
        };
        for piece in program.operations().iter().flat_map(|op| op.operands()) {
            let Some(call) = calls.get_mut(&piece.producer()) else {
                continue;
            };
            let registers = piece.registers();
            for (read, range) in call.reads.iter_mut().zip(&leaves[&piece.producer()]) {
                // Keeping a leaf touched only by a stride hole is conservative.
                *read |= range.start < registers.end && registers.start < range.end;
            }
        }
        calls.retain(|_, call| call.reads.contains(&false));
        calls
    }
}

fn output_ranges(mut start: usize, outputs: &[SolvePureCallOutput]) -> Vec<Range<usize>> {
    outputs
        .iter()
        .map(|output| {
            let end = start + output.value_type().scalar_count() as usize;
            let range = start..end;
            start = end;
            range
        })
        .collect()
}
