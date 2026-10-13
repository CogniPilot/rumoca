//! Typed host lanes of Integer and Boolean model inputs and the exact Integer
//! bindings of each stage (SPEC_0040 SOLVE-C69).
//!
//! Stage programs run on Real registers. An Integer or Boolean external input
//! therefore reaches them through a Real view in its own Solve P slot of the
//! program's P, patched once per call from the input's typed lane; the
//! entry saves the patched slots first and restores them on return, so the
//! host's P is unchanged. Where an Integer value
//! only passes through (into an Integer argument cell of a pure call, or
//! straight to an Integer output) the schedule binds that sink to the `i64`
//! lane itself, so the value never takes a Binary64 hop. A Real argument cell
//! of a pure call fed by an Integer view is the authored coercion of the call
//! and rounds (IEEE 754). Every other Real view of an Integer (an input view
//! read by an untyped stage operation, or a pure-call Integer result cell) is
//! marked checked: the backend refuses a magnitude above 2^53 at run time
//! instead of rounding it.

mod input_runs;
mod register_sources;
mod stage_flow;
use input_runs::NativeInputInventory;
pub use input_runs::{NativeInputCheck, NativeInputRun, NativeInputSelection};
use stage_flow::StageFlow;

#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use super::*;
use crate::{LinearOpSliceKind, SolveVariableStorageRole, SolveVariableValueKind, SolveVisitor};

/// One typed input lane: the value of Solve P slot `p_index` is written by
/// the host to `lane_offset` bytes into the typed lane buffer; the slot of the
/// program's private P copy holds its Real view.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeInputLane {
    p_index: usize,
    lane: NativeOutputLane,
    lane_offset: usize,
    checked: bool,
}

impl NativeInputLane {
    /// The Solve P slot the host does not write; the lane replaces it.
    #[must_use]
    pub fn p_index(&self) -> usize {
        self.p_index
    }
    /// `Integer` (`i64`) or `Boolean` (`u8`, 0 or 1).
    #[must_use]
    pub fn lane(&self) -> NativeOutputLane {
        self.lane
    }
    /// Byte offset of the lane from the start of the typed lane buffer.
    #[must_use]
    pub fn lane_offset(&self) -> usize {
        self.lane_offset
    }
    /// Whether some reader uses the Real view of an Integer input, so a
    /// magnitude above 2^53 must be refused rather than rounded.
    #[must_use]
    pub fn checked(&self) -> bool {
        self.checked
    }
}

/// One exact contiguous argument binding, issued from a direct source load.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeIntegerArgumentRun {
    pub(super) operation: usize,
    pub(super) first_cell: u32,
    pub(super) count: u32,
    pub(super) lane_offset: usize,
}
impl NativeIntegerArgumentRun {
    pub fn cells(&self) -> std::ops::Range<u32> {
        self.first_cell..self.first_cell + self.count
    }
    pub fn lane_offset(&self) -> usize {
        self.lane_offset
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct NativeIntegerBindings {
    pub(super) inputs: Vec<NativeIntegerArgumentRun>,
    pub(super) results: BTreeMap<usize, crate::IndexIntervals>,
    pub(super) read_results: BTreeMap<usize, Vec<NativeInputCheck>>,
}
impl NativeRefreshAssignmentStage {
    pub fn integer_argument_runs(
        &self,
        operation: usize,
    ) -> impl Iterator<Item = &NativeIntegerArgumentRun> {
        self.integer_bindings
            .inputs
            .iter()
            .filter(move |run| run.operation == operation)
    }
    pub fn integer_result_cells(&self, operation: usize) -> Option<&crate::IndexIntervals> {
        self.integer_bindings.results.get(&operation)
    }
    pub fn integer_result_checks(&self, operation: usize) -> Option<&[NativeInputCheck]> {
        self.integer_bindings
            .read_results
            .get(&operation)
            .map(Vec::as_slice)
    }
}

pub(super) type TypedInputs = NativeInputInventory;

pub(super) fn bind_stages(
    stages: &mut [NativeRefreshAssignmentStage],
    inputs: &mut TypedInputs,
    outputs: &[NativeDerivedOutput],
) -> Result<(), NativeEvaluationRefusal> {
    for stage in stages {
        let analysed = match stage.value_kernel.nodes.as_slice() {
            [ComputeNode::ScalarPrograms(block)] => match block.programs() {
                [program] => Some(program.as_slice()),
                _ => None,
            },
            _ => None,
        };
        let mut other_loads = ViewLoads { analysed, inputs };
        let Ok(()) = other_loads.visit_compute_block(&stage.value_kernel);
        if let Some(program) = analysed {
            let sinks = stage_integer_sinks(stage, outputs)?;
            let mut flow = StageFlow {
                inputs,
                sinks,
                stored_output: 0,
                bindings: NativeIntegerBindings::default(),
            };
            flow.program(program)?;
            stage.integer_bindings = flow.bindings;
        }
    }
    inputs.settle_views();
    Ok(())
}

/// Reads outside the exact top-level program are original unbound obligations.
/// Identity selects the same source operations analysed by StageFlow; no load
/// counts or scalar comparison inventory are needed.
struct ViewLoads<'a> {
    analysed: Option<&'a [LinearOp]>,
    inputs: &'a mut TypedInputs,
}
impl SolveVisitor for ViewLoads<'_> {
    type Error = std::convert::Infallible;
    fn visit_linear_op(
        &mut self,
        kind: LinearOpSliceKind,
        index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        let top = self
            .analysed
            .and_then(|ops| ops.get(index))
            .is_some_and(|original| std::ptr::eq(original, op));
        let family = matches!(
            kind,
            LinearOpSliceKind::MapBase { .. } | LinearOpSliceKind::AffineStencilBase { .. }
        );
        let p_read = match op {
            LinearOp::LoadP { index, .. } if !top => Some((*index, 1)),
            LinearOp::LoadIndexedP { base, count, .. } => Some((*base, *count)),
            LinearOp::TensorLoad {
                input: crate::TensorInputKind::P,
                input_start,
                count,
                seed_start,
                lanes,
                ..
            } if !top || seed_start.is_some() || *lanes != 1 => Some((*input_start, *count)),
            _ => None,
        };
        let reads_p = matches!(
            op,
            LinearOp::LoadP { .. }
                | LinearOp::LoadIndexedP { .. }
                | LinearOp::TensorLoad {
                    input: crate::TensorInputKind::P,
                    ..
                }
        );
        if family && reads_p {
            for run in self
                .inputs
                .runs
                .iter()
                .filter(|run| run.lane() == NativeOutputLane::Integer)
            {
                self.inputs.checks.push(NativeInputCheck {
                    selected: NativeInputSelection {
                        start: run.p_range().start,
                        count: run.scalar_count(),
                        stride: 1,
                    },
                    bound: Vec::new(),
                });
            }
        } else if let Some((start, count)) = p_read {
            self.inputs.checks.push(NativeInputCheck {
                selected: NativeInputSelection {
                    start,
                    count,
                    stride: 1,
                },
                bound: Vec::new(),
            });
        }
        Ok(())
    }
}

/// Bind each exact output to its canonical stored register, independently of
/// the other output kinds this stage publishes.
fn stage_integer_sinks(
    stage: &NativeRefreshAssignmentStage,
    outputs: &[NativeDerivedOutput],
) -> Result<BTreeMap<usize, NativeIntegerSource>, NativeEvaluationRefusal> {
    if !matches!(stage.source, NativeStageSource::Discrete { .. }) {
        return Ok(BTreeMap::new());
    }
    let [ComputeNode::ScalarPrograms(block)] = stage.value_kernel.nodes.as_slice() else {
        return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
    };
    let mut sinks = BTreeMap::new();
    for output in super::derived_discrete::work_range(outputs, stage.targets.span.clone()) {
        if let Some(source) = output.integer_source {
            let (_, ordinal) = block
                .output_position(output.work_index - stage.targets.span.start)
                .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
            sinks.insert(ordinal, source);
        }
    }
    Ok(sinks)
}
