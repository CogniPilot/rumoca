//! Every read of a derived-discrete output binds to its private work slot.
//!
//! A derived output's Solve P slot holds no value during a stateless native
//! evaluation, so a scalar `LoadP` of it becomes a `LoadY` of the work slot
//! its producing stage writes earlier in schedule order. Reads that address a
//! range (indexed, tensor, nested or compact family loads) are refused rather
//! than partially rebound.

use std::collections::BTreeMap;

use super::*;
use crate::visitor::walk_linear_op_slice;
use crate::{LinearOpSliceKind, ScalarProgramBlock, SolveVisitor, TensorInputKind};

/// `block` with every scalar read of a rebound P slot redirected to its work
/// slot; unchanged (shared) when nothing is rebound.
pub(super) fn rebind_block(
    block: &ComputeBlock,
    rebinding: &BTreeMap<usize, usize>,
    p_scalars: usize,
) -> Result<ComputeBlock, NativeScheduleRefusal> {
    if rebinding.is_empty() {
        return Ok(block.clone());
    }
    let mut nodes = Vec::with_capacity(block.nodes.len());
    for node in &block.nodes {
        nodes.push(match node {
            ComputeNode::ScalarPrograms(programs) => {
                ComputeNode::ScalarPrograms(rebind_programs(programs, rebinding)?)
            }
            ComputeNode::Map {
                domain,
                base_ops,
                load_strides,
                ..
            }
            | ComputeNode::AffineStencil {
                domain,
                base_ops,
                load_strides,
                ..
            } => {
                refuse_family_reads(domain, base_ops, load_strides, rebinding, p_scalars)?;
                node.clone()
            }
            _ => {
                if node_reads(node, rebinding) {
                    return Err(NativeEvaluationRefusal::DerivedOutputCompactRead.into());
                }
                node.clone()
            }
        });
    }
    Ok(ComputeBlock { nodes })
}

/// Programs with each top-level `LoadP` of a rebound slot redirected.
pub(super) fn rebind_programs(
    block: &ScalarProgramBlock,
    rebinding: &BTreeMap<usize, usize>,
) -> Result<ScalarProgramBlock, NativeScheduleRefusal> {
    let programs = block
        .programs()
        .iter()
        .map(|program| rebind_operations(program, rebinding))
        .collect::<Result<Vec<_>, _>>()?;
    ScalarProgramBlock::with_output_indices(
        programs,
        block.program_spans().to_vec(),
        block.output_indices().to_vec(),
    )
    .map_err(|_| NativeRefreshAssignmentRefusal("malformed rebound native program").into())
}

/// One program with each top-level `LoadP` of a rebound slot redirected; any
/// other read of a rebound slot is refused.
pub(super) fn rebind_operations(
    operations: &[LinearOp],
    rebinding: &BTreeMap<usize, usize>,
) -> Result<Vec<LinearOp>, NativeScheduleRefusal> {
    operations
        .iter()
        .map(|operation| match operation {
            LinearOp::LoadP { dst, index } => Ok(rebinding.get(index).map_or_else(
                || operation.clone(),
                |&work| LinearOp::LoadY {
                    dst: *dst,
                    index: work,
                },
            )),
            _ if reads_rebound(std::slice::from_ref(operation), rebinding) => {
                Err(NativeEvaluationRefusal::DerivedOutputCompactRead.into())
            }
            _ => Ok(operation.clone()),
        })
        .collect()
}

fn refuse_family_reads(
    domain: &rumoca_core::StructuredIndexDomain,
    operations: &[LinearOp],
    load_strides: &[crate::AffineStencilLoadStride],
    rebinding: &BTreeMap<usize, usize>,
    p_scalars: usize,
) -> Result<(), NativeScheduleRefusal> {
    let extents = domain
        .extents()
        .map_err(|_| NativeRefreshAssignmentRefusal("invalid native domain extents"))?;
    for (position, operation) in operations.iter().enumerate() {
        let range = match operation {
            LinearOp::LoadP { index, .. } => {
                let terms = load_strides
                    .iter()
                    .find(|stride| stride.op_position == position)
                    .map_or(&[][..], |stride| stride.terms.as_slice());
                address_range(*index, terms, &extents, p_scalars)?
            }
            _ if reads_rebound(std::slice::from_ref(operation), rebinding) => {
                return Err(NativeEvaluationRefusal::DerivedOutputCompactRead.into());
            }
            _ => continue,
        };
        if rebinding.range(range).next().is_some() {
            return Err(NativeEvaluationRefusal::DerivedOutputCompactRead.into());
        }
    }
    Ok(())
}

fn node_reads(node: &ComputeNode, rebinding: &BTreeMap<usize, usize>) -> bool {
    let mut reads = Reads {
        rebinding,
        found: false,
    };
    let Ok(()) = reads.visit_compute_node(0, node);
    reads.found
}

/// Whether `operations` (including nested fold and conditional programs) read
/// a rebound slot through anything but a top-level scalar `LoadP`.
fn reads_rebound(operations: &[LinearOp], rebinding: &BTreeMap<usize, usize>) -> bool {
    let mut reads = Reads {
        rebinding,
        found: false,
    };
    let Ok(()) = walk_linear_op_slice(
        &mut reads,
        LinearOpSliceKind::ScalarProgram {
            program_index: 0,
            span: None,
        },
        operations,
    );
    reads.found
}

struct Reads<'a> {
    rebinding: &'a BTreeMap<usize, usize>,
    found: bool,
}

impl SolveVisitor for Reads<'_> {
    type Error = std::convert::Infallible;
    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        let range = match op {
            LinearOp::LoadP { index, .. } => *index..*index + 1,
            LinearOp::LoadIndexedP { base, count, .. } => *base..*base + *count,
            LinearOp::TensorLoad {
                input: TensorInputKind::P,
                input_start,
                count,
                ..
            } => *input_start..*input_start + *count,
            _ => return Ok(()),
        };
        self.found |= self.rebinding.range(range).next().is_some();
        Ok(())
    }
}
