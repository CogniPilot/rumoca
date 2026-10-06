//! Construction-issued compact assignment kernels for native refresh families.

mod coverage;
mod dependencies;
mod range;
mod scalar;
mod span_index;
mod targets;
mod varying_constants;

use std::ops::Range;

use super::{derive_target_assignment_shape_for_output, materialize_target_assignment};
use crate::{
    ComputeBlock, ComputeNode, LinearOp, TargetAssignmentShape, TensorOutputMap, VarLayout,
};

#[derive(Clone, Debug, PartialEq)]
enum SourceProjection {
    Tensor {
        output_map: TensorOutputMap,
        store: LinearOp,
    },
    Scalar {
        program: usize,
        output: usize,
        stores: Vec<LinearOp>,
    },
}

/// An exact native value stage, retaining its canonical source identity.
#[derive(Clone, Debug)]
pub struct NativeRefreshAssignmentStage {
    source_node: usize,
    source_projection: SourceProjection,
    targets: coverage::Coverage,
    value_kernel: ComputeBlock,
}

impl NativeRefreshAssignmentStage {
    pub fn source_node(&self) -> usize {
        self.source_node
    }
    /// Exact owned range for dense stages. Sparse stages have no owned range.
    pub fn target_range(&self) -> Option<Range<usize>> {
        (self.targets.stride == 1).then(|| self.targets.span.clone())
    }
    /// Bounding storage span; gaps belong to other stages, not this stage.
    pub fn target_span(&self) -> Range<usize> {
        self.targets.span.clone()
    }
    pub fn target_count(&self) -> usize {
        self.targets.count
    }
    pub fn target_stride(&self) -> usize {
        self.targets.stride
    }
    pub fn target_block_width(&self) -> usize {
        self.targets.width
    }
    pub fn targets_overlap(&self, other: &Self) -> Result<bool, NativeRefreshAssignmentRefusal> {
        self.targets.overlaps(&other.targets)
    }
    pub fn value_kernel(&self) -> &ComputeBlock {
        &self.value_kernel
    }
}

/// A complete family schedule derived from the canonical implicit equations.
/// Neither certificates nor their materialized kernels are accepted from wire.
#[derive(Clone, Debug)]
pub struct NativeRefreshAssignmentSchedule {
    stages: Vec<NativeRefreshAssignmentStage>,
}

impl NativeRefreshAssignmentSchedule {
    pub fn stages(&self) -> &[NativeRefreshAssignmentStage] {
        &self.stages
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeRefreshAssignmentRefusal(pub(super) &'static str);

impl std::fmt::Display for NativeRefreshAssignmentRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}
impl std::error::Error for NativeRefreshAssignmentRefusal {}

type Checked<T> = Result<T, NativeRefreshAssignmentRefusal>;
fn refused<T>(reason: &'static str) -> Checked<T> {
    Err(NativeRefreshAssignmentRefusal(reason))
}

struct Family {
    stage: NativeRefreshAssignmentStage,
    reads: Vec<coverage::Coverage>,
    outputs: Range<usize>,
}

/// Issue direct assignment stages for native families and canonical scalar rows.
/// This is a deliberately bounded subset: affine progression tensor targets or
/// scalar targets, pure arithmetic, and exact direct or zero isolators.
pub(super) fn derive(
    source: &ComputeBlock,
    targets: &[Option<crate::ScalarSlot>],
    layout: &VarLayout,
) -> Checked<NativeRefreshAssignmentSchedule> {
    source
        .validate_shape_contract("native refresh assignments")
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native source"))?;
    if source.nodes.is_empty() {
        return refused("native assignment inventory is empty");
    }
    let outputs = super::source_outputs::SourceOutputs::new(source)
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native output inventory"))?;
    let mut families = Vec::new();
    for (node, family) in source.nodes.iter().enumerate() {
        if let ComputeNode::ScalarPrograms(programs) = family {
            families.extend(scalar::derive(node, programs, &outputs, targets, layout)?);
        } else {
            families.push(derive_family(node, family, targets, layout)?);
        }
    }
    if families.is_empty() {
        return refused("native assignment inventory is empty");
    }
    if !covers(
        families
            .iter()
            .map(|family| family.outputs.clone())
            .collect(),
        targets.len(),
    ) {
        return refused("native source output coverage is incomplete or overlapping");
    }
    if !coverage::complete(
        families.iter().map(|family| &family.stage.targets),
        layout.y_scalars(),
    )? {
        return refused(
            "native stateless targets are overlapping or do not cover the complete Y layout",
        );
    }
    let order = dependency_order(&families)?;
    let stages = order
        .into_iter()
        .map(|index| {
            std::mem::replace(
                &mut families[index].stage,
                NativeRefreshAssignmentStage {
                    source_node: 0,
                    source_projection: SourceProjection::Tensor {
                        output_map: TensorOutputMap {
                            start: 0,
                            strides: Vec::new(),
                        },
                        store: LinearOp::StoreOutput { src: 0 },
                    },
                    targets: coverage::Coverage::dense(0..0),
                    value_kernel: ComputeBlock::default(),
                },
            )
        })
        .collect();
    Ok(NativeRefreshAssignmentSchedule { stages })
}

fn dependency_order(families: &[Family]) -> Checked<Vec<usize>> {
    dependencies::order(families)
}

fn overlaps(a: &Range<usize>, b: &Range<usize>) -> bool {
    a.start < b.end && b.start < a.end
}

fn covers(mut ranges: Vec<Range<usize>>, count: usize) -> bool {
    ranges.sort_by_key(|range| range.start);
    let mut cursor = 0;
    for range in ranges {
        if range.start != cursor {
            return false;
        }
        cursor = range.end;
    }
    cursor == count
}

// SPEC_0021: Exception - one certificate binds the source prefix, isolator, target map, and affine reads.
#[allow(clippy::too_many_lines)]
fn derive_family(
    source_node: usize,
    node: &ComputeNode,
    targets: &[Option<crate::ScalarSlot>],
    layout: &VarLayout,
) -> Checked<Family> {
    let (domain, output_map, operations, load_strides, const_strides) = match node {
        ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            ..
        }
        | ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            ..
        } => (domain, output_map, base_ops, load_strides, const_strides),
        _ => return refused("native assignment source contains an unsupported node family"),
    };
    let dense = TensorOutputMap::dense_contiguous(output_map.start, domain)
        .map_err(|_| NativeRefreshAssignmentRefusal("invalid native assignment domain"))?;
    if dense != *output_map {
        return refused("native assignment output map is not dense");
    }
    let count = domain
        .scalar_count()
        .map_err(|_| NativeRefreshAssignmentRefusal("invalid native domain cardinality"))?;
    let mut strided_operations = std::collections::BTreeSet::new();
    if load_strides
        .iter()
        .any(|stride| !strided_operations.insert(stride.op_position))
    {
        return refused("native assignment requires one affine stride record per source operation");
    }
    let output_end = output_map
        .start
        .checked_add(count)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native output range overflows",
        ))?;
    let selected =
        targets
            .get(output_map.start..output_end)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native output range exceeds target inventory",
            ))?;
    let extents = domain
        .extents()
        .map_err(|_| NativeRefreshAssignmentRefusal("invalid native domain extents"))?;
    let target_map = targets::derive(selected, &extents, layout.y_scalars())?;
    let target_coverage = target_map.coverage.clone();
    let shape = derive_target_assignment_shape_for_output(operations, 0, target_map.first).ok_or(
        NativeRefreshAssignmentRefusal("native family has no exact target isolator"),
    )?;
    if !const_strides.is_empty()
        && !varying_constants::fixed_target_coefficient(operations, &shape, target_map.first)
    {
        return refused(
            "native assignment varying constants require an unsupported coefficient proof",
        );
    }
    if !matches!(
        shape,
        TargetAssignmentShape::Direct { .. } | TargetAssignmentShape::Zero { .. }
    ) {
        return refused("native family requires an unsupported non-direct isolator");
    }
    let mut reads = Vec::new();
    let store = operations
        .iter()
        .position(|operation| matches!(operation, LinearOp::StoreOutput { .. }))
        .ok_or(NativeRefreshAssignmentRefusal(
            "native family has no scalar output",
        ))?;
    if operations.len() != store + 1 {
        return refused("native family has multiple or nonterminal outputs");
    }
    let has_calls = operations[..store]
        .iter()
        .any(|op| matches!(op, LinearOp::PureCall { .. }));
    let mut load_dependencies = vec![None; store];
    for (position, operation) in operations[..store].iter().enumerate() {
        let terms = load_strides
            .iter()
            .find(|stride| stride.op_position == position)
            .map_or(&[][..], |stride| stride.terms.as_slice());
        let (base, capacity, is_y) = match operation {
            LinearOp::LoadY { index, .. } => (*index, layout.y_scalars(), true),
            LinearOp::LoadP { index, .. } => (*index, layout.p_scalars(), false),
            LinearOp::Const { value, .. } if !value.is_finite() => {
                return refused("native assignment contains a nonfinite constant");
            }
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::PureCall { .. } => continue,
            _ => {
                return refused(
                    "native family contains unsupported or effectful scalar operations",
                );
            }
        };
        let range = address_range(base, terms, &extents, capacity)?;
        let read = coverage::reads(range, terms, &extents)?;
        let dependent = is_y && read.overlaps(&target_coverage)?;
        load_dependencies[position] = Some(dependent);
        if dependent {
            if base != target_map.first || terms != target_map.output.strides {
                return refused("native family reads coupled targets in its own assignment range");
            }
        } else if is_y {
            reads.push(read);
        }
    }
    if has_calls {
        range::checked_family_call_inputs(&operations[..store], &load_dependencies)?;
    }
    // Direct isolation selects the already evaluated independent value after
    // retaining every operation in the original source prefix.
    let mut final_operations = operations[..store].to_vec();
    let (value, _) = materialize_target_assignment(&shape, &mut final_operations).ok_or(
        NativeRefreshAssignmentRefusal("native assignment materialization overflows"),
    )?;
    final_operations.push(LinearOp::StoreOutput { src: value });
    let mut final_node = node.clone();
    match &mut final_node {
        ComputeNode::Map {
            output_map,
            base_ops,
            ..
        }
        | ComputeNode::AffineStencil {
            output_map,
            base_ops,
            ..
        } => {
            *output_map = target_map.output;
            *base_ops = final_operations;
        }
        _ => return refused("native value projection requires a Map or AffineStencil source"),
    }
    Ok(Family {
        stage: NativeRefreshAssignmentStage {
            source_node,
            // These are the only original projection facts replaced by the
            // final value kernel. Retain them to bind replay to the complete
            // canonical source, including equivalent residual sign changes.
            source_projection: SourceProjection::Tensor {
                output_map: output_map.clone(),
                store: operations[store].clone(),
            },
            targets: target_coverage,
            value_kernel: ComputeBlock {
                nodes: vec![final_node],
            },
        },
        reads,
        outputs: output_map.start..output_end,
    })
}

fn address_range(
    base: usize,
    terms: &[crate::AffineStencilIndexStrideTerm],
    extents: &[usize],
    capacity: usize,
) -> Checked<Range<usize>> {
    let mut low = base as i128;
    let mut high = low;
    for term in terms {
        let extent = extents
            .get(term.dimension)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native load names an invalid domain dimension",
            ))?;
        let shift = (term.stride as i128)
            .checked_mul(extent.saturating_sub(1) as i128)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native affine address overflows",
            ))?;
        low = low
            .checked_add(shift.min(0))
            .ok_or(NativeRefreshAssignmentRefusal(
                "native affine address overflows",
            ))?;
        high = high
            .checked_add(shift.max(0))
            .ok_or(NativeRefreshAssignmentRefusal(
                "native affine address overflows",
            ))?;
    }
    if low < 0 || high >= capacity as i128 {
        return refused("native affine address exceeds its owned variable layout");
    }
    Ok(low as usize..high as usize + 1)
}

pub(super) fn matches(
    a: &NativeRefreshAssignmentSchedule,
    b: &NativeRefreshAssignmentSchedule,
) -> bool {
    a.stages.len() == b.stages.len()
        && a.stages.iter().zip(&b.stages).all(|(a, b)| {
            a.source_node == b.source_node
                && a.source_projection == b.source_projection
                && a.targets == b.targets
                && node_matches(&a.value_kernel.nodes[0], &b.value_kernel.nodes[0])
        })
}

fn node_matches(a: &ComputeNode, b: &ComputeNode) -> bool {
    match (a, b) {
        (ComputeNode::ScalarPrograms(a), ComputeNode::ScalarPrograms(b)) => {
            a.program_spans() == b.program_spans()
                && a.output_indices() == b.output_indices()
                && a.programs().len() == b.programs().len()
                && a.programs()
                    .iter()
                    .zip(b.programs())
                    .all(|(a, b)| operations_match(a, b))
        }
        (
            ComputeNode::Map {
                domain: ad,
                output_map: ao,
                base_ops: ab,
                load_strides: al,
                const_strides: ac,
                metadata: am,
                span: aspan,
            },
            ComputeNode::Map {
                domain: bd,
                output_map: bo,
                base_ops: bb,
                load_strides: bl,
                const_strides: bc,
                metadata: bm,
                span: bspan,
            },
        )
        | (
            ComputeNode::AffineStencil {
                domain: ad,
                output_map: ao,
                base_ops: ab,
                load_strides: al,
                const_strides: ac,
                metadata: am,
                span: aspan,
            },
            ComputeNode::AffineStencil {
                domain: bd,
                output_map: bo,
                base_ops: bb,
                load_strides: bl,
                const_strides: bc,
                metadata: bm,
                span: bspan,
            },
        ) => (ad, ao, al, ac, am, aspan) == (bd, bo, bl, bc, bm, bspan) && operations_match(ab, bb),
        _ => false,
    }
}

fn operations_match(a: &[LinearOp], b: &[LinearOp]) -> bool {
    a.len() == b.len()
        && a.iter().zip(b).all(|(a, b)| match (a, b) {
            (LinearOp::Const { dst: ad, value: av }, LinearOp::Const { dst: bd, value: bv }) => {
                ad == bd && av.to_bits() == bv.to_bits()
            }
            _ => a == b,
        })
}

#[cfg(test)]
mod tests;
