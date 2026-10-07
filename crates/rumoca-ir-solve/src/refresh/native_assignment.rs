//! Construction-issued compact assignment kernels for native refresh families.

mod coverage;
mod dependencies;
mod derived_discrete;
mod range;
mod rebinding;
mod scalar;
mod span_index;
mod targets;
mod typed_inputs;
mod varying_constants;

use std::ops::Range;

use super::{derive_target_assignment_shape_for_output, materialize_target_assignment};
use crate::{
    ComputeBlock, ComputeNode, LinearOp, Reg, SolveProblem, TargetAssignmentShape, TensorOutputMap,
    VarLayout,
};
pub use derived_discrete::{
    NativeDerivedOutput, NativeEvaluationRefusal, NativeIntegerSource, NativeOutputLane,
};
pub use typed_inputs::NativeInputLane;

/// The canonical Solve owner a native stage evaluates.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeStageSource {
    /// A node of the continuous implicit residual block.
    Continuous { node: usize },
    /// A stateless derived-discrete row (see [`NativeDerivedOutput`]).
    Discrete { row: usize },
}

/// An exact native value stage, retaining its canonical source identity.
#[derive(Clone, Debug)]
pub struct NativeRefreshAssignmentStage {
    integer_bindings: typed_inputs::NativeIntegerBindings,
    source: NativeStageSource,
    targets: coverage::Coverage,
    value_kernel: ComputeBlock,
}

impl NativeRefreshAssignmentStage {
    pub fn source(&self) -> NativeStageSource {
        self.source
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

/// A complete stateless value schedule derived from the canonical implicit
/// equations and the derived-discrete rows. Targets index the work space: the
/// `y` solver coordinates followed by one private slot per derived output.
/// Neither certificates nor their materialized kernels are accepted from wire.
#[derive(Clone, Debug)]
pub struct NativeRefreshAssignmentSchedule {
    stages: Vec<NativeRefreshAssignmentStage>,
    derived_outputs: Vec<NativeDerivedOutput>,
    input_lanes: Vec<NativeInputLane>,
    input_lane_bytes: usize,
    work_layout: VarLayout,
    lane_bytes: usize,
}

impl NativeRefreshAssignmentSchedule {
    pub fn stages(&self) -> &[NativeRefreshAssignmentStage] {
        &self.stages
    }
    /// Derived-discrete outputs in P-slot order, each with its typed lane.
    pub fn derived_outputs(&self) -> &[NativeDerivedOutput] {
        &self.derived_outputs
    }
    /// Typed Integer and Boolean input lanes in P-slot order (SOLVE-C69).
    pub fn input_lanes(&self) -> &[NativeInputLane] {
        &self.input_lanes
    }
    /// Size of the typed input lanes at the start of the typed lane buffer;
    /// the output lanes follow them.
    pub fn input_lane_bytes(&self) -> usize {
        self.input_lane_bytes
    }
    /// The layout stage kernels address: the problem layout with one private
    /// Y work slot per derived output after the solver coordinates.
    pub fn work_layout(&self) -> &VarLayout {
        &self.work_layout
    }
    /// Size of the typed output-lane buffer (8-byte lanes first, then bytes).
    pub fn lane_bytes(&self) -> usize {
        self.lane_bytes
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

/// Why a problem has no native value schedule: a semantic feature the
/// stateless evaluation does not own, or a construction form the schedule does
/// not certify.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativeScheduleRefusal {
    Evaluation(NativeEvaluationRefusal),
    Construction(NativeRefreshAssignmentRefusal),
}

impl std::fmt::Display for NativeScheduleRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Evaluation(refusal) => refusal.fmt(f),
            Self::Construction(refusal) => refusal.fmt(f),
        }
    }
}
impl std::error::Error for NativeScheduleRefusal {}

impl From<NativeEvaluationRefusal> for NativeScheduleRefusal {
    fn from(refusal: NativeEvaluationRefusal) -> Self {
        Self::Evaluation(refusal)
    }
}

impl From<NativeRefreshAssignmentRefusal> for NativeScheduleRefusal {
    fn from(refusal: NativeRefreshAssignmentRefusal) -> Self {
        Self::Construction(refusal)
    }
}

type Checked<T> = Result<T, NativeRefreshAssignmentRefusal>;
fn refused<T>(reason: &'static str) -> Checked<T> {
    Err(NativeRefreshAssignmentRefusal(reason))
}

struct Family {
    stage: NativeRefreshAssignmentStage,
    reads: Vec<coverage::Coverage>,
    outputs: Range<usize>,
}

/// Issue the stateless value schedule of `problem`: continuous implicit rows
/// and derived-discrete rows, every derived read rebound to its work slot.
pub(super) fn derive_for_problem(
    problem: &SolveProblem,
) -> Result<NativeRefreshAssignmentSchedule, NativeScheduleRefusal> {
    let mut inputs = typed_inputs::TypedInputs::classify(problem)?;
    let derived = derived_discrete::classify(problem, &inputs)?;
    let work_layout = problem.layout.with_private_y(derived.outputs.len()).ok_or(
        NativeRefreshAssignmentRefusal("native work layout overflows"),
    )?;
    let source = rebinding::rebind_block(
        &problem.continuous.implicit_rhs,
        &derived.rebinding,
        problem.layout.p_scalars(),
    )?;
    let mut discrete = Vec::with_capacity(derived.outputs.len());
    for output in &derived.outputs {
        discrete.push(scalar::derive_discrete(
            output,
            &problem.discrete.rhs,
            &derived.rebinding,
            &work_layout,
        )?);
    }
    let families = derive_with(
        &source,
        &problem.continuous.implicit_row_targets,
        &work_layout,
        discrete,
    )?;
    // Program registers are Real: no stage may read an Integer work slot.
    for output in derived
        .outputs
        .iter()
        .filter(|output| output.lane == NativeOutputLane::Integer)
    {
        let slot = coverage::Coverage::dense(output.work_index..output.work_index + 1);
        for read in families.iter().flat_map(|family| &family.reads) {
            if read.overlaps(&slot)? {
                return Err(NativeEvaluationRefusal::IntegerReaderRequiresTypedRegisters.into());
            }
        }
    }
    let mut stages = families
        .into_iter()
        .map(|family| family.stage)
        .collect::<Vec<_>>();
    typed_inputs::bind_stages(&mut stages, &mut inputs, &derived.outputs);
    Ok(NativeRefreshAssignmentSchedule {
        stages,
        derived_outputs: derived.outputs,
        input_lanes: inputs.lanes,
        input_lane_bytes: inputs.lane_bytes,
        work_layout,
        lane_bytes: derived.lane_bytes,
    })
}

impl NativeRefreshAssignmentSchedule {
    /// The schedule of one continuous residual block whose problem has no
    /// discrete rows: the stages [`issue_native_assignment_schedule`] issues for
    /// such a problem.
    ///
    /// [`issue_native_assignment_schedule`]: crate::issue_native_assignment_schedule
    pub fn from_continuous_block(
        source: &ComputeBlock,
        targets: &[Option<crate::ScalarSlot>],
        layout: &VarLayout,
    ) -> Result<Self, NativeRefreshAssignmentRefusal> {
        derive(source, targets, layout)
    }
}

fn derive(
    source: &ComputeBlock,
    targets: &[Option<crate::ScalarSlot>],
    layout: &VarLayout,
) -> Checked<NativeRefreshAssignmentSchedule> {
    Ok(NativeRefreshAssignmentSchedule {
        stages: derive_with(source, targets, layout, Vec::new())?
            .into_iter()
            .map(|family| family.stage)
            .collect(),
        derived_outputs: Vec::new(),
        input_lanes: Vec::new(),
        input_lane_bytes: 0,
        work_layout: layout.clone(),
        lane_bytes: 0,
    })
}

/// Issue direct assignment stages for native families, canonical scalar rows
/// and the `discrete` derived-output families. This is a deliberately bounded
/// subset: affine progression tensor targets or scalar targets, pure
/// arithmetic, and exact direct or zero isolators. Families are returned in
/// dependency order.
fn derive_with(
    source: &ComputeBlock,
    targets: &[Option<crate::ScalarSlot>],
    layout: &VarLayout,
    discrete: Vec<Family>,
) -> Checked<Vec<Family>> {
    source
        .validate_shape_contract("native refresh assignments")
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native source"))?;
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
    if !covers(
        families
            .iter()
            .map(|family| family.outputs.clone())
            .collect(),
        targets.len(),
    ) {
        return refused("native source output coverage is incomplete or overlapping");
    }
    families.extend(discrete);
    if families.is_empty() {
        return refused("native assignment inventory is empty");
    }
    if !coverage::complete(
        families.iter().map(|family| &family.stage.targets),
        layout.y_scalars(),
    )? {
        return refused(
            "native stateless targets are overlapping or do not cover the complete work layout",
        );
    }
    let order = dependency_order(&families)?;
    let mut families = families.into_iter().map(Some).collect::<Vec<_>>();
    order
        .into_iter()
        .map(|index| {
            families[index].take().ok_or(NativeRefreshAssignmentRefusal(
                "native dependency order repeats a stage",
            ))
        })
        .collect()
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
            integer_bindings: Default::default(),
            source: NativeStageSource::Continuous { node: source_node },
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

/// Operation equality with constants compared bit for bit (signed zero).
#[cfg(test)]
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
