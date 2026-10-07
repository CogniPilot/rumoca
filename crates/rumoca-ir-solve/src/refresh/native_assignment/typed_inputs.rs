//! Typed host lanes of Integer and Boolean model inputs and the exact Integer
//! bindings of each stage (SPEC_0040 SOLVE-C69).
//!
//! Stage programs run on Real registers. An Integer or Boolean external input
//! therefore reaches them through a Real view in its own Solve P slot of the
//! program's private copy of P, written once per call from the input's typed
//! lane; the host's P is never written. Where an Integer value
//! only passes through (into an Integer argument cell of a pure call, or
//! straight to an Integer output) the schedule binds that sink to the `i64`
//! lane itself, so the value never takes a Binary64 hop. A Real argument cell
//! of a pure call fed by an Integer view is the authored coercion of the call
//! and rounds (IEEE 754). Every other Real view of an Integer (an input view
//! read by an untyped stage operation, or a pure-call Integer result cell) is
//! marked checked: the backend refuses a magnitude above 2^53 at run time
//! instead of rounding it.

use std::collections::{BTreeMap, BTreeSet};

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

/// The exact Integer bindings of one stage program, keyed by operation
/// position and flat (scalar-order) cell of a pure call.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct NativeIntegerBindings {
    /// Integer argument cells read from an input lane (its byte offset).
    inputs: BTreeMap<(usize, u32), usize>,
    /// Integer result cells whose Real view no operation reads.
    unread_results: BTreeSet<(usize, u32)>,
}

impl NativeRefreshAssignmentStage {
    /// The typed input lane an Integer argument cell of the pure call at
    /// `operation` reads instead of its Real register.
    #[must_use]
    pub fn integer_argument_lane(&self, operation: usize, cell: u32) -> Option<usize> {
        self.integer_bindings
            .inputs
            .get(&(operation, cell))
            .copied()
    }
    /// Whether the Real view of an Integer result cell of the pure call at
    /// `operation` is read and so must be exact.
    #[must_use]
    pub fn integer_result_checked(&self, operation: usize, cell: u32) -> bool {
        !self
            .integer_bindings
            .unread_results
            .contains(&(operation, cell))
    }
}

/// The typed input lanes of one problem and the P slots they rebind.
pub(super) struct TypedInputs {
    pub(super) lanes: Vec<NativeInputLane>,
    /// The input lane region, a multiple of 8 bytes.
    pub(super) lane_bytes: usize,
}

impl TypedInputs {
    /// Every Integer or Boolean external-input scalar, in P order, packed
    /// 8-byte lanes first, then bytes.
    pub(super) fn classify(problem: &SolveProblem) -> Result<Self, NativeEvaluationRefusal> {
        let mut lanes = Vec::new();
        for run in &problem.solve_layout.variable_storage_runs {
            let lane = match (run.role, run.value_kind) {
                (SolveVariableStorageRole::ExternalInput, SolveVariableValueKind::Integer) => {
                    NativeOutputLane::Integer
                }
                (SolveVariableStorageRole::ExternalInput, SolveVariableValueKind::Boolean) => {
                    NativeOutputLane::Boolean
                }
                _ => continue,
            };
            let crate::ScalarSlot::P { index, .. } = run.base else {
                return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
            };
            for p_index in index..index + run.scalar_count {
                lanes.push(NativeInputLane {
                    p_index,
                    lane,
                    lane_offset: 0,
                    checked: false,
                });
            }
        }
        lanes.sort_unstable_by_key(|lane| lane.p_index);
        let mut lane_bytes = 0usize;
        for wide in [true, false] {
            for lane in lanes
                .iter_mut()
                .filter(|lane| (lane.lane.width() == 8) == wide)
            {
                lane.lane_offset = lane_bytes;
                lane_bytes += lane.lane.width();
            }
        }
        // Padded so the output lanes that follow start 8-byte aligned.
        Ok(Self {
            lanes,
            lane_bytes: lane_bytes.next_multiple_of(8),
        })
    }

    /// The Integer lane offset of an Integer input's P slot.
    pub(super) fn integer_lane(&self, p_index: usize) -> Option<usize> {
        self.lanes
            .iter()
            .find(|lane| lane.p_index == p_index && lane.lane == NativeOutputLane::Integer)
            .map(|lane| lane.lane_offset)
    }
}

/// Issue each stage's exact Integer bindings and mark every Integer input
/// lane whose Real view some reader uses as checked.
pub(super) fn bind_stages(
    stages: &mut [NativeRefreshAssignmentStage],
    inputs: &mut TypedInputs,
    outputs: &[NativeDerivedOutput],
) -> Result<(), NativeEvaluationRefusal> {
    let views = inputs
        .lanes
        .iter()
        .enumerate()
        .filter(|(_, lane)| lane.lane == NativeOutputLane::Integer)
        .map(|(ordinal, lane)| (lane.p_index, ordinal))
        .collect::<BTreeMap<_, _>>();
    let mut checked = vec![false; inputs.lanes.len()];
    for stage in stages {
        let sink = match stage.source {
            NativeStageSource::Discrete { row } => outputs
                .iter()
                .find(|output| output.row == row)
                .and_then(|output| output.integer_source),
            NativeStageSource::Continuous { .. } => None,
        };
        let mut all_loads = ViewLoads {
            views: &views,
            counts: vec![0; inputs.lanes.len()],
            unbound: vec![false; inputs.lanes.len()],
        };
        let Ok(()) = all_loads.visit_compute_block(&stage.value_kernel);
        let mut flow = StageFlow {
            views: &views,
            lanes: &inputs.lanes,
            sink,
            checked: &mut checked,
            loads: vec![0; inputs.lanes.len()],
            bindings: NativeIntegerBindings::default(),
        };
        if let [ComputeNode::ScalarPrograms(block)] = stage.value_kernel.nodes.as_slice()
            && let [program] = block.programs()
        {
            flow.program(program)?;
        }
        // A P load outside the one analysed program (a nested region, a
        // family node) is an unbound reader.
        let StageFlow {
            loads, bindings, ..
        } = flow;
        for (ordinal, (&all, &bound)) in all_loads.counts.iter().zip(&loads).enumerate() {
            if all > bound || all_loads.unbound[ordinal] {
                checked[ordinal] = true;
            }
        }
        stage.integer_bindings = bindings;
    }
    for (lane, checked) in inputs.lanes.iter_mut().zip(checked) {
        lane.checked = checked;
    }
    Ok(())
}

/// Every element load of an Integer input's P slot anywhere in a stage, per
/// lane. A run-time indexed load, or any P load of a family node (whose
/// strided addresses the base row does not show), reads its lanes unbound.
struct ViewLoads<'a> {
    views: &'a BTreeMap<usize, usize>,
    counts: Vec<usize>,
    unbound: Vec<bool>,
}

impl SolveVisitor for ViewLoads<'_> {
    type Error = std::convert::Infallible;
    fn visit_linear_op(
        &mut self,
        kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        let family = matches!(
            kind,
            LinearOpSliceKind::MapBase { .. } | LinearOpSliceKind::AffineStencilBase { .. }
        );
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
            self.unbound.iter_mut().for_each(|unbound| *unbound = true);
        }
        if let LinearOp::LoadIndexedP { base, count, .. } = *op {
            for (_, &ordinal) in self.views.range(base..base.saturating_add(count)) {
                self.unbound[ordinal] = true;
            }
        }
        for (_, slot) in view_elements(op) {
            if let Some(&ordinal) = self.views.get(&slot) {
                self.counts[ordinal] += 1;
            }
        }
        Ok(())
    }
}

/// The `(register, P slot)` pairs a direct P load defines.
fn view_elements(op: &LinearOp) -> Vec<(Reg, usize)> {
    match *op {
        LinearOp::LoadP { dst, index } => vec![(dst, index)],
        LinearOp::TensorLoad {
            dst_start,
            input: crate::TensorInputKind::P,
            input_start,
            count,
            seed_start: None,
            lanes: 1,
        } => (0..count)
            .filter_map(|offset| {
                let register = Reg::try_from(offset)
                    .ok()
                    .and_then(|offset| dst_start.checked_add(offset))?;
                Some((register, input_start.checked_add(offset)?))
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Register provenance through one top-level stage program.
struct StageFlow<'a> {
    views: &'a BTreeMap<usize, usize>,
    lanes: &'a [NativeInputLane],
    /// The exact source the stage's Integer derived output publishes.
    sink: Option<NativeIntegerSource>,
    checked: &'a mut Vec<bool>,
    /// View element loads analysed here, per lane.
    loads: Vec<usize>,
    bindings: NativeIntegerBindings,
}

impl StageFlow<'_> {
    fn program(&mut self, operations: &[LinearOp]) -> Result<(), NativeEvaluationRefusal> {
        let mut registers = RegisterSources::default();
        for (position, op) in operations.iter().enumerate() {
            self.operation(&mut registers, position, op)?;
        }
        self.bindings.unread_results = registers
            .cells
            .difference(&registers.read_results)
            .copied()
            .collect();
        Ok(())
    }

    /// Follow one operation: its unbound reads, then what its writes hold.
    fn operation(
        &mut self,
        registers: &mut RegisterSources,
        position: usize,
        op: &LinearOp,
    ) -> Result<(), NativeEvaluationRefusal> {
        let bound = self.bound_reads(position, op, &registers.views);
        // An unknown read set could read any held Integer value unchecked.
        let reads = match crate::linear_op::op_read_registers(op) {
            Some(reads) => reads,
            None if registers.views.is_empty() && registers.results.is_empty() => Vec::new(),
            None => return Err(NativeEvaluationRefusal::UnresolvedTypedRead),
        };
        for register in reads
            .into_iter()
            .filter(|register| !bound.contains(register))
        {
            registers.read(register, self.checked);
        }
        registers.define(op);
        let loads = view_elements(op)
            .into_iter()
            .filter_map(|(register, slot)| Some((register, *self.views.get(&slot)?)))
            .collect::<Vec<_>>();
        for (register, ordinal) in loads {
            self.loads[ordinal] += 1;
            registers.views.insert(register, ordinal);
        }
        if let LinearOp::PureCall {
            dst_start, site, ..
        } = op
        {
            registers.call_results(position, *dst_start, site);
        }
        Ok(())
    }

    /// The registers `op` reads through a bound sink: Integer argument cells
    /// holding an Integer input view (recorded as lane bindings), and the
    /// stored register of the stage's lane-published Integer output.
    fn bound_reads(
        &mut self,
        position: usize,
        op: &LinearOp,
        views: &BTreeMap<Reg, usize>,
    ) -> BTreeSet<Reg> {
        let mut bound = BTreeSet::new();
        match op {
            LinearOp::PureCall {
                input_starts, site, ..
            } => {
                // A Real argument cell fed by the Real view of an Integer input is
                // the authored Integer-to-Real coercion of the call (MLS 3.7
                // section 10.5 coerces an Integer actual to a Real formal), so
                // it rounds per IEEE 754 and is not a checked read; the
                // Integer cells of the same call still read the lane.
                bound.extend(
                    argument_cells(input_starts, site)
                        .map(|(_, register, _)| register)
                        .filter(|register| views.contains_key(register)),
                );
                let cells = argument_cells(input_starts, site)
                    .filter(|&(_, _, integer)| integer)
                    .filter_map(|(flat, register, _)| {
                        Some((flat, register, *views.get(&register)?))
                    })
                    .collect::<Vec<_>>();
                for (flat, _, ordinal) in cells {
                    self.bindings
                        .inputs
                        .insert((position, flat), self.lanes[ordinal].lane_offset);
                }
            }
            LinearOp::StoreOutput { src } => {
                let published = match self.sink {
                    Some(NativeIntegerSource::Input { lane_offset }) => views
                        .get(src)
                        .is_some_and(|&ordinal| self.lanes[ordinal].lane_offset == lane_offset),
                    // A result cell copied to the stage's Integer lane is
                    // read exactly.
                    Some(NativeIntegerSource::CallCell { .. }) => true,
                    _ => false,
                };
                if published {
                    bound.insert(*src);
                }
            }
            _ => {}
        }
        bound
    }
}

/// The `(flat cell, register, is Integer)` triple of every argument cell of a
/// call.
fn argument_cells<'a>(
    input_starts: &'a [Reg],
    site: &'a crate::SolvePureCallSite,
) -> impl Iterator<Item = (u32, Reg, bool)> + 'a {
    let mut flat = 0u32;
    input_starts
        .iter()
        .zip(site.inputs())
        .flat_map(move |(&start, value)| {
            let first = flat;
            flat += value.scalar_count();
            let integer = matches!(value.element_type(), crate::SolveScalarType::Integer(_));
            (0..value.scalar_count())
                .filter_map(move |cell| Some((first + cell, start.checked_add(cell)?, integer)))
        })
}

/// The flat (scalar-order) Integer cells of a sequence of call values.
fn integer_cells<'a>(values: impl Iterator<Item = &'a crate::SolveValueType>) -> Vec<u32> {
    let mut cells = Vec::new();
    let mut flat = 0u32;
    for value in values {
        if matches!(value.element_type(), crate::SolveScalarType::Integer(_)) {
            cells.extend(flat..flat + value.scalar_count());
        }
        flat += value.scalar_count();
    }
    cells
}

/// What each register of one top-level program currently holds.
#[derive(Default)]
struct RegisterSources {
    /// Register -> Integer input lane ordinal whose Real view it holds.
    views: BTreeMap<Reg, usize>,
    /// Register -> Integer result cell of a call it holds.
    results: BTreeMap<Reg, (usize, u32)>,
    /// Every Integer result cell, and those whose Real view is read.
    cells: BTreeSet<(usize, u32)>,
    read_results: BTreeSet<(usize, u32)>,
}

impl RegisterSources {
    /// An unbound read of `register`.
    fn read(&mut self, register: Reg, checked: &mut [bool]) {
        if let Some(&ordinal) = self.views.get(&register) {
            checked[ordinal] = true;
        }
        if let Some(&cell) = self.results.get(&register) {
            self.read_results.insert(cell);
        }
    }

    /// Forget whatever the registers `op` writes held.
    fn define(&mut self, op: &LinearOp) {
        let Some(start) = op.dst_register() else {
            return;
        };
        for register in (0..op.dst_register_count())
            .filter_map(|offset| Reg::try_from(offset).ok()?.checked_add(start))
        {
            self.views.remove(&register);
            self.results.remove(&register);
        }
    }

    /// Record the Integer result cells of the call at `position`.
    fn call_results(&mut self, position: usize, dst_start: Reg, site: &crate::SolvePureCallSite) {
        for cell in integer_cells(site.outputs().iter().map(|output| output.value_type())) {
            if let Some(register) = dst_start.checked_add(cell) {
                self.results.insert(register, (position, cell));
                self.cells.insert((position, cell));
            }
        }
    }
}
