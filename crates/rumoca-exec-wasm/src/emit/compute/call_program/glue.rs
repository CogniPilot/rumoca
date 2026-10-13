//! Stage glue between a program's register file and the large Binary64 Real
//! values of its root calls.
//!
//! A root owner takes a large input as the address of its value and returns a
//! large output as the address of a buffer the caller names. Registers live in
//! the private arena, a separate memory, so a callee cannot read them: the
//! glue copies such an input into a staging buffer, unless its registers are
//! exactly a tensor load of Y or P, in which case the callee reads Y or P
//! itself and the load is not emitted when nothing else reads it. An output
//! whose every reader is one store of exactly its registers is built in the
//! stored Y cells, so the result is neither unpacked into registers nor
//! stored again. These decisions are made once, from the register versions of
//! the issued program, when the stage is emitted.
use super::layout::CommitPiece;
use super::*;
use crate::typed_call::program::{CallAbi, Demand};
use rumoca_ir_solve::{RegisterVersionProgram, TensorInputKind};
use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;

/// Bytes of staging buffers one call may need: one for every input passed by
/// reference and one for every output returned by reference.
pub(super) fn staging_bytes(abi: &CallAbi) -> Result<u32, String> {
    abi.inputs
        .iter()
        .filter(|input| input.by_reference)
        .map(|input| input.bytes)
        .chain(
            abi.outputs
                .iter()
                .filter(|output| output.buffer_cell.is_some())
                .map(|output| output.bytes),
        )
        .try_fold(0u32, u32::checked_add)
        .ok_or_else(|| "native call staging overflows".into())
}

/// Offsets from the start of the call input region of the staging buffer of
/// each input and each output (zero where there is none): after the tuple,
/// the inputs in order, then the outputs in order.
pub(super) struct Staging {
    pub inputs: Vec<u32>,
    pub outputs: Vec<u32>,
}

impl Staging {
    pub(super) fn of(abi: &CallAbi, tuple_bytes: u32) -> Result<Self, String> {
        let mut next = tuple_bytes;
        let mut place = |bytes: u32| -> Result<u32, String> {
            let at = next;
            next = next
                .checked_add(bytes)
                .ok_or("native call staging overflows")?;
            Ok(at)
        };
        let inputs = abi
            .inputs
            .iter()
            .map(|input| input.by_reference.then(|| place(input.bytes)).transpose())
            .map(|at| at.map(Option::unwrap_or_default))
            .collect::<Result<Vec<_>, _>>()?;
        let outputs = abi
            .outputs
            .iter()
            .map(|output| {
                output
                    .buffer_cell
                    .map(|_| place(output.bytes))
                    .transpose()
                    .map(Option::unwrap_or_default)
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { inputs, outputs })
    }
}

/// An input whose cells a callee can read where the host holds them.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::emit) enum HostRun {
    /// The registers are exactly cells of host Y or P.
    Cells {
        input: TensorInputKind,
        first_cell: usize,
    },
    /// The cells are bound to a typed Integer input lane (SOLVE-C69), whose
    /// `i64` cells are the callee's own: the bytes from this offset of the
    /// typed lane buffer.
    Lane { offset: u32 },
}

/// How one call receives its inputs and leaves its outputs.
#[derive(Debug, Default)]
pub(in crate::emit) struct CallGlue {
    /// Per input: the host cells the callee reads in place.
    pub inputs: Vec<Option<HostRun>>,
    /// Per output: the first Y cell, relative to the stage targets, the callee
    /// builds it in.
    pub outputs: Vec<Option<usize>>,
    /// Per output built in place: the pieces of carried runs (SOLVE-C101) it
    /// holds that the store may leave unwritten.
    pub carried: Vec<Vec<CommitPiece>>,
    /// Per output: no operation reads its registers, so it is never moved to
    /// them.
    pub unread: Vec<bool>,
}

/// The glue of every root call of one stage program, and the operations the
/// glue makes unnecessary.
#[derive(Debug, Default)]
pub(in crate::emit) struct StageGlue {
    calls: BTreeMap<usize, CallGlue>,
    skipped: BTreeSet<usize>,
}

impl StageGlue {
    pub(in crate::emit) fn call(&self, operation: usize) -> Option<&CallGlue> {
        self.calls.get(&operation)
    }

    /// Whether the operation is a tensor load or a store the glue replaces.
    pub(in crate::emit) fn skips(&self, operation: usize) -> bool {
        self.skipped.contains(&operation)
    }

    /// Derive the glue of `program`, whose stores consume `targets` in order
    /// and write the Y cells from `stage_start`.
    pub(in crate::emit) fn derive(
        program: &[LinearOp],
        plan: &CallProgramPlan,
        (stage_start, stage): (usize, Option<&solve::NativeRefreshAssignmentStage>),
        targets: &[usize],
    ) -> Result<Self, String> {
        let Some(versions) = RegisterVersionProgram::derive(program) else {
            return Ok(Self::default());
        };
        let placements = store_placements(program, targets);
        let mut glue = Self::default();
        let mut glued = BTreeSet::new();
        let mut calls = Vec::new();
        for (index, operation) in program.iter().enumerate() {
            if let LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } = operation
            {
                let abi = plan.call(site)?.abi;
                let inputs =
                    host_runs(program, &versions, (index, stage), input_starts, site, &abi);
                calls.push(PlannedCall {
                    index,
                    dst_start: *dst_start,
                    input_starts,
                    site,
                    abi,
                    inputs,
                });
            }
        }
        for call in &calls {
            let index = call.index;
            // An output is built early in its Y cells, so no call that runs
            // from here on may read those cells in place.
            let outputs = in_place_outputs(
                (program, &versions, index),
                call.dst_start,
                (call.site, &call.abi),
                &host_y_cells(&calls, index),
                (stage_start, &placements),
            );
            glued.extend(
                call.inputs
                    .iter()
                    .zip(call.input_starts)
                    .filter(|(run, _)| run.is_some())
                    .map(|(_, start)| (index, *start as usize)),
            );
            glue.skipped
                .extend(outputs.iter().flatten().map(|(store, _)| *store));
            let carried = carried_outputs(plan, call.site, &outputs, stage_start);
            glue.calls.insert(
                index,
                CallGlue {
                    inputs: call.inputs.clone(),
                    outputs: outputs.iter().map(|o| o.map(|(_, cell)| cell)).collect(),
                    carried,
                    unread: unread_outputs(&versions, index, call.dst_start, call.site),
                },
            );
        }
        glue.skipped
            .extend(unread_loads(program, &versions, &glue.calls, &glued));
        Ok(glue)
    }
}

/// Per output built in place: the pieces of carried runs its cells hold, when
/// the program publishes them into P and nothing reads them from Y. The entry
/// then commits a piece only if the call did not return its paired P cells
/// unchanged.
fn carried_outputs(
    plan: &CallProgramPlan,
    site: &solve::SolvePureCallSite,
    outputs: &[Option<(usize, usize)>],
    stage_start: usize,
) -> Vec<Vec<CommitPiece>> {
    let work_bytes = |first: usize, count: usize| {
        let bytes = |cell: usize| u32::try_from(cell.checked_mul(8)?).ok();
        Some(bytes(first)?..bytes(first.checked_add(count)?)?)
    };
    site.outputs()
        .iter()
        .zip(outputs)
        .map(|(output, placed)| {
            let count = output.value_type().scalar_count() as usize;
            placed
                .and_then(|(_, first)| work_bytes(stage_start + first, count))
                .map_or_else(Vec::new, |y| plan.view().commit_pieces(&y))
        })
        .collect()
}

/// The outputs of each root owner that some stage reads, by owner index
/// (SOLVE-C95): those whose registers a top-level call of a scalar stage
/// program reads, by the same register versions the glue derives its `unread`
/// flags from, and the cells a discrete stage captures as Integer lanes. A
/// root with any other call occurrence (inside a conditional, or in a program
/// without register versions) is read in full, so an owner is absent from the
/// answer unless every occurrence was analysed.
pub(super) fn root_demands(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    plans: &[KernelPlan<'_>],
    sites: &[solve::SolvePureCallSite],
) -> BTreeMap<u32, Demand> {
    let mut reads = RootReads::default();
    for (plan, stage) in plans.iter().zip(schedule.stages()) {
        let captured = captured_cells(schedule, stage);
        let programs = plan.nodes.iter().filter_map(|node| match node {
            NodePlan::Scalar { block, .. } => Some(block.programs()),
            _ => None,
        });
        for program in programs.flatten() {
            reads.add_program(program, &captured);
        }
    }
    reads.demands(sites)
}

/// The outputs of each root owner read so far, with the number of call
/// occurrences those reads were derived from.
#[derive(Default)]
struct RootReads(BTreeMap<u32, (usize, Vec<bool>)>);

impl RootReads {
    fn add_program(&mut self, program: &[LinearOp], captured: &[(usize, u32)]) {
        let Some(versions) = RegisterVersionProgram::derive(program) else {
            return;
        };
        for (index, operation) in program.iter().enumerate() {
            if let LinearOp::PureCall {
                dst_start, site, ..
            } = operation
            {
                let unread = unread_outputs(&versions, index, *dst_start, site);
                self.add_call(site, &unread, captured, index);
            }
        }
    }

    fn add_call(
        &mut self,
        site: &solve::SolvePureCallSite,
        unread: &[bool],
        captured: &[(usize, u32)],
        index: usize,
    ) {
        let (count, read) = self
            .0
            .entry(site.owner().index())
            .or_insert_with(|| (0, vec![false; unread.len()]));
        *count += 1;
        for (read, unread) in read.iter_mut().zip(unread) {
            *read |= !unread;
        }
        let cells = captured.iter().filter(|(call, _)| *call == index);
        for output in cells.filter_map(|(_, cell)| output_of_cell(site, *cell)) {
            read[output] = true;
        }
    }

    fn demands(self, sites: &[solve::SolvePureCallSite]) -> BTreeMap<u32, Demand> {
        let mut occurrences = BTreeMap::<u32, usize>::new();
        for site in sites {
            *occurrences.entry(site.owner().index()).or_default() += 1;
        }
        self.0
            .into_iter()
            .filter(|(owner, (count, _))| occurrences.get(owner) == Some(count))
            .map(|(owner, (_, read))| (owner, Demand::of(read)))
            .collect()
    }
}

/// The `(operation, cell)` call cells a stage captures as Integer lanes.
fn captured_cells(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    stage: &solve::NativeRefreshAssignmentStage,
) -> Vec<(usize, u32)> {
    if !matches!(stage.source(), solve::NativeStageSource::Discrete { .. }) {
        return Vec::new();
    }
    schedule
        .derived_outputs_in(stage.target_span())
        .iter()
        .filter_map(|output| match output.integer_source() {
            Some(solve::NativeIntegerSource::CallCell { operation, cell }) => {
                Some((operation, cell))
            }
            _ => None,
        })
        .collect()
}

/// The output of `site` that holds flat result cell `cell`.
fn output_of_cell(site: &solve::SolvePureCallSite, cell: u32) -> Option<usize> {
    let mut end = 0u32;
    site.outputs().iter().position(|output| {
        end += output.value_type().scalar_count();
        cell < end
    })
}

/// Per output of a call: whether no operation reads any of its registers.
fn unread_outputs(
    versions: &RegisterVersionProgram,
    call: usize,
    dst_start: Reg,
    site: &solve::SolvePureCallSite,
) -> Vec<bool> {
    let mut starts = Vec::with_capacity(site.outputs().len());
    let mut next = dst_start as usize;
    for output in site.outputs() {
        starts.push(next);
        next += output.value_type().scalar_count() as usize;
    }
    let mut read = vec![false; starts.len()];
    for operation in versions.operations().iter().skip(call + 1) {
        for piece in operation
            .operands()
            .iter()
            .filter(|piece| piece.producer() == call)
        {
            let registers = piece.registers();
            let first = starts
                .partition_point(|start| *start <= registers.start)
                .saturating_sub(1);
            let last = starts.partition_point(|start| *start < registers.end);
            read[first..last].fill(true);
        }
    }
    read.into_iter().map(|read| !read).collect()
}

/// The first target of every contiguous ranged store, by operation index.
fn store_placements(program: &[LinearOp], targets: &[usize]) -> BTreeMap<usize, usize> {
    let mut cursor = 0usize;
    let mut placed = BTreeMap::new();
    for (index, operation) in program.iter().enumerate() {
        match operation {
            LinearOp::StoreOutput { .. } => cursor += 1,
            LinearOp::StoreOutputRange { count, .. } => {
                let run = targets.get(cursor..cursor.saturating_add(*count));
                if let Some([first, ..]) = run
                    && run.is_some_and(|run| {
                        run.iter()
                            .enumerate()
                            .all(|(i, target)| *target == first + i)
                    })
                {
                    placed.insert(index, *first);
                }
                cursor = cursor.saturating_add(*count);
            }
            _ => {}
        }
    }
    placed
}

/// The typed lane bytes an Integer input of a call is bound to: the `i64` cells
/// of the input are the `count` consecutive lane cells of one bound run, as the
/// callee reads them (SOLVE-C69).
fn lane_run(
    stage: Option<&solve::NativeRefreshAssignmentStage>,
    call: usize,
    site: &solve::SolvePureCallSite,
    k: usize,
) -> Option<HostRun> {
    let value = site.inputs().get(k)?;
    if !matches!(value.element_type(), solve::SolveScalarType::Integer(_)) {
        return None;
    }
    let first: u32 = site.inputs()[..k]
        .iter()
        .map(solve::SolveValueType::scalar_count)
        .sum();
    let cells = first..first.checked_add(value.scalar_count())?;
    let run = stage?
        .integer_argument_runs(call)
        .find(|run| run.cells().start <= cells.start && cells.end <= run.cells().end)?;
    let offset = (cells.start - run.cells().start)
        .checked_mul(crate::cost_model::LANE_BYTES)?
        .checked_add(u32::try_from(run.lane_offset()).ok()?)?;
    Some(HostRun::Lane { offset })
}

/// Per input of a call: the host cells its registers are exactly, or the typed
/// lane it is bound to.
fn host_runs(
    program: &[LinearOp],
    versions: &RegisterVersionProgram,
    (call, stage): (usize, Option<&solve::NativeRefreshAssignmentStage>),
    input_starts: &[Reg],
    site: &solve::SolvePureCallSite,
    abi: &CallAbi,
) -> Vec<Option<HostRun>> {
    let candidate = |k: usize| -> Option<HostRun> {
        let value = site.inputs().get(k)?;
        let start = *input_starts.get(k)? as usize;
        let count = value.scalar_count() as usize;
        if !abi.inputs.get(k)?.by_reference {
            return None;
        }
        if let Some(lane) = lane_run(stage, call, site, k) {
            return Some(lane);
        }
        let piece = versions.operations()[call]
            .operands()
            .iter()
            .find(|piece| {
                piece.selection() == (start, 1) && piece.registers() == (start..start + count)
            })?;
        let LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            seed_start: None,
            lanes: 1,
            ..
        } = program[piece.producer()]
        else {
            return None;
        };
        Some(HostRun::Cells {
            input,
            first_cell: input_start.checked_add(start.checked_sub(dst_start as usize)?)?,
        })
    };
    let runs = (0..input_starts.len()).map(candidate).collect::<Vec<_>>();
    // Inputs that share their registers are all glued or none is.
    runs.iter()
        .enumerate()
        .map(|(k, run)| {
            let shared_unglued = runs
                .iter()
                .zip(input_starts)
                .any(|(other, start)| other.is_none() && *start == input_starts[k]);
            run.filter(|_| !shared_unglued)
        })
        .collect()
}

/// Per output returned by reference: the store that is its only reader and the
/// first Y cell, relative to the stage targets, the callee can build it in.
fn in_place_outputs(
    (program, versions, call): (&[LinearOp], &RegisterVersionProgram, usize),
    dst_start: Reg,
    (site, abi): (&solve::SolvePureCallSite, &CallAbi),
    host_cells: &[Range<usize>],
    (stage_start, placements): (usize, &BTreeMap<usize, usize>),
) -> Vec<Option<(usize, usize)>> {
    let mut register = dst_start as usize;
    site.outputs()
        .iter()
        .zip(&abi.outputs)
        .map(|(output, returned)| {
            let count = output.value_type().scalar_count() as usize;
            let range = register..register + count;
            register += count;
            returned.buffer_cell?;
            let (store, first) = sole_store(program, versions, call, &range, placements)?;
            let target = stage_start + first..stage_start + first + count;
            let overlaps =
                |cells: &Range<usize>| cells.start < target.end && target.start < cells.end;
            let early_read =
                host_cells.iter().any(overlaps) || reads_y_between(program, call, store, &target);
            (!early_read).then_some((store, first))
        })
        .collect()
}

/// The one ranged store that reads exactly `range` and nothing else reads, and
/// the first target it writes.
fn sole_store(
    program: &[LinearOp],
    versions: &RegisterVersionProgram,
    call: usize,
    range: &Range<usize>,
    placements: &BTreeMap<usize, usize>,
) -> Option<(usize, usize)> {
    let mut store = None;
    for (index, operation) in versions.operations().iter().enumerate().skip(call + 1) {
        for piece in operation.operands().iter().filter(|piece| {
            piece.producer() == call
                && piece.registers().start < range.end
                && range.start < piece.registers().end
        }) {
            let exact = matches!(
                program[index],
                LinearOp::StoreOutputRange { start, count, stride: 1 }
                    if start as usize == range.start && count == range.len()
            ) && piece.registers() == *range;
            if !exact || store.is_some_and(|seen| seen != index) {
                return None;
            }
            store = Some(index);
        }
    }
    let store = store?;
    Some((store, *placements.get(&store)?))
}

/// Whether an operation between a call and its store may read the Y cells the
/// call would write early.
fn reads_y_between(program: &[LinearOp], call: usize, store: usize, cells: &Range<usize>) -> bool {
    program[call + 1..store]
        .iter()
        .any(|operation| may_read_y(operation, cells))
}

/// Whether `operation` may read a Y cell of `cells`. Every operation is
/// classified: those that read Y directly say which cells; an operation with a
/// nested body reads whatever the body does, so it is taken to read all of Y;
/// the others are listed as reading only registers, P, time and their own
/// state. A call reads its inputs from registers or from the cells the glue
/// reads in place (SOLVE-C91), which `StageGlue::derive` accounts for apart.
fn may_read_y(operation: &LinearOp, cells: &Range<usize>) -> bool {
    match operation {
        LinearOp::LoadY { index, .. } => cells.contains(index),
        LinearOp::TensorLoad {
            input: TensorInputKind::Y,
            input_start,
            count,
            ..
        } => *input_start < cells.end && cells.start < input_start.saturating_add(*count),
        LinearOp::FunctionConditional { .. }
        | LinearOp::FunctionFold { .. }
        | LinearOp::GuardedFunctionFold { .. }
        | LinearOp::StoreOutputFunctionFold { .. }
        | LinearOp::StoreOutputFoldTensorUpdate { .. } => true,
        LinearOp::TensorLoad {
            input: TensorInputKind::P,
            ..
        }
        | LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadIndexedP { .. }
        | LinearOp::LoadIndexedRegister { .. }
        | LinearOp::LoadIndexedFoldCarried { .. }
        | LinearOp::LoadIndexedFoldCapture { .. }
        | LinearOp::LoadSeed { .. }
        | LinearOp::LoadFoldCarried { .. }
        | LinearOp::LoadFoldIndex { .. }
        | LinearOp::LoadFoldCapture { .. }
        | LinearOp::LoadFunctionConditionalCapture { .. }
        | LinearOp::LoadFunctionConditionalCaptureRange { .. }
        | LinearOp::LoadIndexedSeed { .. }
        | LinearOp::Move { .. }
        | LinearOp::LinearSolveComponent { .. }
        | LinearOp::DotProduct { .. }
        | LinearOp::MatrixMultiply { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorCross { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorUpdate { .. }
        | LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TableBounds { .. }
        | LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. }
        | LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. }
        | LinearOp::Unary { .. }
        | LinearOp::Binary { .. }
        | LinearOp::Compare { .. }
        | LinearOp::Select { .. }
        | LinearOp::PureCall { .. }
        | LinearOp::PureCallObservation { .. }
        | LinearOp::PureCallDirectional { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutput { .. } => false,
    }
}

/// The Y cells that the calls from operation `from` on read in place.
fn host_y_cells(calls: &[PlannedCall<'_>], from: usize) -> Vec<Range<usize>> {
    calls
        .iter()
        .filter(|call| call.index >= from)
        .flat_map(|call| {
            call.site
                .inputs()
                .iter()
                .zip(&call.inputs)
                .filter_map(|(value, run)| Some((run.as_ref()?, value.scalar_count() as usize)))
                .filter_map(|(run, count)| match run {
                    HostRun::Cells {
                        input: TensorInputKind::Y,
                        first_cell,
                    } => Some(*first_cell..*first_cell + count),
                    _ => None,
                })
        })
        .collect()
}

/// A root call of a stage program with the host cells its inputs are read in
/// place from.
struct PlannedCall<'a> {
    index: usize,
    dst_start: Reg,
    input_starts: &'a [Reg],
    site: &'a solve::SolvePureCallSite,
    abi: std::rc::Rc<CallAbi>,
    inputs: Vec<Option<HostRun>>,
}

/// The tensor loads every reader of which is an input the glue reads in place.
fn unread_loads(
    program: &[LinearOp],
    versions: &RegisterVersionProgram,
    calls: &BTreeMap<usize, CallGlue>,
    glued: &BTreeSet<(usize, usize)>,
) -> BTreeSet<usize> {
    let mut replaced = BTreeSet::new();
    let mut read_elsewhere = BTreeSet::new();
    for (index, operation) in versions.operations().iter().enumerate() {
        for piece in operation.operands() {
            let producer = piece.producer();
            if !matches!(program[producer], LinearOp::TensorLoad { .. }) {
                continue;
            }
            if calls.contains_key(&index) && glued.contains(&(index, piece.selection().0)) {
                replaced.insert(producer);
            } else {
                read_elsewhere.insert(producer);
            }
        }
    }
    replaced.difference(&read_elsewhere).copied().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typed_call::program::ProgramHelpers;
    use rumoca_core::{SourceId, Span};

    const N: usize = 600;

    fn span(offset: usize) -> Span {
        Span::from_offsets(SourceId::from_source_name("glue.mo"), offset, offset + 1)
    }

    fn profile() -> solve::SolveArithmeticProfile {
        solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveIntegerDomain::FULL,
        )
    }

    /// `scale(x, factor)`: `(factor * x, x)`, both outputs above 4 KiB.
    fn scale() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
        let p = profile();
        let tensor =
            solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![N as u32]).unwrap();
        let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
        let mut table = solve::SolvePureCallTable::builder(p);
        let owner = table
            .add_owner(
                solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
                vec![tensor.clone(), scalar],
                vec![
                    solve::SolvePureCallOutput::result(tensor.clone()),
                    solve::SolvePureCallOutput::result(tensor),
                ],
                span(1),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(2))?;
                    let factor = b.load(inputs[1], span(3))?;
                    let scaled = b.broadcast_binary(
                        solve::SolveBinaryOperator::Multiply,
                        x,
                        factor,
                        false,
                        span(4),
                    )?;
                    b.store(outputs[0], scaled, span(5))?;
                    b.store(outputs[1], x, span(6))
                },
            )
            .unwrap();
        let site = table.call_site(owner).unwrap();
        (table.finish(), site)
    }

    fn load(dst: Reg, input: TensorInputKind, start: usize) -> LinearOp {
        LinearOp::TensorLoad {
            dst_start: dst,
            input,
            input_start: start,
            count: N,
            seed_start: None,
            lanes: 1,
        }
    }

    fn store(start: Reg) -> LinearOp {
        LinearOp::StoreOutputRange {
            start,
            count: N,
            stride: 1,
        }
    }

    /// The stage program: the input tensor, the factor, the call (results at
    /// `2N + 1`) and the two stores, optionally with the input computed and the
    /// first result transformed before its store.
    fn program(
        site: &solve::SolvePureCallSite,
        computed_input: bool,
        transformed_output: bool,
    ) -> Vec<LinearOp> {
        let n = N as Reg;
        let mut ops = vec![
            load(0, TensorInputKind::P, 0),
            // The factor is a Y cell, so the call has no immutable coordinate
            // and is not memoized.
            LinearOp::LoadY { dst: n, index: 0 },
        ];
        if computed_input {
            ops.push(load(2 * n + 1, TensorInputKind::P, N));
        }
        let input = if computed_input { 3 * n + 1 } else { 0 };
        if computed_input {
            ops.push(LinearOp::TensorBinary {
                dst_start: input,
                op: solve::BinaryOp::Add,
                lhs_start: 0,
                rhs_start: 2 * n + 1,
                count: N,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            });
        }
        let call = 4 * n + 2;
        ops.push(LinearOp::PureCall {
            dst_start: call,
            input_starts: vec![input, n].into_boxed_slice(),
            site: site.clone(),
        });
        let first = if transformed_output {
            ops.push(LinearOp::TensorBinary {
                dst_start: 7 * n,
                op: solve::BinaryOp::Add,
                lhs_start: call,
                rhs_start: n,
                count: N,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            });
            7 * n
        } else {
            call
        };
        ops.push(store(first));
        ops.push(store(call + n));
        ops
    }

    fn derive(computed_input: bool, transformed_output: bool) -> (Vec<LinearOp>, StageGlue) {
        let (table, site) = scale();
        let ops = program(&site, computed_input, transformed_output);
        let helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let rows = [ops.clone()];
        let plan = CallProgramPlan::new(&rows, &helpers, 2 * N, 0).unwrap();
        let targets = (0..2 * N).collect::<Vec<_>>();
        let glue = StageGlue::derive(&ops, &plan, (0, None), &targets).unwrap();
        (ops, glue)
    }

    fn call_index(ops: &[LinearOp]) -> usize {
        ops.iter()
            .position(|op| matches!(op, LinearOp::PureCall { .. }))
            .unwrap()
    }

    #[test]
    fn a_bare_load_input_and_a_stored_output_are_read_and_built_in_place() {
        let (ops, glue) = derive(false, false);
        let call = glue.call(call_index(&ops)).unwrap();
        assert_eq!(
            call.inputs,
            [
                Some(HostRun::Cells {
                    input: TensorInputKind::P,
                    first_cell: 0
                }),
                None
            ]
        );
        assert_eq!(call.outputs, [Some(0), Some(N)]);
        assert_eq!(call.unread, [false, false]);
        // The load and both stores are replaced.
        assert!(glue.skips(0));
        assert!(glue.skips(ops.len() - 2) && glue.skips(ops.len() - 1));
    }

    #[test]
    fn an_output_nothing_reads_is_not_moved_to_registers() {
        let (table, site) = scale();
        let mut ops = program(&site, false, false);
        // Only the first output is stored.
        ops.pop();
        let helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let rows = [ops.clone()];
        let plan = CallProgramPlan::new(&rows, &helpers, 2 * N, 0).unwrap();
        let targets = (0..N).collect::<Vec<_>>();
        let glue = StageGlue::derive(&ops, &plan, (0, None), &targets).unwrap();
        let call = glue.call(call_index(&ops)).unwrap();
        assert_eq!(call.unread, [false, true]);
        assert_eq!(call.outputs, [Some(0), None]);
    }

    #[test]
    fn a_computed_input_is_staged_and_its_loads_stay() {
        let (ops, glue) = derive(true, false);
        let call = glue.call(call_index(&ops)).unwrap();
        assert_eq!(call.inputs, [None, None]);
        assert!(!glue.skips(0) && !glue.skips(2));
        // The outputs are still built in place.
        assert_eq!(call.outputs, [Some(0), Some(N)]);
    }

    #[test]
    fn an_output_another_operation_reads_is_left_in_registers() {
        let (ops, glue) = derive(false, true);
        let call = glue.call(call_index(&ops)).unwrap();
        // The first output feeds a computation; the second is still stored whole.
        assert_eq!(call.outputs, [None, Some(N)]);
        assert!(!glue.skips(ops.len() - 2));
        assert!(glue.skips(ops.len() - 1));
    }

    #[test]
    fn a_memoized_call_still_stores_each_output_straight_from_its_result() {
        let (table, site) = scale();
        let mut ops = program(&site, false, false);
        // Every input is a P cell: the call has an immutable coordinate.
        ops[1] = LinearOp::LoadP {
            dst: N as Reg,
            index: 2 * N,
        };
        let helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let rows = [ops.clone()];
        let plan = CallProgramPlan::new(&rows, &helpers, 2 * N, 0).unwrap();
        assert_eq!(plan.view().memos.len(), 1);
        let targets = (0..2 * N).collect::<Vec<_>>();
        let glue = StageGlue::derive(&ops, &plan, (0, None), &targets).unwrap();
        let call = glue.call(call_index(&ops)).unwrap();
        // The input is read in place from P and each output goes from the
        // result its memo holds straight to its Y cells, with no register copy.
        assert!(call.inputs[0].is_some());
        assert_eq!(call.outputs, [Some(0), Some(N)]);
        assert!(glue.skips(ops.len() - 1));
    }

    #[test]
    fn an_input_the_early_write_would_overwrite_is_not_built_in_place() {
        let (table, site) = scale();
        let mut ops = program(&site, false, false);
        // The input is the first output's own Y cells, which the call would
        // overwrite while reading them.
        ops[0] = load(0, TensorInputKind::Y, 0);
        let helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let rows = [ops.clone()];
        let plan = CallProgramPlan::new(&rows, &helpers, 2 * N, 0).unwrap();
        let targets = (0..2 * N).collect::<Vec<_>>();
        let glue = StageGlue::derive(&ops, &plan, (0, None), &targets).unwrap();
        let call = glue.call(call_index(&ops)).unwrap();
        assert_eq!(call.outputs[0], None);
        assert_eq!(call.outputs[1], Some(N));
    }
}

#[cfg(test)]
mod carried_tests {
    use super::super::carried::uncovered;
    use super::*;
    use crate::typed_call::program::ProgramHelpers;
    use rumoca_core::{SourceId, Span};

    const N: usize = 600;

    fn span(offset: usize) -> Span {
        Span::from_offsets(SourceId::from_source_name("glue.mo"), offset, offset + 1)
    }

    /// `pass(x)`: `(x, x)`, both outputs the input returned unchanged.
    fn pass_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
        let p = solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveIntegerDomain::FULL,
        );
        let tensor =
            solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![N as u32]).unwrap();
        let mut table = solve::SolvePureCallTable::builder(p);
        let owner = table
            .add_owner(
                solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(2).unwrap()),
                vec![tensor.clone()],
                vec![
                    solve::SolvePureCallOutput::result(tensor.clone()),
                    solve::SolvePureCallOutput::result(tensor),
                ],
                span(1),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(2))?;
                    b.store(outputs[0], x, span(3))?;
                    b.store(outputs[1], x, span(4))
                },
            )
            .unwrap();
        let site = table.call_site(owner).unwrap();
        (table.finish(), site)
    }

    /// The input is `P[0..N]`; the outputs are stored whole to `Y[0..N]` and
    /// `Y[N..2N]`. With `read_y`, an operation also reads `Y[N]`.
    fn stage(site: &solve::SolvePureCallSite, read_y: bool) -> Vec<LinearOp> {
        let n = N as Reg;
        let mut ops = vec![LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 0,
            count: N,
            seed_start: None,
            lanes: 1,
        }];
        if read_y {
            ops.push(LinearOp::LoadY {
                dst: 3 * n,
                index: N,
            });
        }
        ops.push(LinearOp::PureCall {
            dst_start: n,
            input_starts: vec![0].into_boxed_slice(),
            site: site.clone(),
        });
        for start in [n, 2 * n] {
            ops.push(LinearOp::StoreOutputRange {
                start,
                count: N,
                stride: 1,
            });
        }
        ops
    }

    fn derive(
        pairs: &[(Range<usize>, Range<usize>)],
        read_y: bool,
    ) -> (CallGlue, Vec<CommitPiece>) {
        let (table, site) = pass_table();
        let ops = stage(&site, read_y);
        let helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let rows = [ops.clone()];
        let host = solve::NativeCarriedHost::new(
            (2 * N, 2 * N),
            std::iter::once(0..2 * N),
            std::iter::once(0..2 * N),
        );
        let state = solve::NativeCarriedState::construct_for_host(&host, pairs).unwrap();
        let plan = CallProgramPlan::new(&rows, &helpers, 2 * N, 0)
            .unwrap()
            .with_carried(&state)
            .unwrap();
        let targets = (0..2 * N).collect::<Vec<_>>();
        let glue = StageGlue::derive(&ops, &plan, (0, None), &targets).unwrap();
        let call = ops
            .iter()
            .position(|op| matches!(op, LinearOp::PureCall { .. }))
            .unwrap();
        let call = glue.call(call).unwrap();
        let pieces = plan.view().pieces.borrow().clone();
        (
            CallGlue {
                inputs: call.inputs.clone(),
                outputs: call.outputs.clone(),
                carried: call.carried.clone(),
                unread: call.unread.clone(),
            },
            pieces,
        )
    }

    /// The work Y bytes of cells `cells`.
    fn bytes(cells: Range<usize>) -> Range<u32> {
        (cells.start * 8) as u32..(cells.end * 8) as u32
    }

    #[test]
    fn an_output_built_in_the_cells_of_a_carried_run_is_one_piece_of_that_run() {
        let (call, pieces) = derive(&[(N..2 * N, 0..N)], false);
        assert_eq!(call.outputs, [Some(0), Some(N)]);
        assert!(call.carried[0].is_empty());
        let [piece] = call.carried[1].as_slice() else {
            panic!("one piece");
        };
        assert_eq!(
            (piece.y.clone(), piece.at, piece.p),
            (bytes(N..2 * N), 0, 0)
        );
        assert_eq!(pieces, call.carried[1]);
    }

    #[test]
    fn two_carried_outputs_each_commit_on_their_own_flag() {
        let (call, pieces) = derive(&[(0..N, 0..N), (N..2 * N, N..2 * N)], false);
        assert_eq!((call.carried[0].len(), call.carried[1].len()), (1, 1));
        assert_ne!(call.carried[0][0].flag, call.carried[1][0].flag);
        assert_eq!(pieces.len(), 2);
    }

    #[test]
    fn a_run_some_operation_reads_from_y_is_committed_unconditionally() {
        let (call, pieces) = derive(&[(N..2 * N, 0..N)], true);
        assert_eq!(call.outputs, [Some(0), Some(N)]);
        assert!(call.carried.iter().all(Vec::is_empty));
        assert!(pieces.is_empty());
    }

    #[test]
    fn a_run_over_two_outputs_commits_each_part_by_its_own_flag() {
        let (call, pieces) = derive(&[(0..2 * N, 0..2 * N)], false);
        let [first] = call.carried[0].as_slice() else {
            panic!("one piece per output");
        };
        let [second] = call.carried[1].as_slice() else {
            panic!("one piece per output");
        };
        assert_eq!((first.y.clone(), first.at, first.p), (bytes(0..N), 0, 0));
        assert_eq!(
            (second.y.clone(), second.at, second.p),
            (bytes(N..2 * N), 0, (N * 8) as u32)
        );
        assert_eq!(pieces.len(), 2);
    }

    #[test]
    fn a_run_over_part_of_an_output_leaves_the_rest_of_it_uncovered() {
        let (call, pieces) = derive(&[(N + 100..N + 300, 0..200)], false);
        assert!(call.carried[0].is_empty());
        let [piece] = call.carried[1].as_slice() else {
            panic!("one piece");
        };
        assert_eq!(piece.y, bytes(N + 100..N + 300));
        assert_eq!((piece.at, piece.p), (800, 0));
        assert_eq!(uncovered((N * 8) as u32, &pieces), [0..800, 2400..4800]);
    }

    #[test]
    fn a_y_read_outside_a_piece_does_not_stop_it_being_deferred() {
        // The read is of Y[N], the first cell of the second output; the run is
        // the last 100 cells of that output.
        let (call, _) = derive(&[(2 * N - 100..2 * N, 0..100)], true);
        assert_eq!(call.carried[1].len(), 1);
    }
}
