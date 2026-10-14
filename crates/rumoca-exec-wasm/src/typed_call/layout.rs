mod calls;
mod demand;
mod forwarding;
mod homes;
mod lifetimes;
mod outputs;
mod relocate;
mod report;
use super::TypedCallCompileError;
pub(crate) use demand::Demand;
pub(super) use demand::{Liveness, visit_calls, visit_reads};
use homes::binder_intervals;
pub(super) use homes::{Home, Interval, LocalCounts};
use lifetimes::Lifetimes;
use outputs::OutputPolicy;
use rumoca_ir_solve as solve;
use std::collections::BTreeMap;

/// The plans a body's calls resolve against: one per unit, a callee owner
/// built for the outputs one kind of call site reads (SOLVE-C95).
pub(super) struct Callees<'a> {
    pub plans: &'a [Option<FramePlan>],
    units: &'a BTreeMap<(u32, Demand), usize>,
    always: &'a [Box<[bool]>],
}

impl<'a> Callees<'a> {
    pub(super) fn new(
        plans: &'a [Option<FramePlan>],
        units: &'a BTreeMap<(u32, Demand), usize>,
        always: &'a [Box<[bool]>],
    ) -> Self {
        Self {
            plans,
            units,
            always,
        }
    }

    /// The demand an owner is built for when a site reads `demand` of it: the
    /// outputs the site reads and the ones that are always built (`always`, by
    /// output; an assertion predicate or message value is never left unbuilt).
    pub(super) fn built_for(
        always: &[Box<[bool]>],
        owner: solve::SolvePureCallOwnerId,
        demand: Demand,
    ) -> Demand {
        match always.get(owner.index() as usize) {
            Some(always) => Demand::of(
                always
                    .iter()
                    .enumerate()
                    .map(|(output, always)| *always || demand.wants(output)),
            ),
            None => Demand::all(),
        }
    }

    /// The unit a call of `owner` that reads `demand` of its outputs runs.
    pub(super) fn unit(
        &self,
        owner: solve::SolvePureCallOwnerId,
        demand: Demand,
    ) -> Result<usize, TypedCallCompileError> {
        let demand = Self::built_for(self.always, owner, demand);
        if demand.is_all() {
            return Ok(owner.index() as usize);
        }
        self.units
            .get(&(owner.index(), demand))
            .copied()
            .ok_or(TypedCallCompileError::SiteMismatch)
    }

    pub(super) fn plan(&self, unit: usize) -> Result<&FramePlan, TypedCallCompileError> {
        self.plans
            .get(unit)
            .and_then(Option::as_ref)
            .ok_or(TypedCallCompileError::SiteMismatch)
    }
}

impl std::ops::Deref for Callees<'_> {
    type Target = [Option<FramePlan>];

    fn deref(&self) -> &Self::Target {
        self.plans
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct CellRange {
    pub base: u32,
    pub offset: u32,
    pub bytes: u32,
}

/// First `CellRange::base` that names a pointer cell instead of a frame
/// pointer: `base - POINTER_BASE` is the byte offset, in the input tuple, of an
/// eight-byte cell that holds the address of the range. Frame pointers are the
/// three call parameters, so every base below this is a parameter index.
pub(super) const POINTER_BASE: u32 = 1 << 16;

/// First `CellRange::base` that names a pointer cell in the body's scratch:
/// `base - SCRATCH_POINTER_BASE` is the scratch offset of an eight-byte cell
/// that holds the address of the range.
pub(super) const SCRATCH_POINTER_BASE: u32 = 1 << 31;

/// Where the eight-byte cell that holds a range's address lives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PointerCell {
    /// An offset in the input tuple.
    Input(u32),
    /// An offset in the body's scratch.
    Scratch(u32),
}

impl CellRange {
    /// A range the caller owns, reached through input pointer cell `cell`.
    pub(super) fn through_input_cell(cell: u32, bytes: u32) -> Result<Self, TypedCallCompileError> {
        let base = POINTER_BASE
            .checked_add(cell)
            .filter(|base| *base < SCRATCH_POINTER_BASE)
            .ok_or(TypedCallCompileError::SizeLimit)?;
        Ok(Self {
            base,
            offset: 0,
            bytes,
        })
    }

    /// A range reached through the scratch pointer cell at `cell`.
    pub(super) fn through_scratch_cell(
        cell: u32,
        bytes: u32,
    ) -> Result<Self, TypedCallCompileError> {
        if cell >= SCRATCH_POINTER_BASE {
            return Err(TypedCallCompileError::SizeLimit);
        }
        Ok(Self {
            base: SCRATCH_POINTER_BASE + cell,
            offset: 0,
            bytes,
        })
    }

    /// The cell that holds this range's address, for a range reached by pointer.
    pub(super) fn pointer_cell(self) -> Option<PointerCell> {
        if let Some(cell) = self.base.checked_sub(SCRATCH_POINTER_BASE) {
            Some(PointerCell::Scratch(cell))
        } else {
            self.base.checked_sub(POINTER_BASE).map(PointerCell::Input)
        }
    }

    /// Whether the two ranges share a byte of one address space.
    fn overlaps(self, other: Self) -> bool {
        self.base == other.base
            && self.offset < other.offset.saturating_add(other.bytes)
            && other.offset < self.offset.saturating_add(self.bytes)
    }
}

/// Scratch storage of one program body (an owner or a nested region).
///
/// Pinned slots and live register ranges are allocated
/// once, bottom up from `cursor`. The storage an operation owns while it runs
/// (its regions, its callee frame and its loop counter) is dead as soon as the
/// operation has copied its results into the body's own registers, so every
/// operation places it at the same base, directly above the live storage, and
/// the sibling arms of one conditional share that base as well. The body's
/// high-water mark is therefore the largest operation-private span above the
/// live storage, not the sum of all of them.
pub(super) struct FramePlan {
    pub observations: Option<super::observations::ObservationPlan>,
    pub input_types: Vec<solve::SolveValueType>,
    pub slots: Vec<CellRange>,
    pub registers: Vec<CellRange>,
    pub assigned_states: Vec<CellRange>,
    pub assigned_inputs: Vec<CellRange>,
    pub assigned_outputs: Vec<CellRange>,
    pub input_cells: Vec<InputCell>,
    pub input_bytes: u32,
    pub output_bytes: u32,
    /// Whether the body leaves its outputs in the first `output_bytes` of its
    /// scratch instead of copying them to its output buffer. Its callers read
    /// them there, or build their result registers over them.
    pub outputs_in_scratch: bool,
    /// Where the result registers of the call being planned begin, when they
    /// are the callee's outputs.
    call_results: Option<PlacedResults>,
    /// First scratch offset of the body (zero for a call owner).
    base: u32,
    /// Every scratch offset the body addresses is below this mark.
    pub scratch_bytes: u32,
    /// Scratch the body would need if no storage were shared between
    /// sequential operations, conditional arms or call frames.
    pub unshared_bytes: u64,
    /// Bytes of freshly allocated (not borrowed or aliased) slots.
    slot_bytes: u32,
    /// Bytes and count of freshly allocated registers, and the largest.
    register_bytes: u32,
    register_count: u32,
    largest_register: u32,
    /// Top of the storage that stays live to the end of the body.
    cursor: u32,
    pub regions: Vec<Vec<FramePlan>>,
    /// The reference results of each operation, in destination order.
    pub references: Vec<Vec<Option<ReferenceResult>>>,
    pending_references: Vec<Option<ReferenceResult>>,
    /// Registers that are the completed return of an output slot with a hint:
    /// the output ordinal and the hint.
    pub register_hints: BTreeMap<usize, (usize, OutputHint)>,
    /// The outputs returned by reference, by slot index.
    pub ref_outputs: BTreeMap<usize, RefOutput>,
    /// The input ordinals each by-reference output may return unchanged, by slot
    /// index: the address in its result cell may name these arguments.
    pub output_aliases: BTreeMap<usize, Vec<usize>>,
    /// The parent values each output ordinal may name, for an output that is a
    /// hinted reference result of this body.
    forwards: BTreeMap<usize, Vec<CellRange>>,
    /// The operations that run when only the outputs the consumer of this body
    /// reads are built, and the registers they read.
    pub liveness: Liveness,
    /// Call results that may be built in the buffer of the output a conditional
    /// returns them as (see `forwarding`), by register, with that output.
    hint_candidates: BTreeMap<usize, (usize, OutputHint)>,
    /// The values each such result may name besides the buffer, by register.
    forwarded_names: BTreeMap<usize, Vec<CellRange>>,
    /// Call results the parent built in the buffer of the result this body returns
    /// as an output, by that output: the body may name them as its own.
    inherited_forwards: Vec<(usize, forwarding::Forwarded)>,
    pub counter: CellRange,
    pub calls: Vec<Option<CallFrame>>,
    pub output_slots: Vec<usize>,
    /// Whether each slot already has its storage; a region places an output
    /// slot where the body first accesses it (see `place_output`).
    placed: Vec<bool>,
    output_policy: OutputPolicy,
    slot_reusable: Vec<bool>,
    register_reusable: Vec<bool>,
    lifetimes: Lifetimes,
    /// Whether single-cell registers read only as scalars take wasm locals.
    promotes: bool,
    /// Registers some operation reads other than as a scalar value.
    escapes: Vec<bool>,
    read: Vec<bool>,
    /// Where each register lives, in register order.
    pub homes: Vec<Home>,
    /// Closed bounds an Integer register provably stays within, so operations
    /// that cannot fault emit no check.
    pub intervals: Vec<Option<Interval>>,
    /// The same bounds for each slot, set by the parent for region inputs.
    slot_intervals: Vec<Option<Interval>>,
    /// Locals this body promotes, and the most it and its regions need at once.
    pub own_locals: LocalCounts,
    pub demand: LocalCounts,
}

/// What the parent of a region knows of its outputs: the policy, the buffers
/// they may be built in, the ones read, and the call results the parent built
/// in the buffer of a result this region returns.
type RegionOutputs<'a> = (
    OutputPolicy,
    &'a [Option<OutputHint>],
    &'a Demand,
    &'a [(usize, forwarding::Forwarded)],
);

/// A parent buffer reserved for a large conditional result before its arms are
/// planned.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ReservedBuffer {
    pub buffer: CellRange,
    /// The floor of the enclosing hint the buffer was inherited from.
    pub inherited_floor: Option<u32>,
}

/// A conditional result that names its value by address.
///
/// The result register is the scratch cell at `cell`. An arm that passes a
/// parent value through stores that value's address; any other arm copies its
/// value into the buffer and stores the buffer's address.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ReferenceResult {
    /// Scratch offset of the cell that holds the address.
    pub cell: u32,
    /// What each arm stores in the cell.
    pub arms: [ReferenceArm; 2],
}

/// Parent storage a region may build one output in, so that its parent needs no
/// copy: the output is built in `buffer`, or names a value that outlives the
/// region (stored below `floor`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct OutputHint {
    pub buffer: CellRange,
    pub floor: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ReferenceArm {
    /// The arm returns a parent value; the cell holds its address.
    Pass,
    /// The arm copies its value to this parent buffer; the cell holds its address.
    Build(CellRange),
    /// The arm's own reference result, the scratch cell at this offset, names a
    /// value that outlives the arm; its address is copied.
    Forward(u32),
}

#[derive(Clone, Copy)]
struct InputBorrow {
    range: CellRange,
    reusable: bool,
}

/// How a body receives the value of one Input slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct InputCell {
    /// Offset of the slot's cell (or its pointer cell) in the input tuple.
    pub offset: u32,
    /// Bytes of the slot's value.
    pub bytes: u32,
    /// Whether the cell holds the address of the caller's value.
    pub by_reference: bool,
}

/// The input tuple a body expects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum InputConvention {
    /// Every input value is packed into the tuple, in slot order.
    Packed,
    /// A large input value stays in the caller's storage and the tuple holds
    /// its address. Inputs are read-only, so the callee never writes through
    /// it, and the caller is suspended for the whole call.
    ByReference,
    /// The convention of the roots of the native call program, which its entry
    /// glue calls: as `ByReference`, but only values of Binary64 Real elements
    /// are passed or returned by reference, because the glue converts every
    /// other element type between its Real registers and the typed cells.
    Stage,
}

impl InputConvention {
    /// Whether an input value of `bytes` bytes and this type is an address in
    /// a tuple. The glue of the roots stages a Binary64 Real or Integer array
    /// as the callee reads it, or names the host cells that already are it; it
    /// converts every other element type between its Real registers and the
    /// typed cells.
    fn references_input(self, value_type: &solve::SolveValueType, bytes: u32) -> bool {
        let eligible = match self {
            Self::Packed => false,
            Self::ByReference => true,
            Self::Stage => matches!(
                value_type.element_type(),
                solve::SolveScalarType::Real {
                    format: solve::SolveRealFormat::Binary64,
                    ..
                } | solve::SolveScalarType::Integer(_)
            ),
        };
        eligible && crate::cost_model::address_beats_copy(bytes)
    }

    /// Whether an output value of `bytes` bytes and this type is an address in
    /// a result cell. The glue of the roots reads a Binary64 Real result as it
    /// is and converts every other element type from the packed result cells.
    fn references_output(self, value_type: &solve::SolveValueType, bytes: u32) -> bool {
        let eligible = match self {
            Self::Packed => false,
            Self::ByReference => true,
            Self::Stage => matches!(
                value_type.element_type(),
                solve::SolveScalarType::Real {
                    format: solve::SolveRealFormat::Binary64,
                    ..
                }
            ),
        };
        eligible && crate::cost_model::address_beats_copy(bytes)
    }
}

/// A large output a helper returns by reference: the caller passes the address
/// of a buffer to build it in (a cell of the input tuple), and the helper
/// leaves the address of the value (the buffer, or an input it returns
/// unchanged) in a cell at the head of its scratch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RefOutput {
    /// Input tuple offset of the cell that holds the buffer address.
    pub dest_cell: u32,
    /// Scratch offset of the cell that holds the result address.
    pub result_cell: u32,
}

/// The buffer a call builds one by-reference result in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct DestBuffer {
    /// Input tuple offset of the cell the buffer address is stored in.
    pub cell: u32,
    pub buffer: CellRange,
}

/// The result registers of a call being planned.
pub(super) struct PlacedResults {
    /// Where the run of result registers begins.
    pub start: u32,
    pub buffers: Vec<DestBuffer>,
}

pub(super) struct CallFrame {
    /// The unit the call runs: the callee built for the outputs this site reads.
    pub unit: usize,
    pub input: CellRange,
    /// Where the callee's results are read from after the call.
    pub output: CellRange,
    pub scratch: CellRange,
    /// The call's result registers already are the callee's outputs.
    pub results_in_place: bool,
    /// The buffers the callee builds its by-reference results in.
    pub buffers: Vec<DestBuffer>,
}

impl FramePlan {
    /// An empty body whose live storage starts at `base`.
    fn at(base: u32) -> Self {
        Self {
            observations: None,
            input_types: Vec::new(),
            slots: Vec::new(),
            registers: Vec::new(),
            assigned_states: Vec::new(),
            assigned_inputs: Vec::new(),
            assigned_outputs: Vec::new(),
            input_cells: Vec::new(),
            input_bytes: 0,
            output_bytes: 0,
            outputs_in_scratch: false,
            call_results: None,
            base,
            scratch_bytes: base,
            unshared_bytes: 0,
            slot_bytes: 0,
            register_bytes: 0,
            register_count: 0,
            largest_register: 0,
            cursor: base,
            regions: Vec::new(),
            references: Vec::new(),
            pending_references: Vec::new(),
            register_hints: BTreeMap::new(),
            ref_outputs: BTreeMap::new(),
            output_aliases: BTreeMap::new(),
            forwards: BTreeMap::new(),
            liveness: Liveness::default(),
            hint_candidates: BTreeMap::new(),
            forwarded_names: BTreeMap::new(),
            inherited_forwards: Vec::new(),
            counter: CellRange {
                base: 2,
                offset: 0,
                bytes: 0,
            },
            calls: Vec::new(),
            output_slots: Vec::new(),
            placed: Vec::new(),
            output_policy: OutputPolicy::Returned,
            slot_reusable: Vec::new(),
            register_reusable: Vec::new(),
            lifetimes: Lifetimes::default(),
            promotes: true,
            escapes: Vec::new(),
            read: Vec::new(),
            homes: Vec::new(),
            intervals: Vec::new(),
            slot_intervals: Vec::new(),
            own_locals: LocalCounts::default(),
            demand: LocalCounts::default(),
        }
    }

    /// The slot of one input value under `convention`.
    fn input_slot(
        &mut self,
        value_type: &solve::SolveValueType,
        convention: InputConvention,
    ) -> Result<CellRange, TypedCallCompileError> {
        let bytes = bytes(value_type)?;
        let by_reference = convention.references_input(value_type, bytes);
        let cell = alloc(
            0,
            &mut self.input_bytes,
            if by_reference { 8 } else { bytes },
        )?;
        self.input_cells.push(InputCell {
            offset: cell.offset,
            bytes,
            by_reference,
        });
        Ok(if by_reference {
            CellRange::through_input_cell(cell.offset, bytes)?
        } else {
            cell
        })
    }

    /// Storage that stays live to the end of the body.
    fn keep(&mut self, count: u32) -> Result<CellRange, TypedCallCompileError> {
        let range = alloc(2, &mut self.cursor, count)?;
        self.adopt(u64::from(count))?;
        self.scratch_bytes = self.scratch_bytes.max(self.cursor);
        Ok(range)
    }

    /// Account a child body or callee frame the operation owns.
    fn adopt(&mut self, unshared: u64) -> Result<(), TypedCallCompileError> {
        self.unshared_bytes = self
            .unshared_bytes
            .checked_add(unshared)
            .ok_or(TypedCallCompileError::SizeLimit)?;
        Ok(())
    }

    pub(super) fn construct_with(
        program: &solve::TypedProgram,
        callees: &Callees<'_>,
        convention: InputConvention,
        demand: &Demand,
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_with_observations(program, callees, None, (convention, demand), false)
    }

    /// The same body with its top-level scalar registers in wasm locals. Only
    /// a body emitted as one function may use it: an outlined body keeps every
    /// top-level register in memory shared by its continuations.
    pub(super) fn construct_promoted(
        program: &solve::TypedProgram,
        callees: &Callees<'_>,
        convention: InputConvention,
        demand: &Demand,
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_with_observations(program, callees, None, (convention, demand), true)
    }

    pub(super) fn construct_with_observations(
        program: &solve::TypedProgram,
        callees: &Callees<'_>,
        observations: Option<super::observations::ObservationPlan>,
        (convention, demand): (InputConvention, &Demand),
        promote_top_level: bool,
    ) -> Result<Self, TypedCallCompileError> {
        let mut plan = Self::at(0);
        plan.promotes = promote_top_level;
        plan.liveness = Liveness::of(program, demand);
        plan.outputs_in_scratch = convention != InputConvention::Packed;
        // Private output slots occupy the first scratch span, allowing one atomic copy.
        // A helper returns a large output by reference: its span holds one cell.
        for slot in program.slots() {
            if slot.storage() == solve::SolveStorageClass::Output {
                advance(
                    &mut plan.output_bytes,
                    output_span(program, slot, convention)?,
                )?;
            }
        }
        plan.cursor = plan.output_bytes;
        plan.scratch_bytes = plan.output_bytes;
        plan.unshared_bytes = u64::from(plan.output_bytes);
        if let Some(mut observations) = observations {
            observations.range = plan.keep(observations.range.bytes)?;
            plan.observations = Some(observations);
        }
        let mut output_cursor = 0;
        for slot in program.slots() {
            let count = bytes(slot.value_type())?;
            let range = match slot.storage() {
                solve::SolveStorageClass::Input => {
                    plan.input_types.push(slot.value_type().clone());
                    plan.input_slot(slot.value_type(), convention)?
                }
                solve::SolveStorageClass::Output
                    if returns_by_reference(program, slot, convention) =>
                {
                    let result = alloc(2, &mut output_cursor, 8)?;
                    let dest = alloc(0, &mut plan.input_bytes, 8)?;
                    plan.ref_outputs.insert(
                        plan.slots.len(),
                        RefOutput {
                            dest_cell: dest.offset,
                            result_cell: result.offset,
                        },
                    );
                    CellRange::through_input_cell(dest.offset, count)?
                }
                solve::SolveStorageClass::Output => alloc(2, &mut output_cursor, count)?,
                _ => {
                    advance(&mut plan.slot_bytes, count)?;
                    plan.keep(count)?
                }
            };
            if slot.storage() == solve::SolveStorageClass::Output {
                plan.output_slots.push(plan.slots.len());
            }
            plan.slots.push(range);
            plan.slot_intervals.push(None);
            plan.placed.push(true);
            plan.slot_reusable.push(false);
        }
        // A by-reference output is built in the caller's buffer, and may name
        // only what outlives the call: an input.
        let hints = plan
            .output_slots
            .iter()
            .map(|slot| {
                (plan.ref_outputs.contains_key(slot) && !plan.liveness.drops(*slot)).then_some(
                    OutputHint {
                        buffer: plan.slots[*slot],
                        floor: 0,
                    },
                )
            })
            .collect::<Vec<_>>();
        plan.hint_returned_registers(program, &hints);
        plan.plan_regions(program, callees)?;
        plan.summarize_output_aliases(program);
        Ok(plan)
    }

    /// A nested body whose private storage starts at `base`, above every
    /// range of its parent that it borrows.
    fn region(
        program: &solve::TypedProgram,
        base: u32,
        callees: &Callees<'_>,
        input_borrows: &[Option<InputBorrow>],
        output_policy: OutputPolicy,
        input_intervals: &[Option<Interval>],
    ) -> Result<Self, TypedCallCompileError> {
        Self::region_hinted(
            program,
            base,
            callees,
            (input_borrows, input_intervals),
            (output_policy, &[], &Demand::all(), &[]),
        )
    }

    /// A region whose output `k` may be built in `hints[k]`, a buffer its parent
    /// owns (see `OutputHint`).
    fn region_hinted(
        program: &solve::TypedProgram,
        base: u32,
        callees: &Callees<'_>,
        (input_borrows, input_intervals): (&[Option<InputBorrow>], &[Option<Interval>]),
        (output_policy, hints, demand, inherited): RegionOutputs<'_>,
    ) -> Result<Self, TypedCallCompileError> {
        let mut plan = Self::at(base);
        plan.output_policy = output_policy;
        plan.inherited_forwards = inherited.to_vec();
        plan.liveness = Liveness::of(program, demand);
        let mut input_index = 0;
        for slot in program.slots() {
            let count = bytes(slot.value_type())?;
            let mut interval = None;
            let unloaded = slot.storage() == solve::SolveStorageClass::Input
                && !plan.liveness.loads(plan.slots.len());
            let borrowed = if slot.storage() == solve::SolveStorageClass::Input {
                let range = *input_borrows
                    .get(input_index)
                    .ok_or(TypedCallCompileError::SizeLimit)?;
                interval = input_intervals.get(input_index).copied().flatten();
                input_index += 1;
                range
            } else {
                None
            };
            let output = slot.storage() == solve::SolveStorageClass::Output;
            let range = match borrowed {
                // No running operation loads the input: nothing is borrowed or copied.
                _ if unloaded => Self::unplaced(),
                Some(borrow) => checked_capture(slot, borrow.range, count, base)?,
                // An output slot is placed at its first access, so a completed
                // return can take its source's range without a dead span.
                None if output => CellRange {
                    base: 2,
                    offset: base,
                    bytes: if plan.liveness.drops(plan.slots.len()) {
                        0
                    } else {
                        count
                    },
                },
                None => {
                    advance(&mut plan.slot_bytes, count)?;
                    plan.keep(count)?
                }
            };
            if output {
                plan.output_slots.push(plan.slots.len());
            }
            plan.placed.push(!output);
            plan.slot_intervals.push(interval);
            plan.slots.push(range);
            plan.slot_reusable
                .push(borrowed.is_some_and(|borrow| borrow.reusable));
        }
        if input_index != input_borrows.len() {
            return Err(TypedCallCompileError::SizeLimit);
        }
        plan.hint_returned_registers(program, hints);
        plan.plan_regions(program, callees)?;
        Ok(plan)
    }

    fn plan_regions(
        &mut self,
        program: &solve::TypedProgram,
        callees: &Callees<'_>,
    ) -> Result<(), TypedCallCompileError> {
        self.find_forwardable_calls(program);
        self.plan_assigned_storage(program)?;
        let reads = homes::register_reads(
            program,
            matches!(
                self.output_policy,
                OutputPolicy::Element | OutputPolicy::Carried(_)
            ),
        );
        self.escapes = reads.escapes;
        self.read = reads.read;
        self.lifetimes = Lifetimes::construct(program, &self.slots, &self.liveness);
        for (index, operation) in program.operations().iter().enumerate() {
            if self.liveness.is_dead(index) {
                self.skip_dead_operation(index, operation.operation())?;
                continue;
            }
            self.place_accessed_output(program, operation.operation())?;
            self.plan_operation_registers(program, index, operation.operation(), callees)?;
            // Every register is below `base`; nothing the operation places at
            // or above it outlives the operation.
            let base = self.cursor;
            let (regions, call, top) =
                self.plan_private_storage(program, index, operation.operation(), base, callees)?;
            self.scratch_bytes = self.scratch_bytes.max(top);
            self.regions.push(regions);
            self.references
                .push(std::mem::take(&mut self.pending_references));
            self.calls.push(call);
            self.lifetimes.complete(index);
        }
        for slot in 0..self.slots.len() {
            self.place_output(program, slot, None)?;
        }
        // The regions run one at a time, each above this body's own locals.
        let widest = self
            .regions
            .iter()
            .flatten()
            .fold(LocalCounts::default(), |most, region| {
                most.largest(region.demand)
            });
        self.demand = self.own_locals.plus(widest)?;
        Ok(())
    }

    fn plan_assigned_storage(
        &mut self,
        program: &solve::TypedProgram,
    ) -> Result<(), TypedCallCompileError> {
        // One presence cell precedes one compact payload range. Absence never
        // grants permission to consume the payload backing bytes.
        for value_type in program.assigned_state_types() {
            let count = bytes(value_type)?
                .checked_add(8)
                .ok_or(TypedCallCompileError::SizeLimit)?;
            let range = self.keep(count)?;
            self.assigned_states.push(range);
        }
        for value_type in program.assigned_state_inputs() {
            let count = bytes(value_type)?
                .checked_add(8)
                .ok_or(TypedCallCompileError::SizeLimit)?;
            let range = self.keep(count)?;
            self.assigned_inputs.push(range);
        }
        for value_type in program.assigned_state_outputs() {
            let count = bytes(value_type)?
                .checked_add(8)
                .ok_or(TypedCallCompileError::SizeLimit)?;
            let range = self.keep(count)?;
            self.assigned_outputs.push(range);
        }
        Ok(())
    }

    /// The regions and callee frame one operation owns while it runs, placed
    /// from `base`, and the end of that storage.
    fn plan_private_storage(
        &mut self,
        program: &solve::TypedProgram,
        index: usize,
        operation: &solve::SolveOperation,
        base: u32,
        callees: &Callees<'_>,
    ) -> Result<(Vec<Self>, Option<CallFrame>, u32), TypedCallCompileError> {
        match operation {
            solve::SolveOperation::CheckAssertion {
                captures,
                message: solve::SolveAssertionMessage::Captures { program: message },
                ..
            } => self.plan_assertion_message(captures, message, base, callees),
            solve::SolveOperation::AssignedConditional {
                program: conditional,
            } => self.plan_conditional_storage(
                (program, index, true, Demand::all()),
                conditional.captures(),
                conditional.destinations(),
                [conditional.if_true(), conditional.if_false()],
                base,
                callees,
            ),
            solve::SolveOperation::Conditional {
                captures,
                if_true,
                if_false,
                destinations,
                ..
            } => self.plan_conditional_storage(
                (
                    program,
                    index,
                    true,
                    Demand::of_destinations(destinations, self.liveness.needed()),
                ),
                captures,
                destinations,
                [if_true, if_false],
                base,
                callees,
            ),
            solve::SolveOperation::Map {
                domain,
                captures,
                body,
                ..
            } => {
                let region = self.plan_map_region(domain, captures, body, callees, base)?;
                let top = region.counter.offset + region.counter.bytes;
                self.adopt(region.unshared_bytes)?;
                Ok((vec![region], None, top))
            }
            solve::SolveOperation::Fold {
                domain,
                captures,
                destinations,
                transition,
                continuation,
                ..
            } => {
                // Fold destinations are fresh private SSA ranges. They
                // retain the old tuple through every exact old-slot/SSA
                // read. A synchronous transition may consume a private
                // old value only after that complete lifetime ends.
                // Public publication still requires all outputs to succeed.
                let mut borrows = self.capture_borrows(destinations, true);
                // Invariants must also survive all later iterations.
                borrows.extend(self.capture_borrows(captures, false));
                borrows.extend(vec![None; domain.binders.len()]);
                let mut intervals = vec![None; destinations.len()];
                intervals.extend(self.register_intervals(captures));
                intervals.extend(binder_intervals(domain));
                let mut region = Self::region(
                    transition.body(),
                    base,
                    callees,
                    &borrows,
                    OutputPolicy::Carried(destinations.len()),
                    &intervals,
                )?;
                let mut top = region.scratch_bytes;
                region.counter = alloc(2, &mut top, 8)?;
                self.adopt(region.unshared_bytes)?;
                self.adopt(8)?;
                let mut regions = vec![region];
                // An early-exit predicate reads copies of the tuple, the captures
                // and the pass's binders and returns one Boolean; it runs between iterations,
                // so it cannot share the transition's storage.
                let predicate = self.plan_fold_predicate(
                    continuation.as_deref(),
                    top,
                    (destinations, captures, binder_intervals(domain)),
                    callees,
                )?;
                if let Some(predicate) = predicate {
                    top = predicate.scratch_bytes;
                    self.adopt(predicate.unshared_bytes)?;
                    regions.push(predicate);
                }
                Ok((regions, None, top))
            }
            solve::SolveOperation::Call {
                owner,
                destinations,
                ..
            } => self.plan_call_frame(*owner, destinations, base, callees),
            _ => Ok((Vec::new(), None, base)),
        }
    }

    fn plan_conditional_storage(
        &mut self,
        source: (&solve::TypedProgram, usize, bool, Demand),
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        branches: [&solve::SolveProgramRegion; 2],
        base: u32,
        callees: &Callees<'_>,
    ) -> Result<(Vec<Self>, Option<CallFrame>, u32), TypedCallCompileError> {
        let (program, index, references, demand) = source;
        let borrows = self.conditional_borrows(index, captures);
        let intervals = self.register_intervals(captures);
        // The buffers the arms may build large results in are the parent's, so
        // they are reserved before the arms start above them.
        let buffers = if references {
            self.reserve_result_buffers(program, destinations, branches, &borrows)?
        } else {
            vec![None; destinations.len()]
        };
        let base = if references { self.cursor } else { base };
        // A call result built in the buffer of a hinted result of this
        // conditional is built in the buffer of the same result of each arm.
        let mut inherited = Vec::new();
        for (result, destination) in destinations.iter().enumerate() {
            if let Some((output, _)) = self.register_hints.get(&destination.index()) {
                let built = self.forwarded(*output);
                inherited.extend(built.into_iter().map(|forwarded| (result, forwarded)));
            }
        }
        let mut arms = Vec::new();
        for arm in branches {
            let hints = buffers
                .iter()
                .map(|reserved| {
                    reserved.map(|reserved| OutputHint {
                        buffer: reserved.buffer,
                        floor: reserved.inherited_floor.unwrap_or(base),
                    })
                })
                .collect::<Vec<_>>();
            let arm = Self::region_hinted(
                arm.body(),
                base,
                callees,
                (&borrows, &intervals),
                (OutputPolicy::Returned, &hints, &demand, &inherited),
            )?;
            self.adopt(arm.unshared_bytes)?;
            arms.push(arm);
        }
        let hints = buffers
            .iter()
            .map(|reserved| reserved.map(|reserved| reserved.buffer));
        self.place_conditional_results(
            program,
            destinations,
            &mut arms,
            (base, references),
            &hints.collect::<Vec<_>>(),
        )?;
        let top = arms
            .iter()
            .map(|arm| arm.scratch_bytes)
            .fold(base, u32::max);
        Ok((arms, None, top))
    }

    /// The parent buffer of each large result of a conditional whose arms are
    /// about to be planned, with the floor it inherits when it is itself a
    /// buffer an enclosing conditional reserved.
    ///
    /// A result an enclosing region returns whole builds in the buffer of that
    /// output (output-directed placement). Any other large result gets a
    /// buffer of its own unless an arm provably passes a value the parent may
    /// lend without a copy, which needs none.
    fn reserve_result_buffers(
        &mut self,
        program: &solve::TypedProgram,
        destinations: &[solve::SolveRegisterId],
        branches: [&solve::SolveProgramRegion; 2],
        borrows: &[Option<InputBorrow>],
    ) -> Result<Vec<Option<ReservedBuffer>>, TypedCallCompileError> {
        let mut buffers = Vec::with_capacity(destinations.len());
        for (ordinal, destination) in destinations.iter().enumerate() {
            let count = bytes(&program.register_types()[destination.index()])?;
            let buffer = if !crate::cost_model::address_beats_copy(count)
                || !self.liveness.needs(*destination)
            {
                None
            } else if let Some((_, hint)) = self.register_hints.get(&destination.index()) {
                Some(ReservedBuffer {
                    buffer: hint.buffer,
                    inherited_floor: Some(hint.floor),
                })
            } else if outputs::result_needs_buffer(branches, ordinal, borrows) {
                Some(ReservedBuffer {
                    buffer: self.keep(count)?,
                    inherited_floor: None,
                })
            } else {
                None
            };
            buffers.push(buffer);
        }
        Ok(buffers)
    }

    /// Note which registers of the body are the whole return of an output with
    /// a hint.
    fn hint_returned_registers(
        &mut self,
        program: &solve::TypedProgram,
        hints: &[Option<OutputHint>],
    ) {
        for (ordinal, slot) in self.output_slots.iter().enumerate() {
            let Some(Some(hint)) = hints.get(ordinal) else {
                continue;
            };
            if self.liveness.drops(*slot) {
                continue;
            }
            if let Some(register) = outputs::completed_return(program, program.slots()[*slot].id())
            {
                self.register_hints
                    .insert(register.index(), (ordinal, *hint));
            }
        }
    }

    fn plan_assertion_message(
        &mut self,
        captures: &[solve::SolveRegisterId],
        message: &solve::SolveProgramRegion,
        base: u32,
        callees: &Callees<'_>,
    ) -> Result<(Vec<Self>, Option<CallFrame>, u32), TypedCallCompileError> {
        let borrows = self.capture_borrows(captures, false);
        let intervals = self.register_intervals(captures);
        let region = Self::region(
            message.body(),
            base,
            callees,
            &borrows,
            OutputPolicy::Returned,
            &intervals,
        )?;
        let top = region.scratch_bytes;
        self.adopt(region.unshared_bytes)?;
        Ok((vec![region], None, top))
    }

    /// A bounded `while` fold's predicate reads the carried tuple and the
    /// captures, which stay unchanged while it runs, and returns one Boolean.
    /// Both are borrowed from the parent, never copied into the predicate.
    fn plan_fold_predicate(
        &self,
        predicate: Option<&solve::SolveProgramRegion>,
        base: u32,
        (carried, captures, binders): (
            &[solve::SolveRegisterId],
            &[solve::SolveRegisterId],
            Vec<Option<Interval>>,
        ),
        callees: &Callees<'_>,
    ) -> Result<Option<Self>, TypedCallCompileError> {
        let Some(predicate) = predicate else {
            return Ok(None);
        };
        let mut borrows = self.capture_borrows(carried, false);
        borrows.extend(self.capture_borrows(captures, false));
        borrows.extend(vec![None; binders.len()]);
        let mut intervals = vec![None; carried.len()];
        intervals.extend(self.register_intervals(captures));
        intervals.extend(binders);
        Self::region(
            predicate.body(),
            base,
            callees,
            &borrows,
            OutputPolicy::Returned,
            &intervals,
        )
        .map(Some)
    }

    fn plan_map_region(
        &self,
        domain: &rumoca_core::StructuredIndexDomain,
        captures: &[solve::SolveRegisterId],
        body: &solve::SolveProgramRegion,
        callees: &Callees<'_>,
        base: u32,
    ) -> Result<Self, TypedCallCompileError> {
        // Captures are immutable for every iteration; child updates cannot
        // consume their borrowed storage. Binder/counter ranges are private.
        let mut borrows = self.capture_borrows(captures, false);
        borrows.extend(vec![None; domain.binders.len()]);
        let mut intervals = self.register_intervals(captures);
        intervals.extend(binder_intervals(domain));
        let mut region = Self::region(
            body.body(),
            base,
            callees,
            &borrows,
            OutputPolicy::Element,
            &intervals,
        )?;
        let mut top = region.scratch_bytes;
        region.counter = alloc(2, &mut top, 8)?;
        region.unshared_bytes = region
            .unshared_bytes
            .checked_add(8)
            .ok_or(TypedCallCompileError::SizeLimit)?;
        Ok(region)
    }

    fn capture_borrows(
        &self,
        captures: &[solve::SolveRegisterId],
        reusable: bool,
    ) -> Vec<Option<InputBorrow>> {
        captures
            .iter()
            .map(|capture| {
                Some(InputBorrow {
                    range: self.registers[capture.index()],
                    reusable,
                })
            })
            .collect()
    }

    fn conditional_borrows(
        &self,
        index: usize,
        captures: &[solve::SolveRegisterId],
    ) -> Vec<Option<InputBorrow>> {
        captures
            .iter()
            .map(|capture| {
                Some(InputBorrow {
                    range: self.registers[capture.index()],
                    reusable: self.lifetimes.can_consume(
                        index,
                        *capture,
                        &self.registers,
                        &self.register_reusable,
                    ),
                })
            })
            .collect()
    }

    fn plan_operation_registers(
        &mut self,
        program: &solve::TypedProgram,
        index: usize,
        operation: &solve::SolveOperation,
        callees: &Callees<'_>,
    ) -> Result<(), TypedCallCompileError> {
        if let solve::SolveOperation::Call {
            owner,
            arguments,
            destinations,
            ..
        } = operation
        {
            self.call_results =
                self.plan_call_results(program, (*owner, arguments), destinations, callees)?;
            if self.call_results.is_some() {
                return Ok(());
            }
        }
        // A conditional places its results after its arms are planned.
        if matches!(
            operation,
            solve::SolveOperation::Conditional { .. }
                | solve::SolveOperation::AssignedConditional { .. }
        ) {
            return Ok(());
        }
        let mut destinations = Vec::new();
        operation.visit_output_registers(|r| destinations.push(r));
        for destination in destinations {
            self.plan_register(program, operation, index, destination)?;
        }
        Ok(())
    }

    /// The aggregate range an update at `index` rewrites in place: the
    /// aggregate's own storage when this update is its last read, no other
    /// register shares it, and the written value does not overlap it.
    fn consumed_update(
        &self,
        index: usize,
        aggregate: solve::SolveRegisterId,
        value: Option<solve::SolveRegisterId>,
        count: u32,
    ) -> Option<CellRange> {
        let source = self.registers[aggregate.index()];
        let overlaps = value.is_some_and(|value| self.registers[value.index()] == source);
        (!overlaps
            && source.bytes == count
            && self.lifetimes.can_consume(
                index,
                aggregate,
                &self.registers,
                &self.register_reusable,
            ))
        .then_some(source)
    }

    /// An operation the body does not run: its registers have no storage and
    /// nothing reads them.
    fn skip_dead_operation(
        &mut self,
        index: usize,
        operation: &solve::SolveOperation,
    ) -> Result<(), TypedCallCompileError> {
        let mut destinations = Vec::new();
        operation.visit_output_registers(|register| destinations.push(register));
        for destination in destinations {
            self.push_register(destination, Self::unplaced())?;
            self.homes[destination.index()] = Home::Unused;
            self.register_reusable.push(true);
        }
        self.regions.push(Vec::new());
        self.references.push(Vec::new());
        self.calls.push(None);
        self.lifetimes.complete(index);
        Ok(())
    }

    /// The range of a register that is never stored.
    fn unplaced() -> CellRange {
        CellRange {
            base: 2,
            offset: 0,
            bytes: 0,
        }
    }

    /// Storage of a register that owns its value to the end of the body.
    fn fresh_register(&mut self, count: u32) -> Result<CellRange, TypedCallCompileError> {
        advance(&mut self.register_bytes, count)?;
        self.register_count += 1;
        self.largest_register = self.largest_register.max(count);
        self.keep(count)
    }

    /// Ordinary registers alone reuse dead whole allocations. Conditional
    /// results keep their existing fresh cursor/region relocation contract.
    fn recycled_register(&mut self, count: u32) -> Result<CellRange, TypedCallCompileError> {
        let range = if let Some(range) = self.lifetimes.take_free(count) {
            self.adopt(u64::from(count))?;
            range
        } else {
            self.fresh_register(count)?
        };
        self.lifetimes.own(range);
        Ok(range)
    }

    /// Checked SSA construction defines registers in their issuance order.
    fn push_register(
        &mut self,
        destination: solve::SolveRegisterId,
        range: CellRange,
    ) -> Result<(), TypedCallCompileError> {
        if destination.index() != self.registers.len() {
            return Err(TypedCallCompileError::SizeLimit);
        }
        self.lifetimes.issued(destination, range);
        self.registers.push(range);
        self.homes.push(Home::Memory);
        self.intervals.push(None);
        Ok(())
    }

    /// The range a fold's carried value `ordinal` is rewritten in: its initial
    /// value's own storage when the fold is that value's last read and
    /// neither a capture nor another carried value shares it.
    fn consumed_initial(
        &self,
        index: usize,
        initial: &[solve::SolveRegisterId],
        captures: &[solve::SolveRegisterId],
        ordinal: usize,
        count: u32,
    ) -> Option<CellRange> {
        let source = *initial.get(ordinal)?;
        let range = self.registers[source.index()];
        // Registers are compared by range: a capture or another carried value
        // may share this storage through a borrowed result.
        let shared = initial
            .iter()
            .enumerate()
            .filter(|(other, _)| *other != ordinal)
            .map(|(_, register)| register)
            .chain(captures)
            .any(|register| self.registers[register.index()].overlaps(range));
        (!shared
            && range.bytes == count
            && self
                .lifetimes
                .can_consume(index, source, &self.registers, &self.register_reusable))
        .then_some(range)
    }

    fn plan_register(
        &mut self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
        index: usize,
        destination: solve::SolveRegisterId,
    ) -> Result<(), TypedCallCompileError> {
        let count = bytes(&program.register_types()[destination.index()])?;
        let interval = self.interval_of(program, operation);
        if let Some(home) = self.promoted_home(program, operation, destination, interval) {
            // The value never has an address: this range holds no cell.
            let unused = CellRange {
                base: 2,
                offset: 0,
                bytes: 0,
            };
            self.push_register(destination, unused)?;
            self.homes[destination.index()] = home;
            self.intervals[destination.index()] = interval;
            self.register_reusable.push(true);
            return Ok(());
        }
        let alias = match operation {
            solve::SolveOperation::Load { slot, .. }
                if program.slots()[slot.index()].storage() == solve::SolveStorageClass::Input =>
            {
                Some(self.slots[slot.index()])
            }
            solve::SolveOperation::UpdateElement { aggregate, .. } => {
                self.consumed_update(index, *aggregate, None, count)
            }
            // A slice or view update writes only the cells of its value, so
            // the aggregate's own storage is updated in place once nothing
            // reads the old aggregate afterwards (SOLVE-C71).
            solve::SolveOperation::UpdateSlice {
                aggregate, value, ..
            }
            | solve::SolveOperation::UpdateView {
                aggregate, value, ..
            } => self.consumed_update(index, *aggregate, Some(*value), count),
            // A carried value whose initial value nothing reads afterwards is
            // rewritten in the initial value's own storage (SOLVE-C71).
            solve::SolveOperation::Fold {
                initial,
                captures,
                destinations,
                ..
            } => destinations
                .iter()
                .position(|carried| *carried == destination)
                .and_then(|ordinal| {
                    self.consumed_initial(index, initial, captures, ordinal, count)
                }),
            _ => None,
        };
        let range = match alias {
            Some(range) => range,
            None => self.recycled_register(count)?,
        };
        self.push_register(destination, range)?;
        self.intervals[destination.index()] = self.interval_of(program, operation);
        self.register_reusable.push(match operation {
            solve::SolveOperation::Load { slot, .. }
                if program.slots()[slot.index()].storage() == solve::SolveStorageClass::Input =>
            {
                self.slot_reusable[slot.index()]
            }
            _ => true,
        });
        Ok(())
    }
}

fn checked_capture(
    slot: &solve::SolveSlot,
    range: CellRange,
    count: u32,
    parent_end: u32,
) -> Result<CellRange, TypedCallCompileError> {
    let parent_owned = range.base == 0
        || range.pointer_cell().is_some()
        || (range.base == 2
            && range
                .offset
                .checked_add(range.bytes)
                .is_some_and(|end| end <= parent_end));
    if slot.access() != solve::SolveSlotAccess::ReadOnly || range.bytes != count || !parent_owned {
        return Err(TypedCallCompileError::SizeLimit);
    }
    // The synchronous region cannot store to its checked read-only inputs.
    // Borrowed ranges precede child private allocations. Their storage may be
    // reused only under the separately checked old-slot/SSA lifetime relation;
    // invariant captures and caller inputs never receive that permission.
    // Binder inputs remain independently allocated.
    Ok(range)
}

pub(super) fn bytes(value_type: &solve::SolveValueType) -> Result<u32, TypedCallCompileError> {
    value_type
        .scalar_count()
        .checked_mul(8)
        .ok_or(TypedCallCompileError::SizeLimit)
}

pub(super) fn advance(cursor: &mut u32, count: u32) -> Result<(), TypedCallCompileError> {
    *cursor = cursor
        .checked_add(count)
        .ok_or(TypedCallCompileError::SizeLimit)?;
    Ok(())
}

fn alloc(base: u32, cursor: &mut u32, count: u32) -> Result<CellRange, TypedCallCompileError> {
    let range = CellRange {
        base,
        offset: *cursor,
        bytes: count,
    };
    advance(cursor, count)?;
    Ok(range)
}

/// The bytes an output takes in the span at the head of a body's scratch.
fn output_span(
    program: &solve::TypedProgram,
    slot: &solve::SolveSlot,
    convention: InputConvention,
) -> Result<u32, TypedCallCompileError> {
    if returns_by_reference(program, slot, convention) {
        Ok(8)
    } else {
        bytes(slot.value_type())
    }
}

/// Whether the helper returns this output by reference (see `RefOutput`): a
/// large output that the body stores once, whole, as its last act on it.
fn returns_by_reference(
    program: &solve::TypedProgram,
    slot: &solve::SolveSlot,
    convention: InputConvention,
) -> bool {
    slot.storage() == solve::SolveStorageClass::Output
        && bytes(slot.value_type())
            .is_ok_and(|size| convention.references_output(slot.value_type(), size))
        && outputs::completed_return(program, slot.id()).is_some()
}

#[cfg(test)]
mod tests;
