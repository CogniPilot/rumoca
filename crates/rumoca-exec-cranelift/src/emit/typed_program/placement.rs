//! Region slots placed on the enclosing cells that already hold their values.
//!
//! A region normally runs on private frames: a conditional packs its captures
//! into a fresh input tape and unpacks its results from a fresh output tape,
//! and a fold carries its tuple between two frames. Each of those is a copy of
//! every cell that crosses the region boundary, on every entry, and for a fold
//! on every iteration. A region can instead run with its slots placed on the
//! cells that already hold the values: a conditional reads its captures where
//! they live and writes its results into its destinations, and a fold runs its
//! transition on one frame, so a carried value the iteration leaves unchanged
//! is not copied at all and an updated element is written in place.
//!
//! Placement is only correct when no cell is overwritten while the value it
//! held can still be read. [`placement_is_safe`] decides that from the
//! region's own layout and register lifetimes, and a region that fails the
//! check keeps its private frames, so the check only ever removes copies.

use cranelift_codegen::ir::{InstBuilder, Value};
use rumoca_ir_solve as solve;

use super::{
    CELL_BYTES, CompileError, ProgramLayout, ProgramLowerer, StorageBase, ValueLocation,
    checked_cells,
};

/// The cells one value occupies: a root pointer and a cell range from it.
///
/// Roots are compared by value identity. Every frame and tape is its own
/// root, and a placed slot resolves to the root of the cells it sits on, so
/// two cell ranges can only share memory when they share a root.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct PhysicalCells {
    pub(super) root: Value,
    pub(super) cell: u32,
    pub(super) count: u32,
}

impl PhysicalCells {
    pub(super) fn overlaps(self, other: Self) -> bool {
        self.root == other.root
            && self.count > 0
            && other.count > 0
            && u64::from(self.cell) < u64::from(other.cell) + u64::from(other.count)
            && u64::from(other.cell) < u64::from(self.cell) + u64::from(self.count)
    }

    fn at(self, offset: u32, count: u32) -> Self {
        Self {
            root: self.root,
            cell: self.cell + offset,
            count,
        }
    }
}

/// One region slot placed on enclosing cells.
#[derive(Clone, Debug)]
pub(super) struct PlacedSlot {
    /// First cell of the slot in the region's own layout of its base.
    start: u32,
    cells: PhysicalCells,
    /// The root shifted so that adding the slot's region cell lands on
    /// `cells.cell`; computed once where the region is entered.
    pub(super) adjusted: Value,
}

/// The placed slot holding region cell `cell`; slots are in cell order.
pub(super) fn placed_slot(slots: &[PlacedSlot], cell: u32) -> Option<&PlacedSlot> {
    let index = slots.partition_point(|slot| slot.start + slot.cells.count <= cell);
    slots
        .get(index)
        .filter(|slot| slot.cells.count > 0 && slot.start <= cell)
}

impl ProgramLowerer<'_, '_> {
    /// The cells `location` occupies, through any placement of this region.
    pub(super) fn physical(&self, location: &ValueLocation) -> PhysicalCells {
        let count = location.value_type.scalar_count();
        let (placed, base) = match location.base {
            StorageBase::Input => (self.input_placement.as_deref(), self.input),
            StorageBase::Output => (self.output_placement.as_deref(), self.output),
            StorageBase::Tape => (None, self.tape),
        };
        match placed.and_then(|slots| placed_slot(slots, location.cell)) {
            Some(slot) => slot.cells.at(location.cell - slot.start, count),
            None => PhysicalCells {
                root: base,
                cell: location.cell,
                count,
            },
        }
    }

    /// Place `registers`, in order, as a region's consecutive slots on the
    /// cells they occupy here.
    pub(super) fn placed_slots(
        &mut self,
        registers: &[solve::SolveRegisterId],
    ) -> Result<Vec<PlacedSlot>, CompileError> {
        self.placed_slots_from(registers, 0)
    }

    /// [`Self::placed_slots`] for slots whose first region cell is `start`.
    pub(super) fn placed_slots_from(
        &mut self,
        registers: &[solve::SolveRegisterId],
        mut start: u32,
    ) -> Result<Vec<PlacedSlot>, CompileError> {
        let mut slots = Vec::with_capacity(registers.len());
        for register in registers {
            let location = self.register(*register)?.clone();
            let cells = self.physical(&location);
            let shift = (i64::from(cells.cell) - i64::from(start))
                .checked_mul(CELL_BYTES as i64)
                .ok_or_else(|| CompileError::Backend("placed slot offset overflows".into()))?;
            let adjusted = if shift == 0 {
                cells.root
            } else {
                self.builder.ins().iadd_imm(cells.root, shift)
            };
            slots.push(PlacedSlot {
                start,
                cells,
                adjusted,
            });
            start = checked_cells(start, cells.count, "placed region slots")?;
        }
        Ok(slots)
    }
}

impl PlacedSlot {
    /// A slot that stays on its own cells of a frame.
    pub(super) fn on_frame(cells: PhysicalCells) -> Self {
        Self {
            start: cells.cell,
            cells,
            adjusted: cells.root,
        }
    }
}

/// A program run with its first `inputs` slots as inputs and the next
/// `outputs` as outputs: a region's body, or an inlined owner's.
#[derive(Clone, Copy)]
pub(super) struct Shape<'a> {
    pub(super) body: &'a solve::TypedProgram,
    pub(super) inputs: usize,
    pub(super) outputs: usize,
}

impl<'a> Shape<'a> {
    pub(super) fn of(region: &'a solve::SolveProgramRegion) -> Self {
        Self {
            body: region.body(),
            inputs: region.inputs().len(),
            outputs: region.outputs().len(),
        }
    }
}

pub(super) fn slot_cells(slots: &[PlacedSlot]) -> Vec<PhysicalCells> {
    slots.iter().map(|slot| slot.cells).collect()
}

/// Whether a region's storage can be placed on enclosing cells at all: its
/// only inputs and outputs are its slots.
pub(super) fn placeable(region: &solve::SolveProgramRegion) -> bool {
    let body = region.body();
    body.assigned_state_inputs().is_empty() && body.assigned_state_outputs().is_empty()
}

/// How an operation that writes cells may also have read them.
fn reads_before_writing(operation: &solve::SolveOperation) -> bool {
    use solve::SolveOperation as Op;
    // The structured operations pack their inputs into private frames or check
    // their own placement, and a call packs its arguments.
    matches!(
        operation,
        Op::Conditional { .. } | Op::Fold { .. } | Op::Map { .. } | Op::Call { .. }
    )
}

/// Element `e` of the result depends only on element `e` of each operand.
fn elementwise(operation: &solve::SolveOperation) -> bool {
    use solve::SolveOperation as Op;
    matches!(
        operation,
        Op::Unary { .. }
            | Op::Binary { .. }
            | Op::Compare { .. }
            | Op::Convert { .. }
            | Op::Select { .. }
            | Op::Scale { .. }
            | Op::BroadcastBinary { .. }
    )
}

fn update_aggregate(operation: &solve::SolveOperation) -> Option<solve::SolveRegisterId> {
    use solve::SolveOperation as Op;
    match operation {
        Op::UpdateElement { aggregate, .. }
        | Op::UpdateSlice { aggregate, .. }
        | Op::UpdateView { aggregate, .. } => Some(*aggregate),
        _ => None,
    }
}

/// The slot of `slots` (one base, in layout order) holding cell `cell`.
fn containing_slot(slots: &[ValueLocation], cell: u32) -> Option<usize> {
    slots.iter().position(|slot| {
        let count = slot.value_type.scalar_count();
        count > 0 && slot.cell <= cell && cell < slot.cell + count
    })
}

/// Let a functional update whose aggregate dies at it, and sits on placed
/// cells the region may overwrite, write its result onto those cells.
///
/// Input cells are writable only when an output slot sits exactly on them:
/// the enclosing program then holds the region's result there, so the value
/// the input held ends where the region writes it. [`placement_is_safe`]
/// still checks every relocated value.
pub(super) fn relocate_updates_onto_placed(
    region: Shape<'_>,
    layout: &mut ProgramLayout,
    inputs: &[PhysicalCells],
    outputs: &[PhysicalCells],
) {
    let input_count = region.inputs;
    let program = region.body;
    let last_reads = layout.last_uses.clone();
    let writable = |location: &ValueLocation, layout: &ProgramLayout| match location.base {
        StorageBase::Output => true,
        StorageBase::Input => containing_slot(&layout.slots[..input_count], location.cell)
            .is_some_and(|slot| outputs.contains(&inputs[slot])),
        StorageBase::Tape => false,
    };
    for (index, operation) in program.operations().iter().enumerate() {
        let operation = operation.operation();
        let Some(aggregate) = update_aggregate(operation) else {
            continue;
        };
        let mut destination = None;
        operation.visit_output_registers(|register| destination = Some(register));
        let Some(destination) = destination else {
            continue;
        };
        if last_reads.get(aggregate.index()) != Some(&Some(index)) {
            continue;
        }
        let (Some(source), Some(target)) = (
            layout.registers.get(aggregate.index()).cloned(),
            layout.registers.get(destination.index()),
        ) else {
            continue;
        };
        if target.base != StorageBase::Tape
            || target.value_type != source.value_type
            || !writable(&source, layout)
        {
            continue;
        }
        layout.registers[destination.index()] = source;
    }
}

/// Whether `region` may run with its input slots on `inputs` and its output
/// slots on `outputs`.
///
/// Every value held on placed cells occupies them from the operation that
/// writes it to the last operation that reads it there; an input slot's value
/// is written before the region and an output slot's value is read after it.
/// Two different values may share cells only when those spans are disjoint,
/// or meet at one operation that reads the earlier value before writing the
/// later: one that reads all its operands first, an update of the very
/// aggregate it sits on, or an elementwise operation whose operands on those
/// cells sit exactly on its result.
pub(super) fn placement_is_safe(
    region: Shape<'_>,
    layout: &ProgramLayout,
    inputs: &[PhysicalCells],
    outputs: &[PhysicalCells],
) -> Result<bool, CompileError> {
    placement_verdict(region, layout, inputs, outputs)
}

/// Who owns the cells one occupancy holds.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Holder {
    /// The value an input slot entered the region with.
    Entry(usize),
    /// A value an operation wrote.
    Register(usize),
    /// An output slot's copy of a register, made by a store.
    Stored(usize),
}

#[derive(Clone, Copy, Debug)]
struct Occupancy {
    holder: Holder,
    cells: PhysicalCells,
    /// Operation index plus one that writes the value; zero before the region.
    from: usize,
    /// Operation index plus one of the last read; `usize::MAX` after it.
    until: usize,
}

/// Whether placement is sound (see [`placement_is_safe`]).
fn placement_verdict(
    region: Shape<'_>,
    layout: &ProgramLayout,
    inputs: &[PhysicalCells],
    outputs: &[PhysicalCells],
) -> Result<bool, CompileError> {
    if inputs.len() != region.inputs || outputs.len() != region.outputs {
        return Ok(false);
    }
    let shares = outputs
        .iter()
        .any(|output| inputs.iter().any(|input| output.overlaps(*input)));
    if !shares {
        return Ok(true);
    }
    let missing = || CompileError::Backend("placed region slots are missing".into());
    let placed = Placed {
        layout,
        inputs,
        outputs,
        input_slots: layout.slots.get(..region.inputs).ok_or_else(missing)?,
        output_slots: layout
            .slots
            .get(region.inputs..region.inputs + region.outputs)
            .ok_or_else(missing)?,
    };
    let holder = placed.holders(region.body);
    let occupancies = placed.occupancies(region.body, &holder);
    for (first, earlier) in occupancies.iter().enumerate() {
        for later in &occupancies[first + 1..] {
            if !placed.coexist(region.body, &holder, (earlier, later)) {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

/// A region's slots and the enclosing cells they are placed on.
struct Placed<'a> {
    layout: &'a ProgramLayout,
    inputs: &'a [PhysicalCells],
    outputs: &'a [PhysicalCells],
    input_slots: &'a [ValueLocation],
    output_slots: &'a [ValueLocation],
}

impl Placed<'_> {
    /// Physical cells of a register on one of the region's placed slots; a
    /// tape register is private to the region.
    fn cells(&self, location: &ValueLocation) -> Option<PhysicalCells> {
        let (slots, cells) = match location.base {
            StorageBase::Input => (self.input_slots, self.inputs),
            StorageBase::Output => (self.output_slots, self.outputs),
            StorageBase::Tape => return None,
        };
        let slot = containing_slot(slots, location.cell)?;
        Some(cells[slot].at(
            location.cell - slots[slot].cell,
            location.value_type.scalar_count(),
        ))
    }

    fn register_cells(&self, register: solve::SolveRegisterId) -> Option<PhysicalCells> {
        self.layout
            .registers
            .get(register.index())
            .and_then(|location| self.cells(location))
    }

    /// A load whose register sits on the very cells of its slot holds the
    /// slot's entry value; any other placed register holds its own.
    fn holders(&self, program: &solve::TypedProgram) -> Vec<Holder> {
        let mut holder = (0..self.layout.registers.len())
            .map(Holder::Register)
            .collect::<Vec<_>>();
        for operation in program.operations() {
            if let solve::SolveOperation::Load { slot, destination } = operation.operation()
                && slot.index() < self.inputs.len()
                && self.register_cells(*destination) == Some(self.inputs[slot.index()])
            {
                holder[destination.index()] = Holder::Entry(slot.index());
            }
        }
        holder
    }

    /// The output slot, stored register and output cells of a store to one of
    /// the region's output slots.
    fn output_store(
        &self,
        operation: &solve::SolveOperation,
    ) -> Option<(usize, solve::SolveRegisterId, PhysicalCells)> {
        let solve::SolveOperation::Store { slot, source } = operation else {
            return None;
        };
        let output = slot.index().checked_sub(self.inputs.len())?;
        Some((output, *source, *self.outputs.get(output)?))
    }

    /// Every value held on placed cells, with the span it occupies them.
    fn occupancies(&self, program: &solve::TypedProgram, holder: &[Holder]) -> Vec<Occupancy> {
        let operations = program.operations();
        let register_count = self.layout.registers.len();
        let mut definitions = vec![None; register_count];
        let mut entry_until = vec![0usize; self.inputs.len()];
        let mut live_out = vec![false; register_count];
        let mut occupancies = Vec::<Occupancy>::new();
        for (index, operation) in operations.iter().enumerate() {
            let operation = operation.operation();
            operation
                .visit_output_registers(|register| definitions[register.index()] = Some(index));
            if let solve::SolveOperation::Load { slot, .. } = operation
                && slot.index() < self.inputs.len()
            {
                entry_until[slot.index()] = entry_until[slot.index()].max(index + 1);
            }
            let Some((output, source, cells)) = self.output_store(operation) else {
                continue;
            };
            if self.register_cells(source) == Some(cells) {
                live_out[source.index()] = true;
                continue;
            }
            occupancies.push(Occupancy {
                holder: Holder::Stored(output),
                cells,
                from: index + 1,
                until: usize::MAX,
            });
        }
        for (register, location) in self.layout.registers.iter().enumerate() {
            let (Some(cells), Some(definition)) = (self.cells(location), definitions[register])
            else {
                continue;
            };
            let last = self
                .layout
                .last_uses
                .get(register)
                .copied()
                .flatten()
                .unwrap_or(definition)
                + 1;
            let until = if live_out[register] { usize::MAX } else { last };
            match holder[register] {
                Holder::Entry(slot) => entry_until[slot] = entry_until[slot].max(until),
                holder => occupancies.push(Occupancy {
                    holder,
                    cells,
                    from: definition + 1,
                    until,
                }),
            }
        }
        occupancies.extend(
            self.inputs
                .iter()
                .enumerate()
                .map(|(slot, cells)| Occupancy {
                    holder: Holder::Entry(slot),
                    cells: *cells,
                    from: 0,
                    until: entry_until[slot],
                }),
        );
        occupancies
    }

    /// Whether two occupancies can share their cells: different values may
    /// only when their spans are disjoint, or meet at the operation writing
    /// the later one and that operation reads the earlier value first.
    fn coexist(
        &self,
        program: &solve::TypedProgram,
        holder: &[Holder],
        (first, second): (&Occupancy, &Occupancy),
    ) -> bool {
        // Entry values are all written before the region runs; inputs that
        // share cells hold one enclosing value, which the region never writes
        // through an entry.
        let both_entries = matches!(
            (first.holder, second.holder),
            (Holder::Entry(_), Holder::Entry(_))
        );
        if first.holder == second.holder || !first.cells.overlaps(second.cells) || both_entries {
            return true;
        }
        let (earlier, later) = if first.from <= second.from {
            (first, second)
        } else {
            (second, first)
        };
        if earlier.until < later.from {
            return true;
        }
        if earlier.until > later.from || later.from == 0 {
            return false;
        }
        let operation = program.operations()[later.from - 1].operation();
        let register_cells = |register: solve::SolveRegisterId| self.register_cells(register);
        if reads_before_writing(operation) {
            return true;
        }
        if let Some(aggregate) = update_aggregate(operation) {
            let aggregate_holder = holder
                .get(aggregate.index())
                .copied()
                .unwrap_or(Holder::Register(aggregate.index()));
            return aggregate_holder == earlier.holder
                && register_cells(aggregate) == Some(later.cells)
                && operands_avoid(
                    self.layout,
                    operation,
                    Some(aggregate),
                    later.cells,
                    &register_cells,
                );
        }
        elementwise(operation)
            && operands_sit_exactly(self.layout, operation, later.cells, &register_cells)
    }
}

/// Whether every operand other than `except` stays off `cells`.
fn operands_avoid(
    layout: &ProgramLayout,
    operation: &solve::SolveOperation,
    except: Option<solve::SolveRegisterId>,
    cells: PhysicalCells,
    register_cells: &dyn Fn(solve::SolveRegisterId) -> Option<PhysicalCells>,
) -> bool {
    super::fusion::leaf_operands(&layout.fused, operation)
        .into_iter()
        .all(|operand| {
            Some(operand) == except || register_cells(operand).is_none_or(|on| !on.overlaps(cells))
        })
}

/// Whether every operand touching `cells` sits exactly on them.
fn operands_sit_exactly(
    layout: &ProgramLayout,
    operation: &solve::SolveOperation,
    cells: PhysicalCells,
    register_cells: &dyn Fn(solve::SolveRegisterId) -> Option<PhysicalCells>,
) -> bool {
    super::fusion::leaf_operands(&layout.fused, operation)
        .into_iter()
        .all(|operand| register_cells(operand).is_none_or(|on| !on.overlaps(cells) || on == cells))
}
