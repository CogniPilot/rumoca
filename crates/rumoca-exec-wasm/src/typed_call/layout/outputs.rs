//! Completed region returns may share storage; root publication stays private.
use super::{CellRange, FramePlan, ReferenceArm, ReferenceResult, TypedCallCompileError, advance};
use rumoca_ir_solve as solve;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum OutputPolicy {
    Returned,
    Carried(usize),
    /// The body of a Map: a single-cell result is written to its element of the
    /// target by the emitter, so it is never the storage of its slot.
    Element,
}

impl FramePlan {
    /// Place the output slot an operation accesses first (SOLVE-C78).
    pub(super) fn place_accessed_output(
        &mut self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
    ) -> Result<(), TypedCallCompileError> {
        match operation {
            solve::SolveOperation::Store { slot, source } => {
                self.place_output(program, slot.index(), Some(*source))
            }
            solve::SolveOperation::Load { slot, .. } => {
                self.place_output(program, slot.index(), None)
            }
            _ => Ok(()),
        }
    }

    /// Give a region's output slot its storage, once, where the body first
    /// accesses it.
    ///
    /// A slot that is the completed return of one register, with its source in
    /// full and nothing reading it first, is that register's range: the store
    /// is an exact same-range no-op and nothing is allocated. Any other
    /// output is a fresh range above the registers placed so far, so the
    /// storage of every later operation stays disjoint from it. Placing the
    /// slot before it is known to be borrowed would leave its span allocated
    /// but unused in every region level that returns an aggregate.
    pub(super) fn place_output(
        &mut self,
        program: &solve::TypedProgram,
        slot: usize,
        store: Option<solve::SolveRegisterId>,
    ) -> Result<(), TypedCallCompileError> {
        if self.placed[slot] {
            return Ok(());
        }
        self.placed[slot] = true;
        let id = program.slots()[slot].id();
        let bytes = self.slots[slot].bytes;
        if let Some(source) = store.filter(|source| completed_return(program, id) == Some(*source))
        {
            let range = self.registers[source.index()];
            if range.bytes == bytes && self.returns_in_place(program, id, range) {
                self.lifetimes.pin(range);
                self.slots[slot] = range;
                self.slot_reusable[slot] = self.register_reusable[source.index()];
                return Ok(());
            }
        }
        advance(&mut self.slot_bytes, bytes)?;
        self.slots[slot] = self.keep(bytes)?;
        Ok(())
    }

    /// Whether returning `range` for output `slot` is permitted by the policy.
    ///
    /// Cross-ordinal returns of a carried tuple retain independent snapshots:
    /// committing a tuple swap must not overwrite the next old carried source.
    fn returns_in_place(
        &self,
        program: &solve::TypedProgram,
        slot: solve::SolveSlotId,
        range: super::CellRange,
    ) -> bool {
        let count = match self.output_policy {
            OutputPolicy::Returned => return true,
            OutputPolicy::Element => return range.bytes != 8,
            OutputPolicy::Carried(count) => count,
        };
        let ordinal = interface(program, solve::SolveStorageClass::Output)
            .iter()
            .position(|output| *output == slot);
        let inputs = interface(program, solve::SolveStorageClass::Input);
        ordinal.is_some_and(|ordinal| {
            ordinal < count
                && inputs
                    .get(ordinal)
                    .is_some_and(|input| range == self.slots[input.index()])
        })
    }

    /// Place the result registers of a conditional whose arms are planned at
    /// `base`, the end of the parent's live storage (SOLVE-C78).
    ///
    /// A result both arms return as one parent-owned range, or that one arm
    /// passes through, is that range. A large result that an arm passes
    /// through without the right to rewrite it is a reference: the register
    /// is a scratch cell holding the address of the value, the passing arm
    /// stores the address of the parent value it returns, and the other arm
    /// copies its value into the result's buffer and stores that buffer's
    /// address. An arm whose own result already is such a reference to a
    /// value that outlives it forwards the address. Every other result is a
    /// fresh register at `base`, in its buffer when it has one. The arms,
    /// which only know their private storage starts at `base`, move up past
    /// those registers. Allocating the results before the arms are planned
    /// would leave a dead span for every borrowed result.
    pub(super) fn place_conditional_results(
        &mut self,
        program: &solve::TypedProgram,
        destinations: &[solve::SolveRegisterId],
        arms: &mut [Self],
        (base, references): (u32, bool),
        buffers: &[Option<CellRange>],
    ) -> Result<(), TypedCallCompileError> {
        // Region slots retain their issued order, including output ordinal.
        let returned = [arms[0].returned_outputs(), arms[1].returned_outputs()];
        let mut results = Vec::with_capacity(destinations.len());
        let mut fresh = 0;
        for (ordinal, destination) in destinations.iter().enumerate() {
            let count = super::bytes(&program.register_types()[destination.index()])?;
            if !self.liveness.needs(*destination) {
                results.push((count, ConditionalResult::Unread));
                continue;
            }
            let outputs = [&returned[0], &returned[1]];
            let hint = self.register_hints.get(&destination.index());
            let limit = hint.map_or(base, |(_, hint)| hint.floor);
            let forwarded = hint
                .map(|(output, _)| self.forwarded(*output))
                .unwrap_or_default()
                .into_iter()
                .map(|forwarded| forwarded.range)
                .collect::<Vec<_>>();
            let result = match conditional_result(ordinal, outputs, base, count) {
                Some(borrowed) => ConditionalResult::Borrowed(borrowed),
                None => match references
                    .then(|| reference_values(ordinal, arms, outputs, (limit, &forwarded), count))
                    .flatten()
                {
                    Some(values) => ConditionalResult::Reference(values),
                    None => ConditionalResult::Fresh,
                },
            };
            advance(
                &mut fresh,
                result.fresh_bytes(count, buffers[ordinal].is_some()),
            )?;
            results.push((count, result));
        }
        for arm in arms.iter_mut() {
            arm.relocate(base, fresh)?;
        }
        let mut placed = Vec::with_capacity(destinations.len());
        for (ordinal, (destination, (count, result))) in
            destinations.iter().zip(results).enumerate()
        {
            let buffer = buffers[ordinal];
            let (range, reusable, reference) = match result {
                // No operation reads it, so no arm builds it.
                ConditionalResult::Unread => (Self::unplaced(), true, None),
                // Borrowing immutable storage never creates mutation permission.
                ConditionalResult::Borrowed((borrowed, reusable)) => (borrowed, reusable, None),
                ConditionalResult::Fresh => match buffer {
                    Some(buffer) => (buffer, true, None),
                    None => (self.fresh_register(count)?, true, None),
                },
                ConditionalResult::Reference(values) => {
                    let cell = self.keep(8)?;
                    let buffer =
                        match (buffer, values.iter().all(|v| !matches!(v, ArmValue::Build))) {
                            (_, true) => None,
                            (Some(buffer), false) => Some(buffer),
                            (None, false) => Some(self.fresh_register(count)?),
                        };
                    let arm = |arm: usize| match (&values[arm], buffer) {
                        (ArmValue::Pass(_), _) => ReferenceArm::Pass,
                        (ArmValue::Forward { cell, .. }, _) => ReferenceArm::Forward(*cell + fresh),
                        (ArmValue::Build, Some(buffer)) => ReferenceArm::Build(buffer),
                        // A building arm always has the buffer allocated above.
                        (ArmValue::Build, None) => ReferenceArm::Pass,
                    };
                    let range = CellRange::through_scratch_cell(cell.offset, count)?;
                    let sources = values
                        .iter()
                        .flat_map(ArmValue::sources)
                        .collect::<Vec<_>>();
                    let result = ReferenceResult {
                        cell: cell.offset,
                        arms: [arm(0), arm(1)],
                    };
                    (range, false, Some((result, sources)))
                }
            };
            self.push_register(*destination, range)?;
            self.register_reusable.push(reusable);
            if let Some((_, sources)) = &reference {
                self.lifetimes.reference(*destination, range, sources);
                self.offer_forward(*destination, sources);
            }
            placed.push(reference.map(|(result, _)| result));
        }
        self.pending_references = placed;
        Ok(())
    }

    /// A hinted result that names parent values lets the enclosing conditional
    /// forward its address instead of copying it.
    fn offer_forward(&mut self, destination: solve::SolveRegisterId, sources: &[CellRange]) {
        if let Some((output, _)) = self.register_hints.get(&destination.index()) {
            let named = self.expand_forwarded(sources);
            self.forwards.insert(*output, named);
        }
    }

    fn returned_outputs(&self) -> Vec<(super::CellRange, bool)> {
        self.output_slots
            .iter()
            .map(|slot| (self.slots[*slot], self.slot_reusable[*slot]))
            .collect()
    }
}

pub(super) fn completed_return(
    program: &solve::TypedProgram,
    output: solve::SolveSlotId,
) -> Option<solve::SolveRegisterId> {
    let mut result = None;
    let mut tail = false;
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Store { slot, source } if *slot == output => {
                // Multiple stores or an earlier load require a real snapshot.
                if result.is_some() {
                    return None;
                }
                result = Some(*source);
                tail = true;
            }
            solve::SolveOperation::Load { slot, .. } if *slot == output => return None,
            solve::SolveOperation::Store { slot, .. }
                if program.slots()[slot.index()].storage() == solve::SolveStorageClass::Output => {}
            // Assigned-state transport copies presence and payload cells of the
            // internal state ranges and defines only its own registers; it neither
            // consumes nor faults on the returned source.
            solve::SolveOperation::AssignedState { .. } => {}
            _ if tail => return None,
            _ => {}
        }
    }
    // After this store only output stores remain. Each borrowed output store
    // becomes an exact same-range no-op; retained output slots are disjoint
    // fresh allocations. Thus no later operation, including a nested region,
    // can mutate the returned source or fault after its publication.
    result
}

/// The parent range output `ordinal` of a conditional takes, and whether it may
/// be rewritten, from what its two arms return.
///
/// Arm-private ranges are placed at the same base and die with the operation,
/// so only a range the parent already owns (below `base`) may become a register
/// of the parent. Both arms returning the same parent range is an immutable
/// borrow. One arm passing the parent range through while the other builds its
/// value elsewhere puts the result in the parent range, so only the building
/// arm copies; that rewrites the range, so the passing arm's slot must carry the
/// capture's last-read proof (SOLVE-C71) and no other output of either arm may
/// return the range, since it would observe the rewrite.
fn conditional_result(
    ordinal: usize,
    arms: [&Vec<(super::CellRange, bool)>; 2],
    base: u32,
    bytes: u32,
) -> Option<(super::CellRange, bool)> {
    let owned =
        |(range, _): (super::CellRange, bool)| range.bytes == bytes && parent_owned(range, base);
    let (left, right) = (arms[0][ordinal], arms[1][ordinal]);
    if left.0 == right.0 {
        return owned(left).then_some((left.0, left.1 && right.1));
    }
    let passed = passed_through(left, right, owned)?;
    let returned_elsewhere = arms.iter().any(|outputs| {
        outputs
            .iter()
            .enumerate()
            .any(|(other, (range, _))| other != ordinal && *range == passed.0)
    });
    (passed.1 && !returned_elsewhere).then_some(passed)
}

/// The parent-owned range exactly one arm returns, with the permission to
/// rewrite it.
fn passed_through(
    left: (super::CellRange, bool),
    right: (super::CellRange, bool),
    owned: impl Fn((super::CellRange, bool)) -> bool,
) -> Option<(super::CellRange, bool)> {
    match (owned(left), owned(right)) {
        (true, false) => Some(left),
        (false, true) => Some(right),
        _ => None,
    }
}

pub(super) fn interface(
    program: &solve::TypedProgram,
    storage: solve::SolveStorageClass,
) -> Vec<solve::SolveSlotId> {
    program
        .slots()
        .iter()
        .filter(|slot| slot.storage() == storage)
        .map(|slot| slot.id())
        .collect()
}

/// A range a parent body keeps alive: an input span, or scratch below the
/// base where the operation's private storage starts.
pub(super) fn parent_owned(range: super::CellRange, base: u32) -> bool {
    match range.pointer_cell() {
        // A scratch cell made by the arm itself dies with the operation.
        Some(super::PointerCell::Scratch(cell)) => cell
            .checked_add(POINTER_CELL_BYTES)
            .is_some_and(|end| end <= base),
        Some(super::PointerCell::Input(_)) => true,
        None => {
            range.base != 2
                || range
                    .offset
                    .checked_add(range.bytes)
                    .is_some_and(|end| end <= base)
        }
    }
}

/// How a conditional's result register is stored.
enum ConditionalResult {
    /// No running operation reads the result, so no arm builds it.
    Unread,
    /// The parent range both arms return, or the one an arm passes through.
    Borrowed((CellRange, bool)),
    /// A fresh register both arms copy their result into.
    Fresh,
    /// A cell holding the address of the value; see `reference_values`.
    Reference([ArmValue; 2]),
}

/// What one arm of a reference result stores in the cell.
enum ArmValue {
    /// The address of this parent value.
    Pass(CellRange),
    /// The address its own reference result holds, which names these parent
    /// values or the buffer.
    Forward { cell: u32, sources: Vec<CellRange> },
    /// The address of the buffer its value is copied into.
    Build,
}

impl ArmValue {
    fn sources(&self) -> Vec<CellRange> {
        match self {
            Self::Pass(range) => vec![*range],
            Self::Forward { sources, .. } => sources.clone(),
            Self::Build => Vec::new(),
        }
    }
}

impl ConditionalResult {
    /// Parent storage the result takes at the operation's base, given whether
    /// its buffer is already reserved.
    fn fresh_bytes(&self, count: u32, buffered: bool) -> u32 {
        let buffer = if buffered { 0 } else { count };
        match self {
            Self::Borrowed(_) | Self::Unread => 0,
            Self::Fresh => buffer,
            Self::Reference(values) if values.iter().all(|v| !matches!(v, ArmValue::Build)) => {
                POINTER_CELL_BYTES
            }
            Self::Reference(_) => POINTER_CELL_BYTES.saturating_add(buffer),
        }
    }
}

/// Bytes of the cell that holds a reference result's address.
const POINTER_CELL_BYTES: u32 = 8;

/// What each arm of result `ordinal` stores, when the result is large enough
/// to be a reference and at least one arm names a value without copying it.
///
/// A passing arm returns a range of the result's size that lives below `limit`
/// without the right to rewrite it (an input of the body, or a value read
/// after the conditional), so the result may name that range instead of a
/// copy. An arm whose own result is a reference to such values forwards it.
/// The size is a layout decision of the construction, never a run-time test.
fn reference_values(
    ordinal: usize,
    arms: &[FramePlan],
    outputs: [&Vec<(CellRange, bool)>; 2],
    (limit, forwarded): (u32, &[CellRange]),
    bytes: u32,
) -> Option<[ArmValue; 2]> {
    if !crate::cost_model::address_beats_copy(bytes) {
        return None;
    }
    let value = |arm: usize| {
        let (range, reusable) = outputs[arm][ordinal];
        if range.bytes != bytes {
            return ArmValue::Build;
        }
        if let (Some(sources), Some(super::PointerCell::Scratch(cell))) =
            (arms[arm].forwards.get(&ordinal), range.pointer_cell())
        {
            return ArmValue::Forward {
                cell,
                sources: sources.clone(),
            };
        }
        // A call result built in this output's buffer names the buffer or a
        // value that outlives the body.
        if forwarded.contains(&range) || (parent_owned(range, limit) && !reusable) {
            ArmValue::Pass(range)
        } else {
            ArmValue::Build
        }
    };
    let values = [value(0), value(1)];
    values
        .iter()
        .any(|v| !matches!(v, ArmValue::Build))
        .then_some(values)
}

/// Whether the result `ordinal` of a conditional needs a buffer of its own in
/// its parent, judged from the arms' regions before they are planned: not when
/// both arms return the same capture, or when one returns a capture the parent
/// lends for rewriting and the other builds, which share the capture's range.
pub(super) fn result_needs_buffer(
    branches: [&solve::SolveProgramRegion; 2],
    ordinal: usize,
    borrows: &[Option<super::InputBorrow>],
) -> bool {
    let borrow = |input: usize| borrows.get(input).copied().flatten();
    let [left, right] = branches.map(|branch| identity_input(branch.body(), ordinal));
    match (left, right) {
        (Some(left), Some(right)) => {
            let (left, right) = (borrow(left), borrow(right));
            left.map(|b| b.range) != right.map(|b| b.range)
                && [left, right].iter().flatten().any(|b| b.reusable)
        }
        (Some(input), None) | (None, Some(input)) => {
            !borrow(input).is_some_and(|borrowed| borrowed.reusable)
        }
        (None, None) => true,
    }
}

/// The input ordinal an output of a region returns unchanged, if it does.
fn identity_input(program: &solve::TypedProgram, ordinal: usize) -> Option<usize> {
    let output = interface(program, solve::SolveStorageClass::Output)
        .get(ordinal)
        .copied()?;
    let source = completed_return(program, output)?;
    let slot = program
        .operations()
        .iter()
        .find_map(|operation| match operation.operation() {
            solve::SolveOperation::Load { slot, destination } if *destination == source => {
                Some(*slot)
            }
            _ => None,
        })?;
    interface(program, solve::SolveStorageClass::Input)
        .iter()
        .position(|input| *input == slot)
}
