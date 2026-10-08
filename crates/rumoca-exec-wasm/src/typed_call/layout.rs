mod lifetimes;
mod outputs;
mod relocate;
mod report;
use super::TypedCallCompileError;
use lifetimes::Lifetimes;
use outputs::OutputPolicy;
use rumoca_ir_solve as solve;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct CellRange {
    pub base: u32,
    pub offset: u32,
    pub bytes: u32,
}

/// Scratch storage of one program body (an owner or a nested region).
///
/// Slots and registers stay live to the end of the body and are allocated
/// once, bottom up from `cursor`. The storage an operation owns while it runs
/// (its regions, its callee frame and its loop counter) is dead as soon as the
/// operation has copied its results into the body's own registers, so every
/// operation places it at the same base, directly above the live storage, and
/// the sibling arms of one conditional share that base as well. The body's
/// high-water mark is therefore the largest operation-private span above the
/// live storage, not the sum of all of them.
pub(super) struct FramePlan {
    pub slots: Vec<CellRange>,
    pub registers: Vec<CellRange>,
    pub input_bytes: u32,
    pub output_bytes: u32,
    /// First scratch offset of the body (zero for a call owner).
    base: u32,
    /// Every scratch offset the body addresses is below this mark.
    pub scratch_bytes: u32,
    /// Scratch the body would need if no storage were shared between
    /// sequential operations, conditional arms or call frames.
    pub unshared_bytes: u32,
    /// Bytes of freshly allocated (not borrowed or aliased) slots.
    slot_bytes: u32,
    /// Bytes and count of freshly allocated registers, and the largest.
    register_bytes: u32,
    register_count: u32,
    largest_register: u32,
    /// Top of the storage that stays live to the end of the body.
    cursor: u32,
    pub regions: Vec<Vec<FramePlan>>,
    pub counter: CellRange,
    pub calls: Vec<Option<CallFrame>>,
    output_slots: Vec<usize>,
    /// Whether each slot already has its storage; a region places an output
    /// slot where the body first accesses it (see `place_output`).
    placed: Vec<bool>,
    output_policy: OutputPolicy,
    slot_reusable: Vec<bool>,
    register_reusable: Vec<bool>,
    lifetimes: Lifetimes,
}

#[derive(Clone, Copy)]
struct InputBorrow {
    range: CellRange,
    reusable: bool,
}

pub(super) struct CallFrame {
    pub input: CellRange,
    pub output: CellRange,
    pub scratch: CellRange,
}

impl FramePlan {
    /// An empty body whose live storage starts at `base`.
    fn at(base: u32) -> Self {
        Self {
            slots: Vec::new(),
            registers: Vec::new(),
            input_bytes: 0,
            output_bytes: 0,
            base,
            scratch_bytes: base,
            unshared_bytes: 0,
            slot_bytes: 0,
            register_bytes: 0,
            register_count: 0,
            largest_register: 0,
            cursor: base,
            regions: Vec::new(),
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
        }
    }

    /// Storage that stays live to the end of the body.
    fn keep(&mut self, count: u32) -> Result<CellRange, TypedCallCompileError> {
        let range = alloc(2, &mut self.cursor, count)?;
        advance(&mut self.unshared_bytes, count)?;
        self.scratch_bytes = self.scratch_bytes.max(self.cursor);
        Ok(range)
    }

    /// Account a child body or callee frame the operation owns.
    fn adopt(&mut self, unshared: u32) -> Result<(), TypedCallCompileError> {
        advance(&mut self.unshared_bytes, unshared)
    }

    pub(super) fn construct(
        program: &solve::TypedProgram,
        callees: &[Option<Self>],
    ) -> Result<Self, TypedCallCompileError> {
        let mut plan = Self::at(0);
        // Private output slots occupy the first scratch span, allowing one atomic copy.
        for slot in program.slots() {
            if slot.storage() == solve::SolveStorageClass::Output {
                advance(&mut plan.output_bytes, bytes(slot.value_type())?)?;
            }
        }
        plan.cursor = plan.output_bytes;
        plan.scratch_bytes = plan.output_bytes;
        plan.unshared_bytes = plan.output_bytes;
        let mut output_cursor = 0;
        for slot in program.slots() {
            let count = bytes(slot.value_type())?;
            let range = match slot.storage() {
                solve::SolveStorageClass::Input => alloc(0, &mut plan.input_bytes, count)?,
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
            plan.placed.push(true);
            plan.slot_reusable.push(false);
        }
        plan.plan_regions(program, callees)?;
        Ok(plan)
    }

    /// A nested body whose private storage starts at `base`, above every
    /// range of its parent that it borrows.
    fn region(
        program: &solve::TypedProgram,
        base: u32,
        callees: &[Option<Self>],
        input_borrows: &[Option<InputBorrow>],
        output_policy: OutputPolicy,
    ) -> Result<Self, TypedCallCompileError> {
        let mut plan = Self::at(base);
        plan.output_policy = output_policy;
        let mut input_index = 0;
        for slot in program.slots() {
            let count = bytes(slot.value_type())?;
            let borrowed = if slot.storage() == solve::SolveStorageClass::Input {
                let range = *input_borrows
                    .get(input_index)
                    .ok_or(TypedCallCompileError::SizeLimit)?;
                input_index += 1;
                range
            } else {
                None
            };
            let output = slot.storage() == solve::SolveStorageClass::Output;
            let range = match borrowed {
                Some(borrow) => checked_capture(slot, borrow.range, count, base)?,
                // An output slot is placed at its first access, so a completed
                // return can take its source's range without a dead span.
                None if output => CellRange {
                    base: 2,
                    offset: base,
                    bytes: count,
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
            plan.slots.push(range);
            plan.slot_reusable
                .push(borrowed.is_some_and(|borrow| borrow.reusable));
        }
        if input_index != input_borrows.len() {
            return Err(TypedCallCompileError::SizeLimit);
        }
        plan.plan_regions(program, callees)?;
        Ok(plan)
    }

    fn plan_regions(
        &mut self,
        program: &solve::TypedProgram,
        callees: &[Option<Self>],
    ) -> Result<(), TypedCallCompileError> {
        self.lifetimes = Lifetimes::construct(program, &self.slots);
        for (index, operation) in program.operations().iter().enumerate() {
            self.place_accessed_output(program, operation.operation())?;
            self.plan_operation_registers(program, index, operation.operation())?;
            // Every register is below `base`; nothing the operation places at
            // or above it outlives the operation.
            let base = self.cursor;
            let (regions, call, top) =
                self.plan_private_storage(program, index, operation.operation(), base, callees)?;
            self.scratch_bytes = self.scratch_bytes.max(top);
            self.regions.push(regions);
            self.calls.push(call);
        }
        for slot in 0..self.slots.len() {
            self.place_output(program, slot, None)?;
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
        callees: &[Option<Self>],
    ) -> Result<(Vec<Self>, Option<CallFrame>, u32), TypedCallCompileError> {
        match operation {
            solve::SolveOperation::Conditional {
                captures,
                if_true,
                if_false,
                destinations,
                ..
            } => {
                let borrows = self.conditional_borrows(index, captures);
                let mut arms = Vec::new();
                for arm in [if_true, if_false] {
                    let arm =
                        Self::region(arm.body(), base, callees, &borrows, OutputPolicy::Returned)?;
                    self.adopt(arm.unshared_bytes)?;
                    arms.push(arm);
                }
                self.place_conditional_results(program, destinations, &mut arms, base)?;
                let top = arms
                    .iter()
                    .map(|arm| arm.scratch_bytes)
                    .fold(base, u32::max);
                Ok((arms, None, top))
            }
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
                let mut region = Self::region(
                    transition.body(),
                    base,
                    callees,
                    &borrows,
                    OutputPolicy::Carried(destinations.len()),
                )?;
                let mut top = region.scratch_bytes;
                region.counter = alloc(2, &mut top, 8)?;
                self.adopt(region.unshared_bytes)?;
                self.adopt(8)?;
                let mut regions = vec![region];
                // A bounded `while` predicate reads copies of the tuple and the
                // captures and returns one Boolean; it runs between iterations,
                // so it cannot share the transition's storage.
                let predicate = self.plan_fold_predicate(
                    continuation.as_deref(),
                    top,
                    (destinations, captures),
                    callees,
                )?;
                if let Some(predicate) = predicate {
                    top = predicate.scratch_bytes;
                    self.adopt(predicate.unshared_bytes)?;
                    regions.push(predicate);
                }
                Ok((regions, None, top))
            }
            solve::SolveOperation::Call { owner, .. } => {
                let callee = callees
                    .get(owner.index() as usize)
                    .and_then(Option::as_ref)
                    .ok_or(TypedCallCompileError::SiteMismatch)?;
                let mut top = base;
                let frame = CallFrame {
                    input: alloc(2, &mut top, callee.input_bytes)?,
                    output: alloc(2, &mut top, callee.output_bytes)?,
                    scratch: alloc(2, &mut top, callee.scratch_bytes)?,
                };
                self.adopt(callee.input_bytes)?;
                self.adopt(callee.output_bytes)?;
                self.adopt(callee.unshared_bytes)?;
                Ok((Vec::new(), Some(frame), top))
            }
            _ => Ok((Vec::new(), None, base)),
        }
    }

    /// A bounded `while` fold's predicate reads the carried tuple and the
    /// captures, which stay unchanged while it runs, and returns one Boolean.
    /// Both are borrowed from the parent, never copied into the predicate.
    fn plan_fold_predicate(
        &self,
        predicate: Option<&solve::SolveProgramRegion>,
        base: u32,
        (carried, captures): (&[solve::SolveRegisterId], &[solve::SolveRegisterId]),
        callees: &[Option<Self>],
    ) -> Result<Option<Self>, TypedCallCompileError> {
        let Some(predicate) = predicate else {
            return Ok(None);
        };
        let mut borrows = self.capture_borrows(carried, false);
        borrows.extend(self.capture_borrows(captures, false));
        Self::region(
            predicate.body(),
            base,
            callees,
            &borrows,
            OutputPolicy::Returned,
        )
        .map(Some)
    }

    fn plan_map_region(
        &self,
        domain: &rumoca_core::StructuredIndexDomain,
        captures: &[solve::SolveRegisterId],
        body: &solve::SolveProgramRegion,
        callees: &[Option<Self>],
        base: u32,
    ) -> Result<Self, TypedCallCompileError> {
        // Captures are immutable for every iteration; child updates cannot
        // consume their borrowed storage. Binder/counter ranges are private.
        let mut borrows = self.capture_borrows(captures, false);
        borrows.extend(vec![None; domain.binders.len()]);
        let mut region =
            Self::region(body.body(), base, callees, &borrows, OutputPolicy::Returned)?;
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
    ) -> Result<(), TypedCallCompileError> {
        // A conditional places its results after its arms are planned.
        if matches!(operation, solve::SolveOperation::Conditional { .. }) {
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

    /// Storage of a register that owns its value to the end of the body.
    fn fresh_register(&mut self, count: u32) -> Result<CellRange, TypedCallCompileError> {
        advance(&mut self.register_bytes, count)?;
        self.register_count += 1;
        self.largest_register = self.largest_register.max(count);
        self.keep(count)
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
        self.registers.push(range);
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
        let listed_once = initial
            .iter()
            .filter(|register| **register == source)
            .count()
            == 1
            && !captures.contains(&source);
        let range = self.registers[source.index()];
        (listed_once
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
            None => self.fresh_register(count)?,
        };
        self.push_register(destination, range)?;
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

#[cfg(test)]
mod tests;
