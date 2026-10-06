mod lifetimes;
mod outputs;
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

pub(super) struct FramePlan {
    pub slots: Vec<CellRange>,
    pub registers: Vec<CellRange>,
    pub input_bytes: u32,
    pub output_bytes: u32,
    pub scratch_bytes: u32,
    pub regions: Vec<Vec<FramePlan>>,
    pub counter: CellRange,
    pub calls: Vec<Option<CallFrame>>,
    output_slots: Vec<usize>,
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
    pub(super) fn construct(
        program: &solve::TypedProgram,
        callees: &[Option<Self>],
    ) -> Result<Self, TypedCallCompileError> {
        let mut plan = Self {
            slots: Vec::new(),
            registers: Vec::new(),
            input_bytes: 0,
            output_bytes: 0,
            scratch_bytes: 0,
            regions: Vec::new(),
            calls: Vec::new(),
            counter: CellRange {
                base: 2,
                offset: 0,
                bytes: 0,
            },
            output_slots: Vec::new(),
            slot_reusable: Vec::new(),
            register_reusable: Vec::new(),
            lifetimes: Lifetimes::default(),
        };
        // Private output slots occupy the first scratch span, allowing one atomic copy.
        for slot in program.slots() {
            if slot.storage() == solve::SolveStorageClass::Output {
                advance(&mut plan.output_bytes, bytes(slot.value_type())?)?;
            }
        }
        plan.scratch_bytes = plan.output_bytes;
        let mut output_cursor = 0;
        for slot in program.slots() {
            let count = bytes(slot.value_type())?;
            let range = match slot.storage() {
                solve::SolveStorageClass::Input => alloc(0, &mut plan.input_bytes, count)?,
                solve::SolveStorageClass::Output => alloc(2, &mut output_cursor, count)?,
                _ => alloc(2, &mut plan.scratch_bytes, count)?,
            };
            if slot.storage() == solve::SolveStorageClass::Output {
                plan.output_slots.push(plan.slots.len());
            }
            plan.slots.push(range);
            plan.slot_reusable.push(false);
        }
        plan.plan_regions(program, callees)?;
        Ok(plan)
    }

    fn region(
        program: &solve::TypedProgram,
        cursor: &mut u32,
        callees: &[Option<Self>],
        input_borrows: &[Option<InputBorrow>],
        output_policy: OutputPolicy,
    ) -> Result<Self, TypedCallCompileError> {
        let mut plan = Self {
            slots: Vec::new(),
            registers: Vec::new(),
            input_bytes: 0,
            output_bytes: 0,
            scratch_bytes: *cursor,
            regions: Vec::new(),
            calls: Vec::new(),
            counter: CellRange {
                base: 2,
                offset: 0,
                bytes: 0,
            },
            output_slots: Vec::new(),
            slot_reusable: Vec::new(),
            register_reusable: Vec::new(),
            lifetimes: Lifetimes::default(),
        };
        let parent_end = *cursor;
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
            let range = match borrowed {
                Some(borrow) => checked_capture(slot, borrow.range, count, parent_end)?,
                None => alloc(2, &mut plan.scratch_bytes, count)?,
            };
            if slot.storage() == solve::SolveStorageClass::Output {
                plan.output_slots.push(plan.slots.len());
            }
            plan.slots.push(range);
            plan.slot_reusable
                .push(borrowed.is_some_and(|borrow| borrow.reusable));
        }
        if input_index != input_borrows.len() {
            return Err(TypedCallCompileError::SizeLimit);
        }
        plan.plan_regions(program, callees)?;
        plan.borrow_completed_outputs(program, output_policy);
        *cursor = plan.scratch_bytes;
        Ok(plan)
    }

    fn plan_regions(
        &mut self,
        program: &solve::TypedProgram,
        callees: &[Option<Self>],
    ) -> Result<(), TypedCallCompileError> {
        self.lifetimes = Lifetimes::construct(program, &self.slots);
        for (index, operation) in program.operations().iter().enumerate() {
            self.plan_operation_registers(program, index, operation.operation())?;
            let mut regions = Vec::new();
            let mut call = None;
            match operation.operation() {
                solve::SolveOperation::Conditional {
                    captures,
                    if_true,
                    if_false,
                    destinations,
                    ..
                } => {
                    let borrows = self.conditional_borrows(index, captures);
                    regions.push(Self::region(
                        if_true.body(),
                        &mut self.scratch_bytes,
                        callees,
                        &borrows,
                        OutputPolicy::Returned,
                    )?);
                    regions.push(Self::region(
                        if_false.body(),
                        &mut self.scratch_bytes,
                        callees,
                        &borrows,
                        OutputPolicy::Returned,
                    )?);
                    self.borrow_conditional_results(destinations, &regions);
                }
                solve::SolveOperation::Map {
                    domain,
                    captures,
                    body,
                    ..
                } => {
                    regions.push(self.plan_map_region(domain, captures, body, callees)?);
                }
                solve::SolveOperation::Fold {
                    domain,
                    captures,
                    destinations,
                    transition,
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
                        &mut self.scratch_bytes,
                        callees,
                        &borrows,
                        OutputPolicy::Carried(destinations.len()),
                    )?;
                    region.counter = alloc(2, &mut self.scratch_bytes, 8)?;
                    regions.push(region);
                }
                solve::SolveOperation::Call { owner, .. } => {
                    let callee = callees
                        .get(owner.index() as usize)
                        .and_then(Option::as_ref)
                        .ok_or(TypedCallCompileError::SiteMismatch)?;
                    call = Some(CallFrame {
                        input: alloc(2, &mut self.scratch_bytes, callee.input_bytes)?,
                        output: alloc(2, &mut self.scratch_bytes, callee.output_bytes)?,
                        scratch: alloc(2, &mut self.scratch_bytes, callee.scratch_bytes)?,
                    });
                }
                _ => {}
            }
            self.regions.push(regions);
            self.calls.push(call);
        }
        Ok(())
    }

    fn plan_map_region(
        &mut self,
        domain: &rumoca_core::StructuredIndexDomain,
        captures: &[solve::SolveRegisterId],
        body: &solve::SolveProgramRegion,
        callees: &[Option<Self>],
    ) -> Result<Self, TypedCallCompileError> {
        // Captures are immutable for every iteration; child updates cannot
        // consume their borrowed storage. Binder/counter ranges are private.
        let mut borrows = self.capture_borrows(captures, false);
        borrows.extend(vec![None; domain.binders.len()]);
        let mut region = Self::region(
            body.body(),
            &mut self.scratch_bytes,
            callees,
            &borrows,
            OutputPolicy::Returned,
        )?;
        region.counter = alloc(2, &mut self.scratch_bytes, 8)?;
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
        let mut destinations = Vec::new();
        operation.visit_output_registers(|r| destinations.push(r));
        for destination in destinations {
            self.plan_register(program, operation, index, destination)?;
        }
        Ok(())
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
            solve::SolveOperation::UpdateElement { aggregate, .. }
                if self.lifetimes.can_consume(
                    index,
                    *aggregate,
                    &self.registers,
                    &self.register_reusable,
                ) =>
            {
                let source = self.registers[aggregate.index()];
                (source.bytes == count).then_some(source)
            }
            _ => None,
        };
        let range = match alias {
            Some(range) => range,
            None => alloc(2, &mut self.scratch_bytes, count)?,
        };
        // Checked SSA construction defines registers in their issuance order.
        if destination.index() != self.registers.len() {
            return Err(TypedCallCompileError::SizeLimit);
        }
        self.registers.push(range);
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

fn bytes(value_type: &solve::SolveValueType) -> Result<u32, TypedCallCompileError> {
    value_type
        .scalar_count()
        .checked_mul(8)
        .ok_or(TypedCallCompileError::SizeLimit)
}

fn advance(cursor: &mut u32, count: u32) -> Result<(), TypedCallCompileError> {
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
