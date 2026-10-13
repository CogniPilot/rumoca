//! Pass loops keep their pass counter in a wasm local and write each binder
//! from it, so a pass reads no counter or binder back from memory.
use super::super::scalars::cell_store;
use super::*;

/// The passes of a domain: their number and the last binder tuple, or `None`
/// for an empty domain, which runs no pass.
pub(super) fn passes(
    domain: &StructuredIndexDomain,
) -> Result<Option<(usize, Vec<i64>)>, TypedCallCompileError> {
    let count = domain
        .scalar_count()
        .map_err(|_| TypedCallCompileError::SizeLimit)?;
    if count == 0 {
        return Ok(None);
    }
    let last = domain
        .index_tuple_at(count - 1)
        .map_err(|_| TypedCallCompileError::SizeLimit)?
        .ok_or(TypedCallCompileError::SizeLimit)?;
    Ok(Some((count, last)))
}

/// A pass loop being emitted: its counter local and, when the binders of a
/// Cartesian domain are held in locals, the first of their locals.
#[derive(Clone, Copy)]
pub(super) struct PassLoop {
    pub counter: u32,
    pub binders: Option<u32>,
    width: u32,
}

impl PassLoop {
    /// The local of binder `ordinal`, when the binders are in locals.
    pub(super) fn binder_local(self, ordinal: usize) -> Option<u32> {
        self.binders.map(|first| first + ordinal as u32)
    }
}

impl<'a> Emitter<'a> {
    /// Whether the binders of `domain` are held in wasm locals: a Cartesian
    /// domain whose passes need no copy of them in their slots.
    pub(super) fn binders_in_locals(domain: &StructuredIndexDomain, tested: bool) -> bool {
        domain.binders.len() > 1 && !tested
    }

    /// Open a loop that runs its first pass unconditionally; the caller
    /// proved the domain non-empty.
    pub(super) fn open_passes(&mut self, domain: &StructuredIndexDomain, locals: bool) -> PassLoop {
        let binders = if locals {
            domain.binders.len() as u32
        } else {
            0
        };
        let counter = self.loop_local_base + self.loop_locals;
        self.loop_locals += 1 + binders;
        self.push(I::I64Const(0));
        self.push(I::LocalSet(counter));
        if locals {
            for (ordinal, binder) in domain.binders.iter().enumerate() {
                self.push(I::I64Const(binder.lower));
                self.push(I::LocalSet(counter + 1 + ordinal as u32));
            }
        }
        self.push(I::Block(BlockType::Empty));
        self.push(I::Loop(BlockType::Empty));
        PassLoop {
            counter,
            binders: locals.then_some(counter + 1),
            width: 1 + binders,
        }
    }

    pub(super) fn close_passes(&mut self, pass: PassLoop) {
        self.push(I::End);
        self.push(I::End);
        self.loop_locals -= pass.width;
    }

    /// End a pass: when another follows, write its binders and repeat.
    pub(super) fn next_pass(
        &mut self,
        pass: PassLoop,
        count: usize,
        (binders, domain, last): (&[CellRange], &StructuredIndexDomain, &[i64]),
    ) {
        self.push(I::LocalGet(pass.counter));
        self.push(I::I64Const(1));
        self.push(I::I64Add);
        self.push(I::LocalTee(pass.counter));
        self.push(I::I64Const(count as i64));
        self.push(I::I64LtU);
        self.push(I::If(BlockType::Empty));
        if let Some(first) = pass.binders {
            self.advance_binder_locals(first, domain, last);
        } else if Self::binders_in_slots(domain) {
            self.advance_binders(binders, domain, last);
        }
        self.push(I::Br(1));
        self.push(I::End);
    }

    pub(super) fn fold_loop(
        &mut self,
        index: usize,
        (region, continuation): (&'a solve::SolveProgramRegion, Option<PredicateFrame<'a>>),
        plan: &'a FramePlan,
        carried: &[CellRange],
        (inputs, known): (&[CellRange], Facts),
        domain: &StructuredIndexDomain,
    ) -> Result<(), TypedCallCompileError> {
        let Some((count, last)) = passes(domain)? else {
            return Ok(());
        };
        if let Some(count) = self.program.unrolled_passes(index, self.observed) {
            let site = super::unroll::UnrolledLoop {
                index,
                region,
                plan,
                domain,
                count,
                known,
            };
            return self.unrolled_fold(site, carried, inputs);
        }
        let observed = self.observed
            && (region.body().observes()
                || continuation
                    .as_ref()
                    .is_some_and(|(predicate, ..)| predicate.body().observes()));
        let locals = Self::binders_in_locals(domain, continuation.is_some());
        let pass_loop = self.open_passes(domain, locals);
        if observed {
            self.address(plan.counter);
            self.push(I::LocalGet(pass_loop.counter));
            self.push(I::I64Store(CELL));
        }
        if let Some(predicate) = &continuation {
            self.fold_exit_test(
                index,
                predicate,
                pass_loop,
                (carried, inputs),
                &known,
                domain,
            )?;
        }
        self.copy_pairs(&inputs[..carried.len()], carried);
        let mut pass = RegionLoop::new(pass_loop.counter, region.body(), domain);
        pass.direct = direct_outputs(region.body(), plan, carried);
        pass.binder_locals = binder_locals(region.body(), pass_loop, domain);
        self.pending_loop = Some(pass);
        self.pending_known = Some(known);
        self.region_body(index, 0, region, plan)?;
        self.copy_unwritten_results(region, plan, carried);
        let binder_cells = &inputs[inputs.len() - domain.binders.len()..];
        self.next_pass(pass_loop, count, (binder_cells, domain, &last));
        self.close_passes(pass_loop);
        Ok(())
    }

    /// MLS §11.2.3: leave before the first pass whose condition is false.
    fn fold_exit_test(
        &mut self,
        index: usize,
        (predicate, predicate_plan, predicate_inputs): &PredicateFrame<'a>,
        pass_loop: PassLoop,
        (carried, inputs): (&[CellRange], &[CellRange]),
        known: &Facts,
        domain: &StructuredIndexDomain,
    ) -> Result<(), TypedCallCompileError> {
        self.copy_pairs(&predicate_inputs[..carried.len()], carried);
        // The predicate reads the binders of the pass about to run.
        if Self::binders_in_slots(domain) {
            let binders = domain.binders.len();
            self.copy_pairs(
                &predicate_inputs[predicate_inputs.len() - binders..],
                &inputs[inputs.len() - binders..],
            );
        }
        self.pending_loop = Some(RegionLoop::new(pass_loop.counter, predicate.body(), domain));
        self.pending_known = Some(known.clone());
        self.region_body(index, 1, predicate, predicate_plan)?;
        let [output] = interface(
            predicate.body(),
            predicate_plan,
            solve::SolveStorageClass::Output,
        )[..] else {
            return Err(TypedCallCompileError::SiteMismatch);
        };
        self.address(output);
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::BrIf(1));
        Ok(())
    }
}

/// The input slot of each binder of a loop whose binders are in locals, with
/// the local that holds its value.
pub(super) fn binder_locals(
    program: &solve::TypedProgram,
    pass: PassLoop,
    domain: &StructuredIndexDomain,
) -> Vec<(usize, u32)> {
    let slots = input_slots(program);
    let first = slots.len().saturating_sub(domain.binders.len());
    slots[first..]
        .iter()
        .enumerate()
        .filter_map(|(ordinal, slot)| Some((*slot, pass.binder_local(ordinal)?)))
        .collect()
}

/// The deepest nesting of pass loops in `program`, counting the regions a
/// loop or a conditional runs in the same function.
pub(in crate::typed_call) fn max_loop_locals(program: &solve::TypedProgram) -> u32 {
    use solve::SolveOperation as O;
    program
        .operations()
        .iter()
        .map(|operation| match operation.operation() {
            O::Fold {
                domain,
                transition,
                continuation,
                ..
            } => {
                loop_width(domain)
                    + max_loop_locals(transition.body()).max(
                        continuation
                            .as_ref()
                            .map_or(0, |predicate| max_loop_locals(predicate.body())),
                    )
            }
            O::Map { domain, body, .. } => loop_width(domain) + max_loop_locals(body.body()),
            O::Conditional {
                if_true, if_false, ..
            } => max_loop_locals(if_true.body()).max(max_loop_locals(if_false.body())),
            O::AssignedConditional { program } => max_loop_locals(program.if_true().body())
                .max(max_loop_locals(program.if_false().body())),
            O::CheckAssertion {
                message: solve::SolveAssertionMessage::Captures { program },
                ..
            } => max_loop_locals(program.body()),
            _ => 0,
        })
        .max()
        .unwrap_or(0)
}

/// How a pass of a loop reaches its binder and its Map result: the pass
/// counter local, the only binder read from it, the Map element it writes.
#[derive(Clone)]
pub(in crate::typed_call) struct RegionLoop {
    pub counter: u32,
    /// The input slot of the only binder, with the first value and the step.
    pub binder: Option<(usize, i64, i64)>,
    pub output: Option<MapOutput>,
    /// Carried results the body stores straight into their carried cell.
    pub direct: Vec<DirectOutput>,
    /// For a pass emitted on its own (an unrolled loop): its index and the
    /// value of its binder.
    pub unrolled: Option<(usize, i64)>,
    /// The binders of a Cartesian domain held in locals: their input slots
    /// and the local holding each value.
    pub binder_locals: Vec<(usize, u32)>,
}

/// The result slot of a Map body and the element of the target it fills.
#[derive(Clone)]
pub(in crate::typed_call) struct MapOutput {
    pub slot: usize,
    pub target: CellRange,
    pub bytes: u32,
    pub written: bool,
}

/// A carried result slot and the cell of its carried value.
#[derive(Clone)]
pub(in crate::typed_call) struct DirectOutput {
    pub slot: usize,
    pub target: CellRange,
    pub written: bool,
}

impl RegionLoop {
    /// The result slots whose store already reached their final cell.
    pub(in crate::typed_call) fn written_slots(&self) -> Vec<usize> {
        self.output
            .iter()
            .filter(|output| output.written)
            .map(|output| output.slot)
            .chain(
                self.direct
                    .iter()
                    .filter(|direct| direct.written)
                    .map(|direct| direct.slot),
            )
            .collect()
    }
}

/// The slot ids of the input interface of `program`, in order.
fn input_slots(program: &solve::TypedProgram) -> Vec<usize> {
    program
        .slots()
        .iter()
        .filter(|slot| slot.storage() == solve::SolveStorageClass::Input)
        .map(|slot| slot.id().index())
        .collect()
}

impl RegionLoop {
    /// The loop of a region whose only binder is its last input slot.
    pub(super) fn new(
        counter: u32,
        program: &solve::TypedProgram,
        domain: &StructuredIndexDomain,
    ) -> Self {
        let binder = match &domain.binders[..] {
            [binder] => input_slots(program)
                .last()
                .map(|slot| (*slot, binder.lower, binder.step)),
            _ => None,
        };
        Self {
            counter,
            binder,
            output: None,
            direct: Vec::new(),
            unrolled: None,
            binder_locals: Vec::new(),
        }
    }
}

impl Emitter<'_> {
    /// Whether the binders of `domain` are stored in their input slots, as
    /// opposed to computed from the pass counter.
    pub(super) fn binders_in_slots(domain: &StructuredIndexDomain) -> bool {
        domain.binders.len() != 1
    }

    /// Push the value of the binder `Load` of a slot reads, when the pass
    /// counter determines it.
    pub(super) fn push_binder_of(&mut self, slot: usize) -> bool {
        let Some(RegionLoop {
            counter,
            binder: Some((binder_slot, lower, step)),
            unrolled,
            ..
        }) = self.region_loop.clone()
        else {
            return false;
        };
        if binder_slot != slot {
            return false;
        }
        if let Some((_, value)) = unrolled {
            self.push(I::I64Const(value));
            return true;
        }
        self.push(I::LocalGet(counter));
        if step != 1 {
            self.push(I::I64Const(step));
            self.push(I::I64Mul);
        }
        if lower != 0 {
            self.push(I::I64Const(lower));
            self.push(I::I64Add);
        }
        true
    }

    /// Store `source` where the pass leaves its result when `slot` is a result
    /// slot of the loop body: the Map element the pass owns, or the carried
    /// cell of a result nothing reads afterwards.
    pub(in crate::typed_call) fn store_loop_output(
        &mut self,
        slot: usize,
        source: solve::SolveRegisterId,
    ) -> bool {
        if self.plan.ref_outputs.contains_key(&slot) {
            return false;
        }
        if let Some(direct) = self.region_loop.as_mut().and_then(|pass| {
            pass.direct
                .iter_mut()
                .find(|d| d.slot == slot && !d.written)
        }) {
            direct.written = true;
            let target = direct.target;
            self.address(target);
            self.push_scalar(source);
            self.push(cell_store(self.is_real(source)));
            return true;
        }
        let Some(RegionLoop {
            counter,
            output: Some(output),
            unrolled,
            ..
        }) = self.region_loop.clone()
        else {
            return false;
        };
        if output.slot != slot {
            return false;
        }
        match unrolled {
            Some((pass, _)) => self.address(CellRange {
                offset: output.target.offset + pass as u32 * output.bytes,
                bytes: output.bytes,
                ..output.target
            }),
            None => {
                self.address(output.target);
                self.push(I::LocalGet(counter));
                self.push(I::I64Const(i64::from(output.bytes)));
                self.push(I::I64Mul);
                self.push(I::I32WrapI64);
                self.push(I::I32Add);
            }
        }
        if output.bytes == 8 && self.program.register_types()[source.index()].scalar_count() == 1 {
            self.push_scalar(source);
            self.push(cell_store(self.is_real(source)));
        } else {
            self.address(self.reg(source));
            self.copy_addresses(output.bytes);
        }
        if let Some(RegionLoop {
            output: Some(output),
            ..
        }) = self.region_loop.as_mut()
        {
            output.written = true;
        }
        true
    }
}

impl Emitter<'_> {
    /// Define `destination` as the binder `slot` holds when the pass counter
    /// determines it. Returns whether it did.
    pub(in crate::typed_call) fn binder_load(
        &mut self,
        destination: solve::SolveRegisterId,
        slot: usize,
    ) -> bool {
        let local = self.region_loop.as_ref().and_then(|pass| {
            pass.binder_locals
                .iter()
                .find(|(binder, _)| *binder == slot)
                .map(|(_, local)| *local)
        });
        if let Some(local) = local {
            self.begin_result(destination);
            self.push(I::LocalGet(local));
            self.end_result(destination);
            return true;
        }
        let in_counter = self
            .region_loop
            .as_ref()
            .and_then(|pass| pass.binder)
            .is_some_and(|(binder, ..)| binder == slot);
        if !in_counter {
            return false;
        }
        if let Some((_, value)) = self.region_loop.as_ref().and_then(|pass| pass.unrolled) {
            self.define_known(destination, value);
            return true;
        }
        self.begin_result(destination);
        self.push_binder_of(slot);
        self.end_result(destination);
        true
    }
}

impl Emitter<'_> {
    /// Store the first binder values in their slots, for a domain whose
    /// binders are read from slots rather than from the pass counter.
    pub(super) fn store_first_binders(
        &mut self,
        slots: &[CellRange],
        domain: &StructuredIndexDomain,
    ) {
        if !Self::binders_in_slots(domain) {
            return;
        }
        for (slot, binder) in slots.iter().zip(&domain.binders) {
            self.store_integer(*slot, binder.lower);
        }
    }
}

/// The ids of the slots of `program` with `storage`, in order.
pub(super) fn slot_ids(
    program: &solve::TypedProgram,
    storage: solve::SolveStorageClass,
) -> Vec<usize> {
    program
        .slots()
        .iter()
        .filter(|slot| slot.storage() == storage)
        .map(|slot| slot.id().index())
        .collect()
}

/// Whether two ranges of the scratch frame share a byte.
fn overlap(first: CellRange, second: CellRange) -> bool {
    first.base == second.base
        && first.bytes > 0
        && second.bytes > 0
        && first.offset < second.offset.saturating_add(second.bytes)
        && second.offset < first.offset.saturating_add(first.bytes)
}

/// The carried results the body of a pass may store straight into the carried
/// cell instead of its result slot and a copy after the pass.
///
/// A result qualifies when it is one cell reached without a pointer, the body
/// has one store to its slot, and nothing after that store reads or writes a
/// register sharing the carried cell or loads the carried input again: the
/// early write is then invisible to the rest of the pass.
pub(super) fn direct_outputs(
    program: &solve::TypedProgram,
    plan: &FramePlan,
    carried: &[CellRange],
) -> Vec<DirectOutput> {
    use solve::SolveOperation as O;
    let inputs = slot_ids(program, solve::SolveStorageClass::Input);
    let outputs = slot_ids(program, solve::SolveStorageClass::Output);
    let operations = program.operations();
    let mut direct = Vec::new();
    for (ordinal, cell) in carried.iter().enumerate() {
        let (Some(&input), Some(&output)) = (inputs.get(ordinal), outputs.get(ordinal)) else {
            continue;
        };
        if cell.bytes != 8
            || cell.pointer_cell().is_some()
            || plan.ref_outputs.contains_key(&output)
        {
            continue;
        }
        let stores = operations
            .iter()
            .enumerate()
            .filter(|(_, operation)| {
                matches!(operation.operation(), O::Store { slot, .. } if slot.index() == output)
            })
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        let [store] = stores[..] else {
            continue;
        };
        let touched_later = operations[store + 1..].iter().any(|operation| {
            let mut touches = matches!(
                operation.operation(),
                O::Load { slot, .. } if slot.index() == input
            );
            operation
                .operation()
                .visit_input_registers(|r| touches |= overlap(plan.registers[r.index()], *cell));
            operation
                .operation()
                .visit_output_registers(|r| touches |= overlap(plan.registers[r.index()], *cell));
            touches
        });
        if !touched_later {
            direct.push(DirectOutput {
                slot: output,
                target: *cell,
                written: false,
            });
        }
    }
    direct
}

/// The locals a loop over `domain` holds: its counter and, for a Cartesian
/// domain, one per binder.
fn loop_width(domain: &StructuredIndexDomain) -> u32 {
    1 + if domain.binders.len() > 1 {
        domain.binders.len() as u32
    } else {
        0
    }
}
