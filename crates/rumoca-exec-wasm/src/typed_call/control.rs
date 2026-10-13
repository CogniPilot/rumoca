//! Target-local region frames preserve lazy activation and carried snapshots.
mod domain;
mod loops;
pub(super) use loops::{MapOutput, RegionLoop, max_loop_locals};
mod map;
mod unroll;
use super::TypedCallCompileError;
use super::emit::{CELL, Emitter};
use super::known::Facts;
use super::layout::{CellRange, FramePlan, LocalCounts, ReferenceArm, ReferenceResult};
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl<'a> Emitter<'a> {
    pub(super) fn control_operation(
        &mut self,
        index: usize,
        spanned: &'a solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        match spanned.operation() {
            solve::SolveOperation::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => {
                if let Some(arm) = self.known_arm(*condition) {
                    let region = [if_true, if_false][arm];
                    return self.branch(index, arm, region, captures, destinations);
                }
                self.push_scalar(*condition);
                self.push(I::I64Eqz);
                self.push(I::I32Eqz);
                self.push(I::If(BlockType::Empty));
                self.branch(index, 0, if_true, captures, destinations)?;
                self.push(I::Else);
                self.branch(index, 1, if_false, captures, destinations)?;
                self.push(I::End);
                Ok(())
            }
            solve::SolveOperation::Fold { .. } => self.fold_operation(index, spanned),
            _ => unreachable!("checked control dispatch"),
        }
    }

    fn prepare_fold_predicate(
        &mut self,
        index: usize,
        predicate: Option<&'a solve::SolveProgramRegion>,
        initial_count: usize,
        capture_count: usize,
        capture_ranges: &[CellRange],
        state_sources: &[CellRange],
    ) -> Option<(&'a solve::SolveProgramRegion, &'a FramePlan, Vec<CellRange>)> {
        let predicate = predicate?;
        let plan = &self.plan.regions[index][1];
        let predicate_inputs = interface(predicate.body(), plan, solve::SolveStorageClass::Input);
        self.copy_pairs(
            &predicate_inputs[initial_count..initial_count + capture_count],
            capture_ranges,
        );
        self.copy_read_state_inputs(predicate.body(), plan, state_sources);
        Some((predicate, plan, predicate_inputs))
    }

    fn branch(
        &mut self,
        index: usize,
        arm: usize,
        region: &'a solve::SolveProgramRegion,
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
    ) -> Result<(), TypedCallCompileError> {
        let plan = &self.plan.regions[index][arm];
        let inputs = interface(region.body(), plan, solve::SolveStorageClass::Input);
        let sources = captures.iter().map(|r| self.reg(*r)).collect::<Vec<_>>();
        self.copy_pairs(&inputs, &sources);
        self.pending_known = Some(self.known_inputs(0, captures, 0));
        self.region_body(index, arm, region, plan)?;
        let outputs = interface(region.body(), plan, solve::SolveStorageClass::Output);
        self.publish_results(index, arm, destinations, outputs);
        Ok(())
    }

    /// Give each result of a conditional the value `arm` returned: a copy, or the
    /// address of it for a reference result.
    pub(super) fn publish_results(
        &mut self,
        index: usize,
        arm: usize,
        destinations: &[solve::SolveRegisterId],
        outputs: Vec<CellRange>,
    ) {
        let references = &self.plan.references[index];
        for ((destination, output), reference) in destinations.iter().zip(outputs).zip(references) {
            if !self.plan.liveness.needs(*destination) {
                continue;
            }
            match reference {
                Some(reference) => self.publish_reference(reference, output, arm),
                None => self.copy(self.reg(*destination), output),
            }
        }
    }

    /// Point a reference result at the value this arm returns: the parent value
    /// it passes through, the address its own reference result holds, or the
    /// copy of its own value in the result's buffer.
    fn publish_reference(&mut self, reference: &ReferenceResult, output: CellRange, arm: usize) {
        self.push(I::LocalGet(2));
        self.push(I::I32Const(reference.cell as i32));
        self.push(I::I32Add);
        match reference.arms[arm] {
            ReferenceArm::Pass => self.address_as_cell(output),
            ReferenceArm::Build(buffer) => {
                self.copy(buffer, output);
                self.address_as_cell(buffer);
            }
            ReferenceArm::Forward(cell) => {
                self.push(I::LocalGet(2));
                self.push(I::I32Const(cell as i32));
                self.push(I::I32Add);
                self.push(I::I64Load(CELL));
                self.push(I::I64Store(CELL));
                return;
            }
        }
        self.push(I::I64Store(CELL));
    }

    /// Push the address of `value` as an eight-byte cell value.
    fn address_as_cell(&mut self, value: CellRange) {
        self.address_span(value);
        self.push(I::I64ExtendI32U);
    }

    pub(super) fn region_body(
        &mut self,
        index: usize,
        arm: usize,
        region: &'a solve::SolveProgramRegion,
        plan: &'a FramePlan,
    ) -> Result<(), TypedCallCompileError> {
        let (old_program, old_plan) = (self.program, self.plan);
        let folded = matches!(
            old_program.operations()[index].operation(),
            solve::SolveOperation::Fold { .. }
        );
        let old_floor = self.local_floor;
        let old_states = std::mem::replace(&mut self.state_cells, plan.assigned_states.clone());
        let pending = self.pending_loop.take();
        let old_loop = std::mem::replace(&mut self.region_loop, pending);
        let old_known = std::mem::replace(
            &mut self.known,
            vec![None; region.body().register_types().len()],
        );
        let old_intervals = std::mem::replace(&mut self.intervals, plan.intervals.clone());
        let old_cells = std::mem::replace(
            &mut self.known_cells,
            vec![None; region.body().register_types().len()],
        );
        let inputs_known = self.pending_known.take().unwrap_or_default();
        let old_slot_facts = std::mem::replace(&mut self.slot_facts, inputs_known);
        self.local_floor = LocalCounts {
            integers: old_floor.integers + old_plan.own_locals.integers,
            reals: old_floor.reals + old_plan.own_locals.reals,
        };
        if folded {
            self.loop_counters.push(old_plan.regions[index][0].counter);
        }
        self.program = region.body();
        self.plan = plan;
        self.region_path.push((index, arm));
        let result = self.emit_body();
        self.region_path.pop();
        if folded {
            self.loop_counters.pop();
        }
        self.written_outputs = self
            .region_loop
            .as_ref()
            .map_or_else(Vec::new, RegionLoop::written_slots);
        self.region_result = self.stored_result(region.body());
        self.program = old_program;
        self.plan = old_plan;
        self.local_floor = old_floor;
        self.state_cells = old_states;
        self.region_loop = old_loop;
        self.known = old_known;
        self.intervals = old_intervals;
        self.known_cells = old_cells;
        self.slot_facts = old_slot_facts;
        result
    }

    pub(super) fn copy_pairs(&mut self, destinations: &[CellRange], sources: &[CellRange]) {
        for (destination, source) in destinations.iter().zip(sources) {
            self.copy(*destination, *source);
        }
    }

    pub(super) fn store_integer(&mut self, destination: CellRange, value: i64) {
        self.address(destination);
        self.push(I::I64Const(value));
        self.push(I::I64Store(CELL));
    }

    pub(super) fn increment(&mut self, range: CellRange, step: i64) {
        self.address(range);
        self.address(range);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(step));
        self.push(I::I64Add);
        self.push(I::I64Store(CELL));
    }
}

impl<'a> Emitter<'a> {
    fn fold_operation(
        &mut self,
        index: usize,
        spanned: &'a solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::Fold {
            domain,
            initial,
            captures,
            state_captures,
            destinations,
            transition,
            continuation,
        } = spanned.operation()
        else {
            unreachable!("checked fold dispatch")
        };
        let child = &self.plan.regions[index][0];
        let carried = destinations
            .iter()
            .map(|r| self.reg(*r))
            .collect::<Vec<_>>();
        let sources = initial.iter().map(|r| self.reg(*r)).collect::<Vec<_>>();
        self.copy_pairs(&carried, &sources);
        let capture_ranges = captures.iter().map(|r| self.reg(*r)).collect::<Vec<_>>();
        let inputs = interface(transition.body(), child, solve::SolveStorageClass::Input);
        // Captures are immutable snapshots of the outer invocation.
        self.copy_pairs(
            &inputs[initial.len()..initial.len() + captures.len()],
            &capture_ranges,
        );
        let state_sources = state_captures
            .iter()
            .map(|state| self.state_cells[state.index()])
            .collect::<Vec<_>>();
        self.copy_read_state_inputs(transition.body(), child, &state_sources);
        // A test that no pass changes holds on every pass or on none: it is
        // evaluated once, before the loop.
        let guard = solve::invariant_guard(spanned.operation());
        let continuation = if guard.is_some() {
            None
        } else {
            self.prepare_fold_predicate(
                index,
                continuation.as_deref(),
                initial.len(),
                captures.len(),
                &capture_ranges,
                &state_sources,
            )
        };
        if !Self::binders_in_locals(domain, continuation.is_some()) {
            self.store_first_binders(&inputs[initial.len() + captures.len()..], domain);
        }
        let known = self.known_inputs(initial.len(), captures, domain.binders.len());
        let looped = (inputs.as_slice(), known.clone());
        let Some(predicate) = guard else {
            return self.fold_loop(
                index,
                (transition, continuation),
                child,
                &carried,
                looped,
                domain,
            );
        };
        let test = self.invariant_test(
            index,
            predicate,
            (initial.len(), captures),
            (&capture_ranges, &state_sources),
            known,
        )?;
        let guarded = |this: &mut Self| {
            this.fold_loop(index, (transition, None), child, &carried, looped, domain)
        };
        match test {
            InvariantTest::Never => Ok(()),
            InvariantTest::Always => guarded(self),
            InvariantTest::Register(_) | InvariantTest::Cell(_) => {
                self.push_invariant_test(&test);
                self.push(I::If(BlockType::Empty));
                guarded(self)?;
                self.push(I::End);
                Ok(())
            }
        }
    }

    /// Push the exit test decided at run time as an `i32` condition.
    fn push_invariant_test(&mut self, test: &InvariantTest) {
        match test {
            InvariantTest::Register(register) => self.push_scalar(*register),
            InvariantTest::Cell(output) => {
                self.address(*output);
                self.push(I::I64Load(CELL));
            }
            InvariantTest::Never | InvariantTest::Always => return,
        }
        self.push(I::I64Eqz);
        self.push(I::I32Eqz);
    }

    /// Decide the invariant exit test of a fold once: from the register it
    /// returns unchanged, else by evaluating the predicate in its own frame.
    fn invariant_test(
        &mut self,
        index: usize,
        predicate: &'a solve::SolveProgramRegion,
        (initial, captures): (usize, &[solve::SolveRegisterId]),
        (capture_ranges, state_sources): (&[CellRange], &[CellRange]),
        known: Facts,
    ) -> Result<InvariantTest, TypedCallCompileError> {
        // A test that returns one capture unchanged is that capture's register:
        // reading it needs neither the predicate's frame nor its region.
        if let Some(capture) = forwarded_capture(predicate.body(), initial, captures.len()) {
            let register = captures[capture];
            return Ok(match self.known_integer(register) {
                Some(0) => InvariantTest::Never,
                Some(_) => InvariantTest::Always,
                None => InvariantTest::Register(register),
            });
        }
        let (predicate, plan, _) = self
            .prepare_fold_predicate(
                index,
                Some(predicate),
                initial,
                captures.len(),
                capture_ranges,
                state_sources,
            )
            .ok_or(TypedCallCompileError::SiteMismatch)?;
        self.pending_known = Some(known);
        self.region_body(index, 1, predicate, plan)?;
        match self.region_result.take() {
            Some(0) => return Ok(InvariantTest::Never),
            Some(_) => return Ok(InvariantTest::Always),
            None => {}
        }
        let [output] = interface(predicate.body(), plan, solve::SolveStorageClass::Output)[..]
        else {
            return Err(TypedCallCompileError::SiteMismatch);
        };
        Ok(InvariantTest::Cell(output))
    }

    /// The known Integer or Boolean value `program` stored to its Output slot.
    fn stored_result(&self, program: &solve::TypedProgram) -> Option<i64> {
        program
            .operations()
            .iter()
            .find_map(|operation| match operation.operation() {
                solve::SolveOperation::Store { slot, source }
                    if program.slots()[slot.index()].storage()
                        == solve::SolveStorageClass::Output =>
                {
                    Some(self.known_integer(*source))
                }
                _ => None,
            })
            .flatten()
    }
}

/// What the exit test of a fold that no pass changes decided.
enum InvariantTest {
    /// False at compile time: the fold runs no pass.
    Never,
    /// True at compile time: every pass runs.
    Always,
    /// Decided at run time by the Boolean in this register.
    Register(solve::SolveRegisterId),
    /// Decided at run time by the Boolean in this cell.
    Cell(CellRange),
}

/// A bounded `while` fold's predicate region, its frame plan, and its
/// input cells.
pub(super) type PredicateFrame<'a> = (&'a solve::SolveProgramRegion, &'a FramePlan, Vec<CellRange>);

pub(super) fn interface(
    program: &solve::TypedProgram,
    plan: &FramePlan,
    storage: solve::SolveStorageClass,
) -> Vec<CellRange> {
    program
        .slots()
        .iter()
        .filter(|slot| slot.storage() == storage)
        .map(|slot| plan.slots[slot.id().index()])
        .collect()
}

/// The ordinal among the captures of the one capture `predicate` returns
/// unchanged, when it only loads slots and stores that capture's value.
fn forwarded_capture(
    predicate: &solve::TypedProgram,
    initial: usize,
    captures: usize,
) -> Option<usize> {
    let mut stored = None;
    for operation in predicate.operations() {
        match operation.operation() {
            solve::SolveOperation::Load { .. } => {}
            solve::SolveOperation::Store { source, .. } if stored.is_none() => {
                stored = Some(*source);
            }
            _ => return None,
        }
    }
    let source = stored?;
    let slot = predicate
        .operations()
        .iter()
        .find_map(|operation| match operation.operation() {
            solve::SolveOperation::Load { destination, slot } if *destination == source => {
                Some(*slot)
            }
            _ => None,
        })?;
    let ordinal = predicate
        .slots()
        .iter()
        .filter(|candidate| candidate.storage() == solve::SolveStorageClass::Input)
        .position(|input| input.id() == slot)?;
    (initial..initial + captures)
        .contains(&ordinal)
        .then(|| ordinal - initial)
}
