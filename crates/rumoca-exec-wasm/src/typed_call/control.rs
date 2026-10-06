//! Target-local region frames preserve lazy activation and carried snapshots.
mod domain;
mod map;
use super::TypedCallCompileError;
use super::emit::{CELL, Emitter};
use super::layout::{CellRange, FramePlan};
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
                self.address(self.reg(*condition));
                self.push(I::I64Load(CELL));
                self.push(I::I64Eqz);
                self.push(I::I32Eqz);
                self.push(I::If(BlockType::Empty));
                self.branch(index, 0, if_true, captures, destinations)?;
                self.push(I::Else);
                self.branch(index, 1, if_false, captures, destinations)?;
                self.push(I::End);
                Ok(())
            }
            solve::SolveOperation::Fold {
                domain,
                initial,
                captures,
                destinations,
                transition,
            } => {
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
                for (slot, binder) in inputs[initial.len() + captures.len()..]
                    .iter()
                    .zip(&domain.binders)
                {
                    self.store_integer(*slot, binder.lower);
                }
                self.store_integer(child.counter, 0);
                self.fold_loop(index, transition, child, &carried, &inputs, domain)
            }
            _ => unreachable!("checked control dispatch"),
        }
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
        self.region_body(index, arm, region, plan)?;
        let outputs = interface(region.body(), plan, solve::SolveStorageClass::Output);
        let targets = destinations
            .iter()
            .map(|r| self.reg(*r))
            .collect::<Vec<_>>();
        self.copy_pairs(&targets, &outputs);
        Ok(())
    }

    pub(super) fn region_body(
        &mut self,
        index: usize,
        arm: usize,
        region: &'a solve::SolveProgramRegion,
        plan: &'a FramePlan,
    ) -> Result<(), TypedCallCompileError> {
        let (old_program, old_plan) = (self.program, self.plan);
        self.program = region.body();
        self.plan = plan;
        self.region_path.push((index, arm));
        let result = self.emit_body();
        self.region_path.pop();
        self.program = old_program;
        self.plan = old_plan;
        result
    }

    fn fold_loop(
        &mut self,
        index: usize,
        region: &'a solve::SolveProgramRegion,
        plan: &'a FramePlan,
        carried: &[CellRange],
        inputs: &[CellRange],
        domain: &StructuredIndexDomain,
    ) -> Result<(), TypedCallCompileError> {
        let count = domain
            .scalar_count()
            .map_err(|_| TypedCallCompileError::SizeLimit)?;
        let last = if count == 0 {
            Vec::new()
        } else {
            domain
                .index_tuple_at(count - 1)
                .map_err(|_| TypedCallCompileError::SizeLimit)?
                .ok_or(TypedCallCompileError::SizeLimit)?
        };
        self.push(I::Block(BlockType::Empty));
        self.push(I::Loop(BlockType::Empty));
        self.address(plan.counter);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(count as i64));
        self.push(I::I64GeU);
        self.push(I::BrIf(1));
        self.copy_pairs(&inputs[..carried.len()], carried);
        self.region_body(index, 0, region, plan)?;
        let outputs = interface(region.body(), plan, solve::SolveStorageClass::Output);
        self.copy_pairs(carried, &outputs);
        self.increment(plan.counter, 1);
        // Never increment a source binder after its final iteration. The
        // checked domain proves each intermediate binder remains in-domain.
        self.address(plan.counter);
        self.push(I::I64Load(CELL));
        self.push(I::I64Const(count as i64));
        self.push(I::I64GeU);
        self.push(I::BrIf(1));
        self.advance_binders(
            &inputs[inputs.len() - domain.binders.len()..],
            domain,
            &last,
        );
        self.push(I::Br(0));
        self.push(I::End);
        self.push(I::End);
        Ok(())
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

fn interface(
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
