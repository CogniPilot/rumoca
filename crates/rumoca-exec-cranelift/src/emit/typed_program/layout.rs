//! Projection of construction-issued storage homes and frame offsets.
//! No target-side lifetime, alias, fusion or compaction decisions live here.

use super::{CompileError, StorageBase, ValueLocation, assigned_state, checked_cells};
use rumoca_ir_solve as solve;

pub(super) struct ProgramLayout {
    pub(super) slots: Box<[ValueLocation]>,
    pub(super) registers: Box<[ValueLocation]>,
    pub(super) assigned_states: Box<[assigned_state::AssignedLocation]>,
    pub(super) assigned_inputs: Box<[assigned_state::AssignedLocation]>,
    pub(super) assigned_outputs: Box<[assigned_state::AssignedLocation]>,
    pub(super) tape_cells: u32,
    /// The operation each fused register defers (see [`solve::SolveFusionPlan`]); such a
    /// register is never stored.
    pub(super) fused: Box<[Option<solve::SolveOperation>]>,
    /// Last reading operation of each register, extended over fusion.
    pub(super) last_uses: Box<[Option<usize>]>,
}

impl ProgramLayout {
    pub(super) fn region(region: &solve::SolveProgramRegion) -> Result<Self, CompileError> {
        Self::new(
            region.body(),
            (region.inputs().len(), region.outputs().len()),
            IssuedHomes::of(region.body()),
        )
    }

    /// Lay `program` out as its issued plan places it: fusion and last reads
    /// from [`solve::SolveStoragePlan::fusion`], register homes and frame
    /// cells from `issued`.
    pub(super) fn new(
        program: &solve::TypedProgram,
        slot_counts: (usize, usize),
        issued: IssuedHomes<'_>,
    ) -> Result<Self, CompileError> {
        let (slots, mut input_cell, mut output_cell, register_base) =
            slot_layout(program, slot_counts)?;
        let registers = issued.locations(program, &slots, register_base)?;
        let mut tape_cell = checked_cells(
            register_base,
            issued.cells.extent(),
            "typed register layout",
        )?;
        let fusion = program.storage_plan().fusion();
        let fused = fusion
            .registers()
            .iter()
            .map(|fused| {
                fused
                    .map(|fused| deferred_operation(program, fused))
                    .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let assigned_states = assigned_state::layout(
            program.assigned_state_types(),
            StorageBase::Tape,
            &mut tape_cell,
        )?;
        let assigned_inputs = assigned_state::layout(
            program.assigned_state_inputs(),
            StorageBase::Input,
            &mut input_cell,
        )?;
        let assigned_outputs = assigned_state::layout(
            program.assigned_state_outputs(),
            StorageBase::Output,
            &mut output_cell,
        )?;
        Ok(Self {
            slots: slots.into_boxed_slice(),
            registers: registers.into_boxed_slice(),
            assigned_states,
            assigned_inputs,
            assigned_outputs,
            tape_cells: tape_cell,
            fused: fused.into_boxed_slice(),
            last_uses: fusion.last_reads().into(),
        })
    }
}

/// The register homes and frame cells one body is laid out under: the
/// program's own issued plan, or its owner's observation-entry plan.
#[derive(Clone, Copy)]
pub(super) struct IssuedHomes<'p> {
    pub(super) aliases: &'p solve::SolveAliasPlan,
    pub(super) cells: &'p solve::SolveFrameCells,
}

impl<'p> IssuedHomes<'p> {
    pub(super) fn of(program: &'p solve::TypedProgram) -> Self {
        let plan = program.storage_plan();
        Self {
            aliases: plan.aliases(),
            cells: plan.cells(),
        }
    }

    /// Each register's location: the slot its home names, or the frame cells
    /// its class was issued, after `register_base` tape cells of slots.
    fn locations(
        self,
        program: &solve::TypedProgram,
        slots: &[ValueLocation],
        register_base: u32,
    ) -> Result<Vec<ValueLocation>, CompileError> {
        let homes = self.aliases.registers();
        let offsets = self.cells.offsets();
        let types = program.register_types();
        if homes.len() != types.len() || offsets.len() != types.len() {
            return Err(CompileError::Backend(
                "issued storage plan does not cover the typed registers".into(),
            ));
        }
        homes
            .iter()
            .zip(offsets)
            .zip(types)
            .enumerate()
            .map(|(register, ((home, offset), value_type))| {
                let (base, cell) = home_cell(*home, *offset, slots, register_base, register)?;
                Ok(ValueLocation {
                    base,
                    cell,
                    value_type: value_type.clone(),
                })
            })
            .collect()
    }
}

/// The base and cell of a register whose issued home is `home` and whose
/// issued frame offset is `offset`.
fn home_cell(
    home: solve::SolveRegisterHome,
    offset: Option<u32>,
    slots: &[ValueLocation],
    register_base: u32,
    register: usize,
) -> Result<(StorageBase, u32), CompileError> {
    match home {
        solve::SolveRegisterHome::Scalar { .. } => Ok((
            StorageBase::Scalar,
            u32::try_from(register)
                .map_err(|_| CompileError::Backend("issued scalar ordinal overflows".into()))?,
        )),
        solve::SolveRegisterHome::InputSlot { slot }
        | solve::SolveRegisterHome::OutputSlot { slot } => slots
            .get(slot)
            .map(|slot| (slot.base, slot.cell))
            .ok_or_else(|| CompileError::Backend("issued register home slot is missing".into())),
        solve::SolveRegisterHome::Own | solve::SolveRegisterHome::Register { .. } => {
            let offset = offset
                .ok_or_else(|| CompileError::Backend("typed register layout overflows".into()))?;
            Ok((
                StorageBase::Tape,
                checked_cells(register_base, offset, "typed register layout")?,
            ))
        }
    }
}

/// The operation an issued fused register defers.
fn deferred_operation(
    program: &solve::TypedProgram,
    fused: solve::SolveFusedRegister,
) -> Result<solve::SolveOperation, CompileError> {
    program
        .operations()
        .get(fused.definition)
        .map(|issued| issued.operation().clone())
        .ok_or_else(|| CompileError::Backend("issued fused definition is missing".into()))
}

/// Every slot's location, then the cells used so far of the input buffer,
/// the output buffer and the tape (where registers start).
fn slot_layout(
    program: &solve::TypedProgram,
    (input_slots, output_slots): (usize, usize),
) -> Result<(Vec<ValueLocation>, u32, u32, u32), CompileError> {
    let mut input_cell = 0u32;
    let mut output_cell = 0u32;
    let mut tape_cell = 0u32;
    let mut slots = Vec::with_capacity(program.slots().len());
    for (index, slot) in program.slots().iter().enumerate() {
        let count = slot.value_type().scalar_count();
        let (base, cell) = if index < input_slots {
            let cell = input_cell;
            input_cell = checked_cells(input_cell, count, "typed input layout")?;
            (StorageBase::Input, cell)
        } else if index < input_slots.saturating_add(output_slots) {
            let cell = output_cell;
            output_cell = checked_cells(output_cell, count, "typed output layout")?;
            (StorageBase::Output, cell)
        } else {
            let cell = tape_cell;
            tape_cell = checked_cells(tape_cell, count, "typed slot layout")?;
            (StorageBase::Tape, cell)
        };
        slots.push(ValueLocation {
            base,
            cell,
            value_type: slot.value_type().clone(),
        });
    }
    Ok((slots, input_cell, output_cell, tape_cell))
}
