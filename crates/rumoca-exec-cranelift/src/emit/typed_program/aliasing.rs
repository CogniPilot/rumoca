//! Register aliasing for a typed program's layout. Instead of cells of its
//! own, a register can take the location of:
//! - the input-buffer slot it loads, which is an immutable external view;
//! - an output slot, when its final store is the register's last read and
//!   nothing loads or stores that slot between the register's definition and
//!   that store;
//! - the aggregate a functional update consumes, when the update is the
//!   aggregate's last read and both live on the tape.
//!
//! A register that meets none of these keeps its own cells. [`register_flow`]
//! supplies each register's single definition and last read.

use rumoca_ir_solve as solve;

use super::{CompileError, StorageBase, ValueLocation};

pub(super) type RegisterFlow = (Vec<Option<usize>>, Vec<Option<usize>>);

pub(super) fn register_flow(
    program: &solve::TypedProgram,
    register_count: usize,
) -> Result<RegisterFlow, CompileError> {
    let mut last_uses = program.register_last_reads().to_vec();
    last_uses.resize(register_count, None);
    let mut definitions = vec![None; register_count];
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let mut invalid_definition = false;
        operation.operation().visit_output_registers(|register| {
            let Some(definition) = definitions.get_mut(register.index()) else {
                invalid_definition = true;
                return;
            };
            if definition.replace(operation_index).is_some() {
                invalid_definition = true;
            }
        });
        if invalid_definition {
            return Err(CompileError::Backend(
                "checked typed register definition is invalid".into(),
            ));
        }
    }
    Ok((last_uses, definitions))
}

pub(super) fn alias_read_only_loads(
    program: &solve::TypedProgram,
    slots: &[ValueLocation],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for operation in program.operations() {
        let solve::SolveOperation::Load { destination, slot } = operation.operation() else {
            continue;
        };
        let slot_definition = program
            .slots()
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed load slot is missing".into()))?;
        if slot_definition.access() != solve::SolveSlotAccess::ReadOnly {
            continue;
        }
        let source = slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed read-only load slot is missing".into()))?
            .clone();
        // Only the call/region input buffer is an immutable external view.
        // Constant slots also have ReadOnly access but live on the mutable
        // tape; aliasing one into a consumed functional update could corrupt a
        // later load from that constant slot.
        if source.base != StorageBase::Input {
            continue;
        }
        let target = registers
            .get_mut(destination.index())
            .ok_or_else(|| CompileError::Backend("typed load destination is missing".into()))?;
        if target.value_type != source.value_type {
            return Err(CompileError::Backend(
                "checked typed load type differs".into(),
            ));
        }
        *target = source;
    }
    Ok(())
}

pub(super) fn alias_final_output_stores(
    program: &solve::TypedProgram,
    (slots, unaliased_slots): (&[ValueLocation], &[usize]),
    (last_uses, definitions): (&[Option<usize>], &[Option<usize>]),
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let solve::SolveOperation::Store { slot, source } = operation.operation() else {
            continue;
        };
        if unaliased_slots.contains(&slot.index()) {
            continue;
        }
        if last_uses.get(source.index()) != Some(&Some(operation_index)) {
            continue;
        }
        let destination = slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed store slot is missing".into()))?
            .clone();
        if destination.base != StorageBase::Output {
            continue;
        }
        let definition = definitions
            .get(source.index())
            .copied()
            .flatten()
            .ok_or_else(|| {
                CompileError::Backend("typed stored register definition is missing".into())
            })?;
        if output_slot_is_observed(program, *slot, definition, operation_index) {
            continue;
        }
        let source_location = registers
            .get_mut(source.index())
            .ok_or_else(|| CompileError::Backend("typed stored register is missing".into()))?;
        if source_location.value_type != destination.value_type {
            return Err(CompileError::Backend(
                "checked typed output store type differs".into(),
            ));
        }
        *source_location = destination;
    }
    Ok(())
}

fn output_slot_is_observed(
    program: &solve::TypedProgram,
    slot: solve::SolveSlotId,
    definition: usize,
    store: usize,
) -> bool {
    program.operations()[definition..store]
        .iter()
        .any(|operation| {
            matches!(
                operation.operation(),
                solve::SolveOperation::Load { slot: candidate, .. }
                    | solve::SolveOperation::Store { slot: candidate, .. }
                    if *candidate == slot
            )
        })
}

pub(super) fn alias_consumed_functional_updates(
    program: &solve::TypedProgram,
    last_uses: &[Option<usize>],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let (destination, aggregate) = match operation.operation() {
            solve::SolveOperation::UpdateElement {
                destination,
                aggregate,
                ..
            }
            | solve::SolveOperation::UpdateSlice {
                destination,
                aggregate,
                ..
            }
            | solve::SolveOperation::UpdateView {
                destination,
                aggregate,
                ..
            } => (*destination, *aggregate),
            _ => continue,
        };
        if last_uses.get(aggregate.index()) != Some(&Some(operation_index)) {
            continue;
        }
        let source = registers
            .get(aggregate.index())
            .ok_or_else(|| CompileError::Backend("typed update source is missing".into()))?
            .clone();
        let target = registers
            .get_mut(destination.index())
            .ok_or_else(|| CompileError::Backend("typed update destination is missing".into()))?;
        if source.base != StorageBase::Tape || target.base != StorageBase::Tape {
            continue;
        }
        if target.value_type != source.value_type {
            return Err(CompileError::Backend(
                "checked functional update type differs".into(),
            ));
        }
        *target = source;
    }
    Ok(())
}
