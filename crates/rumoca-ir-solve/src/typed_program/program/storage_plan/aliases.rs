//! Register aliasing facts of one program body.
//!
//! Instead of cells of its own, a register can take the location of:
//! - the input-buffer slot it loads read-only, an immutable external view;
//! - an output-buffer slot, when its final store is its last read and nothing
//!   loads or stores that slot between its definition and that store;
//! - the cells of the aggregate a functional update consumes, or of the
//!   entry value a fold consumes, when both live in the body's own cells.
//!
//! The rules are applied in that order, each reading the locations the
//! earlier ones left, exactly as the in-process compiled target applied them
//! on its own; the plan now issues the resolved result. They are ported
//! unchanged from that target's `emit/typed_program/aliasing.rs` and the fold
//! rule of `emit/typed_program/storage.rs`. A target that ignores them keeps
//! every register in its own cells, as before.

use serde::Serialize;

use super::super::{
    SolveOperation, SolveRegisterId, SolveSlot, SolveSlotAccess, SolveSlotId,
    SolveSpannedOperation, SolveStorageClass, SolveValueType,
};

/// Where one register's value lives once the alias rules are applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "home", rename_all = "snake_case")]
pub enum SolveRegisterHome {
    /// Cells of its own.
    Own,
    /// The cells of input-buffer slot `slot`, read in place.
    InputSlot { slot: usize },
    /// The cells of output-buffer slot `slot`.
    OutputSlot { slot: usize },
    /// The cells of register `register`, whose home is `Own`.
    Register { register: usize },
}

/// The resolved home of every register of one body.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveAliasPlan {
    homes: Box<[SolveRegisterHome]>,
}

impl SolveAliasPlan {
    /// `last_reads` are the fusion-extended last reads; `unaliased` are
    /// output slots whose cells are written outside the body's stores, which
    /// no register may take.
    pub(in crate::typed_program) fn of(
        slots: &[SolveSlot],
        register_types: &[SolveValueType],
        operations: &[SolveSpannedOperation],
        last_reads: &[Option<usize>],
        unaliased: &[usize],
    ) -> Self {
        let mut homes = vec![SolveRegisterHome::Own; register_types.len()];
        let same_type = |register: SolveRegisterId, slot: SolveSlotId| {
            register_types.get(register.index())
                == slots.get(slot.index()).map(SolveSlot::value_type)
        };
        alias_read_only_loads(slots, operations, &same_type, &mut homes);
        alias_final_output_stores(
            slots, operations, last_reads, unaliased, &same_type, &mut homes,
        );
        alias_consumed_functional_updates(register_types, operations, last_reads, &mut homes);
        alias_consumed_fold_carries(register_types, operations, last_reads, &mut homes);
        Self {
            homes: homes.into_boxed_slice(),
        }
    }

    /// The home of each register.
    #[must_use]
    pub fn registers(&self) -> &[SolveRegisterHome] {
        &self.homes
    }
}

fn last_read_at(last_reads: &[Option<usize>], register: SolveRegisterId, index: usize) -> bool {
    last_reads.get(register.index()) == Some(&Some(index))
}

/// The register `register`'s cells belong to, when they are the body's own.
fn own_root(homes: &[SolveRegisterHome], register: SolveRegisterId) -> Option<usize> {
    match homes.get(register.index())? {
        SolveRegisterHome::Own => Some(register.index()),
        SolveRegisterHome::Register { register } => Some(*register),
        SolveRegisterHome::InputSlot { .. } | SolveRegisterHome::OutputSlot { .. } => None,
    }
}

/// Only the call or region input buffer is an immutable external view.
/// Constant slots are also read-only but live in the body's own cells, and
/// aliasing one into a consumed update could corrupt a later load of it.
fn alias_read_only_loads(
    slots: &[SolveSlot],
    operations: &[SolveSpannedOperation],
    same_type: &impl Fn(SolveRegisterId, SolveSlotId) -> bool,
    homes: &mut [SolveRegisterHome],
) {
    for issued in operations {
        let SolveOperation::Load { destination, slot } = &issued.operation else {
            continue;
        };
        let input_view = slots.get(slot.index()).is_some_and(|declared| {
            declared.access() == SolveSlotAccess::ReadOnly
                && declared.storage() == SolveStorageClass::Input
        });
        if input_view
            && same_type(*destination, *slot)
            && let Some(home) = homes.get_mut(destination.index())
        {
            *home = SolveRegisterHome::InputSlot { slot: slot.index() };
        }
    }
}

fn alias_final_output_stores(
    slots: &[SolveSlot],
    operations: &[SolveSpannedOperation],
    last_reads: &[Option<usize>],
    unaliased: &[usize],
    same_type: &impl Fn(SolveRegisterId, SolveSlotId) -> bool,
    homes: &mut [SolveRegisterHome],
) {
    let definitions = definitions(homes.len(), operations);
    for (index, issued) in operations.iter().enumerate() {
        let SolveOperation::Store { slot, source } = &issued.operation else {
            continue;
        };
        let output = slots
            .get(slot.index())
            .is_some_and(|declared| declared.storage() == SolveStorageClass::Output);
        if unaliased.contains(&slot.index())
            || !output
            || !last_read_at(last_reads, *source, index)
            || !same_type(*source, *slot)
        {
            continue;
        }
        let Some(definition) = definitions.get(source.index()).copied().flatten() else {
            continue;
        };
        if output_slot_is_observed(operations, *slot, definition, index) {
            continue;
        }
        if let Some(home) = homes.get_mut(source.index()) {
            *home = SolveRegisterHome::OutputSlot { slot: slot.index() };
        }
    }
}

/// The operation defining each register.
fn definitions(registers: usize, operations: &[SolveSpannedOperation]) -> Vec<Option<usize>> {
    let mut definitions = vec![None; registers];
    for (index, issued) in operations.iter().enumerate() {
        issued.operation.visit_output_registers(|register| {
            if let Some(definition) = definitions.get_mut(register.index()) {
                *definition = Some(index);
            }
        });
    }
    definitions
}

/// Whether `slot` is loaded or stored between `definition` and `store`.
fn output_slot_is_observed(
    operations: &[SolveSpannedOperation],
    slot: SolveSlotId,
    definition: usize,
    store: usize,
) -> bool {
    operations[definition..store].iter().any(|issued| {
        matches!(
            issued.operation,
            SolveOperation::Load { slot: candidate, .. }
                | SolveOperation::Store { slot: candidate, .. }
                if candidate == slot
        )
    })
}

fn alias_consumed_functional_updates(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    last_reads: &[Option<usize>],
    homes: &mut [SolveRegisterHome],
) {
    for (index, issued) in operations.iter().enumerate() {
        let (destination, aggregate) = match &issued.operation {
            SolveOperation::UpdateElement {
                destination,
                aggregate,
                ..
            }
            | SolveOperation::UpdateSlice {
                destination,
                aggregate,
                ..
            }
            | SolveOperation::UpdateView {
                destination,
                aggregate,
                ..
            } => (*destination, *aggregate),
            _ => continue,
        };
        if last_read_at(last_reads, aggregate, index) {
            take_own_cells(register_types, homes, aggregate, destination);
        }
    }
}

/// A fold whose entry value of a carried target dies at the fold leaves its
/// result in that value's cells.
fn alias_consumed_fold_carries(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    last_reads: &[Option<usize>],
    homes: &mut [SolveRegisterHome],
) {
    for (index, issued) in operations.iter().enumerate() {
        let SolveOperation::Fold {
            initial,
            captures,
            destinations,
            ..
        } = &issued.operation
        else {
            continue;
        };
        for (entry, destination) in initial.iter().zip(destinations) {
            let reads = initial
                .iter()
                .chain(captures.iter())
                .filter(|register| *register == entry)
                .count();
            if reads == 1 && last_read_at(last_reads, *entry, index) {
                take_own_cells(register_types, homes, *entry, *destination);
            }
        }
    }
}

/// `destination` takes `source`'s cells when both are the body's own and of
/// one type.
fn take_own_cells(
    register_types: &[SolveValueType],
    homes: &mut [SolveRegisterHome],
    source: SolveRegisterId,
    destination: SolveRegisterId,
) {
    let (Some(root), Some(_)) = (own_root(homes, source), own_root(homes, destination)) else {
        return;
    };
    let same_type = register_types.get(source.index()).is_some()
        && register_types.get(source.index()) == register_types.get(destination.index());
    if same_type && let Some(home) = homes.get_mut(destination.index()) {
        *home = SolveRegisterHome::Register { register: root };
    }
}

/// Checks an observation table against the ordinary one for the same body.
/// A register is freed when the ordinary table puts it in a predicate slot.
/// The only homes that may differ are those of freed registers: a freed
/// register's last read is its store, so no later rule consumes it, and
/// the cells it takes instead are the cascade. Returns the freed registers
/// that took another register's cells; any other difference, or any home
/// in a predicate slot, is refused.
#[cfg(test)]
pub(in crate::typed_program) fn observation_cascades(
    ordinary: &[SolveRegisterHome],
    observation: &[SolveRegisterHome],
    predicate_slots: &[usize],
) -> Result<Vec<usize>, String> {
    let in_predicate = |home: &SolveRegisterHome| matches!(home, SolveRegisterHome::OutputSlot { slot } if predicate_slots.contains(slot));
    if ordinary.len() != observation.len() {
        return Err("the tables cover different registers".into());
    }
    let mut cascades = Vec::new();
    for (register, (ordinary, observation)) in ordinary.iter().zip(observation).enumerate() {
        if in_predicate(observation) {
            return Err(format!("register {register} lives in a predicate slot"));
        }
        if ordinary == observation {
            continue;
        }
        if !in_predicate(ordinary) {
            return Err(format!(
                "register {register} changed from {ordinary:?} to {observation:?} without being freed"
            ));
        }
        if matches!(observation, SolveRegisterHome::Register { .. }) {
            cascades.push(register);
        }
    }
    Ok(cascades)
}
