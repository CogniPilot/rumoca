//! A fold transition may return its incoming array with an element or a small slice
//! replaced. Return those edits to the caller, which already owns the array.
//! All other carried outputs and all reads see the ordinary old values until
//! the transition completes. Both arms of a conditional must prove the fact.
use serde::Serialize;

use super::super::{
    SolveOperation, SolveRegisterId, SolveRegisterPlacement, SolveSlotPlacement, SolveStorageClass,
    TypedProgram,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub(super) struct FoldEdit {
    output: usize,
    slot: usize,
    store: usize,
    result: usize,
    producer: usize,
    capacity: u32,
    change: Change,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum Change {
    Unchanged,
    Element,
    Slice {
        count: u32,
    },
    Branch {
        yes: Box<FoldEdit>,
        no: Box<FoldEdit>,
    },
}

pub(super) fn plan(op: &SolveOperation) -> Option<FoldEdit> {
    let SolveOperation::Fold { transition, .. } = op else {
        return None;
    };
    transition
        .outputs()
        .iter()
        .enumerate()
        .filter(|(_, ty)| ty.scalar_count() > 1)
        .find_map(|(output, _)| region(transition.body(), output, &[output]))
}

fn input(program: &TypedProgram, register: SolveRegisterId) -> Option<usize> {
    let storage = program.storage_plan();
    let SolveRegisterPlacement::View { slot } = storage.registers()[register.index()] else {
        return None;
    };
    let SolveSlotPlacement::Argument { argument } = storage.slots()[slot] else {
        return None;
    };
    Some(argument)
}

fn region(program: &TypedProgram, output: usize, inputs: &[usize]) -> Option<FoldEdit> {
    let slot = program
        .slots()
        .iter()
        .filter(|s| s.storage() == SolveStorageClass::Output)
        .nth(output)?
        .id();
    let ops = program.operations();
    let mut stores = ops
        .iter()
        .enumerate()
        .filter_map(|(i, op)| match op.operation() {
            SolveOperation::Store {
                slot: target,
                source,
            } if *target == slot => Some((i, *source)),
            _ => None,
        });
    let (store, result) = stores.next()?;
    if stores.next().is_some()
        || ops.iter().any(
            |op| matches!(op.operation(), SolveOperation::Load { slot: read, .. } if *read == slot),
        )
    {
        return None;
    }
    let (producer, op) = ops[..store].iter().enumerate().rev().find(|(_, op)| {
        let mut defines = false;
        op.operation()
            .visit_output_registers(|r| defines |= r == result);
        defines
    })?;
    let change = if input(program, result).is_some_and(|arg| inputs.contains(&arg)) {
        Change::Unchanged
    } else {
        let mut uses = 0;
        for op in ops {
            op.operation()
                .visit_input_registers(|r| uses += usize::from(r == result));
        }
        if uses != 1 {
            return None;
        }
        match op.operation() {
            SolveOperation::UpdateElement { aggregate, .. }
                if input(program, *aggregate).is_some_and(|arg| inputs.contains(&arg)) =>
            {
                Change::Element
            }
            SolveOperation::UpdateSlice {
                aggregate, value, ..
            }
            | SolveOperation::UpdateView {
                aggregate, value, ..
            } if input(program, *aggregate).is_some_and(|arg| inputs.contains(&arg)) => {
                sparse_slice(program, *aggregate, *value)?
            }
            SolveOperation::Conditional {
                destinations,
                captures,
                if_true,
                if_false,
                ..
            } if destinations.len() == 1 => {
                let child_inputs: Vec<_> = captures
                    .iter()
                    .enumerate()
                    .filter_map(|(i, r)| {
                        input(program, *r)
                            .is_some_and(|arg| inputs.contains(&arg))
                            .then_some(i)
                    })
                    .collect();
                if child_inputs.is_empty() {
                    return None;
                }
                Change::Branch {
                    yes: Box::new(region(if_true.body(), 0, &child_inputs)?),
                    no: Box::new(region(if_false.body(), 0, &child_inputs)?),
                }
            }
            _ => return None,
        }
    };
    Some(FoldEdit {
        output,
        slot: slot.index(),
        store,
        result: result.index(),
        producer,
        capacity: match &change {
            Change::Unchanged | Change::Element => 1,
            Change::Slice { count } => *count,
            Change::Branch { yes, no } => yes.capacity.max(no.capacity),
        },
        change,
    })
}

fn sparse_slice(
    program: &TypedProgram,
    aggregate: SolveRegisterId,
    value: SolveRegisterId,
) -> Option<Change> {
    let count = program.register_types()[value.index()].scalar_count();
    let extent = program.register_types()[aggregate.index()].scalar_count();
    // Each returned value carries an index. Keep dense replacements on the ordinary path.
    (count > 0 && count <= extent / 4).then_some(Change::Slice { count })
}
