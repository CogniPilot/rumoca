//! Conservative dependence on values that can change in Continuous-Time Mode.

use crate::{
    SolveOperation as Op, SolvePureCallTable, SolveRegisterId, SolveStorageClass, TypedProgram,
};

pub(super) fn outputs(
    table: &SolvePureCallTable,
    program: &TypedProgram,
    inputs: &[bool],
) -> Vec<bool> {
    let mut slots = vec![false; program.slots().len()];
    for (slot, input) in program
        .slots()
        .iter()
        .filter(|s| s.storage() == SolveStorageClass::Input)
        .zip(inputs)
    {
        slots[slot.id().index()] = *input;
    }
    let mut registers = vec![false; program.register_types().len()];
    for operation in program.operations() {
        transfer(table, operation.operation(), &mut slots, &mut registers);
    }
    program
        .slots()
        .iter()
        .filter(|s| s.storage() == SolveStorageClass::Output)
        .map(|s| slots[s.id().index()])
        .collect()
}

fn read(registers: &[bool], ids: &[SolveRegisterId]) -> Vec<bool> {
    ids.iter().map(|id| registers[id.index()]).collect()
}
fn write(registers: &mut [bool], ids: &[SolveRegisterId], values: &[bool]) {
    for (id, value) in ids.iter().zip(values) {
        registers[id.index()] = *value;
    }
}

fn transfer(table: &SolvePureCallTable, op: &Op, slots: &mut [bool], registers: &mut [bool]) {
    match op {
        Op::Load { destination, slot } => registers[destination.index()] = slots[slot.index()],
        Op::Store { slot, source } => slots[slot.index()] = registers[source.index()],
        Op::Call {
            owner,
            arguments,
            destinations,
        } => {
            let values = table
                .owner(*owner)
                .map(|owner| outputs(table, owner.body(), &read(registers, arguments)))
                .unwrap_or_else(|| vec![false; destinations.len()]);
            write(registers, destinations, &values);
        }
        Op::Conditional {
            condition,
            captures,
            destinations,
            if_true,
            if_false,
        } => {
            let args = read(registers, captures);
            let a = outputs(table, if_true.body(), &args);
            let b = outputs(table, if_false.body(), &args);
            let condition = registers[condition.index()];
            let values = a
                .iter()
                .zip(b)
                .map(|(a, b)| condition && *a && b)
                .collect::<Vec<_>>();
            write(registers, destinations, &values);
        }
        Op::Fold {
            domain,
            initial,
            captures,
            destinations,
            transition,
        } => {
            let mut carried = read(registers, initial);
            let captures = read(registers, captures);
            // Meet the initial tuple with every possible iteration. A finite
            // Boolean lattice reaches a fixed point without expanding loops.
            loop {
                let mut args = carried.clone();
                args.extend_from_slice(&captures);
                args.extend(std::iter::repeat_n(true, domain.binders.len()));
                let next = outputs(table, transition.body(), &args);
                let next = carried
                    .iter()
                    .zip(next)
                    .map(|(a, b)| *a && b)
                    .collect::<Vec<_>>();
                if next == carried {
                    break;
                }
                carried = next;
            }
            write(registers, destinations, &carried);
        }
        Op::Map {
            captures,
            destination,
            body,
            ..
        } => {
            // Uniform inputs are necessary but a binder is also constant
            // across time, even though it changes across coordinates.
            let mut args = read(registers, captures);
            args.resize(body.inputs().len(), true);
            registers[destination.index()] = outputs(table, body.body(), &args).iter().all(|v| *v);
        }
        _ => {
            let mut stable = true;
            op.visit_input_registers(|id| stable &= registers[id.index()]);
            op.visit_output_registers(|id| registers[id.index()] = stable);
        }
    }
}
