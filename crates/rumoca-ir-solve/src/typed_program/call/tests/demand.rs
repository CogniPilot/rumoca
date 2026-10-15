use super::*;
use crate::{FaultClass, SolveLiveness, SolveOutputDemand};

/// A caller that calls a three-output owner and reads only its first output.
fn caller_reading_the_first_output() -> SolvePureCallTable {
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap();
    let inputs = vec![vector.clone(), vector.clone()];
    let outputs = vec![
        SolvePureCallOutput::result(vector.clone()),
        SolvePureCallOutput::result(vector),
        SolvePureCallOutput::assertion_predicate(),
    ];
    SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1),
            inputs.clone(),
            outputs.clone(),
            span(0),
            |builder, inputs, outputs| {
                let first = builder.load(inputs[0], span(1))?;
                let second = builder.load(inputs[1], span(2))?;
                let safe = builder.constant(SolveValue::boolean(true), span(3))?;
                builder.store(outputs[0], first, span(4))?;
                builder.store(outputs[1], second, span(5))?;
                builder.store(outputs[2], safe, span(6))
            },
        )?;
        table.add_owner(
            identity(2),
            inputs,
            outputs,
            span(7),
            |builder, inputs, outputs| {
                let first = builder.load(inputs[0], span(8))?;
                let second = builder.load(inputs[1], span(9))?;
                let values = builder.call(inner, &[first, second], span(10))?;
                builder.store(outputs[0], values[0], span(11))?;
                builder.store(outputs[1], first, span(12))?;
                builder.store(outputs[2], values[2], span(13))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn the_demand_of_a_call_site_is_the_set_of_outputs_the_caller_reads() {
    let table = caller_reading_the_first_output();
    let caller = table.owners()[1].body();
    let call = caller
        .operations()
        .iter()
        .position(|operation| matches!(operation.operation(), SolveOperation::Call { .. }))
        .unwrap();
    let SolveOperation::Call { destinations, .. } = caller.operations()[call].operation() else {
        unreachable!()
    };
    let reads = |output: usize| {
        caller.operations().iter().any(|operation| {
            let mut read = false;
            operation
                .operation()
                .visit_input_registers(|register| read |= register == destinations[output]);
            read
        })
    };
    let expected = SolveOutputDemand::of((0..destinations.len()).map(reads));
    assert_eq!(caller.call_site_demand(call), Some(&expected));
    assert!(expected.wants(0) && !expected.wants(1) && expected.wants(2));
    assert_eq!(caller.call_site_demand(call + 1), None);
}

#[test]
fn a_pruned_body_keeps_every_operation_that_can_fault_or_has_an_effect() {
    let table = caller_reading_the_first_output();
    let caller = table.owners()[1].body();
    let only_first = SolveOutputDemand::of([true, false, false]);
    let liveness = SolveLiveness::of(caller, &only_first);
    for (index, operation) in caller.operations().iter().enumerate() {
        if liveness.is_dead(index) {
            assert!(
                caller.fault_class(index) == FaultClass::CannotFault
                    || matches!(operation.operation(), SolveOperation::Store { .. }),
                "operation {index} is dropped but {:?}",
                caller.fault_class(index)
            );
        }
    }
    let call = caller
        .operations()
        .iter()
        .position(|operation| matches!(operation.operation(), SolveOperation::Call { .. }))
        .unwrap();
    assert!(!liveness.is_dead(call));
}

/// An owner whose body fills a vector nothing reads before storing its
/// outputs from its inputs, the shape of a compiler-generated seed of a
/// value every executed path assigns before reading.
fn owner_with_an_unread_fill() -> SolvePureCallTable {
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap();
    let inputs = vec![vector.clone()];
    let outputs = vec![
        SolvePureCallOutput::result(vector.clone()),
        SolvePureCallOutput::assertion_predicate(),
    ];
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            inputs,
            outputs,
            span(0),
            |builder, inputs, outputs| {
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1))?;
                let _seed = builder.fill(zero, vec![3], span(2))?;
                let first = builder.load(inputs[0], span(3))?;
                let safe = builder.constant(SolveValue::boolean(true), span(4))?;
                builder.store(outputs[0], first, span(5))?;
                builder.store(outputs[1], safe, span(6))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_full_demand_still_drops_a_prunable_operation_nothing_reads() {
    let table = owner_with_an_unread_fill();
    let body = table.owners()[0].body();
    let liveness = SolveLiveness::of(body, &SolveOutputDemand::all());
    let fill = body
        .operations()
        .iter()
        .position(|operation| matches!(operation.operation(), SolveOperation::Fill { .. }))
        .unwrap();
    assert!(
        liveness.is_dead(fill),
        "the unread seed fill runs under a full demand"
    );
    for (index, operation) in body.operations().iter().enumerate() {
        match operation.operation() {
            SolveOperation::Store { .. } | SolveOperation::Load { .. } => {
                assert!(
                    !liveness.is_dead(index),
                    "operation {index} builds an output"
                );
            }
            _ => {}
        }
    }
    assert!(!liveness.drops(0) && !liveness.drops(1));
}
