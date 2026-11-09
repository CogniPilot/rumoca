use super::*;
use crate::{SolveRegisterHome, SolveStorageClass, SolveUnaryOperator};

/// An owner with a vector result and an assertion predicate taken straight
/// from its Boolean input.
fn asserting_owner() -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(911),
            vec![
                vector_type(),
                SolveValueType::scalar(SolveScalarType::Boolean),
            ],
            vec![
                SolvePureCallOutput::result(vector_type()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(911),
            |builder, inputs, outputs| {
                let vector = builder.load(inputs[0], span(912))?;
                let negated = builder.unary(SolveUnaryOperator::Negate, vector, span(913))?;
                builder.store(outputs[0], negated, span(914))?;
                let condition = builder.load(inputs[1], span(915))?;
                builder.store(outputs[1], condition, span(916))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// An observation entry writes the predicate cell itself, so no register may
/// take the predicate slot. Every other difference from the ordinary table
/// is a register a removed alias freed, or one that shares such a register's
/// cells.
#[test]
fn an_observation_entry_never_aliases_a_predicate_slot() {
    let table = asserting_owner();
    let owner = table.owners().first().unwrap();
    let body = owner.body();
    let predicate_slot = owner.inputs().len() + 1;
    assert_eq!(
        body.slots()[predicate_slot].storage(),
        SolveStorageClass::Output
    );
    let ordinary = body.storage_plan().aliases().registers();
    let observation = owner.observation_aliases();
    let observation = observation.registers();
    let condition = 2;
    assert_eq!(
        ordinary[condition],
        SolveRegisterHome::OutputSlot {
            slot: predicate_slot
        }
    );
    assert_eq!(
        observation[condition],
        SolveRegisterHome::InputSlot { slot: 1 }
    );
    // Only the freed condition changes, back to its input view, so there is
    // no cascade.
    assert_eq!(
        crate::typed_program::program::observation_cascades(
            ordinary,
            observation,
            &[predicate_slot]
        ),
        Ok(vec![])
    );
    // The result output is not a predicate and keeps its alias.
    assert_eq!(observation[1], SolveRegisterHome::OutputSlot { slot: 2 });
}

/// A computed predicate must have private cells in the observation form:
/// its output is published by the assertion protocol, not a register alias.
#[test]
fn a_computed_observation_predicate_has_its_own_frame_cells() {
    let real = SolveValueType::scalar(SolveScalarType::real(profile()));
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(921),
            vec![real],
            vec![SolvePureCallOutput::assertion_predicate()],
            span(921),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(922))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(923))?;
                let positive =
                    builder.compare(crate::SolveCompareOperator::Greater, x, zero, span(924))?;
                builder.store(outputs[0], positive, span(925))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = table.owners().first().unwrap();
    let body = owner.body();
    let SolveOperation::Store { source, .. } = body.operations().last().unwrap().operation() else {
        panic!("the fixture ends by storing its predicate")
    };
    assert_eq!(
        body.storage_plan().aliases().registers()[source.index()],
        SolveRegisterHome::OutputSlot { slot: 1 }
    );
    assert_eq!(body.storage_plan().cells().offsets()[source.index()], None);
    let aliases = owner.observation_aliases();
    assert_eq!(aliases.registers()[source.index()], SolveRegisterHome::Own);
    let cells = owner.observation_cells();
    let offset = cells.offsets()[source.index()].expect("private predicate cells");
    assert!(offset < cells.extent());
}
