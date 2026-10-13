use super::*;
use rumoca_core::{SourceId, Span};

fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("typed_storage.modelica"),
        offset,
        offset + 1,
    )
}

fn arithmetic() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

#[test]
fn checked_storage_forwards_one_functional_update_without_mutating_input() {
    let arithmetic = arithmetic();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![4]).unwrap();
    let program = solve::TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            tensor.clone(),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            tensor.clone(),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let aggregate = builder.load(input, span(2))?;
        let index =
            builder.constant(solve::SolveValue::integer(arithmetic, 2).unwrap(), span(3))?;
        let value = builder.constant(solve::SolveValue::real(arithmetic, 9.0), span(4))?;
        let updated = builder.update_element(aggregate, value, &[index], span(5))?;
        builder.store(output, updated, span(6))
    })
    .unwrap();

    let layout = ProgramLayout::new(&program, (1, 1), &[]).unwrap();
    let solve::SolveOperation::Load {
        destination: aggregate,
        ..
    } = program.operations()[0].operation()
    else {
        panic!("first operation is the aggregate load")
    };
    let solve::SolveOperation::UpdateElement {
        destination: updated,
        ..
    } = program.operations()[3].operation()
    else {
        panic!("fourth operation is the functional update")
    };
    let aggregate = &layout.registers[aggregate.index()];
    let updated = &layout.registers[updated.index()];
    assert_eq!(aggregate.base, StorageBase::Input);
    assert_eq!(aggregate.cell, 0);
    assert_eq!(updated.base, StorageBase::Output);
    assert_eq!(updated.cell, 0);
}

#[test]
fn checked_storage_never_aliases_a_read_only_constant_tape_slot() {
    let arithmetic = arithmetic();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![4]).unwrap();
    let program = solve::TypedProgram::construct(arithmetic, |builder| {
        let constant = builder.declare_slot(
            tensor,
            solve::SolveStorageClass::Constant,
            solve::SolveSlotAccess::ReadOnly,
            span(10),
        )?;
        let aggregate = builder.load(constant, span(11))?;
        let index =
            builder.constant(solve::SolveValue::integer(arithmetic, 2).unwrap(), span(12))?;
        let value = builder.constant(solve::SolveValue::real(arithmetic, 9.0), span(13))?;
        let _updated = builder.update_element(aggregate, value, &[index], span(14))?;
        Ok(())
    })
    .unwrap();

    let layout = ProgramLayout::new(&program, (0, 0), &[]).unwrap();
    let solve::SolveOperation::Load {
        destination: aggregate,
        slot,
    } = program.operations()[0].operation()
    else {
        panic!("first operation is the constant-slot load")
    };
    let slot = &layout.slots[slot.index()];
    let aggregate = &layout.registers[aggregate.index()];
    assert_eq!(slot.base, StorageBase::Tape);
    assert_eq!(aggregate.base, StorageBase::Tape);
    assert_ne!(aggregate.cell, slot.cell);
}

#[test]
fn checked_storage_reuses_tape_only_after_the_last_use() {
    let arithmetic = arithmetic();
    let program = solve::TypedProgram::construct(arithmetic, |builder| {
        let first = builder.constant(solve::SolveValue::real(arithmetic, 2.0), span(20))?;
        let live = builder.unary(solve::SolveUnaryOperator::Negate, first, span(21))?;
        let reused = builder.constant(solve::SolveValue::real(arithmetic, 3.0), span(22))?;
        let _result = builder.binary(solve::SolveBinaryOperator::Add, live, reused, span(23))?;
        Ok(())
    })
    .unwrap();

    let layout = ProgramLayout::new(&program, (0, 0), &[]).unwrap();
    let first = &layout.registers[0];
    let live = &layout.registers[1];
    let reused = &layout.registers[2];
    assert_eq!(first.cell, reused.cell);
    assert_ne!(first.cell, live.cell);
}

#[test]
fn checked_storage_keeps_same_operation_inputs_and_output_disjoint() {
    let arithmetic = arithmetic();
    let program = solve::TypedProgram::construct(arithmetic, |builder| {
        let lhs = builder.constant(solve::SolveValue::real(arithmetic, 2.0), span(30))?;
        let rhs = builder.constant(solve::SolveValue::real(arithmetic, 3.0), span(31))?;
        let _result = builder.binary(solve::SolveBinaryOperator::Add, lhs, rhs, span(32))?;
        Ok(())
    })
    .unwrap();

    let layout = ProgramLayout::new(&program, (0, 0), &[]).unwrap();
    let cells = layout
        .registers
        .iter()
        .map(|location| location.cell)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(cells.len(), 3);
}
