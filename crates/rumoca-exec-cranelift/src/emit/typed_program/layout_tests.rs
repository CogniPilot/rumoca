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

    let layout = ProgramLayout::new(&program, (1, 1), IssuedHomes::of(&program)).unwrap();
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

    let layout = ProgramLayout::new(&program, (0, 0), IssuedHomes::of(&program)).unwrap();
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
fn checked_storage_keeps_address_free_scalars_out_of_the_tape() {
    let arithmetic = arithmetic();
    let program = solve::TypedProgram::construct(arithmetic, |builder| {
        // The slot value is unknown to the arithmetic constructor, so this
        // fixture retains the dependency whose storage lifetime it checks.
        let source = builder.declare_slot(
            solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic)),
            solve::SolveStorageClass::Constant,
            solve::SolveSlotAccess::ReadOnly,
            span(20),
        )?;
        let first = builder.load(source, span(20))?;
        let live = builder.unary(solve::SolveUnaryOperator::Negate, first, span(21))?;
        let reused = builder.constant(solve::SolveValue::real(arithmetic, 3.0), span(22))?;
        let _result = builder.binary(solve::SolveBinaryOperator::Add, live, reused, span(23))?;
        Ok(())
    })
    .unwrap();

    let layout = ProgramLayout::new(&program, (0, 0), IssuedHomes::of(&program)).unwrap();
    assert_eq!(layout.tape_cells, 1, "only the constant slot needs a cell");
    for (ordinal, location) in layout.registers.iter().enumerate() {
        assert_eq!(location.base, StorageBase::Scalar);
        assert_eq!(location.cell as usize, ordinal);
    }
}

#[test]
fn checked_storage_keeps_same_operation_inputs_and_output_disjoint() {
    let arithmetic = arithmetic();
    let program = solve::TypedProgram::construct(arithmetic, |builder| {
        let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
        let lhs = builder.declare_slot(
            scalar.clone(),
            solve::SolveStorageClass::Constant,
            solve::SolveSlotAccess::ReadOnly,
            span(30),
        )?;
        let rhs = builder.declare_slot(
            scalar,
            solve::SolveStorageClass::Constant,
            solve::SolveSlotAccess::ReadOnly,
            span(31),
        )?;
        let lhs = builder.load(lhs, span(30))?;
        let rhs = builder.load(rhs, span(31))?;
        let _result = builder.binary(solve::SolveBinaryOperator::Add, lhs, rhs, span(32))?;
        Ok(())
    })
    .unwrap();

    let layout = ProgramLayout::new(&program, (0, 0), IssuedHomes::of(&program)).unwrap();
    assert!(
        layout
            .registers
            .iter()
            .all(|location| location.base == StorageBase::Scalar)
    );
    let cells = layout
        .registers
        .iter()
        .map(|location| location.cell)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(cells.len(), 3);
}
