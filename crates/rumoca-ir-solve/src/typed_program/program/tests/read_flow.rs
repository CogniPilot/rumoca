use super::*;

/// The last read of every register and of a read-only slot is fixed by the
/// program, whatever the extent of the aggregate it carries.
#[test]
fn last_reads_follow_operation_order() {
    let arithmetic = profile();
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap();
    let index_type = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let mut ids = Vec::new();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            tensor.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            tensor.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let first = builder.load(input, span(2))?;
        let second = builder.load(input, span(3))?;
        let value = builder.constant(SolveValue::real(arithmetic, 2.0), span(4))?;
        let index = builder.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(5))?;
        let updated = builder.update_element(second, value, &[index], span(6))?;
        builder.store(output, updated, span(7))?;
        ids.extend([first.id, second.id, updated.id]);
        assert_eq!(builder.register_type(index, span(8))?, &index_type);
        Ok(())
    })
    .unwrap();
    let [first, second, updated] = ids[..] else {
        panic!("three registers were recorded");
    };
    let input = SolveSlotId(0);
    let output = SolveSlotId(1);
    // Operations: 0 load, 1 load, 2 constant, 3 constant, 4 update, 5 store.
    assert!(!program.slot_last_load_at(input, 0));
    assert!(program.slot_last_load_at(input, 1));
    assert!(!program.slot_last_load_at(output, 5));
    assert!(program.register_last_read_at(second, 4));
    assert!(!program.register_last_read_at(second, 3));
    assert!(program.register_last_read_at(updated, 5));
    assert!(!program.register_last_read_at(first, 0));
    assert!((0..6).all(|operation| !program.register_last_read_at(first, operation)));
}

/// An operation that lists one register twice reads it last, but cannot move
/// it out for either operand.
#[test]
fn a_register_listed_twice_by_its_last_reader_is_not_movable() {
    let arithmetic = profile();
    let mut doubled = None;
    let program = TypedProgram::construct(arithmetic, |builder| {
        let output = builder.declare_slot(
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(0),
        )?;
        let value = builder.constant(SolveValue::real(arithmetic, 3.0), span(1))?;
        let square = builder.binary(SolveBinaryOperator::Multiply, value, value, span(2))?;
        builder.store(output, square, span(3))?;
        doubled = Some(value.id);
        Ok(())
    })
    .unwrap();
    let doubled = doubled.unwrap();
    assert!((0..4).all(|operation| !program.register_last_read_at(doubled, operation)));
}
