use super::*;

#[test]
fn repeated_scalar_computations_share_one_register() {
    let arithmetic = profile();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap();
    let index_type = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            vector.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let index = builder.declare_slot(
            index_type.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let output = builder.declare_slot(
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let samples = builder.load(input, span(3))?;
        let position = builder.load(index, span(4))?;
        let first = builder.project_element_dynamic(samples, &[position], span(5))?;
        let second = builder.project_element_dynamic(samples, &[position], span(6))?;
        assert_eq!(first, second);
        let one = SolveValue::integer(arithmetic, 1).unwrap();
        let (a, b) = (
            builder.constant(one.clone(), span(8))?,
            builder.constant(one, span(9))?,
        );
        assert_eq!(a, b);
        let next_a = builder.binary(SolveBinaryOperator::Add, position, a, span(10))?;
        let next_b = builder.binary(SolveBinaryOperator::Add, position, b, span(11))?;
        assert_eq!(next_a, next_b);
        // A different operand order is a different computation.
        let swapped = builder.binary(SolveBinaryOperator::Subtract, a, position, span(12))?;
        let plain = builder.binary(SolveBinaryOperator::Subtract, position, a, span(13))?;
        assert_ne!(swapped, plain);
        builder.store(output, first, span(14))
    })
    .unwrap();
    let count = |matches: fn(&SolveOperation) -> bool| {
        program
            .operations()
            .iter()
            .filter(|operation| matches(operation.operation()))
            .count()
    };
    assert_eq!(
        count(|operation| matches!(operation, SolveOperation::ProjectElementDynamic { .. })),
        1
    );
    assert_eq!(
        count(|operation| matches!(operation, SolveOperation::Constant { .. })),
        1
    );
    assert_eq!(
        count(|operation| matches!(operation, SolveOperation::Binary { .. })),
        3
    );
}

#[test]
fn immutable_scalar_loads_enable_shared_coefficient_computation() {
    let arithmetic = profile();
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let program = TypedProgram::construct(arithmetic, |b| {
        let input = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(20),
        )?;
        let output = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(21),
        )?;
        let first = b.load(input, span(22))?;
        let coefficient = b.unary(SolveUnaryOperator::Sin, first, span(23))?;
        let second = b.load(input, span(24))?;
        let repeated = b.unary(SolveUnaryOperator::Sin, second, span(25))?;
        assert_eq!(first, second);
        assert_eq!(coefficient, repeated);
        let sum = b.binary(SolveBinaryOperator::Add, coefficient, repeated, span(26))?;
        b.store(output, sum, span(27))
    })
    .unwrap();
    assert_eq!(
        program
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Load { .. }))
            .count(),
        1
    );
    assert_eq!(
        program
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Unary { .. }))
            .count(),
        1
    );
    assert_eq!(program.register_types().len(), 3);
}

#[test]
fn immutable_scalar_slots_do_not_exchange_load_identity() {
    let arithmetic = profile();
    for element in [
        SolveScalarType::Boolean,
        SolveScalarType::integer(arithmetic),
        SolveScalarType::real(arithmetic),
    ] {
        TypedProgram::construct(arithmetic, |b| {
            let ty = SolveValueType::scalar(element);
            let left = b.declare_slot(
                ty.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(30),
            )?;
            let right = b.declare_slot(
                ty,
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(31),
            )?;
            let a = b.load(left, span(32))?;
            let b_value = b.load(right, span(33))?;
            assert_ne!(a, b_value);
            assert_eq!(a, b.load(left, span(34))?);
            assert_eq!(b_value, b.load(right, span(35))?);
            Ok(())
        })
        .unwrap();
    }
}

#[test]
fn writable_scalar_loads_remain_distinct_across_stores() {
    let arithmetic = profile();
    let ty = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let program = TypedProgram::construct(arithmetic, |b| {
        let input = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadWrite,
            span(40),
        )?;
        let before = b.load(input, span(41))?;
        let zero = b.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(42))?;
        b.store(input, zero, span(43))?;
        let after = b.load(input, span(44))?;
        assert_ne!(before, after);
        let again = b.load(input, span(45))?;
        assert_ne!(after, again);
        Ok(())
    })
    .unwrap();
    assert_eq!(
        program
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Load { .. }))
            .count(),
        3
    );
}

#[test]
fn immutable_tensor_loads_remain_distinct_for_payload_lifetimes() {
    let arithmetic = profile();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 2]).unwrap();
    let program = TypedProgram::construct(arithmetic, |b| {
        let input = b.declare_slot(
            ty,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(50),
        )?;
        let first = b.load(input, span(51))?;
        let second = b.load(input, span(52))?;
        assert_ne!(first, second);
        Ok(())
    })
    .unwrap();
    assert_eq!(program.register_types().len(), 2);
}

#[test]
fn repeated_load_still_checks_provenance_and_rejects_readonly_store() {
    let arithmetic = profile();
    TypedProgram::construct(arithmetic, |b| {
        let input = b.declare_slot(SolveValueType::scalar(SolveScalarType::integer(arithmetic)),
            SolveStorageClass::Input, SolveSlotAccess::ReadOnly, span(60))?;
        let first = b.load(input, span(61))?;
        assert_eq!(b.load(input, Span::DUMMY).unwrap_err(), SolveProgramConstructionError::MissingProvenance);
        assert!(matches!(b.store(input, first, span(62)), Err(SolveProgramConstructionError::ReadOnlyStore { provenance }) if provenance == span(62)));
        Ok(())
    }).unwrap();
}

#[test]
fn wire_replay_preserves_preexisting_distinct_scalar_load_registers() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |b| {
        // Model a pre-optimization body; the wire replay must keep its exact SSA IDs.
        b.numbered_values = false;
        let ty = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
        let input = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(70),
        )?;
        let output = b.declare_slot(
            ty,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(71),
        )?;
        let a = b.load(input, span(72))?;
        let second = b.load(input, span(73))?;
        assert_ne!(a, second);
        let sum = b.binary(SolveBinaryOperator::Add, a, second, span(74))?;
        b.store(output, sum, span(75))
    })
    .unwrap();
    let wire = serde_json::to_string(&program).unwrap();
    let replay: TypedProgram = serde_json::from_str(&wire).unwrap();
    assert_eq!(replay, program);
    assert_eq!(serde_json::to_string(&replay).unwrap(), wire);
    assert_eq!(
        replay
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Load { .. }))
            .count(),
        2
    );
}

#[test]
fn scalar_selects_share_one_register_while_tensor_selects_stay_distinct() {
    let arithmetic = profile();
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let program = TypedProgram::construct(arithmetic, |b| {
        let mut read = |ty: &SolveValueType, at: usize| {
            let slot = b.declare_slot(
                ty.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(at),
            )?;
            b.load(slot, span(at + 1))
        };
        let (x, y) = (read(&real, 80)?, read(&real, 82)?);
        let (u, v) = (read(&vector, 84)?, read(&vector, 86)?);
        let condition = b.compare(SolveCompareOperator::Less, x, y, span(88))?;
        let first = b.select(condition, x, y, span(89))?;
        let second = b.select(condition, x, y, span(90))?;
        assert_eq!(first, second);
        // Swapped branches are a different value.
        let swapped = b.select(condition, y, x, span(91))?;
        assert_ne!(first, swapped);
        let tensor_a = b.select(condition, u, v, span(92))?;
        let tensor_b = b.select(condition, u, v, span(93))?;
        assert_ne!(tensor_a, tensor_b);
        Ok(())
    })
    .unwrap();
    assert_eq!(
        program
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Select { .. }))
            .count(),
        4
    );
}

#[test]
fn wire_replay_preserves_preexisting_duplicate_scalar_selects() {
    let arithmetic = profile();
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let program = TypedProgram::construct(arithmetic, |b| {
        b.numbered_values = false;
        let slot = b.declare_slot(
            real.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(100),
        )?;
        let output = b.declare_slot(
            real.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(101),
        )?;
        let x = b.load(slot, span(102))?;
        let zero = b.constant(SolveValue::real(arithmetic, 0.0), span(103))?;
        let condition = b.compare(SolveCompareOperator::Less, x, zero, span(104))?;
        let first = b.select(condition, x, zero, span(105))?;
        let second = b.select(condition, x, zero, span(106))?;
        assert_ne!(first, second);
        let sum = b.binary(SolveBinaryOperator::Add, first, second, span(107))?;
        b.store(output, sum, span(108))
    })
    .unwrap();
    let wire = serde_json::to_string(&program).unwrap();
    let replay: TypedProgram = serde_json::from_str(&wire).unwrap();
    assert_eq!(replay, program);
    assert_eq!(
        replay
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Select { .. }))
            .count(),
        2
    );
}
