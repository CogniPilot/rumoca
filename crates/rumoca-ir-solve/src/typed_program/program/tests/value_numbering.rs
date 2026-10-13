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
