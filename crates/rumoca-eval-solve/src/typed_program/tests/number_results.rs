use super::super::number::{
    eval_binary_typed, eval_compare_typed, eval_convert_typed, eval_unary_typed,
};
use super::*;

#[test]
fn scalar_arithmetic_matches_tensor_elements_at_ieee_boundaries() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let p = profile(format);
        let scalar = SolveValueType::scalar(SolveScalarType::real(p));
        let tensor = SolveValueType::tensor(SolveScalarType::real(p), vec![2]).unwrap();
        for (a, b) in [
            (-0.0, 0.0),
            (f64::INFINITY, f64::NEG_INFINITY),
            (f64::NAN, 2.0),
            (16_777_217.0, 1.0),
        ] {
            let lhs = TypedValue::construct(scalar.clone(), vec![real_kind(format, a)]).unwrap();
            let rhs = TypedValue::construct(scalar.clone(), vec![real_kind(format, b)]).unwrap();
            let lhs_tensor =
                TypedValue::construct(tensor.clone(), vec![real_kind(format, a); 2]).unwrap();
            let rhs_tensor =
                TypedValue::construct(tensor.clone(), vec![real_kind(format, b); 2]).unwrap();
            for op in [
                SolveBinaryOperator::Add,
                SolveBinaryOperator::Multiply,
                SolveBinaryOperator::Divide,
                SolveBinaryOperator::Min,
                SolveBinaryOperator::Max,
            ] {
                let value = eval_binary_typed(op, &lhs, &rhs, span(2000)).unwrap();
                let aggregate =
                    eval_binary_typed(op, &lhs_tensor, &rhs_tensor, span(2000)).unwrap();
                assert_eq!(value.value_type(), &scalar);
                assert_eq!(value.elements()[0], aggregate.elements()[0]);
            }
            let value = eval_unary_typed(SolveUnaryOperator::Negate, &lhs, span(2001)).unwrap();
            let aggregate =
                eval_unary_typed(SolveUnaryOperator::Negate, &lhs_tensor, span(2001)).unwrap();
            assert_eq!(value.elements()[0], aggregate.elements()[0]);
            let value =
                eval_compare_typed(SolveCompareOperator::Equal, &lhs, &rhs, span(2002)).unwrap();
            let aggregate = eval_compare_typed(
                SolveCompareOperator::Equal,
                &lhs_tensor,
                &rhs_tensor,
                span(2002),
            )
            .unwrap();
            assert_eq!(value.elements()[0], aggregate.elements()[0]);
        }
    }
}

#[test]
fn one_cell_tensors_retain_their_shapes_and_checked_conversions() {
    let p = profile(SolveRealFormat::Binary64);
    let real = SolveScalarType::real(p);
    let integer = SolveScalarType::integer(p);
    for shape in [vec![1], vec![1, 1]] {
        let source_type = SolveValueType::tensor(real, shape.clone()).unwrap();
        let target_type = SolveValueType::tensor(integer, shape).unwrap();
        let value = TypedValue::construct(
            source_type,
            vec![SolveValueKind::Real64((-2.5_f64).to_bits()); target_type.scalar_count() as usize],
        )
        .unwrap();
        let converted = eval_convert_typed(
            SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
            &value,
            target_type.clone(),
            span(2010),
        )
        .unwrap();
        assert_eq!(converted.value_type(), &target_type);
        assert!(
            converted
                .elements()
                .iter()
                .all(|element| *element == SolveValueKind::Integer(-3))
        );
        let wrong_shape = SolveValueType::tensor(integer, vec![2]).unwrap();
        assert!(
            matches!(eval_convert_typed(SolveConversionOperator::RealToIntegerTowardZero, &value, wrong_shape, span(2011)), Err(TypedProgramEvalError::InvalidCheckedProgram { operation: "construct runtime value", provenance }) if provenance == span(2011))
        );
    }
}
