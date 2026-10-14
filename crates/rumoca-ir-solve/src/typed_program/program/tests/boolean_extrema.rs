use super::*;

/// SPEC_0049 operator leaves: MLS §10.3.4 admits Boolean extrema, not sums.
#[test]
fn boolean_extrema_constructor_parity_retains_scalar_and_array_types() {
    let program = TypedProgram::construct(profile(), |builder| {
        let low = builder.constant(SolveValue::boolean(false), span(1))?;
        let high = builder.constant(SolveValue::boolean(true), span(2))?;
        for op in [SolveBinaryOperator::Min, SolveBinaryOperator::Max] {
            let result = builder.binary(op, low, high, span(3))?;
            assert_eq!(
                builder.register_type(result, span(4))?,
                &SolveValueType::scalar(SolveScalarType::Boolean)
            );
        }
        let array = builder.construct_aggregate(&[high, low], vec![2], span(5))?;
        for op in [
            SolveReductionOperator::Minimum,
            SolveReductionOperator::Maximum,
        ] {
            let result = builder.reduce(op, array, span(6))?;
            assert_eq!(
                builder.register_type(result, span(7))?,
                &SolveValueType::scalar(SolveScalarType::Boolean)
            );
            assert!(matches!(
                builder.reduce(op, low, span(8)),
                Err(SolveProgramConstructionError::InvalidTensorAlgebra { .. })
            ));
        }
        for op in [SolveReductionOperator::Sum, SolveReductionOperator::Product] {
            assert!(matches!(
                builder.reduce(op, array, span(9)),
                Err(SolveProgramConstructionError::InvalidTensorAlgebra { .. })
            ));
        }
        assert!(matches!(
            builder.binary(SolveBinaryOperator::Add, low, high, span(10)),
            Err(SolveProgramConstructionError::TypeMismatch { .. })
        ));
        let wrong_shape = builder.fill(high, vec![3], span(11))?;
        assert!(matches!(
            builder.binary(SolveBinaryOperator::Min, array, wrong_shape, span(12)),
            Err(SolveProgramConstructionError::TypeMismatch { .. })
        ));
        Ok(())
    })
    .expect("Boolean extrema construct through the production checker");
    let replayed: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("Boolean extrema replay through the same constructors");
    assert_eq!(program, replayed);
}
