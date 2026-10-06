use super::*;
use crate::SolveIntegerDomain;

#[test]
fn integer_quotients_require_integer_operands_and_replay_checked_wire() {
    let arithmetic =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    for operator in [
        SolveBinaryOperator::IntegerQuotient,
        SolveBinaryOperator::IntegerModulo,
        SolveBinaryOperator::IntegerRemainder,
    ] {
        let program = TypedProgram::construct(arithmetic, |b| {
            let lhs = b.constant(
                SolveValue::integer(arithmetic, 9_007_199_254_740_993).unwrap(),
                span(0),
            )?;
            let rhs = b.constant(SolveValue::integer(arithmetic, 7).unwrap(), span(1))?;
            let result = b.binary(operator, lhs, rhs, span(2))?;
            assert_eq!(
                b.register_type(result, span(3))?.element_type(),
                SolveScalarType::integer(arithmetic)
            );
            for value in [SolveValue::real(arithmetic, 7.0), SolveValue::boolean(true)] {
                let value = b.constant(value, span(4))?;
                let count = b.operations.len();
                assert_eq!(
                    b.binary(operator, value, value, span(5)),
                    Err(SolveProgramConstructionError::TypeMismatch {
                        provenance: span(5)
                    })
                );
                assert_eq!(b.operations.len(), count);
            }
            Ok(())
        })
        .unwrap();
        let json = serde_json::to_string(&program).unwrap();
        let decoded: TypedProgram = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded, program);
        assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
    }
}

#[test]
fn forged_real_integer_quotient_cannot_enter_through_wire() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |b| {
        let real = b.constant(SolveValue::real(arithmetic, 7.0), span(0))?;
        b.binary(SolveBinaryOperator::Divide, real, real, span(1))?;
        Ok(())
    })
    .unwrap();
    let wire = serde_json::to_string(&program).unwrap();
    for operator in ["integer_divide", "integer_modulo", "integer_remainder"] {
        let forged = wire.replace(
            "\"operator\":\"divide\"",
            &format!("\"operator\":\"{operator}\""),
        );
        assert_ne!(forged, wire);
        assert!(serde_json::from_str::<TypedProgram>(&forged).is_err());
    }
}
