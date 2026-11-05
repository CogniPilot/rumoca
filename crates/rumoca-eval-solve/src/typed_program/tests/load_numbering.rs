//! Scalar immutable-load sharing keeps the existing numeric and fault relation.
use super::*;
use rumoca_ir_solve::{SolveOperation, SolveSlotAccess, SolveStorageClass};

fn input_or_writable_copy<'p>(
    b: &mut TypedProgramBuilder<'p>,
    input: ProgramSlot<'p>,
    ty: SolveValueType,
    writable: bool,
) -> Result<ProgramSlot<'p>, SolveProgramConstructionError> {
    if !writable {
        return Ok(input);
    }
    let local = b.declare_slot(
        ty,
        SolveStorageClass::MethodLocal,
        SolveSlotAccess::ReadWrite,
        span(990),
    )?;
    let value = b.load(input, span(991))?;
    b.store(local, value, span(992))?;
    Ok(local)
}

fn repeated_coefficient(format: SolveRealFormat, writable: bool) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(990),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty.clone())],
            span(993),
            |b, inputs, outputs| {
                let source = input_or_writable_copy(b, inputs[0], ty.clone(), writable)?;
                let a = b.load(source, span(994))?;
                let first = b.unary(SolveUnaryOperator::Sin, a, span(995))?;
                let first = b.binary(SolveBinaryOperator::Divide, first, a, span(996))?;
                let second_input = b.load(source, span(997))?;
                let second = b.unary(SolveUnaryOperator::Sin, second_input, span(998))?;
                let second =
                    b.binary(SolveBinaryOperator::Divide, second, second_input, span(999))?;
                let sum = b.binary(SolveBinaryOperator::Add, first, second, span(1000))?;
                b.store(outputs[0], sum, span(1001))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn real_value(format: SolveRealFormat, value: f64) -> TypedValue {
    TypedValue::construct(
        SolveValueType::scalar(SolveScalarType::real(profile(format))),
        vec![real_kind(format, value)],
    )
    .unwrap()
}

#[test]
fn shared_load_coefficients_keep_primal_and_directional_ieee_bits() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let shared = repeated_coefficient(format, false);
        let separate = repeated_coefficient(format, true);
        let shared_id = shared.owners()[0].id();
        let separate_id = separate.owners()[0].id();
        let sine_count = |table: &SolvePureCallTable| {
            table.owners()[0]
                .body()
                .operations()
                .iter()
                .filter(|op| {
                    matches!(
                        op.operation(),
                        SolveOperation::Unary {
                            operator: SolveUnaryOperator::Sin,
                            ..
                        }
                    )
                })
                .count()
        };
        assert_eq!((sine_count(&shared), sine_count(&separate)), (1, 2));
        for value in [
            0.0,
            -0.0,
            1.0,
            -2.0,
            f64::EPSILON,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::from_bits(0x7ff8_0000_0000_0123),
        ] {
            let input = real_value(format, value);
            assert_eq!(
                stopping_call(&shared, shared_id, std::slice::from_ref(&input)).unwrap(),
                stopping_call(&separate, separate_id, std::slice::from_ref(&input)).unwrap()
            );
            for seed in [0.0, -0.0, 1.0, -2.0] {
                let args = [input.clone(), real_value(format, seed)];
                assert_eq!(
                    stopping_directional_call(&shared, shared_id, &args).unwrap(),
                    stopping_directional_call(&separate, separate_id, &args).unwrap()
                );
            }
        }
    }
}

fn quotient_owner(writable: bool) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let ty = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1002),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty.clone())],
            span(1002),
            |b, inputs, outputs| {
                let source = input_or_writable_copy(b, inputs[0], ty.clone(), writable)?;
                let one = b.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(1003))?;
                let a = b.load(source, span(1004))?;
                let first = b.binary(SolveBinaryOperator::IntegerQuotient, one, a, span(1005))?;
                let second_input = b.load(source, span(1006))?;
                let second = b.binary(
                    SolveBinaryOperator::IntegerQuotient,
                    one,
                    second_input,
                    span(1007),
                )?;
                let result = b.binary(SolveBinaryOperator::Add, first, second, span(1008))?;
                b.store(outputs[0], result, span(1009))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn shared_load_does_not_change_the_first_integer_fault_span() {
    let shared = quotient_owner(false);
    let separate = quotient_owner(true);
    let argument =
        TypedValue::scalar(&SolveValue::integer(profile(SolveRealFormat::Binary64), 0).unwrap());
    let a = stopping_call(
        &shared,
        shared.owners()[0].id(),
        std::slice::from_ref(&argument),
    )
    .unwrap_err();
    let b = stopping_call(&separate, separate.owners()[0].id(), &[argument]).unwrap_err();
    assert_eq!(a, b);
    assert!(
        matches!(a, TypedProgramEvalError::IntegerArithmetic { provenance, .. } if provenance == span(1005))
    );
}

#[test]
fn writable_scalar_store_is_observed_by_the_next_load() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let ty = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1010),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty.clone())],
            span(1010),
            |b, inputs, outputs| {
                let local = input_or_writable_copy(b, inputs[0], ty.clone(), true)?;
                let before = b.load(local, span(1011))?;
                let zero = b.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(1012))?;
                b.store(local, zero, span(1013))?;
                let after = b.load(local, span(1014))?;
                let result = b.binary(SolveBinaryOperator::Subtract, before, after, span(1015))?;
                b.store(outputs[0], result, span(1016))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let input = TypedValue::scalar(&SolveValue::integer(arithmetic, 7).unwrap());
    let result = stopping_call(&table, table.owners()[0].id(), &[input]).unwrap();
    assert_eq!(result[0].elements(), &[SolveValueKind::Integer(7)]);
}
